#load results table
simulation_results <- readRDS("simulation_results_final.rds")

#add column scenario
simulation_results <- simulation_results |>
  mutate(
    scenario = paste0(
      "miss", missingness_level*100, "%_",
      imputation_method, "_",
      estimator
    )
  )

simulation_results <- simulation_results |> mutate(failed = NULL, fail_reason = NULL)


# calculate se
simulation_results<- simulation_results |> mutate(se = (tau_hat - true_tau)^2)  

#convert to factor 
simulation_results<- simulation_results |>
  mutate(
    missingness_level  = factor(missingness_level, 
                                levels = c(0, 0.10, 0.50, 0.70)),
    imputation_method   = factor(imputation_method, 
                                 levels = c("none", "cca", "mean_mode", "mice")),
    estimator    = factor(estimator, 
                          levels = c("s_learner_rf", "t_learner_rf", 
                                     "x_learner_rf", "causal_forest"))
  )  


rmse_by_rep <- simulation_results |>
  group_by(replication, missingness_level, imputation_method, estimator) |>
  summarise(rmse = sqrt(mean(se)), .groups = "drop")
  
############
#####1#####
############
##ANOVA 3 WAY
anova_rmse <- aov(rmse ~ missingness_level * imputation_method * estimator,
                  data = rmse_by_rep)
results_anova_rmse<-tidy(anova_rmse) |>
  kable(format = "latex", booktabs = TRUE, digits = 3,
        caption = "Three-Way ANOVA Results for RMSE") |>
  kable_styling(latex_options = c("hold_position"))

save_kable(results_anova_rmse, here("output", "1", "results_anova_rmse.tex"))

# EFFECT PER SCENARIO
rmse_by_rep <- rmse_by_rep |>
  mutate(
    scenario = case_when(
      missingness_level == 0 ~ paste0("perfect_", estimator),
      TRUE ~ paste0("miss", missingness_level, 
                    "_", imputation_method,
                    "_", estimator)
    ),
    scenario = factor(scenario),
    scenario = relevel(scenario, ref = "perfect_s_learner_rf")
  )

results_m_rmse <- feols(rmse ~ scenario, data = rmse_by_rep, cluster = ~replication)
results_m_rmse

capture.output(summary(results_m_rmse), file = here("output", "1", "m_rmse_summary.txt"))

#graphs
plot_df <- rmse_by_rep|>
  group_by(missingness_level, imputation_method, estimator) |>
  summarise(
    mean_rmse = mean(rmse),
    sd_rmse   = sd(rmse),
    se_rmse   = sd(rmse) / sqrt(n()),
    .groups = "drop"
  )

graph_rmse<-ggplot(plot_df, aes(x = missingness_level, y = mean_rmse, 
                    color = estimator, group = estimator)) +
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(
    aes(ymin = mean_rmse - sd_rmse, ymax = mean_rmse + sd_rmse),
    position = position_dodge(0.3)
  ) +
  facet_wrap(~imputation_method) +
  theme_minimal(base_size = 12) +
  labs(
    title = "RMSE of CATE estimates by scenario",
    subtitle = "Points = mean RMSE; bars = ±1 SD across replications",
    x = "Missingness level", y = "RMSE", color = "Estimator"
  )
  
ggsave(here("output","1", "rmse_plot.pdf"), graph_rmse , width = 10, height = 6)


# var+bias decomposition
rmse_decomposition <- simulation_results %>%
  filter(missingness_level == 0) %>%
  group_by(replication, estimator) %>%
  summarise(
    # Term 1: ATE error squared
    ate_error_sq = (mean(tau_hat) - mean(true_tau))^2,
    
    # Term 2: heterogeneity detection error
    het_error_sq = mean(((tau_hat - mean(tau_hat)) - 
                           (true_tau - mean(true_tau)))^2),
    
    # Total MSE = Term 1 + Term 2
    total_mse = mean((tau_hat - true_tau)^2),
    
    # Check: ate_error_sq + het_error_sq should ≈ total_mse
    .groups = "drop"
  ) %>%
  group_by(estimator) %>%
  summarise(
    mean_ate_error_sq = mean(ate_error_sq),
    mean_het_error_sq = mean(het_error_sq),
    mean_total_mse    = mean(total_mse),
    pct_from_ate_bias = mean(ate_error_sq / total_mse) * 100,
    pct_from_het      = mean(het_error_sq / total_mse) * 100
  )

rmse_decomposition

save_kable(rmse_decomposition, here("output", "1", "rmse_decomposition.tex"))

#####################2#############################
plot_df <- rmse_by_rep|>
  group_by(missingness_level, imputation_method, estimator) |>
  summarise(
    mean_rmse = mean(rmse),
    sd_rmse   = sd(rmse),
    se_rmse   = sd(rmse) / sqrt(n()),
    .groups = "drop"
  )
write.csv(plot_df,here("output", "2", "sd_se_rmse.csv"), row.names = FALSE)



############
#####3######
############
overlap_at_pct <- function(tau_hat, true_tau, pct) {
  n <- length(tau_hat)
  k <- round(n * pct)
  pred_top <- order(tau_hat, decreasing = TRUE)[1:k]
  true_top <- order(true_tau, decreasing = TRUE)[1:k]
  length(intersect(pred_top, true_top)) / k
}

overlap_by_rep <- simulation_results |>
  group_by(replication, missingness_level, imputation_method, estimator) %>%
  summarise(
    overlap_20 = overlap_at_pct(tau_hat, true_tau, 0.20),
    .groups = "drop"
  )

overlap_summary <- overlap_by_rep|>
  group_by(missingness_level, imputation_method, estimator) |>
  summarise(mean_overlap = mean(overlap_20), sd_overlap = sd(overlap_20), .groups="drop")

overlap_summary


ranking_quality_graph<- ggplot(overlap_summary, aes(x = missingness_level, y = mean_overlap, color = estimator, group = estimator)) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "grey60") +
  geom_line(position = position_dodge(0.3)) +
  geom_pointrange(aes(ymin = mean_overlap - sd_overlap, ymax = mean_overlap + sd_overlap),
                  position = position_dodge(0.3)) +
  facet_wrap(~imputation_method) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Top-20% targeting overlap: predicted vs. true",
    subtitle = "Dashed line = 20% (overlap expected under random selection)",
    x = "Missingness level", y = "Overlap (fraction of predicted top-20% also in true top-20%)",
    color = "Estimator"
  )

ggsave(here("output", "3", "ranking_quality_graph.pdf"), ranking_quality_graph, width = 10, height = 6)

