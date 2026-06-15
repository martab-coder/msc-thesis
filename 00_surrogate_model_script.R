# P0 surrugate model development and getting to valid coefficents

top10_features <- importance_df %>%
  arrange(desc(Gain)) %>%
  slice_head(n = 10) %>%
  pull(Feature) %>%
  as.character()

top10_features
plot_df <- analysis_set %>%
  select(all_of(c(target_col, top10_features))) %>%
  mutate(
    !!target_col := as.numeric(.data[[target_col]])
  )

plot_feature_vs_target <- function(df, feature, target_col) {
  
  x <- df[[feature]]
  
  if (is.numeric(x) || is.integer(x)) {
    
    df %>%
      mutate(bin = ntile(.data[[feature]], 10)) %>%
      group_by(bin) %>%
      summarise(
        mean_x = mean(.data[[feature]], na.rm = TRUE),
        conversion_rate = mean(.data[[target_col]], na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = mean_x, y = conversion_rate)) +
      geom_line() +
      geom_point() +
      theme_minimal(base_size = 13) +
      labs(
        title = paste("Conversion rate by", feature),
        x = feature,
        y = "Average conversion rate"
      )
    
  } else {
    
    df %>%
      mutate(level = fct_lump_n(as.factor(.data[[feature]]), n = 15)) %>%
      group_by(level) %>%
      summarise(
        conversion_rate = mean(.data[[target_col]], na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(conversion_rate) %>%
      ggplot(aes(x = reorder(level, conversion_rate), y = conversion_rate)) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(
        title = paste("Conversion rate by", feature),
        x = feature,
        y = "Average conversion rate"
      )
  }
}

plots <- lapply(top10_features, function(f) {
  plot_feature_vs_target(plot_df, f, target_col)
})

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]]

#not linear relationships

corr_df <- train_df %>%
  select(all_of(top10_features))

corr_matrix <- cor(corr_df, use = "pairwise.complete.obs")

corrplot(
  corr_matrix,
  method = "number",
  type = "upper",
  tl.cex = 0.8,
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 0.6
)

#multicollinearity present !

logit_df <- analysis_set %>%
  select(all_of(c(target_col, top10_features))) %>%
  mutate(
    !!target_col := as.numeric(.data[[target_col]])
  ) %>%
  drop_na()


#FINAL TO GET THE COEFFICIENTS FOR THE P0 based on developed model 
p_hat<-predict(model, X_train)
summary(p_hat)

p0_coeff_dev<-analysis_set |>
  mutate(p_hat = p_hat)

logistic_relationship_approximation_with_predicted_phat<-glm(
  p_hat~ DEAL_OWNER + QUOTED_VALUE + DAYS_TO_SEND_QUOTE_ENGINEER +
    CUSTOMER_LIFETIME_VALUE_LOG    +       
    QUOTE_REQUEST_MONTH      +
    RFM_SEGMENT +                          
    NUM_FRAMES +
    CHANNEL_TYPE +
    NUM_PREVIOUS_ORDERS +                  
    DAYS_BETWEEN_FIRST_AND_LAST_QUOTE_LOG,
  data = analysis_set,
  family = quasibinomial()
)

summary(logistic_relationship_approximation_with_predicted_phat)

p_logistic <- predict(
  logistic_relationship_approximation_with_predicted_phat,
  type = "response"
)

cor(
  p_hat,
  p_logistic
)

rmse<-sqrt(mean((p_logistic - p_hat)^2))
rmse

beta_table <- tidy(
  logistic_relationship_approximation_with_predicted_phat
) %>%
  select(term, estimate) %>%
  mutate(
    estimate = round(estimate, 4)
  ) %>%
  rename(
    `Model term` = term,
    `Coefficient used in p0` = estimate
  )

kable(
  beta_table,
  caption = "Coefficients used to define the baseline conversion probability",
  align = "lr",
  format = "latex"
  
)

write.csv(
  beta_table,
  file = "C:/Users/marta/Desktop/Thesis/Final_outputs_folder/p0_betas.csv",
  row.names = FALSE
)
