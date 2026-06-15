# ============================================================
# setup
# ============================================================
SEED_POPULATION<-2026

SEED_REPLICATIONS<-42

N_POP    <- 50000L
N_SAMPLE <- 2000L 
R_REPS   <- 1000     # full simulation is R= 1K
N_TREES  <- 500   # RF meta-learners; causal forest uses max(500, N_TREES) = 500
M_MICE   <- 5
M_EM <- 5

TRAIN_PROP <- 0.80

TARGET_CONVERSION_RATE<- 0.45

missingness_level <- c(0.00, 0.10, 0.50, 0.70)
world_names <- c("world_0", "world_10", "world_50", "world_70")


HANDLING_METHODS <- c("cca", "mean_mode", "mice")

# rf is random forest
CATE_ESTIMATORS <- c(
  "s_learner_rf",
  "t_learner_rf",
  "x_learner_rf",
  "causal_forest"
)
FACTOR_LEVELS <- list(
  RFM_SEGMENT  = c("RFM_SEGMENT_REFERENCE",
                   "RFM_SEGMENT_A", "RFM_SEGMENT_B", "RFM_SEGMENT_C",
                   "RFM_SEGMENT_D", "RFM_SEGMENT_E", "RFM_SEGMENT_F",
                   "RFM_SEGMENT_G", "RFM_SEGMENT_H", "RFM_SEGMENT_I",
                   "RFM_SEGMENT_J"),
  DEAL_OWNER   = c("DEAL_OWNER_REFERENCE",
                   "DEAL_OWNER_A", "DEAL_OWNER_B", "DEAL_OWNER_C",
                   "DEAL_OWNER_D", "DEAL_OWNER_E", "DEAL_OWNER_F",
                   "DEAL_OWNER_G"),
  CHANNEL_TYPE = c("CHANNEL_TYPE_REFERENCE",
                   "CHANNEL_TYPE_A", "CHANNEL_TYPE_B", "CHANNEL_TYPE_C",
                   "CHANNEL_TYPE_D", "CHANNEL_TYPE_E", "CHANNEL_TYPE_F",
                   "CHANNEL_TYPE_G", "CHANNEL_TYPE_H", "CHANNEL_TYPE_I")
)


# ============================================================
# Parallelisation
# ============================================================

# 16 cores available; leave 2 free for the OS
N_CORES <- 14
plan(multisession, workers = N_CORES)

# plain-text progress bar for Rscript terminal output
handlers(global = TRUE)
handlers("txtprogressbar")

# ============================================================
# Helper functions
# ============================================================

#find mode for qualitative covariates 
mode_value <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

enforce_factor_levels <- function(d, factor_levels = FACTOR_LEVELS) {
  for (col in names(factor_levels)) {
    if (col %in% names(d)) {
      d[[col]] <- factor(d[[col]], levels = factor_levels[[col]])
    }
  }
  return(d)
}