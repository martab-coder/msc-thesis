
# ============================================================
# Anwser strategy (as defined by MIDA authors) 
# ============================================================

# HANDLING MISSING DATA FUNCTION
handle_missing_data <- function(world, method) {
  
  if (method == "cca") {
    return(enforce_factor_levels(world |> drop_na(QUOTED_VALUE, RFM_SEGMENT)))
  }
  
  if (method == "mean_mode") {
    
    world$QUOTED_VALUE[is.na(world$QUOTED_VALUE)] <- mean(
      world$QUOTED_VALUE, na.rm = TRUE
    )
    
    world$RFM_SEGMENT <- as.character(world$RFM_SEGMENT)
    world$RFM_SEGMENT[is.na(world$RFM_SEGMENT)] <- names(sort(
      table(world$RFM_SEGMENT), decreasing = TRUE
    ))[1]
    
    world$RFM_SEGMENT <- factor(world$RFM_SEGMENT, levels = FACTOR_LEVELS$RFM_SEGMENT)    
    return(enforce_factor_levels(world))
  }
  
  if (method == "mice") {
    
    mice_input <- world |>       
      select(
        DEAL_OWNER, RFM_SEGMENT, QUOTED_VALUE,
        NUM_PREVIOUS_ORDERS, CHANNEL_TYPE, 
        CUSTOMER_LIFETIME_VALUE_LOG, DAYS_TO_SEND_QUOTE,
        QUOTE_REQUEST_MONTH, DAYS_BETWEEN_FIRST_AND_LAST_QUOTE_LOG,
        NUM_PRODUCT
      )
    
    pred_matrix <- make.predictorMatrix(mice_input)
    pred_matrix[] <- 0
    pred_matrix["QUOTED_VALUE", ] <- 1  
    pred_matrix["RFM_SEGMENT", ]  <- 1  
    
    completed <- complete(
      mice(mice_input, m = M_MICE, method = "pmm",
           predictorMatrix = pred_matrix, printFlag = FALSE),
      action = "all"
    )
    
    world$QUOTED_VALUE <- rowMeans(
      sapply(completed, function(d) d$QUOTED_VALUE)
    )
    world$RFM_SEGMENT <- factor(
      apply(
        sapply(completed, function(d) as.character(d$RFM_SEGMENT)),
        1,
        function(x) names(sort(table(x), decreasing = TRUE))[1]
      ),
      levels = FACTOR_LEVELS$RFM_SEGMENT
    )
    
    return(enforce_factor_levels(world))
  }
  
 
  
  stop("Unknown missing-data handling method: ", method)
}

#the split function
split_data <- function(world, train_prop, factor_levels = FACTOR_LEVELS, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  train_idx <- sample(1:nrow(world), size = floor(train_prop * nrow(world)), replace = FALSE)
  
  train <- enforce_factor_levels(world[ train_idx, ], factor_levels)
  test  <- enforce_factor_levels(world[-train_idx, ], factor_levels)
  
  return(list(train = train, test = test))
}

#estimation function
estimate_cate_train_test <- function(train_data, test_data, estimator, n_trees) {
  # ----------------------------------------------------------------
  # setting up the components for the models
  # ----------------------------------------------------------------
  
  covariate_cols <- c("DEAL_OWNER", "CHANNEL_TYPE", "NUM_PREVIOUS_ORDERS",
                      "CUSTOMER_LIFETIME_VALUE_LOG", "RFM_SEGMENT", "NUM_PRODUCT",
                      "QUOTED_VALUE", "DAYS_TO_SEND_QUOTE", "QUOTE_REQUEST_MONTH",
                      "DAYS_BETWEEN_FIRST_AND_LAST_QUOTE_LOG")
  covariate_formula <-  ~ DEAL_OWNER + CHANNEL_TYPE + NUM_PREVIOUS_ORDERS +
    CUSTOMER_LIFETIME_VALUE_LOG + RFM_SEGMENT + NUM_PRODUCT +
    QUOTED_VALUE + DAYS_TO_SEND_QUOTE + QUOTE_REQUEST_MONTH +
    DAYS_BETWEEN_FIRST_AND_LAST_QUOTE_LOG
  
  # feature df 
  X_train_df <- train_data|> select(all_of(covariate_cols))
  X_test_df  <- test_data |> select(all_of(covariate_cols))
    
  # features in numeric matrix for causal forest

  X_train_num_mat <- model.matrix(covariate_formula, data = train_data)
  X_test_num_mat  <- model.matrix(covariate_formula, data = test_data)
  
  # outcome and treatment vectors IN NUM
  Y_train_num <- as.numeric(train_data$Y)
  W_train_num <- as.numeric(train_data$W)
  Y_test_num  <- as.numeric(test_data$Y)
  W_test_num  <- as.numeric(test_data$W)
  
  
  train_df <- data.frame(Y = factor(train_data$Y), W = train_data$W, X_train_df)
  test_df <- data.frame(Y = factor(test_data$Y), W = test_data$W, X_test_df)
  # ----------------------------------------------------------------
  # S-learner
  # ----------------------------------------------------------------
  if (estimator == "s_learner_rf") {
    
    fit <- ranger(
      Y ~ .,
      data      = train_df,
      num.trees = n_trees,
      probability = TRUE,
      num.threads = 1
    )
    
    p0 <- predict(fit, data = data.frame(W = 0, X_test_df))$predictions[, "1"]
    p1 <- predict(fit, data = data.frame(W = 1, X_test_df))$predictions[, "1"]
    
    tau_hat <- p1-p0
      
    
    return(data.frame(CUSTOMER_ID = test_data$CUSTOMER_ID, tau_hat = tau_hat))
  }
  
  # ----------------------------------------------------------------
  # T-learner
  # ----------------------------------------------------------------
  if (estimator == "t_learner_rf") {
    

    fit1 <- ranger(
      Y ~ .,
      data        = train_df |> filter(W == 1) |> select(-W),
      num.trees   = n_trees,
      probability = TRUE,
      num.threads = 1
    )
    fit0 <- ranger(
      Y ~ .,
      data        = train_df|> filter(W == 0) |> select(-W),
      num.trees   = n_trees,
      probability = TRUE,
      num.threads = 1
    )
    
    p0 <- predict(fit0, data = X_test_df)$predictions[, "1"]
    p1 <- predict(fit1, data = X_test_df)$predictions[, "1"]
    
    tau_hat <- p1 - p0
      
    
    return(data.frame(CUSTOMER_ID = test_data$CUSTOMER_ID, tau_hat = tau_hat))
  }
  
  # ----------------------------------------------------------------
  # X-learner
  # ----------------------------------------------------------------
  if (estimator == "x_learner_rf") {
    
    fit_mu1 <- ranger(
      Y ~ .,
      data      = train_df |> filter(W == 1)|> select(-W),
      num.trees = n_trees,
      probability = TRUE,
      num.threads = 1
    )
    
    fit_mu0 <- ranger(
      Y ~ .,
      data      = train_df |> filter(W == 0)|> select(-W),
      num.trees = n_trees,
      probability = TRUE,
      num.threads = 1
    )
    
    mu1_train <- predict(fit_mu1, data = X_train_df)$predictions[, "1"]
    mu0_train <- predict(fit_mu0, data = X_train_df)$predictions[, "1"]
    
    d1 <- Y_train_num[W_train_num == 1] - mu0_train[W_train_num == 1]
    d0 <- mu1_train[W_train_num == 0] - Y_train_num[W_train_num == 0]
    
    fit_tau1 <- ranger(
      D ~ .,
      data      = data.frame(D = d1, X_train_df[W_train_num == 1, ]),
      num.trees = n_trees,
      num.threads = 1
    )
    fit_tau0 <- ranger(
      D ~ .,
      data      = data.frame(D = d0, X_train_df[W_train_num == 0, ]),
      num.trees = n_trees,
      num.threads = 1
    )
    
    #ref: https://statisticaloddsandends.wordpress.com/2022/05/20/t-learners-s-learners-and-x-learners/
    tau_hat <- 0.5 * predict(fit_tau1, data = X_test_df)$predictions +
      0.5 * predict(fit_tau0, data = X_test_df)$predictions
    
    return(data.frame(CUSTOMER_ID = test_data$CUSTOMER_ID, tau_hat = tau_hat))
  }
  

  
  # ----------------------------------------------------------------
  # Causal Forest
  # ----------------------------------------------------------------
  if (estimator == "causal_forest") {
    
    fit <- causal_forest(
      X         = X_train_num_mat,
      Y         = Y_train_num,
      W         = W_train_num,
      num.trees = n_trees,
      num.threads = 1
    )
    
    tau_hat <- predict(fit, newdata = X_test_num_mat)$predictions
    
    return(data.frame(CUSTOMER_ID = test_data$CUSTOMER_ID, tau_hat = tau_hat))
  }
  
  stop("Unknown CATE estimator: ", estimator)
  
  
}



  
 
  
 
