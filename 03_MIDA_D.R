# ============================================================
# D: 
# ============================================================

# D: sampling function

sample_data <- function(population, n) {
  
  sample_idx <- sample(1:nrow(population), size = n, replace = FALSE)
  
  return(population[sample_idx, ])
}

# D: treatment assignment
assign_treatment <- function(d, prop_treated = 0.5) {
  d$W <- rbinom(nrow(d), 1, prop_treated)
  d$Y <- ifelse(d$W == 1, d$Y1, d$Y0)
  return(d)
}

# D: MAR missingness mechanism of quant and qual variables

impose_covariate_missingness <- function(sample, missingness_level)
  {
  N<-nrow(sample)
  
  
  if (missingness_level == 0) {
    world <- sample
    attr(world, "miss_rate_target")       <- 0
    attr(world, "miss_rate_quote_actual") <- 0
    attr(world, "miss_rate_rfm_actual")   <- 0
    attr(world, "p_miss_quote_summary")   <- summary(rep(0, N))
    attr(world, "p_miss_rfm_summary")     <- summary(rep(0, N))
    return(world)
  }
  # ----------------------------------------------------------------
  # QUOTED_VALUE missingness
  # MAR mechanism: depends on CHANNEL_TYPE and DEAL_OWNER
  # Phone channel (CHANNEL_TYPE_D) + any owner raises base prob.
  # One owner (DEAL_OWNER_A) raises it further — reflects poorest
  # manual note-taking discipline in the CRM workflow.
  # ----------------------------------------------------------------
  
  # Additive shifts on the logit scale
  # CHANNEL_TYPE_D (phone): hardest to record completely → large positive shift
  # DEAL_OWNER_A: worst recording habits → additional positive shift
  
  channel_shift <- ifelse(sample$CHANNEL_TYPE == "CHANNEL_TYPE_D",  1.5, 0)
  owner_shift   <- ifelse(sample$DEAL_OWNER   == "DEAL_OWNER_A",    0.8, 0)
  

  # Calibrate intercept so mean missingness == missingness_level
  intercept_quote_missingness_model <- uniroot(
    function(a) mean(plogis(a + channel_shift + owner_shift)) - missingness_level,
    interval = c(-20, 20)
  )$root
  
  probability_missing_quote <- plogis(intercept_quote_missingness_model + channel_shift + owner_shift)
  
  random_missing_quote <- rbinom(N, 1, probability_missing_quote)
  
  # ----------------------------------------------------------------
  # RFM_SEGMENT missingness
  # MAR mechanism: depends on NUM_PREVIOUS_ORDERS
  # Customers with 0 prior orders lack sufficient behavioural history
  # for the CRM to assign a stable RFM segment.
  # ----------------------------------------------------------------
  
  low_history_shift<-ifelse(sample$NUM_PREVIOUS_ORDERS == 0,  1.8, 0)
  
  # Calibrate intercept so mean missingness == missingness_level
  intercept_rfm_missingness_model <- uniroot(
    function(a) mean(plogis(a +low_history_shift )) - missingness_level,
    interval = c(-20, 20)
  )$root
  
  probability_missing_rfm <- plogis(intercept_rfm_missingness_model + low_history_shift)
  random_missing_rfm <- rbinom(N, 1, probability_missing_rfm)
  
  # ----------------------------------------------------------------
  # Apply missingness to a copy of the data
  # ----------------------------------------------------------------
  world <- sample
  world$QUOTED_VALUE[random_missing_quote == 1] <- NA
  world$RFM_SEGMENT[random_missing_rfm   == 1] <- NA
  
  # ----------------------------------------------------------------
  # Attach diagnostics as attributes (useful for verification)
  # ----------------------------------------------------------------
  attr(world, "miss_rate_target")       <- missingness_level
  attr(world, "miss_rate_quote_actual") <- mean(random_missing_quote)
  attr(world, "miss_rate_rfm_actual")   <- mean(random_missing_rfm)
  attr(world, "p_miss_quote_summary")   <- summary(probability_missing_quote)
  attr(world, "p_miss_rfm_summary")     <- summary(probability_missing_rfm)
  
  return(world)
}
