# ============================================================
# M: Synthetic CRM population
# Synthetic CRM-like data generation informed by the BAW project
# ============================================================

generate_synthetic_crm_population <- function(N_POP,target_conversion_rate, seed)
  {
  
  set.seed(seed)
  
  # ------------------------------------------------------------
  # CRM covariates
  # ------------------------------------------------------------
  
  DEAL_OWNER <- sample(
    c(
      "DEAL_OWNER_REFERENCE",
      "DEAL_OWNER_A",
      "DEAL_OWNER_B",
      "DEAL_OWNER_C",
      "DEAL_OWNER_D",
      "DEAL_OWNER_E",
      "DEAL_OWNER_F",
      "DEAL_OWNER_G"
    ),
    size = N_POP,
    replace = TRUE,
    prob = rep(1 / 8, 8)
  )
  # Channel weights: website form dominant, personal email second,
  # remaining channels generate much less inflow
  # Informed by real CRM data channel distribution
  channel_weights <- c(
    0.10,  # CHANNEL_TYPE_REFERENCE
    0.01,  # CHANNEL_TYPE_A - minor
    0.04,  # CHANNEL_TYPE_B - minor
    0.22,  # CHANNEL_TYPE_C - Website Form (dominant)
    0.05,  # CHANNEL_TYPE_D - Personal Phone (rare)
    0.18,  # CHANNEL_TYPE_E - Personal Email (second)
    0.03,  # CHANNEL_TYPE_F - ManuallyCreated (rare)
    0.03,  # CHANNEL_TYPE_G - rare
    0.12,  # CHANNEL_TYPE_H - Zendesk (moderate)
    0.22   # CHANNEL_TYPE_I - Quotes Email (moderate)
  )
  
  CHANNEL_TYPE <- sample(
    c(
      "CHANNEL_TYPE_REFERENCE",
      "CHANNEL_TYPE_A",
      "CHANNEL_TYPE_B",
      "CHANNEL_TYPE_C",
      "CHANNEL_TYPE_D",
      "CHANNEL_TYPE_E",
      "CHANNEL_TYPE_F",
      "CHANNEL_TYPE_G",
      "CHANNEL_TYPE_H",
      "CHANNEL_TYPE_I"
    ),
    size    = N_POP,
    replace = TRUE,
    prob    = channel_weights
  )

  NUM_PREVIOUS_ORDERS <- pmin(150,
                              pmax(0, rnbinom(N_POP, mu = 7.7, size = 0.15))
  )
  
  CUSTOMER_LIFETIME_VALUE_LOG <- rnorm(
    N_POP,
    mean = 7.5 + 0.35 * log1p(NUM_PREVIOUS_ORDERS),
    sd = 0.55
  )
  
  RFM_SCORE <- 
    0.60 * log1p(NUM_PREVIOUS_ORDERS) +
    0.50 * as.numeric(scale(CUSTOMER_LIFETIME_VALUE_LOG)) +
    rnorm(N_POP, mean = 0, sd = 0.60)
  
  RFM_SEGMENT <- cut(
    RFM_SCORE,
    breaks = quantile(RFM_SCORE, probs = seq(0, 1, length.out = 12)),
    labels = c(
      "RFM_SEGMENT_REFERENCE",
      "RFM_SEGMENT_A",
      "RFM_SEGMENT_B",
      "RFM_SEGMENT_C",
      "RFM_SEGMENT_D",
      "RFM_SEGMENT_E",
      "RFM_SEGMENT_F",
      "RFM_SEGMENT_G",
      "RFM_SEGMENT_H",
      "RFM_SEGMENT_I",
      "RFM_SEGMENT_J"
    ),
    include.lowest = TRUE
  )
  
  NUM_PRODUCT <- pmax(1, rnbinom(N_POP, mu = 4, size = 0.5))
  
  QUOTED_VALUE <- round(
    rlnorm(
      N_POP,
      meanlog = 7.6 + 0.08 * NUM_PRODUCT,
      sdlog = 0.45
    ),
    0
  )
  
  DAYS_TO_SEND_QUOTE <- pmin(300,
                                      pmax(1, rnbinom(N_POP, mu = 10, size = 0.5))
  )
  
  # Seasonal weights for month distribution: peak March-April 
  # Based on real data pattern: high spring, lower winter
  month_weights <- c(
    0.06,  # Jan - low
    0.07,  # Feb - picking up
    0.11,  # Mar - peak
    0.11,  # Apr - peak
    0.09,  # May - high
    0.09,  # Jun - high
    0.08,  # Jul - moderate
    0.09,  # Aug - moderate
    0.10,  # Sep - second bump
    0.09,  # Oct - declining
    0.06,  # Nov - low
    0.05   # Dec - lowest
  )
  
  QUOTE_REQUEST_MONTH <- sample(
    1:12,
    size    = N_POP,
    replace = TRUE,
    prob    = month_weights
  )
  #DAYS_BETWEEN_FIRST_AND_LAST_QUOTE skewed and because skewed needs log transformation
  DAYS_BETWEEN_FIRST_AND_LAST_QUOTE_LOG<-log1p(ifelse(
    rbinom(N_POP, 1, prob = 0.60),
    0,
    pmax(0, rnbinom(N_POP, mu = 150, size = 0.3))
  ))
  # ------------------------------------------------------------
  # Baseline conversion probability under no follow-up: p0
  # ------------------------------------------------------------
  
  lp0_no_intercept <-
    0.0004 * QUOTED_VALUE +
    0.0069 * DAYS_TO_SEND_QUOTE +
    0.0220 * CUSTOMER_LIFETIME_VALUE_LOG +
    0.0231 * QUOTE_REQUEST_MONTH +
    0.0005 * NUM_PRODUCT -
    0.0033 * NUM_PREVIOUS_ORDERS -
    0.0314 * DAYS_BETWEEN_FIRST_AND_LAST_QUOTE_LOG +
    
    0.2998 * (RFM_SEGMENT == "RFM_SEGMENT_A") -
    0.0550 * (RFM_SEGMENT == "RFM_SEGMENT_B") +
    0.5440 * (RFM_SEGMENT == "RFM_SEGMENT_C") +
    0.2253 * (RFM_SEGMENT == "RFM_SEGMENT_D") +
    0.5431 * (RFM_SEGMENT == "RFM_SEGMENT_E") +
    0.3951 * (RFM_SEGMENT == "RFM_SEGMENT_F") -
    0.2656 * (RFM_SEGMENT == "RFM_SEGMENT_G") +
    0.1141 * (RFM_SEGMENT == "RFM_SEGMENT_H") +
    0.1509 * (RFM_SEGMENT == "RFM_SEGMENT_I") -
    0.3546 * (RFM_SEGMENT == "RFM_SEGMENT_J") -
    
    0.1619 * (CHANNEL_TYPE == "CHANNEL_TYPE_A") -
    0.5087 * (CHANNEL_TYPE == "CHANNEL_TYPE_B") +
    0.1235 * (CHANNEL_TYPE == "CHANNEL_TYPE_C") -
    0.2253 * (CHANNEL_TYPE == "CHANNEL_TYPE_D") +
    0.1256 * (CHANNEL_TYPE == "CHANNEL_TYPE_E") -
    0.4884 * (CHANNEL_TYPE == "CHANNEL_TYPE_F") -
    0.3907 * (CHANNEL_TYPE == "CHANNEL_TYPE_G") -
    0.4319 * (CHANNEL_TYPE == "CHANNEL_TYPE_H") -
    0.5735 * (CHANNEL_TYPE == "CHANNEL_TYPE_I")
  
  

  # Calibrate intercept to target baseline conversion rate
  beta0 <- uniroot(
    function(a) {
      mean(plogis(a + lp0_no_intercept)) -
        target_conversion_rate
    },
    interval = c(-20, 20)
  )$root
  
  # Baseline conversion probability
  p0 <- plogis(beta0 + lp0_no_intercept)
  
  #Y0 outcomes
  Y0 <- rbinom(N_POP,1,p0)
  
  
  #Tau depends on quote_value, rfm, deal_owner
  #standartise quoted_value
  z_quote <- (log(QUOTED_VALUE) - mean(log(QUOTED_VALUE))) / sd(log(QUOTED_VALUE))
  
  # RFM weights on logit scale
  # A-J quantile-based: lower segments = less loyal = more unmet potential
  rfm_gamma <-
    0.15 * (RFM_SEGMENT == "RFM_SEGMENT_A") +
    0.10 * (RFM_SEGMENT == "RFM_SEGMENT_B") +
    0.05 * (RFM_SEGMENT == "RFM_SEGMENT_C") +
    0.00 * (RFM_SEGMENT == "RFM_SEGMENT_D") +
    0.00 * (RFM_SEGMENT == "RFM_SEGMENT_E") +
    -0.05 * (RFM_SEGMENT == "RFM_SEGMENT_F") +
    -0.08 * (RFM_SEGMENT == "RFM_SEGMENT_G") +
    -0.10 * (RFM_SEGMENT == "RFM_SEGMENT_H") +
    -0.15 * (RFM_SEGMENT == "RFM_SEGMENT_I") +
    -0.20 * (RFM_SEGMENT == "RFM_SEGMENT_J") +
    0.00 * (RFM_SEGMENT == "RFM_SEGMENT_REFERENCE")
  
  
  # Deal owner weights: reflect follow-up execution quality, most sales reps are the same with two mild outliers in each direction
  owner_gamma <-
    0.10 * (DEAL_OWNER == "DEAL_OWNER_A") +
    0.08 * (DEAL_OWNER == "DEAL_OWNER_B") +
    0.05 * (DEAL_OWNER == "DEAL_OWNER_C") +
    0.03 * (DEAL_OWNER == "DEAL_OWNER_D") +
    -0.03 * (DEAL_OWNER == "DEAL_OWNER_E") +
    -0.05 * (DEAL_OWNER == "DEAL_OWNER_F") +
    -0.08 * (DEAL_OWNER == "DEAL_OWNER_G") +
    0.00 * (DEAL_OWNER == "DEAL_OWNER_REFERENCE")
  
  # tau on logit scale
  # gamma_0 = 0.20: base lift ~5pp ATE at average p0
  # gamma_quote = 0.10: higher value quotes respond more
  # rfm_gamma: customer relationship strength
  # owner_gamma: follow-up execution quality
  # floor at 0: follow-up is inert, never harmful
  # ceiling at 100% : max ~24pp lift
  tau_logit_scale <- pmin(1.0, pmax(0,
                                    0.20 +
                                      3 * (
                                        0.10 * z_quote +
                                          rfm_gamma +
                                          owner_gamma
                                      )
  ))
  

  
  p1<- plogis(beta0 + lp0_no_intercept + tau_logit_scale)
  Y1<- rbinom(N_POP, 1, p1)
  
  
  tau <- p1 - p0
  
  

  # ------------------------------------------------------------
  # Return synthetic population
  # ------------------------------------------------------------

  return(data.frame(
    CUSTOMER_ID = seq_len(N_POP),Y0, Y1, p0, p1, tau,
    DEAL_OWNER, RFM_SEGMENT, QUOTED_VALUE,
    NUM_PREVIOUS_ORDERS, CHANNEL_TYPE,
    CUSTOMER_LIFETIME_VALUE_LOG,
    DAYS_TO_SEND_QUOTE,
    QUOTE_REQUEST_MONTH,
    DAYS_BETWEEN_FIRST_AND_LAST_QUOTE_LOG,
    NUM_PRODUCT
  ))
}
  

  





