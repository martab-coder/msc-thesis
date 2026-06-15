# ============================================================
# 05_simulation_loop.R
# ============================================================
run_simulation <- function(
    population         = fixed_population,
    r_reps             = R_REPS,
    n_sample           = N_SAMPLE,
    missingness_levels = missingness_level,
    handling_methods   = HANDLING_METHODS,
    cate_estimators    = CATE_ESTIMATORS,
    train_prop         = TRAIN_PROP,
    n_trees            = N_TREES,
    m_mice             = M_MICE,
    seed_replications  = SEED_REPLICATIONS
) {
  
  p <- progressor(steps = r_reps)
  
  results_list <- future_lapply(seq_len(r_reps), function(r) {
    t0 <- Sys.time() 
    
    rep_seed <- seed_replications + r
    
    set.seed(rep_seed)
    sampled <- sample_data(population, n = n_sample)
    treated <- assign_treatment(sampled)
    
    rep_chunks <- list()
    chunk_idx  <- 1L
    
    for (miss in missingness_levels) {
      
      set.seed(rep_seed)
      world_miss <- impose_covariate_missingness(treated, missingness_level = miss)
      
      for (method in handling_methods) {
        
        if (miss == 0 && method != "cca") next
        
        world_imp <- handle_missing_data(world_miss, method = method)
        
        splits  <- split_data(world_imp, train_prop = train_prop, seed = rep_seed)
        train_d <- splits$train
        test_d  <- splits$test
        
        for (est in cate_estimators) {
          
          rows <- estimate_cate_train_test(
            train_data = train_d,
            test_data  = test_d,
            estimator  = est,
            n_trees    = n_trees
          )
          
          rows$replication       <- r
          rows$missingness_level <- miss
          rows$imputation_method <- if (miss == 0) "none" else method
          rows$estimator         <- est
          rows$true_tau          <- test_d$tau[match(rows$CUSTOMER_ID,
                                                     test_d$CUSTOMER_ID)]
          rows$failed            <- FALSE
          rows$fail_reason       <- NA_character_
          
          rep_chunks[[chunk_idx]] <- rows
          chunk_idx <- chunk_idx + 1L
          
        }  # end estimator loop
      }    # end method loop
    }      # end missingness loop
    
    elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
    p(sprintf("rep %d done in %.1fs", r, elapsed))

    do.call(rbind, rep_chunks)    # return this replication's results
    
  }, future.seed = TRUE)
  
  # ── combine all replications ───────────────────────────────────
  out <- do.call(rbind, results_list)
  
  out[, c("replication", "CUSTOMER_ID", "true_tau",
          "missingness_level", "imputation_method",
          "estimator", "tau_hat", "failed", "fail_reason")]
}

