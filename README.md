# CATE Estimation under Missing Data — Simulation Study

Simulation study examining how covariate missingness affects the accuracy, stability, and ranking quality of CATE (Conditional Average Treatment Effect) estimators in a synthetic B2B CRM setting. The design follows the MIDA framework (Model, Inquiry, Data strategy, Answer strategy).

---

## How to Run

### Full simulation (1 000 replications)

```r
Rscript MAIN_running_sim.R
```

Results are saved to `simulation_results_final.rds` HOWEVER, the dataset is too large to be stored in the git repo. 

### Results analysis & figures

```r
Rscript Results_analysis.R
```
---

## File Overview

| File | Description |
|------|-------------|
| `MAIN_running_sim.R` | Main entry point. Loads all modules, generates the fixed synthetic population, runs the full simulation (1 000 reps), and saves output. |
| `01_Setup.R` | Global constants (seeds, sample sizes, missingness levels, estimator names, parallelisation with `furrr`, helper functions). |
| `02_MIDA_M_I.R` | **Model & Inquiry.** Funcyions that generate the synthetic CRM population of N = 5 000 units using a logistic surrogate model to define baseline conversion probabilities and individual treatment effects (τᵢ). |
| `03_MIDA_D.R` | **Data strategy.** function that imposes MAR missingness on `QUOTED_VALUE` and `RFM_SEGMENT` at four levels (0 %, 10 %, 50 %, 70 %). |
| `04_MIDA_A.R` | **Answer strategy.** Functions that impose the three missing-data handling methods (CCA, mean/mode imputation, MICE), the train/test split, and the four CATE estimators (S-Learner RF, T-Learner RF, X-Learner RF, Causal Forest). |
| `05_Simulation.R` | Function of a simulation loop. Iterates over replications, missingness worlds, handling methods, and estimators in parallel, returns a combined results rds|
| `00_surrogate_model_script.R` | One-off script used to fit the LightGBM surrogate model that informs the data-generating process in `02_MIDA_M_I.R`. |
| `Results_analysis.R` | Loads saved `.rds` results and produces tables and plots (RMSE, SD of RMSE, top-20 % targeting overlap). |
| `sim_1rep_graph.R` | Generates the Graphviz diagram illustrating the structure of a single simulation replication. |

```
