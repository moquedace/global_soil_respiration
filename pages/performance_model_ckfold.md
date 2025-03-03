<p align="center">
<img src="../img/soil_respiration_github.png" width="1200">
</p>

<p>&nbsp;</p>

# Performance Soil Respiration Model Training with Data Division Restriction

## Load Required Packages
```{r packages}
# Core machine learning and data processing packages
pkg <- c("caret", "dplyr", "terra", "sf", "stringr", "parallelly", "data.table",
         "parallel", "quantregForest", "doParallel")
sapply(pkg, require, character.only = TRUE)
```

## Set Working Directories
```{r paths}
# Define main paths for data and results
root_path <- "//200.235.173.229/backup_2023/hd_externo_seagate_8tb/bkp_cassio/R/co2_pc_clara/"
results_path <- paste0(root_path, "results_100_cs_skfold/")
setwd(root_path)

# Create necessary directories if they don't exist
dir.create(paste0(results_path, "select/cor"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(results_path, "select/rfe/metric"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(results_path, "select/rfe/select"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(results_path, "performance/csv"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(results_path, "performance/imp_pred"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(results_path, "img"), showWarnings = FALSE)
```

# Custom QRF Model Configuration
```{r model_config}
# Customize QRF to predict mean instead of quantiles
qrf_mean <- getModelInfo("qrf")$qrf
qrf_mean$predict <- function(modelFit, newdata, submodels = NULL) {
  out <- predict(modelFit, newdata, what = mean)
  if(is.matrix(out)) out <- out[,1]
  out
}
```

# Global Experiment Parameters
```{r params}
# Experiment setup parameters
nruns <- 100               # Number of independent runs
fold_rfe <- 10             # Folds for Recursive Feature Elimination (RFE)
rep_rfe <- 1               # Repeats for RFE
metric_opt <- "MAE"        # Optimization metric
size_rfe <- seq(1, 15, 1)  # Feature subset sizes to test
tune_length <- 10          # Tuning grid length
fold_model <- 10           # Folds for model training
rep_model <- 10            # Repeats for model training
selected_model <- qrf_mean # Selected algorithm
```

# Parallel Processing Configuration
```{r parallel}
# Setup parallel processing using available cores
cl <- parallel::makeCluster(parallelly::availableCores() - 1)
cl <- parallelly::autoStopCluster(cl)
```

# Data Processing Pipeline

## Load and Prepare Data Files
```{r load_files}
# Get list of input files and target variables
data_files <- list.files(
  path = "./extract_xy_cs", 
  pattern = ".csv$",
  full.names = TRUE
) %>% sort(decreasing = TRUE)

target_vars <- c("rs", "rh")  # Target variables for modeling
factor_var <- "local"         # Categorical variable identifier
```

## Main Analysis Workflow
```{r main_loop, results='hide'}
for (i in seq_along(data_files)) {
  start_time <- Sys.time()
  current_var <- target_vars[i]
  
  # Initialize performance tracking
  performance_df <- data.frame(
    model = character(nruns),
    n_train = integer(nruns),
    MAE_train = numeric(nruns),
    RMSE_train = numeric(nruns),
    Rsquared_train = numeric(nruns),
    n_test = integer(nruns),
    MAE_test = numeric(nruns),
    RMSE_test = numeric(nruns),
    Rsquared_test = numeric(nruns),
    MAE_null = numeric(nruns),
    RMSE_null = numeric(nruns)
  )

  # Data Preparation with Group Handling
  processed_data <- raw_data %>%
    filter(!!sym(current_var) > 0) %>%
    na.omit() %>%
    mutate(local = as.factor(local)) %>%
    dplyr::select(-nearZeroVar(., names = TRUE))
  
  # Group-based KFold Preparation
  group_stats <- processed_data %>%
    group_by(local) %>%
    summarise(target_mean = mean(!!sym(current_var))) %>%
    as.data.frame()

  # Main Experiment Loop
  for (n in 1:nruns) {
    set.seed(run_seeds[n])
    
    # Stratified Group Split
    train_groups <- createDataPartition(group_stats$target_mean, p = 0.75, list = FALSE)
    train_locations <- group_stats$local[train_groups]
    
    # Data Splitting
    train_set <- processed_data %>% 
      filter(local %in% train_locations) %>%
      dplyr::select(-local)
    
    test_set <- processed_data %>%
      filter(!local %in% train_locations) %>%
      dplyr::select(-local)
    
    # Grouped KFold Indices
    group_folds <- groupKFold(processed_data$local[processed_data$local %in% train_locations], 
                             k = fold_rfe)
    
    # RFE Configuration with Grouped CV
    if(current_var == "rs") {
      rfe_ctrl <- rfeControl(
        method = "repeatedcv",
        index = group_folds,
        repeats = rep_rfe,
        number = fold_rfe,
        verbose = FALSE
      )
      
      # RFE Execution
      rfe_result <- rfe(
        x = train_set %>% dplyr::select(-current_var),
        y = train_set[[current_var]],
        sizes = size_rfe,
        metric = metric_opt,
        rfeControl = rfe_ctrl,
        method = selected_model
      )
      
      # Save RFE results
      write.csv2(rfe_result$results,
                file = paste0(results_path, "select/rfe/metric/", current_var,
                            "/RFE_metrics_", n, ".csv"), 
                row.names = FALSE)
      
      optimal_features <- predictors(rfe_result)
      train_set <- train_set %>% dplyr::select(all_of(c(current_var, optimal_features)))
    }
    
    # Model Training with Grouped CV
    train_ctrl <- trainControl(
      method = "repeatedcv",
      index = group_folds,
      number = fold_model,
      repeats = rep_model,
      savePredictions = TRUE
    )
    
    final_model <- train(
      x = train_set %>% dplyr::select(-current_var),
      y = train_set[[current_var]],
      method = selected_model,
      metric = metric_opt,
      trControl = train_ctrl,
      tuneLength = tune_length,
      importance = TRUE
    )
    
    # Performance Evaluation
    test_pred <- predict(final_model, test_set)
    test_perf <- postResample(test_pred, test_set[[current_var]])
    
    # Store Results
    performance_df[n,] <- c(
      final_model$modelInfo$label,
      nrow(train_set),
      getTrainPerf(final_model)$TrainMAE,
      getTrainPerf(final_model)$TrainRMSE,
      getTrainPerf(final_model)$TrainRsquared,
      nrow(test_set),
      test_perf["MAE"],
      test_perf["RMSE"],
      test_perf["Rsquared"],
      null_perf["MAE"],
      null_perf["RMSE"]
    )
    
    # Save Iteration Results
    write.csv2(performance_df, 
              paste0(results_path, "performance/csv/", current_var, 
                    "_performance.csv"), 
              row.names = FALSE)
    
    # Cleanup
    rm(final_model, test_pred, train_set, test_set)
    gc()
  }
  
  # Save aggregated results
  save.image(paste0(results_path, "img/", target_vars[i], ".RData"))
  cat("Completed", target_vars[i], "processing in",
      round(difftime(Sys.time(), start_time, units = "hours"), 1), "hours\n")
}
```
