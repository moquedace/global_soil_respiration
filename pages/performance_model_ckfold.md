<p align="center">
<img src="../img/soil_respiration_github.png" width="1200">
</p>

<p>&nbsp;</p>

# Performance Soil Respiration Model Training Without Restriction on Data Division

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
# Iterate through each input file
for (i in seq_along(data_files)) {
  start_time <- Sys.time()
  cat("\nProcessing", target_vars[i], "dataset...\n")
  
  # Load and preprocess data
  raw_data <- read.csv2(data_files[i])
  
  # Data cleaning pipeline
  processed_data <- raw_data %>%
    filter(!!sym(target_vars[i]) > 0) %>%    # Remove zero/negative values
    na.omit() %>%                            # Remove missing values
    mutate_at(factor_var, as.factor) %>%     # Convert to factor
    dplyr::select(-nearZeroVar(., names = TRUE))  # Remove near-zero variance predictors
  
  # Correlation-based feature elimination
  cor_matrix <- processed_data %>%
    dplyr::select(-one_of(target_vars[i])) %>%
    dplyr::select_if(is.numeric) %>%
    cor(method = "spearman")
  
  high_cor <- findCorrelation(cor_matrix, cutoff = 0.95, names = TRUE)
  processed_data <- processed_data %>% 
    dplyr::select(-one_of(high_cor), -local)
  
  # Initialize result containers
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
    RMSE_null = numeric(nruns))
  
  # Initialize parallel backend
  registerDoParallel(cl)
  
  # Multiple experimental runs
  set.seed(666)
  run_seeds <- sample(1:100000, nruns)
  
  for (n in 1:nruns) {
    iteration_start <- Sys.time()
    cat("Run", n, "/", nruns, "-", target_vars[i], "\n")
    
    # Data partitioning
    set.seed(run_seeds[n])
    train_idx <- createDataPartition(processed_data[, target_vars[i]], 
                                    p = 0.75, list = FALSE)
    train_data <- processed_data[train_idx, ]
    test_data <- processed_data[-train_idx, ]
    
    # Feature Selection (RFE for first target variable)
    if (target_vars[i] == "rs") {
      # Recursive Feature Elimination setup
      rfe_ctrl <- rfeControl(
        method = "repeatedcv",
        repeats = rep_rfe,
        number = fold_rfe,
        verbose = FALSE)
      
      # RFE execution
      rfe_result <- rfe(
        x = train_data %>% dplyr::select(-target_vars[i]),
        y = train_data[[target_vars[i]]],
        sizes = size_rfe,
        metric = metric_opt,
        rfeControl = rfe_ctrl,
        method = selected_model)
      
      # Save RFE results
      write.csv2(rfe_result$results,
                file = paste0(results_path, "select/rfe/metric/", target_vars[i],
                            "/RFE_metrics_", n, ".csv"), 
                row.names = FALSE)
      
      # Select optimal features
      optimal_features <- predictors(rfe_result)
      train_data <- train_data %>% 
        dplyr::select(all_of(c(target_vars[i], optimal_features)))
    }
    
    # Model Training Configuration
    train_ctrl <- trainControl(
      method = "repeatedcv",
      number = fold_model,
      repeats = rep_model,
      savePredictions = TRUE)
    
    # Model Training
    final_model <- train(
      x = train_data %>% dplyr::select(-target_vars[i]),
      y = train_data[[target_vars[i]]],
      method = selected_model,
      metric = metric_opt,
      trControl = train_ctrl,
      tuneLength = tune_length,
      importance = TRUE)
    
    # Performance Evaluation
    train_perf <- getTrainPerf(final_model)
    test_pred <- predict(final_model, test_data)
    test_perf <- postResample(test_pred, test_data[[target_vars[i]]])
    
    # Null model comparison
    null_pred <- rep(mean(test_data[[target_vars[i]]], nrow(test_data))
    null_perf <- postResample(null_pred, test_data[[target_vars[i]]])
    
    # Store results
    performance_df[n,] <- c(
      final_model$modelInfo$label,
      nrow(train_data),
      train_perf$TrainMAE,
      train_perf$TrainRMSE,
      train_perf$TrainRsquared,
      nrow(test_data),
      test_perf["MAE"],
      test_perf["RMSE"],
      test_perf["Rsquared"],
      null_perf["MAE"],
      null_perf["RMSE"])
    
    # Save variable importance
    var_importance <- varImp(final_model)$importance %>%
      tibble::rownames_to_column("predictor") %>%
      mutate(run = n)
    
    write.csv2(var_importance,
              paste0(results_path, "performance/imp_pred/", target_vars[i], "/",
                    "variable_importance_", n, ".csv"), 
              row.names = FALSE)
    
    # Save intermediate results
    write.csv2(performance_df, 
              paste0(results_path, "performance/csv/", target_vars[i], 
                    "_performance.csv"), 
              row.names = FALSE)
    
    # Cleanup memory
    rm(final_model, var_importance)
    gc()
    
    cat("Completed run", n, "in", 
        round(difftime(Sys.time(), iteration_start, units = "mins"), 1), "minutes\n")
  }
  
  # Save aggregated results
  save.image(paste0(results_path, "img/", target_vars[i], ".RData"))
  cat("Completed", target_vars[i], "processing in",
      round(difftime(Sys.time(), start_time, units = "hours"), 1), "hours\n")
}
```
