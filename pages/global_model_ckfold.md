<p align="center">
<img src="../img/soil_respiration_github.png" width="1200">
</p>

<p>&nbsp;</p>

# Global Soil Respiration Model Training with Data Division Restriction

## Load Required Packages
```{r message=FALSE, warning=FALSE}
library(caret)
library(dplyr)
library(terra)
library(sf)
library(stringr)
library(parallelly)
library(data.table)
library(parallel)
library(quantregForest)
library(doParallel)

# Clean Environment
rm(list = ls())
```

## Define Paths and Model Parameters
```{r message=FALSE, warning=FALSE}
path_raiz <- "./global_soil_respiration/"
path_resultados <- "./global_soil_respiration/results_cs/"

setwd(path_raiz)

qrf_mean <- getModelInfo("qrf")$qrf
qrf_mean$predict <- function(modelFit, newdata, submodels = NULL) {
  out <- predict(modelFit, newdata, what = mean)
  if(is.matrix(out)) out <- out[,1]
  out
}

nruns <- 1
fold_rfe <- 10
rep_rfe <- 5
metric_otm <- "MAE"
size_rfe <- seq(1, 21, 1)
tn_length <- 10
fold_model <- 10
rep_model <- 10
model <- qrf_mean

cl <- parallel::makeCluster(16)
cl <- parallelly::autoStopCluster(cl)
```

## Load and Preprocess Data
```{r message=FALSE, warning=FALSE}
ly <- list.files(path = "./extract_xy", pattern = ".csv$", full.names = TRUE) %>% sort(decreasing = TRUE)
varsy <- c("rs", "rh")
varfact <- "local"
col_kfold <- "local"
```

## Model Training Loop
```{r eval=FALSE, message=FALSE, warning=FALSE, include=TRUE}
for (i in seq_along(ly)) {
  if (i == 1) {
    tfull <- Sys.time()
  }
  tvar <- Sys.time()
  dfbase <- read.csv2(ly[i])
  var <- varsy[i]

  # Ensure necessary directories exist
  dir_list <- c("select", "select/cor", "select/rfe", "select/rfe/metric", 
                "select/rfe/select", "performance", "performance/csv", 
                "performance/imp_pred", "performance/imp_pred/" %>% paste0(var), "img")
  sapply(paste0(path_resultados, dir_list), dir.create, recursive = TRUE, showWarnings = FALSE)

  dyx_sel <- dfbase %>% dplyr::select({var}) %>%
    bind_cols(dfbase %>% dplyr::select(-{var})) %>%
    filter(!!sym(var) > 0) %>%
    na.omit()

  if (var == "rs") {
    dyx_sel <- dyx_sel %>%
      mutate_at(.vars = varfact, as.factor) %>%
      dplyr::select(-one_of(nearZeroVar(., names = TRUE)))

    # Correlation-based feature selection
    mcor <- dyx_sel %>% dplyr::select(-one_of(var)) %>%
      dplyr::select_if(is.numeric) %>%
      cor(method = "spearman")
    fc <- findCorrelation(mcor, cutoff = 0.95, names = TRUE)
    dyx_sel <- dyx_sel %>% dplyr::select(-one_of(fc))
  }

  set.seed(666)
  nseed <- sample(1:100000, nruns)
  lmodel <- list()
  lpredimp <- list()
  if (var == "rs") {
    lrfepred <- list()
    lrferes <- list()
  }

  for (n in 1:nruns) {
    trun <- Sys.time()
    set.seed(nseed[n])
    gkfold <- groupKFold(dyx_sel[, col_kfold], k = fold_model)
    dyx_sel <- dyx_sel %>% dplyr::select(-one_of(col_kfold))

    if (var == "rs") {
      registerDoParallel(cl)
      rfe_ctrl <- rfeControl(method = "repeatedcv", repeats = rep_rfe, number = fold_rfe, verbose = FALSE, index = gkfold)
      model_ctrl <- trainControl(method = "repeatedcv", number = fold_rfe, repeats = rep_rfe, savePredictions = TRUE)
      formu <- as.formula(paste(var, "~ ."))

      rfe_res <- rfe(form = formu, data = dyx_sel, sizes = size_rfe, method = model, metric = metric_otm,
                     trControl = model_ctrl, tuneLength = tn_length, rfeControl = rfe_ctrl, 
                     maximize = ifelse(metric_otm %in% c("RMSE", "MAE"), FALSE, TRUE))
      pick <- caret::pickSizeTolerance(x = rfe_res$result, metric = metric_otm, tol = 4, maximize = !metric_otm %in% c("RMSE", "MAE"))
      lrfepred[[n]] <- c(rfe_res$optVariables[1:pick], "soc_sand_0_30cm")
      dfselrfe <- dyx_sel %>% dplyr::select({var}, one_of(lrfepred[[n]]))
    } else {
      dfselrfe <- dyx_sel %>% dplyr::select({var}, one_of(lrfepred[[n]]))
    }

    model_ctrl <- trainControl(method = "repeatedcv", number = fold_model, repeats = rep_model, savePredictions = TRUE, index = gkfold)
    registerDoParallel(cl)
    fit <- train(form = as.formula(paste(var, "~ .")), data = dfselrfe, metric = metric_otm, method = model,
                 trControl = model_ctrl, tuneLength = tn_length, importance = TRUE, maximize = !metric_otm %in% c("RMSE", "MAE"))

    lmodel[[n]] <- fit
    pr_train <- getTrainPerf(lmodel[[n]])
    pred_imp <- varImp(lmodel[[n]])
    pr_null <- data.frame(obs = dfselrfe[, var], pred = mean(dfselrfe[, var])) %>% caret:::postResample(pred = ., obs = dfselrfe[, var])
    lpredimp[[n]] <- data.frame(pred_imp[1]) %>% mutate(predictor = row.names(.), importance = Overall) %>% dplyr::select(-Overall) %>% relocate(predictor)
    write.csv2(lpredimp[[n]], paste0(path_resultados, "performance/imp_pred/", var, "/imp_pred_", n, ".csv"), row.names = FALSE)
  }
}
```
