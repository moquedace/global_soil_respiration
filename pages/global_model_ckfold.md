<p align="center">
<img src="../img/soil_respiration_github.png" width="1200">
</p>

<p>&nbsp;</p>

# Global Soil Respiration Model Training
## Load Required Packages
```{r message=FALSE, warning=FALSE}
pkg <- c("caret", "dplyr", "terra", "sf", "stringr", "parallelly", "data.table", "parallel", "quantregForest", "doParallel")

# Load each package listed. If a package is not installed, it must be installed beforehand.
sapply(pkg, require, character.only = T)
```
## Clean Environment and Memory
```{r}
rm(list = ls())  # Remove all objects from the global environment
gc()             # Force garbage collection to optimize memory usage
```

## Define Paths and Load Custom Functions
```{r message=FALSE, warning=FALSE}
# Set the main processing directory
path_raiz <- "./global_soil_respiration/"
path_resultados <- "/global_soil_respiration/results_cs/"
setwd(path_raiz)  # Change the working directory to the specified path

# Load custom functions if needed (update with actual function path)
# source("path_to_custom_functions.R")
```

## Model Training Configuration
```{r}
qrf_mean <- getModelInfo("qrf")$qrf
qrf_mean$predict <- function(modelFit, newdata, submodels = NULL) {
  out <- predict(modelFit, newdata, what = mean)
  if (is.matrix(out)) out <- out[,1]
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
```

## Load Dataset and Preprocessing
```{r}
ly <- list.files(path = "./extract_xy", pattern = ".csv$", full.names = TRUE) %>% 
sort(decreasing = TRUE)

varsy <- c("rs", "rh")
varfact <- "local"
col_kfold <- "local"

cl <- parallel::makeCluster(16)
cl <- parallelly::autoStopCluster(cl)
```

## Model Training Loop
```{r eval=FALSE, message=FALSE, warning=FALSE, include=TRUE}
for (i in seq_along(ly)) {
  if (i == 1) {
    tfull <- Sys.time()
  }
  
  tvar <- Sys.time()
  dfbase <- read.csv2(ly[i])
  dfperf <- data.frame(model = integer(nruns),
                       n_cross = integer(nruns),
                       MAE_cross = integer(nruns),
                       RMSE_cross = integer(nruns),
                       Rsquared_cross = integer(nruns),
                       MAE_NULL = integer(nruns),
                       RMSE_NULL = integer(nruns))
  var <- varsy[i]
  
  dir.create(file.path(path_resultados, "select", "rfe", "metric", var), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path_resultados, "performance", "imp_pred", var), recursive = TRUE, showWarnings = FALSE)
  
  dy <- dfbase %>% dplyr::select({var})
  dx <- dfbase %>% dplyr::select(-{var})
  
  dyx_sel <- cbind(dy, dx) %>% filter(!!sym(var) > 0) %>% na.omit()
  
  if (var == "rs") {
    dyx_sel <- dyx_sel %>% mutate_at(.vars = varfact, as.factor)
    dyx_sel <- dyx_sel %>% dplyr::select(-one_of(nearZeroVar(., names = TRUE)))
    
    mcor <- dyx_sel %>% dplyr::select(-one_of(var)) %>% dplyr::select_if(is.numeric) %>% cor(method = "spearman")
    fc <- findCorrelation(mcor, cutoff = 0.95, names = TRUE)
    dyx_sel <- dyx_sel %>% dplyr::select(-one_of(fc))
  }
  
  set.seed(666)
  nseed <- sample(1:100000, nruns)
  
  lmodel <- list()
  lpredimp <- list()
  
  for (n in 1:nruns) {
    trun <- Sys.time()
    set.seed(nseed[n])
    gkfold <- groupKFold(dyx_sel[, col_kfold], k = fold_model)
    dyx_sel <- dyx_sel %>% dplyr::select(-one_of(col_kfold))
    
    model_ctrl <- trainControl(method = "repeatedcv", number = fold_model, repeats = rep_model, savePredictions = TRUE, index = gkfold)
    
    formu <- as.formula(paste(var, "~ ."))
    registerDoParallel(cl)
    set.seed(nseed[n])
    fit <- train(form = formu, data = dyx_sel, metric = metric_otm, method = model, trControl = model_ctrl, tuneLength = tn_length, importance = TRUE, maximize = metric_otm != "MAE")
    
    lmodel[[n]] <- fit
    pr_train <- getTrainPerf(lmodel[[n]])
    pred_imp <- varImp(lmodel[[n]])
    
    lpredimp[[n]] <- data.frame(pred_imp[1]) %>% mutate(predictor = row.names(.), importance = Overall) %>% dplyr::select(-Overall) %>% relocate(predictor)
    
    write.csv2(lpredimp[[n]], paste0(path_resultados, "performance/imp_pred/", var, "/imp_pred_", n, ".csv"), row.names = FALSE)
  }
}
stopCluster(cl)
gc()
```
