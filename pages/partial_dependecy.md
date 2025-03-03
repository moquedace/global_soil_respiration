<p align="center">
<img src="../img/soil_respiration_github.png" width="1200">
</p>

<p>&nbsp;</p>

# Partial dependence analysis for total soil respiration (Rs) and heterotrophic respiration (Rh) models.

## Load packages
``` r
pkg <- c("dplyr", "caret", "rfUtilities", "quantregForest", "tidyr",
         "beepr", "ggplot2", "stringr")

sapply(pkg, require, character.only = T)
#> Carregando pacotes exigidos: dplyr
#> 
#> Anexando pacote: 'dplyr'
#> Os seguintes objetos são mascarados por 'package:stats':
#> 
#>     filter, lag
#> Os seguintes objetos são mascarados por 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Carregando pacotes exigidos: caret
#> Carregando pacotes exigidos: ggplot2
#> Carregando pacotes exigidos: lattice
#> Carregando pacotes exigidos: rfUtilities
#> Carregando pacotes exigidos: quantregForest
#> Carregando pacotes exigidos: randomForest
#> randomForest 4.7-1.2
#> Type rfNews() to see new features/changes/bug fixes.
#> 
#> Anexando pacote: 'randomForest'
#> O seguinte objeto é mascarado por 'package:ggplot2':
#> 
#>     margin
#> O seguinte objeto é mascarado por 'package:dplyr':
#> 
#>     combine
#> Carregando pacotes exigidos: RColorBrewer
#> Carregando pacotes exigidos: tidyr
#> Carregando pacotes exigidos: beepr
#> Carregando pacotes exigidos: stringr
#>          dplyr          caret    rfUtilities quantregForest          tidyr 
#>           TRUE           TRUE           TRUE           TRUE           TRUE 
#>          beepr        ggplot2        stringr 
#>           TRUE           TRUE           TRUE

rm(list = ls())
```

## Partial dependency function 
``` r
partial_dependency <- function(model, data, y_var, x_var, lci = 0.25, uci = 0.75, delta = FALSE) {
  if (!any(class(model) %in% c("randomForest", "list"))) 
    stop("Model must be a randomForest object")
  if (model$type != "regression") 
    stop("Only regression models are supported")
  
  conf_int <- (uci - lci) * 100
  sorted_values <- sort(data[, x_var])
  
  pred_mean <- vector()
  pred_lower <- vector()
  pred_upper <- vector()
  baseline <- stats::predict(model, data, what = 0.5)
  
  for (i in 1:length(sorted_values)) {
    data[, x_var] <- sorted_values[i]
    predictions <- stats::predict(model, data, what = mean)
    
    if (delta) predictions <- predictions - baseline
    
    pred_mean[i] <- stats::weighted.mean(predictions, na.rm = TRUE)
    pred_lower[i] <- stats::quantile(predictions, lci, na.rm = TRUE)
    pred_upper[i] <- stats::quantile(predictions, uci, na.rm = TRUE)
  }
  
  result_df <- data.frame(
    x = sorted_values,
    y = pred_mean,
    lower = pred_lower,
    upper = pred_upper
  )
  names(result_df) <- c(x_var, y_var, "lci", "uci")
  
  return(result_df)
}
```
## Load models 
``` r
model_files <- list.files(
  path = "./results/img", 
  pattern = ".RData$",
  full.names = TRUE
)
```
## Initialize storage list
``` r
partial_results <- list()
```
## Process models
``` r
for (model_idx in seq_along(model_files)) {
  start_time <- Sys.time()
  load(model_files[model_idx])
  
  predictor_names <- names(training_data)[-1]  # Assuming dsel_rfe is loaded
  response_name <- names(training_data)[1]
  
  model_results <- list()
  
  for (pred_idx in seq_along(predictor_names)) {
    iter_start <- Sys.time()
    
    model_results[[pred_idx]] <- partial_dependency(
      model = final_model,  # Assuming fit$finalModel is loaded
      data = training_data,
      y_var = response_name,
      x_var = predictor_names[pred_idx]
    )
    
    print(paste(predictor_names[pred_idx], pred_idx, Sys.time() - iter_start))
  }
  
  partial_results[[model_idx]] <- model_results
  save(partial_results, file = "./results/img_out/partial_dependency_median.RData")
  
  print(Sys.time() - start_time)
  beep(3)
}

save(partial_results, file = "./results/img_out/partial_dependency_median.RData")


```
## Load results
``` r
load("./results/img_out/partial_dependency_median.RData")
```
## Plotting section 1 
``` r
ggplot(partial_results[[1]][[1]], aes(y = rs, x = npp)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/BPtAyNn.png)<!-- -->
  


## Data processing for Rs plots
  ``` r
rs_data <- data.frame()
for (i in seq_along(partial_results[[1]])) {
  temp_df <- partial_results[[1]][[i]] %>% 
    tidyr::gather(key = "variable", value = "value", -c(rs, lci, uci))
  
  if (i == 1) {
    rs_data <- temp_df
  } else {
    rs_data <- rbind(rs_data, temp_df)
  }
}
  ``` 
## Rs plot grid
  ``` r
rs_plot <- ggplot(rs_data, aes(y = rs, x = value)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~variable, scales = "free", labeller = label_parsed) +
  theme(axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"))

plot(rs_plot)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/vFkZNUp.png)<!-- -->
  
  ``` r

ggsave(rs_plot, 
       filename = "./figuras/rs_partial_dependencies.jpg",
       dpi = 600,
       width = 19,
       height = 13.436,
       units = "in")
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  ```
## Data processing for Rh plots
  ``` r
rh_data <- data.frame()
for (i in seq_along(partial_results[[2]])) {
  temp_df <- partial_results[[2]][[i]] %>% 
    tidyr::gather(key = "variable", value = "value", -c(rh, lci, uci))
  
  if (i == 1) {
    rh_data <- temp_df
  } else {
    rh_data <- rbind(rh_data, temp_df)
  }
}
  ``` 
## Rh plot grid 

  ``` r
rh_plot <- ggplot(rh_data, aes(y = rh, x = value)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~variable, scales = "free", labeller = label_parsed) +
  theme(axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"))

plot(rh_plot)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](https://i.imgur.com/X1oPfMk.png)<!-- -->
  
  ``` r

ggsave(rh_plot, 
       filename = "./figuras/rh_partial_dependencies.jpg",
       dpi = 600,
       width = 19,
       height = 13.436,
       units = "in")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
  ```
## Combined plot
``` r
combined_data <- bind_rows(
  rs_data %>% 
    gather(key = "process", value = "vr", -c("variable", "lci", "uci", "value")) %>% 
    mutate(process = "Rs~(g~C~m^{-2}~year^{-1})"),
  rh_data %>%
    gather(key = "process", value = "vr", -c("variable", "lci", "uci", "value")) %>% 
    mutate(process = "Rh~(g~C~m^{-2}~year^{-1})")
) %>% mutate(
  variable = factor(variable,
                    levels = c("npp", "bio12", "bio4", "bio3", "bio6",
                               "bio13", "bio19", "bio1", "clay_0_30cm",
                               "soc_sand_0_30cm"),
                    labels = c("NPP~(g~C~m^{-2}~year^{-1})",
                               "BIO~12~(mm~year^{-1})",
                               "BIO~4~(`%`)",
                               "BIO~3~(`%`)",
                               "BIO~6~(ºC)",
                               "BIO~13~(kg~m^{-2})",
                               "BIO~19~(kg~m^{-2})",
                               "BIO~1~(ºC)",
                               "Clay~(g~kg^-1)",
                               "atop(SOC/sand, (hg~C~~~kg~`sand`^-1))"))
)

full_plot <- ggplot(combined_data,
                    aes(y = vr, x = value)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_grid(rows = vars(process), 
             cols = vars(variable),
             scales = "free",
             labeller = label_parsed,
             switch = "both") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90"))

plot(full_plot)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/lrCedTR.png)<!-- -->
  
  ``` r

ggsave(full_plot,
       filename = "./figuras/full_partial_dependencies.jpg",
       dpi = 600,
       width = 19,
       height = 6.718,
       units = "in")
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  ``` 
## Individual variable plots
  ``` r
for (var in unique(combined_data$variable)) {
  var_plot <- ggplot(filter(combined_data, variable == var),
                     aes(y = vr, x = value)) +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_grid(rows = vars(process), labeller = label_parsed,
               scales = "free", switch = "both",
               cols = vars(variable)) +
    labs(title = parse(text = var),
         x = NULL, y = NULL) +
    theme(strip.background = element_blank(),
          strip.placement = "output",
          axis.ticks = element_line(color = "black"),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, inherit.blank = T),
          panel.grid = element_line(colour = "grey90"))
  
  
  plot(var_plot)
  ggsave(var_plot,
         filename = paste0("./figuras/partial_dep_", gsub("[^a-zA-Z0-9]", "_", var), ".jpg"),
         dpi = 600,
         width = 16,
         height = 11,
         units = "in")
}
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/A6pF2pW.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/BHCWbGX.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/RHc63gn.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/8nCZ7ks.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/jOKFuvw.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/DBoiyHi.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/E3TDDxC.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/rwXNqZZ.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/sgFxQ84.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/yqMlV7x.png)<!-- -->
