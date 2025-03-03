``` r
# load --------------------------------------------------------------------
pkg <- c("dplyr", "caret", "rfUtilities", "quantregForest", "tidyr",
         "beepr", "ggplot2", "stringr")


setwd("//200.235.173.229/backup_2023/hd_externo_seagate_8tb/bkp_cassio/R/co2/")

sapply(pkg, require, character.only = T)

```
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

df_dep <- function (m, x, yname, xname, lci = 0.25, uci = 0.75, delta = FALSE) 
{
  if (!any(class(m) %in% c("randomForest", "list"))) 
    stop("m is not a randomForest object")
  if (m$type != "regression") 
    stop("classification is not supported")
  conf.int <- (uci - lci) * 100
  temp <- sort(x[, xname])
  y.hat.mean <- vector()
  y.hat.lb <- vector()
  y.hat.ub <- vector()
  y <- stats::predict(m, x, what = 0.5)
  for (i in 1:length(temp)) {
    x[, xname] <- temp[i]
    y.hat <- stats::predict(m, x, what = mean)
    if (delta == TRUE) {
      y.hat <- y.hat - y
    }
    y.hat.mean[i] <- stats::weighted.mean(y.hat, na.rm = T)
    y.hat.lb[i] <- stats::quantile(y.hat, lci, na.rm = T)
    y.hat.ub[i] <- stats::quantile(y.hat, uci, na.rm = T)
  }
  m.ci <- as.data.frame(cbind(temp, y.hat.mean, y.hat.lb, 
                              y.hat.ub))
  names(m.ci) <- c(xname, yname, "lci", "uci")
  
  return(m.ci)
}



models <- list.files(path = "./results/img", pattern = ".RData$",
                     full.names = T)


pp = 2
h = 1
lg <- list()


for (pp in seq_along(models)) {
  
  t11 <- Sys.time()
  load(models[pp])
  
  lxvars <- names(dsel_rfe)[-1]
  lyvar <- names(dsel_rfe)[1]
  
  dfs <- list()
  for (h in seq_along(lxvars)) {
    
    t22 <- Sys.time()
    
    dfs[[h]] <- df_dep(m = fit$finalModel, x = dsel_rfe, yname = lyvar,
                       xname = lxvars[h], lci = 0.25, uci = 0.75)
    
    print(paste(lxvars[h], h, Sys.time() - t22))
    
    
    
  }
  
  lg[[pp]] <- dfs
  
  # save(lg, file = "./results/img_out/partial_dependency_median.RData")
  
  print(Sys.time() - t11)
  beep(3)
  
}


# save(lg, file = "./results/img_out/partial_dependency_median.RData")
```
# load --------------------------------------------------------------------
``` r
load("./results/img_out/partial_dependency_median.RData")

ggplot(lg[[1]][[1]], aes(y = rs, x = npp)) +
  
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/JW0FYCs.png)<!-- -->
  
  ``` r




for (i in seq_along(lg[[1]])) {
  
  d <- lg[[1]][[i]] %>% 
    gather(key = "var", value = "cci", -c(rs, lci, uci))
  
  if (i == 1){
    df <- d
    
    
  } else {
    
    df <- rbind(df, d)
    
  }
  
}

dfrs <- df

ggplot(df, aes(y = rs, x = cci)) +
  
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~var, scales = "free")
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/IzJELxN.png)<!-- -->
  
  ``` r










for (i in seq_along(lg[[2]])) {
  
  d <- lg[[2]][[i]] %>% 
    gather(key = "var", value = "cci", -c(rh, lci, uci))
  
  if (i == 1){
    df <- d
    
    
  } else {
    
    df <- rbind(df, d)
    
  }
  
}

dfrh <- df
ggplot(df, aes(y = rh, x = cci)) +
  
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~var, scales = "free")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](https://i.imgur.com/diubqxx.png)<!-- -->
  
  ``` r






dfrs <- dfrs %>% 
  gather(key = "tr", value = "vr", -c("var", "lci", "uci", "cci"))

dfrh <- dfrh %>% 
  gather(key = "tr", value = "vr", -c("var", "lci", "uci", "cci"))


dfg <- rbind(dfrs, dfrh) %>% 
  # filter(var %in% c("bio1", "bio6", "bio12", "bio13", "npp")) %>% 
  # mutate(tr = factor(recode(tr,
  #                           rs = "Rs~(g~C~m^{-2}~year^{-1})",
  #                           rh = "Rh~(g~C~m^{-2}~year^{-1})"),
  #                    levels = c("Rs~(g~C~m^{-2}~year^{-1})",
  #                               "Rh~(g~C~m^{-2}~year^{-1})")),
  #        var = factor(recode(var,
  #                            bio1 = "BIO~1~(ºC)",
  #                            bio6 = "BIO~6~(ºC)",
  #                            bio12 = "BIO~12~(mm~year^{-1})",
  #                            bio13 = "BIO~13~(kg~m^{-2})",
  #                            npp = "NPP~(g~C~m^{-2}~year^{-1})"),
  #                     levels = c("BIO~1~(ºC)", "BIO~6~(ºC)",
  #                                "BIO~12~(mm~year^{-1})",
  #                                "BIO~13~(kg~m^{-2})",
  #                                "NPP~(g~C~m^{-2}~year^{-1})"))) %>% 
  relocate(tr, var, lci, uci, vr)




gg_model_partial <- ggplot(dfg, aes(y = vr, x = cci)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = NULL, y = NULL) +
  facet_grid(rows = vars(tr), cols = vars(var), scales = "free",
             labeller = label_parsed, switch = "both") +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) ; gg_model_partial
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/VHcZO4z.png)<!-- -->
  
  ``` r




# ggsave(gg_model_partial, filename = "./figuras/gg_model_partial.jpg", dpi = 600,
#        width = 19, height = 6.718, units = "in")









# gg geral ----------------------------------------------------------------



unique(dfg$var)
#>  [1] "npp"             "bio12"           "bio4"            "bio3"           
#>  [5] "bio6"            "bio13"           "bio19"           "bio1"           
#>  [9] "clay_0_30cm"     "soc_sand_0_30cm"


dfg <- rbind(dfrs, dfrh) %>% 
  # filter(var %in% c("bio1", "bio6", "bio12", "bio13", "npp")) %>% 
  mutate(tr = factor(recode(tr,
                            rs = "Rs~(g~C~m^{-2}~year^{-1})",
                            rh = "Rh~(g~C~m^{-2}~year^{-1})"),
                     levels = c("Rs~(g~C~m^{-2}~year^{-1})",
                                "Rh~(g~C~m^{-2}~year^{-1})")),
         var = factor(recode(var,
                             bio1 = "BIO~1~(ºC)",
                             bio6 = "BIO~6~(ºC)",
                             bio12 = "BIO~12~(mm~year^{-1})",
                             bio13 = "BIO~13~(kg~m^{-2})",
                             bio19 = "BIO~19~(kg~m^{-2})",
                             bio3 = "BIO~3~(`%`)",
                             bio4 = "BIO~4~(`%`)",
                             clay_0_30cm = "Clay~(g~kg^-1)",
                             soc_sand_0_30cm = "atop(SOC/sand, (hg~C~~~kg~`sand`^-1))",
                             npp = "NPP~(g~C~m^{-2}~year^{-1})"),
                      levels = c("BIO~1~(ºC)", "BIO~3~(`%`)", "BIO~4~(`%`)",
                                 "BIO~6~(ºC)",
                                 "BIO~12~(mm~year^{-1})",
                                 "BIO~13~(kg~m^{-2})",
                                 "BIO~19~(kg~m^{-2})",
                                 "NPP~(g~C~m^{-2}~year^{-1})",
                                 "Clay~(g~kg^-1)",
                                 "atop(SOC/sand, (hg~C~~~kg~`sand`^-1))"))) %>% 
  relocate(tr, var, lci, uci, vr)



# rs ----------------------------------------------------------------------



gg_model_partial_rs <- ggplot(filter(dfg, tr == "Rs~(g~C~m^{-2}~year^{-1})"),
                              aes(y = vr, x = cci)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = NULL, y = expression(Rs~(g~C~m^{-2}~year^{-1}))) +
  facet_wrap(~var, scales = "free_x",
             labeller = label_parsed,
             nrow = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) ; gg_model_partial_rs
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/mGyngZg.png)<!-- -->
  
  ``` r




ggsave(gg_model_partial_rs, filename = "./figuras/gg_model_partial_rs_geral.jpg", dpi = 600,
       width = 19, height = 13.436, units = "in")
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'






# rh ----------------------------------------------------------------------


gg_model_partial_rh <- ggplot(filter(dfg, tr == "Rh~(g~C~m^{-2}~year^{-1})"),
                              aes(y = vr, x = cci)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
  geom_point() +
  geom_smooth(se = F) +
  labs(x = NULL, y = expression(Rh~(g~C~m^{-2}~year^{-1}))) +
  facet_wrap(~var, scales = "free_x",
             labeller = label_parsed,
             nrow = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(),
        strip.placement = "output",
        legend.position = "top",
        axis.ticks = element_line(color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, inherit.blank = T),
        panel.grid = element_line(colour = "grey90")) ; gg_model_partial_rh
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

![](https://i.imgur.com/5HW5a62.png)<!-- -->
  
  ``` r



ggsave(gg_model_partial_rh, filename = "./figuras/gg_model_partial_rh_geral.jpg", dpi = 600,
       width = 19, height = 13.436, units = "in")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'







# gg_dinamico -------------------------------------------------------------




unique(dfg$var)
#>  [1] NPP~(g~C~m^{-2}~year^{-1})            BIO~12~(mm~year^{-1})                
#>  [3] BIO~4~(`%`)                           BIO~3~(`%`)                          
#>  [5] BIO~6~(ºC)                            BIO~13~(kg~m^{-2})                   
#>  [7] BIO~19~(kg~m^{-2})                    BIO~1~(ºC)                           
#>  [9] Clay~(g~kg^-1)                        atop(SOC/sand, (hg~C~~~kg~`sand`^-1))
#> 10 Levels: BIO~1~(ºC) BIO~3~(`%`) BIO~4~(`%`) ... atop(SOC/sand, (hg~C~~~kg~`sand`^-1))


dfg <- rbind(dfrs, dfrh) %>% 
  # filter(var %in% c("bio1", "bio6", "bio12", "bio13", "npp")) %>% 
  mutate(tr = factor(recode(tr,
                            rs = "Rs~(g~C~m^{-2}~year^{-1})",
                            rh = "Rh~(g~C~m^{-2}~year^{-1})"),
                     levels = c("Rs~(g~C~m^{-2}~year^{-1})",
                                "Rh~(g~C~m^{-2}~year^{-1})"))) %>% 
  relocate(tr, var, lci, uci, vr)





vvars <- unique(dfg$var) %>% sort()
i = 1

for (i in seq_along(vvars)) {
  
  
  gg_model_partial <- ggplot(filter(dfg, var %in% vvars[i]), aes(y = vr, x = cci)) +
    geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.5) +
    geom_point() +
    geom_smooth(se = F) +
    labs(x = NULL, y = NULL) +
    facet_grid(rows = vars(tr), cols = vars(var), scales = "free",
               labeller = label_parsed, switch = "both") +
    theme(strip.background = element_blank(),
          strip.placement = "output",
          legend.position = "top",
          axis.ticks = element_line(color = "black"),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, inherit.blank = T),
          panel.grid = element_line(colour = "grey90")) 
  
  plot(gg_model_partial)
  
  
  
  
  ggsave(gg_model_partial,
         filename = paste0("./figuras/", vvars[i], ".jpg"), dpi = 600,
         width = 16, height = 11, units = "in")
  
  
}
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

![](https://i.imgur.com/MTVwKTD.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/kA6gcRS.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/LewgTkd.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/2e37aaW.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/ZAfbkMI.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/3N5Xu7e.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/hzxYnnH.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/U1ij7JB.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/YH20GFo.png)<!-- -->
  
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  #> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
  
  ![](https://i.imgur.com/TPKpa8B.png)<!-- -->
  
  
