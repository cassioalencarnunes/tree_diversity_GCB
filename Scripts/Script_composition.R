#### Script to run PERMANOVAs ####

library(vegan) # Version 2.6-2
library(dplyr)
library(pairwiseAdonis)
library(ggplot2)
source("Scripts/adonis_OmegaSq.R")

#### Loading Large PGM ####

data_plot_PGM <- read.csv("Data/data_plot_PGM.csv", h=T, row.names = 1)
data_plot_PGM$Degrad <- factor(data_plot_PGM$Degrad, levels = c("Undisturbed",
                                                                "Logged",
                                                                "Logged-and-burned",
                                                                "Secondary forest"))

levels(data_plot_PGM$Degrad) <- c("UF", "LF", "LBF", "SF")

data_plot_PGM$catchment <- substr(data_plot_PGM$Plot, 1, 3)
data_plot_PGM$catchment <- as.factor(data_plot_PGM$catchment)

## TD

large_PGM_TD_q0 <- read.csv("Data/Matrices_dissimilarity/large_PGM_TD_C_q0.csv", h=T, row.names = 1,
                                      check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_TD_q0 <- left_join(x = data_plot_PGM, y = large_PGM_TD_q0)

large_PGM_TD_q1 <- read.csv("Data/Matrices_dissimilarity/large_PGM_TD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_TD_q1 <- left_join(x = data_plot_PGM, y = large_PGM_TD_q1)

large_PGM_TD_q2 <- read.csv("Data/Matrices_dissimilarity/large_PGM_TD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_TD_q2 <- left_join(x = data_plot_PGM, y = large_PGM_TD_q2)


## FD

large_PGM_FD_q0 <- read.csv("Data/Matrices_dissimilarity/large_PGM_FD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_FD_q0 <- left_join(x = data_plot_PGM, y = large_PGM_FD_q0)

large_PGM_FD_q1 <- read.csv("Data/Matrices_dissimilarity/large_PGM_FD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_FD_q1 <- left_join(x = data_plot_PGM, y = large_PGM_FD_q1)

large_PGM_FD_q2 <- read.csv("Data/Matrices_dissimilarity/large_PGM_FD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_FD_q2 <- left_join(x = data_plot_PGM, y = large_PGM_FD_q2)

## PD

large_PGM_PD_q0 <- read.csv("Data/Matrices_dissimilarity/large_PGM_PD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_PD_q0 <- left_join(x = data_plot_PGM, y = large_PGM_PD_q0)

large_PGM_PD_q1 <- read.csv("Data/Matrices_dissimilarity/large_PGM_PD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_PD_q1 <- left_join(x = data_plot_PGM, y = large_PGM_PD_q1)

large_PGM_PD_q2 <- read.csv("Data/Matrices_dissimilarity/large_PGM_PD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_PGM_PD_q2 <- left_join(x = data_plot_PGM, y = large_PGM_PD_q2)

#### Loading Small PGM ####

## TD

small_PGM_TD_q0 <- read.csv("Data/Matrices_dissimilarity/small_PGM_TD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_TD_q0 <- left_join(x = data_plot_PGM, y = small_PGM_TD_q0)

small_PGM_TD_q1 <- read.csv("Data/Matrices_dissimilarity/small_PGM_TD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_TD_q1 <- left_join(x = data_plot_PGM, y = small_PGM_TD_q1)

small_PGM_TD_q2 <- read.csv("Data/Matrices_dissimilarity/small_PGM_TD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_TD_q2 <- left_join(x = data_plot_PGM, y = small_PGM_TD_q2)

small_PGM_TD_q0 <- read.csv("Data/Matrices_dissimilarity/small_PGM_TD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_TD_q0 <- left_join(x = data_plot_PGM, y = small_PGM_TD_q0)

## FD

small_PGM_FD_q0 <- read.csv("Data/Matrices_dissimilarity/small_PGM_FD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_FD_q0 <- left_join(x = data_plot_PGM, y = small_PGM_FD_q0)

small_PGM_FD_q1 <- read.csv("Data/Matrices_dissimilarity/small_PGM_FD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_FD_q1 <- left_join(x = data_plot_PGM, y = small_PGM_FD_q1)

small_PGM_FD_q2 <- read.csv("Data/Matrices_dissimilarity/small_PGM_FD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_FD_q2 <- left_join(x = data_plot_PGM, y = small_PGM_FD_q2)

## PD

small_PGM_PD_q0 <- read.csv("Data/Matrices_dissimilarity/small_PGM_PD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_PD_q0 <- left_join(x = data_plot_PGM, y = small_PGM_PD_q0)

small_PGM_PD_q1 <- read.csv("Data/Matrices_dissimilarity/small_PGM_PD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_PD_q1 <- left_join(x = data_plot_PGM, y = small_PGM_PD_q1)

small_PGM_PD_q2 <- read.csv("Data/Matrices_dissimilarity/small_PGM_PD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_PGM_PD_q2 <- left_join(x = data_plot_PGM, y = small_PGM_PD_q2)


#### Loading Large STM ####

data_plot_STM <- read.csv("Data/data_plot_STM.csv", h=T, row.names = 1)
data_plot_STM$Degrad <- factor(data_plot_STM$Degrad, levels = c("Undisturbed",
                                                                "Logged",
                                                                "Logged and burnt",
                                                                "Secondary forest"))
levels(data_plot_STM$Degrad) <- c("UF", "LF", "LBF", "SF")

data_plot_STM$catchment <- substr(data_plot_STM$Plot, 1, 3)
data_plot_STM$catchment <- as.factor(data_plot_STM$catchment)

## TD

large_STM_TD_q0 <- read.csv("Data/Matrices_dissimilarity/large_STM_TD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_TD_q0 <- left_join(x = data_plot_STM, y = large_STM_TD_q0)

large_STM_TD_q1 <- read.csv("Data/Matrices_dissimilarity/large_STM_TD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_TD_q1 <- left_join(x = data_plot_STM, y = large_STM_TD_q1)

large_STM_TD_q2 <- read.csv("Data/Matrices_dissimilarity/large_STM_TD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_TD_q2 <- left_join(x = data_plot_STM, y = large_STM_TD_q2)

large_STM_TD_q0 <- read.csv("Data/Matrices_dissimilarity/large_STM_TD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_TD_q0 <- left_join(x = data_plot_STM, y = large_STM_TD_q0)

## FD

large_STM_FD_q0 <- read.csv("Data/Matrices_dissimilarity/large_STM_FD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_FD_q0 <- left_join(x = data_plot_STM, y = large_STM_FD_q0)

large_STM_FD_q1 <- read.csv("Data/Matrices_dissimilarity/large_STM_FD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_FD_q1 <- left_join(x = data_plot_STM, y = large_STM_FD_q1)

large_STM_FD_q2 <- read.csv("Data/Matrices_dissimilarity/large_STM_FD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_FD_q2 <- left_join(x = data_plot_STM, y = large_STM_FD_q2)

## PD

large_STM_PD_q0 <- read.csv("Data/Matrices_dissimilarity/large_STM_PD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_PD_q0 <- left_join(x = data_plot_STM, y = large_STM_PD_q0)

large_STM_PD_q1 <- read.csv("Data/Matrices_dissimilarity/large_STM_PD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_PD_q1 <- left_join(x = data_plot_STM, y = large_STM_PD_q1)

large_STM_PD_q2 <- read.csv("Data/Matrices_dissimilarity/large_STM_PD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
large_STM_PD_q2 <- left_join(x = data_plot_STM, y = large_STM_PD_q2)

#### Loading Small STM ####

## TD

small_STM_TD_q0 <- read.csv("Data/Matrices_dissimilarity/small_STM_TD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_TD_q0 <- left_join(x = data_plot_STM, y = small_STM_TD_q0)

small_STM_TD_q1 <- read.csv("Data/Matrices_dissimilarity/small_STM_TD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_TD_q1 <- left_join(x = data_plot_STM, y = small_STM_TD_q1)

small_STM_TD_q2 <- read.csv("Data/Matrices_dissimilarity/small_STM_TD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_TD_q2 <- left_join(x = data_plot_STM, y = small_STM_TD_q2)

small_STM_TD_q0 <- read.csv("Data/Matrices_dissimilarity/small_STM_TD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_TD_q0 <- left_join(x = data_plot_STM, y = small_STM_TD_q0)

## FD

small_STM_FD_q0 <- read.csv("Data/Matrices_dissimilarity/small_STM_FD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_FD_q0 <- left_join(x = data_plot_STM, y = small_STM_FD_q0)

small_STM_FD_q1 <- read.csv("Data/Matrices_dissimilarity/small_STM_FD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_FD_q1 <- left_join(x = data_plot_STM, y = small_STM_FD_q1)

small_STM_FD_q2 <- read.csv("Data/Matrices_dissimilarity/small_STM_FD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_FD_q2 <- left_join(x = data_plot_STM, y = small_STM_FD_q2)

## PD

small_STM_PD_q0 <- read.csv("Data/Matrices_dissimilarity/small_STM_PD_C_q0.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_PD_q0 <- left_join(x = data_plot_STM, y = small_STM_PD_q0)

small_STM_PD_q1 <- read.csv("Data/Matrices_dissimilarity/small_STM_PD_C_q1.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_PD_q1 <- left_join(x = data_plot_STM, y = small_STM_PD_q1)

small_STM_PD_q2 <- read.csv("Data/Matrices_dissimilarity/small_STM_PD_C_q2.csv", h=T, row.names = 1,
                            check.names = F) %>% 
  tibble::rownames_to_column("Plot")
small_STM_PD_q2 <- left_join(x = data_plot_STM, y = small_STM_PD_q2)



#### PERMANOVAS ####

list_data <- list(large_PGM_TD_q0, large_PGM_TD_q1, large_PGM_TD_q2,
                  large_PGM_FD_q0, large_PGM_FD_q1, large_PGM_FD_q2,
                  large_PGM_PD_q0, large_PGM_PD_q1, large_PGM_PD_q2,
                  small_PGM_TD_q0, small_PGM_TD_q1, small_PGM_TD_q2,
                  small_PGM_FD_q0, small_PGM_FD_q1, small_PGM_FD_q2,
                  small_PGM_PD_q0, small_PGM_PD_q1, small_PGM_PD_q2,
                  large_STM_TD_q0, large_STM_TD_q1, large_STM_TD_q2,
                  large_STM_FD_q0, large_STM_FD_q1, large_STM_FD_q2,
                  large_STM_PD_q0, large_STM_PD_q1, large_STM_PD_q2,
                  small_STM_TD_q0, small_STM_TD_q1, small_STM_TD_q2,
                  small_STM_FD_q0, small_STM_FD_q1, small_STM_FD_q2,
                  small_STM_PD_q0, small_STM_PD_q1, small_STM_PD_q2)
names(list_data) <- c("large_PGM_TD_q0", "large_PGM_TD_q1", "large_PGM_TD_q2",
                      "large_PGM_FD_q0", "large_PGM_FD_q1", "large_PGM_FD_q2",
                      "large_PGM_PD_q0", "large_PGM_PD_q1", "large_PGM_PD_q2",
                      "small_PGM_TD_q0", "small_PGM_TD_q1", "small_PGM_TD_q2",
                      "small_PGM_FD_q0", "small_PGM_FD_q1", "small_PGM_FD_q2",
                      "small_PGM_PD_q0", "small_PGM_PD_q1", "small_PGM_PD_q2",
                      "large_STM_TD_q0", "large_STM_TD_q1", "large_STM_TD_q2",
                      "large_STM_FD_q0", "large_STM_FD_q1", "large_STM_FD_q2",
                      "large_STM_PD_q0", "large_STM_PD_q1", "large_STM_PD_q2",
                      "small_STM_TD_q0", "small_STM_TD_q1", "small_STM_TD_q2",
                      "small_STM_FD_q0", "small_STM_FD_q1", "small_STM_FD_q2",
                      "small_STM_PD_q0", "small_STM_PD_q1", "small_STM_PD_q2")


mod <- NULL
mod_pair <- NULL
mod_pair_final <- NULL
omega <- NULL


for (i in names(list_data)){
  mod <- pairwise.adonis2(x = as.dist(list_data[[i]][,-c(1:3)]) ~ Degrad + catchment,
                 nperm = 999, data = list_data[[i]])
  
  class(mod$LBF_vs_LF)
  class(mod)

  mod_pair <- pairwise.adonis(x = as.dist(list_data[[i]][,-c(1:3)]),
                              factors = list_data[[i]]$Degrad, perm = 999)
  
  if(stringr::str_detect(string = i, pattern = "PGM")){
  omega_degrad <- c(adonis_OmegaSq(mod$LBF_vs_LF)$parOmegaSq[1],
                    adonis_OmegaSq(mod$LBF_vs_SF)$parOmegaSq[1],
                    adonis_OmegaSq(mod$LBF_vs_UF)$parOmegaSq[1],
                    adonis_OmegaSq(mod$LF_vs_SF)$parOmegaSq[1],
                    adonis_OmegaSq(mod$LF_vs_UF)$parOmegaSq[1],
                    adonis_OmegaSq(mod$SF_vs_UF)$parOmegaSq[1])
  
  omega_catchment <- c(adonis_OmegaSq(mod$LBF_vs_LF)$parOmegaSq[2],
                       adonis_OmegaSq(mod$LBF_vs_SF)$parOmegaSq[2],
                       adonis_OmegaSq(mod$LBF_vs_UF)$parOmegaSq[2],
                       adonis_OmegaSq(mod$LF_vs_SF)$parOmegaSq[2],
                       adonis_OmegaSq(mod$LF_vs_UF)$parOmegaSq[2],
                       adonis_OmegaSq(mod$SF_vs_UF)$parOmegaSq[2])
  }
  
  else{
    omega_degrad <- c(adonis_OmegaSq(mod$SF_vs_LF)$parOmegaSq[1],
                      adonis_OmegaSq(mod$SF_vs_LBF)$parOmegaSq[1],
                      adonis_OmegaSq(mod$SF_vs_UF)$parOmegaSq[1],
                      adonis_OmegaSq(mod$LF_vs_LBF)$parOmegaSq[1],
                      adonis_OmegaSq(mod$LF_vs_UF)$parOmegaSq[1],
                      adonis_OmegaSq(mod$LBF_vs_UF)$parOmegaSq[1])
    
    omega_catchment <- c(adonis_OmegaSq(mod$SF_vs_LF)$parOmegaSq[2],
                         adonis_OmegaSq(mod$SF_vs_LBF)$parOmegaSq[2],
                         adonis_OmegaSq(mod$SF_vs_UF)$parOmegaSq[2],
                         adonis_OmegaSq(mod$LF_vs_LBF)$parOmegaSq[2],
                         adonis_OmegaSq(mod$LF_vs_UF)$parOmegaSq[2],
                         adonis_OmegaSq(mod$LBF_vs_UF)$parOmegaSq[2])}
  
  mod_pair <- mod_pair %>% 
    mutate(model = i, .before = pairs) %>% 
    mutate(OmegaSq_degrad = omega_degrad, .before = p.value) %>% 
    mutate(OmegaSq_catchment = omega_catchment, .before = p.value)
  
  mod_pair_final <- rbind(mod_pair_final, mod_pair)
  
  mod <- NULL
  mod_pair <- NULL

}


mod_pair_final <- mod_pair_final %>% 
  mutate(stem = stringr::str_sub(mod_pair_final$model, start = 1, end = 5),
         .before = model) %>%
  mutate(region = stringr::str_sub(mod_pair_final$model, start = 7, end = 9),
         .before = model) %>% 
  mutate(diversity = stringr::str_sub(mod_pair_final$model, start = 11, end = 12),
         .before = model) %>%
  mutate(q_value = stringr::str_sub(mod_pair_final$model, start = 14, end = 15))

mod_pair_final$pairs <- as.factor(mod_pair_final$pairs)
levels(mod_pair_final$pairs)

levels(mod_pair_final$pairs) <- c("LF vs LBF", "LBF vs SF", "UF vs LBF", "LF vs LBF", 
                            "LF vs SF", "UF vs LF", "LBF vs SF", "LF vs SF", "UF vs SF")

write.csv(x = mod_pair_final, file = "Results/effect_size_permanova.csv")
