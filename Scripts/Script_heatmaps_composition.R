# Libraries
library(tidyverse)
library(dplyr)
library(stringr)


##### Loading data ######

#### Large PGM ####

large_q0_PGM_TD <- read.csv("Data/Matrices_averages/large_PGM_TD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q1_PGM_TD <- read.csv("Data/Matrices_averages/large_PGM_TD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q2_PGM_TD <- read.csv("Data/Matrices_averages/large_PGM_TD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

large_q0_PGM_FD <- read.csv("Data/Matrices_averages/large_PGM_FD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q1_PGM_FD <- read.csv("Data/Matrices_averages/large_PGM_FD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q2_PGM_FD <- read.csv("Data/Matrices_averages/large_PGM_FD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

large_q0_PGM_PD <- read.csv("Data/Matrices_averages/large_PGM_PD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q1_PGM_PD <- read.csv("Data/Matrices_averages/large_PGM_PD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q2_PGM_PD <- read.csv("Data/Matrices_averages/large_PGM_PD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")


large_PGM <- rbind(large_q0_PGM_TD, large_q0_PGM_FD, large_q0_PGM_PD,
                   large_q1_PGM_TD, large_q1_PGM_FD, large_q1_PGM_PD,
                   large_q2_PGM_TD, large_q2_PGM_FD, large_q2_PGM_PD)


large_PGM$Diversity <- rep(rep(c("TD", "FD", "PD"), each = 16), 3)
large_PGM$q <- rep(c("q0", "q1", "q2"), each = 16*3)

large_PGM <- na.omit(large_PGM)

large_PGM$Comparison2 <- paste(large_PGM$X, large_PGM$comparison, sep = "_")

large_PGM2 <- large_PGM %>%
  pivot_wider(id_cols = c("Comparison2","q"), names_from = Diversity, values_from = dissimilarity)

large_PGM2$Comparison2 <- factor(large_PGM2$Comparison2, levels=c("UF_UF", "LF_UF", "LBF_UF", 
                                                                  "LF_LF", "LBF_LF", "LBF_LBF",
                                                                  "SF_SF", "SF_LBF", "SF_LF",
                                                                  "SF_UF"))
large_PGM$X <- factor(large_PGM$X, levels = c("UF", "LF", "LBF", "SF"))
large_PGM$comparison <- factor(large_PGM$comparison, levels = c("UF", "LF", "LBF", "SF"))
large_PGM$Diversity <- factor(large_PGM$Diversity, levels = c("TD", "FD", "PD"))

summary(large_PGM)
# Plot

#### Plots ####

library(ggplot2)
library(stringr)

#### Heat maps ####
p_large_PGM <- large_PGM %>% 
  ggplot(aes(x = X, y = comparison, fill = dissimilarity)) +
  geom_tile() +
  geom_text(aes(label = round(dissimilarity, 2)), size = 3) +  # <- This adds the values
  facet_grid(rows = vars(Diversity), cols = vars(q)) +
  scale_fill_gradient(high = 'red', low = "#f5f5f5", limits = c(0,1)) +
  labs(x = "Forest class", y = "Forest class", fill = "Dissimilarity") +
  ggtitle("Large trees in Paragominas", ) +
  theme_classic()


#### Small PGM ####

small_q0_PGM_TD <- read.csv("Data/Matrices_averages/small_PGM_TD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q1_PGM_TD <- read.csv("Data/Matrices_averages/small_PGM_TD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q2_PGM_TD <- read.csv("Data/Matrices_averages/small_PGM_TD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

small_q0_PGM_FD <- read.csv("Data/Matrices_averages/small_PGM_FD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q1_PGM_FD <- read.csv("Data/Matrices_averages/small_PGM_FD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q2_PGM_FD <- read.csv("Data/Matrices_averages/small_PGM_FD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

small_q0_PGM_PD <- read.csv("Data/Matrices_averages/small_PGM_PD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q1_PGM_PD <- read.csv("Data/Matrices_averages/small_PGM_PD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q2_PGM_PD <- read.csv("Data/Matrices_averages/small_PGM_PD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")


small_PGM <- rbind(small_q0_PGM_TD, small_q0_PGM_FD, small_q0_PGM_PD,
                   small_q1_PGM_TD, small_q1_PGM_FD, small_q1_PGM_PD,
                   small_q2_PGM_TD, small_q2_PGM_FD, small_q2_PGM_PD)


small_PGM$Diversity <- rep(rep(c("TD", "FD", "PD"), each = 16), 3)
small_PGM$q <- rep(c("q0", "q1", "q2"), each = 16*3)

small_PGM <- na.omit(small_PGM)

small_PGM$Comparison2 <- paste(small_PGM$X, small_PGM$comparison, sep = "_")

small_PGM2 <- small_PGM %>%
  pivot_wider(id_cols = c("Comparison2","q"), names_from = Diversity, values_from = dissimilarity)

small_PGM2$Comparison2 <- factor(small_PGM2$Comparison2, levels=c("UF_UF", "LF_UF", "LBF_UF", 
                                                                  "LF_LF", "LBF_LF", "LBF_LBF",
                                                                  "SF_SF", "SF_LBF", "SF_LF",
                                                                  "SF_UF"))
small_PGM$X <- factor(small_PGM$X, levels = c("UF", "LF", "LBF", "SF"))
small_PGM$comparison <- factor(small_PGM$comparison, levels = c("UF", "LF", "LBF", "SF"))
small_PGM$Diversity <- factor(small_PGM$Diversity, levels = c("TD", "FD", "PD"))

summary(small_PGM)
# Plot

#### Heat maps ####
p_small_PGM <- small_PGM %>% 
  ggplot(aes(x = X, y = comparison, fill = dissimilarity)) +
  geom_tile() +
  geom_text(aes(label = round(dissimilarity, 2)), size = 3) +  # <- This adds the values
  facet_grid(rows = vars(Diversity), cols = vars(q)) +
  scale_fill_gradient(high = 'red', low = "#f5f5f5", limits = c(0,1)) +
  labs(x = "Forest class", y = "Forest class", fill = "Dissimilarity") +
  ggtitle("Small trees in Paragominas", ) +
  theme_classic()


#### Large STM ####

large_q0_STM_TD <- read.csv("Data/Matrices_averages/large_STM_TD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q1_STM_TD <- read.csv("Data/Matrices_averages/large_STM_TD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q2_STM_TD <- read.csv("Data/Matrices_averages/large_STM_TD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

large_q0_STM_FD <- read.csv("Data/Matrices_averages/large_STM_FD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q1_STM_FD <- read.csv("Data/Matrices_averages/large_STM_FD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q2_STM_FD <- read.csv("Data/Matrices_averages/large_STM_FD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

large_q0_STM_PD <- read.csv("Data/Matrices_averages/large_STM_PD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q1_STM_PD <- read.csv("Data/Matrices_averages/large_STM_PD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
large_q2_STM_PD <- read.csv("Data/Matrices_averages/large_STM_PD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")


large_STM <- rbind(large_q0_STM_TD, large_q0_STM_FD, large_q0_STM_PD,
                   large_q1_STM_TD, large_q1_STM_FD, large_q1_STM_PD,
                   large_q2_STM_TD, large_q2_STM_FD, large_q2_STM_PD)


large_STM$Diversity <- rep(rep(c("TD", "FD", "PD"), each = 16), 3)
large_STM$q <- rep(c("q0", "q1", "q2"), each = 16*3)

large_STM <- na.omit(large_STM)

large_STM$Comparison2 <- paste(large_STM$X, large_STM$comparison, sep = "_")

large_STM2 <- large_STM %>%
  pivot_wider(id_cols = c("Comparison2","q"), names_from = Diversity, values_from = dissimilarity)

large_STM2$Comparison2 <- factor(large_STM2$Comparison2, levels=c("UF_UF", "LF_UF", "LBF_UF", 
                                                                  "LF_LF", "LBF_LF", "LBF_LBF",
                                                                  "SF_SF", "SF_LBF", "SF_LF",
                                                                  "SF_UF"))
large_STM$X <- factor(large_STM$X, levels = c("UF", "LF", "LBF", "SF"))
large_STM$comparison <- factor(large_STM$comparison, levels = c("UF", "LF", "LBF", "SF"))
large_STM$Diversity <- factor(large_STM$Diversity, levels = c("TD", "FD", "PD"))

summary(large_STM)
# Plot

#### Heat maps ####
p_large_STM <- large_STM %>% 
  ggplot(aes(x = X, y = comparison, fill = dissimilarity)) +
  geom_tile() +
  geom_text(aes(label = round(dissimilarity, 2)), size = 3) +  # <- This adds the values
  facet_grid(rows = vars(Diversity), cols = vars(q)) +
  scale_fill_gradient(high = 'red', low = "#f5f5f5", limits = c(0,1)) +
  labs(x = "Forest class", y = "Forest class", fill = "Dissimilarity") +
  ggtitle("Large trees in Santarém", ) +
  theme_classic()


#### Small STM ####

small_q0_STM_TD <- read.csv("Data/Matrices_averages/small_STM_TD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q1_STM_TD <- read.csv("Data/Matrices_averages/small_STM_TD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q2_STM_TD <- read.csv("Data/Matrices_averages/small_STM_TD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

small_q0_STM_FD <- read.csv("Data/Matrices_averages/small_STM_FD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q1_STM_FD <- read.csv("Data/Matrices_averages/small_STM_FD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q2_STM_FD <- read.csv("Data/Matrices_averages/small_STM_FD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")

small_q0_STM_PD <- read.csv("Data/Matrices_averages/small_STM_PD_q0.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q1_STM_PD <- read.csv("Data/Matrices_averages/small_STM_PD_q1.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")
small_q2_STM_PD <- read.csv("Data/Matrices_averages/small_STM_PD_q2.csv") %>% 
  pivot_longer(!X, names_to = "comparison", values_to = "dissimilarity")


small_STM <- rbind(small_q0_STM_TD, small_q0_STM_FD, small_q0_STM_PD,
                   small_q1_STM_TD, small_q1_STM_FD, small_q1_STM_PD,
                   small_q2_STM_TD, small_q2_STM_FD, small_q2_STM_PD)


small_STM$Diversity <- rep(rep(c("TD", "FD", "PD"), each = 16), 3)
small_STM$q <- rep(c("q0", "q1", "q2"), each = 16*3)

small_STM <- na.omit(small_STM)

small_STM$Comparison2 <- paste(small_STM$X, small_STM$comparison, sep = "_")

small_STM2 <- small_STM %>%
  pivot_wider(id_cols = c("Comparison2","q"), names_from = Diversity, values_from = dissimilarity)

small_STM2$Comparison2 <- factor(small_STM2$Comparison2, levels=c("UF_UF", "LF_UF", "LBF_UF", 
                                                                  "LF_LF", "LBF_LF", "LBF_LBF",
                                                                  "SF_SF", "SF_LBF", "SF_LF",
                                                                  "SF_UF"))
small_STM$X <- factor(small_STM$X, levels = c("UF", "LF", "LBF", "SF"))
small_STM$comparison <- factor(small_STM$comparison, levels = c("UF", "LF", "LBF", "SF"))
small_STM$Diversity <- factor(small_STM$Diversity, levels = c("TD", "FD", "PD"))

summary(small_STM)
# Plot

#### Heat maps ####
p_small_STM <- small_STM %>% 
  ggplot(aes(x = X, y = comparison, fill = dissimilarity)) +
  geom_tile() +
  geom_text(aes(label = round(dissimilarity, 2)), size = 3) +  # <- This adds the values
  facet_grid(rows = vars(Diversity), cols = vars(q)) +
  scale_fill_gradient(high = 'red', low = "#f5f5f5", limits = c(0,1)) +
  labs(x = "Forest class", y = "Forest class", fill = "Dissimilarity") +
  ggtitle("Small trees in Santarém", ) +
  theme_classic()