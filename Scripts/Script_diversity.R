#### Script to run analyses of diversity by forest types (TD, FD, PD, q0, q1, q2) ####

library(multcomp)
library(dplyr)
library(lme4)
library(DHARMa)
library(performance)

## Loading tables

data <- read.csv("Data/diversity_all_inext3d.csv", h= T)

data$Degrad<-as.factor(data$Degrad)


data$Degrad<-factor(data$Degrad, levels=c("Undisturbed", "Logged", "Logged-and-burned",
                                          "Logged and burnt","Secondary forest"))

#Changing the labels
data$Degrad <- plyr::mapvalues(data$Degrad, from = c("Undisturbed", "Logged", 
                                                     "Logged-and-burned", 
                                                     "Logged and burnt","Secondary forest"), 
                         to = c("UF", "LF", "LBF", "LBF", "SF"))

data$catchment <- substr(data$Plot, 1, 3)
data$catchment <- as.factor(data$catchment)

data <- data %>%
  group_by(Region, Size, Order.q) %>% 
  mutate(qD_s = as.vector(scale(qD, center = T, scale = T))) %>% 
  mutate(qAUC_s = as.vector(scale(qAUC, center = T, scale = T))) %>% 
  mutate(qPD_s = as.vector(scale(qPD, center = T, scale = T))) %>% 
  ungroup()

#### 1 PGM ####
#### 1.1 Large trees ####
#### 1.1.1 Taxonomic ####

# q0

TD_q0_large_PGM <- lmer(qD_s ~ Degrad + (1|catchment), 
                        data = dplyr::filter(data, Region == "PGM", 
                                             Size == "large", 
                                             Order.q == 0))
summary(TD_q0_large_PGM)

model_checking_large_PGM <- simulateResiduals(TD_q0_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
plotQQunif(model_checking_large_PGM)

car::Anova(TD_q0_large_PGM)
summary(glht(TD_q0_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q0_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 0))

# q1

TD_q1_large_PGM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large",
                                        Order.q == 1))

model_checking_large_PGM <- simulateResiduals(TD_q1_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 1)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(TD_q1_large_PGM)
summary(glht(TD_q1_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q1_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 1))

# q2

TD_q2_large_PGM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large",
                                        Order.q == 2))

model_checking_large_PGM <- simulateResiduals(TD_q2_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 2)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(TD_q2_large_PGM)
summary(glht(TD_q2_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q2_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 2))

#### 1.1.2 Functional ####

# q0

FD_q0_large_PGM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 0))

model_checking_large_PGM <- simulateResiduals(FD_q0_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 0)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(FD_q0_large_PGM)
summary(glht(FD_q0_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q0_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 0))

# q1

FD_q1_large_PGM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large",
                                        Order.q == 1))

model_checking_large_PGM <- simulateResiduals(FD_q1_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 1)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(FD_q1_large_PGM)
summary(glht(FD_q1_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q1_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 1))

# q2

FD_q2_large_PGM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large",
                                        Order.q == 2))

model_checking_large_PGM <- simulateResiduals(FD_q2_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 2)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(FD_q2_large_PGM)
summary(glht(FD_q2_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q2_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 2))


#### 1.1.3 Phylogenetic ####

# q0

PD_q0_large_PGM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 0))

model_checking_large_PGM <- simulateResiduals(PD_q0_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 0)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(PD_q0_large_PGM)
summary(glht(PD_q0_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q0_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 0))

# q1

PD_q1_large_PGM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large",
                                        Order.q == 1))

model_checking_large_PGM <- simulateResiduals(PD_q1_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 1)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(PD_q1_large_PGM)
summary(glht(PD_q1_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q1_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 1))

# q2

PD_q2_large_PGM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "large",
                                        Order.q == 2))

model_checking_large_PGM <- simulateResiduals(PD_q2_large_PGM, plot = T)
testDispersion(model_checking_large_PGM)
testCategorical(model_checking_large_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 2)$Degrad)
plotQQunif(model_checking_large_PGM)

car::Anova(PD_q2_large_PGM)
summary(glht(PD_q2_large_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q2_large_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "large", Order.q == 2))

#### 1.2 Small trees ####
#### 1.2.1 Taxonomic ####

# q0

TD_q0_small_PGM <- lmer(qD_s ~ Degrad + (1|catchment),
                         data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0))

model_checking_small_PGM <- simulateResiduals(TD_q0_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = as.vector(dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0)$Degrad))
plotQQunif(model_checking_small_PGM)

car::Anova(TD_q0_small_PGM)
summary(glht(TD_q0_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q0_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0))

# q1

TD_q1_small_PGM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small",
                                        Order.q == 1))

model_checking_small_PGM <- simulateResiduals(TD_q1_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 1)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(TD_q1_small_PGM)
summary(glht(TD_q1_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q1_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 1))

# q2

TD_q2_small_PGM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small",
                                        Order.q == 2))

model_checking_small_PGM <- simulateResiduals(TD_q2_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 2)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(TD_q2_small_PGM)
summary(glht(TD_q2_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q2_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 2))


#### 1.2.1 Functional ####

# q0

FD_q0_small_PGM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0))

model_checking_small_PGM <- simulateResiduals(FD_q0_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(FD_q0_small_PGM)
summary(glht(FD_q0_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q0_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0))

# q1

FD_q1_small_PGM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small",
                                        Order.q == 1))

model_checking_small_PGM <- simulateResiduals(FD_q1_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 1)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(FD_q1_small_PGM)
summary(glht(FD_q1_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q1_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 1))

# q2

FD_q2_small_PGM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small",
                                        Order.q == 2))

model_checking_small_PGM <- simulateResiduals(FD_q2_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 2)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(FD_q2_small_PGM)
summary(glht(FD_q2_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q2_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 2))


#### 1.2.2 Phylogenetic ####

# q0

PD_q0_small_PGM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0))

model_checking_small_PGM <- simulateResiduals(PD_q0_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(PD_q0_small_PGM)
summary(glht(PD_q0_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q0_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 0))

# q1

PD_q1_small_PGM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small",
                                        Order.q == 1))

model_checking_small_PGM <- simulateResiduals(PD_q1_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 1)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(PD_q1_small_PGM)
summary(glht(PD_q1_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q1_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 1))

# q2

PD_q2_small_PGM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "PGM", Size == "small",
                                        Order.q == 2))

model_checking_small_PGM <- simulateResiduals(PD_q2_small_PGM, plot = T)
testDispersion(model_checking_small_PGM)
testCategorical(model_checking_small_PGM,
                catPred = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 2)$Degrad)
plotQQunif(model_checking_small_PGM)

car::Anova(PD_q2_small_PGM)
summary(glht(PD_q2_small_PGM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q2_small_PGM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "PGM", Size == "small", Order.q == 2))


#### 2 STM ####
#### 2.1 Large trees ####
#### 2.1.1 Taxonomic ####

# q0

TD_q0_large_STM <- lmer(qD_s ~ Degrad + (1|catchment), 
                        data = dplyr::filter(data, Region == "STM", 
                                             Size == "large", 
                                             Order.q == 0))
summary(TD_q0_large_STM)

model_checking_large_STM <- simulateResiduals(TD_q0_large_STM, plot = T)
testDispersion(model_checking_large_STM)
plotQQunif(model_checking_large_STM)

car::Anova(TD_q0_large_STM)
summary(glht(TD_q0_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q0_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 0))

# q1

TD_q1_large_STM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large",
                                        Order.q == 1))

model_checking_large_STM <- simulateResiduals(TD_q1_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 1)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(TD_q1_large_STM)
summary(glht(TD_q1_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q1_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 1))

# q2

TD_q2_large_STM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large",
                                        Order.q == 2))

model_checking_large_STM <- simulateResiduals(TD_q2_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 2)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(TD_q2_large_STM)
summary(glht(TD_q2_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q2_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 2))

#### 1.1.2 Functional ####

# q0

FD_q0_large_STM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large", Order.q == 0))

model_checking_large_STM <- simulateResiduals(FD_q0_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 0)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(FD_q0_large_STM)
summary(glht(FD_q0_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q0_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 0))

# q1

FD_q1_large_STM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large",
                                        Order.q == 1))

model_checking_large_STM <- simulateResiduals(FD_q1_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 1)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(FD_q1_large_STM)
summary(glht(FD_q1_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q1_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 1))

# q2

FD_q2_large_STM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large",
                                        Order.q == 2))

model_checking_large_STM <- simulateResiduals(FD_q2_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 2)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(FD_q2_large_STM)
summary(glht(FD_q2_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q2_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 2))


#### 1.1.3 Phylogenetic ####

# q0

PD_q0_large_STM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large", Order.q == 0))

model_checking_large_STM <- simulateResiduals(PD_q0_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 0)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(PD_q0_large_STM)
summary(glht(PD_q0_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q0_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 0))

# q1

PD_q1_large_STM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large",
                                        Order.q == 1))

model_checking_large_STM <- simulateResiduals(PD_q1_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 1)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(PD_q1_large_STM)
summary(glht(PD_q1_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q1_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 1))

# q2

PD_q2_large_STM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "large",
                                        Order.q == 2))

model_checking_large_STM <- simulateResiduals(PD_q2_large_STM, plot = T)
testDispersion(model_checking_large_STM)
testCategorical(model_checking_large_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 2)$Degrad)
plotQQunif(model_checking_large_STM)

car::Anova(PD_q2_large_STM)
summary(glht(PD_q2_large_STM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q2_large_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "large", Order.q == 2))

#### 1.2 Small trees ####
#### 1.2.1 Taxonomic ####

# q0

TD_q0_small_STM <- lmer(qD_s ~ Degrad + (1|catchment),
                        data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0))

model_checking_small_STM <- simulateResiduals(TD_q0_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = as.vector(dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0)$Degrad))
plotQQunif(model_checking_small_STM)

car::Anova(TD_q0_small_STM)
summary(glht(TD_q0_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q0_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0))

# q1

TD_q1_small_STM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small",
                                        Order.q == 1))

model_checking_small_STM <- simulateResiduals(TD_q1_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 1)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(TD_q1_small_STM)
summary(glht(TD_q1_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q1_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 1))

# q2

TD_q2_small_STM <- lmer(qD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small",
                                        Order.q == 2))

model_checking_small_STM <- simulateResiduals(TD_q2_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 2)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(TD_q2_small_STM)
summary(glht(TD_q2_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(TD_q2_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 2))


#### 1.2.1 Functional ####

# q0

FD_q0_small_STM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0))

model_checking_small_STM <- simulateResiduals(FD_q0_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(FD_q0_small_STM)
summary(glht(FD_q0_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q0_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0))

# q1

FD_q1_small_STM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small",
                                        Order.q == 1))

model_checking_small_STM <- simulateResiduals(FD_q1_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 1)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(FD_q1_small_STM)
summary(glht(FD_q1_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q1_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 1))

# q2

FD_q2_small_STM <- lmer(qAUC_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small",
                                        Order.q == 2))

model_checking_small_STM <- simulateResiduals(FD_q2_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 2)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(FD_q2_small_STM)
summary(glht(FD_q2_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(FD_q2_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qAUC_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 2))


#### 1.2.2 Phylogenetic ####

# q0

PD_q0_small_STM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0))

model_checking_small_STM <- simulateResiduals(PD_q0_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(PD_q0_small_STM)
summary(glht(PD_q0_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q0_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 0))

# q1

PD_q1_small_STM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small",
                                        Order.q == 1))

model_checking_small_STM <- simulateResiduals(PD_q1_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 1)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(PD_q1_small_STM)
summary(glht(PD_q1_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q1_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 1))

# q2

PD_q2_small_STM <- lmer(qPD_s ~ Degrad + (1|catchment), data = 
                          dplyr::filter(data, Region == "STM", Size == "small",
                                        Order.q == 2))

model_checking_small_STM <- simulateResiduals(PD_q2_small_STM, plot = T)
testDispersion(model_checking_small_STM)
testCategorical(model_checking_small_STM,
                catPred = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 2)$Degrad)
plotQQunif(model_checking_small_STM)

car::Anova(PD_q2_small_STM)
summary(glht(PD_q2_small_STM, mcp(Degrad="Tukey")))
cld(summary(glht(PD_q2_small_STM, mcp(Degrad="Tukey"))), decreasing = F)

plot(qPD_s~Degrad, data = dplyr::filter(data, Region == "STM", Size == "small", Order.q == 2))


#### Joining effect sizes ####

efsize_final <- NULL
anova_test_pvalue_final <- NULL

models <- c("TD_q0_large_PGM", "TD_q1_large_PGM", "TD_q2_large_PGM",
               "FD_q0_large_PGM", "FD_q1_large_PGM", "FD_q2_large_PGM",
               "PD_q0_large_PGM", "PD_q1_large_PGM", "PD_q2_large_PGM",
               "TD_q0_large_STM", "TD_q1_large_STM", "TD_q2_large_STM",
               "FD_q0_large_STM", "FD_q1_large_STM", "FD_q2_large_STM",
               "PD_q0_large_STM", "PD_q1_large_STM", "PD_q2_large_STM",
               "TD_q0_small_PGM", "TD_q1_small_PGM", "TD_q2_small_PGM",
               "FD_q0_small_PGM", "FD_q1_small_PGM", "FD_q2_small_PGM",
               "PD_q0_small_PGM", "PD_q1_small_PGM", "PD_q2_small_PGM",
               "TD_q0_small_STM", "TD_q1_small_STM", "TD_q2_small_STM",
               "FD_q0_small_STM", "FD_q1_small_STM", "FD_q2_small_STM",
               "PD_q0_small_STM", "PD_q1_small_STM", "PD_q2_small_STM")

for (i in models){
  
  anova_test <- car::Anova(get(i))
  anova_test_pvalue <- as.data.frame(anova_test$`Pr(>Chisq)`)
  anova_test_pvalue$model <- i
  
  contrast_LU <- multcomp::glht(get(i), linfct = mcp(Degrad="Tukey"))
  contrast_LU_test <- summary(contrast_LU)
  contrast_LU_ci <- confint(contrast_LU, level = 0.95)
  
  efsize <- as.data.frame(contrast_LU_ci$confint)
  efsize$p_value <- contrast_LU_test$test$pvalues
  efsize$std_error <- contrast_LU_test$test$sigma
  efsize$model <- i
  
  efsize_final <- rbind(efsize_final, efsize)
  anova_test_pvalue_final <- rbind(anova_test_pvalue_final, anova_test_pvalue)
  
  efsize <- NULL
  anova_test <- NULL
  anova_test_pvalue <- NULL

}

write.csv(file = "Results/efsize_diversity.csv", x = efsize_final)


#### Plotting the box plots ####
library(ggplot2)

data2 <- tidyr::pivot_longer(data = data, cols = c("qD", "qAUC", "qPD"), names_to = "Diversity_type",
                             values_to = "Diversity_value")

data2$Diversity_type <- factor(data2$Diversity_type, levels = c("qD", "qAUC", "qPD"))
data2$Order.q <- as.factor(data2$Order.q)
data2$Size <- as.factor(data2$Size)
data2$Order.q <- paste("q", data2$Order.q, sep ="")

#### PGM ####

## Large

list_model_large_PGM <- list(TD_q0_large_PGM, TD_q1_large_PGM, TD_q2_large_PGM,
                             FD_q0_large_PGM, FD_q1_large_PGM, FD_q2_large_PGM,
                             PD_q0_large_PGM, PD_q1_large_PGM, PD_q2_large_PGM)
names(list_model_large_PGM) <- c("TD_q0", "TD_q1", "TD_q2",
                                 "FD_q0", "FD_q1", "FD_q2",
                                 "PD_q0", "PD_q1", "PD_q2")
letters_final <- NULL
for (i in names(list_model_large_PGM)){
  letters <- cld(summary(glht(list_model_large_PGM[[i]], mcp(Degrad="Tukey"))), decreasing = F)
  letters <- letters$mcletters$Letters
  letters <- cbind(t(letters), substr(i, start = 1, stop = 2), 
                   substr(i, start = 4, stop = 5))
  letters_final <- rbind(letters_final, letters)
}

letters_final <- as.data.frame(cbind(letters_final, data2 %>% 
                                       dplyr::filter(Region == "PGM", Size == "large") %>%
                                       group_by(Diversity_type, Order.q) %>% 
                                       summarise("max_y" = max(Diversity_value))))
letters_final <- tidyr::pivot_longer(data = letters_final, cols = c("UF", "LF", "LBF", "SF"), 
                                     values_to = "lab", names_to = "x_value") %>% 
  select(!c("V5","V6"))

letters_final[letters_final$Diversity_type=="qD", "max_y"] <- letters_final[letters_final$Diversity_type=="qD", "max_y"]+4

plot_large_PGM <- data2 %>% 
  dplyr::filter(Region == "PGM", Size == "large") %>% 
  ggplot(aes(x = Degrad, y = Diversity_value)) +
  geom_boxplot(aes(fill = Degrad), width=0.5, color="black") +
  scale_fill_manual(values = c("#006d2c", "#2ca25f", "#66c2a4", "#b2e2e2")) +
  # ylim(0,90)+
  labs(x = "Forest Class", y = "Alpha-diversity") +
  geom_text(data = letters_final, aes(x = x_value,  y = max_y+2, label = lab)) +
  facet_grid(rows = vars(Diversity_type), cols = vars(Order.q), scales = "free") +
  ggtitle("Large trees in PGM") +
  theme_classic() +
  theme(legend.position = "none")

## Small

list_model_small_PGM <- list(TD_q0_small_PGM, TD_q1_small_PGM, TD_q2_small_PGM,
                             FD_q0_small_PGM, FD_q1_small_PGM, FD_q2_small_PGM,
                             PD_q0_small_PGM, PD_q1_small_PGM, PD_q2_small_PGM)
names(list_model_small_PGM) <- c("TD_q0", "TD_q1", "TD_q2",
                                 "FD_q0", "FD_q1", "FD_q2",
                                 "PD_q0", "PD_q1", "PD_q2")
letters_final <- NULL
for (i in names(list_model_small_PGM)){
  letters <- cld(summary(glht(list_model_small_PGM[[i]], mcp(Degrad="Tukey"))), decreasing = F)
  letters <- letters$mcletters$Letters
  letters <- cbind(t(letters), substr(i, start = 1, stop = 2), 
                   substr(i, start = 4, stop = 5))
  letters_final <- rbind(letters_final, letters)
}

letters_final <- as.data.frame(cbind(letters_final, data2 %>% 
                                       dplyr::filter(Region == "PGM", Size == "small") %>%
                                       group_by(Diversity_type, Order.q) %>% 
                                       summarise("max_y" = max(Diversity_value))))
letters_final <- tidyr::pivot_longer(data = letters_final, cols = c("UF", "LF", "LBF", "SF"), 
                                     values_to = "lab", names_to = "x_value") %>% 
  select(!c("V5","V6"))

letters_final[letters_final$Diversity_type=="qD", "max_y"] <- letters_final[letters_final$Diversity_type=="qD", "max_y"]+4

plot_small_PGM <- data2 %>% 
  dplyr::filter(Region == "PGM", Size == "small") %>% 
  ggplot(aes(x = Degrad, y = Diversity_value)) +
  geom_boxplot(aes(fill = Degrad), width=0.5, color="black") +
  scale_fill_manual(values = c("#006d2c", "#2ca25f", "#66c2a4", "#b2e2e2")) +
  # ylim(0,90)+
  labs(x = "Forest Class", y = "Alpha-diversity") +
  geom_text(data = letters_final, aes(x = x_value,  y = max_y+2, label = lab)) +
  facet_grid(rows = vars(Diversity_type), cols = vars(Order.q), scales = "free") + 
  ggtitle("Small trees in PGM") +
  theme_classic()+
  theme(legend.position = "none")


#### STM ####

## Large

list_model_large_STM <- list(TD_q0_large_STM, TD_q1_large_STM, TD_q2_large_STM,
                             FD_q0_large_STM, FD_q1_large_STM, FD_q2_large_STM,
                             PD_q0_large_STM, PD_q1_large_STM, PD_q2_large_STM)
names(list_model_large_STM) <- c("TD_q0", "TD_q1", "TD_q2",
                                 "FD_q0", "FD_q1", "FD_q2",
                                 "PD_q0", "PD_q1", "PD_q2")
letters_final <- NULL
for (i in names(list_model_large_STM)){
  letters <- cld(summary(glht(list_model_large_STM[[i]], mcp(Degrad="Tukey"))), decreasing = F)
  letters <- letters$mcletters$Letters
  letters <- cbind(t(letters), substr(i, start = 1, stop = 2), 
                   substr(i, start = 4, stop = 5))
  letters_final <- rbind(letters_final, letters)
}

letters_final <- as.data.frame(cbind(letters_final, data2 %>% 
                                       dplyr::filter(Region == "STM", Size == "large") %>%
                                       group_by(Diversity_type, Order.q) %>% 
                                       summarise("max_y" = max(Diversity_value))))
letters_final <- tidyr::pivot_longer(data = letters_final, cols = c("UF", "LF", "LBF", "SF"), 
                                     values_to = "lab", names_to = "x_value") %>% 
  select(!c("V5","V6"))

letters_final[letters_final$Diversity_type=="qD", "max_y"] <- letters_final[letters_final$Diversity_type=="qD", "max_y"]+4

plot_large_STM <- data2 %>% 
  dplyr::filter(Region == "STM", Size == "large") %>% 
  ggplot(aes(x = Degrad, y = Diversity_value)) +
  geom_boxplot(aes(fill = Degrad), width=0.5, color="black") +
  scale_fill_manual(values = c("#006d2c", "#2ca25f", "#66c2a4", "#b2e2e2")) +
  # ylim(0,90)+
  labs(x = "Forest Class", y = "Alpha-diversity") +
  geom_text(data = letters_final, aes(x = x_value,  y = max_y+2, label = lab)) +
  facet_grid(rows = vars(Diversity_type), cols = vars(Order.q), scales = "free") +
  ggtitle("Large trees in STM") +
  theme_classic() +
  theme(legend.position = "none")

## Small

list_model_small_STM <- list(TD_q0_small_STM, TD_q1_small_STM, TD_q2_small_STM,
                             FD_q0_small_STM, FD_q1_small_STM, FD_q2_small_STM,
                             PD_q0_small_STM, PD_q1_small_STM, PD_q2_small_STM)
names(list_model_small_STM) <- c("TD_q0", "TD_q1", "TD_q2",
                                 "FD_q0", "FD_q1", "FD_q2",
                                 "PD_q0", "PD_q1", "PD_q2")
letters_final <- NULL
for (i in names(list_model_small_STM)){
  letters <- cld(summary(glht(list_model_small_STM[[i]], mcp(Degrad="Tukey"))), decreasing = F)
  letters <- letters$mcletters$Letters
  letters <- cbind(t(letters), substr(i, start = 1, stop = 2), 
                   substr(i, start = 4, stop = 5))
  letters_final <- rbind(letters_final, letters)
}

letters_final <- as.data.frame(cbind(letters_final, data2 %>% 
                                       dplyr::filter(Region == "STM", Size == "small") %>%
                                       group_by(Diversity_type, Order.q) %>% 
                                       summarise("max_y" = max(Diversity_value))))
letters_final <- tidyr::pivot_longer(data = letters_final, cols = c("UF", "LF", "LBF", "SF"), 
                                     values_to = "lab", names_to = "x_value") %>% 
  select(!c("V5","V6"))

letters_final[letters_final$Diversity_type=="qD", "max_y"] <- letters_final[letters_final$Diversity_type=="qD", "max_y"]+4

plot_small_STM <- data2 %>% 
  dplyr::filter(Region == "STM", Size == "small") %>% 
  ggplot(aes(x = Degrad, y = Diversity_value)) +
  geom_boxplot(aes(fill = Degrad), width=0.5, color="black") +
  scale_fill_manual(values = c("#006d2c", "#2ca25f", "#66c2a4", "#b2e2e2")) +
  # ylim(0,90)+
  labs(x = "Forest Class", y = "Alpha-diversity") +
  geom_text(data = letters_final, aes(x = x_value,  y = max_y+2, label = lab)) +
  facet_grid(rows = vars(Diversity_type), cols = vars(Order.q), scales = "free") + 
  ggtitle("Small trees in STM") +
  theme_classic()+
  theme(legend.position = "none")
