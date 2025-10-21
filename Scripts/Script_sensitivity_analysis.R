# Libraries
library(dplyr)
library(stringr)
library(MuMIn)
library(partR2)
library(merTools)
library(multcomp)


#### Alpha diversity ####

##### Loading effect sizes ######

effect_size <- read.csv("Results/efsize_diversity.csv", h=T)

effect_size$comparison <- rep(effect_size[1:6,"X"], 216/6)
effect_size$comparison <- str_replace(effect_size$comparison, " - ", "_")
effect_size$diversity <- str_sub(effect_size$model, 1, 2)
effect_size$stem <- str_sub(effect_size$model, 7, 11)
effect_size$region <- str_sub(effect_size$model, 13, 15)
effect_size$q_value <- str_sub(effect_size$model, 4, 5)

effect_size$comparison <- factor(effect_size$comparison, levels=c("LF_UF", "LBF_LF", 
                                                                  "LBF_UF", "SF_LBF",
                                                                  "SF_LF", "SF_UF"))
effect_size$diversity <- factor(effect_size$diversity, levels=c("TD", "FD", "PD"))

effect_size$abs_estimate <- abs(effect_size$Estimate)


#### Testing global model ####

m1 <- lmer(abs_estimate ~ comparison*stem*diversity*q_value + (1|region),
             data = effect_size)

car::Anova(m1)

m1.2 <- lmer(abs_estimate ~ comparison*stem + diversity
             + (1|region), data = effect_size)
car::Anova(m1.2)


#### Contrast analysis of the interaction ####

library(emmeans)
m1.2_emm <- emmeans(m1.2, ~ comparison*stem + diversity)

stem_contrast <- mvcontrast(m1.2_emm, "pairwise", mult.name = c("diversity"),
                            by = "comparison",
                            adjust = "bonferroni", show.ests = T)
comparison_contrast <- mvcontrast(m1.2_emm, "pairwise", mult.name = c("diversity"),
                                  by = "stem",
                                  adjust = "bonferroni", show.ests = T)

large <- comparison_contrast$tests %>% filter(stem == "large")
small <- comparison_contrast$tests %>% filter(stem == "small")

teste_cld <- summary(glht(m1.2, mcp(comparison = "Tukey", interaction_average = T)))

# Large
teste_cld$test$pvalues <- large$p.value
cld(teste_cld, decreasing = T)

#Small
teste_cld$test$pvalues <- small$p.value
cld(teste_cld, decreasing = T)

comparison_facet <- mvcontrast(m1.2_emm, "pairwise", mult.name = c("stem", "comparison"),
                                  adjust = "bonferroni", show.ests = T)

summary_m1.2_emm <- summary(m1.2_emm)

summary_m1.2_emm <- summary_m1.2_emm %>% 
  arrange(desc(emmean)) 

write.csv(x = summary_m1.2_emm, file = "Results/estimates_diversity.csv")

comparison_stem <- summary(emmeans(m1.2, ~ comparison*stem))
write.csv(x = comparison_stem, file = "Results/emmeans_comparison_stem_diversity.csv")
diversity <- summary(emmeans(m1.2, ~ diversity))
write.csv(x = diversity, file = "Results/emmeans_facet_diversity.csv")


#### Testing model adequacy ####

DHARMa::simulateResiduals(m1.2, plot = T)


#### R² of the best model ####

MuMIn::r.squaredGLMM(m1.2)

## Variance partitioning ##
#https://cran.r-project.org/web/packages/partR2/vignettes/Using_partR2.html

modGP1 <- lmer(abs_estimate ~ comparison * stem + diversity + (1|region),
               data = effect_size)
R2_GPc_part1 <- partR2(modGP1, partvars = c("stem:comparison", "diversity"),
                       data = effect_size, 
                       nboot = 100, R2_type = "marginal")

modGP2 <- lmer(abs_estimate ~ comparison + stem + diversity + (1|region),
               data = effect_size)
R2_GPc_part2 <- partR2(modGP2, partvars = c("stem", "comparison", "diversity"),
                       data = effect_size, max_level = 1, nboot = 100,
                       R2_type = "marginal")
# the first partR2 object is based on the full model
R2_GPc <- mergeR2(R2_GPc_part1, R2_GPc_part2) 
R2_GPc


Rpart_df_plot <- R2_GPc$R2 %>% 
  dplyr::select(term, estimate) %>% 
  filter(term == "stem" | term == "comparison" | term == "diversity" | term == "stem:comparison") %>% 
  mutate(R2 = "R2") %>% 
  add_row(term = "R2 marginal", estimate = 0.7111654, R2 =  "R2 total") %>%
  add_row(term = "R2 conditional", estimate = 0.7135461-0.7111654, R2 =  "R2 total")

write.csv(x = Rpart_df_plot, file = "Results/R2_diversity.csv")



#### Composition ####

##### Loading effect sizes ######

effect_size <- read.csv("Results/effect_size_permanova.csv", h=T)

#### Testing global model ####

m1 <- lmer(OmegaSq_degrad ~ pairs*stem*diversity*q_value + (1|region),
           data = effect_size)

car::Anova(m1)

m1.2 <- lmer(OmegaSq_degrad ~ pairs + stem + diversity + q_value + (1|region),
             data = effect_size)
car::Anova(m1.2)

m1.3 <- lmer(OmegaSq_degrad ~ pairs + diversity + q_value + (1|region),
             data = effect_size)
car::Anova(m1.3)

m1.4 <- lmer(OmegaSq_degrad ~ pairs + q_value + (1|region),
             data = effect_size)
car::Anova(m1.4)


#### Contrast analysis of the interaction ####

library(emmeans)
m1.4_emm <- emmeans(m1.4, ~ pairs + q_value)

summary_m1.4_emm <- summary(m1.4_emm)

summary_m1.4_emm <- summary_m1.4_emm %>% 
  arrange(desc(emmean)) 

write.csv(x = summary_m1.4_emm, file = "Results/estimates_omega.csv")

pairs <- summary(emmeans(m1.4, ~ pairs))
write.csv(x = pairs, file = "Results/emmeans_comparison_composition.csv")
q_value <- summary(emmeans(m1.4, ~ q_value))
write.csv(x = q_value, file = "Results/emmeans_qvalue_composition.csv")


summary(glht(m1.4, mcp(pairs = "Tukey", interaction_average = T)))
cld(summary(glht(m1.4, mcp(pairs = "Tukey", interaction_average = T))), decreasing = T)

summary(glht(m1.4, mcp(q_value = "Tukey", interaction_average = T)))
cld(summary(glht(m1.4, mcp(q_value = "Tukey", interaction_average = T))), decreasing = T)



#### Testing model adequacy ####

DHARMa::simulateResiduals(m1.4, plot = T)

DHARMa::testSimulatedResiduals(m1.4)
DHARMa::testCategorical(m1.4, catPred = effect_size$diversity)
DHARMa::testCategorical(m1.4, catPred = effect_size$stem)
DHARMa::testCategorical(m1.4, catPred = effect_size$pairs)
DHARMa::testCategorical(m1.4, catPred = effect_size$q_value)


#### R² of the best model ####

MuMIn::r.squaredGLMM(m1.4)

## Variance partitioning ##
#https://cran.r-project.org/web/packages/partR2/vignettes/Using_partR2.html

R2_part_output <- partR2(m1.4, partvars = c("pairs", "q_value"),
                       data = effect_size, 
                       nboot = 100, R2_type = "marginal")

Rpart_df_plot <- R2_part_output$R2 %>% 
  dplyr::select(term, estimate) %>% 
  filter(term == "stem" | term == "pairs" | term == "q_value") %>% 
  mutate(R2 = "R2") %>% 
  add_row(term = "R2 marginal", estimate = 0.4564, R2 =  "R2 total") %>%
  add_row(term = "R2 conditional", estimate = 0.7198-0.4564, R2 =  "R2 total")

write.csv(x = Rpart_df_plot, file = "Results/R2_composition.csv")

