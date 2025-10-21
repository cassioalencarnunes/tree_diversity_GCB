#### Script to create the graphs of the article ####

library(dplyr)
library(ggplot2)
library(gridExtra)
library(egg)

#### Loading tables ####

diversity <- read.csv(file = "Results/estimates_diversity.csv", h = T)
composition <- read.csv(file = "Results/estimates_omega.csv", h = T)

comp_comparison <- read.csv(file = "Results/emmeans_comparison_composition.csv", h = T,
                            row.names = 1)
div_interaction <- read.csv(file = "Results/emmeans_comparison_stem_diversity.csv", h = T,
                            row.names = 1)
div_diversity <- read.csv(file = "Results/emmeans_facet_diversity.csv", h = T,
                          row.names = 1)
comp_q_value <- read.csv(file = "Results/emmeans_qvalue_composition.csv", h = T,
                         row.names = 1)

diversity <- bind_rows(div_interaction, div_diversity)

R2_diversity <- read.csv("Results/R2_diversity.csv", h=T)
R2_composition <- read.csv("Results/R2_composition.csv", h=T)


#### Plots ####


#### Diversity ####

div_interaction <- div_interaction %>% 
  arrange(emmean)

efsize_data_ordered <- div_interaction %>%
  group_by(comparison) %>% 
  summarise(mean_es = mean(emmean))

efsize_data_ordered <- efsize_data_ordered %>% 
  ungroup() %>%
  arrange(mean_es) %>% 
  mutate(order = row_number())

effect_size_plot <- div_interaction %>% 
  left_join(efsize_data_ordered, by = c("comparison"))

effect_size_plot$comparison <- as.factor(effect_size_plot$comparison)

levels(effect_size_plot$comparison) <- c("UF_LF", "LF_LBF", "UF_LBF", "LBF_SF", "LF_SF", "UF_SF")

effect_size_plot$stem <- factor(effect_size_plot$stem, levels = c("small", "large"))

# "#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF"


# a)

a_div <- ggplot(effect_size_plot, aes(y = as.factor(order), x = emmean)) +
  geom_pointrange(aes(y = as.factor(order), xmin = emmean-SE, xmax = emmean+SE, 
                      color = stem)) +
  scale_color_manual(values = c("#006CD1", "#FFC20A")) +
  facet_grid(cols = vars(stem), scales= 'free', space='free') +
  scale_y_discrete(
    breaks = rep(as.numeric(efsize_data_ordered$order),2),
    labels = rep(as.factor(efsize_data_ordered$comparison), 2),
    expand = c(0.05,0)
  ) +
  ylab("Pair of forest class") +
  xlab( "Predicted Mean Effect size") +
  xlim(c(0,1.65))+
  theme_classic() +
  theme(legend.position = "none")


# b)

b_div <- ggplot(effect_size_plot, aes(y = stem, x = emmean)) +
  geom_pointrange(aes(y = stem, xmin = emmean - SE, xmax = emmean + SE, 
                      color = stem)) +
  scale_color_manual(values = c("#006CD1", "#FFC20A")) +
  facet_grid(cols = vars(comparison), scales = 'free', space = 'fixed') +
  ylab("Tree size") +
  xlab("Predicted Mean Effect size") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(
    breaks = function(limits) {
      # Define two breaks slightly away from the edges
      c(limits[1] + diff(limits) * 0.25, limits[1] + diff(limits) * 0.75)
    },
    labels = scales::number_format(accuracy = 0.01) # Round to 2 decimal places
  )



# c)

div_diversity <- div_diversity %>% 
  arrange(emmean)

div_diversity$diversity <- factor(div_diversity$diversity, levels = c("PD", "FD", "TD"))

c_div <- ggplot(div_diversity, aes(y = diversity, x = emmean)) +
  geom_pointrange(aes(xmin=emmean-SE, xmax=emmean+SE)) +
  xlim(c(0,1))+
  ylab("Diversity facet") +
  xlab("Predicted Mean Effect size") +
  theme_classic()


# d) - R2

d_div <- ggplot(R2_diversity[-c(5:6),], aes(fill = term, y = estimate, x = R2)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#a6611a", "#dfc27d", "#80cdc1", "#018571")) +
  ylim(c(0,1)) +
  ylab("R² value") +
  xlab("Fixed terms") +
  theme_classic() +
  theme(legend.position = "none")

layout_figure_1 <- rbind(c(1,1,2),
                         c(3,3,4))
grid.arrange(a_div, c_div, b_div, d_div, layout_matrix = layout_figure_1)




####Composition ####

comp_comparison <- comp_comparison %>% 
  arrange(emmean)

comp_comparison$pairs <- factor(comp_comparison$pairs,
                                levels = c("LF vs LBF", "UF vs LF", "LBF vs SF",
                                           "UF vs LBF", "LF vs SF", "UF vs SF"))

# a)

a_comp <- ggplot(comp_comparison, aes(y = pairs, x = emmean)) +
  geom_pointrange(aes(xmin=emmean-SE, xmax=emmean+SE)) +
  xlim(c(0,0.35))+
  ylab("Pair of forest class") +
  xlab("Predicted Mean Effect size") +
  theme_classic()

# b)

b_comp <- ggplot(comp_q_value, aes(y = q_value, x = emmean)) +
  geom_pointrange(aes(xmin=emmean-SE, xmax=emmean+SE)) +
  xlim(c(0,0.3)) +
  ylab("q value") +
  xlab("Predicted Mean Effect size") +
  theme_classic()

# c) - R2

# Stacked + percent
c_comp <- ggplot(R2_composition[c(1:2),], aes(fill = term, y = estimate, x = R2)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#a6611a", "#998ec3")) +
  ylim(c(0,1)) +
  ylab("R² value") +
  xlab("Fixed terms") +
  theme_classic() +
  theme(legend.position = "none")

grid.arrange(a_comp, b_comp, c_comp, nrow = 1)
