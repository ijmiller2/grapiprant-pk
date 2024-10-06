# load modules
library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)
library(ggpmisc)

##### load data #####

# load data
params_path <- "~/Desktop/Home/Lauren/grapiprant-pk/NCA 0.0.xls"

# load params df
params_sheet <- "Final Parameters Pivoted"
params_df <- read_excel(params_path, params_sheet)
row.names(params_df) <-params_df$Animal_ID_ 

# load group df
group_sheet <- "Group"
group_df <- read_excel(params_path, group_sheet)
row.names(group_df) <-group_df$Animal_ID_

# merge data
merged_df <- merge(params_df, group_df)

##### example boxplot, extra aes #####

# plot metric box plot, group by sex
merged_df %>%
  ggplot(aes(x=Sex, y=Cmax)) +
    geom_boxplot(aes(fill=Sex), show.legend = F) +
    #geom_jitter(aes(size=WeightKg, color=Animal_ID_), alpha=0.7) +
    geom_jitter(alpha=0.8, size=5) +
    labs(color="Animal ID") +
    theme_light()

##### subset pk params by sex #####

# wilcox test for difference in Cmax
male_params_df <- merged_df %>%
  filter(Sex=="M") %>%
  select(Animal_ID_, Cmax, AUClast, Tmax, Sex, AgeYrs)
female_params_df <- merged_df %>%
  filter(Sex=="F") %>%
  select(Animal_ID_, Cmax, AUClast, Tmax, Sex, AgeYrs)

##### Cmax #####

# compute mean, by sex
merged_df %>%
  select(Cmax, AUClast, Tmax, Sex) %>%
  group_by(Sex) %>%
  summarize(Cmax_mean = mean(Cmax), Cmax_sd = sd(Cmax))

# compute combined
merged_df %>%
  select(Cmax, AUClast, Tmax, Sex) %>%
  summarize(Cmax_mean = mean(Cmax), Cmax_sd = sd(Cmax))

# wilcox
cmax_sex_wilcox_results <- wilcox.test(male_params_df$Cmax, female_params_df$Cmax,
                                       paired = F, alternative = "two.sided")
cmax_sex_wilcox_results

# boxplot with p-val
title <- paste("Cmax: Wilcoxon Rank Sum Test of Male vs. Female, p-value:", round(cmax_sex_wilcox_results$p.value, 5))
merged_df %>%
  ggplot(aes(x=Sex, y=Cmax)) +
  geom_boxplot(aes(fill=Sex), show.legend = F) +
  geom_jitter(alpha=0.8, size=5) +
  labs(color="Animal ID", title=title) +
  theme_light()

# cmax vs. age
lm_formula <- y ~ x
merged_df %>%
  ggplot(aes(x=AgeYrs, y=Cmax)) +
  geom_point(alpha=0.8, size=5, aes(color=Sex), show.legend = F) +
  geom_smooth(method='lm', formula= lm_formula) +
  stat_poly_eq(formula = lm_formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  #facet_wrap(~Sex, scales = 'free_x') +
  theme_light()

# spearman's rho
cor.test(merged_df$AgeYrs, merged_df$Cmax)
cor.test(male_params_df$AgeYrs, male_params_df$Cmax)
cor.test(female_params_df$AgeYrs, female_params_df$Cmax)

##### Tmax #####

# compute mean, by sex
merged_df %>%
  select(Cmax, AUClast, Tmax, Sex) %>%
  group_by(Sex) %>%
  summarize(Tmax_mean = mean(Tmax), Tmax_sd = sd(Tmax))

# compute combined
merged_df %>%
  select(Cmax, AUClast, Tmax, Sex) %>%
  summarize(Tmax_mean = mean(Tmax), Tmax_sd = sd(Tmax))

# wilcox
tmax_sex_wilcox_results <- wilcox.test(male_params_df$Tmax, female_params_df$Tmax,
                                       paired = F, alternative = "two.sided")
tmax_sex_wilcox_results

# boxplot with p-val
title <- paste("Tmax: Wilcoxon Rank Sum Test of Male vs. Female, p-value:", round(tmax_sex_wilcox_results$p.value, 5))
merged_df %>%
  ggplot(aes(x=Sex, y=Tmax)) +
  geom_boxplot(aes(fill=Sex), show.legend = F) +
  geom_jitter(alpha=0.8, size=5) +
  labs(color="Animal ID", title=title) +
  theme_light()

##### AUClast #####

# compute mean, by sex
merged_df %>%
  select(Cmax, AUClast, Tmax, Sex) %>%
  group_by(Sex) %>%
  summarize(AUClast_mean = mean(AUClast), AUClast_sd = sd(AUClast))

# compute combined
merged_df %>%
  select(Cmax, AUClast, Tmax, Sex) %>%
  summarize(AUClast_mean = mean(AUClast), AUClast_sd = sd(AUClast))

# wilcox
AUClast_sex_wilcox_results <- wilcox.test(male_params_df$AUClast, female_params_df$AUClast,
                                       paired = F, alternative = "two.sided")
AUClast_sex_wilcox_results

# boxplot with p-val
title <- paste("AUClast: Wilcoxon Rank Sum Test of Male vs. Female, p-value:", round(AUClast_sex_wilcox_results$p.value, 5))
merged_df %>%
  ggplot(aes(x=Sex, y=AUClast)) +
  geom_boxplot(aes(fill=Sex), show.legend = F) +
  geom_jitter(alpha=0.8, size=5) +
  labs(color="Animal ID", title=title) +
  theme_light()

# auclast vs. age
lm_formula <- y ~ x
merged_df %>%
  ggplot(aes(x=AgeYrs, y=AUClast)) +
  geom_point(alpha=0.8, size=5, aes(color=Sex), show.legend = F) +
  geom_smooth(method='lm', formula= lm_formula) +
  stat_poly_eq(formula = lm_formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  #facet_wrap(~Sex, scales = 'free_x') +
  theme_light()

# spearman's rho
cor.test(merged_df$AgeYrs, merged_df$AUClast)
cor.test(male_params_df$AgeYrs, male_params_df$AUClast)
cor.test(female_params_df$AgeYrs, female_params_df$AUClast)
