# load modules
library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)

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

# plot metric box plot, group by sex
merged_df %>%
  ggplot(aes(x=Sex, y=Cmax)) +
    geom_boxplot(aes(fill=Sex), show.legend = F) +
    geom_jitter(aes(size=WeightKg, color=Animal_ID_), alpha=0.7) +
    labs(color="Animal ID") +
    theme_light()

# wilcox test for difference in Cmax
male_cmax_df <- merged_df %>%
  filter(Sex=="M") %>%
  select(Animal_ID_, Cmax, Sex)
female_cmax_df <- merged_df %>%
  filter(Sex=="F") %>%
  select(Animal_ID_, Cmax, Sex)
cmax_sex_wilcox_results <- wilcox.test(male_cmax_df$Cmax, female_cmax_df$Cmax,
                                       paired = F, alternative = "two.sided")
cmax_sex_wilcox_results

# replot with stats
title <- paste("Cmax: Wilcoxon Rank Sum Test of Male vs. Female, p-value:", round(cmax_sex_wilcox_results$p.value, 5))
merged_df %>%
  ggplot(aes(x=Sex, y=Cmax)) +
  geom_boxplot(aes(fill=Sex), show.legend = F) +
  geom_jitter(aes(size=WeightKg, color=Animal_ID_), alpha=0.7) +
  labs(color="Animal ID", title=title) +
  theme_light()


# via plotly
box_plot <- merged_df %>%
  ggplot(aes(x=Sex, y=Cmax, id=Animal_ID_)) +
  geom_boxplot(aes(fill=Sex), show.legend = F) +
  geom_jitter(alpha=0.7, size=2) +
  theme_light()
ggplotly(box_plot)

# # filter out most relevant params
# 
# #### facet plot by param ####
# 
# ?t.test
# 
# # Cmax, Tmax, AUC, CL
# params_of_interest <- c("Cmax", "AUClast")
# 
# # load long params df
# long_params_sheet <- "Final Parameters"
# long_params_df <-  read_excel(params_path, long_params_sheet)
# 
# # filter for params of interest
# long_params_filtered_df <- long_params_df %>%
#   filter(Parameter %in% params_of_interest)
# row.names(long_params_filtered_df) <- long_params_filtered_df$Animal_ID_
# 
# # merge the metadata
# long_merged_df <- merge(long_params_filtered_df, group_df)
# 
# # box plot
# long_merged_df %>%
#   ggplot(aes(x=Sex, y=Estimate)) +
#   geom_boxplot() +
#   geom_jitter(aes(size=AgeYrs, fill=Animal_ID_),
#               alpha=0.7, shape=21) +
#   theme_light() +
#   facet_wrap(~Parameter, scales = "free_y")
# 
# # no color on jitter, fill color by Sex
# long_merged_df %>%
#   ggplot(aes(x=Sex, y=Estimate)) +
#   geom_boxplot(aes(fill=Sex)) +
#   geom_jitter(aes(size=AgeYrs),
#               alpha=0.7, shape=21) +
#   theme_light() +
#   facet_wrap(~Parameter, scales = "free_y")
# 
