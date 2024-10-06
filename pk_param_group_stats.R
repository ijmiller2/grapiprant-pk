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

# update Vz_F_obs, Cl_obs
merged_df$Vz_F_obs <- merged_df$`Dose (mg)` / (merged_df$Lambda_z * merged_df$AUCINF_obs)
merged_df$Cl_F_obs <- merged_df$`Dose (mg)` / merged_df$AUCINF_obs

# compute means, by sex
stats_by_sex <- merged_df %>%
  select(Cmax, AUClast, Tmax, Lambda_z, Vz_F_obs, Cl_F_obs, Sex) %>%
  group_by(Sex) %>%
  summarize(
    Cmax_mean = round(mean(Cmax), 1), Cmax_sd = round(sd(Cmax), 1),
    Tmax_mean = mean(Tmax), Tmax_sd = sd(Tmax),
    AUClast_mean = round(mean(AUClast), 1), AUClast_sd = round(sd(AUClast), 1),
    Lambda_z_mean = round(mean(Lambda_z), 1), Lambda_z_sd = round(sd(Lambda_z), 1),
    Vz_F_obs_mean = round(mean(Vz_F_obs), 1), Vz_F_obs_sd = round(sd(Vz_F_obs), 1),
    Cl_F_obs_mean = round(mean(Cl_F_obs), 1), Cl_F_obs_sd = round(sd(Cl_F_obs), 1)
    )

# compute combined group stats
whole_group_stats <- merged_df %>%
  select(Cmax, AUClast, Tmax, Lambda_z, Vz_F_obs, Cl_F_obs) %>%
  summarize(
    Cmax_mean = round(mean(Cmax), 1), Cmax_sd = round(sd(Cmax), 1),
    Tmax_mean = mean(Tmax), Tmax_sd = sd(Tmax),
    AUClast_mean = round(mean(AUClast), 1), AUClast_sd = round(sd(AUClast), 1),
    Lambda_z_mean = round(mean(Lambda_z), 1), Lambda_z_sd = round(sd(Lambda_z), 1),
    Vz_F_obs_mean = round(mean(Vz_F_obs), 1), Vz_F_obs_sd = round(sd(Vz_F_obs), 1),
    Cl_F_obs_mean = round(mean(Cl_F_obs), 1), Cl_F_obs_sd = round(sd(Cl_F_obs), 1)
  )

# wilcox tests

# filter merged df by sex
male_params_df <- merged_df %>%
  filter(Sex=="M") %>%
  select(Animal_ID_, Cmax, AUClast, Tmax, Lambda_z, Vz_F_obs, Cl_F_obs, Sex)
female_params_df <- merged_df %>%
  filter(Sex=="F") %>%
  select(Animal_ID_, Cmax, AUClast, Tmax, Lambda_z, Vz_F_obs, Cl_F_obs, Sex)

# cmax
cmax_sex_wilcox_results <- wilcox.test(male_params_df$Cmax, female_params_df$Cmax,
                                       paired = F, alternative = "two.sided")
cmax_sex_wilcox_results
cmax_sex_wilcox_results$p.value

# AUClast
auclast_sex_wilcox_results <- wilcox.test(male_params_df$AUClast, female_params_df$AUClast,
                                       paired = F, alternative = "two.sided")
auclast_sex_wilcox_results
auclast_sex_wilcox_results$p.value

# Lambda_z
lambdaz_sex_wilcox_results <- wilcox.test(male_params_df$Lambda_z, female_params_df$Lambda_z,
                                          paired = F, alternative = "two.sided")
lambdaz_sex_wilcox_results
lambdaz_sex_wilcox_results$p.value

# Vz_F_obs
vz_sex_wilcox_results <- wilcox.test(male_params_df$Vz_F_obs, female_params_df$Vz_F_obs,
                                          paired = F, alternative = "two.sided")
vz_sex_wilcox_results
vz_sex_wilcox_results$p.value

# Cl_F_obs
cl_sex_wilcox_results <- wilcox.test(male_params_df$Cl_F_obs, female_params_df$Cl_F_obs,
                                     paired = F, alternative = "two.sided")
cl_sex_wilcox_results
cl_sex_wilcox_results$p.value
