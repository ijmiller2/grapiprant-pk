# load modules
library(dplyr)
library(readxl)

##### load data #####

# load data
params_path <- "data/NCA_0.0.xls"

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
  select(Cmax, AUCINF_obs, Tmax, Lambda_z, HL_Lambda_z, Vz_F_obs, Cl_F_obs, Sex) %>%
  group_by(Sex) %>%
  summarize(
    Cmax_mean = round(mean(Cmax), 1), Cmax_sd = round(sd(Cmax), 1),
    Tmax_mean = mean(Tmax), Tmax_sd = sd(Tmax),
    AUCINF_obs_mean = round(mean(AUCINF_obs), 1), AUCINF_obs_sd = round(sd(AUCINF_obs), 1),
    Lambda_z_mean = round(mean(Lambda_z), 1), Lambda_z_sd = round(sd(Lambda_z), 1),
    HL_Lambda_z_mean = round(mean(HL_Lambda_z), 1), HL_Lambda_z_sd = round(sd(HL_Lambda_z), 1),
    Vz_F_obs_mean = round(mean(Vz_F_obs), 1), Vz_F_obs_sd = round(sd(Vz_F_obs), 1),
    Cl_F_obs_mean = round(mean(Cl_F_obs), 1), Cl_F_obs_sd = round(sd(Cl_F_obs), 1)
    )
stats_by_sex

# compute combined group stats
whole_group_stats <- merged_df %>%
  select(Cmax, AUCINF_obs, Tmax, Lambda_z, HL_Lambda_z, Vz_F_obs, Cl_F_obs) %>%
  summarize(
    Cmax_mean = round(mean(Cmax), 1), Cmax_sd = round(sd(Cmax), 1),
    Tmax_mean = mean(Tmax), Tmax_sd = sd(Tmax),
    AUCINF_obs_mean = round(mean(AUCINF_obs), 1), AUCINF_obs_sd = round(sd(AUCINF_obs), 1),
    Lambda_z_mean = round(mean(Lambda_z), 1), Lambda_z_sd = round(sd(Lambda_z), 1),
    HL_Lambda_z_mean = round(mean(HL_Lambda_z), 1), HL_Lambda_z_sd = round(sd(HL_Lambda_z), 1),
    Vz_F_obs_mean = round(mean(Vz_F_obs), 1), Vz_F_obs_sd = round(sd(Vz_F_obs), 1),
    Cl_F_obs_mean = round(mean(Cl_F_obs), 1), Cl_F_obs_sd = round(sd(Cl_F_obs), 1)
  )
whole_group_stats

# wilcox tests

# filter merged df by sex
male_params_df <- merged_df %>%
  filter(Sex=="M") %>%
  select(Animal_ID_, Cmax, AUCINF_obs, Tmax, Lambda_z, HL_Lambda_z, Vz_F_obs, Cl_F_obs, Sex)
female_params_df <- merged_df %>%
  filter(Sex=="F") %>%
  select(Animal_ID_, Cmax, AUCINF_obs, Tmax, Lambda_z, HL_Lambda_z, Vz_F_obs, Cl_F_obs, Sex)

# cmax
cmax_sex_wilcox_results <- wilcox.test(male_params_df$Cmax, female_params_df$Cmax,
                                       paired = F, alternative = "two.sided")
cmax_sex_wilcox_results
cmax_sex_wilcox_results$p.value

# AUCINF_obs
AUCINF_obs_sex_wilcox_results <- wilcox.test(male_params_df$AUCINF_obs, female_params_df$AUCINF_obs,
                                       paired = F, alternative = "two.sided")
AUCINF_obs_sex_wilcox_results
AUCINF_obs_sex_wilcox_results$p.value

# Lambda_z
lambdaz_sex_wilcox_results <- wilcox.test(male_params_df$Lambda_z, female_params_df$Lambda_z,
                                          paired = F, alternative = "two.sided")
lambdaz_sex_wilcox_results
lambdaz_sex_wilcox_results$p.value

# HC_Lambda_z
hl_lambdaz_sex_wilcox_results <- wilcox.test(male_params_df$HL_Lambda_z, female_params_df$HL_Lambda_z,
                                          paired = F, alternative = "two.sided")
hl_lambdaz_sex_wilcox_results
hl_lambdaz_sex_wilcox_results$p.value

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

