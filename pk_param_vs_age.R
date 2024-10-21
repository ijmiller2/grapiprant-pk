# load modules
library(dplyr)
library(ggplot2)
library(readxl)
library(ggpmisc)

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

# histogram of ages
age_histogram <- merged_df %>%
  ggplot(aes(x=AgeYrs)) +
  #geom_histogram(aes(fill=Sex), bins=10, alpha=0.8, position='identity') +
  geom_histogram(aes(fill=Sex), bins=10, alpha=0.8, position='stack') +
  scale_fill_brewer(palette="Paired") +
  labs(x="Age (years)", y="Subject Count") + 
  theme_light() +
  theme(
    axis.text=element_text(size=20), 
    axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title=element_text(size=26,face="bold"),
    legend.title=element_text(size=26,face="bold"),
    legend.text =element_text(size=20)
  )
age_histogram
age_histogram_outpath <- 'figures/age_histogram_4x3.pdf'
ggsave(age_histogram_outpath,
       age_histogram,
       width = 4,
       height = 3
)
  
# initialize df
param_vs_age_df <- data.frame()

# for each param, get corr coef
params <- c("Cmax", "AUCINF_obs", "Lambda_z", "Vz_F_obs", "Cl_F_obs")
for (param in params){
  print(param)
  spearman_corr = cor.test(merged_df[[param]], merged_df[['AgeYrs']])
  spearman_p_val <- round(spearman_corr$p.value, 5)
  spearman_corr_coef <- round(spearman_corr$estimate, 3)
  param_df <- data.frame(
    spearman_corr_coef = spearman_corr_coef,
    spearman_p_val = spearman_p_val,
    param = param,
    group = "combined"
   
  )
  param_vs_age_df <- rbind(param_vs_age_df, param_df)
  print(paste(spearman_corr_coef, spearman_p_val))
}

# now segment by sex
for (sex in c("M", "F")){
  sex_subset_df <- merged_df %>%
    filter(Sex==sex)
  for (param in params){
    print(paste(param, sex))
    spearman_corr = cor.test(sex_subset_df[[param]], sex_subset_df[['AgeYrs']])
    spearman_p_val <- round(spearman_corr$p.value, 5)
    spearman_corr_coef <- round(spearman_corr$estimate, 3)
    param_df <- data.frame(
      spearman_corr_coef = spearman_corr_coef,
      spearman_p_val = spearman_p_val,
      param = param,
      group = sex
    )
    param_vs_age_df <- rbind(param_vs_age_df, param_df)
    print(paste(spearman_corr_coef, spearman_p_val))
  }
}

# multiple hypothesis correction
param_vs_age_df$spearman_q_val <- p.adjust(param_vs_age_df$spearman_p_val, method = 'BH') 

# write out the table
param_vs_age_df_outpath <- "data/spearman_param_vs_age_df.csv"
write.csv(param_vs_age_df,
          param_vs_age_df_outpath)

# cmax vs. age
lm_formula <- y ~ x
aucinf_vs_age_scatter <- merged_df %>%
  ggplot(aes(x=AgeYrs, y=AUCINF_obs)) +
  geom_point(alpha=0.8, size=5, aes(color=Sex), show.legend = F) +
  geom_smooth(method='lm', formula= lm_formula) +
  stat_poly_eq(formula = lm_formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  facet_wrap(~Sex, scales = 'free_x') +
  theme_light() +
  labs(x="Age (Years)", y='AUC_0-inf') +
  scale_color_brewer(palette="Paired") +
  theme(
    axis.text=element_text(size=20), 
    axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title=element_text(size=26,face="bold"),
    legend.title=element_text(size=26,face="bold"),
    legend.text =element_text(size=20),
    strip.text = element_text(size=22,face="bold")
  )
aucinf_vs_age_scatter
aucinf_vs_age_scatter_outpath <- 'figures/aucinf_vs_age_scatter_5x3.pdf'
ggsave(aucinf_vs_age_scatter_outpath,
       aucinf_vs_age_scatter,
       width = 5,
       height = 3
)
