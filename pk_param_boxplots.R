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

# now pivot table, capture Cmax, AUClast, Lambda_z, Vz_F_obs, CL_F_obs
pk_param_boxplots <- merged_df %>%
  select(all_of(c("animal_id", "Sex", "Cmax", "AUCINF_obs", "Lambda_z", "HL_Lambda_z", "Vz_F_obs", "Cl_F_obs"))) %>%
  mutate(animal_id=factor(animal_id)) %>%
  tidyr::pivot_longer(
    cols =  c("Cmax", "AUCINF_obs", "Lambda_z", "HL_Lambda_z", "Vz_F_obs", "Cl_F_obs"),
    names_to = "param",
    values_to = "value"
  ) %>%
  ggplot(aes(x=Sex,y=value)) +
    geom_boxplot(aes(fill=Sex), show.legend = F) +
    geom_point(aes(color=Sex), position = position_jitterdodge(),
               alpha=0.8, size=3, show.legend = F) +
    facet_wrap(~param, scales = "free", nrow = 1) +
    labs(x="Parameter", y='Value') +
    scale_color_brewer(palette="Paired") +
    scale_fill_brewer(palette="Paired") +
    theme_light() +
    theme(
          axis.text=element_text(size=20), 
          axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title=element_text(size=26,face="bold"),
          legend.title=element_text(size=26,face="bold"),
          legend.text =element_text(size=20),
          strip.text = element_text(size=22,face="bold")
          )
pk_param_boxplots
pk_param_boxplots_outpath <- 'figures/pk_params_boxplots_14x6.pdf'
ggsave(pk_param_boxplots_outpath,
       pk_param_boxplots,
       width = 14,
       height = 6
)

