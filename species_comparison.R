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

# wilcox test
wilcox_results <- wilcox.test(merged_df$Cmax, mu = 1210)
p_val <- formatC(wilcox_results$p.value, format = "e", digits = 2)

wilcox_results_therapeutic <- wilcox.test(merged_df$Cmax, mu = 114)
p_val_therapeutic <- formatC(wilcox_results_therapeutic$p.value, format = "e", digits = 2)

# boxplot
cmax_comparison <- merged_df %>%
  ggplot(., aes(y=Cmax)) +
    # https://loading.io/color/feature/Paired-12
    geom_boxplot(fill="grey") + 
    geom_jitter(aes(y=Cmax,x=0,color=Sex), size=4) +
    geom_hline(yintercept = 1210, linetype="dashed") +
    geom_hline(yintercept = 114, linetype="dashed", color='#fdbf6f') +
    theme_light() +
    scale_color_brewer(palette="Paired") +
    scale_y_log10() +
    scale_x_discrete(name=c("")) +
    labs(x="This Study", title=paste0("Wilcoxon P-value: ", p_val, " (mu=1210 ng/mL)")) +
    theme(
      axis.text=element_text(size=20), 
      axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
      axis.title=element_text(size=26,face="bold"),
      legend.title=element_text(size=26,face="bold"),
      legend.text =element_text(size=20),
      strip.text = element_text(size=22,face="bold")
    )
cmax_comparison
cmax_comparison_outpath <- 'cmax_comparison_4x3.pdf'
ggsave(cmax_comparison_outpath,
       cmax_comparison,
       width = 4,
       height = 3
)
