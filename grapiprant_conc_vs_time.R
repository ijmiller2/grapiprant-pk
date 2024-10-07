library(dplyr)
library(ggplot2)

# load data
conc_vs_time_df <- read.csv('grapipriant_conc_vs_time.csv')

# plot individuals
individual_plot <- conc_vs_time_df %>%
  ggplot(aes(x=timepoint, y=plasma_conc_ng)) +
    geom_point(aes(color=factor(subject_id))) +
    geom_line(aes(color=factor(subject_id))) +
    labs(color='Subject ID', x='Timepoint (hrs)', y='Plasma Concentration (ng/mL)') +
    theme_light() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
          legend.title=element_text(size=12,face="bold"),
          legend.text =element_text(size=10)
    )
individual_plot
individual_outpath <- 'grapipriant_conc_vs_time_individuals_7x6.pdf'
ggsave(individual_outpath,
       individual_plot,
       width = 7,
       height = 6
       )

# group by sex
group_by_sex_plot <- conc_vs_time_df %>%
  group_by(sex, timepoint) %>%
  summarize(
    plasma_conc_ng_mean = mean(plasma_conc_ng),
    plasma_conc_ng_sd = sd(plasma_conc_ng)
    ) %>%
    ggplot(aes(x=timepoint, y=plasma_conc_ng_mean)) +
      geom_point(aes(color=sex)) +
      geom_errorbar(aes(
        ymin = plasma_conc_ng_mean-plasma_conc_ng_sd,
        ymax = plasma_conc_ng_mean+plasma_conc_ng_sd,
        color=sex),
        width = 1) +
      geom_line(aes(color=factor(sex))) +
      labs(color='Subject Sex', x='Timepoint (hrs)', y='Plasma Concentration (ng/mL)') +
      theme_light() + 
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.title=element_text(size=12,face="bold"),
            legend.text =element_text(size=10)
            )

group_by_sex_plot
group_by_sex_plot_outpath <- 'grapipriant_conc_vs_time_sex_7x6.pdf'
ggsave(group_by_sex_plot_outpath,
       group_by_sex_plot,
       width = 7,
       height = 6
)

# combined group
combined_group_plot <- conc_vs_time_df %>%
  group_by(timepoint) %>%
  summarize(
    plasma_conc_ng_mean = mean(plasma_conc_ng),
    plasma_conc_ng_sd = sd(plasma_conc_ng)
  ) %>%
  mutate(group='Combined') %>%
  ggplot(aes(x=timepoint, y=plasma_conc_ng_mean)) +
  geom_point(aes(color=group)) +
  geom_errorbar(aes(
    ymin = plasma_conc_ng_mean-plasma_conc_ng_sd,
    ymax = plasma_conc_ng_mean+plasma_conc_ng_sd,
    color=group),
    width = 1) +
  geom_line(aes(color=group)) +
  labs(x='Timepoint (hrs)', y='Plasma Concentration (ng/mL)', color='Combined') +
  theme_light() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=12,face="bold"),
        legend.text =element_text(size=10)
  ) +
  scale_color_manual(values=c("black"))
combined_group_plot

combined_group_plot_outpath <- 'grapipriant_conc_vs_time_combined_7x6.pdf'
ggsave(combined_group_plot_outpath,
       combined_group_plot,
       width = 7,
       height = 6)
