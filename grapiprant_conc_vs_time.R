library(dplyr)
library(ggplot2)

# load data
conc_vs_time_df <- read.csv('data/grapiprant_conc_vs_time.csv')

# set timepoints for x-axis ticks
x_major_ticks <- c(0, 0.25, 0.5, 1, 2, 4, 8, 12, 24, 48)
#x_major_ticks <- c(0, 2, 4, 8, 12, 24, 48)

# format tick axis labels
scale_fun <- function(x) sprintf("%.1f", x)

# plot individuals
individual_plot <- conc_vs_time_df %>%
  ggplot(aes(x=timepoint, y=plasma_conc_ng)) +
    geom_line(aes(color=factor(subject_id)), size=1) +
    #geom_point(aes(fill=factor(subject_id)), size=3, pch=21, color='black') +
    geom_point(aes(color=factor(subject_id)), size=3) +
    labs(color='Subject ID', fill='Subject ID', x='Timepoint (hrs)', y='Plasma Concentration (ng/mL)') +
    scale_x_continuous(breaks=x_major_ticks, minor_breaks = NULL, labels=scale_fun) +
    scale_color_brewer(palette="Paired") +
    theme_light() +
    theme(axis.text=element_text(size=20), 
          axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title=element_text(size=26,face="bold"),
          legend.title=element_text(size=26,face="bold"),
          legend.text =element_text(size=20)
    )
individual_plot
individual_outpath <- 'figures/grapiprant_conc_vs_time_individuals_7x6.pdf'
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
    geom_line(aes(color=factor(sex)), size=1) +
      geom_point(aes(color=sex), size=3) +
      geom_errorbar(aes(
        ymin = plasma_conc_ng_mean-plasma_conc_ng_sd,
        ymax = plasma_conc_ng_mean+plasma_conc_ng_sd,
        color=sex),
        width = 2, size=1) +
      scale_x_continuous(breaks=x_major_ticks, minor_breaks = NULL, labels=scale_fun) +
      scale_color_brewer(palette="Paired") +
      labs(color='Subject Sex', x='Timepoint (hrs)', y='Plasma Concentration (ng/mL)') +
      theme_light() + 
      theme(axis.text=element_text(size=20), 
            axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
            axis.title=element_text(size=26,face="bold"),
            legend.title=element_text(size=26,face="bold"),
            legend.text =element_text(size=20)
      )

group_by_sex_plot
group_by_sex_plot_outpath <- 'figures/grapiprant_conc_vs_time_sex_7x6.pdf'
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
  geom_point(aes(color=group), size=3) +
  geom_errorbar(aes(
    ymin = plasma_conc_ng_mean-plasma_conc_ng_sd,
    ymax = plasma_conc_ng_mean+plasma_conc_ng_sd,
    color=group),
    width = 2,
    size=1) +
  geom_line(aes(color=group), size=1) +
  scale_x_continuous(breaks=x_major_ticks, minor_breaks = NULL, labels=scale_fun) +
  labs(x='Timepoint (hrs)', y='Plasma Concentration (ng/mL)', color='Group') +
  theme_light() + 
  theme(axis.text=element_text(size=20), 
        axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title=element_text(size=26,face="bold"),
        legend.title=element_text(size=26,face="bold"),
        legend.text =element_text(size=20)
  ) +
  scale_color_manual(values=c("black"))
combined_group_plot

combined_group_plot_outpath <- 'figures/grapiprant_conc_vs_time_combined_7x6.pdf'
ggsave(combined_group_plot_outpath,
       combined_group_plot,
       width = 7,
       height = 6)
