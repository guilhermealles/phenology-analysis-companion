#!/usr/local/bin/Rscript
library(tidyverse)
library(colorspace)

data <- bind_rows(
  read_csv(
    '../../data/mtk-experiment.csv',
    col_types = cols(cores = col_integer(), exectime = col_double())
  ) %>%
  mutate(dataset = 'MTK'),
  
  read_csv(
    '../../data/tky-experiment.csv',
    col_types = cols(cores = col_integer(), exectime = col_double())
  ) %>%
  mutate(dataset = 'TKY'),

  read_csv(
    '../../data/ahs-experiment.csv',
    col_types = cols(cores = col_integer(), exectime = col_double())
  ) %>%
  mutate(dataset = 'AHS')
) %>%
  mutate(exectimeInSeconds = exectime/1000) %>%
  mutate(exectimeInMinutes = exectimeInSeconds/60) %>%
  group_by(cores, dataset) %>%
  summarize(
    avgMinutes = mean(exectimeInMinutes),
    errorMinutes = 3 * sd(exectimeInMinutes)/sqrt(n())
  ) %>%
  ungroup

p <- data %>%
  ggplot(aes(x = cores, y = avgMinutes, col = dataset)) +
    geom_line(alpha = 0.7) +
    geom_point() +
    geom_errorbar(aes(
      ymin = avgMinutes-errorMinutes,
      ymax = avgMinutes+errorMinutes
    ), width = 1.5, alpha = 0.8) +
    theme_bw(base_size = 12) +
    theme(
      plot.margin = unit(c(0,0,0,0), 'cm'),
      legend.position = c(0.9, 0.7),
      legend.background = element_rect(color = 'black', size = 0.2)
    ) +
    labs(
      x = 'Threads (count)',
      y = 'Mean execution time (min)',
      col = 'Dataset'
    ) +
    scale_x_continuous(breaks = unique(data$cores)) +
    scale_y_continuous(breaks = seq(0, 140, 20), limits = c(0, 140))

ggsave('performance-analysis.pdf', plot=p, width=8, height=2.5)
