#!/usr/local/bin/Rscript
library(tidyverse)

df.weights = tibble(
  Hour = c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  Weight = c(0.025, 0.05, 0.05, 0.1, 0.15, 0.25, 0.15, 0.1, 0.05, 0.05, 0.025)
)

p <- df.weights %>%
  mutate(Hour = as.factor(Hour)) %>%
  ggplot(aes(x = Hour, y = Weight)) +
  geom_col() +
  geom_text(aes(label = Weight, angle = 45), nudge_y = 0.07) +
  theme_bw(base_size = 12) +
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      legend.spacing = unit(0, "mm"),
      panel.grid = element_blank(),
      legend.position = "none",
      legend.justification = "left",
      legend.box.spacing = unit(0, "pt"),
      legend.box.margin = margin(0,0,0,0),
      legend.title = element_blank()
    ) +
    xlab('Hour') +
    ylab('Weight') +
    ylim(0, 0.37)

ggsave('./mtk-weights.pdf', plot = p, width = 4.5, height = 1.5)
