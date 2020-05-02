#!/usr/bin/Rscript
library(tidyverse)

get_weight <- function(day, hour, group) {
  baseHourlyWeight <- case_when (
    hour == 10 ~ 0.1,
    hour == 11 ~ 0.15,
    hour == 12 ~ 0.25,
    hour == 13 ~ 0.3,
    hour == 14 ~ 0.2,
  )

  dayGroupDelta <- group - day
  weightCoefficient <- case_when(
    dayGroupDelta == 2 ~ 0.25,
    dayGroupDelta == 1 ~ 0.35,
    dayGroupDelta == 0 ~ 0.4
  )
  
  return (weightCoefficient * baseHourlyWeight)
}

xLabel = 'Day (n-2)                          Day (n-1)                          Day (n)'

p <- expand.grid(Hour = 10:14, Day = 1:3) %>%
  mutate(Index = row_number()) %>%
  mutate(Group = 3) %>%
  mutate(Weight = get_weight(Day, Hour, Group)) %>%
  ggplot(aes(x = Index, y = Weight)) +
    geom_col() +
    theme_bw(base_size = 12) +
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      legend.spacing = unit(0, "mm"),
      panel.grid = element_blank(),
      legend.position = "none",
      legend.justification = "left",
      legend.box.spacing = unit(0, "pt"),
      legend.box.margin = margin(0,0,0,0),
      legend.title = element_blank(),
      axis.text.x = element_blank()
    ) +
    scale_x_continuous(
      name = xLabel,
      breaks = c(0.5, 5.5, 10.5, 15.5),
      limits = c(0.5, 15.5)
    ) +
    scale_y_continuous(
      name = 'Weight',
      breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125),
      limits = c(0, 0.135)
    )

ggsave('multiyear-weights.pdf', plot=p, width=6, height=2)
