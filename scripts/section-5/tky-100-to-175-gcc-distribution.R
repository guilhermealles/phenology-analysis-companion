#!/usr/bin/Rscript
library(tidyverse)
library(colorspace)
library(cowplot)
library(latex2exp)

get_col_types <- function() {
  cols(
    Dataset = col_character(),
    Camera_ID = col_character(),
    Year = col_integer(),
    Day = col_integer(),
    Hour = col_integer(),
    Minute = col_integer(),
    Metric_Type = col_character(),
    Considered_Pixels = col_integer(),
    HSV_Bin = col_integer(),
    HSV_H = col_double(),
    HSV_SMean = col_double(),
    HSV_VMean = col_double(),
    HSV_SMode = col_double(),
    HSV_VMode = col_double(),
    Gcc_Bin = col_integer(),
    Gcc_Value = col_double(),
    Gcc_Mean_R = col_double(),
    Gcc_Mean_G = col_double(),
    Gcc_Mean_B = col_double()
  )
}

get_theme <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.spacing = unit(0, "mm"),
    panel.grid = element_blank(),
    legend.position = "none",
    legend.justification = "left",
    legend.box.spacing = unit(0, "pt"),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
}

df.weights <- tibble(
  Hour = c(7, 9, 10, 12, 13, 15, 16),
  Weight = c(0.05, 0.075, 0.075, 0.3, 0.3, 0.15, 0.05)
)

relevant_hours <- df.weights %>% filter(Weight != 0) %>% pull(Hour)

data <- read_csv(
  '../../data/tky.csv.gz',
  col_types = get_col_types()
) %>%
  filter(Camera_ID == 'y18_d') %>%
  filter(Year >= 2006) %>%
  filter(Hour %in% relevant_hours) %>%
  group_by(Year, Day, Hour, HSV_Bin, Gcc_Bin) %>%
  slice(1) %>%
  ungroup %>%
  left_join(df.weights, by=c('Hour'))

full_days <- data  %>%
  group_by(Year, Day) %>%
  summarize(
    imagesCount = length(Hour %>% unique)
  ) %>%
  ungroup %>%
  filter(imagesCount == 7)

data2 <- full_days %>%
  left_join(data, by=c('Year', 'Day')) %>%
  mutate(
    HSV_H = Weight * HSV_H,
    HSV_SMean = Weight * HSV_SMean,
    HSV_VMean = Weight * HSV_VMean,
    HSV_SMode = Weight * HSV_SMode,
    HSV_VMode = Weight * HSV_VMode,
    Gcc_Value = Weight * Gcc_Value,
    Gcc_Mean_R = Weight * Gcc_Mean_R,
    Gcc_Mean_G = Weight * Gcc_Mean_G,
    Gcc_Mean_B = Weight * Gcc_Mean_B
  ) %>%
  group_by(Year, Day, HSV_Bin, Gcc_Bin) %>%
  summarize(
    HSV_H = as.integer(sum(HSV_H)),
    HSV_SMean = sum(HSV_SMean, na.rm = TRUE),
    HSV_VMean = sum(HSV_VMean, na.rm = TRUE),
    HSV_SMode = sum(HSV_SMode, na.rm = TRUE),
    HSV_VMode = sum(HSV_VMode, na.rm = TRUE),
    Gcc_Value = as.integer(sum(Gcc_Value)),
    Gcc_Mean_R = sum(Gcc_Mean_R, na.rm = TRUE),
    Gcc_Mean_G = sum(Gcc_Mean_G, na.rm = TRUE),
    Gcc_Mean_B = sum(Gcc_Mean_B, na.rm = TRUE)
  ) %>%
  ungroup %>%
  mutate(
    Metric_Type = case_when(
      HSV_Bin == -1 ~ 'Gcc',
      Gcc_Bin == -1 ~ 'HSV'
    )
  )

p <- data %>%
  filter(Year == 2011) %>%
  filter(Day %in% c(100, 125, 150, 175)) %>%
  filter(Metric_Type == 'Gcc') %>%
  select(-contains('HSV')) %>%
  filter(Gcc_Value != 0) %>%
  mutate(Day = paste0('Day ', Day)) %>%
  ggplot(aes(x = Gcc_Bin, y = Gcc_Value)) +
    geom_col(width=0.8) +
    theme_bw(base_size = 12) +
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      legend.spacing = unit(0, "mm"),
      panel.grid = element_blank(),
      legend.position = 'top',
      legend.justification = "left",
      legend.box.spacing = unit(0, "pt"),
      legend.box.margin = margin(0,0,0,0),
      #axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlim(0, 100) +
    facet_wrap(~ Day, ncol = 1, scales = 'free_y') +
    scale_x_continuous(name = TeX("$g_{cc}$ Bin"), breaks = seq(0, 100, 10)) +
    ylab('Normalized size of bins')


ggsave('tky-100-to-175-gcc-distribution.pdf', plot=p, width=6, height=3)
