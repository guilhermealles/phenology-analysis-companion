#!/usr/bin/Rscript
library(tidyverse)

library(tidyverse)
library(colorspace)

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

data <- read_csv(
  '../../data/mtk.csv.gz',
  col_types = get_col_types()
) %>%
  filter(Camera_ID == 'btp_w') %>%
  filter(Year == 2016) %>%
  group_by(Year, Day, Hour, HSV_Bin, Gcc_Bin) %>%
  slice(1) %>%
  ungroup %>%
  # Remove unnecessary columns
  select(-Dataset, -Camera_ID, -Minute, -Considered_Pixels) %>%
  # Compute the color codes
  mutate(
    Color.Code.H = hex(HSV(HSV_Bin, 1, 1)),
    Color.Code.Mean = hex(HSV(HSV_Bin, HSV_SMean, HSV_VMean)),
    Color.Code.Mode = hex(HSV(HSV_Bin, HSV_SMode, HSV_VMode)),
    Color.Code.Gcc = hex(RGB(Gcc_Mean_R, Gcc_Mean_G, Gcc_Mean_B))
  ) %>% 
  select(
    -HSV_SMean, -HSV_VMean,
    -HSV_SMode, -HSV_VMode,
    -Gcc_Mean_R, -Gcc_Mean_G, -Gcc_Mean_B
  ) %>%
  # Extract the bin and bin value information
  mutate(
    Bin = case_when(
      Metric_Type == 'HSV' ~ HSV_Bin,
      Metric_Type == 'Gcc' ~ Gcc_Bin
    ),
    Bin_Value = as.integer(case_when(
      Metric_Type == 'HSV' ~ HSV_H,
      Metric_Type == 'Gcc' ~ Gcc_Value
    ))
  ) %>%
  select(-HSV_Bin, -HSV_H, -Gcc_Bin, -Gcc_Value) %>%
  select(Year, Day, Hour, Metric_Type, Bin, Bin_Value, everything()) %>%
  # Make it tidy
  gather(Style, Color, -Year, -Day, -Hour, -Metric_Type, -Bin, -Bin_Value) %>%
  mutate(Style = gsub('Color.Code.', '', Style)) %>%
  # Remove useless combinations
  filter(!(Metric_Type == 'Gcc' & Style %in% c('H', 'Mean', 'Mode'))) %>%
  filter(!(Metric_Type == 'HSV' & Style == 'Gcc')) %>%
  # Create the key
  mutate(Key = paste(Year, Day, Hour, Bin, Style, sep='_')) %>%
  mutate(Key = factor(Key, levels=Key)) %>%
  # Sort the bins
  arrange(Style, Year, Day, Hour, Bin) %>%
  # Calculate the cumsums
  group_by(Style, Year, Day, Hour) %>%
  arrange(Bin) %>%
  filter(Bin_Value != 0) %>%
  mutate(
    Y.min = cumsum(Bin_Value) - Bin_Value,
    Y.max = cumsum(Bin_Value)
  ) %>%
  ungroup %>%
  arrange(Style, Year, Day, Hour, Bin)

palette <- data %>% pull(Color)
names(palette) <- data %>% pull(Key)

p <- data %>%
  filter(Style %in% c('H', 'Mode')) %>%
  mutate(Style = paste0('HSV_', Style)) %>%
  ggplot() +
    scale_fill_manual(values=palette) +
    geom_rect(
      aes(xmin=Day, xmax=Day+1, ymin=Y.min, ymax=Y.max, fill=Key)
    ) +
    get_theme(base_size = 18) +
    xlab('DOY (2016)') +
    facet_grid(Hour~Style)

ggsave('./mtk-2016-hourly-8-18.pdf', plot = p, width = 8, height = 8)
