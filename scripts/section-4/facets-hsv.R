#!/usr/local/bin/Rscript
library(colorspace)
library(tidyverse)

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

histogramsPath <- '../../data/tky-partial.csv.gz'
data <- read_csv(
  histogramsPath,
  col_types=get_col_types()
) %>%
  filter(Year == 2006) %>%
  filter(Metric_Type == 'HSV') %>%
  select(-contains('Gcc')) %>%
  select(-Year, -Dataset, -Camera_ID, -Hour, -Minute) %>%
  select(-Metric_Type) %>%
  mutate(
    Color.Code.H = hex(HSV(HSV_Bin, 1, 1)),
    Color.Code.Mean = hex(HSV(HSV_Bin, HSV_SMean, HSV_VMean)),
    Color.Code.Mode = hex(HSV(HSV_Bin, HSV_SMode, HSV_VMode))
  ) %>%
  gather(Variable, Value, -contains("HSV"), -Day, -Considered_Pixels) %>%
  mutate(Style = gsub("Color.Code.", "", Variable)) %>%
  select(-Considered_Pixels, -contains("Mean"), -contains("Mode"), -Variable) %>%
  arrange(Style, Day, HSV_Bin) %>%
  mutate(Key = paste(Style, Day, HSV_Bin, sep="_")) %>%
  mutate(Key = factor(Key, levels=Key)) %>%
  rename(Bin = HSV_Bin) %>%
  rename(Color = Value) %>%
  group_by(Style, Day) %>%
  arrange(Bin) %>%
  filter(HSV_H != 0) %>%
  mutate(
    Y.min = cumsum(HSV_H) - HSV_H,
    Y.max = cumsum(HSV_H)
  ) %>%
  ungroup %>%
  arrange(Style, Day, Bin) %>%
  select(Style, Day, Bin, everything())

data.palette <- data %>%
  select(Key, Color)

palette <- data.palette$Color
names(palette) <- data.palette$Key

p <- data %>%
  mutate(Style = paste0('HSV_', Style)) %>%
  ggplot(aes(
    xmin=Day,
    xmax=Day+1,
    ymin=Y.min,
    ymax=Y.max,
    fill=Key
  )) +
    scale_fill_manual(values=palette) +
    geom_rect() +
    theme_bw(base_size=12) +
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
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    xlab('DOY') +
    facet_wrap(~Style, ncol=1)

ggsave('./facets-hsv.pdf', plot=p, width=6.5, height= 2.5)
