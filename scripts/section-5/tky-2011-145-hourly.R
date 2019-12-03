#!/usr/bin/Rscript
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
  filter(Year == 2011) %>%
  filter(Day == 145) %>%
  filter(Metric_Type == 'HSV') %>%
  select(-contains('Gcc')) %>%
  select(-Dataset, -Camera_ID, -Year, -Metric_Type) %>%
  # Process data
  mutate(
    Color.Code.H = hex(HSV(HSV_Bin, 1, 1)),
    Color.Code.Mean = hex(HSV(HSV_Bin, HSV_SMean, HSV_VMean)),
    Color.Code.Mode = hex(HSV(HSV_Bin, HSV_SMode, HSV_VMode))
  ) %>%
  # Make it tidy
  gather(Variable, Value, -contains('HSV'), -Day, -Hour, -Considered_Pixels) %>%
  # Add style and key
  mutate(Style = gsub('Color.Code.', '', Variable)) %>%
  mutate(Key = paste(Style, Hour, Day, HSV_Bin, sep='_')) %>%
  mutate(Key = factor(Key, levels=Key)) %>%
  # Cleanup
  select(-Considered_Pixels, -contains('Mean'), -contains('Mode'), -Variable) %>%
  arrange(Style, Hour, Day, HSV_Bin) %>%
  # Prepare for plot, calculate cumsums
  rename(Bin = HSV_Bin) %>%
  rename(Color = Value) %>%
  group_by(Style, Hour, Day) %>%
  arrange(Bin) %>%
  filter(HSV_H != 0) %>%
  mutate(
    Y.min = cumsum(HSV_H) - HSV_H,
    Y.max = cumsum(HSV_H)
  ) %>%
  ungroup %>%
  arrange(Style, Hour, Day, Bin)

palette <- data %>% pull(Color)
names(palette) <- data %>% pull(Key)

p <- data %>%
  filter(Style == 'H') %>%
  mutate(Fct = case_when(
    Hour == 7 ~ 'Hour 07',
    Hour == 9 ~ 'Hour 09',
    TRUE ~ paste0('Hour ', Hour)
  )) %>%
  ggplot() +
    scale_fill_manual(values=palette) +
    geom_rect(
      aes(xmin=Day, xmax=Day+1, ymin=Y.min, ymax=Y.max, fill=Key)
    ) +
    get_theme(base_size = 18) +
    theme(
      strip.text.y = element_blank(),
      panel.spacing = unit(0, 'mm'),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    xlab('Day 145') +
    facet_grid(Style ~ Fct)

ggsave('tky-2011-145-hourly.pdf', plot=p, width=6, height=6)
