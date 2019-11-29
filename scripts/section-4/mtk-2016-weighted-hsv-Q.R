#!/usr/local/bin/Rscript
library(tidyverse)
library(colorspace)
library(cowplot)

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
  Hour = c(9, 10, 11, 12, 13, 14, 15),
  Weight = c(0.05, 0.05, 0.1, 0.3, 0.3, 0.15, 0.05)
)

relevant_hours <- df.weights %>% filter(Weight != 0) %>% pull(Hour)

hourly_data <- read_csv(
  '../../data/mtk.csv.gz',
  col_types = get_col_types()
) %>%
  filter(Year == 2016) %>%
  filter(Metric_Type == 'HSV') %>% 
  select(-contains('gcc')) %>%
  filter(Hour %in% relevant_hours) %>%
  group_by(Year, Day, Hour, HSV_Bin) %>%
  slice(1) %>%
  ungroup %>%
  left_join(df.weights, by=c('Hour'))


weighted_data <- hourly_data %>%
  mutate(HSV_H = Weight * HSV_H) %>%
  mutate(HSV_SMean = Weight * HSV_SMean) %>%
  mutate(HSV_VMean = Weight * HSV_VMean) %>%
  mutate(HSV_SMode = Weight * HSV_SMode) %>%
  mutate(HSV_VMode = Weight * HSV_VMode) %>%
  group_by(Day, HSV_Bin, Considered_Pixels) %>%
  summarize(
    HSV_H = as.integer(sum(HSV_H)),
    HSV_SMean = sum(HSV_SMean, na.rm = TRUE),
    HSV_VMean = sum(HSV_VMean, na.rm = TRUE),
    HSV_SMode = sum(HSV_SMode, na.rm = TRUE),
    HSV_VMode = sum(HSV_VMode, na.rm = TRUE)
  ) %>%
  ungroup

hourly_histograms <- hourly_data %>%
  select(Year, Day, Hour, Metric_Type, HSV_Bin, HSV_H) %>%
  mutate(
    Bin = case_when(
      Metric_Type == 'HSV' ~ HSV_Bin
    ),
    Bin_Value = case_when(
      Metric_Type == 'HSV' ~ HSV_H
    )
  ) %>%
  select(-HSV_Bin, -HSV_H)

weighted_histogram <- weighted_data %>%
  mutate(Year = 2016) %>%
  mutate(Metric_Type = 'HSV') %>%
  select(Year, Day, Metric_Type, HSV_Bin, HSV_H) %>%
  mutate(
    Bin = case_when(
      Metric_Type == 'HSV' ~ HSV_Bin
    ),
    Bin_Value = case_when(
      Metric_Type == 'HSV' ~ HSV_H
    )
  ) %>%
  select(-HSV_Bin, -HSV_H)

a <- hourly_histograms %>%
  select(Year, Day, Hour, Metric_Type, Bin, Bin_Value)
b <- weighted_histogram %>%
  select(Year, Day, Metric_Type, Bin, Bin_Value)

a <- a %>% filter(Day %in% b$Day)
b <- b %>% filter(Day %in% a$Day)

df.EMD <- a %>%
  left_join(b, by=c('Year', 'Day', 'Metric_Type', 'Bin')) %>%
  rename(
    P = Bin_Value.x,
    Q = Bin_Value.y
  ) %>%
  group_by(Year, Day, Hour, Metric_Type) %>%
  mutate(`P_i-Q_i` = lag(lead(P-Q))) %>%
  mutate(`EMD_i` = ifelse(is.na(`P_i-Q_i`), 0, `P_i-Q_i`)) %>%
  mutate(`EMD_i` = cumsum(`EMD_i`)) %>%
  summarize(Distance = sum(abs(`EMD_i`))) %>%
  ungroup %>%
  arrange(Year, Day, Metric_Type)

df.EMD.weighted <- df.EMD %>%
  left_join(df.weights, by=c('Hour')) %>%
  mutate(Weighted_Distance = Distance * Weight) %>%
  group_by(Year, Day, Metric_Type) %>%
  summarize(Q = sum(Weighted_Distance))

uncertainty_by_style <- df.EMD.weighted %>%
  mutate(
    Q = case_when(
      Metric_Type == 'Gcc' ~ Q/100,
      Metric_Type == 'HSV' ~ Q/360
    )
  )

hsv_weighted <- weighted_data %>%
  mutate(Year = 2016) %>%
  mutate(Metric_Type = 'HSV') %>%
  filter(Year == 2016) %>%
  filter(Metric_Type == 'HSV')

hsv_cpm_for_plot <- hsv_weighted %>%
  # Compute the color codes
  mutate(
    Color.Code.H = hex(HSV(HSV_Bin, 1, 1)),
    Color.Code.Mean = hex(HSV(HSV_Bin, HSV_SMean, HSV_VMean)),
    Color.Code.Mode = hex(HSV(HSV_Bin, HSV_SMode, HSV_VMode)),
    #Color.Code.Gcc = hex(RGB(Gcc_Mean_R, Gcc_Mean_G, Gcc_Mean_B))
  ) %>% 
  select(
    -HSV_SMean, -HSV_VMean,
    -HSV_SMode, -HSV_VMode,
    #-Gcc_Mean_R, -Gcc_Mean_G, -Gcc_Mean_B
  ) %>%
  # Extract the bin and bin value information
  mutate(
    Bin = case_when(
      Metric_Type == 'HSV' ~ HSV_Bin
      #Metric_Type == 'Gcc' ~ Gcc_Bin
    ),
    Bin_Value = as.integer(case_when(
      Metric_Type == 'HSV' ~ HSV_H
      #Metric_Type == 'Gcc' ~ Gcc_Value
    ))
  ) %>%
  select(-HSV_Bin, -HSV_H) %>%
  select(Year, Day, Metric_Type, Bin, Bin_Value, everything()) %>%
  # Make it tidy
  gather(Style, Color, -Year, -Day, -Metric_Type, -Bin, -Bin_Value) %>%
  mutate(Style = gsub('Color.Code.', '', Style)) %>%
  # Remove useless combinations
  filter(!(Metric_Type == 'Gcc' & Style %in% c('H', 'Mean', 'Mode'))) %>%
  filter(!(Metric_Type == 'HSV' & Style == 'Gcc')) %>%
  # Create the key
  mutate(Key = paste(Year, Day, Bin, Style, sep='_')) %>%
  mutate(Key = factor(Key, levels=Key)) %>%
  # Sort the bins
  arrange(Style, Year, Day, Bin) %>%
  # Calculate the cumsums
  group_by(Style, Year, Day) %>%
  arrange(Bin) %>%
  filter(Bin_Value != 0) %>%
  mutate(
    Y.min = cumsum(Bin_Value) - Bin_Value,
    Y.max = cumsum(Bin_Value)
  ) %>%
  ungroup %>%
  arrange(Style, Year, Day, Bin)

palette <- hsv_cpm_for_plot %>% pull(Color)
names(palette) <- hsv_cpm_for_plot %>% pull(Key)

hsv_cpm_plot <- hsv_cpm_for_plot %>%
  filter(Style == 'H') %>%
  ggplot() +
    scale_fill_manual(values=palette) +
    geom_rect(
      aes(xmin=Day, xmax=Day+1, ymin=Y.min, ymax=Y.max, fill=Key)
    ) +
    get_theme(base_size = 12) +
    theme(
      plot.margin = unit(c(0,0,0,0), 'mm'),
    )
    scale_x_continuous(
      name = 'DOY (2016)',
      breaks = c(0, 50, 100, 150, 200, 250, 300, 350)
    )

quality_plot <- uncertainty_by_style %>%
  filter(Year == 2016) %>%
  filter(Metric_Type == 'HSV') %>%
  ungroup %>%
  mutate(Q = Q/max(Q)) %>%
  ggplot(aes(x = Day, y = Q)) +
    geom_col(width = 1) +
    theme_bw() +
    theme(
      plot.margin = unit(c(1,0,0,0), 'mm'),
      panel.grid = element_blank(),
      legend.position = 'none',
      legend.spacing = unit(0, 'mm'),
      legend.box.spacing = unit(0, 'pt'),
      legend.box.margin = margin(0,0,0,0),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size=10),
    ) +
    ylab("Uncert.\nLevel")

p <- plot_grid(
  quality_plot,
  hsv_cpm_plot, 
  nrow=2,
  ncol = 1,
  align='vh',
  rel_heights=c(1, 3)
)

ggsave('./mtk-2016-weighted-hsv-Q.pdf', plot=p, width=8, height=3)
