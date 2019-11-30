#!/usr/local/bin/Rscript
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

data <- read_csv(
  '../../data/mtk.csv.gz',
  col_types = get_col_types()
) %>%
  filter(Camera_ID == 'btp_w') %>%
  filter(Metric_Type == 'HSV') %>%
  filter(Year %in% 2012:2016) %>%
  select(-contains('Gcc'), -contains('Mean'), -contains('Mode')) %>%
  spread(HSV_Bin, HSV_H) %>%
  group_by(Year, Day, Hour, Metric_Type) %>%
  slice(1) %>%
  ungroup %>%
  group_by(Year, Day) %>%
  filter(n() >= 7) %>%
  ungroup() %>%
  arrange(Year, Day, Hour)

base_structure <- expand.grid(
  Hour = 10:14,
  Day = 1:365,
  Year = 2012:2016
) %>%
  as_tibble() %>%
  mutate(Minute = 0)

spread_data <- base_structure %>%
  left_join(data, by=c('Year', 'Day', 'Hour', 'Minute'))

years_list <- spread_data %>%
  split(.$Year)

daysPerGroup = 3
imagesPerDay = 5
imagesPerGroup = daysPerGroup * imagesPerDay

groupsCount = nrow(data)/imagesPerGroup

groupList <- c()
indexList <- c()
for (group in 1:357) {
  groupStart <- (group * imagesPerDay) - (imagesPerGroup-1)
  groupEnd <- (group * imagesPerDay)
  indexList <- c(indexList, groupStart:groupEnd)
  groupList <- c(groupList, rep(group, times = imagesPerGroup))
}

skeleton <- tibble(
  Index = indexList,
  Group = groupList
) %>%
  filter(Index >= 1)

year.2012 <- years_list[['2012']] %>%
  slice(skeleton %>% pull(Index)) %>%
  cbind(skeleton) %>%
  select(Index, Group, everything()) %>%
  as.tibble

year.2013 <- years_list[['2013']] %>%
  slice(skeleton %>% pull(Index)) %>%
  cbind(skeleton) %>%
  select(Index, Group, everything()) %>%
  as.tibble

year.2014 <- years_list[['2014']] %>%
  slice(skeleton %>% pull(Index)) %>%
  cbind(skeleton) %>%
  select(Index, Group, everything()) %>%
  as.tibble

year.2015 <- years_list[['2015']] %>%
  slice(skeleton %>% pull(Index)) %>%
  cbind(skeleton) %>%
  select(Index, Group, everything()) %>%
  as.tibble

year.2016 <- years_list[['2016']] %>%
  slice(skeleton %>% pull(Index)) %>%
  cbind(skeleton) %>%
  select(Index, Group, everything()) %>%
  as.tibble

data_for_plot <- rbind(
  year.2012,
  year.2013,
  year.2014,
  year.2015,
  year.2016
) %>%
  mutate(Weight = get_weight(Day, Hour, Group)) %>%
  gather(
    HSV_Bin,
    HSV_H,
    -Index,
    -Group, 
    -Weight,
    -Dataset, 
    -Camera_ID, 
    -Year, 
    -Day, 
    -Hour, 
    -Minute, 
    -Metric_Type, 
    -Considered_Pixels
    ) %>%
  mutate(HSV_Bin = as.integer(HSV_Bin)) %>%
  group_by(Year, Group) %>% 
  filter(sum(Weight) / 360 == 1) %>%
  ungroup %>%
  mutate(HSV_H = Weight * HSV_H) %>%
  group_by(Year, Group, HSV_Bin) %>%
  summarize(
    HSV_H = as.integer(sum(HSV_H))
  ) %>%
  ungroup %>%
  mutate(Color.Code.H = hex(HSV(HSV_Bin, 1, 1))) %>%
  rename(Bin = HSV_Bin) %>%
  rename(Bin_Value = HSV_H) %>%
  gather(Style, Color, -Group, -Year, -Bin, -Bin_Value) %>%
  mutate(Style = gsub('Color.Code.', '', Style)) %>%
  mutate(Key = paste(Year, Group, Bin, Style, sep='_')) %>%
  mutate(Key = factor(Key, levels=Key)) %>%
  arrange(Style, Year, Group, Bin) %>%
  # Calculate the cumsums
  group_by(Style, Year, Group) %>%
  arrange(Bin) %>%
  filter(Bin_Value != 0) %>%
  mutate(
    Y.min = cumsum(Bin_Value) - Bin_Value,
    Y.max = cumsum(Bin_Value)
  ) %>%
  ungroup %>%
  arrange(Style, Year, Group, Bin)

palette <- data_for_plot %>% pull(Color)
names(palette) <- data_for_plot %>% pull(Key)

p <- data_for_plot %>%
  ggplot() +
    scale_fill_manual(values=palette) +
    geom_rect(
      aes(xmin=Group, xmax=Group+1, ymin=Y.min, ymax=Y.max, fill=Key)
    ) +
    get_theme(base_size = 12) +
    scale_x_continuous(
      name = 'DOY',
      breaks = c(0, 50, 100, 150, 200, 250, 300, 350)
    ) +
    facet_grid(Year ~ Style)

ggsave('mtk-2016-multiday-manyyears.pdf', plot=p, width=8, height=8)
