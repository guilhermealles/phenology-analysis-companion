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

get_weight2 <- function(day, hour, group) {
  baseHourlyWeight <- case_when (
    hour == 8 ~ 0.025,
    hour == 9 ~ 0.025,
    hour == 10 ~ 0.05,
    hour == 11 ~ 0.1,
    hour == 12 ~ 0.175,
    hour == 13 ~ 0.25,
    hour == 14 ~ 0.175,
    hour == 15 ~ 0.1,
    hour == 16 ~ 0.05,
    hour == 17 ~ 0.025,
    hour == 18 ~ 0.025
  )

  dayGroupDelta <- group - day
  weightCoefficient <- case_when(
    dayGroupDelta == 1 ~ 0.3,
    dayGroupDelta == 0 ~ 0.7
  )
  
  return (weightCoefficient * baseHourlyWeight)
}

get_weight5 <- function(day, hour, group) {
  baseHourlyWeight <- case_when (
    hour == 8 ~ 0.025,
    hour == 9 ~ 0.025,
    hour == 10 ~ 0.05,
    hour == 11 ~ 0.1,
    hour == 12 ~ 0.175,
    hour == 13 ~ 0.25,
    hour == 14 ~ 0.175,
    hour == 15 ~ 0.1,
    hour == 16 ~ 0.05,
    hour == 17 ~ 0.025,
    hour == 18 ~ 0.025
  )

  dayGroupDelta <- group - day
  weightCoefficient <- case_when(
    dayGroupDelta == 4 ~ 0.2,
    dayGroupDelta == 3 ~ 0.2,
    dayGroupDelta == 2 ~ 0.2,
    dayGroupDelta == 1 ~ 0.2,
    dayGroupDelta == 0 ~ 0.2
  )
  
  return (weightCoefficient * baseHourlyWeight)
}

get_weight7 <- function(day, hour, group) {
  baseHourlyWeight <- case_when (
    hour == 8 ~ 0.025,
    hour == 9 ~ 0.025,
    hour == 10 ~ 0.05,
    hour == 11 ~ 0.1,
    hour == 12 ~ 0.175,
    hour == 13 ~ 0.25,
    hour == 14 ~ 0.175,
    hour == 15 ~ 0.1,
    hour == 16 ~ 0.05,
    hour == 17 ~ 0.025,
    hour == 18 ~ 0.025
  )

  dayGroupDelta <- group - day
  weightCoefficient <- case_when(
    dayGroupDelta == 6 ~ 0.1,
    dayGroupDelta == 5 ~ 0.1,
    dayGroupDelta == 4 ~ 0.15,
    dayGroupDelta == 3 ~ 0.15,
    dayGroupDelta == 2 ~ 0.15,
    dayGroupDelta == 1 ~ 0.15,
    dayGroupDelta == 0 ~ 0.2
  )
  
  return (weightCoefficient * baseHourlyWeight)
}

get_weight10 <- function(day, hour, group) {
  baseHourlyWeight <- case_when (
    hour == 8 ~ 0.025,
    hour == 9 ~ 0.025,
    hour == 10 ~ 0.05,
    hour == 11 ~ 0.1,
    hour == 12 ~ 0.175,
    hour == 13 ~ 0.25,
    hour == 14 ~ 0.175,
    hour == 15 ~ 0.1,
    hour == 16 ~ 0.05,
    hour == 17 ~ 0.025,
    hour == 18 ~ 0.025
  )

  dayGroupDelta <- group - day
  weightCoefficient <- case_when(
    dayGroupDelta == 9 ~ 0.05,
    dayGroupDelta == 8 ~ 0.1,
    dayGroupDelta == 7 ~ 0.1,
    dayGroupDelta == 6 ~ 0.1,
    dayGroupDelta == 5 ~ 0.1,
    dayGroupDelta == 4 ~ 0.1,
    dayGroupDelta == 3 ~ 0.1,
    dayGroupDelta == 2 ~ 0.1,
    dayGroupDelta == 1 ~ 0.1,
    dayGroupDelta == 0 ~ 0.15
  )
  
  return (weightCoefficient * baseHourlyWeight)
}

build_skeleton <- function (imagesPerGroup, imagesPerDay, dataRows) {
  groupsCount = nrow(dataRows)/imagesPerGroup

  groupList <- c()
  indexList <- c()
  for (group in 1:357) {
    groupStart <- (group * imagesPerDay) - (imagesPerGroup-1)
    groupEnd <- (group * imagesPerDay)
    indexList <- c(indexList, groupStart:groupEnd)
    groupList <- c(groupList, rep(group, times = imagesPerGroup))
  }

  return (
    tibble(
      Index = indexList,
      Group = groupList
    ) %>%
    filter(Index >= 1)
  )
}

generate_data_with_skeleton <- function (
  spread_data,
  skeleton,
  mode,
  get_weight_fun
) {
  return(
    spread_data %>%
      slice(skeleton %>% pull(Index)) %>%
      cbind(skeleton) %>%
      as_tibble %>%
      mutate(Weight = get_weight_fun(Day, Hour, Group)) %>%
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
    group_by(Group) %>% 
    filter(sum(Weight) / 360 > 0.99) %>%
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
    arrange(Style, Year, Group, Bin) %>%
    mutate(Mode = mode)
  )
}

data <- read_csv(
  '../../data/mtk.csv.gz',
  col_types = get_col_types()
) %>%
  filter(Camera_ID == 'btp_w') %>%
  filter(Year == 2016) %>%
  filter(Metric_Type == 'HSV') %>%
  select(-contains('Gcc'), -contains('Mean'), -contains('Mode')) %>%
  spread(HSV_Bin, HSV_H) %>%
  filter(Minute == 0) %>%
  group_by(Day) %>%
  filter(n() == 11) %>%
  ungroup()

base_structure <- expand.grid(Hour = 8:18, Day = 1:365) %>%
  as_tibble() %>%
  mutate(Year = 2016) %>%
  mutate(Minute = 0)

spread_data <- base_structure %>%
  left_join(data, by=c('Year', 'Day', 'Hour', 'Minute'))

skeleton.2 <- build_skeleton(
  imagesPerGroup = 22,
  imagesPerDay = 11,
  dataRows = nrow(data)
)

skeleton.5 <- build_skeleton(
  imagesPerGroup = 55,
  imagesPerDay = 11,
  dataRows = nrow(data)
)

skeleton.7 <- build_skeleton(
  imagesPerGroup = 77,
  imagesPerDay = 11,
  dataRows = nrow(data)
)

skeleton.10 <- build_skeleton(
  imagesPerGroup = 110,
  imagesPerDay = 11,
  dataRows = nrow(data)
)

plot.data.2 <- generate_data_with_skeleton(
  spread_data = spread_data,
  skeleton = skeleton.2,
  mode = '02-day window',
  get_weight_fun = get_weight2
)

plot.data.5 <- generate_data_with_skeleton(
  spread_data = spread_data,
  skeleton = skeleton.5,
  mode = '05-day window',
  get_weight_fun = get_weight5
)

plot.data.7 <- generate_data_with_skeleton(
  spread_data = spread_data,
  skeleton = skeleton.7,
  mode = '07-day window',
  get_weight_fun = get_weight7
)

plot.data.10 <- generate_data_with_skeleton(
  spread_data = spread_data,
  skeleton = skeleton.10,
  mode = '10-day window',
  get_weight_fun = get_weight10
)

plot.data <- rbind(
  plot.data.2,
  plot.data.5,
  plot.data.7,
  plot.data.10
)

palette <- plot.data %>% pull(Color)
names(palette) <- plot.data %>% pull(Key)

p <- plot.data %>%
  ggplot() +
    scale_fill_manual(values=palette) +
    geom_rect(
      aes(xmin=Group, xmax=Group+1, ymin=Y.min, ymax=Y.max, fill=Key)
    ) +
    get_theme(base_size = 22) +
    xlab('DOY (2016)') +
    facet_wrap(~ Mode, ncol = 1)

ggsave('./mtk-2016-varying-window-sizes.pdf', plot=p, width=8, height=6)
