#!/usr/local/bin/Rscript
library(tidyverse)

add_plot_metadata <- function (data) {
  data %>% 
  mutate(Filename = gsub('.*/', '', Name)) %>%
  mutate(Filename = gsub('.jpg', '', Filename)) %>%
  mutate(Filename = gsub('dc_', '', Filename)) %>%
  separate(Filename, sep='__', into=c('Filename', 'Camera_ID')) %>%
  separate(Filename, sep='_', into=c('Year', 'Day', 'Time', 'Dataset')) %>%
  mutate(Time = gsub('\\+.*', '', Time)) %>%
  mutate(Hour = as.integer(substr(Time, 1, 2))) %>%
  mutate(Minute = as.integer(substr(Time, 3, 4))) %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(Day = as.integer(Day)) %>%
  select(-Time)
}

meanGcc <- read.csv('../../data/tky-2006-mean-gcc.csv');
meanRcc <- read.csv('../../data/tky-2006-mean-rcc.csv') %>% select(MeanRcc)

values <- cbind(meanGcc, meanRcc) %>% as.tibble %>%
  add_plot_metadata

p <- values %>%
  ggplot() +
  geom_point(aes(x = Day, y = MeanGcc, col='Gcc'), size=0.5) +
  geom_point(aes(x = Day, y = MeanRcc, col='Rcc'), size=0.5) + 
  scale_colour_manual(values=c(rgb(0, 0.75, 0),
                               rgb(0.75, 0, 0)),
                      labels=c(expression(g[cc]),
                               expression(r[cc]))
                      ) +
  ylim(0.25, 0.525) +
  xlab('Day of the year') +
  ylab('Mean coefficient') +
  theme_bw(base_size=12) +
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"),
    legend.spacing = unit(0, "mm"),
    legend.justification = "left",
    legend.box.spacing = unit(0, "pt"),
    legend.box.margin = margin(0,0,0,0),
    legend.title = element_blank()
  )

df.redLines <- tribble(~X,50,165,295,345) %>% mutate(Y=1)

p +
    geom_vline(data=df.redLines, aes(xintercept=X), size=0.75, color="red") +
    geom_label(data=df.redLines, aes(x=X, y=0.5125, label = X), size=2.75)

ggsave('tky-2006-mean-gcc-rcc.pdf', width = 8, height = 2.5)
