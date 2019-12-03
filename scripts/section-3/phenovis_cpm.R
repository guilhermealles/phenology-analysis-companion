#!/usr/bin/Rscript
library(tidyverse)
DATAFILE <- "../../data/dc_TKY_y18_n_2015_grain100_masked.csv"

lowLimit <- 30
highLimit <- 50

data <- read_csv(
  DATAFILE,
  col_types=cols(
    .default = col_double(),
    database = col_character(),
    sequence = col_character()
  )
) %>%
  gather(variable, value, -database, -sequence, -year) %>%
  mutate(sequence = as.integer(sequence)) %>%
  mutate(variable = as.integer(gsub("X", "", variable))) %>%
  mutate(value = as.integer(value))

paletteFilename <- '../../data/phenovis.palette'
palette <- toupper(
  read.csv(
    paletteFilename,
    comment.char="?",
    header=FALSE
  )$V1
)

p <- data %>%
  filter(variable >= lowLimit, variable < highLimit) %>%
  mutate(variable = variable/100) %>%
  group_by(database, year, sequence) %>%
  arrange(variable) %>%
  mutate(Y.max = cumsum(value)/sum(value)) %>%
  mutate(Y.min = (Y.max - value/sum(value))) %>%
  ungroup %>%
  ggplot(aes(fill=as.factor(variable))) +
    geom_rect(
      aes(
        xmin=sequence,
        xmax=sequence+1,
        ymin=Y.min,
        ymax=Y.max
      )
    ) +
    ylim(0,NA) +
    theme_bw() +
    scale_fill_manual(values=palette) +
    guides(
      fill = guide_legend(
        title = expression(g[cc]),
        title.position = "top",
        ncol = 1
      )
    ) +
    xlab("DOY") +
    coord_flip() +
    scale_x_reverse(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_bw(base_size=12) +
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      legend.spacing = unit(1, "mm"),
      panel.grid = element_blank(),
      legend.box.spacing = unit(0, "pt"),
      legend.box.margin = margin(0,0,0,0)
    )

ggsave('./phenovis_cpm.pdf', plot = p, width = 7, height = 6)
