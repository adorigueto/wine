setwd("C:/Coding/R/05_Wine_Reviews")
wine <- read.csv("C:/Coding/R/05_Wine_Reviews/winemag-data-130k-v2.csv/winemag-data-130k-v2.csv")
#renv::init()

library(ggplot2)
library(viridis)
library(tidyr)
library(ggdist)
library(tidyverse)

head(wine)
nrow(wine)

# Eliminate blank rows
wine_wo_na <- wine %>% drop_na()

# Plot price variation
boxplot(wine_wo_na$price)

# Eliminate outliers
outliers <- boxplot(wine_wo_na$price, plot = FALSE)$out
wine_wo_price_out <- subset(wine_wo_na, subset = !(price %in% outliers))

# Filter for some countries
countries <- c("Argentina", "Chile",
               "England", "France", "Italy",
               "Portugal", "Spain", "US")

wine_wo_out_subset_countries <- subset(wine_wo_price_out,
                                       country %in% countries)

# Make a violin plot
ggplot(wine_wo_out_subset_countries,
       aes(x=country, y=price,
           color='palevioletred1', fill=country)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_minimal() +
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ylab("Wine price") +
  xlab("") +
  geom_boxplot(width=0.05) +
  annotate(geom="text", x=2.5, y=0, label="* Outliers removed from sample")


png("wine_rain.png", width = 1000, #height = 600,
    res = 250, pointsize = 1)
wine_wo_out_subset_countries %>%
  
  ggplot(aes(x = country, y = price, fill = country)) +
  
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -.2,
    .width = 0,
    point_color = NA
  ) +
  
  geom_boxplot(
    width = .12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  
  ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = .05,
    stackratio = 0.01,
    layout = "bin"
  ) +
  tidyquant::scale_fill_tq() +
  tidyquant::theme_tq() +
  theme(legend.position = "none") +
  coord_flip()
dev.off()

