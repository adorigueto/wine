setwd("C:/Coding/R/05_Wine_Reviews")
renv::init()

library(ggplot2)
library(viridis)
library(tidyr)

head(wine)
nrow(wine)

# Eliminate some columns
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
  ylab("Preço") +
  xlab("") +
  geom_boxplot(width=0.05) +
  annotate(geom="text", x=0.65, y=0, label="* Amostra sem outliers")
