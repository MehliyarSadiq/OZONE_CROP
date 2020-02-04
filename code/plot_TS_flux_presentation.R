library(ggplot2); library(gridExtra)

setwd('~/Desktop/analysis/data/dose/')
load('dose_2sites.RData')

df_harvest = data.frame(region = c('China', 'US'), harvest = c(228, 286))

p2 = ggplot(dose_2010, aes(x = one_year, y = flux)) +
  geom_line() +
  geom_segment(aes(x = harvest-90, y = 3, xend = harvest, yend = 3), data = df_harvest, color='red') +
  #geom_rect(aes(xmin = harvest-90, xmax = harvest, ymin = 3, ymax = Inf), data = df_harvest) +
  facet_wrap(~region) +
  labs(x = 'day', y = 'flux', title = "Flux") +
  theme(legend.title = element_blank())
print(p2)

