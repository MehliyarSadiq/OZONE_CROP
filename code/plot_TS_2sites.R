library(ggplot2); library(gridExtra)

setwd('~/Desktop/analysis/data/dose/')
load('dose_2sites.RData')
df = rbind(dose_2010, dose_2011, dose_2012, dose_2013, dose_2014, dose_2015, dose_2016, dose_2017, dose_2018)

# pod
p1 = ggplot(df, aes(x = one_year, y = pod, group = factor(year))) +
  geom_line(aes(color = factor(year))) +
  labs(x = 'day', y = 'POD', title = "POD") +
  facet_wrap(~region) +
  theme(legend.title = element_blank())
print(p1)

p2 = ggplot(df, aes(x = one_year, y = flux, group = factor(year))) +
  geom_line(aes(color = factor(year))) +
  labs(x = 'day', y = 'flux', title = "Flux") +
  facet_wrap(~region) +
  theme(legend.title = element_blank())
print(p2)

# humidity
p3 = ggplot(df, aes(x = one_year, y = fd, group = factor(year))) +
  geom_line(aes(color = factor(year))) +
  ylim(0,1) +
  labs(x = 'day', y = 'Humidity factor', title = " ") +
  facet_wrap(~region) +
  theme(legend.title = element_blank())
print(p3)

grid.arrange(p1,p3, nrow = 2)

