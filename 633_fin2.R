library(plotly)
library(tidyverse)
library(readr)
data <- read_csv("data.csv")
data$`Alert Triggered` <- as.factor(data$`Alert Triggered`)
data %>% group_by(`Attack Type`, `Alert Triggered`) %>%
  summarize(n=n()) %>% ungroup() %>% group_by(`Attack Type`) %>%
  mutate(full_tot = sum(n), pct_1 = n / full_tot) %>%
  filter(`Alert Triggered` == "1") %>%
  ggplot(aes(x = `Attack Type`, y = pct_1, fill = `Attack Type`)) +
  geom_bar(stat = "identity", position = "dodge")

data %>% group_by(`Network Segment`, `Alert Triggered`,
                  `Severity Level`) %>%
  summarize(n=n()) %>% ungroup() %>% group_by(`Network Segment`,
                                              `Severity Level`) %>%
  mutate(full_tot = sum(n), pct_1 = n / full_tot) %>%
  filter(`Alert Triggered` == "1") %>%
  ggplot(aes(x = `Network Segment`, y = pct_1, fill = `Network Segment`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(`Severity Level`))

#data %>% ggplot(aes(x = `Anomaly Scores`)) + geom_histogram(bins = 15)

px <- data %>%
  ggplot(aes(x = `Alert Triggered`, y = `Anomaly Scores`, fill = `Alert Triggered`)) +
  geom_boxplot() + facet_wrap(vars(`Network Segment`))
ggplotly(px)