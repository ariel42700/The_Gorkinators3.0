library(readr)
library(plotly)
library(tidyverse)
library(ggridges)
library(gridExtra)
library(scales)
library(EnvStats)
Housing <- read_csv("Housing.csv")
Sys.setenv("MAPBOX_TOKEN" =
"pk.eyJ1IjoiYmdvbGRiZXJnNTUiLCJhIjoiY2x1a2Q0dGdpMGl5azJsbWk5ajg0bnVseSJ9.IJm-DcfHT955Ye-I2CBMxw")

df <- Housing
rm(Housing)

dat <- df %>% mutate(bedrooms_filt = case_when(
  bedrooms >=8 ~ "8+",
  TRUE ~ as.factor(bedrooms)),
  bathrooms = case_when(
    bathrooms >= 5 ~ "5+",
    TRUE~ as.factor(round(bathrooms,0))))

p <- dat %>% ggplot(aes(y = price)) + geom_boxplot() +
  scale_y_continuous(labels = comma)
ggplotly(p)

dat %>% ggplot(aes(x = bedrooms_filt, y = price, fill = bedrooms_filt)) +
  geom_violin() +
  geom_boxplot() + scale_y_log10(labels = comma) + labs(y = "Log Ten Price") +
  stat_n_text(size = 3.5)

dat %>% ggplot(aes(x = bathrooms, y = price, fill = bathrooms)) +
  geom_violin() +
  geom_boxplot() + scale_y_log10(labels = comma) + labs(y = "Log Ten Price") +
  stat_n_text()

dat %>% ggplot(aes(x = as.factor(waterfront), y = price,
                   fill = as.factor(waterfront))) +
  geom_violin() +
  geom_boxplot() + scale_y_log10(labels = comma) + labs(y = "Log Ten Price") +
  stat_n_text()

dat %>% ggplot(aes(x = sqft_living, y = price)) + geom_point(alpha = 0.3) +
  scale_y_log10(labels = comma) + labs(y = "Log Ten Price")

plot_mapbox() %>% add_markers(data = dat, x = ~long, y = ~lat,
                              color = ~log10(price),
                              alpha = I(0.7),
        text = ~paste("The Price For This House is:",
        paste0("$",prettyNum(price,big.mark=",",scientific=FALSE,trim=TRUE))),
                      hoverinfo = "text") %>%
  layout(mapbox = list(
    center = list(lat = 47.53005, lon = -122.1139),
    zoom = 9))

