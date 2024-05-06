library(readr)
library(plotly)
library(tidyverse)
library(ggridges)
library(gridExtra)
library(scales)
library(EnvStats)
library(factoextra)
library(FactoMineR)
library(cluster)
library(randomForest)
Housing <- read_csv("Housing.csv")
Sys.setenv("MAPBOX_TOKEN" =
"pk.eyJ1IjoiYmdvbGRiZXJnNTUiLCJhIjoiY2x1a2Q0dGdpMGl5azJsbWk5ajg0bnVseSJ9.IJm-DcfHT955Ye-I2CBMxw")

df <- Housing
rm(Housing)

dat <- df %>%
  mutate( super_rich = case_when(
    price <= quantile(price, probs = c(0.75)) + 1.5*iqr(price) ~ "Regular",
    TRUE ~ "Super Rich")) %>% filter(price <= 4000000)

dat_filt <- dat %>% mutate(bedrooms_filt = case_when(
  bedrooms >=7 ~ "7+",
  bedrooms <= 1 ~ "0-1",
  TRUE ~ as.factor(bedrooms)),
  floors = round(floors,0),
  bathrooms = case_when(
    bathrooms >= 5 ~ "5+",
    TRUE~ as.factor(round(bathrooms,0))))


dat %>% ggplot(aes(sample = price)) + geom_qq() +
  facet_wrap(vars(super_rich), scales = 'free')

dat %>% ggplot(aes(y = price)) + geom_boxplot() +
  facet_wrap(vars(super_rich), scales = 'free')
#Setup
all_vars <- dat %>% dplyr::select(-c(id,date,price))
model_dat <- data.frame(scale(model.matrix(super_rich ~ .,
                                           data = all_vars)[,-1]))
model_dat$super_rich <- as.factor(all_vars$super_rich)
model_dat$price <- dat$price
#Super Rich RF
dat_vars_rich <- model_dat %>% filter(super_rich == "Super Rich") %>%
  dplyr::select(-c(super_rich))
rf_dat_rich <- randomForest(price ~ ., data = dat_vars_rich,
                            ntree = 100)
var_imp_rf_rich <- as.data.frame(rf_dat_rich$importance) %>% 
  rownames_to_column("var_name") %>%
  mutate(var_name = str_to_title(var_name)) %>%
  arrange(desc(IncNodePurity)) %>%
  mutate(rank = rank(-IncNodePurity))
#Regular RF
dat_vars_reg <- model_dat %>% filter(super_rich != "Super Rich") %>%
  dplyr::select(-c(super_rich))
rf_dat <- randomForest(price ~ ., data = dat_vars_reg, ntree = 100)
var_imp_rf <- as.data.frame(rf_dat$importance) %>% 
  rownames_to_column("var_name") %>%
  mutate(var_name = str_to_title(var_name)) %>%
  arrange(desc(IncNodePurity)) %>%
  mutate(rank = rank(-IncNodePurity))
#Make Plots
#Differnece Plot
wait <- full_join(var_imp_rf,var_imp_rf_rich, by = "var_name") %>%
  rename(inc_reg = IncNodePurity.x, inc_rich = IncNodePurity.y,
         rank_reg = rank.x, rank_rich = rank.y) %>%
  mutate(diff = rank_reg - rank_rich, rich_bigger = case_when(
    diff > 0 ~ "Super Rich",
    diff == 0 ~ "Neither",
    TRUE ~ "Regular People"
  ))
p <- wait %>% dplyr::arrange(desc(diff)) %>%
  ggplot(aes(x = diff,
      y = fct_reorder(var_name, diff))) +
  geom_segment(aes(yend = var_name), xend = 0,
               colour = "black", linewidth = 1) +
  geom_point(aes(color = rich_bigger,fill = rich_bigger,
  text = paste("The differnce is",diff,"for variable",var_name)),
                 size = 3, shape = 21) +
labs(x =
"Difference in Variable Rank For Increase Node Purity(Regular - Rich)",
       y = "Variable", color = "More Important Group",
       fill = "More Important Group") +
  theme_bw() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
ggplotly(p, tooltip = "text")
#Alt rf
sr <- var_imp_rf_rich %>% dplyr::arrange(desc(IncNodePurity)) %>%
  ggplot(aes(x = IncNodePurity,
             y = fct_reorder(var_name, IncNodePurity))) +
  geom_segment(aes(yend = var_name), xend = 0, colour = "black", linewidth = 1) +
  geom_point(size = 3, color = "red", shape = 21, fill = 'red') +
  labs(x = "Increase Node Purity", y = "Variable") +
  theme_bw() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

rg <- var_imp_rf %>% dplyr::arrange(desc(IncNodePurity)) %>%
  ggplot(aes(x = IncNodePurity,
             y = fct_reorder(var_name, IncNodePurity))) +
  geom_segment(aes(yend = var_name), xend = 0, colour = "black", linewidth = 1) +
  geom_point(size = 3, color = "red", shape = 21, fill = 'red') +
  labs(x = "Increase Node Purity", y = "Variable") +
  theme_bw() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
subplot(ggplotly(sr),ggplotly(rg), margin = 0.08, titleX = TRUE) %>%
  layout(annotations = list(
    list(x = 0.1 , y = 1.06, text = "Super Rich People", showarrow = FALSE,
         xref='paper', yref='paper'),
    list(x = 0.9 , y = 1.06, text = "Regular People", showarrow = FALSE,
         xref='paper', yref='paper')))

dat %>% ggplot(aes(x = sqft_above, y = price)) +
  geom_smooth(method = 'gam') + scale_y_continuous(labels = comma)  +
  facet_wrap(vars(super_rich), scales = "free")

dat %>% ggplot(aes(x = sqft_living, y = price)) +
  geom_smooth(method = 'gam') + scale_y_continuous(labels = comma)  +
  facet_wrap(vars(super_rich), scales = "free")

dat %>% ggplot(aes(x = long, y = price)) +
  geom_smooth(method = 'lm') + scale_y_continuous(labels = comma)  +
  facet_wrap(vars(super_rich), scales = "free")

dat %>% ggplot(aes(x = sqft_lot15, y = price)) +
  geom_smooth(method = 'gam') + scale_y_continuous(labels = comma)  +
  facet_wrap(vars(super_rich), scales = "free")

dat %>% ggplot(aes(x = sqft_basement, y = price)) +
  geom_smooth(method = 'gam') + scale_y_continuous(labels = comma)  +
  facet_wrap(vars(super_rich), scales = "free")

p <- dat_filt %>% group_by(bedrooms_filt, super_rich) %>%
  summarize(med = median(price)) %>%
  ggplot(aes(x = bedrooms_filt, y = med, fill = bedrooms_filt)) +
  scale_y_continuous(labels = comma) +
  geom_bar(aes(text = paste("Median Price is:",
      paste0("$",prettyNum(med,big.mark=",",scientific=FALSE,trim=TRUE)))
               ),stat = "identity") +
  labs(y = "Median Price") +
  facet_wrap(vars(super_rich), scales = "free_y")
ggplotly(p, tooltip ="text")

p <- dat_filt %>% group_by(bathrooms, super_rich) %>%
  summarize(med = median(price)) %>%
  ggplot(aes(x = bathrooms, y = med, fill = bathrooms)) +
  scale_y_continuous(labels = comma) +
  geom_bar(aes(text = paste("Median Price is:",
  paste0("$",prettyNum(med,big.mark=",",scientific=FALSE,trim=TRUE)))
  ),stat = "identity") +
  labs(y = "Median Price") +
  facet_wrap(vars(super_rich), scales = "free_y")
ggplotly(p, tooltip ="text")

p <- dat_filt %>% group_by(floors, super_rich) %>%
  summarize(med = median(price), n = n()) %>%
  ggplot(aes(x = as.factor(floors), y = med,
             fill = as.factor(floors))) +
  scale_y_continuous(labels = comma) +
  geom_bar(aes(text = paste("Median Price is:",
          paste0("$",prettyNum(med,big.mark=",",scientific=FALSE,trim=TRUE)))
  ),stat = "identity") +
  labs(y = "Median Price") +
  geom_text(aes(label = n),size = 3.5) +
  facet_wrap(vars(super_rich), scales = "free_y")
ggplotly(p, tooltip ="text")

p <- dat_filt %>% group_by(grade, super_rich) %>%
  summarize(med = median(price), n = n()) %>%
  ggplot(aes(x = as.factor(grade), y = med,
             fill = as.factor(grade))) +
  scale_y_continuous(labels = comma) +
  geom_bar(aes(text = paste("Median Price is:",
      paste0("$",prettyNum(med,big.mark=",",scientific=FALSE,trim=TRUE)))
  ),stat = "identity") +
  labs(y = "Median Price") +
  geom_text(aes(label = n),size = 3.5) +
  facet_wrap(vars(super_rich), scales = "free_y")
ggplotly(p, tooltip ="text")

p <- dat_filt %>% group_by(waterfront, super_rich) %>%
  summarize(med = median(price), n = n()) %>%
  ggplot(aes(x = as.factor(waterfront), y = med,
             fill = as.factor(waterfront))) +
  scale_y_continuous(labels = comma) +
  geom_bar(aes(text = paste("Median Price is:",
paste0("$",prettyNum(med,big.mark=",",scientific=FALSE,trim=TRUE)))
  ),stat = "identity") +
  labs(y = "Median Price") +
  geom_text(aes(label = n),size = 3.5) +
  facet_wrap(vars(super_rich), scales = "free_y")
ggplotly(p, tooltip ="text")

#Pct Graphs

p <- dat_filt %>% group_by(waterfront, super_rich) %>%
  summarize(n = n()) %>% ungroup() %>% group_by(super_rich) %>%
  mutate(tot_rich = sum(n), pct_n = n / tot_rich) %>%
  filter(waterfront == 1) %>%
  ggplot(aes(x = as.factor(super_rich), y = pct_n,
             fill = as.factor(super_rich))) +
  scale_y_continuous(labels = comma) +
  geom_bar(aes(text = paste(pct_n)),stat = "identity") +
  labs(y = "Percent of People With a Waterfront Property")
ggplotly(p, tooltip ="text")

plot_mapbox() %>% add_markers(data = dat_filt, x = ~long, y = ~lat,
                              color = ~super_rich,
                              colors = c("#21918c","#440154"),
                              alpha = I(0.7),
        text = ~paste("The Price For This House is:",
        paste0("$",prettyNum(price,big.mark=",",scientific=FALSE,trim=TRUE))),
                      hoverinfo = "text") %>%
  layout(mapbox = list(
    center = list(lat = 47.53005, lon = -122.1139),
    zoom = 9))
