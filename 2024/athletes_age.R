library(tidyverse)
library(hrbrthemes)
library(dlookr)
tuesdata <- tidytuesdayR::tt_load(2024, week = 32)
olympics <- tuesdata$olympics

dim(olympics)
diagnose(olympics)
diagnose_numeric(olympics)
diagnose_outlier(olympics) |> 
  filter(outliers_cnt>0)
describe(olympics)
normality(olympics)

data_for_plot <- olympics |> 
  select(-id) |> 
  group_by(sex, year) |> 
  summarise_if(is.numeric, mean, na.rm = TRUE) |> 
  mutate(sex = case_when(sex == "F" ~ "Female", 
                   sex == "M" ~ "Male")) |> 
  rename(Sex = sex, 
         Age = age, 
         Year = year, 
         Height = height)

ggplot(data_for_plot, aes(Year,Age, color = Sex)) +
  geom_point(size = 3, alpha =.8)+
  geom_hline(yintercept = 26, linetype = "dashed", color = "#B0B0B0", linewidth = 0.8) +
  scale_color_manual(values = c("Female" = "#F4A261", "Male" = "#2A9D8F")) +
  scale_x_continuous(breaks = seq(1890, 2016, by= 20)) +
  labs(
    title = "Peak Performance at age of 26", 
    caption = "Data = www.sports-reference.com, Created by MhKirmizi"
  ) +
  theme_modern_rc() +
  theme(
    legend.position = c(.9,.8), 
    legend.justification = c(.9,.8),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    plot.title = element_text(size= 32, face = "bold", vjust = .5, hjust = .5), 
   plot.subtitle = element_text(size = 24, vjust = .5, hjust = .5), 
    plot.caption = element_text(size = 12)
  )
  ggsave("athletes_age_plot.pdf", width = 12, height = 8)
  
  