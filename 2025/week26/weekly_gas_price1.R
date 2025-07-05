library(tidyverse)
library(dlookr)
weekly_gas_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')
diagnose(weekly_gas_prices)
glimpse(weekly_gas_prices)
subtitle <- "Gas prices has increased significantly since 90s. A gallon of gas weas nearly a dollar almost a decade then things have changed and we end up paying three times for a gallon of gas."
df <- weekly_gas_prices |> 
  filter(grade == "regular", formulation == "all")
ggplot(df, aes(date, price)) +
  geom_line() +
  labs(x = "", 
       y = "Price", 
       title = "Gas Prices: A Threefold Increase Since the 1990s",
       subtitle = "Gasoline prices have risen dramatically since the 1990s. Back then, a gallon of gas cost around a dollar; today, we're paying nearly three times that amount.",
       caption = "Data: EIA | Vis: MhKirmizi")+
  hrbrthemes::theme_ft_rc(base_size = 14) +
  theme(
    plot.title = element_text(size = 36, face = "bold", hjust = .5), 
    plot.subtitle = element_text(size =20, hjust = .5)
  )
ggsave("weekly_gas_price.png", width = 1920, height = 1080, units = "px", dpi = 132)

