library(tidyverse)
library(gt)

tb <- df |> 
  select(Name, `Drop (ft)`, City, Country) |> 
  gt() |> 
  tab_spanner(label = md('Location'), 
              columns = c('City', 'Country')) |> 
  tab_header(
    title = 'Tallest RollerCoaster Around the Globe'
  ) |> 
  tab_style(
    locations = cells_title(groups = 'title'), 
    style = list(
      cell_text(
        size = 'xx-large', 
        weight = 'bold', 
        color = '#355C7D'
      )
    )
  ) |> 
  tab_style(
    locations = cells_body(Name),
    style = list(
      cell_text(
        size = 'large', 
        weight = 'bold', 
        color = '#355C7D'
      )
    )
  ) |> 
  tab_source_note(
    source_note = md('Data = TripSavvy | Vis: MhKirmizi')
  ) |> 
  tab_style(
    style = cell_text(align = 'center'), 
    locations = cells_body()
  ) |> 
  opt_table_font(
    font = list(
      google_font(name = 'Solway')
    )
  ) |> opt_stylize(style = 6, color = 'cyan') 
gtsave(tb, 'week1.html')
