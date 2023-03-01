library(dplyr)
library(leaflet)

canadian_map_by_year <-
  function(df, cad_map, year) {
    
    summarized_data <- 
      df %>% 
      filter(
        file_year == year
      ) %>% 
      group_by(
        genstat,
        pr
      ) %>% 
      summarise(
        pop = sum(weight)
      ) %>%
      tidyr::pivot_wider(
        names_from = genstat,
        values_from = pop
      ) %>% 
      left_join(
        df %>% 
          filter(
            file_year == year
          ) %>% 
          group_by(pr) %>% 
          summarise(
            total_pop = sum(weight)
          ),
        by = c("pr")
      ) %>% 
      janitor::clean_names() %>% 
      mutate(
        first_generation_prop = first_generation/total_pop,
        second_generation_prop = second_generation/total_pop,
        third_generation_or_more_prop = third_generation_or_more/total_pop
      ) 
    
    gradient = colorNumeric(c("deeppink", "royalblue", "darkorange"), domain = c(0, 1))
    
    cad_map %>% 
      left_join(summarized_data,
                by = c("PRENAME" = 'pr')) %>% 
      leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addPolygons(
        weight = 0.5, # Grossura das borda
        fillColor = ~gradient(first_generation_prop),
        color = "grey", # Cor das bordas
        fillOpacity = 0.5, # Tranparência
        smoothFactor = 0.25,
        popup = ~paste0(
          PRENAME, "<br>",
          "<hr>",
          "First Generation: ", formatC(floor(first_generation), big.mark = ",", format = "d"),
          " (", round(first_generation_prop*100, digits = 2), "%)", "<br>",
          "Second Generation: ", formatC(floor(second_generation), big.mark = ",", format = "d"),
          " (", round(second_generation_prop*100, digits = 2), "%)", "<br>",
          "Third Generation or More: ", formatC(floor(third_generation_or_more), big.mark = ",", format = "d"),
          " (", round(third_generation_or_more_prop*100, digits = 2), "%)", "<br>",
          "<hr>",
          "<strong>Total Population: ", formatC(floor(total_pop), big.mark = ",", format = "d"), "</strong>"
        )
      ) %>% 
      addLegend(position = "bottomright", 
                pal = gradient,
                values = seq(0, 1, 0.1))
    
  }
