census_data_analysis %>% 
  mutate(
    generation = case_when(
      genstat == 1 ~ "First Generation",
      genstat %in% c(2, 3) ~ "Second Generation",
      genstat == 4 ~ "Third Generation and Beyond",
      TRUE ~ "Not Applicable"
    ),
    agegrp = case_when(
      agegrp == 1 ~ "0 to 4 years",
      agegrp == 2 ~ "5 to 9 years",
      agegrp == 3 ~ "5 to 9 years",
      agegrp == 4 ~ "10 to 14 years",
      agegrp == 5 ~ "10 to 14 years",
      agegrp == 6 ~ "15 to 19 years",
      agegrp == 7 ~ "15 to 19 years",
      agegrp == 8 ~ "20 to 24 years",
      agegrp == 9 ~ "25 to 29 years",
      agegrp == 10 ~ "30 to 34 years",
      agegrp == 11 ~ "35 to 39 years",
      agegrp == 12 ~ "40 to 44 years",
      agegrp == 13 ~ "45 to 49 years",
      agegrp == 14 ~ "50 to 54 years",
      agegrp == 15 ~ "55 to 59 years",
      agegrp == 16 ~ "60 to 64 years",
      agegrp == 17 ~ "65 to 69 years",
      agegrp == 18 ~ "70 to 74 years",
      agegrp == 19 ~ "75 to 79 years",
      agegrp == 20 ~ "80 to 74 years",
      agegrp == 21 ~ "85 years or older",
      TRUE ~ "Not applicable"
    ),
    file_year = as.numeric(file_year)
  ) %>% 
  filter(
    generation != "Not Applicable",
    !agegrp %in% c("Not applicable", "0 to 4 years", "5 to 9 years", "10 to 14 years")
  ) %>% 
  group_by(file_year, generation, agegrp) %>% 
  summarise(
    pop = sum(weight)
  ) %>% 
  ggplot(aes(x = file_year, y = pop, fill = generation)) +
  geom_area() + 
  # geom_line() +
  # geom_point() +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_continuous(breaks = c(2001, 2006, 2011, 2016)) +
  theme_light() +
  facet_wrap(~agegrp) -> p
  
ggplotly(p)
