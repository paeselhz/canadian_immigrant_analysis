age_pyramid_by_province <-
  function(df, year) {
    df %>% 
      filter(
        file_year == year
      ) %>% 
      group_by(
        agegrp, genstat, sex, pr
      ) %>% 
      summarise(
        pop = sum(weight)
      ) %>% 
      ungroup() %>% 
      mutate(
        pop = ifelse(sex == "Female", -pop, pop)
      ) %>% 
      ggplot(aes(x = agegrp, y = pop, fill = interaction(genstat, sex))) +
      geom_col() +
      scale_x_discrete(limits = levels(factor(df$agegrp, levels = age_order))) + 
      scale_fill_manual(values = c("darkorange", "royalblue", "deeppink", "navy", "#008080", "purple"),
                        name = "Gender and\nGeneration",
                        labels = c("Female - 1st Gen", "Female - 2nd Gen", "Female - 3rd Gen or More", 
                                   "Male - 1st Gen", "Male - 2nd Gen", "Male - 3d Gen or more")) +
      scale_y_continuous(labels = abs_comma, expand = c(0, 0)) +
      coord_flip() +
      theme_light() +
      facet_wrap(~ pr, scales = "free_x") +
      theme(legend.position = "bottom",
            axis.text.x = element_blank()) +
      labs(
        title = paste0("Age Pyramid Plot by Province for ", year),
        subtitle = "Data source: StatCan - Census of Population",
        x = "Age Group",
        y = "Population"
      )
  }
