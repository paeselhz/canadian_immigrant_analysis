library(dplyr)
library(ggplot2)
library(gganimate)

abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Prologue
# Agegrp
# 1 = 0 to 4 years
# 2 = 5 to 6 years
# 3 = 7 to 9 years
# 4 = 10 to 11 years
# 5 = 12 to 14 years
# 6 = 15 to 17 years 
# 7 = 18 to 19 years 
# 8 = 20 to 24 years 
# 9 = 25 to 29 years 
# 10 = 30 to 34 years 
# 11 = 35 to 39 years 
# 12 = 40 to 44 years 
# 13 = 45 to 49 years 
# 14 = 50 to 54 years 
# 15 = 55 to 59 years 
# 16 = 60 to 64 years 
# 17 = 65 to 69 years 
# 18 = 70 to 74 years 
# 19 = 75 to 79 years 
# 20 = 80 to 84 years 
# 21 = 85 years and over 
# 88 = Not available 

# genstat
# 1 = First generation, born outside of Canada
# 2 = Second generation, born in Canada, both parents born outside of Canada
# 3 = Second generation, born in Canada, at least one parent born outside of Canada
# 4 = Third generation or more, born in Canada, both parents born in Canada
# 8 = Not applicable

# sex
# 1 = Female
# 2 = Male

# pr (Province of Residence)
# 10 Newfoundland and Labrador 
# 11 Prince Edward Island 
# 12 Nova Scotia 
# 13 New Brunswick 
# 24 Quebec 
# 35 Ontario 
# 46 Manitoba 
# 47 Saskatchewan 
# 48 Alberta 
# 59 British Columbia 
# 70 Northern Canada 

age_order <-
  c(
    "0 to 4 years", #"5 to 6 years", 
    "5 to 9 years", "10 to 14 years", #"12 to 14 years", 
    "15 to 19 years", #"18 to 19 years", 
    "20 to 24 years",
    "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years",
    "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years",
    "65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 74 years",
    "85 years or older"
  )

proc_files <-
  list.files(
    path = 'data/processed',
    pattern = '.parquet',
    recursive = TRUE,
    full.names = TRUE
  )

census_data_analysis <-
  purrr::map_df(
    proc_files,
    function(x) {
      
      df <- 
        arrow::read_parquet(x)
      
      if(unique(df$file_year) == "2001") {
        
        df <- 
          df %>% 
          mutate(
            agegrp = case_when(
              agep >= 0 & agep <= 4 ~ 1,
              agep >= 5 & agep <= 6 ~ 2,
              agep >= 7 & agep <= 9 ~ 3,
              agep >= 10 & agep <= 11 ~ 4,
              agep >= 12 & agep <= 14 ~ 5,
              agep >= 15 & agep <= 17 ~ 6,
              agep >= 18 & agep <= 19 ~ 7,
              agep >= 20 & agep <= 24 ~ 8,
              agep >= 25 & agep <= 29 ~ 9,
              agep >= 30 & agep <= 34 ~ 10,
              agep >= 35 & agep <= 39 ~ 11,
              agep >= 40 & agep <= 44 ~ 12,
              agep >= 45 & agep <= 49 ~ 13,
              agep >= 50 & agep <= 54 ~ 14,
              agep >= 55 & agep <= 59 ~ 15,
              agep >= 60 & agep <= 64 ~ 16,
              agep >= 65 & agep <= 69 ~ 17,
              agep >= 70 & agep <= 74 ~ 18,
              agep >= 75 & agep <= 79 ~ 19,
              agep >= 80 & agep <= 84 ~ 20,
              agep >= 85 ~ 21,
              TRUE ~ 88
            )
          ) %>% 
          select(
            file_year,
            sex = sexp,
            pr = provp,
            agegrp,
            genstat = genstpob,
            weight = weightp
          )
        
      } else {
        
        df <- 
          df %>% 
          select(
            file_year,
            sex,
            pr,
            agegrp,
            genstat,
            weight
          )
      }
      
      return(df)
      
    }
  )

census_summary_data <- 
  census_data_analysis %>% 
  mutate(
    sex = ifelse(sex == 1, "Female", "Male"),
    genstat = case_when(
      genstat == 1 ~ "First Generation",
      genstat == 2 ~ "Second Generation",
      genstat == 3 ~ "Second Generation",
      genstat == 4 ~ "Third Generation or more",
      TRUE ~ "Not applicable"
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
    pr = case_when(
      pr == 10 ~ "Newfoundland and Labrador",
      pr == 11 ~ "Prince Edward Island",
      pr == 12 ~ "Nova Scotia",
      pr == 13 ~ "New Brunswick",
      pr == 24 ~ "Quebec",
      pr == 35 ~ "Ontario",
      pr == 46 ~ "Manitoba",
      pr == 47 ~ "Saskatchewan",
      pr == 48 ~ "Alberta",
      pr == 59 ~ "British Columbia",
      pr %in% c(60, 70) ~ "Northern Canada",
      TRUE ~ "Not applicable"
    )
  ) %>% 
  filter(
    genstat != "Not applicable",
    agegrp != "Not applicable",
    pr != "Not applicable"
  ) %>% 
  group_by(
    file_year, agegrp, genstat, sex, pr
  ) %>% 
  summarise(
    pop = sum(weight)
  ) %>% 
  ungroup() %>% 
  mutate(
    pop = ifelse(sex == "Female", -pop, pop)
  ) 

purrr::walk(
  c(2001, 2006, 2011, 2016),
  function(year) {
    
    year_data <-
      census_summary_data %>% 
      filter(
        file_year == year
      )
    
    p_country <- 
      year_data %>% 
      ggplot(aes(x = agegrp, y = pop, fill = interaction(genstat, sex))) +
      geom_col() +
      scale_x_discrete(limits = levels(factor(year_data$agegrp, levels = age_order))) + 
      scale_fill_manual(values = c("darkorange", "royalblue", "deeppink", "navy", "#008080", "purple"),
                        name = "Gender and Generation",
                        labels = c("Female - First Generation", "Female - Second Generation", "Female - Third Generation or More", 
                                   "Male - First Generation", "Male - Second Generation", "Male - Third Generation or more")) +
      scale_y_continuous(labels = abs_comma, expand = c(0, 0)) +
      ggtitle(paste0("Age Pyramid Plot - ", year)) +
      xlab("Age") +
      ylab("Population") +
      coord_flip() +
      theme_light()
    
    p_provinces <-
      year_data %>% 
      ggplot(aes(x = agegrp, y = pop, fill = interaction(genstat, sex))) +
      geom_col() +
      scale_x_discrete(limits = levels(factor(year_data$agegrp, levels = age_order))) + 
      scale_fill_manual(values = c("darkorange", "royalblue", "deeppink", "navy", "#008080", "purple"),
                        name = "Gender and Generation",
                        labels = c("Female - First Generation", "Female - Second Generation", "Female - Third Generation or More", 
                                   "Male - First Generation", "Male - Second Generation", "Male - Third Generation or more")) +
      scale_y_continuous(labels = abs_comma, expand = c(0, 0)) +
      ggtitle(paste0("Age Pyramid Plot - ", year)) +
      xlab("Age") +
      ylab("Population") +
      coord_flip() +
      theme_light() +
      facet_wrap(~ pr)
    
    p_provinces_free_y <-
      year_data %>% 
      ggplot(aes(x = agegrp, y = pop, fill = interaction(genstat, sex))) +
      geom_col() +
      scale_x_discrete(limits = levels(factor(year_data$agegrp, levels = age_order))) + 
      scale_fill_manual(values = c("darkorange", "royalblue", "deeppink", "navy", "#008080", "purple"),
                        name = "Gender and Generation",
                        labels = c("Female - First Generation", "Female - Second Generation", "Female - Third Generation or More", 
                                   "Male - First Generation", "Male - Second Generation", "Male - Third Generation or more")) +
      scale_y_continuous(labels = abs_comma, expand = c(0, 0)) +
      ggtitle(paste0("Age Pyramid Plot - ", year)) +
      xlab("Age") +
      ylab("Population") +
      coord_flip() +
      theme_light() +
      facet_wrap(~ pr, scales = "free_y")
    
    ggsave(
      paste0('plots/age_pyramid_country_', year, '.png'), 
      p_country,
      width = 5120,
      height = 3200,
      units = "px"
    )
    
    ggsave(
      paste0('plots/age_pyramid_provinces_', year, '.png'), 
      p_provinces,
      width = 5120,
      height = 3200,
      units = "px"
    )
    
    ggsave(
      paste0('plots/age_pyramid_provinces_free_y_', year, '.png'), 
      p_provinces_free_y,
      width = 5120,
      height = 3200,
      units = "px"
    )
    
  }
)

country_gif <-
  census_summary_data %>% 
  ggplot(aes(x = agegrp, y = pop, fill = interaction(genstat, sex))) +
  geom_col() +
  scale_x_discrete(limits = levels(factor(census_summary_data$agegrp, levels = age_order))) + 
  scale_fill_manual(values = c("darkorange", "royalblue", "deeppink", "navy", "#008080", "purple"),
                    name = "Gender and Generation",
                    labels = c("Female - First Generation", "Female - Second Generation", "Female - Third Generation or More", 
                               "Male - First Generation", "Male - Second Generation", "Male - Third Generation or more")) +
  scale_y_continuous(labels = abs_comma, expand = c(0, 0)) +
  labs(title = "Canada Age Pyramid - {previous_state}", x = "Age", y = "Population",
       subtitle = "Data source: StatCan - Census of Population") + 
  coord_flip() +
  theme_light() +
  transition_states(file_year, transition_length = 2, state_length = 2, wrap = FALSE)

anim_save(
  'plots/age_pyramid_country_gif.gif',
  animation = animate(country_gif, fps = 5),
  width = 1080,
  height = 1080,
  units = "px"
)
