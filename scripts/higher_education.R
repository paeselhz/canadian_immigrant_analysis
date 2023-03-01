library(dplyr)
library(ggplot2)

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
            weight = weightp,
            pr = provp,
            sex = sexp,
            agegrp,
            cfsize = cfsizep, #Census Family Size
            totinc = totincp, #Total Income
            hdgree = dgreep, #Highest education
            genstat = genstpob
          )
        
      } else {
        
        df <- 
          df %>% 
          select(
            file_year,
            weight,
            pr,
            sex,
            agegrp,
            cfsize, #Census Family Size
            totinc, #Total Income
            hdgree, #Highest education
            genstat
          )
      }
      
      return(df)
      
    }
  )

xx <- 
  census_data_analysis %>% 
  mutate(
    generation = case_when(
      genstat == 1 ~ "First Generation",
      genstat %in% c(2, 3) ~ "Second Generation",
      genstat == 4 ~ "Third Generation and Beyond",
      TRUE ~ "Not Applicable"
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
    ),
    at_least_graduation = 
      ifelse(file_year == "2001" & hdgree %in% c(5, 6, 7, 8, 9, 10), 1,
             ifelse(hdgree %in% c(8:13), 1, 0))
  ) %>% 
  filter(
    generation != "Not Applicable",
    agegrp > 6 # keeping only greater than 18 years old
  ) 

xx %>% 
  group_by(
    file_year, generation
  ) %>% 
  summarise(
    population = sum(weight),
    at_least_graduation = sum(weight * at_least_graduation)
  ) %>% 
  mutate(
    prop_at_least_graduation = at_least_graduation/population
  ) %>% 
  mutate(
    file_year = as.numeric(file_year)
  ) %>% 
  ggplot(aes(x = file_year, y = prop_at_least_graduation, fill = generation)) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = c(2001, 2006, 2011, 2016)) +
  theme_light()

xx %>% 
  mutate(
    sex = ifelse(sex == 1, "Female", "Male"),
    agegrp = as.character(agegrp),
    pr = as.character(pr)
  ) %>% 
  lm(at_least_graduation ~ sex + agegrp + pr + generation * file_year, data = ., weights = weight) %>% 
  summary()


