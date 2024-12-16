# Calculation #1: All HC - All Obese ----

bmi.delta.calculation <- function(my_tidy, my_population){
  # keep tidy dataframe as a temporary df
  temp.df <- my_tidy
  
  # only include marker of interest in new df
  subset.df <- temp.df %>%
    dplyr::filter(population_name %in% my_population) %>% 
    dplyr::select(BMI_Category, count, population_name)
  
  math.df <- subset.df %>% 
    dplyr::group_by(population_name) %>% 
    dplyr::summarize(healthy_avg = mean(count[BMI_Category == 'Healthy']),
                     obese_avg = mean(count[BMI_Category == 'Obese'])) %>% 
    dplyr::mutate(delta_exp = obese_avg - healthy_avg) %>% 
    dplyr::mutate_if(is.numeric, ~round(., 2))
  
  return(math.df)
}

