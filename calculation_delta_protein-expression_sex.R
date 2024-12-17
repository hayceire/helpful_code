# Calculation #2: Female/Male HC - Female/Male Obese ----

sex.bmi.delta.calculation <- function(my_tidy, my_population){
  # keep tidy dataframe as a temporary df
  temp.df <- my_tidy
  
  # only include marker of interest in new df
  subset.df <- temp.df %>%
    dplyr::filter(population_name %in% my_population) %>% 
    dplyr::select(BMI_Category, Sex, count, population_name)
  
  math.df <- subset.df %>% 
    dplyr::group_by(population_name) %>% 
    dplyr::summarize(healthy_female_avg = mean(count[BMI_Category == 'Healthy' & Sex == 'Female']),
                     obese_female_avg = mean(count[BMI_Category == 'Obese' & Sex == 'Female']),
                     healthy_male_avg = mean(count[BMI_Category == 'Healthy' & Sex == 'Male']),
                     obese_male_avg = mean(count[BMI_Category == 'Obese' & Sex == 'Male'])) %>% 
    dplyr::mutate(delta_exp_female = obese_female_avg - healthy_female_avg) %>% 
    dplyr::mutate(delta_exp_male = obese_male_avg - healthy_male_avg) %>% 
    dplyr::mutate_if(is.numeric, ~round(., 2))
  
  return(math.df)
}