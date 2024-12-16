metabolic.profile.calculation <- function(my_tidy, my_population){
  # keep tidy dataframe as a temporary df
  temp.df <- my_tidy
  
  # only include marker of interest in new df
  subset.df <- temp.df %>%
    dplyr::filter(population_name == my_population) %>% 
    dplyr::select(Nautilus_Formatted, Condition, count)
  
  metabolic.df <- subset.df %>% 
    dplyr::group_by(Nautilus_Formatted) %>% 
    dplyr::summarise(Glucose_Dependence = (((count[Condition == 'Control'] - count[Condition == 'DG'])/(count[Condition == 'Control'] - count[Condition == 'DGO']))*100),
                     FAO_Capacity = 100-Glucose_Dependence,
                     Mito_Dependence = (((count[Condition == 'Control'] - count[Condition == 'O'])/(count[Condition == 'Control'] - count[Condition == 'DGO']))*100),
                     Glycolytic_Capacity = 100-Mito_Dependence) %>% 
    dplyr::mutate_if(is.numeric, ~round(., 3)) %>% 
    dplyr::mutate_if(is.numeric, funs(replace(., .>100, 100))) %>% 
    dplyr::mutate_if(is.numeric, funs(replace(., .<0, 0))) %>% 
    dplyr::mutate(population_name = my_population)
  
  return(metabolic.df)
}