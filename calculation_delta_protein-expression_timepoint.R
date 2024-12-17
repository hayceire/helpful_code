# calculate change in protein expression from pre to post
timepoint.delta.calculation <- function(my_tidy, my_population){
  # keep tidy dataframe as a temporary df
  temp.df <- my_tidy
  
  # only include marker of interest in new df
  subset.df <- temp.df %>%
    dplyr::filter(population_name == my_population) %>% 
    dplyr::select(Patient_Type, PID_Formatted, count)
  
  math.df <- subset.df %>% 
    dplyr::group_by(PID_Formatted) %>% 
    dplyr::summarize(timepoint_delta_expression = ((count[Patient_Type == 'BS_Pre_Obese']) - (count[Patient_Type == 'BS_Post_Obese']))) %>% 
    dplyr::mutate_if(is.numeric, ~round(., 2))
  
  return(math.df)
}

# use apply to interate unique population names over function
delta.protein.list <- lapply(seq_along(unique(matched.df$population_name)), function(i) {
  target.population <- unique(matched.df$population_name)[[i]]
  timepoint.delta.calculation2(my_tidy = matched.df,
                               my_population = target.population)
})

# add names back to plots in list
names(delta.protein.list) <- unique(matched.df$population_name)

# When you supply a column name with the `.id` argument, a new
# column is created to link each row to its original data frame
# in this case the id column is named "population_nam
delta.protein.df <- dplyr::bind_rows(delta.protein.list, .id = "population_name")

delta.protein.join <- delta.protein.df %>% dplyr::left_join(matched.df, by = c('PID_Formatted', 'population_name'))