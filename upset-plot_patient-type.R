#UPSET PLOT FROM FLOW DATA

#NOTE: This plot requires boolean combination gates

upset.plot <- function(my_tidy, my_parent_gate_name, my_gate_type){
  
  #save my_tidy as a temp df
  my.temp.df <- my_tidy
  
  #subset df
  subset.df <- my.temp.df %>% 
    dplyr::filter(parent_gate_name == my_parent_gate_name) %>%
    dplyr::filter(gate_type == my_gate_type) %>% 
    dplyr::select(sample_name, count, population_gate, Patient_Type, y_axis_title) %>%
    #replace plus sign with "pos"
    dplyr::mutate(population_gate = gsub("+", "pos_", population_gate, fixed = TRUE)) %>%
    #replace minus sign with "neg"
    dplyr::mutate(population_gate = gsub("-", "neg_", population_gate, fixed = TRUE)) %>%
    #create a column 'split_pop' by splitting the strings in population gate, separated by underscore
    dplyr::mutate(split_pop = strsplit(population_gate, split = "_")) %>%
    #need to indicate that calculations below are by row NOT colums
    dplyr::rowwise() %>%
    dplyr::mutate(new_pop = list(split_pop[grepl("pos", split_pop)])) %>%
    dplyr::mutate(new_pop = list(gsub("pos", "", new_pop))) %>% 
    dplyr::mutate(Patient_Type_Label = dplyr::recode(Patient_Type,
                                                     'HC_Null_Healthy' = 'Healthy',
                                                     'HC_Null_Obese' = 'Obese',
                                                     'BS_Pre_Obese' = 'Pre Surgery',
                                                     'BS_Post_Obese' = 'Post Surgery'))
  
  #define color palette (based on ggsci::pal_nejm("default")(4)[1])
  patient_type_palette <- c(HC_Null_Healthy = "gray",
                            HC_Null_Obese = "#BC3C29FF",
                            BS_Pre_Obese = "#0072B5FF",
                            BS_Post_Obese = "#E18727FF")
  # build plot
  g <- ggplot(subset.df, aes(x = new_pop, y = count))+
    geom_boxplot(outlier.shape = NA, colour = "black") +
    ggbeeswarm::geom_quasirandom(aes(fill = Patient_Type), shape = 21, colour = "black", size = 3)+
    scale_fill_manual(values = patient_type_palette) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(y = subset.df$y_axis_title, x = "") +
    ggupset::scale_x_upset(order_by = "freq")
  
  #facet wrap
  final.plot <- g + ggplot2::facet_wrap(~Patient_Type_Label) + theme(panel.spacing=unit(3,"lines"))
  
  return(final.plot)
  
}