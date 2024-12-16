
delta.bmi.diff.bar <- function(my_populations, my_parent, my_calc_df, my_parameter){
  # create a vector of plot population
  plot.pops <- paste0(my_parent, "_", my_populations)
  
  #subset tidy data
  temp.df <- my_calc_df %>% dplyr::filter(population_name %in% plot.pops)
  
  # remove parent string from population name
  remove.pattern <- paste0(my_parent, "", "_")
  
  temp.df <- temp.df %>%
    dplyr::mutate(population_name = gsub(remove.pattern, "", population_name, fixed = TRUE))
  
  # plot
  g <- ggplot2::ggplot(temp.df, aes(y = population_name,
                                    x = {{my_parameter}},
                                    fill = as.character(sign({{my_parameter}}))))
  g <- g +
    geom_col(color = "black") + 
    ggprism::theme_prism() +
    scale_x_continuous(expand = expansion(mult = 0.1)) + 
    scale_fill_manual(values = c("gray", "#E18727FF")) +
    geom_text(aes(label={{my_parameter}}), vjust = -0.2, hjust = -0.2, size =5)+
    #scale_y_discrete(labels = my_populations) +
    labs(x = "Obesity - Healthy Control", y = "", fill = "Trend") +
    theme(legend.position = "none",
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  return(g)
  
}
