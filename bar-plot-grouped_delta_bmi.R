grouped.delta.bmi.diff.bar <- function(my_populations, my_parent, my_calc_df){
  # reshape data
  reshape.df <- my_calc_df %>% 
    tidyr::pivot_longer(-population_name) %>%
    dplyr::filter(name == "delta_exp_female" | name == "delta_exp_male")
  
  # create a vector of plot population
  plot.pops <- paste0(my_parent, "_", my_populations)
  
  #subset tidy data
  temp.df <- reshape.df %>% dplyr::filter(population_name %in% plot.pops)
  
  # remove parent string from population name
  remove.pattern <- paste0(my_parent, "", "_")
  temp.df <- temp.df %>% dplyr::mutate(population_name = gsub(remove.pattern, "", population_name, fixed = TRUE))
  
  # plot aesthetics
  g <- ggplot2::ggplot(temp.df, aes(y = forcats::fct(population_name),
                                    x = value,
                                    fill = name))
  # geom bar plot
  g <- g + geom_bar(stat = "identity", position = position_dodge(width = 1), color = "black") + 
    ggprism::theme_prism() +
    scale_fill_manual(values = c("pink", "lightblue")) +
    labs(x = "Healthy Control - Obesity", y = "", fill = "Trend") +
    theme(legend.position = "bottom",
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  # add text labels to bar plot
  g <- g + geom_text(aes(label = round(value, 2)),
                     position = position_dodge(width = 1),
                     hjust = -0.25, vjust = 0)
  
  return(g)
  
}