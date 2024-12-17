correlation.grid <- function(my_tidy, my_populations, my_parent, my_x_var, my_x_label, my_y_var){
  
  #subset df with manually gated data
  temp.tidy <- my_tidy %>%
    dplyr::filter(population_gate %in% my_populations) %>%
    dplyr::filter(parent_gate_name == my_parent)
  
  # change to factor so plots are in the desired order
  temp.tidy$population_gate <- factor(temp.tidy$population_gate, levels = unique(my_populations))
  
  #rename facets
  desired.facet.titles <- unique(temp.tidy$population_gate)
  current.facet.titles <- unique(temp.tidy$population_name)
  names(desired.facet.titles) <- current.facet.titles
  my_labels <- desired.facet.titles
  
  patient_type_palette <- c(HC_Null_Healthy = "gray", HC_Null_Obese = "#BC3C29FF")
  
  #make scatter plot with linear regression model
  p1 <- ggplot(temp.tidy, aes(x= {{my_x_var}}, y= {{my_y_var}})) +
    ggplot2::geom_smooth(method=lm , color="red", fill="gray", se=TRUE) +
    ggbeeswarm::geom_beeswarm(aes(fill = Patient_Type), shape = 21, colour = "black", size = 3) +
    scale_fill_manual(values = patient_type_palette) +
    #5% spaces are added at the bottom and the top of the plot
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), limits = c(NA, NA)) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.05)), limits = c(NA, NA)) +
    labs(caption = temp.tidy$full_parent_path, y = temp.tidy$y_axis_title, x= my_x_label) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size= 14, face = "bold"),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  # facet_grid
  p2 <- p1 + ggh4x::facet_grid2(~population_gate, scales = "free_y", independent = "y",
                                labeller = labeller(population_gate = my_labels)) +
    theme(strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold"),
          # Change spacing between facets on both axis
          theme(panel.spacing = unit(3, "lines")))
  
  return(p2)
  
}