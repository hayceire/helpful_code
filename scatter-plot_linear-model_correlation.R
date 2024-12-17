#DRAFT

# NEED TO FIGURE OUT HOW TO GET R2 VALUES TO MAP CORRECTLY

correlation.grid <- function(my_tidy, my_populations, my_parent, my_x_var, my_x_label, my_y_var, my_y_label, h_shift, v_shift){
  
  #subset df with manually gated data
  temp.tidy <- my_tidy %>%
    dplyr::filter(population_gate %in% my_populations) %>%
    dplyr::filter(parent_gate_name == my_parent)
  
  # change to factor so plots are in the desired order
  temp.tidy$population_gate <- factor(temp.tidy$population_gate, levels = unique(my_populations))
  
  #calculate correlation
  corr.df <- temp.tidy %>%
    tidyr::drop_na(Delta_BMI_Value) %>%
    dplyr::group_by(population_gate) %>% 
    dplyr::summarize(R_squared = cor({{my_x_var}}, {{my_y_var}}, method = "spearman")^2)
  
  #rename facets
  desired.facet.titles <- unique(temp.tidy$population_gate)
  current.facet.titles <- unique(temp.tidy$population_name)
  names(desired.facet.titles) <- current.facet.titles
  my_labels <- desired.facet.titles
  
  #make scatter plot with linear regression model
  p1 <- ggplot(temp.tidy, aes(x= {{my_x_var}}, y= {{my_y_var}})) +
    ggbeeswarm::geom_beeswarm(colour = "black", size = 3) +
    ggplot2::geom_smooth(method=lm , color="red", fill="gray", se=TRUE) +
    #2% and 10% spaces are respectively added at the bottom and the top of the plot
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)), limits = c(NA, NA)) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02)), limits = c(NA, NA)) +
    labs(caption = temp.tidy$full_parent_path, y = my_y_label, x= my_x_label) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size= 14, face = "bold"),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  x_pos <- min(temp.tidy[{{my_x_var}}])
  y_pos <- max(temp.tidy[{{my_y_var}}])
  
  # add R^2 to plot
  p2 <- p1 + geom_text(data=corr.df, aes(label=paste0('R^2 == ',round(R_squared,2))), parse = TRUE, color='black', x= x_pos, y= y_pos, hjust= h_shift, vjust= v_shift)
  
  # facet_grid
  p3 <- p2 + ggh4x::facet_grid2(~population_gate, scales = "free_y", independent = "y",
                                labeller = labeller(population_gate = my_labels)) +
    theme(strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold"),
          # Change spacing between facets on both axis
          theme(panel.spacing = unit(3, "lines")))
  
  #return(p3)
  #return(temp.tidy)
  return(corr.df)
  
}