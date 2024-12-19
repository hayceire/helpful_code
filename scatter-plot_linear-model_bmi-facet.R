#linear model, facet by population name (cols) and bmi category (rows)
#independent axes

linear.model.grid <- function(my_tidy, my_populations, my_parent, my_x_var, my_x_label, my_y_var){
  
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
  
  bmi_cat_palette <- c('Healthy Weight' = "gray", 'Obesity' = "#BC3C29FF")
  
  #make scatter plot with linear regression model
  p1 <- ggplot(temp.tidy, aes(x= {{my_x_var}}, y= {{my_y_var}}, color = BMI_CDC_Category, group = BMI_CDC_Category)) +
    geom_smooth(method = "lm", se = TRUE, aes(fill = BMI_CDC_Category),  color = "black")+
    ggbeeswarm::geom_beeswarm(aes(fill = BMI_CDC_Category), shape = 21, color = "black", size = 3) +
    scale_fill_manual(values = bmi_cat_palette) +
    #5% spaces are added at the bottom and the top of the plot
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)), limits = c(NA, NA)) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.1)), limits = c(NA, NA)) +
    labs(caption = temp.tidy$full_parent_path, y = temp.tidy$y_axis_title, x= my_x_label) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size= 14, face = "bold"),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  # facet_grid
  p2 <- p1 + ggh4x::facet_grid2(BMI_CDC_Category~population_gate, scales = "free", independent = "all",
                                labeller = labeller(population_gate = my_labels)) +
    theme(strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold"),
          # Change spacing between facets on both axis
          theme(panel.spacing = unit(3, "lines")))
  
  return(p2)
  
}