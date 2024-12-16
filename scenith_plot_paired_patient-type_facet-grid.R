#boxplot + line plots
# only HC is shown as boxplot
# pre/post bs samples connected by line
# requires my_stats to be paired t test results

paired.patient.grid <- function(my_tidy, my_populations, my_parent, my_stats){
  
  #subset df with manually gated data
  temp.tidy <- my_tidy %>%
    dplyr::filter(population_gate %in% my_populations) %>%
    dplyr::filter(parent_gate_name == my_parent)
  
  #subset df with stat results
  temp.stats <- my_stats %>% dplyr::filter(population_name %in% temp.tidy$population_name)
  
  #rename facets
  desired.facet.titles <- unique(temp.tidy$population_gate)
  current.facet.titles <- unique(temp.tidy$population_name)
  names(desired.facet.titles) <- current.facet.titles
  my_labels <- desired.facet.titles
  
  #plot order
  plot_order <- c("BS_Pre_Obese", "BS_Post_Obese") 
  
  #define color palette (based on ggsci::pal_nejm("default")(4)[1])
  patient_type_palette <- c(BS_Pre_Obese = "#0072B5FF",BS_Post_Obese = "#E18727FF")
  
  #make boxplot
  bp <- ggplot(temp.tidy, aes(x=factor(Patient_Type, levels = plot_order), y=count)) +
    ggbeeswarm::geom_beeswarm(data = temp.tidy, aes(fill = Patient_Type), shape = 21, colour = "black", size = 3) +
    geom_line(aes(group = PID_Formatted)) + 
    scale_fill_manual(values = patient_type_palette) +
    #2% and 10% spaces are respectively added at the bottom and the top of the plot
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.1)), limits = c(0, NA)) +
    scale_x_discrete(labels = c('BS_Pre_Obese' = 'Pre',
                                'BS_Post_Obese' = 'Post')) +
    labs(caption = temp.tidy$full_parent_path, y = temp.tidy$y_axis_title, x= "") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  # facet_grid
  bp.grid <- bp + ggh4x::facet_grid2(~population_name, scales = "free_y", independent = "y",
                                     labeller = labeller(population_name = my_labels)) +
    theme(strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold"),
          # Change spacing between facets on both axis
          theme(panel.spacing = unit(3, "lines")))
  
  # add stats to facet plot
  final.plot <- bp.grid + ggpubr::stat_pvalue_manual(data = temp.stats %>% dplyr::group_by(population_name) %>%
                                                       dplyr::mutate(y.position = min(y.position)) %>%
                                                       dplyr::ungroup(),
                                                     step.group.by = c("population_name"),
                                                     tip.length = 0.03,
                                                     bracket.nudge.y = 0.2,
                                                     step.increase = 0.2,
                                                     hide.ns = TRUE,
                                                     label = "p.signif")
  
  return(final.plot)
  
}