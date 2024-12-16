#boxplot + line plots
# only HC is shown as boxplot
# pre/post bs samples connected by line
# requires my_stats to be pairwise t test results

scenith.patient.type.grid <- function(my_tidy, my_populations, my_parent, my_stats){
  
  #subset df with manually gated data
  temp.tidy <- my_tidy %>%
    dplyr::filter(population_gate %in% my_populations) %>%
    dplyr::filter(parent_gate == my_parent) %>% 
    dplyr::select(Nautilus_Formatted, PID_Formatted, population_name, y_axis_title, full_parent_path,
                  metabolic_profile, calculation, Patient_Type, population_gate, parent_gate) %>% 
    dplyr::distinct()
  
  #subset df with stat results
  temp.stats <- my_stats %>% dplyr::filter(population_name %in% temp.tidy$population_name)
  
  #rename facets
  desired.facet.titles <- unique(temp.tidy$population_gate)
  current.facet.titles <- unique(temp.tidy$population_name)
  names(desired.facet.titles) <- current.facet.titles
  my_labels <- desired.facet.titles
  
  #plot order
  plot_order <- c("HC_Null_Healthy", "HC_Null_Obese", "BS_Pre_Obese", "BS_Post_Obese") 
  
  #define color palette (based on ggsci::pal_nejm("default")(4)[1])
  patient_type_palette <- c(HC_Null_Healthy = "gray", HC_Null_Obese = "#BC3C29FF",
                            BS_Pre_Obese = "#0072B5FF",BS_Post_Obese = "#E18727FF")
  
  #make boxplot
  bp <- ggplot(temp.tidy, aes(x=factor(Patient_Type, levels = plot_order), y=calculation)) +
    # only 'HC' is shown with boxplot
    geom_boxplot(data = temp.tidy, aes(fill=Patient_Type), outlier.shape = NA, colour = "black") +
    # connect pre-post samples with line
    geom_line(aes(group = PID_Formatted)) + 
    ggbeeswarm::geom_beeswarm(aes(fill = Patient_Type), shape = 21, colour = "black", size = 3) +
    scale_fill_manual(values = patient_type_palette) +
    #2% and 10% spaces are respectively added at the bottom and the top of the plot
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.1)), limits = c(0, NA)) +
    scale_x_discrete(labels = c('HC_Null_Healthy' = 'HC',  'HC_Null_Obese' = 'Obese',
                                'BS_Pre_Obese' = 'Pre', 'BS_Post_Obese' = 'Post')) +
    labs(caption = temp.tidy$full_parent_path, y = temp.tidy$y_axis_title, x= "") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  # facet plot ----
  # columns by population, rows by condition
  # use ggh4x::facet_grid2 for plots with unique y axis
  # factor rows in desired top to bottom order
  bp.grid <- bp +
    ggh4x::facet_grid2(factor(metabolic_profile,
                              levels = c("Glucose_Dependence", "Mito_Dependence", "FAO_Capacity", "Glycolytic_Capacity"),
                              labels = c("Glucose \nDependence", "Mitochondrial \nDependence", "FAO \nCapacity", "Glycolytic \nCapacity")) ~ population_name,
                       scales = "fixed",
                       independent = "none",
                       labeller = labeller(population_name = my_labels)) +
    theme(strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold"),
          # Change spacing between facets on both axis
          theme(panel.spacing = unit(3, "lines")))
  
  # add stats to facet plot
  final.plot <- bp.grid + ggpubr::stat_pvalue_manual(data = temp.stats %>%
                                                       dplyr::group_by(metabolic_profile, population_name) %>%
                                                       dplyr::mutate(y.position = min(y.position)) %>%
                                                       dplyr::ungroup(),
                                                     step.group.by = c("metabolic_profile", "population_name"),
                                                     tip.length = 0.03,
                                                     bracket.nudge.y = 0.15,
                                                     step.increase = 0.15,
                                                     hide.ns = TRUE,
                                                     label = "p.signif")
  
  return(final.plot)
  
}