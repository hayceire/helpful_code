#linear model for each flow population ----
flow.lm <- flow.df %>%
  tidyr::nest(data = -population_name) %>%
  dplyr::mutate(fit = purrr::map(data, ~ lm(count ~ BMI_Value + BMI_Percentile_For_Age + Age + Sex, data = .x)),
                fit_tidy = purrr::map(fit, broom::tidy, conf.int = TRUE),
                fit_glance = purrr::map(fit, broom::glance),
                fit_augment = purrr::map(fit, broom::augment))

#forest plot for beta coefficient -----
forest.grid.plot <- function(my_lm_res, my_populations, my_parent, my_title){
  
  my_population_names <- paste0(my_parent, "_", my_populations)
  
  temp.tidy <- my_lm_res %>% 
    tidyr::unnest(fit_tidy) %>% 
    dplyr::filter(population_name %in% my_population_names) %>% 
    dplyr::filter(term != "(Intercept)") %>%
    # reorder the coefficients so that the largest is at the top of the plot
    dplyr::mutate(term = forcats::fct_reorder(term, estimate))
  
  # retrieve the population_gate column from nested data
  # this step is needed for facet to work
  
  temp.tidy.filter <- temp.tidy %>%
    dplyr::mutate(population_gate = purrr::map(.data[["data"]], function(x){unique(x$population_gate)})) %>%
    dplyr::mutate(population_gate = unlist(population_gate))
  
  plot <- ggplot(temp.tidy.filter, aes(estimate, term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
    # add in a dotted line at zero
    geom_vline(xintercept = 0, lty = 2) +
    scale_y_discrete(labels = c('BMI_Value' = 'BMI',
                                'BMI_Percentile_For_Age' = 'BMI %tile',
                                'Age' = 'Age (yrs)',
                                'SexMale' = 'Male')) +
    #need to use expression for greek symbols to work
    labs(x = expression("Effect Size (" ~ beta ~ "Coefficient)"), y = "")+
    theme_bw()+
    theme(legend.position = "none",
          axis.title.x = element_text(size= 14, face = "bold"),
          axis.title.y = element_text(size= 14, face = "bold"),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(size = 10, face = "italic"))
  
  plot2 <- plot +
    ggh4x::facet_grid2(~population_gate, scales = "fixed") +
    theme(strip.text.x = element_text(size = 14, face = "bold"),
          strip.text.y = element_text(size = 14, face = "bold"),
          # Change spacing between facets on both axis
          theme(panel.spacing = unit(3, "lines"))) +
    ggtitle(my_title)
  
  plot2
}
