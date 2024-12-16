scenith.paired.test <- function(my_tidy_table, my_profile){
  
  temp.df <- my_tidy_table
  temp.df <- temp.df %>% 
    dplyr::select(Nautilus_Formatted, population_name, metabolic_profile, calculation, Patient_Type) %>% 
    dplyr::filter(metabolic_profile == my_profile) %>% 
    dplyr::distinct()
  
  # subset data frame to identify zero variance populations
  zero.var.groups <- temp.df %>%
    dplyr::group_by(population_name, Patient_Type) %>%
    dplyr::summarize(group.var = var(calculation), group.mean = mean(calculation)) %>%
    dplyr::summarize(n.groups.zero.var = sum((group.var == 0) & (group.mean > 0)))
  
  # extract populations with zero variance
  zero.var.nonzero.mean.pops <- zero.var.groups %>%
    dplyr::filter(n.groups.zero.var > 1) %>%
    dplyr::pull(population_name)
  
  temp.df <- temp.df %>% 
    #filter out populations where more than 1 disease has zero variance AND nonzero means
    dplyr::filter(!(population_name %in% zero.var.nonzero.mean.pops)) %>%
    # group the data by the manually gated population of interest
    dplyr::group_by(population_name) %>%
    # run an pairwise t-test without correction
    rstatix::t_test(calculation ~ Patient_Type, paired = TRUE, p.adjust.method = "none") %>%
    # add significance symbols based on p values  stored in a column named 'p.adj.signif'
    rstatix::add_significance() %>%
    # add the coordinates of where to place the p-value brackets in subsequent plots
    rstatix::add_xy_position(x = "Patient_Type", scales = "free_y") %>%
    # in a new column 'Comparisons' separate the groups being compared per stat test by a hyphen
    # makes results more readable
    dplyr::mutate(Comparison = paste(group1, "-", group2)) %>% 
    # add a column to indicate that stat test was run on populations
    dplyr::mutate(ran_test = "Yes") %>%
    # add a column to indicate stat test
    dplyr::mutate(stat_test = "paired-t-test") %>%
    # add a column to indicate multiple comparisons correction
    dplyr::mutate(p_adjust_method = "none") %>%
    dplyr::mutate(metabolic_profile = my_profile) %>% 
    # add excluded populations back to stat results
    # add a row containing the zero variance populations
    # Add 'No' to the 'ran_test' column for each population added
    dplyr::add_row(population_name = zero.var.nonzero.mean.pops,
                   ran_test = "No")
  
  return(temp.df)
}