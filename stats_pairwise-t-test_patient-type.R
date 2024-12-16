#UNCORRECTED PAIRWISE T TEST

patient.type.stat.test <- function(my_tidy_table){
  
  temp.df <- my_tidy_table
  
  # subset data frame to identify zero variance populations
  zero.var.groups <- temp.df %>%
    dplyr::group_by(population_name, Patient_Type) %>%
    dplyr::summarize(group.var = var(count), group.mean = mean(count)) %>%
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
    rstatix::pairwise_t_test(count ~ Patient_Type, pool.sd = FALSE, paired = FALSE, p.adjust.method = "none") %>%
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
    dplyr::mutate(stat_test = "pairwise-two-sample-t-test") %>%
    # add a column to indicate multiple comparisons correction
    dplyr::mutate(p_adjust_method = "none") %>%
    # add excluded populations back to stat results
    # add a row containing the zero variance populations
    # Add 'No' to the 'ran_test' column for each population added
    dplyr::add_row(population_name = zero.var.nonzero.mean.pops,
                   ran_test = "No")
  
  return(temp.df)
}