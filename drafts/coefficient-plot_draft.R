#draft plot ----
num.lm.res %>%
  dplyr::filter(term != "(Intercept)") %>%
  # reorder the coefficients so that the largest is at the top of the plot
  dplyr::mutate(term = forcats::fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_discrete(labels = c('BMI_Value' = 'BMI',
                              'BMI_Percentile_For_Age' = 'BMI %tile',
                              'Age' = 'Age (yrs)')) +
  #need to use expression for greek symbols to work
  labs(x = expression("Effect Size (" ~ beta ~ "Coefficient)"), y = "")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_text(size= 14, face = "bold"),
        axis.title.y = element_text(size= 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic"))

# linear regression per population ----
# source: https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
lm.res.test <- df %>%
  tidyr::nest(data = -population_name) %>%
  dplyr::mutate(fit = purrr::map(data, ~ lm(count ~ BMI_Value + BMI_Percentile_For_Age + Age + Sex, data = .x)),
                fit_tidy = purrr::map(fit, broom::tidy)) %>% 
  tidyr::unnest(fit_tidy)

# linear regression on multiple populations ----
# parent_gate_name = "Live CD3+_CD8+"
#plot_populations = all.mem.subtypes

my_population_names <- paste0(parent_gate_name, "_", texh.markers.select2)

lm.res.test.filter <- lm.res.test %>% 
  tidyr::unnest(fit_tidy) %>% 
  dplyr::filter(population_name %in% my_population_names) %>% 
  dplyr::filter(term != "(Intercept)") %>%
  # reorder the coefficients so that the largest is at the top of the plot
  dplyr::mutate(term = forcats::fct_reorder(term, estimate))

# plot linear regression ----
plot <- ggplot(lm.res.test.filter, aes(estimate, term)) +
  geom_point() +
  #geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
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

plot2 <- plot + facet_wrap(~population_name, scales = "free_y")
