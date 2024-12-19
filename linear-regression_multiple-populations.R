#linear model per population name
flow.lm <- flow.df %>%
  tidyr::nest(data = -population_name) %>%
  dplyr::mutate(fit = purrr::map(data, ~ lm(count ~ BMI_Value + BMI_Percentile_For_Age + Age + Sex, data = .x)),
                fit_tidy = purrr::map(fit, broom::tidy, conf.int = TRUE),
                fit_glance = purrr::map(fit, broom::glance),
                fit_augment = purrr::map(fit, broom::augment))