# predict BU incidence at meshblock level as a distace-weighted function of MU
# positivity in possum scat and possum abundance.

# possum abundance multiplied by scat prevalence should be proportional to the
# number of infected possums at a location, which is proportional to the
# force-of-infection (FOI) from possums to mosquitos at that location.

# assuming a uniform abundance of mosquitoes within the study area, the
# prevalence of MU amongst moquitoes should be a monotone increasing function of
# this FOI, and likely close to proportional, given the very large numbers of
# mosquitoes and low observed prevalence in mosquitoes (little saturation).

# the prevalence in mosquitoes should be proportional to the FOI from mosquitoes
# to humans, and therefore the incidence among humans (low incidence and no
# immunity, so unlikely to saturate)

# the movement ranges of possums and humans about their home locations, and the
# dispersal of mosquitoes, mean that the locations of higher possum abundance
# and scat positivity will not correspond exactly with the hotspots of human
# cases. Instead, the incidence likelihood should be a distance-weighted average
# of the FOI to mosquitoes.

# model the FOI to mosquitoes as a function of possum abundance and scat
# positivity

# We have a continuous modelled surface of expected possum abundance, and point
# locations where possum scats have been screened for MU. We want to be able to
# predict BU incidence from the MU scat positivity, so we can use this a
# predictive tool in the future, for targetting interventions

# So we want a simple model to predict BU incidence on map, and therefore
# compute the expected number of cases at meshblock level.

# step 1: load data

# load in possum abundance map
possum_density <- load_possum_density()

# load in all scat locations and MU positivity
scat_positivity <- load_scat_positivity()

# subset to RT scats, and prepare for modelling, extracting possum densities
rt_scat_positivity <- prep_rt_scat_positivity(scat_positivity, possum_density)

# load in meshblock coordinates, cropping to peninsula
meshblocks <- load_meshblocks()

# load in residential BU cases and use 2016 populations
cases <- load_cases()

# assign survey periods and seasons
cases_survey_periods <- assign_survey_periods(cases)

cases_seasons <- assign_seasons(cases)

# plot these a la Koen, to check they make sense
plot_cases_by_period(cases_survey_periods)

# and plot by season
plot_cases_by_period(cases_seasons)

# compute incidence by meshblock, grouped by scat survey period
meshblock_incidence_survey_periods <- prep_meshblock_incidence(
  cases_survey_periods,
  meshblocks
)

meshblock_incidence_seasons <- prep_meshblock_incidence(
  cases_seasons,
  meshblocks
)

# plot meshblock incidence by season
plot_meshblock_incidence_by_period(
  meshblock_incidence_seasons
)

# 1. train the model on two separate periods - DONE

# 2. do spatial block CV on these (3 blocks) to validate model
#    - pull out the prediction code into a function - DONE
#    - wrap up the fitting and prediction code in functions - DONE
#    - define spatial blocks - DONE
#    - loop through blocks, fitting and predicting

# 3. compare hold-out predictions against prediction based on previous
# incidence by meshblock (null model)

n_cv_blocks <- 3

# define a spatial block pattern for cross-validation
meshblock_incidence_survey_periods_blocked <- define_blocks(
  meshblock_incidence_survey_periods,
  n_blocks = n_cv_blocks
)

# plot these blocks
plot_blocked_incidence(meshblock_incidence_survey_periods_blocked)
ggsave("figures/cv_blocks.png",
       width = 8,
       height = 6,
       bg = "white")

# split into training and testing sets
training <- split_data(
  meshblock_incidence_survey_periods_blocked,
  which = "train"
)

testing <- split_data(
  meshblock_incidence_survey_periods_blocked,
  which = "test"
)

# loop through fitting models (takes some time)
fitted_models <- lapply(
  training,
  train_model,
  rt_scat_positivity = rt_scat_positivity
)

# loop through doing checks
fits <- lapply(
  fitted_models,
  check_fitted_model,
)

# loop through doing predictions to hold-out data
predictions <- mapply(
  FUN = predict_model,
  fitted_model = fitted_models,
  meshblocks = testing,
  MoreArgs = list(rt_scat_positivity = rt_scat_positivity),
  SIMPLIFY = FALSE
)

# combine
predictions_all <- do.call(
  bind_rows,
  predictions
)

# simplify blocking info, for joining to previous data
blocking <- predictions_all %>%
  st_drop_geometry() %>%
  select(
    meshblock,
    block
  ) %>%
  distinct()

# calculate empirical incidences from previous year (2018 financial year)
previous_incidence = meshblock_incidence_seasons %>%
  st_drop_geometry() %>%
  filter(
    period == "2018",
    meshblock %in% predictions_all$meshblock
  ) %>%
  left_join(
    blocking,
    by = "meshblock"
  ) %>%
  rename(
    incidence_meshblock_2018 = incidence
  ) %>%
  group_by(
    block
  ) %>%
  mutate(
    incidence_block_2018 = sum(cases) / sum(pop)
  ) %>%
  ungroup() %>%
  select(
    meshblock,
    incidence_meshblock_2018,
    incidence_block_2018
  )

# get a dataframe of all the predictions to compare
predictions_to_evaluate <- predictions_all %>%
  left_join(
    previous_incidence,
    by = "meshblock"
  ) %>%
  left_join(
    survey_period_incidence_multipliers(),
    by = "period"
  ) %>%
  # 1. correlation in annualised incidence (need to adjust those defined on survey period)
  mutate(
    annualincidence = incidence / multiplier,
    pred_annualincidence_model = incidence_pred_mean / multiplier,
    pred_annualincidence_meshblock2018 = incidence_meshblock_2018,
    pred_annualincidence_cvblock2018 = incidence_block_2018,
  ) %>%
  # 2. poisson deviance on case counts (convert empirical incidence to predicted
  mutate(
    pred_cases_model = incidence_pred_mean * pop,
    pred_cases_meshblock2018 = incidence_meshblock_2018 * multiplier * pop,
    pred_cases_cvblock2018 = incidence_block_2018 * multiplier * pop,
  ) %>%
  # 3. AUC for presence of any cases
  mutate(
    any = as.numeric(cases > 0),
    pred_any_model = prob_any_cases(pred_cases_model),
    pred_any_meshblock2018 = prob_any_cases(pred_cases_meshblock2018),
    pred_any_cvblock2018 = prob_any_cases(pred_cases_cvblock2018),
  ) %>%
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = c(".value", "prediction"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  select(
    period,
    block,
    meshblock,
    cases,
    annualincidence,
    any,
    method = prediction,
    starts_with("pred")
  ) %>%
  mutate(
    method = factor(
      method,
      levels = c("model", "meshblock2018", "cvblock2018")
    )
  )
  

predictions_to_evaluate %>%
  group_by(method) %>%
  summarise(
    # 1. correlation in annual incidence
    cor_annualincidence = cor(annualincidence, pred_annualincidence),
    # 2. poisson deviance on case counts
    dev_cases = poisson_deviance(cases, pred_cases),
    # 3. AUC for presence of any cases
    auc_any = Metrics::auc(any, pred_any),
    # 4. fraction of cases intervened with for a fraction of meshblocks targetted
    fraction_intervened_5pc = fraction_cases_intervened(cases, pred_cases, 0.05),
    fraction_intervened_10pc = fraction_cases_intervened(cases, pred_cases, 0.10),
    fraction_intervened_20pc = fraction_cases_intervened(cases, pred_cases, 0.20),
    fraction_intervened_50pc = fraction_cases_intervened(cases, pred_cases, 0.50)
  ) %>%
  arrange(
    method
  )


predictions_to_evaluate %>%
  group_by(method, block) %>%
  summarise(
    # 1. correlation in annual incidence
    cor_annualincidence = cor(annualincidence, pred_annualincidence),
    # 2. poisson deviance on case counts
    dev_cases = poisson_deviance(cases, pred_cases),
    # 3. AUC for presence of any cases
    auc_any = Metrics::auc(any, pred_any),
    # 4. fraction of cases intervened with for a fraction of meshblocks targetted
    fraction_intervened_5pc = fraction_cases_intervened(cases, pred_cases, 0.05),
    fraction_intervened_10pc = fraction_cases_intervened(cases, pred_cases, 0.10),
    fraction_intervened_20pc = fraction_cases_intervened(cases, pred_cases, 0.20),
    fraction_intervened_50pc = fraction_cases_intervened(cases, pred_cases, 0.50)
  ) %>%
  arrange(
    block,
    method
  )








# observed incidence
fit %>%
  arrange(incidence) %>%
  ggplot(
    aes(
      colour = incidence
    )
  ) +
  geom_sf() +
  coord_sf() +
  scale_colour_distiller(direction = 1) +
  theme_minimal()

# predicted incidence
fit %>%
  arrange(predicted_incidence) %>%
  ggplot(
    aes(
      colour = predicted_incidence
    )
  ) +
  geom_sf() +
  coord_sf() +
  scale_colour_distiller(direction = 1) +
  theme_minimal()

ggsave("figures/predicted_incidence.png",
       bg = "white",
       width = 8,
       height = 7)

# whether there were any cases
fit %>%
  mutate(
    any_cases = as.numeric(cases > 0)
  ) %>%
  arrange(any_cases) %>%
  ggplot(
    aes(
      colour = any_cases
    )
  ) +
  geom_sf() +
  coord_sf() +
  scale_colour_distiller(direction = 1) +
  theme_minimal()

# posterior simulation of the same
fit %>%
  mutate(
    sim_any_cases = as.numeric(sim_cases_3 > 0)
  ) %>%
  arrange(sim_any_cases) %>%
  ggplot(
    aes(
      colour = sim_any_cases
    )
  ) +
  geom_sf() +
  coord_sf() +
  scale_colour_distiller(direction = 1) +
  theme_minimal()

# population density
fit %>%
  arrange(pop) %>%
  ggplot(
    aes(
      colour = pop
    )
  ) +
  geom_sf() +
  coord_sf() +
  scale_colour_distiller(direction = 1) +
  theme_minimal()


fit %>%
  ggplot(
    aes(
      x = predicted_incidence,
      y = incidence
    )
  ) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  theme_minimal()

# overall incidence is well-enough calibrated
fit %>%
  summarise(
    across(
      ends_with("incidence"),
      sum
    )
  )

idx <- which(fit$cases > 1)

library(bayesplot)
ppc_ecdf_overlay(y = fit$cases,
                 yrep = cases_sim)

ppc_dens(y = fit$cases[idx],
         yrep = cases_sim[, idx])

coords <- fit %>%
  st_coordinates()

clus <- fit %>%
  mutate(
    cluster = kmeans(
      st_coordinates(.),
      centers = 100
      )$cluster
    ) %>%
  group_by(
    cluster
  ) %>%
  mutate(
    incidence = sum(cases) / sum(pop)
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      colour = incidence
    )
  ) +
  geom_sf() +
  coord_sf() +
  scale_colour_distiller(direction = 1) +
  theme_minimal()

clus

ggsave("figures/observed_incidence_clusters.png",
       bg = "white",
       width = 8,
       height = 7)
