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

# 1. train the model on two separate periods

# 2. do spatial block CV on these (3 blocks) to validate model

# 3. compare hold-out predictions against prediction based on previous
# incidence by meshblock (null model)


# step 2: prepare data for modelling

meshblock_incidence <- meshblock_incidence_survey_periods

# split incidence and scat positivity into summer and winter surveys

meshblock_incidence_summer <- meshblock_incidence %>%
  filter(
    period == "summer"
  )

rt_scat_positivity_summer <- rt_scat_positivity %>%
  filter(
    period == "summer"
  )

meshblock_incidence_winter <- meshblock_incidence %>%
  filter(
    period == "winter"
  )

rt_scat_positivity_winter <- rt_scat_positivity %>%
  filter(
    period == "winter"
  )

# step 3: define greta model
cutoff_distance <- 1

# define a vague-ish positive prior on the distance decay, with most mass at 0
# (slightly favouring shorter decays)
sigma <- normal(0, cutoff_distance / 2, truncation = c(0, Inf))
# 2 * (1 - pnorm(cutoff_distance, 0, cutoff_distance / 2))
# 5% chance *a priori* that the sigma goes beyond the cutoff distance

# scaling parameter on the FOI
beta <- normal(0, 1, truncation = c(0, Inf))

# define likelihoods on the two seasons separately
likelihood_summer <- define_likelihood(
  meshblock_incidence = meshblock_incidence_summer,
  rt_scat_positivity = rt_scat_positivity_summer,
  beta = beta,
  sigma = sigma,
  cutoff_distance = cutoff_distance
)
likelihood_winter <- define_likelihood(
  meshblock_incidence = meshblock_incidence_winter,
  rt_scat_positivity = rt_scat_positivity_winter,
  beta = beta,
  sigma = sigma,
  cutoff_distance = cutoff_distance
)

m <- model(beta, sigma)
draws <- mcmc(m)

plot(draws)
coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)

# get posterior means of the predicted incidence and cases
incidence_posterior <- calculate(incidence, values = draws, nsim = 1000)[[1]]
incidence_posterior_mean <- colMeans(incidence_posterior[, , 1])

expected_cases_posterior <- calculate(expected_cases, values = draws, nsim = 1000)[[1]]
expected_cases_posterior_mean <- colMeans(expected_cases_posterior[, , 1])

# simulate cases for PPCs and exceedance probabilities
cases_new <- poisson(expected_cases)
cases_sim <- calculate(
  cases_new,
  values = draws,
  nsim = 1000
)[[1]][, , 1]

p_some_cases <- colMeans(cases_sim > 0)

meshblock_incidence_posterior <- meshblock_incidence %>%
  mutate(
    predicted_incidence = incidence_posterior_mean,
    predicted_cases = expected_cases_posterior_mean,
    probability_of_cases = p_some_cases,
    sim_cases_1 = cases_sim[sample.int(1000, 1), ],
    sim_cases_2 = cases_sim[sample.int(1000, 1), ],
    sim_cases_3 = cases_sim[sample.int(1000, 1), ],
  )

# observed incidence
meshblock_incidence_posterior %>%
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
meshblock_incidence_posterior %>%
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
meshblock_incidence_posterior %>%
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
meshblock_incidence_posterior %>%
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
meshblock_incidence_posterior %>%
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


meshblock_incidence_posterior %>%
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
meshblock_incidence_posterior %>%
  summarise(
    across(
      ends_with("incidence"),
      sum
    )
  )

idx <- which(meshblock_incidence$cases > 1)

library(bayesplot)
ppc_ecdf_overlay(y = meshblock_incidence$cases,
                 yrep = cases_sim)

ppc_dens(y = meshblock_incidence$cases[idx],
         yrep = cases_sim[, idx])


coords <- meshblock_incidence %>%
  st_coordinates()
clus <- 

meshblock_incidence %>%
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

ggsave("figures/observed_incidence_clusters.png",
       bg = "white",
       width = 8,
       height = 7)
