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

# plot these a la Koen, to check they make sense
plot_cases_surveys(cases)

# compute incidence by meshblock
meshblock_incidence <- prep_meshblock_incidence(cases, meshblocks)

# step 2: prepare data for modelling

positive_scats <- rt_scat_positivity %>%
  filter(mu_positive)


# remove any meshblock (or scat) datapoint that is more than 1km from the
# nearest other type of datapoint (drops 2 cases, no scats)
cutoff_distance <- 1
meshblock_incidence <- meshblock_incidence %>%
  filter_by_distance(
    cutoff_distance,
    positive_scats
  )

rt_scat_positivity <- rt_scat_positivity %>%
  filter_by_distance(
    cutoff_distance,
    positive_scats
  )

# 60 cases left in
sum(meshblock_incidence$cases)

# compute distance matrix d_ij between meshblock locations and possum scat
# sampling locations, in km
distance <- get_distance(meshblock_incidence, rt_scat_positivity)

# step 3: define greta model

mu_positivity <- as_data(rt_scat_positivity$mu_positive)

# define a vague-ish positive prior on the distance decay, with most mass at 0
# (slightly favouring shorter decays)
sigma <- normal(0, cutoff_distance / 2, truncation = c(0, Inf))
# 2 * (1 - pnorm(cutoff_distance, 0, cutoff_distance / 2))
# 5% chance *a priori* that the sigma goes beyond the cutoff distance

# do sparse matrix-multiply with distance cutoff for computational efficiency
keep_idx <- which(distance <= cutoff_distance, arr.ind = TRUE)
row_idx <- keep_idx[, 1]
col_idx <- keep_idx[, 2]
distance_sparse <- distance[keep_idx]

# compute unnormalised weights
weights_raw_sparse <- exp(-0.5 * (distance_sparse / sigma) ^ 2)
# normalise them (so prediction does not depend on number of samples nearby)
weights_raw_sum_sparse <- tapply(weights_raw_sparse, row_idx, FUN = "sum")
weights_sparse <- weights_raw_sparse / weights_raw_sum_sparse[row_idx]
# multiply weights by covariate and aggregate
weighted_positivity_elements_sparse <- weights_sparse * mu_positivity[col_idx]
weighted_positivity <- tapply(weighted_positivity_elements_sparse, row_idx, FUN = "sum")

# # confirm this gives the same answer as the matrix-multiply version
# mask <- distance <= cutoff_distance
# weights_raw <- exp(-0.5 * (distance / sigma) ^ 2) * mask
# weights <- sweep(weights_raw, 1, rowSums(weights_raw), FUN = "/")
# weighted_positivity_dense <- weights %*% mu_positivity
# diff <- max(abs(weighted_positivity - weighted_positivity_dense))
# calculate(diff, nsim = 10)

# scaling parameter on the FOI
beta <- normal(0, 1, truncation = c(0, Inf))

incidence <- beta * weighted_positivity
expected_cases <- incidence * meshblock_incidence$pop

distribution(meshblock_incidence$cases) <- poisson(expected_cases)

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
