# predict BU incidence at meshblock level as a distace-weighted function of MU
# positivity in possum scat and possum abundance.

# possum abundance multiplied by scat prevalence should be proportional to the
# number of infected possums at a location, which is proportional ot the
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

# compute incidence by meshblock
meshblock_incidence <- prep_meshblock_incidence(cases, meshblocks)

# step 2: prepare data for modelling

# compute distance matrix d_ij between meshblock locations and possum scat
# sampling locations, in km
distance <- sf::st_distance(
  meshblock_incidence,
  rt_scat_positivity,
  which = "Euclidean",
)
units(distance) <- "km"
units(distance) <- NULL

# step 3: define greta model

# define a vague-ish positive prior on the distance decay, with most mass at 0
# (slightly favouring shorter decays)
sigma <- normal(0, 10, truncation = c(0, Inf))
# 2 * dnorm(max(distance), 0, 10) # 0.5% chance *a priori* that the sigma spans
# the entire area

# define normalised weights
weights_raw <- exp(-0.5 * (distance / sigma) ^ 2)
weights <- sweep(weights_raw, 1, rowSums(weights_raw), FUN = "/")

mu_positivity <- as_data(rt_scat_positivity$bu_positive)

weighted_positivity <- weights %*% mu_positivity

# log-normal scaling parameter on the FOI
beta <- normal(0, 1)

eta <- beta * log(weighted_positivity)
incidence <- exp(eta)
# cases <- as_data(meshblock_incidence$cases)
# population <- as_data(meshblock_incidence$pop_1)

distribution(meshblock_incidence$cases) <- poisson(incidence * meshblock_incidence$pop)

m <- model(beta, sigma)
draws <- mcmc(m)


# calculate(sum(population))
# 
mean(meshblock_incidence$cases)
sim <- calculate(incidence * meshblock_incidence$pop_1,
                 values = list(
                   beta = 0.1,
                   sigma = 1
                 ),
                 nsim = 1)[[1]][1, , 1]
sum(dpois(meshblock_incidence$cases, sim, log = TRUE))

# calculate(sum(cases))
# calculate(beta, nsim = 10)

n_chains <- 4
inits <- replicate(n_chains,
                   initials(
                     beta = 0.1,
                     sigma = 1
                   ),
                   simplify = FALSE)
draws <- mcmc(m, initial_values = inits)

tmp <- calculate(incidence * population,
                 values = list(
                   beta = 0.5,
                   sigma = 1
                 ), nsim = 1)[[1]][1, , ]

range(tmp)

str(tmp)

# compute local force of infection
# mu_scat_prevalence_i = \sum_{ij}^n mu_scat_pos_j w_{ij}