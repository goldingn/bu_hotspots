# library(sf)
# 
# mb <- st_read("data/MB_2011_VIC_Census_counts_Centroids_LatLong_MorPen_Only/MB_2011_VIC_Census_counts_Centroids_LatLong_MorPen_Only.shp")
# mb %>%
#   st_geometry() %>%
#   plot()

library(readxl)
positivity <- read_excel("data/20210811_Extract4Nick.xlsx")  %>%
  rename_with(
    tolower
  ) %>%
  mutate(
    date = as.Date(
      start_survey,
      format = "%a %b %d %H:%M:%S UTC %Y"
    ),
    species = case_when(
      suspected_organism == 1 ~ "ringtail",
      suspected_organism == 2 ~ "brushtail",
      TRUE ~ NA_character_
    ),
    bu_positive = `cq(is2404)` != "NEG"
  ) %>%
  filter(
    !is.na(date),
    found_pellets == 1
  ) %>%
  select(
    date,
    latitude,
    longitude,
    species,
    bu_positive
  ) %>%
  arrange(
    bu_positive
  )

# subset to ringtails, and prepare data for modelling
rt_positivity <- positivity %>%
  filter(
    species == "ringtail"
  ) %>%
  select(
    -species
  ) %>%
  # coerce to an sf object
  st_as_sf(
    coords = c("longitude", "latitude")
  ) %>%
  # set these as lat-long
  st_set_crs(
    4326
  ) %>%
  # transform to MGA zone 55 (projected system for Melbourne area)
  st_transform(
    28355
  ) %>%
  # pull out the coordinates as columns
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2],
  ) %>%
  # drop the sf structure
  st_set_geometry(NULL) %>%
  # standardise the dates and scale the coordinates
  mutate(
    date_num = as.numeric(date - min(date)),
    x_scaled = (x - mean(x)) / 1000,
    y_scaled = (y - mean(y)) / 1000,
  )
    
plot(y_scaled ~ x_scaled,
     pch = 16,
     col = ifelse(bu_positive, "black", "grey"),
     asp = 1,
     cex = 0.5,
     data = filter(rt_positivity))

library(mgcv)
m <- bam(
  bu_positive ~ 
    # 'Gaussian process smooth' over space with Matern 5/2 kernel
    s(x_scaled, y_scaled, date_num,
      bs = "gp",
      k = 50,
      m = 4),
    # # temporal smooth term
    # s(date_num) +
    # # space-time smooth to mop up interaction
    # s(y_scaled, x_scaled, date_num),
  # bernoulli likelihood on scat positivity
  family = stats::binomial,
  # ringtail data only
  data = rt_positivity
)
summary(m)
plot(m)

# cluster points and assess prevalence in clusters
rt_positivity

summary(rt_positivity)

clus <- rt_positivity %>%
  select(x, y) %>%
  kmeans(centers = 200)


rt_positivity_clusters <- rt_positivity %>%
  mutate(
    cluster = clus$cluster
  ) %>%
  group_by(cluster) %>%
  summarise(
    samples = n(),
    positives = sum(bu_positive),
    across(
      c(x, y),
      mean
    )
  ) %>%
  mutate(
    prevalence = positives / samples
  )


rt_positivity_clusters %>%
  ggplot(
    aes(
      x = x,
      y = y,
      colour = prevalence
    )
  ) +
  geom_point() +
  coord_fixed() +
  theme_minimal()

plot(y ~ x,
     col = prevalence,
     data = rt_positivity_clusters)

# fit a full Bayesian GP model
library(greta.gp)
rt_positivity

# pull out coordinates and define spatial GP inducing points
coords <- rt_positivity %>%
  select(
    x_scaled, y_scaled
  ) %>%
  as.matrix()

n_inducing <- 100
coords_inducing <- kmeans(coords, centers = n_inducing)$centers

# GP hyperparameters
len_spatial <- lognormal(1, 1)
var_spatial <- normal(0, 1, truncation = c(0, Inf))
len_spatial_both <- c(len_spatial, len_spatial)
k_spatial <- mat52(lengthscales = len_spatial_both, variance = var_spatial) + bias(10)

# evaluate spatial GP at data
f_spatial <- gp(
  x = coords,
  kernel = k_spatial,
  inducing = coords_inducing
)

p_positive <- ilogit(f_spatial)

# define likelihood
distribution(rt_positivity$bu_positive) <- bernoulli(p_positive)

m <- model(len_spatial, var_spatial)
draws <- mcmc(m, chains = 10, warmup = 500, n_samples = 500)     
