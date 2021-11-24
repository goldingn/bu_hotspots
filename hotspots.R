library(tidyverse)
library(readxl)

# load 2016 meshblock centroids from here:
# https://figshare.com/articles/dataset/Mesh_block_centroids_csv_/8319734/1
centroids <- read_csv(
  "data/mb_2016_centroids.csv",
  col_types = cols(
    MB_CODE16 = col_double(),
    MB_CAT16 = col_character(),
    SA1_MAIN16 = col_double(),
    SA1_7DIG16 = col_double(),
    SA2_MAIN16 = col_double(),
    SA2_5DIG16 = col_double(),
    SA2_NAME16 = col_character(),
    SA3_CODE16 = col_double(),
    SA3_NAME16 = col_character(),
    SA4_CODE16 = col_double(),
    SA4_NAME16 = col_character(),
    GCC_CODE16 = col_character(),
    GCC_NAME16 = col_character(),
    STE_CODE16 = col_double(),
    STE_NAME16 = col_character(),
    AREASQKM16 = col_double(),
    layer = col_character(),
    xcoord = col_double(),
    ycoord = col_double()
  )
) %>%
  select(
    MB2016 = MB_CODE16,
    xcoord,
    ycoord
  )

# convert to 2011 centroids
cg_file <- "data/CG_VIC_MB_2011_VIC_MB_2016.xls"
mb_lookup_1 <- readxl::read_excel(
  cg_file,
  sheet = "Table 3a",
  skip = 5
) %>%
  slice(-1)

mb_lookup_2 <- readxl::read_excel(
  cg_file,
  sheet = "Table 3b",
  skip = 5
) %>%
  slice(-1)

mb_lookup <- bind_rows(
  mb_lookup_1,
  mb_lookup_2
) %>%
  select(
    MB2011 = `MB_CODE_2011...1`,
    MB2016 = `MB_CODE_2016...3`,
    weight = RATIO
  )

mb2011_centroids <- mb_lookup %>%
  left_join(
    centroids,
    by = "MB2016" 
  ) %>%
  group_by(
    MB2011
  ) %>%
  summarise(
    across(
      ends_with("coord"),
      ~weighted.mean(.x, weight)
    )
  ) %>%
  mutate(
    MB2011 = as.character(MB2011)
  )

cases_file <- "data/BU NHMRC NickGolding SpatialEpi data NO PHESSID_Jan to 9 Nov 2021.xlsx"
cases <- cases_file %>%
  readxl::read_excel() %>%
  select(
    MB2011,
    likely_exposure,
    date = date_sxonset,
    type_of_contact = typecontact0,
    exposure_MB2011 = mb0
  ) %>%
  mutate(
    date = as.Date(date),
    exposure_MB2011 = as.character(exposure_MB2011)
  ) %>%
  filter(
    type_of_contact == "Resident" | !is.na(exposure_MB2011)
  ) %>%
  mutate(
    MB2011 = coalesce(exposure_MB2011, MB2011) 
  ) %>%
  select(
    MB2011,
    date,
    likely_exposure
  ) %>%
  left_join(
    mb2011_centroids,
    by = "MB2011"
  )

# exclude presumed Bairnsdale case
cases_melbourne <- cases %>%
  filter(
    likely_exposure != "Bairnsdale"
  )

plot(
  ycoord ~ xcoord,
  data = cases_melbourne,
  asp = 1,
  pch = 16,
  cex = 0.3
)

# do a KDE on this?
