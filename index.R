library(dplyr)
library(r4np)
library(naniar)
library(ggplot2)
library(tidyverse)

Star_Wars_characters <- read.csv("00_raw_data/SW-characters.csv")
# glimpse(Star_Wars_characters$birth_year)
# skimr::skim(Star_Wars_characters)
sw_clean <- janitor::clean_names(Star_Wars_characters)
sw_clean <- sw_clean |> mutate(
  mass = as.integer(mass),
  gender = as.factor(gender)
)
# glimpse(sw_clean)
sw_clean |>
  count(gender) |>
  ggplot(aes(
    x = fct_reorder(gender, n, .desc = TRUE),
    y = n
  )) +
  geom_col()
# Star_Wars_characters |> gg_miss_var()
sw_clean |>
  select(-name) |>
  mcar_test()
sw_clean %>% count()

sw_nona <-
  sw_clean %>%
  select(-name) %>%
  as.data.frame() %>%
  simputation::impute_knn(. ~ .)

sw_nona <-
  sw_nona %>%
  mutate(
    height = as.numeric(height),
    mass = as.numeric(mass)
  )

glimpse(sw_nona)
