library(dplyr)
library(r4np)
library(naniar)
library(ggplot2)
library(tidyverse)
library(igraph)
library(ggridges)

Star_Wars_characters <- read.csv("00_raw_data/SW-characters.csv")
# glimpse(Star_Wars_characters$birth_year)
# skimr::skim(Star_Wars_characters)
sw_clean <- janitor::clean_names(Star_Wars_characters)
sw_clean <- sw_clean |> mutate(
  mass = as.integer(mass),
  gender = as.factor(gender)
)
# glimpse(sw_clean)
# A bar plot of the factor levels genders sorted by amount
sw_clean |>
  count(gender) |>
  ggplot(aes(
    x = fct_reorder(gender, n, .desc = TRUE),
    y = n
  )) +
  geom_col()

# Dealing with missing data
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

write_csv(sw_clean, "01_tidy_data/sw_nona.csv") # data wrangled

sw_clean <- read.csv("01_tidy_data/sw_nona.csv")
glimpse(sw_clean)

edge_net <- graph_from_edgelist(cbind(head(sw_clean$height), head(sw_clean$homeworld)),
  directed = F
)
set.seed(123)
plot.igraph(edge_net) # matrix from Star Wars data

# the most popular world among the Star Wars homeworlds (the entire plot rotated by 90 degrees, sorted sorted by frequency)
sw_clean %>%
  count(homeworld) %>%
  ggplot(aes(x = reorder(homeworld, n), y = n)) +
  geom_col() +
  ggtitle("Most popular worlds") +
  xlab("homeworld") +
  ylab("frequency") +
  coord_flip()

# what heights are in each of the world. The mean provides the ‘average’ height of characters of a world
sw_clean %>%
  group_by(homeworld) %>%
  # Compute the mean for each group (remove NAs via na.rm = TRUE)
  summarise(mean_heights = mean(height, na.rm = TRUE)) %>%
  # Create the plot
  ggplot(aes(x = reorder(homeworld, mean_heights), y = mean_heights)) +
  geom_col() +
  coord_flip()

# We can retrieve the exact heights by removing the plot from the above code.
sw_clean %>%
  group_by(homeworld) %>%
  summarise(mean_heights = mean(height, na.rm = TRUE)) %>%
  arrange(desc(mean_heights))

# Let’s add the number of characters (n) in each world to our table. We can achieve this by using the function n().
sw_clean %>%
  group_by(homeworld) %>%
  summarise(
    mean_heights = mean(height, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(mean_heights))

# As a final step, we can plot the sum of all heights per world as yet another indicator for the ‘most highest world.’
sw_clean %>%
  filter(!is.na(height)) %>%
  group_by(homeworld) %>%
  summarise(sum_heights = sum(height)) %>%
  ggplot(aes(x = homeworld, y = sum_heights)) +
  geom_col() +
  coord_flip()
# These results confirm that the Naboo world is the highest one out of all worlds covered by the Star War movie.
#  If we are curious to know which character contributed the most to this result, we can achieve this easily by drawing on some functions
sw_clean %>%
  select(name, homeworld, height) %>%
  filter(homeworld == "Naboo") %>%
  slice_max(
    order_by = height,
    n = 5
  )
# Roos Tarpals with the 224 height rank the highest out of all Naboo world.
# In the last line, I sneaked in another new function from dplyr called slice_max(). This function allows us to pick the top 5 movies in our data.
# If we want to pick the lowest observations in our dataframe, we can use slice_min()

# We filter() our dataset to only show heights in the world Naboo and also remove characters that have no value for height.
sw_clean %>%
  filter(homeworld == "Naboo" & !is.na(height)) %>%
  ggplot(aes(
    x = reorder(name, height),
    y = height
  )) +
  geom_col() +
  coord_flip()

# We can visualise medians using boxplots. Boxplots are a very effective tool to show how your data is distributed in one single data visualisation, and it offers more than just the mean. It also shows the spread of the data. We can represent each character as a point by using geom_point().
sw_clean %>%
  filter(homeworld == "Naboo" & !is.na(height)) %>%
  ggplot(aes(height)) +
  geom_boxplot() +
  geom_point(aes(y = 0, col = "red"),
    show.legend = FALSE
  )
# The dot on the right is the Roos Tarpals with 224 height. This plot also nicely demonstrates why boxplots are so popular and helpful: They provide so many insights, not only into the central tendency of a variable, but also highlight outliers and, more generally, give a sense of the spread of our data.


# the most frequently occurring value for species (only species >= 2)
# sw_clean %>%
#   count(species) %>%
#   filter(n >= 2) %>%
#   ggplot(aes(
#     x = as_factor(species),
#     y = n,
#     label = n
#   )) +
#   geom_col() +
#   geom_text(nudge_y = 1)
