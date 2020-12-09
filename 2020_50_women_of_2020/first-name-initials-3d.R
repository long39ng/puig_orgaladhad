library(tidyverse)
library(scales)
library(rgl)
library(ggrgl)

source(here::here("scripts/default_theme.R"))

women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

women %>%
  slice(-1) %>%
  mutate(first_initial = str_sub(name, end = 1) %>%
           fct_expand(LETTERS) %>%
           fct_relevel(LETTERS)) %>%
  ggplot(aes(first_initial, fill = fct_infreq(first_initial))) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(breaks = breaks_extended(8)) +
  scale_fill_manual(values = viridis_pal()(26)) +
  facet_wrap(~category)

women_initials_bar <- women %>%
  slice(-1) %>%
  mutate(first_initial = str_sub(name, end = 1) %>%
           fct_expand(LETTERS) %>%
           fct_relevel(LETTERS)) %>%
  ggplot(aes(first_initial, fill = fct_infreq(first_initial))) +
  geom_bar_z(z = 30, show.legend = FALSE, extrude = TRUE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(breaks = breaks_extended(8)) +
  scale_fill_manual(values = viridis_pal()(26)) +
  panel_grid("Y")

devoutrgl::rgldev(fov = 30, view_angle = -30)
women_initials_bar
invisible(dev.off())

