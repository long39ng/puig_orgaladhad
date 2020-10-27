TidyTuesday 2020/44: Canadian Wind Turbines
================

``` r
library(tidyverse)
library(sf)
library(rnaturalearth)
library(gganimate)
```

``` r
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

glimpse(wind_turbine)
#> Rows: 6,698
#> Columns: 15
#> $ objectid                   [3m[38;5;246m<dbl>[39m[23m 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,…
#> $ province_territory         [3m[38;5;246m<chr>[39m[23m "Alberta", "Alberta", "Alberta", "Alberta"…
#> $ project_name               [3m[38;5;246m<chr>[39m[23m "Optimist Wind Energy", "Castle River Wind…
#> $ total_project_capacity_mw  [3m[38;5;246m<dbl>[39m[23m 0.90, 44.00, 3.78, 3.78, 3.78, 3.78, 19.50…
#> $ turbine_identifier         [3m[38;5;246m<chr>[39m[23m "OWE1", "CRW1", "WWT1", "WWT2", "WWT3", "W…
#> $ turbine_number_in_project  [3m[38;5;246m<chr>[39m[23m "1/2", "1/60", "1/6", "2/6", "3/6", "4/6",…
#> $ turbine_rated_capacity_k_w [3m[38;5;246m<dbl>[39m[23m 150, 600, 600, 600, 600, 660, 1300, 1300, …
#> $ rotor_diameter_m           [3m[38;5;246m<dbl>[39m[23m 23, 44, 44, 44, 44, 47, 60, 60, 60, 60, 60…
#> $ hub_height_m               [3m[38;5;246m<dbl>[39m[23m 30, 40, 50, 50, 50, 50, 46, 46, 46, 46, 46…
#> $ manufacturer               [3m[38;5;246m<chr>[39m[23m "Bonus", "Vestas", "Vestas", "Vestas", "Ve…
#> $ model                      [3m[38;5;246m<chr>[39m[23m "AN 150/30", "V44/600", "V44/600", "V44/60…
#> $ commissioning_date         [3m[38;5;246m<chr>[39m[23m "1993", "1997", "1998", "1998", "1998", "2…
#> $ latitude                   [3m[38;5;246m<dbl>[39m[23m 49.53, 49.51, 49.23, 49.23, 49.22, 49.22, …
#> $ longitude                  [3m[38;5;246m<dbl>[39m[23m -114.1, -114.0, -113.7, -113.6, -113.7, -1…
#> $ notes                      [3m[38;5;246m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

wind_turbine_welp <- wind_turbine %>% 
  mutate(commissioning_date_welp = str_sub(commissioning_date, end = 4) %>% 
           as.integer())
```

``` r
anim_turbines <- ne_states("Canada", returnclass = "sf") %>% 
  ggplot() +
  geom_sf(colour = "#f8f8f8") +
  geom_jitter(data = wind_turbine_welp,
              aes(longitude, latitude, colour = turbine_rated_capacity_k_w,
                  group = seq_along(objectid)),
              alpha = .4, size = 2.5) +
  scale_colour_viridis_c(
    begin = .3, end = .95, direction = -1,
    labels = function(x) paste(x, "kW"),
    guide = guide_legend(override.aes = list(alpha = 1, size = 2.5))
  ) +
  labs(title = "Wind turbines in Canada",
       subtitle = "Year: {frame_along}") +
  theme_void(base_family = "Asap Condensed", base_size = 16) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  transition_reveal(commissioning_date_welp)

animate(anim_turbines, end_pause = 6, width = 600, height = 600, units = "px")
```

![](figs/animate-1.gif)<!-- -->
