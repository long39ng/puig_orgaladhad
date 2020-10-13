TidyTuesday 2020/42: Datasaurus Dozen
================

``` r
library(tidyverse)
library(ggridges)
library(patchwork)

source(here::here("scripts/default_theme.R"))
```

``` r
datasaurus <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2020/2020-10-13/datasaurus.csv")

glimpse(datasaurus)
#> Rows: 1,846
#> Columns: 3
#> $ dataset <chr> "dino", "dino", "dino", "dino", "dino", "dino", "dino", "dino…
#> $ x       <dbl> 55.3846, 51.5385, 46.1538, 42.8205, 40.7692, 38.7179, 35.6410…
#> $ y       <dbl> 97.1795, 96.0256, 94.4872, 91.4103, 88.3333, 84.8718, 79.8718…
```

``` r
datasaurus %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~dataset) +
  panel_grid()
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="figs/unnamed-chunk-1-1.png" width="672" />

``` r
datasaurus_lm <- datasaurus %>% 
  nest(data = -dataset) %>% 
  mutate(model = map(data, ~ lm(y ~ x, data = .x)),
         coefs = map(model, broom::tidy)) %>% 
  unnest(coefs)

datasaurus_lm
#> # A tibble: 26 x 8
#>    dataset data            model term      estimate std.error statistic  p.value
#>    <chr>   <list>          <lis> <chr>        <dbl>     <dbl>     <dbl>    <dbl>
#>  1 dino    <tibble [142 ×… <lm>  (Interce…  53.5        7.69      6.95  1.29e-10
#>  2 dino    <tibble [142 ×… <lm>  x          -0.104      0.136    -0.764 4.46e- 1
#>  3 away    <tibble [142 ×… <lm>  (Interce…  53.4        7.69      6.94  1.31e-10
#>  4 away    <tibble [142 ×… <lm>  x          -0.103      0.135    -0.760 4.48e- 1
#>  5 h_lines <tibble [142 ×… <lm>  (Interce…  53.2        7.70      6.91  1.53e-10
#>  6 h_lines <tibble [142 ×… <lm>  x          -0.0992     0.136    -0.732 4.66e- 1
#>  7 v_lines <tibble [142 ×… <lm>  (Interce…  53.9        7.69      7.01  9.38e-11
#>  8 v_lines <tibble [142 ×… <lm>  x          -0.112      0.135    -0.824 4.12e- 1
#>  9 x_shape <tibble [142 ×… <lm>  (Interce…  53.6        7.69      6.97  1.17e-10
#> 10 x_shape <tibble [142 ×… <lm>  x          -0.105      0.135    -0.778 4.38e- 1
#> # … with 16 more rows
```

``` r
datasaurus_lm %>% 
  mutate(term = if_else(term == "x", "Slope", "Intercept")) %>% 
  ggplot(aes(estimate, fct_reorder(dataset, estimate, max))) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - std.error,
                     xmax = estimate + std.error),
                 height = 0) +
  labs(x = NULL, y = NULL, title = "Linear regression estimates") +
  panel_grid("X") +
  facet_wrap(~term, scales = "free_x")
```

<img src="figs/unnamed-chunk-3-1.png" width="672" />

``` r
ridges_x <- datasaurus %>% 
  ggplot(aes(x, fct_reorder(dataset, x, PerformanceAnalytics::kurtosis),
             fill = stat(x))) +
  geom_density_ridges_gradient(colour = "white", show.legend = FALSE,
                               scale = 2.3, rel_min_height = .01) +
  scale_fill_viridis_c(option = "A", direction = -1) +
  labs(y = NULL, subtitle = "Ordered by kurtosis") +
  panel_grid("Xx")

ridges_y <- datasaurus %>% 
  ggplot(aes(y, fct_reorder(dataset, y, PerformanceAnalytics::kurtosis),
             fill = stat(x))) +
  geom_density_ridges_gradient(colour = "white", show.legend = FALSE,
                               scale = 2.3, rel_min_height = .01) +
  scale_fill_viridis_c(option = "A", direction = -1) +
  labs(y = NULL, subtitle = "Ordered by kurtosis") +
  theme(plot.subtitle = element_markdown(colour = "white")) +
  panel_grid("Xx")

ridges_x + ridges_y & theme(plot.margin = margin(15, 15, 15, 15))
#> Picking joint bandwidth of 5.46
#> Picking joint bandwidth of 9
```

<img src="figs/unnamed-chunk-4-1.png" width="100%" />
