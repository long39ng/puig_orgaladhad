TidyTuesday 2021/09: Employed Status
================

``` r
library(tidyverse)
library(ragg)
```

``` r
earn <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv" %>% 
  read_csv(col_types = "cccciiii")
```

``` r
rose <- function(petals, angle = 0, scale = 1) {
  rad <- pi * angle / 180
  df <- data.frame(theta = seq(0, (petals - 2) * pi, by = pi / 180)) %>% 
    mutate(x = scale * cos(petals / (petals - 2) * theta + rad) * cos(theta),
           y = scale * cos(petals / (petals - 2) * theta + rad) * sin(theta))
  df
}

roses <- earn %>% 
  mutate(quarter_label = glue::glue("{year}-Q{quarter}"),
         date = lubridate::yq(quarter_label),
         race2 = if_else(race == "All Races", ethnic_origin, race)) %>%
  filter(age == "25 years and over",
         race2 != "All Origins",
         sex != "Both Sexes") %>% 
  mutate(petals = median_weekly_earn %/% 100 - 2) %>% 
  group_by(sex, race2) %>% 
  mutate(angle = row_number() * 15,
         points = pmap(
           list(petals, angle, median_weekly_earn),
           function(x, y, z) rose(petals = x, angle = y, scale = z)
         )) %>% 
  ungroup() %>% 
  select(sex, race2, quarter_label, median_weekly_earn, points) %>% 
  unnest(points) %>% 
  group_split(quarter_label)
```

``` r
agg_png("figs/tmp/frame-%02d.png",
        width = 1294, height = 800, res = 96, bg = "#1A1D21")

walk(roses, ~ print({
  ggplot(.x) +
    geom_path(aes(x, y, colour = x * y), show.legend = FALSE) +
    geom_text(aes(x = 0, y = max(y),
                  label = scales::dollar(median_weekly_earn)),
              colour = "white", size = 5, nudge_y = 250) +
    scale_x_continuous(limits = 1850 * c(-1, 1)) +
    scale_y_continuous(limits = 1850 * c(-1, 1)) +
    scale_colour_viridis_c(option = "magma", begin = .3) +
    facet_grid(sex ~ str_wrap(race2, 17), switch = "both") +
    coord_equal() +
    labs(title = "Median Weekly Earning in the US — People Ages 25 and Over",
         subtitle = .x$quarter_label[1],
         caption = "Note: \"Hispanic or Latino\" overlaps with other categories\nData: US Bureau of Labor Statistics — Visualisation: Long Nguyen (@long39ng) · #TidyTuesday") +
    theme_void(base_size = 20) +
    theme(text = element_text(family = "Source Sans Pro",
                              colour = "white"),
          plot.background = element_rect(fill = "#1A1D21", colour = "#1A1D21"),
          plot.margin = margin(20, 20, 20, 20),
          plot.title.position = "plot",
          plot.subtitle = element_text(face = "bold",
                                       margin = margin(20, 10, 20, 10)),
          plot.caption = element_text(size = 12,
                                      lineheight = 1.2,
                                      margin = margin(60, 10, 10, 10)),
          plot.caption.position = "plot",
          strip.text.y = element_text(margin = margin(10, 20, 10, 10)))
}))

dev.off()
#> png 
#>   2
```

``` r
system("convert -delay 25 figs/tmp/*.png figs/roses.gif")
fs::dir_delete("figs/tmp/")
```

![](figs/roses.gif)

<details>
<summary>
Session information
</summary>

``` r
sessionInfo()
#> R version 4.0.4 (2021-02-15)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: openSUSE Leap 15.2
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib64/libopenblas_pthreads.so.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
#>  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
#>  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] ragg_1.1.0      forcats_0.5.1   stringr_1.4.0   dplyr_1.0.4    
#>  [5] purrr_0.3.4     readr_1.4.0     tidyr_1.1.2     tibble_3.0.6   
#>  [9] ggplot2_3.3.3   tidyverse_1.3.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyselect_1.1.0  xfun_0.21         haven_2.3.1       colorspace_2.0-0 
#>  [5] vctrs_0.3.6       generics_0.1.0    viridisLite_0.3.0 htmltools_0.5.1.1
#>  [9] yaml_2.2.1        rlang_0.4.10      pillar_1.4.7      glue_1.4.2       
#> [13] withr_2.4.1       DBI_1.1.1         dbplyr_2.1.0      modelr_0.1.8     
#> [17] readxl_1.3.1      lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0     
#> [21] cellranger_1.1.0  rvest_0.99.0.9000 evaluate_0.14     labeling_0.4.2   
#> [25] knitr_1.31        curl_4.3          parallel_4.0.4    broom_0.7.5      
#> [29] Rcpp_1.0.6        scales_1.1.1      backports_1.2.1   jsonlite_1.7.2   
#> [33] farver_2.0.3      fs_1.5.0          systemfonts_1.0.1 textshaping_0.3.0
#> [37] hms_1.0.0         digest_0.6.27     stringi_1.5.3     grid_4.0.4       
#> [41] cli_2.3.0         tools_4.0.4       magrittr_2.0.1    crayon_1.4.1     
#> [45] pkgconfig_2.0.3   ellipsis_0.3.1    xml2_1.3.2        reprex_1.0.0     
#> [49] lubridate_1.7.9.2 assertthat_0.2.1  rmarkdown_2.7.1   httr_1.4.2       
#> [53] rstudioapi_0.13   R6_2.5.0          compiler_4.0.4
```

</details>
