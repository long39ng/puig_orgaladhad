TidyTuesday 2021/11: Bechdel Test
================

``` r
knitr::opts_chunk$set(
  echo = TRUE, collapse = TRUE, comment = "#>",
  fig.path = "figs/", dpi = 300,
  dev = "ragg_png", dev.args = list(bg = "black")
)
knitr::knit_hooks$set(optipng = knitr::hook_optipng)

library(tidyverse)
library(scales)
```

``` r
library(furrr)
library(omdbapi)

raw_bechdel <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2021/2021-03-09/raw_bechdel.csv",
  col_types = "iicci"
) %>%
  mutate(imdb_id = paste0("tt", imdb_id))

omdb_api_keys <- vector("character", length = 9L) # Redacted

plan(multisession)

bechdel <- raw_bechdel %>%
  mutate(group = sort(rep(1:9, 1000))[1:nrow(bechdel)]) %>%
  group_split(group) %>%
  future_map2_dfr(.y = omdb_api_keys, ~ mutate(.x, genre = map(
    imdb_id,
    possibly(function(id) {
      get_genres(find_by_id(id, api_key = .y))
    }, otherwise = NULL)
  ))) %>%
  select(-group)

saveRDS(bechdel, here::here("2021_11_bechdel_test/bechdel.RDS"))
```

``` r
# Source: https://twitter.com/maxnoethe/status/1176398931208671232
stripes_pal <- c(
  brewer_pal(palette = "Reds", direction = -1)(9)[2:8],
  brewer_pal(palette = "Blues")(9)[2:8]
)
```

``` r
bechdel <- readRDS(here::here("2021_11_bechdel_test/bechdel.RDS"))

# Take care of duplicates
bechdel %>%
  add_count(year, title) %>%
  filter(n > 1)
#> # A tibble: 17 x 7
#>     year    id imdb_id     title                  rating genre         n
#>    <int> <int> <chr>       <chr>                   <int> <list>    <int>
#>  1  1931  1985 tt0021814   Dracula                     2 <chr [3]>     2
#>  2  1931  8213 tt0021815   Dracula                     3 <chr [3]>     2
#>  3  1959  9209 tt0053285   Sleeping Beauty             3 <chr [5]>     2
#>  4  1959   474 tt53285     Sleeping Beauty             3 <NULL>        2
#>  5  1983  4449 tt0086425   Terms of Endearment         3 <chr [2]>     2
#>  6  1983  4448 tt0086425   Terms of Endearment         1 <chr [2]>     2
#>  7  1997  4380 tt0117056   Ayneh                       3 <chr [1]>     2
#>  8  1997  4381 tt0117056   Ayneh                       3 <chr [1]>     2
#>  9  2011  4889 tt2043900   Last Call at the Oasis      3 <chr [1]>     2
#> 10  2011  4907 tt2043900   Last Call at the Oasis      3 <chr [1]>     2
#> 11  2014  6000 tt2180411   Into the Woods              3 <chr [5]>     2
#> 12  2014  8702 tt002180411 Into the Woods              3 <NULL>        2
#> 13  2015  6655 tt0810819   The Danish Girl             3 <chr [3]>     2
#> 14  2015  9081 ttNA        The Danish Girl             3 <NULL>        2
#> 15  2017  7241 tt0451279   Wonder Woman                3 <chr [5]>     3
#> 16  2017  9294 ttNA        Wonder Woman                3 <NULL>        3
#> 17  2017  9293 ttNA        Wonder Woman                3 <NULL>        3
# There were actually two different Dracula (Drácula) movies in 1931 :D
# What's up with Terms of Endearment tho?

bechdel_long <- bechdel %>%
  mutate(n_genres = map_int(genre, length)) %>%
  filter(n_genres > 0) %>%
  distinct(year, title, rating, .keep_all = TRUE) %>%
  select(-n_genres) %>%
  unnest(genre)

bechdel_long %>%
  count(genre) %>%
  arrange(n)
#> # A tibble: 27 x 2
#>    genre           n
#>    <chr>       <int>
#>  1 Game-Show       1
#>  2 Adult           3
#>  3 N/A             5
#>  4 News            8
#>  5 Film-Noir      68
#>  6 Documentary    98
#>  7 Short         121
#>  8 Western       139
#>  9 Sport         185
#> 10 Music         323
#> # … with 17 more rows
```

``` r
theme_set(
  theme_void(base_size = 13, base_family = "Inter Medium") +
    theme(
      text = element_text(colour = "white"),
      legend.position = "top",
      strip.text = element_text(
        family = "Inter Semi Bold",
        size = rel(1),
        margin = margin(b = 1)
      ),
      plot.title = element_text(
        family = "Metropolis Extra Bold",
        size = rel(2)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        size = rel(1.4),
        margin = margin(t = 10, b = 15)
      ),
      plot.caption = element_text(
        margin = margin(t = 20)
      ),
      plot.background = element_rect(fill = "black"),
      plot.margin = margin(15, 20, 15, 20)
    )
)

stripes <- bechdel_long %>%
  add_count(genre) %>%
  filter(n > 10) %>%
  mutate(pass = if_else(rating == 3, 1L, 0L)) %>%
  slice(sort(rep(seq_len(nrow(bechdel_long)), 2))) %>%
  mutate(genre = if_else(row_number() %% 2 == 0, "OVERALL", genre)) %>%
  group_by(genre, year) %>%
  summarise(prop_pass = mean(pass)) %>%
  ungroup() %>%
  ggplot(aes(year, 1, fill = prop_pass)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = stripes_pal,
    breaks = c(0, 1),
    labels = label_percent(),
    guide = guide_colourbar(
      title = NULL,
      label.hjust = c(1.5, -.3),
      label.vjust = 6.5,
      barwidth = 7,
      barheight = .3,
      ticks = FALSE
    )
  ) +
  facet_wrap(~ fct_reorder(genre, prop_pass, mean) %>%
    fct_relevel("OVERALL", after = Inf),
  ncol = 6
  ) +
  labs(
    title = "Cooling Stripes?",
    subtitle = glue::glue("Percentage of movies having at least two women who talk to each other about something other than a man, {min(bechdel$year)}–{max(bechdel$year)}"),
    caption = "Data: IMDb/OMDb & BechdelTest.com — Graphic: Long Nguyen (@long39ng) — #TidyTuesday"
  )

stripes
```

![](figs/strips-1.png)<!-- -->

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
#>  [1] scales_1.1.1    forcats_0.5.1   stringr_1.4.0   dplyr_1.0.5    
#>  [5] purrr_0.3.4     readr_1.4.0     tidyr_1.1.3     tibble_3.1.0   
#>  [9] ggplot2_3.3.3   tidyverse_1.3.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] Rcpp_1.0.6         lubridate_1.7.10   here_1.0.1         assertthat_0.2.1  
#>  [5] rprojroot_2.0.2    digest_0.6.27      utf8_1.1.4         R6_2.5.0          
#>  [9] cellranger_1.1.0   backports_1.2.1    reprex_1.0.0       evaluate_0.14     
#> [13] highr_0.8          httr_1.4.2         pillar_1.5.1       rlang_0.4.10      
#> [17] readxl_1.3.1       rstudioapi_0.13    rmarkdown_2.7.3    labeling_0.4.2    
#> [21] textshaping_0.3.1  munsell_0.5.0      gridtext_0.1.4     broom_0.7.5       
#> [25] compiler_4.0.4     modelr_0.1.8       xfun_0.21          pkgconfig_2.0.3   
#> [29] systemfonts_1.0.1  htmltools_0.5.1.1  ggtext_0.1.1       tidyselect_1.1.0  
#> [33] fansi_0.4.2        crayon_1.4.1       dbplyr_2.1.0       withr_2.4.1       
#> [37] grid_4.0.4         jsonlite_1.7.2     gtable_0.3.0       lifecycle_1.0.0   
#> [41] DBI_1.1.1          magrittr_2.0.1     cli_2.3.1          stringi_1.5.3     
#> [45] farver_2.1.0       fs_1.5.0           xml2_1.3.2         ellipsis_0.3.1    
#> [49] ragg_1.1.1         generics_0.1.0     vctrs_0.3.6        RColorBrewer_1.1-2
#> [53] tools_4.0.4        glue_1.4.2         markdown_1.1       hms_1.0.0         
#> [57] parallel_4.0.4     yaml_2.2.1         colorspace_2.0-0   rvest_1.0.0       
#> [61] knitr_1.31         haven_2.3.1
```

</details>
