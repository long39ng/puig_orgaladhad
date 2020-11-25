TidyTuesday 2020/48: Washington Trails
================

``` r
library(tidyverse)

theme_set(
  theme_minimal(base_size = 15,
                base_family = "FantasqueSansMono Nerd Font") +
    theme(panel.ontop = TRUE,
          panel.grid = element_line(colour = "#EDEAC2"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill = "#EDEAC2"),
          plot.margin = margin(30, 30, 30, 30),
          plot.title.position = "plot",
          plot.caption.position = "plot")
)
```

``` r
hike_data <- tidytuesdayR::tt_load(2020, 48)$hike_data
#> --- Compiling #TidyTuesday Information for 2020-11-24 ----
#> --- There is 1 file available ---
#> --- Starting Download ---
#> 
#>  Downloading file 1 of 1: `hike_data.rds`
#> --- Download complete ---

glimpse(hike_data)
#> Rows: 1,958
#> Columns: 8
#> $ name        [3m[38;5;246m<chr>[39m[23m "Lake Hills Greenbelt", "Snow Lake", "Skookum Flats", "Te…
#> $ location    [3m[38;5;246m<chr>[39m[23m "Puget Sound and Islands -- Seattle-Tacoma Area", "Snoqua…
#> $ length      [3m[38;5;246m<chr>[39m[23m "2.3 miles, roundtrip", "7.2 miles, roundtrip", "7.8 mile…
#> $ gain        [3m[38;5;246m<chr>[39m[23m "50", "1800", "300", "1585", "500", "500", "425", "450", …
#> $ highpoint   [3m[38;5;246m<chr>[39m[23m "330.0", "4400.0", "2550.0", "2370.0", "1000.0", "2200.0"…
#> $ rating      [3m[38;5;246m<chr>[39m[23m "3.67", "4.16", "3.68", "3.92", "4.14", "3.14", "5.00", "…
#> $ features    [3m[38;5;246m<list>[39m[23m [<"Dogs allowed on leash", "Wildlife", "Good for kids", …
#> $ description [3m[38;5;246m<chr>[39m[23m "Hike through a pastoral area first settled and farmed in…

# Check data type
hike_data %>% 
  filter(str_detect(gain, "\\.") | !str_detect(highpoint, ".0$"))
#> # A tibble: 0 x 8
#> # … with 8 variables: name <chr>, location <chr>, length <chr>, gain <chr>,
#> #   highpoint <chr>, rating <chr>, features <list>, description <chr>

(all_features <- hike_data %>% 
    pull(features) %>% 
    unlist() %>% 
    unique() %>% 
    sort())
#>  [1] "Coast"                 "Dogs allowed on leash" "Dogs not allowed"     
#>  [4] "Established campsites" "Fall foliage"          "Good for kids"        
#>  [7] "Lakes"                 "Mountain views"        "Old growth"           
#> [10] "Ridges/passes"         "Rivers"                "Summits"              
#> [13] "Waterfalls"            "Wildflowers/Meadows"   "Wildlife"

hike_data_cleaned <- hike_data %>% 
  rownames_to_column("id") %>% 
  # https://www.youtube.com/watch?v=8w1itDDm8QU
  mutate(location_general = str_replace_all(location, "(.*)\\s[-][-].*", "\\1"),
         length_total = parse_number(length) * (str_detect(length, "one-way") + 1),
         gain = as.integer(gain),
         highpoint = as.numeric(highpoint),
         rating = as.numeric(rating))
```

``` r
hike_data_long <- hike_data_cleaned %>% 
  unnest(features, keep_empty = TRUE)

hike_data_onehot <- hike_data_long %>% 
  mutate(n = 1L) %>% 
  pivot_wider(names_from = features, values_from = n) %>% 
  select(-`NA`) %>% 
  mutate(across(everything(), ~ replace_na(.x, 0L)))
```

``` r
hike_data_onehot %>% 
  select(rating, `Dogs allowed on leash`:Summits) %>% 
  correlation::correlation() %>% 
  filter(Parameter1 == "rating") %>% 
  ggplot(aes(r, fct_reorder(Parameter2, r))) +
  geom_col(aes(fill = r > 0), width = .5, show.legend = FALSE) +
  scale_x_continuous(position = "top") +
  scale_fill_manual(values = c("#A36B2B", "#2686A0")) +
  annotation_raster(magick::image_read("https://i.imgflip.com/sepum.jpg") %>% 
                      as.raster(),
                    .102, .143, 1.5, 6, interpolate = TRUE) +
  labs(x = NULL, y = NULL,
       title = "Correlation of User Ratings with Features",
       subtitle = "of Washington Hiking Trails",
       caption = "Data: Washington Trails Association")
```

![](figs/corr-plot-1.png)<!-- -->
