TidyTuesday 2020/46: Historical Phones
================

``` r
library(tidyverse)
library(colorspace)
library(patchwork)
library(magick)
library(ggtext)

theme_set(
  theme_minimal() +
    theme(axis.text = element_text(family = "Space Mono"),
          panel.grid = element_blank(),
          plot.title = element_text(family = "Space Mono",
                                    face = "bold",
                                    size = rel(1.6)),
          plot.caption = element_markdown(family = "Space Mono",
                                          face = "bold.italic",
                                          hjust = 0))
)
```

``` r
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv',
                            col_types = "cciinnc")

glimpse(landline)
#> Rows: 6,974
#> Columns: 7
#> $ entity        <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanist…
#> $ code          <chr> "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG",…
#> $ year          <int> 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1…
#> $ total_pop     <int> 12412000, 13299000, 14486000, 15817000, 17076000, 18111…
#> $ gdp_per_cap   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1063.63…
#> $ landline_subs <dbl> 0.29553158, 0.28475432, 0.20742093, 0.19211533, 0.17931…
#> $ continent     <chr> "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia",…

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv',
                            col_types = "cciinnc")

glimpse(mobile)
#> Rows: 6,277
#> Columns: 7
#> $ entity      <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan…
#> $ code        <chr> "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "…
#> $ year        <int> 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 199…
#> $ total_pop   <int> 13032161, 14069854, 15472076, 17053213, 18553819, 1978988…
#> $ gdp_per_cap <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1063.636,…
#> $ mobile_subs <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.…
#> $ continent   <chr> "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "Asia", "…
```

``` r
world_map <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  select(iso_a3, geometry)
```

``` r
map_hist_layout <- c(
  area(1, 1, 13, 14), # Map
  area(12, 8, 13, 11), # Histogram
  area(14, 1, 15, 14) # Slider
)

# # Determine histogram axis limit and number of bins
# mobile %>% 
#   semi_join(world_map, by = c("code" = "iso_a3")) %>% 
#   group_by(year) %>% 
#   slice_max(mobile_subs) %>% 
#   View()

mobile_img <- image_graph(875, 600, res = 96, bg = "transparent")

mobile_year_list <- group_split(mobile, year)

out <- map(mobile_year_list, function(data) {
  mobile_data <- world_map %>% 
    filter(iso_a3 != "ATA") %>% # Antarctica
    left_join(data %>% 
                select(code, mobile_subs), by = c("iso_a3" = "code"))
  
  mobile_map <- ggplot(
    mobile_data,
    aes(fill = cut_width(mobile_subs, width = 20, boundary = 0))
  ) +
    geom_sf(colour = "#f8f8f8", size = .25, show.legend = FALSE) +
    coord_sf(label_axes = "----", expand = FALSE,
             crs = "+proj=robin") + # Robinson projection
    scale_fill_discrete_sequential("YlGnBu", na.value = "#cccccc", nmax = 11) +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  mobile_hist <- ggplot(
    mobile_data,
    aes(mobile_subs, fill = cut_width(mobile_subs, width = 20, boundary = 0))
  ) +
    geom_histogram(binwidth = 20, boundary = 0, show.legend = FALSE) +
    scale_x_continuous(limits = c(0, 220), n.breaks = 12,
                       labels = function(x) {
                         x[seq(2, 12, by = 2)] <- ""
                         x
                       }) +
    scale_fill_discrete_sequential("YlGnBu", nmax = 11) +
    coord_cartesian(expand = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = margin(0, 0, 0, 0))
  
  year_slider <- ggplot(data = NULL, aes(x = 1990:2017, y = 1)) +
    geom_line(size = 2, colour = "#cccccc") +
    geom_point(aes(x = data$year), shape = 21, size = 4.5, stroke = .6,
               fill = "#f8f8f8", colour = "#26185f") +
    geom_text(aes(x = data$year, y = 1, label = data$year),
              vjust = 1.75, family = "Space Mono") +
    theme_void()
  
  print(
    mobile_map + mobile_hist + year_slider +
      plot_layout(design = map_hist_layout) +
      plot_annotation(
        title = "Mobile phone subscriptions per 100 people",
        caption = "Source: OurWorldInData.org<br>
                   *Antarctica not included"
      )
  )
})

dev.off()
#> png 
#>   2

mobile_anim <- image_animate(mobile_img, fps = 2)
print(mobile_anim)
#> # A tibble: 28 x 7
#>    format width height colorspace matte filesize density
#>    <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
#>  1 gif      875    600 sRGB       TRUE         0 +96x+96
#>  2 gif      875    600 sRGB       TRUE         0 +96x+96
#>  3 gif      875    600 sRGB       TRUE         0 +96x+96
#>  4 gif      875    600 sRGB       TRUE         0 +96x+96
#>  5 gif      875    600 sRGB       TRUE         0 +96x+96
#>  6 gif      875    600 sRGB       TRUE         0 +96x+96
#>  7 gif      875    600 sRGB       TRUE         0 +96x+96
#>  8 gif      875    600 sRGB       TRUE         0 +96x+96
#>  9 gif      875    600 sRGB       TRUE         0 +96x+96
#> 10 gif      875    600 sRGB       TRUE         0 +96x+96
#> # … with 18 more rows
```

![](figs/world-mobile-animation-1.gif)<!-- -->
