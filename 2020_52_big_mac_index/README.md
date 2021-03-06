TidyTuesday 2020/52: The Big Mac index
================

``` r
library(tidyverse)
library(ggtext)
library(ggtextures)

theme_set(
  theme_minimal(base_family = "Libre Franklin", base_size = 16) +
    theme(axis.text.x = element_text(size = 10, colour = "#f07178"),
          axis.text.y = element_text(face = "bold", colour = "#82aaff"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(colour = "#373c53", size = .5),
          plot.background = element_rect(fill = "#292d3e"),
          plot.margin = margin(30, 30, 30, 30),
          plot.title = element_markdown(size = rel(1.3),
                                        family = "BioRhyme Expanded",
                                        colour = "#c3e88d",
                                        lineheight = 1.5),
          plot.subtitle = element_markdown(colour = "#c792ea",
                                           lineheight = 1.2,
                                           margin = margin(b = 15)),
          plot.title.position = "plot",
          plot.caption = element_markdown(size = 10,
                                          colour = "#c792ea",
                                          lineheight = 1.2,
                                          margin = margin(t = -20)))
)
```

``` r
big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')
glimpse(big_mac)
#> Rows: 1,386
#> Columns: 19
#> $ date          <date> 2000-04-01, 2000-04-01, 2000-04-01, 2000-04-01, 2000-0…
#> $ iso_a3        <chr> "ARG", "AUS", "BRA", "CAN", "CHE", "CHL", "CHN", "CZE",…
#> $ currency_code <chr> "ARS", "AUD", "BRL", "CAD", "CHF", "CLP", "CNY", "CZK",…
#> $ name          <chr> "Argentina", "Australia", "Brazil", "Canada", "Switzerl…
#> $ local_price   <dbl> 2.50, 2.59, 2.95, 2.85, 5.90, 1260.00, 9.90, 54.37, 24.…
#> $ dollar_ex     <dbl> 1.0000000, 1.6800000, 1.7900000, 1.4700000, 1.7000000, …
#> $ dollar_price  <dbl> 2.500000, 1.541667, 1.648045, 1.938776, 3.470588, 2.451…
#> $ usd_raw       <dbl> -0.00398, -0.38579, -0.34341, -0.22758, 0.38270, -0.023…
#> $ eur_raw       <dbl> 0.05007, -0.35246, -0.30778, -0.18566, 0.45774, 0.02964…
#> $ gbp_raw       <dbl> -0.16722, -0.48645, -0.45102, -0.35417, 0.15609, -0.183…
#> $ jpy_raw       <dbl> -0.09864, -0.44416, -0.40581, -0.30099, 0.25130, -0.116…
#> $ cny_raw       <dbl> 1.09091, 0.28939, 0.37836, 0.62152, 1.90267, 1.05023, 0…
#> $ gdp_dollar    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ adj_price     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ usd_adjusted  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ eur_adjusted  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ gbp_adjusted  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ jpy_adjusted  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ cny_adjusted  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

burger_img <- "https://twemoji.maxcdn.com/v/latest/72x72/1f354.png"

big_mac_capita <- big_mac %>% 
  slice_max(date) %>% 
  drop_na(gdp_dollar) %>% 
  mutate(n_burgers = gdp_dollar / dollar_price / 1000)
```

``` r
big_mac_capita %>% 
  ggplot(aes(x = fct_reorder(name, n_burgers), y = n_burgers)) +
  geom_isotype_col(image = burger_img,
                   img_width = unit(1, "native"),
                   img_height = unit(.7, "native"),
                   ncol = NA, nrow = 1, hjust = 0, vjust = .5) +
  scale_y_continuous(position = "right", labels = function(x) {
    x <- paste0(x, "k")
    x[x == "0k"] <- "0"
    x
  }) +
  annotate(geom = "text", x = 36.5, y = 17.5, label = "}",
           hjust = 0, vjust = .39, angle = 270,
           family = "Libre Franklin", colour = "#c792ea", size = 7) +
  annotate(geom = "text", x = 35.9, y = 17.5, label = "1000\nBig Macs",
           hjust = .5, vjust = 1, lineheight = 1,
           family = "Libre Franklin", colour = "#c792ea") +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = toupper("Big Macs per capita"),
       subtitle = "How many Big Macs could be bought at local price<br>
                   with the GDP per person in 2020",
       caption = "Data: *The Economist*<br>Hamburger emoji: *Twemoji*")
```

![](figs/big-mac-capita-1.png)<!-- -->
