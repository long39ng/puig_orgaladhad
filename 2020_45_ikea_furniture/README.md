TidyTuesday 2020/45: IKEA Furniture
================

``` r
library(tidyverse)
library(gender)
library(ggtext)
library(ggalt)
library(ggrepel)

theme_set(
  theme_minimal(base_size = 16) +
    theme(axis.text = element_text(family = "Poppins", colour = "#0057A5"),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "#DCDCDC"),
          plot.margin = margin(30, 30, 30, 30),
          plot.title = element_markdown(family = "BioRhyme",
                                        face = "bold",
                                        colour = "#978466"),
          plot.title.position = "plot",
          plot.caption = element_markdown(family = "Poppins",
                                          size = 11,
                                          colour = "#0057A5"),
          plot.caption.position = "plot")
)
```

``` r
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv',
                        col_types = "iiccnclcccciii")
#> Warning: Missing column names filled in: 'X1' [1]

glimpse(ikea)
#> Rows: 3,694
#> Columns: 14
#> $ X1                [3m[38;5;246m<int>[39m[23m 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 1…
#> $ item_id           [3m[38;5;246m<int>[39m[23m 90420332, 368814, 9333523, 80155205, 30180504, 1012…
#> $ name              [3m[38;5;246m<chr>[39m[23m "FREKVENS", "NORDVIKEN", "NORDVIKEN / NORDVIKEN", "…
#> $ category          [3m[38;5;246m<chr>[39m[23m "Bar furniture", "Bar furniture", "Bar furniture", …
#> $ price             [3m[38;5;246m<dbl>[39m[23m 265, 995, 2095, 69, 225, 345, 129, 195, 129, 2176, …
#> $ old_price         [3m[38;5;246m<chr>[39m[23m "No old price", "No old price", "No old price", "No…
#> $ sellable_online   [3m[38;5;246m<lgl>[39m[23m TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
#> $ link              [3m[38;5;246m<chr>[39m[23m "https://www.ikea.com/sa/en/p/frekvens-bar-table-in…
#> $ other_colors      [3m[38;5;246m<chr>[39m[23m "No", "No", "No", "Yes", "No", "No", "No", "No", "N…
#> $ short_description [3m[38;5;246m<chr>[39m[23m "Bar table, in/outdoor,          51x51 cm", "Bar ta…
#> $ designer          [3m[38;5;246m<chr>[39m[23m "Nicholai Wiig Hansen", "Francis Cayouette", "Franc…
#> $ depth             [3m[38;5;246m<int>[39m[23m NA, NA, NA, 50, 60, 45, 44, 50, 44, NA, 44, 45, 47,…
#> $ height            [3m[38;5;246m<int>[39m[23m 99, 105, NA, 100, 43, 91, 95, NA, 95, NA, 103, 102,…
#> $ width             [3m[38;5;246m<int>[39m[23m 51, 80, NA, 60, 74, 40, 50, 50, 50, NA, 52, 40, 46,…
```

``` r
ikea %>% 
  count(category)
#> # A tibble: 17 x 2
#>    category                                 n
#>    <chr>                                <int>
#>  1 Bar furniture                           47
#>  2 Beds                                   208
#>  3 Bookcases & shelving units             548
#>  4 Cabinets & cupboards                   292
#>  5 Café furniture                          26
#>  6 Chairs                                 481
#>  7 Chests of drawers & drawer units       125
#>  8 Children's furniture                   124
#>  9 Nursery furniture                       97
#> 10 Outdoor furniture                      216
#> 11 Room dividers                           13
#> 12 Sideboards, buffets & console tables    23
#> 13 Sofas & armchairs                      428
#> 14 Tables & desks                         612
#> 15 Trolleys                                28
#> 16 TV & media furniture                   190
#> 17 Wardrobes                              236

ikea %>% 
  distinct(designer)
#> # A tibble: 381 x 1
#>    designer            
#>    <chr>               
#>  1 Nicholai Wiig Hansen
#>  2 Francis Cayouette   
#>  3 Henrik Preutz       
#>  4 Marcus Arvonen      
#>  5 Carina Bengs        
#>  6 K Hagberg/M Hagberg 
#>  7 Sarah Fager         
#>  8 Ehlén Johansson     
#>  9 Nike Karlsson       
#> 10 Maja Ganszyniec     
#> # … with 371 more rows

ikea %>% 
  select(designer) %>% 
  filter(str_detect(designer, "/"))
#> # A tibble: 1,327 x 1
#>    designer                       
#>    <chr>                          
#>  1 K Hagberg/M Hagberg            
#>  2 K Hagberg/M Hagberg            
#>  3 K Hagberg/M Hagberg            
#>  4 John/Jonas/Petrus/Paul/Caroline
#>  5 K Hagberg/M Hagberg            
#>  6 John/Jonas/Petrus/Paul/Caroline
#>  7 Nike Karlsson/Maja Ganszyniec  
#>  8 J Karlsson/N Karlsson          
#>  9 IKEA of Sweden/Karl Malmvall   
#> 10 J Karlsson/N Karlsson          
#> # … with 1,317 more rows

ikea %>% 
  mutate(n_designers = str_count(designer, "/") + 1L,
         .keep = "used") %>% 
  arrange(-n_designers)
#> # A tibble: 3,694 x 2
#>    designer                                                  n_designers
#>    <chr>                                                           <int>
#>  1 IKEA of Sweden/John/Jonas/Petrus/Paul/Caroline/David Wahl           7
#>  2 K Hagberg/M Hagberg/John/Jonas/Petrus/Paul/Caroline                 7
#>  3 David Wahl/IKEA of Sweden/John/Jonas/Petrus/Paul/Caroline           7
#>  4 David Wahl/IKEA of Sweden/John/Jonas/Petrus/Paul/Caroline           7
#>  5 K Hagberg/M Hagberg/John/Jonas/Petrus/Paul/Caroline                 7
#>  6 IKEA of Sweden/John/Jonas/Petrus/Paul/Caroline/David Wahl           7
#>  7 Jon Karlsson/John/Jonas/Petrus/Paul/Caroline                        6
#>  8 David Wahl/John/Jonas/Petrus/Paul/Caroline                          6
#>  9 John/Jonas/Petrus/Paul/Caroline/Lisa Norinder                       6
#> 10 Lisa Norinder/John/Jonas/Petrus/Paul/Caroline                       6
#> # … with 3,684 more rows

ikea %>% 
  filter(is.na(designer))
#> # A tibble: 0 x 14
#> # … with 14 variables: X1 <int>, item_id <int>, name <chr>, category <chr>,
#> #   price <dbl>, old_price <chr>, sellable_online <lgl>, link <chr>,
#> #   other_colors <chr>, short_description <chr>, designer <chr>, depth <int>,
#> #   height <int>, width <int>
```

``` r
ikea_designers <- ikea %>% 
  separate(designer, into = paste0("designer", 1:7), sep = "/", fill = "right") %>% 
  pivot_longer(starts_with("designer"),
               names_to = "designer_no", values_to = "designer") %>% 
  drop_na(designer) %>% 
  mutate(designer_first_name = str_extract(designer, "^\\p{Lu}\\p{Ll}+"))

# ikea_designers %>% 
#   filter(is.na(designer_first_name)) %>% 
#   distinct(designer_first_name) %>% 
#   View()
```

``` r
designer_names <- ikea_designers %>% 
  drop_na(designer_first_name) %>% 
  pull(designer_first_name) %>% 
  unique()

names_genderized <- gender(designer_names, method = "genderize") %>% 
  # Manual fix-ups
  mutate(gender = case_when(name == "Ehlén" ~ "female",
                            name == "Synnöve" ~ "female",
                            name == "Hilland" ~ "female",
                            name == "Chenyi" ~ "female",
                            name == "Studio" ~ NA_character_,
                            TRUE ~ gender)) %>% 
  select(designer_first_name = name, gender)
```

``` r
ikea_gender_count <- ikea_designers %>% 
  left_join(names_genderized, by = "designer_first_name") %>% 
  drop_na(gender) %>% 
  distinct(item_id, category, gender, .keep_all = TRUE) %>% 
  count(category, gender)

ikea_gender_count %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  ggplot(aes(y = fct_reorder(category, female))) +
  geom_dumbbell(aes(x = female, xend = male),
                colour_x = "#72BEBA", colour_xend = "#D85827",
                size_x = 4.5, size_xend = 4.5,
                colour = "#978466", size = 2,
                dot_guide = TRUE, dot_guide_colour = "#FFDC00") +
  geom_text_repel(aes(x = n, y = category, label = n, colour = gender),
                  data = ikea_gender_count,
                  family = "Poppins", size = 5, direction = "x", nudge_y = -.4,
                  segment.size = 0, force = .1, show.legend = FALSE) +
  scale_colour_manual(values = c("#72BEBA", "#D85827")) +
  labs(x = NULL, y = NULL,
       title = "Number of <span style='color:#0057A5'>IKEA</span> 
       products designed by<br><br><span style='color:#72BEBA'>female</span> vs 
       <span style='color:#D85827'>male</span> designers<br>",
       caption = "<br>Gender is predicted based on designers' first names using the Genderize.io API.<br>
       Names that cannot be classified (e.g. initials only, studio names, \"IKEA\") are excluded.<br>
       Products designed by both female and male designers are counted twice.") +
  theme(axis.text.x = element_blank())
```

![](figs/dumbbell-1.png)<!-- -->
