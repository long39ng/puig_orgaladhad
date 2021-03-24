library(shiny)
library(shinycssloaders)
library(data.table)
library(dplyr)
library(tidyr)
library(widyr)
library(sf)
library(leaflet)

# Data --------------------------------------------------------------------

roll_calls <- readRDS("roll_calls.RDS")

issues <- readRDS("issues.RDS")

un_votes <- readRDS("un_votes.RDS")

country_codes <- un_votes %>%
  distinct(country, country_code) %>%
  rename(iso_a2 = country_code) %>%
  mutate(iso_a2 = case_when(
    # Missing country codes:
    country == "Federal Republic of Germany" ~ "DE",
    country == "Yemen Arab Republic" ~ "YE",
    TRUE ~ iso_a2
  )) %>%
  drop_na()

votes_by_issue <- un_votes %>%
  left_join(issues, by = "rcid") %>%
  drop_na(issue) %>%
  split(by = "issue")

votes_by_issue[["(All issues)"]] <- un_votes

world_map <- readRDS("world_map.RDS")

# Helpers ----------------------------------------------------------------

vote_cors <- function(tbl) {
  tbl %>%
    mutate(vote = as.integer(vote)) %>%
    pairwise_cor(country, rcid, vote, use = "pairwise.complete.obs") %>%
    left_join(country_codes, by = c("item2" = "country"))
}

filter_country <- function(tbl, country) {
  tbl %>%
    filter(item1 == country) %>%
    add_row(
      item1 = country,
      item2 = country,
      correlation = 1,
      iso_a2 = country_codes$iso_a2[country_codes$country == country]
    ) %>%
    mutate(label = paste0("<strong>", item2, ":</strong> ", round(correlation, 2)))
}

add_geometry <- function(tbl) {
  world_map %>%
    left_join(tbl, by = "iso_a2") %>%
    drop_na(iso_a2)
}

fill_palette <- colorNumeric(
  "RdYlBu",
  domain = c(-1, 1),
  na.color = "#cccccc"
)

legend_palette <- colorNumeric(
  "RdYlBu",
  domain = c(-1, 1),
  na.color = "#cccccc",
  reverse = TRUE
)

find_country <- function(lng, lat) {
  ref <- world_map
  st_agr(ref) <- "constant"

  clicked_iso2c <- st_point(c(lng, lat)) %>%
    st_sfc(crs = st_crs(world_map)) %>%
    st_intersection(ref, .) %>%
    pull(iso_a2)

  clicked_country <- country_codes$country[country_codes$iso_a2 == clicked_iso2c]

  clicked_country[length(clicked_country)]
}

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "//brick.freetls.fastly.net/Fira+Sans:400,500,700",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body, .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {
        background-color: #eeeeee;
        font-family: 'Fira Sans', sans-serif;
      }
      .irs, .leaflet, .info {
        font-family: 'Fira Sans', sans-serif;
        font-variant-numeric: tabular-nums;
      }
      .info {
        background-color: #f5f5f5;
      }
      .leaflet-container {
        background: transparent;
      }
    ")),
  ),

  titlePanel(
    title = "",
    windowTitle = "World Map of Voting Agreement in the UN General Assembly"
  ),

  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("country",
        "Country:",
        choices = sort(unique(un_votes$country))
      ),
      div("(You can also click on the map to select a country.)"),
      sliderInput("years",
        "Years:",
        min = min(un_votes$year),
        max = max(un_votes$year),
        value = range(un_votes$year),
        ticks = FALSE,
        sep = ""
      ),
      radioButtons("issue",
        "Issue:",
        choices = sort(names(votes_by_issue))
      ),
      br(),
      hr(),
      div(a(
        "Source code",
        href = "https://github.com/long39ng/puig_orgaladhad/tree/main/2021_13_un_votes/"
      ))
    ),
    mainPanel(width = 10,
      h2(textOutput("maptitle")),
      leafletOutput("cormap", height = "calc(100vh - 100px)") %>%
        withSpinner()
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$maptitle <- renderText({
    paste0(
      input$country,
      "'s voting agreement with other countries in the UN General Assembly"
    )
  })

  cors_world <- reactive({
    votes_by_issue[[input$issue]] %>%
      filter(year %in% input$years[1]:input$years[2]) %>%
      vote_cors()
  })

  output$cormap <- renderLeaflet({
    leaflet(
      world_map,
      options = leafletOptions(zoomDelta = .25, zoomSnap = .25)
    ) %>%
      addPolygons(fillOpacity = 0, weight = 0) %>%
      addLegend(
        pal = legend_palette,
        values = c(-1, 1),
        opacity = 1,
        labFormat = labelFormat(
          transform = function(x) sort(x, decreasing = TRUE)
        ),
        title = "Correlation"
      ) %>%
      setView(lat = 30, lng = 0, zoom = 2.25)
  })

  observe({
    leafletProxy(
      "cormap",
      data = cors_world() %>%
        filter_country(input$country) %>%
        add_geometry()
    ) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ fill_palette(correlation), fillOpacity = 1,
        color = "#aaaaaa", weight = .5, opacity = 1,
        label = ~ lapply(label, HTML),
        labelOptions = labelOptions(textsize = "14px"),
        highlightOptions = highlightOptions(
          color = "#444444", weight = 2, bringToFront = TRUE
        )
      )
  })

  observeEvent(input$cormap_shape_click, {
    clicked_country <- find_country(
      lng = input$cormap_shape_click$lng,
      lat = input$cormap_shape_click$lat
    )

    if (length(clicked_country) == 1 && clicked_country %in% unique(un_votes$country)) {
      updateSelectInput(inputId = "country", selected = clicked_country)
    }
  })
}

shinyApp(ui, server)
