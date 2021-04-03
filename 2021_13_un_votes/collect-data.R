library(data.table)
library(dplyr)
library(rnaturalearth)
library(sf)

roll_calls <- fread(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv"
)

issues <- fread(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv"
)

un_votes <- fread(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv"
) %>%
  mutate(vote_num = match(vote, c("no", "abstain", "yes")) - 2L) %>%
  left_join(
    roll_calls %>%
      mutate(year = session + 1945L) %>%
      select(rcid, year),
    by = "rcid"
  ) %>%
  filter(country != "Zanzibar") # Only two votes, both "abstain"

world_map <- ne_countries(returnclass = "sf", scale = "medium") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  filter(name != "Antarctica") %>%
  select(iso_a2, geometry)

saveRDS(roll_calls, "2021_13_un_votes/unvotes-agreement-map/roll_calls.RDS")
saveRDS(issues, "2021_13_un_votes/unvotes-agreement-map/issues.RDS")
saveRDS(un_votes, "2021_13_un_votes/unvotes-agreement-map/un_votes.RDS")
saveRDS(world_map, "2021_13_un_votes/unvotes-agreement-map/world_map.RDS")
