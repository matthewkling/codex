

f <- "variables.csv"
base_url <- "https://github.com/matthewkling/ca2cc-county/raw/main/data/output/"
download.file(paste0(base_url, f),
              paste0("local_data/", f))


read_csv(paste0("local_data/", "variables.csv")) %>% saveRDS("data/variables.rds")


d <- bind_rows(
  read_csv(paste0("local_data/", "time_series_data.csv"), col_types = "ccccddddd") %>%
    mutate(value = ifelse(source_dataset == "AgCensus", imputed, value)), 
  
  read_csv(paste0("local_data/", "static_data.csv")) %>%
    mutate(source_dataset = ifelse(variable == "pct_clay", "SoilGrids", "Land grants"),
           year = "NA"), 
  
  read_csv(paste0("local_data/", "climate_data.csv")) %>%
    gather(variable, value, -fips, -season, -start, -end) %>%
    unite(variable, variable, season) %>%
    unite(year, start, end, sep = "-") %>%
    mutate(source_dataset = "Schlenker climate",
           fips = str_pad(fips, 5, "left", 0)),
  
  read_csv(paste0("local_data/", "gini_agland_counties.csv")) %>%
    mutate(source_dataset = "Gini",
           year = as.character(year),
           variable = "gini_index") %>%
    rename(fips = fips_code,
           value = gini)) %>%
  select(source_dataset:value) %>%
  mutate(id = paste(source_dataset, variable, year) %>% factor() %>% as.integer())

lookup <- d %>%
  select(source_dataset, variable, year, id) %>%
  distinct()
saveRDS(lookup, "data/lookup.rds")

for(i in unique(d$id)) d %>% filter(id == i) %>% 
  select(fips, value) %>% saveRDS(paste0("data/", i, ".rds"))



weights <- d %>% filter(variable == "acres_operated", year == "2017") %>%
  select(fips, farmland = value)

counties <- rgdal::readOGR("local_data/Counties/cb_2018_us_county_500k.shp")
counties <- counties[counties$STATEFP != "02",]
counties@data <- counties@data %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  select(fips, NAME) %>%
  left_join(weights)

saveRDS(counties, "data/counties.rds")
