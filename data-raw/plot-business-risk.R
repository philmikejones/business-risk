library("assertthat")
library("dplyr")
library("readr")
library("readxl")
library("stringr")
library("sf")
library("rmapshaper")
library("ggplot2")
library("viridis")

dir.create("inst/extdata/regions", recursive = TRUE, showWarnings = FALSE)
dir.create("export")

if(!file.exists("inst/extdata/regions/infuse_rgn_2011.shp")) {
    download.file("https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_rgn_2011.zip", destfile = "inst/extdata/regions/infuse_rgn_2011.zip")
    unzip("inst/extdata/regions/infuse_rgn_2011.zip", exdir = "inst/extdata/regions")
}

if(!file.exists("inst/extdata/regions/infuse_ctry_2011.shp")){
    download.file("https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_ctry_2011.zip", destfile = "inst/extdata/regions/infuse_ctry_2011.zip")
    unzip("inst/extdata/regions/infuse_ctry_2011.zip", exdir = "inst/extdata/regions")
}

country = 
    sf::read_sf("inst/extdata/regions/infuse_ctry_2011.shp") %>%
    filter(geo_label == "Scotland" | geo_label == "Wales") %>%
    select(c("geo_code", "geo_label", "label", "name", "geometry"))

regions =
    sf::read_sf("inst/extdata/regions/infuse_rgn_2011.shp") %>%
    select(c("geo_code", "geo_label", "label", "name", "geometry"))

regions = bind_rows(regions, country)
regions = rmapshaper::ms_simplify(regions, keep = 0.02)

cities = 
    readr::read_csv("cities-coordinates.csv", skip = 1) %>%  # first row is credit
    sf::st_as_sf(coords = c("longitude", "latitude")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(crs = sf::st_crs(27700))

risk = 
    readxl::read_xlsx("cities-business-risk.xlsx", sheet = "Sheet1") %>%
    select(-`...15`, -`...21`) %>%
    mutate(
        City = if_else(City == "Kingston upon Hull", "Hull", City),
        City = if_else(City == "Newcastle upon Tyne", "Newcastle Upon Tyne", City),
        City = if_else(City == "Brighton", "Brighton & Hove", City)
    ) %>%
    mutate(
        p_bus_risk = `Total at Risk` / Total,
        p_bus_pop_risk = `Total at Risk` / Population
    )

nrow_risk = nrow(risk)

risk = left_join(risk, cities, by = c("City" = "city"))
risk = sf::st_as_sf(risk)

stopifnot(
    all.equal(nrow(risk), nrow_risk),
    all(!is.na(unlist(risk$geometry)))
)

region = regions[regions[["name"]] == "East Midlands", ]
risk = risk[region, ]
plot = ggplot() +
    geom_sf(data = region) +
    geom_sf(data = risk, aes(size = p_bus_risk, colour = `% of total population`)) +
    scale_colour_viridis()
ggsave(plot, file = paste0("export/region_", "eastmidlands", "_bus_denom.pdf"))

plot = ggplot() +
    geom_sf(data = region) +
    geom_sf(data = risk, aes(size = p_bus_pop_risk, colour = p_bus_pop_risk)) +
    scale_colour_viridis()
ggsave(plot, file = paste0("export/region_", "eastmidlands", "_popdenom.pdf"))
