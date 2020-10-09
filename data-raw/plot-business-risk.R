library("assertthat")
library("dplyr")
library("readr")
library("stringr")
library("sf")
library("rmapshaper")
library("ggplot2")
library("viridis")

if(!file.exists("inst/extdata/region/infuse_rgn_2011.shp")) {
    download.file("https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_rgn_2011.zip", destfile = "inst/extdata/region/infuse_rgn_2011.zip")
    unzip("inst/extdata/region/infuse_rgn_2011.zip", exdir = "inst/extdata/region")
}

if(!file.exists("inst/extdata/region/infuse_ctry_2011.shp")){
    download.file("https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_ctry_2011.zip", destfile = "inst/extdata/region/infuse_ctry_2011.zip")
    unzip("inst/extdata/region/infuse_ctry_2011.zip", exdir = "inst/extdata/region")
}

country = 
    sf::read_sf("inst/extdata/region/infuse_ctry_2011.shp") %>%
    filter(geo_label == "Scotland" | geo_label == "Wales") %>%
    select(c("geo_code", "geo_label", "label", "name", "geometry"))

region =
    sf::read_sf("inst/extdata/region/infuse_rgn_2011.shp") %>%
    select(c("geo_code", "geo_label", "label", "name", "geometry"))

region = bind_rows(region, country)
region = rmapshaper::ms_simplify(region, keep = 0.02)

cities = 
    readr::read_csv("cities-coordinates.csv", skip = 1) %>%  # first row is credit
    sf::st_as_sf(coords = c("longitude", "latitude")) %>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(crs = sf::st_crs(27700))

risk = 
    readr::read_csv("cities-business-risk.csv") %>%
    mutate(
        City = if_else(City == "Kingston upon Hull", "Hull", City),
        City = if_else(City == "Newcastle upon Tyne", "Newcastle Upon Tyne", City),
        City = if_else(City == "Brighton", "Brighton & Hove", City)
    )

nrow_risk = nrow(risk)

risk = left_join(risk, cities, by = c("City" = "city"))
risk = sf::st_as_sf(risk)

stopifnot(
    all.equal(nrow(risk), nrow_risk),
    all(!is.na(unlist(risk$geometry)))
)

ggplot() +
    geom_sf(data = region) +
    geom_sf(data = risk, aes(size = `% of total population`, colour = `% of total population`)) +
    scale_colour_viridis()
