rm(list = ls())
# package
library(sf)
library(mapview)
library(maps)
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

#### Loading data ####
# ------------------ #

pnb_trip <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_TsTrajets_filtre_Audrey_data.txt",
                       h = TRUE,
                       sep = "\t")
head(pnb_trip$DATE)

# ---- Date class
pnb_trip$DATE <- as.POSIXct(pnb_trip$DATE,
                         format = "%Y-%m-%d %H:%M")

# Localisation colonie
lon_col = 55.5
lat_col = -21.25

pnb_mig <- pnb_trip[pnb_trip$TYPE == "MIG", ]
dim(pnb_mig) # from 6481 rows to 3620
table(pnb_mig$TYPE)

#### Data filtre - oiseau avec dosage isotope uniquement ####
# --------------------------------------------------------- #
pnb_mig <- pnb_mig[pnb_mig$ID != "PNB-C2143",]
table(pnb_mig$ID)
length(unique(pnb_mig$ID))

#### Sf/sp class ####
# ----------------- #
# Conversion in sf Spatial Object
projLatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# UTM 43S corresponding to the center of the Indian ocean
# UTM 43 => 32743
projUTM <- '+init=epsg:32743'

# Non projected spatial object
pnb_sf <- st_as_sf(pnb_mig,
                   coords = c("LON", "LAT"),
                   crs = projLatLon)

pnb_sp <- as(pnb_sf, "Spatial")

# Projected spatial object
pnb_sf_utm <- st_transform(pnb_sf,
                           crs = 32743)
pnb_sp_utm <- as(pnb_sf_utm,
                 "Spatial")

# list one level per bird
pnb_sf_utm_list <- split(pnb_sf_utm,
                         pnb_sf_utm$ID)

#### Range of dates for each tracks ####
range_mig <- lapply(pnb_sf_utm_list,
                    function(x) {
                        min_date <- range(x$DATE)[1]
                        max_date <- range(x$DATE)[2]
                        mig_day <- round(diff(range(x$DATE)))
                        mat <- st_distance(x)
                        diago <- diag(mat[, -1])
                        dist_trav <- sum(diago)/1000 # distance in km
                        loc_number <- nrow(x)
                        data.frame(min_date,
                                   max_date,
                                   mig_day,
                                   dist_trav,
                                   loc_number)
                        })
range_mig_df <- do.call("rbind",
                        range_mig)
range_mig_df$ID <- row.names(range_mig_df)

range_lat_lon <- lapply(split(pnb_mig, pnb_mig$ID),
                    function(x) {
                        lat <- range(x$LAT)
                        lon <- range(x$LON)
                        data.frame(min_lat = lat[1],
                                   max_lat = lat[2],
                                   min_lon = lon[1],
                                   max_lon = lon[2])
                    })
range_lat_lon_df <- do.call("rbind", range_lat_lon)
range_lat_lon_df$ID <- row.names(range_lat_lon_df)

final_df <- left_join(range_lat_lon_df,
                      range_mig_df,
                      by = "ID")

final_df <- final_df[, c(5:10, 1:4)]
final_df <- final_df[order(final_df$min_date), ]
final_df$dist_trav <- as.numeric(final_df$dist_trav)

# saveRDS(final_df,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_synthese_par_ind.rds")
