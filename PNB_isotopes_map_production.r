rm(list = ls())
# package
library(sf)
library(mapview)
library(maps)
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# DATA #
iso <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates_SST_degre.rds")
iso_sp <- SpatialPointsDataFrame(coords = iso[, c("CENTRO_LON", "CENTRO_LAT")],
                                 data = iso,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

kernels <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/kernel50-90.rds")
str(kernels)

k50 <- lapply(kernels,
              function(x){
                   ker <- x[[1]]
                   ker
              })
k50_sp <- lapply(k50, as, "Spatial")

sst <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_SST_mean_global.rds")
sst <- terra::rast(sst)
new_ext <- extent(20, 130, -50, 30 ) #extent(xmin, xmax, ymin, ymax)
sst_crop <- crop(x = sst,
              y = new_ext)

bathy <- terra::rast("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/Indian_Ocean_Bathy.tif")

# Palette color for bathy
# https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-colour-palettes.html
blue.col <- colorRampPalette(c("darkblue", "lightblue"))

mi <- min(values(bathy)) + 100
ma <- max(values(bathy)) + 100

# Break points sequence for below sea level
s1 <- seq(from = mi, to = 0, by = 0 - mi / 50)
# Break points sequence for above sea level
s2 <- seq(from = 0, to = ma, by = ma / 50)

# Round sequence to nearest 100
s1 <- round(s1, -2)
s2 <- round(s2, -2)

# Only show unique numbers
s1 <- unique(s1)
s2 <- unique(s2)

# Combine sequences and remove the first value from second sequence
s3 <- c(s1, s2[-1])

#### kernel map without background ####
# png("C:/Users/ccjuhasz/Desktop/ISOSCAPE_maps/ISOSCAPES_K50_global_noBG2_vert.png",
#     width = 1600,
#     height = 1000,
#     units = "px",
#     pointsize = 12,
#     bg = "white",
#     res = 200)
levelplot(bathy,
          margin = FALSE,
          col.regions = c(rep("transparent", 50),
                          terrain.colors(50, rev = F)),
          at = s3,
          color.key = FALSE,
          panel = function(...) {
               lapply(k50_sp,
                      sp.polygons,
                      col = NA,
                      fill = "#8b2a0072")
               sp.points(iso_sp, col = "black")
               
               panel.levelplot(...)
          })

dev.off()
#### kernel map with bathy background ####
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_maps/ISOSCAPES_K50_global_bathyBG3.png",
#     width = 1600,
#     height = 1000,
#     units = "px",
#     pointsize = 12,
#     bg = "white",
#     res = 200)

levelplot(bathy,
          margin = FALSE,
          col.regions = c(blue.col(50),
                          terrain.colors(50, rev = F)),
          at = s3,
          color.key = FALSE) +
levelplot(bathy,
          margin = FALSE,
          col.regions = c(rep("transparent", 50),
                          terrain.colors(50, rev = F)),
          at = s3,
          color.key = FALSE,
          panel = function(...) {
               lapply(k50_sp,
                      sp.polygons,
                      col = "black",
                      fill = "#423c3a51")
               sp.points(iso_sp, col = "black")               
               panel.levelplot(...)
          })

dev.off()
#### kernel map with SST background ####

# sst palette
## Option 1: Use `at=` and `col.regions=` to set color gradient
nlev <- 200
my_at <- seq(from = min(values(sst), na.rm = T),
             to = max(values(sst), na.rm = T),
             length.out = nlev + 1)
my_cols <- viridis_pal(option = "H", direction = 1)(nlev)
levelplot(sst, margin = FALSE,
          at = my_at,
          col.regions = my_cols)

# png("C:/Users/ccjuhasz/Desktop/ISOSCAPE_maps/ISOSCAPES_K50_global_sstBG5.png",
#     width = 1600,
#     height = 1000,
#     units = "px",
#     pointsize = 12,
#     bg = "white",
#     res = 200)

x11()
levelplot(sst_crop,
          margin = FALSE,
          color.key = FALSE,
          at = my_at,
          col.regions = my_cols) +
levelplot(bathy,
          margin = FALSE,
          col.regions = c(rep("transparent", 50),
                          rep("white", 50)),
          at = s3,
          color.key = FALSE,
          panel = function(...) {
               lapply(k50_sp,
                      sp.polygons,
                      col = "black",
                      fill = "#423c3a72")
               sp.points(iso_sp, col = "black")               
               panel.levelplot(...)
          })

dev.off()

# diminution de l'emprise (taille de la carte)

new_ext2 <- extent(20, 130, -40, 30 ) #extent(xmin, xmax, ymin, ymax)
sst_dec <- crop(x = sst,
                y = new_ext2)
bathy_dec <- crop(x = bathy,
                  y = new_ext2)

nlev <- 200
my_at <- seq(from = min(values(sst_dec), na.rm = T),
             to = max(values(sst_dec), na.rm = T),
             length.out = nlev + 1)
my_cols <- viridis_pal(option = "H", direction = 1)(nlev)

x11()
levelplot(sst_dec,
          margin = FALSE,
          at = my_at,
          col.regions = my_cols)

# png("C:/Users/ccjuhasz/Desktop/ISOSCAPE_maps/ISOSCAPES_K50_global_sst_dec.png",
#     width = 1600,
#     height = 1000,
#     units = "px",
#     pointsize = 12,
#     bg = "white",
#     res = 200)

levelplot(sst_dec,
          margin = FALSE,
          color.key = FALSE,
          at = my_at,
          col.regions = my_cols) +
levelplot(bathy_dec,
          margin = FALSE,
          col.regions = c(rep("transparent", 50),
                          rep("white", 50)),
          at = s3,
          color.key = FALSE,
          panel = function(...) {
               lapply(k50_sp,
                      sp.polygons,
                      col = "black",
                      fill = "#423c3a72")
               sp.points(iso_sp, col = "black")               
               panel.levelplot(...)
          })

dev.off()