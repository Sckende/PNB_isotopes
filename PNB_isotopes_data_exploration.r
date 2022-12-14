# obj - calcul des kernel 50 par individu
    #   - extraire lat/long du centroide du K50
    #   - extraire min & max des dates des points dans le K50
    #   - moyenne + sd des SST sous le K50 dans la periode de temps minDate maxDate
    #   - completer le tableau de synthese avec dosage isotopes avec les informations précédentes
    
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
                         format = "%Y-%m-%d %H:%M") # Date format


#### Data filtre - migration uniquement ####
# ---------------------------------------- #
# Localisation colonie
lon_col = 55.5
lat_col = -21.25

plot(pnb_trip$LON[pnb_trip$TYPE == "REPRO"],
     pnb_trip$LAT[pnb_trip$TYPE == "REPRO"],
     pch = 16,
     col = "#000000a7",
     xlim = c(min(pnb_trip$LON), max(pnb_trip$LON)),
     ylim = c(min(pnb_trip$LAT), max(pnb_trip$LAT)))
points(pnb_trip$LON[pnb_trip$TYPE == "MIG"],
     pnb_trip$LAT[pnb_trip$TYPE == "MIG"],
     pch = 16,
     col = "#a9a9a976")

map("world",
    add = TRUE,
    col = "#cecaca",
    fill = TRUE)

ll1 <- split(pnb_trip, pnb_trip$ID)

# lapply(ll1, function(x) {
    
#     # Localisation visualisation mig vs repro
#     png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/maps/repro_vs_mig/PNB_isotope_zone_activite",
#               unique(x$ID),
#               ".png",
#               sep = ""),
#     res = 300,
#     width = 20,
#     height = 15,
#     pointsize = 12,
#     unit = "cm",
#     bg = "white")
    
#     plot(x$LON[x$TYPE == "REPRO"],
#      x$LAT[x$TYPE == "REPRO"],
#      main = unique(x$ID),
#      xlab = "Longitude",
#      ylab = "Latitude",
#      pch = 16,
#      col = "#000000a7",
#      bty = "n",
#      xlim = c(min(x$LON), max(x$LON)),
#      ylim = c(min(x$LAT), max(x$LAT)))
    
#     points(x$LON[x$TYPE == "MIG"],
#            x$LAT[x$TYPE == "MIG"],
#            pch = 16,
#            col = "#a9a9a976")
    
#     points(x = 55.5,
#            y = -21.25,
#            pch = 15,
#            col = "darkorange")

#     legend("topleft",
#            legend = c("repro", "mig"),
#            fill = c("black", "darkgrey"),
#            bty = "n",
#            horiz = FALSE)
#     dev.off()
    
#     # Min & max dates for each periods
    
#     min_repro <- min(x$DATE[x$TYPE == "REPRO"])
#     max_repro <- max(x$DATE[x$TYPE == "REPRO"])
    
#     min_mig <- min(x$DATE[x$TYPE == "MIG"])
#     max_mig <- max(x$DATE[x$TYPE == "MIG"])
    
#     print(paste("Periode migration : ", min_mig, "-", max_mig))
#     print(paste("Periode reproduction : ", min_repro, "-", max_repro))
    
# })

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

#### Data visualisation ####
# ------------------------ #

# ---- Factor class
pnb_sp_utm$ID <- as.factor(pnb_sp_utm$ID)
mapview(pnb_sp_utm,
        zcol = "ID")

# ---- Creation d'un deuxieme ID pour voir tous les points d'un meme individu
# head(pnb_sp_utm)
# pnb_sp_utm$ID2 <- substr(pnb_sp_utm$ID,
#                          5,
#                          9)

# pnb_list <- split(pnb_sp_utm,
#                   pnb_sp_utm$ID2)
# mapview(pnb_list)

# ---- Creation d'une liste par ID
pnb_list2 <- split(pnb_sp_utm,
                  pnb_sp_utm$ID)
mapview(pnb_list2)

#### kernel 50 ####
# --------------- #

# ATTENTION - Calcul des kernels à partir des données en UTM #
summary(pnb_list2)
length(pnb_list2)

ker50_list <- lapply(pnb_list2,
                     function(x) {
                         KUD <- kernelUD(x,
                                        #  h = 2,
                                        h = "href",
                                         grid = 500
                                         )
                         KUDvol <- getvolumeUD(KUD)
                         ver50 <- getverticeshr(KUDvol, 50)
                         ver50
                     })
length(ker50_list)
mapview(ker50_list)

ker90_list <- lapply(pnb_list2,
                     function(x) {
                         KUD <- kernelUD(x,
                                        #  h = 2,
                                        h = "href",
                                         grid = 500
                                         )
                         KUDvol <- getvolumeUD(KUD)
                         ver90 <- getverticeshr(KUDvol, 90)
                         ver90
                     })
length(ker90_list)
mapview(ker90_list)

#### map production ####

# Cadrage de la donnees - defini la fenetre de l'ocean indien ou les petrels peuvent potentiellement s'alimenter
lon_min = 20
lon_max = 130 
lat_min = -40
lat_max = 30

# ----- #

kl <- 
lapply(pnb_list2, function (x) {
    
    print(unique(x$ID))
    
    
    KUD <- kernelUD(x,
                #    h = 2,
                h = "href",
                grid = 500)
    KUDvol <- getvolumeUD(KUD)
    ver50 <- getverticeshr(KUDvol, 50) # calcul en UTM
    ver50sf <- st_as_sf(ver50) 
    ver50spLL <- st_transform(ver50sf, crs = 4326) # retour en lat lon pour les cartes
    
    ver90 <- getverticeshr(KUDvol, 90)
    ver90sf <- st_as_sf(ver90) 
    ver90spLL <- st_transform(ver90sf, crs = 4326)
    
    kernel_list <- list(ver50spLL, ver90spLL)
    kernel_list
})

for(i in 1:length(kl)){
    names(kl[[i]])[1] <- names(kl[i])
    names(kl[[i]])[2] <- names(kl[i])    
   }
# for the GithubPage
# saveRDS(kl,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/kernel50-90.rds")

lapply(kl, function(x) {
    id <- names(x)[1]
    print(id)
        
    # png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/maps/kernels/PNB_isotope_ker_50_90_",
    #           id,
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 70,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")
    x11()    
    plot(lon_col,
         lat_col,
         pch = 16,
         col = "darkred",
         cex.lab = 2,
         font.lab = 2,
         main = id,
         xlab = "Longitude",
         ylab = "Latitude",
         cex.axis = 1.5,
         xlim = c(lon_min, lon_max),
         ylim = c(lat_min, lat_max))
  
  plot(x[[2]],
       add = TRUE,
       lwd = 1.5,
       border = "#bfd1ff",
       col = "#bfd1ff")
  
  plot(x[[1]],
       add = TRUE,
       lwd = 1.5,
       border = "#004FFF",
       col = "#004FFF")
  plot(pnb_sf[pnb_sf$ID == id,],
       add = TRUE,
       col = "darkgrey",
       pch = 3,
       cex = 2,
       lwd = 2)
  
  map('world',
      add = T,
      fill = T,
      col = gray(0.86),
      border = gray(0.5))
  
  
  dev.off()
})

#### Centroides des kernels 50 ####

kern <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/kernel50-90.rds")
kern

ker50 <- lapply(kern, function(x) {x[[1]]})
length(ker50)
mapview::mapview(ker50)

# ----- test for kernel with 2 areas, such as ker50[[9]] ker50[[22]]
test <- ker50[[22]]
plot(sf::st_geometry(test))
plot(sf::st_centroid(test), add = T)

test1 <- ker50[[9]]
plot(sf::st_geometry(test1))
plot(sf::st_centroid(test1), add = T)

# ----- Calcul automatique des centroides de kernel

centro <- lapply(ker50,
                 function(x) {
                     
                     if(sf::st_is_valid(x) == FALSE) { # certains polygones non valides
                         x <- sf::st_make_valid(x)
                     }
                     sf::st_centroid(x)
                 })
# ----- verification visuelle

# for (i in 1:length(ker50)) {
#     x11()
#     plot(sf::st_geometry(ker50[[i]]))
#     plot(centro[[i]],
#          add = T,
#          col = "red")
# }

# ----- extraction & completion des metadonnees
coord_centro <- lapply(centro,
                       function(x) {
                           sf::st_coordinates(x)
                       })

coord <- data.frame(ID = names(centro), do.call("rbind", coord_centro), row.names = NULL)
names(coord) <- c("ID", "CENTRO_LON", "CENTRO_LAT")

# verification visuelle
# for (i in 1:length(ker50)) {
#     x11()
#     plot(sf::st_geometry(ker50[[i]]))
#     points(coord$CENTRO_LON[i],
#          coord$CENTRO_LAT[i],
#          col = "red")
# }

# Creation du nouveau fichier
# metadat <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_SI_HG_GLS.txt",
#                       sep = "\t",
#                       dec = ",",
#                       h = TRUE)
# metadat
# all(metadat$ID %in% coord$ID)
# all(coord$ID %in% metadat$ID)

# metadat2 <- left_join(metadat,
#                       coord,
#                       by = "ID")

# write.table(metadat2,
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_SI_HG_GLS_centroide.txt",
#             sep = "\t")

#### Regression entre carbone, azote & mercure vs. centroides kernel 50 ####

iso <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_SI_HG_GLS_centroide.txt",
                      sep = "\t",
                      dec = ".",
                      h = TRUE)
iso

### MERCURY vs. LATITUDE CENTROIDE
plot(iso$CENTRO_LAT,
     iso$HG_MEAN,
     xlab = "Latitude",
     ylab = "Mean Hg",
     bty = "n")

# ----> models
mod1 <- lm(iso$HG_MEAN ~ iso$CENTRO_LAT)
summary(mod1)

mod_quadra1 <- lm(iso$HG_MEAN ~ poly(iso$CENTRO_LAT, 2))
summary(mod_quadra1)

mod_gam1 <- mgcv::gam(iso$HG_MEAN ~ s(iso$CENTRO_LAT))
summary(mod_gam1)

# Visualization
pred_mod <- predict.lm(mod1,
                    se.fit = TRUE)
pred_mod_quadra <- predict.lm(mod_quadra1,
                              se.fit = TRUE)
pred_mod_gam <- predict(mod_gam1,
                        se.fit = TRUE)
pred_df <- data.frame(lat = iso$CENTRO_LAT,
                      fit_hg = pred_mod$fit,
                      se_fit_hg = pred_mod$se.fit,
                      fit_hg_quadra = pred_mod_quadra$fit,
                      se_fit_hg_quadra = pred_mod_quadra$se.fit,
                      fit_hg_gam = pred_mod_gam$fit,
                      se_fit_hg_gam = pred_mod_gam$se.fit)
pred_df <- pred_df[order(pred_df$lat), ]

lines(pred_df$lat,
    pred_df$fit_hg,
     col = "red")
lines(pred_df$lat,
      pred_df$fit_hg - pred_df$se_fit_hg,
      col = "red",
      lty = 4)
lines(pred_df$lat,
      pred_df$fit_hg + pred_df$se_fit_hg,
      col = "red",
      lty = 4)

lines(pred_df$lat,
    pred_df$fit_hg_quadra,
     col = "darkgreen")
lines(pred_df$lat,
      pred_df$fit_hg_quadra - pred_df$se_fit_hg_quadra,
      col = "darkgreen",
      lty = 4)
lines(pred_df$lat,
      pred_df$fit_hg_quadra + pred_df$se_fit_hg_quadra,
      col = "darkgreen",
      lty = 4)

lines(pred_df$lat,
    pred_df$fit_hg_gam,
     col = "darkblue")
lines(pred_df$lat,
      pred_df$fit_hg_gam - pred_df$se_fit_hg_gam,
      col = "darkblue",
      lty = 4)
lines(pred_df$lat,
      pred_df$fit_hg_gam + pred_df$se_fit_hg_gam,
      col = "darkblue",
      lty = 4)

# MERCURY vs. LONGITUDE CENTROIDE
plot(iso$CENTRO_LON,
     iso$HG_MEAN,
     xlab = "Longitude",
     ylab = "Mean Hg",
     bty = "n")

mod11 <- lm(iso$HG_MEAN ~ iso$CENTRO_LON)
summary(mod11)

pred_mod11 <- predict.lm(mod11,
                    se.fit = TRUE)
pred_df11 <- data.frame(lon = iso$CENTRO_LON,
                        fit_hg = pred_mod11$fit,
                        se_fit_hg = pred_mod11$se.fit)
pred_df11 <- pred_df11[order(pred_df11$lon), ]

lines(pred_df11$lon,
      pred_df11$fit_hg,
      col = "red")
lines(pred_df11$lon,
      pred_df11$fit_hg - pred_df11$se_fit_hg,
      col = "red",
      lty = 4)
lines(pred_df11$lon,
      pred_df11$fit_hg + pred_df11$se_fit_hg,
      col = "red",
      lty = 4)

# CARBON vs. LATITUDE CENTROIDE
plot(iso$CENTRO_LAT,
     iso$d13C_MEAN,
     xlab = "Latitude",
     ylab = "Mean d13C",
     bty = "n")
mod2 <- lm(iso$d13C_MEAN ~ iso$CENTRO_LAT)
summary(mod2)

pred_mod2 <- predict.lm(mod2,
                    se.fit = TRUE)
pred_df2 <- data.frame(lat = iso$CENTRO_LAT,
                       fit_13c = pred_mod2$fit,
                       se_fit_13c = pred_mod2$se.fit)
pred_df2 <- pred_df2[order(pred_df2$lat), ]

lines(pred_df2$lat,
      pred_df2$fit_13c,
      col = "red")
lines(pred_df2$lat,
      pred_df2$fit_13c - pred_df2$se_fit_13c,
      col = "red",
      lty = 4)
lines(pred_df2$lat,
      pred_df2$fit_13c + pred_df2$se_fit_13c,
      col = "red",
      lty = 4)

# CARBON vs. LONGITUDE CENTROIDE
plot(iso$CENTRO_LON,
     iso$d13C_MEAN,
     xlab = "Longitude",
     ylab = "Mean d13C",
     bty = "n")
mod22 <- lm(iso$d13C_MEAN ~ iso$CENTRO_LON)
summary(mod22)

pred_mod22 <- predict.lm(mod22,
                         se.fit = TRUE)
pred_df22 <- data.frame(lon = iso$CENTRO_LON,
                        fit_13c = pred_mod22$fit,
                        se_fit_13c = pred_mod22$se.fit)
pred_df22 <- pred_df22[order(pred_df22$lon), ]

lines(pred_df22$lon,
      pred_df22$fit_13c,
      col = "red")
lines(pred_df22$lon,
      pred_df22$fit_13c - pred_df22$se_fit_13c,
      col = "red",
      lty = 4)
lines(pred_df22$lon,
      pred_df22$fit_13c + pred_df22$se_fit_13c,
      col = "red",
      lty = 4)

# AZOTE vs LATITUDE CENTROIDE
plot(iso$CENTRO_LAT,
     iso$d15N_MEAN,
     xlab = "Latitude",
     ylab = "Mean d15N",
     bty = "n")

mod3 <- lm(iso$d15N_MEAN ~ iso$CENTRO_LAT)
summary(mod3)

pred_mod3 <- predict.lm(mod3,
                        se.fit = TRUE)
pred_df3 <- data.frame(lat = iso$CENTRO_LAT,
                       fit_15n = pred_mod3$fit,
                       se_fit_15n = pred_mod3$se.fit)
pred_df3 <- pred_df3[order(pred_df3$lat), ]

lines(pred_df3$lat,
      pred_df3$fit_15n,
      col = "red")
lines(pred_df3$lat,
      pred_df3$fit_15n - pred_df3$se_fit_15n,
      col = "red",
      lty = 4)
lines(pred_df3$lat,
      pred_df3$fit_15n + pred_df3$se_fit_15n,
      col = "red",
      lty = 4)

# AZOTE vs LONGITUDE CENTROIDE
plot(iso$CENTRO_LON,
     iso$d15N_MEAN,
     xlab = "Longitude",
     ylab = "Mean d15N",
     bty = "n")

mod33 <- lm(iso$d15N_MEAN ~ iso$CENTRO_LON)
summary(mod33)

pred_mod33 <- predict.lm(mod33,
                         se.fit = TRUE)
pred_df33 <- data.frame(lon = iso$CENTRO_LON,
                        fit_15n = pred_mod33$fit,
                        se_fit_15n = pred_mod33$se.fit)
pred_df33 <- pred_df33[order(pred_df33$lon), ]

lines(pred_df33$lon,
      pred_df33$fit_15n,
      col = "red")
lines(pred_df33$lon,
      pred_df33$fit_15n - pred_df33$se_fit_15n,
      col = "red",
      lty = 4)
lines(pred_df33$lon,
      pred_df33$fit_15n + pred_df33$se_fit_15n,
      col = "red",
      lty = 4)

# global pred for GH page
# glob_pred_LAT <- left_join(pred_df,
#                            pred_df2,
#                            "lat")
# glob_pred_LAT <- left_join(glob_pred_LAT,
#                            pred_df3,
#                            "lat")

# glob_pred_LON <- left_join(pred_df11,
#                            pred_df22,
#                            "lon")
# glob_pred_LON <- left_join(glob_pred_LON,
#                            pred_df33,
#                            "lon")
# saveRDS(glob_pred_LAT,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_pred_LAT_vs_HG_C_N.rds")

# saveRDS(glob_pred_LON,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_pred_LON_vs_HG_C_N.rds")

# saveRDS(list(mod1, mod2, mod3),
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_lm_LAT_vs_HG_C_N.rds")

# saveRDS(list(mod11, mod22, mod33),
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_lm_LON_vs_HG_C_N.rds")
# ---------- #
plot(iso$CENTRO_LON,
     iso$CENTRO_LAT,
     pch = 16,
     cex = scales::rescale(as.numeric(iso$d13C_MEAN), to = c(1,5)))

plot(iso$CENTRO_LON,
     iso$CENTRO_LAT,
     pch = 16,
     cex = scales::rescale(as.numeric(iso$d15N_MEAN), to = c(1, 5)))

plot(iso$CENTRO_LON,
     iso$CENTRO_LAT,
     pch = 16,
     cex = scales::rescale(as.numeric(iso$HG_MEAN), to = c(1, 5)))

#### Extraction des points dans les kernels 50 ####
# chargement des points
pnb_trip <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_TsTrajets_filtre_Audrey_data.txt",
                       h = TRUE,
                       sep = "\t")
head(pnb_trip$DATE)

# traitement des points
pnb_trip$DATE <- as.POSIXct(pnb_trip$DATE,
                         format = "%Y-%m-%d %H:%M") # Date format
pnb_mig <- pnb_trip[pnb_trip$TYPE == "MIG", ]
dim(pnb_mig) # from 6481 rows to 3620
table(pnb_mig$TYPE)

pnb_mig <- pnb_mig[pnb_mig$ID != "PNB-C2143", ] # retrait car pas de dosages
table(pnb_mig$ID)
length(unique(pnb_mig$ID))

# Conversion in sf Spatial Object
projLatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# UTM 43S corresponding to the center of the Indian ocean
# UTM 43 => 32743
# projUTM <- '+init=epsg:32743'

pnb_sf <- st_as_sf(pnb_mig,
                   coords = c("LON", "LAT"),
                   crs = projLatLon)
pnb_sf_list <- split(pnb_sf,
                     pnb_sf$ID)

# chargement kernels
kernels <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/kernel50-90.rds")
str(kernels)

k50 <- lapply(kernels,
              function(x){
                   ker <- x[[1]]
                   ker
              })
length(k50)
mapview(k50)
class(k50)
names(k50)[1]
plot(k50[[1]])
plot(pnb_sf_list[[1]])

# intersection kernel & points
points_in_k <- lapply(pnb_sf_list,
                      function(x) {
                          id <- unique(x$ID)
                          k <- k50[[id]]
                          plot(st_geometry(x))
                          plot(st_geometry(k), add = T)
                          
                          sf_use_s2(FALSE)
                          ex <- st_intersects(st_geometry(k),
                                              st_geometry(x))
                          x[unlist(ex), ]
                      })
# verification ==> OK
names(pnb_sf_list) == names(points_in_k)
names(points_in_k) == names(k50)
for (i in 1:22) {
      x11()
      plot(st_geometry(pnb_sf_list[[i]]))
      plot(st_geometry(k50[[i]]),
           add = T)
      plot(st_geometry(points_in_k[[i]]),
           col = "red",
           add = T)
}
# saveRDS(points_in_k,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels.rds")

#### Extraction des dates min & max par indiv
range_in_k <- lapply(points_in_k,
                     function(x) {
                          data.frame(ID = unique(x$ID),
                                     MIN_DATE_K50 = min(x$DATE),
                                     MAX_DATE_K50 = max(x$DATE))
                     })
range_dates <- do.call("rbind",
                       range_in_k)

iso2 <- left_join(iso,
                  range_dates,
                  by = "ID")
#### Time range for SST download
min(iso2$MIN_DATE_K50)
max(iso2$MAX_DATE_K50)

# saveRDS(iso2,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates.rds")

# iso2 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates.rds")

#### Extraction des températures sous les kernels 50 & calcul moyennes et sd ####
sst_rasters <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/SST_2018-2019/",
                   full.names = TRUE)

sst <- rast(sst_rasters)
names(sst) <- time(sst)

x <- iso2[1, ]
d1 <- date(x$MIN_DATE_K50)
d2 <- date(x$MAX_DATE_K50)
sst_part <- sst[[date(time(sst)) >= d1 & date(time(sst)) <= d2]]
range(sst_part)

# --> conversion kelvin en degre celsius: K - 273.15 = °c
sst_part2 <- sst_part - 273.15
range(sst_part2)

# moyenne
sst_mean <- mean(sst_part2)
plot(sst_mean)
sst_ext <- terra::extract(sst_mean,
                          k50[[x$ID]])
sst_mean_pol <- mean(sst_ext$mean,
                     na.rm = T)
sst_sd_poly <- sd(sst_ext$mean,
                  na.rm = T)

sst_bilan <- lapply(split(iso2, iso2$ID),
                   function(x){
                       d1 <- date(x$MIN_DATE_K50)
                       d2 <- date(x$MAX_DATE_K50)
                       
                       sst_part <- sst[[date(time(sst)) >= d1 & date(time(sst)) <= d2]]
                       conv <- sst_part - 273.15
                       sst_mean <- mean(conv)
                       sst_ext <- terra::extract(sst_mean,
                          k50[[x$ID]])
                       
                       sst_mean_poly <- mean(sst_ext$mean,
                                            na.rm = T)
                       sst_sd_poly <- sd(sst_ext$mean,
                                         na.rm = T)
                       n_pix <- nrow(sst_ext)
                       
                       f <- data.frame(ID = x$ID,
                                       N_PIX = n_pix,
                                       SST_MEAN = sst_mean_poly,
                                       SST_SD = sst_sd_poly)
                   })
sst_bilan_df <- do.call("rbind",
                        sst_bilan)

iso3 <- left_join(iso2,
                  sst_bilan_df,
                  "ID")

# saveRDS(iso3,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates_SST_degre.rds")
# write.table(iso3,
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates_SST_degre.txt",
#             sep = "\t",
#             row.names = FALSE)
iso3 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates_SST_degre.rds")

plot(iso3$CENTRO_LON,
     iso3$CENTRO_LAT,
     cex = scales::rescale(iso3$SST_MEAN,
                   c(0.5, 5)))

#### CARTO K50 GLOBALE ####
# bathy <- terra::rast("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/ETOPO1_Bed_g_geotiff_BATHY.tif")
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

# Plot
k50_sp <- lapply(k50, as, "Spatial")
plot(bathy,
     col = c(blue.col(50),
             terrain.colors(50)),
     breaks = s2)
levelplot(bathy,
          col.regions = c(blue.col(50),
             terrain.colors(50)),
          at = s3,
          color.key = FALSE) +
layer(lapply(k50_sp,
             sp.polygons,
             col = "#8b2a0072",
             fill = "#8b2a0072")) +
layer(levelplot(bathy_test))

# see https://gis.stackexchange.com/questions/243954/rastervis-polygon-under-raster-layer-in-r


levelplot(bathy,
          margin = FALSE,
          col.regions = c(rep("transparent", 50),
             terrain.colors(50)),
          at = s3,
          color.key = FALSE,
          panel=function(...) {
            sp.polygons(test, fill = "grey40", col = NA)
            panel.levelplot(...)
          })

# plot(bathy,
#      col = c(rep("transparent", 50),
#              terrain.colors(50)),
#      breaks = s3)
bathy_test <- bathy
bathy_test[bathy_test < 0] <- NA

lat_min <- -50
lat_max <- 30

lon_min <- 20
lon_max <- 130
# png("C:/Users/ccjuhasz/Desktop/ISOSCAPES_K50_global2.png",
#     width = 1600,
#     height = 1000,
#     units = "px",
#     pointsize = 12,
#     bg = "white",
#     res = 100)

par(mai = c(1, 1, 0.8, 0.8))
# maps::map("world",
#           add = FALSE,
#              fill = T,
#              xlim = c(lon_min, lon_max),
#              ylim = c(lat_min, lat_max),
#              col = "grey")
# plot(lon_col,
#      lat_col,
#      pch = 16,
#      col = "white",
#      cex.lab = 2,
#      font.lab = 2,
#      xlab = "Longitude",
#      ylab = "Latitude",
#      cex.axis = 1.5,
#      xlim = c(lon_min, lon_max),
#      ylim = c(lat_min, lat_max),
#      xaxt = "n",
#      yaxt = "n")
plot(bathy,
     col = c(blue.col(50),
             terrain.colors(50)),
     breaks = s3,
     add = FALSE,
     cex.lab = 3,
     cex.axis = 3,
     xlim = c(lon_min, lon_max),
     ylim = c(lat_min, lat_max),
     axes = FALSE,
     legend = FALSE)
lapply(k50,
       plot,
       add = TRUE,
       lwd = 1.5,
       border = "#8b2a0072",
       col = "#8b2a0072",
     xlim = c(lon_min, lon_max),
     ylim = c(lat_min, lat_max))

plot(bathy,
     col = c(rep("transparent", 50),
             terrain.colors(50)),
     breaks = s3,
     add = TRUE,
     axes = FALSE,
     legend = FALSE)

points(lon_col,
       lat_col,
       pch = 23,
       bg = "yellow",
       cex = 3)
points(iso3$CENTRO_LON,
       iso3$CENTRO_LAT,
       pch = 20,
       col = "black",
       cex = 2)
# box(lwd = 2)

dev.off()

#### CARTO SST GLOBALE ####
# date min = date min d'entree dans k50 tout ind confondus
# date max = date max de sortie de k50 tout ind confondus

dmin <- min(date(iso3$MIN_DATE_K50))
dmax <- max(date(iso3$MAX_DATE_K50))

sst_part <- sst[[date(time(sst)) >= dmin & date(time(sst)) <= dmax]]
conv <- sst_part - 273.15
sst_mean <- mean(conv)

plot(sst_mean)
# saveRDS(sst_mean,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_SST_mean_global.rds")


#### Relation btw SST & all others indices ####
meta <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates_SST_degre.rds") 

head(meta)
summary(meta)
x11()
plot(x = meta$CENTRO_LAT,
     y = meta$SST_MEAN,
     xlim = c(min(meta$CENTRO_LAT), 25),
     ylim = c(min(meta$SST_MEAN - meta$SST_SD), max(meta$SST_MEAN + meta$SST_SD)),
     bty = "n",
     ylab = "SST (°c)",
     xlab = "Latitude (°)")

segments(x0 = meta$CENTRO_LAT,
         y0 = meta$SST_MEAN + meta$SST_SD,
         x1 = meta$CENTRO_LAT,
         y1 = meta$SST_MEAN - meta$SST_SD)

mini_meta <- meta[, c("CENTRO_LAT", "CENTRO_LON", "HG_MEAN", "d13C_MEAN", "d15N_MEAN")]

X11()
par(mfrow = c(2, 3))
graf <- list()
apply(mini_meta,
      2,
      function(x) {
           plot(x = x,
                y = meta$SST_MEAN,
                xlim = c(min(x), max(x)),
                ylim = c(min(meta$SST_MEAN - meta$SST_SD), max(meta$SST_MEAN + meta$SST_SD)),
                bty = "n",
                ylab = "SST (°c)",
                xlab = names(x),
                cex = scales::rescale(as.numeric(meta$N_PIX), to = c(1, 5)))
           segments(x0 = x,
                    y0 = meta$SST_MEAN + meta$SST_SD,
                    x1 = x,
                    y1 = meta$SST_MEAN - meta$SST_SD)
           
           mod <- lm(meta$SST_MEAN ~ x)
          #  print(head(x))
           pred_mod <- predict.lm(mod,
                                  se.fit = TRUE)
           pred_df <- data.frame(x = x,
                                 fit_sst = pred_mod$fit,
                                 se_fit_sst = pred_mod$se.fit)
           pred_df <- pred_df[order(pred_df$x), ]
           lines(pred_df$x,
                 pred_df$fit_sst,
                 col = "red")
           lines(pred_df$x,
                 pred_df$fit_sst - pred_df$se_fit_sst,
                 col = "red",
                 lty = 4)
           lines(pred_df$x,
                 pred_df$fit_sst + pred_df$se_fit_sst,
                 col = "red",
                 lty = 4)           
      })

#### EXTRA ####
 mig_list <- split(pnb_mig, year(pnb_mig$DATE))
 
 lapply(mig_list, summary)
 
