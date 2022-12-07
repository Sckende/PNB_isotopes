# obj - production des figures pour le ms
rm(list = ls())
# package
library(sf)
library(mapview)
library(maps)
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

iso <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/PNB_ISOTOPES_locs_in_kernels_dates_SST_degre.txt",
                      sep = "\t",
                      dec = ".",
                      h = TRUE)
models_lat <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_lm_LAT_vs_HG_C_N.rds")
models_lon <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_lm_LON_vs_HG_C_N.rds")

glob_pred_lat <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_pred_LAT_vs_HG_C_N.rds")
glob_pred_lon <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/DATA/GitHub_Pages/ISOSCAPE_pred_LON_vs_HG_C_N.rds")


#### LATITUDE ####
# C
png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_C_vs_LAT.png",
    res = 300,
    width = 50,
    height = 50,
    pointsize = 24,
    unit = "cm",
    bg = "white")

plot(iso$CENTRO_LAT,
     iso$d13C_MEAN,
     xlab = "Latitude (°)",
     ylab = "Mean d13C (per mil)",
     ylim = c(min(iso$d13C_MEAN - iso$d13C_SD, na.rm = T), max(iso$d13C_MEAN + iso$d13C_SD, na.rm = T)),
     cex.axis = 1.5,
     cex.lab = 1.5,
    #  bty = "n",
     pch = 19)
segments(x0 = iso$CENTRO_LAT,
         y0 = iso$d13C_MEAN + iso$d13C_SD,
         x1 = iso$CENTRO_LAT,
         y1 = iso$d13C_MEAN - iso$d13C_SD)

lines(glob_pred_lat$lat,
      glob_pred_lat$fit_13c,
      col = "red",
      lwd = 2)
lines(glob_pred_lat$lat,
      glob_pred_lat$fit_13c - glob_pred_lat$se_fit_13c,
      col = "red",
      lty = 4,
      lwd = 2)
lines(glob_pred_lat$lat,
      glob_pred_lat$fit_13c + glob_pred_lat$se_fit_13c,
      col = "red",
      lty = 4,
      lwd = 2)

summary(models_lat[[2]])
text(x = -29,
     y = -15.4,
     labels = "n = 22\nR2 = 0.1637; F(1, 20) = 3.916\nP = 0.06",
     adj = c(0, 0),
     cex = 1.5)

dev.off()

# N
png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_N_vs_LAT.png",
    res = 300,
    width = 50,
    height = 50,
    pointsize = 24,
    unit = "cm",
    bg = "white")
plot(iso$CENTRO_LAT,
     iso$d15N_MEAN,
     xlab = "Latitude (°)",
     ylab = "Mean d15N (per mil)",
    #  bty = "n",
     ylim = c(min(iso$d15N_MEAN - iso$d15N_SD, na.rm = T),
              max(iso$d15N_MEAN + iso$d15N_SD, na.rm = T)),
     cex.axis = 1.5,
     cex.lab = 1.5,
     pch = 19)
segments(x0 = iso$CENTRO_LAT,
         y0 = iso$d15N_MEAN + iso$d15N_SD,
         x1 = iso$CENTRO_LAT,
         y1 = iso$d15N_MEAN - iso$d15N_SD)

lines(glob_pred_lat$lat,
      glob_pred_lat$fit_15n,
      col = "red",
      lwd = 2)
lines(glob_pred_lat$lat,
      glob_pred_lat$fit_15n - glob_pred_lat$se_fit_15n,
      col = "red",
      lty = 4,
      lwd = 2)
lines(glob_pred_lat$lat,
      glob_pred_lat$fit_15n + glob_pred_lat$se_fit_15n,
      col = "red",
      lty = 4,
      lwd = 2)

summary(models_lat[[3]])
text(x = -29,
     y = 12.3,
     labels = "n = 22\nR2 = 0.0004907; F(1, 20) = 0.009818\nP = 0.9",
     adj = c(0, 0),
     cex = 1.5)

dev.off()

# Hg
png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_Hg_vs_LAT.png",
    res = 300,
    width = 50,
    height = 50,
    pointsize = 24,
    unit = "cm",
    bg = "white")

plot(iso$CENTRO_LAT,
     iso$HG_MEAN,
     xlab = "Latitude",
     ylab = "Mean Hg",
    #  bty = "n",
     ylim = c(min(iso$HG_MEAN - iso$HG_SD, na.rm = T),
              max(iso$HG_MEAN + iso$HG_SD, na.rm = T)),
     cex.axis = 1.5,
     cex.lab = 1.5,
     pch = 19)

segments(x0 = iso$CENTRO_LAT,
         y0 = iso$HG_MEAN + iso$HG_SD,
         x1 = iso$CENTRO_LAT,
         y1 = iso$HG_MEAN - iso$HG_SD)

lines(glob_pred_lat$lat,
      glob_pred_lat$fit_hg,
      col = "red",
      lwd = 2)
lines(glob_pred_lat$lat,
      glob_pred_lat$fit_hg - glob_pred_lat$se_fit_hg,
      col = "red",
      lty = 4,
      lwd = 2)
lines(glob_pred_lat$lat,
      glob_pred_lat$fit_hg + glob_pred_lat$se_fit_hg,
      col = "red",
      lty = 4,
      lwd = 2)

summary(models_lat[[1]])
text(x = -29,
     y = 4,
     labels = "n = 22\nR2 = 0.5278; F(1, 20) = 22.36\nP < 0.001",
     adj = c(0, 0),
     cex = 1.5)

dev.off()

# SST
png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_SST_vs_LAT.png",
    res = 300,
    width = 50,
    height = 50,
    pointsize = 24,
    unit = "cm",
    bg = "white")

plot(x = iso$CENTRO_LAT,
     y = iso$SST_MEAN,
     xlim = c(min(iso$CENTRO_LAT), max(iso$CENTRO_LAT)),
     ylim = c(min(iso$SST_MEAN - iso$SST_SD), max(iso$SST_MEAN + iso$SST_SD)),
    #  bty = "n",
     ylab = "SST (°C)",
     xlab = "Centroid latitude (°)",
     cex.axis = 1.5,
     cex.lab = 1.5,
     pch = 19)

segments(x0 = iso$CENTRO_LAT,
         y0 = iso$SST_MEAN + iso$SST_SD,
         x1 = iso$CENTRO_LAT,
         y1 = iso$SST_MEAN - iso$SST_SD)
           
mod <- lm(iso$SST_MEAN ~ iso$CENTRO_LAT)
pred_mod <- predict.lm(mod,
                       se.fit = TRUE)
pred_df <- data.frame(lat = iso$CENTRO_LAT,
                      fit_sst = pred_mod$fit,
                      se_fit_sst = pred_mod$se.fit)
pred_df <- pred_df[order(pred_df$lat), ]

####

lines(pred_df$lat,
      pred_df$fit_sst,
      col = "red",
      lwd = 2)
lines(pred_df$lat,
      pred_df$fit_sst - pred_df$se_fit_sst,
      col = "red",
      lty = 4,
      lwd = 2)
lines(pred_df$lat,
      pred_df$fit_sst + pred_df$se_fit_sst,
      col = "red",
      lty = 4,
      lwd = 2)
summary(mod)

text(x = -29,
     y = 29,
     labels = "n = 22\nR2 = 0.5793; F(1, 20) = 27.54\nP < 0.0001",
     adj = c(0, 0),
     cex = 1.5)
dev.off()

#### LONGITUDE #### REPRENDRE ICI ****
# C
plot(iso$CENTRO_LON,
     iso$d13C_MEAN,
     xlab = "Longitude",
     ylab = "Mean d13C",
     bty = "n")

lines(glob_pred_lon$lon,
      glob_pred_lon$fit_13c,
      col = "red")
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_13c - glob_pred_lon$se_fit_13c,
      col = "red",
      lty = 4)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_13c + glob_pred_lon$se_fit_13c,
      col = "red",
      lty = 4)

mod_gam <- mgcv::gam(iso$d13C_MEAN ~ s(iso$CENTRO_LON))
pred_mod_gam <- predict(mod_gam,
                        se.fit = TRUE)
pred_df <- data.frame(x = iso$CENTRO_LON,
                      fit_gam = pred_mod_gam$fit,
                      se_fit_gam = pred_mod_gam$se.fit)
pred_df <- pred_df[order(pred_df$x), ]

lines(pred_df$x,
      pred_df$fit_gam,
      col = "darkblue")
lines(pred_df$x,
      pred_df$fit_gam - pred_df$se_fit_gam,
      col = "darkblue",
      lty = 4)
lines(pred_df$x,
      pred_df$fit_gam + pred_df$se_fit_gam,
      col = "darkblue",
      lty = 4)

summary(models_lon[[2]])

# N
plot(iso$CENTRO_LON,
     iso$d15N_MEAN,
     xlab = "longitude",
     ylab = "Mean d15N",
     bty = "n")

lines(glob_pred_lon$lon,
      glob_pred_lon$fit_15n,
      col = "red")
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_15n - glob_pred_lon$se_fit_15n,
      col = "red",
      lty = 4)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_15n + glob_pred_lon$se_fit_15n,
      col = "red",
      lty = 4)

mod_gam <- mgcv::gam(iso$d15N_MEAN ~ s(iso$CENTRO_LON))
pred_mod_gam <- predict(mod_gam,
                        se.fit = TRUE)
pred_df <- data.frame(x = iso$CENTRO_LON,
                      fit_gam = pred_mod_gam$fit,
                      se_fit_gam = pred_mod_gam$se.fit)
pred_df <- pred_df[order(pred_df$x), ]

lines(pred_df$x,
      pred_df$fit_gam,
      col = "darkblue")
lines(pred_df$x,
      pred_df$fit_gam - pred_df$se_fit_gam,
      col = "darkblue",
      lty = 4)
lines(pred_df$x,
      pred_df$fit_gam + pred_df$se_fit_gam,
      col = "darkblue",
      lty = 4)

summary(models_lon[[3]])

# Hg
plot(iso$CENTRO_LON,
     iso$HG_MEAN,
     xlab = "Longitude",
     ylab = "Mean Hg",
     bty = "n")

lines(glob_pred_lon$lon,
      glob_pred_lon$fit_hg,
      col = "red")
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_hg - glob_pred_lon$se_fit_hg,
      col = "red",
      lty = 4)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_hg + glob_pred_lon$se_fit_hg,
      col = "red",
      lty = 4)

mod_gam <- mgcv::gam(iso$HG_MEAN ~ s(iso$CENTRO_LON))
pred_mod_gam <- predict(mod_gam,
                        se.fit = TRUE)
pred_df <- data.frame(x = iso$CENTRO_LON,
                      fit_gam = pred_mod_gam$fit,
                      se_fit_gam = pred_mod_gam$se.fit)
pred_df <- pred_df[order(pred_df$x), ]

lines(pred_df$x,
      pred_df$fit_gam,
      col = "darkblue")
lines(pred_df$x,
      pred_df$fit_gam - pred_df$se_fit_gam,
      col = "darkblue",
      lty = 4)
lines(pred_df$x,
      pred_df$fit_gam + pred_df$se_fit_gam,
      col = "darkblue",
      lty = 4)

summary(models_lon[[1]])

# SST
plot(x = iso$CENTRO_LON,
                y = iso$SST_MEAN,
                xlim = c(min(iso$CENTRO_LON), max(iso$CENTRO_LON)),
                ylim = c(min(iso$SST_MEAN - iso$SST_SD), max(iso$SST_MEAN + iso$SST_SD)),
                bty = "n",
                ylab = "SST (°C)",
                xlab = "Centroide longitude (°)",
                cex = scales::rescale(as.numeric(iso$N_PIX), to = c(1, 5)))
           segments(x0 = iso$CENTRO_LON,
                    y0 = iso$SST_MEAN + iso$SST_SD,
                    x1 = iso$CENTRO_LON,
                    y1 = iso$SST_MEAN - iso$SST_SD)
           
           mod <- lm(iso$SST_MEAN ~ iso$CENTRO_LON)
           
           mod_gam <- mgcv::gam(iso$SST_MEAN ~ s(iso$CENTRO_LON))


# Visualization
pred_mod <- predict.lm(mod,
                    se.fit = TRUE)

pred_mod_gam <- predict(mod_gam,
                        se.fit = TRUE)
pred_df <- data.frame(x = iso$CENTRO_LON,
                      fit_sst = pred_mod$fit,
                      se_fit_sst = pred_mod$se.fit,
                      fit_sst_gam = pred_mod_gam$fit,
                      se_fit_sst_gam = pred_mod_gam$se.fit)

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
           
           
          lines(pred_df$x,
                pred_df$fit_sst_gam,
                col = "darkblue")
          lines(pred_df$x,
                pred_df$fit_sst_gam - pred_df$se_fit_sst_gam,
                col = "darkblue",
                lty = 4)
          lines(pred_df$x,
                pred_df$fit_sst_gam + pred_df$se_fit_sst_gam,
                col = "darkblue",
                lty = 4)

summary(mod)