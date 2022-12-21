# obj - production des figures pour le ms
rm(list = ls())
# package
library(sf)
library(mapview)
library(maps)
library(DHARMa)
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
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_C_vs_LAT.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")

# x11()
plot(iso$CENTRO_LAT,
     iso$d13C_MEAN,
     xlab = "Latitude of centroids (°)",
     ylab = "Mean d13C (per mil)",
     ylim = c(min(iso$d13C_MEAN - iso$d13C_SD, na.rm = T),
              max(iso$d13C_MEAN + iso$d13C_SD, na.rm = T)),
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

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(models_lat[[2]])
hist(resid(models_lat[[2]]))
plot(fitted(models_lat[[2]]), resid(models_lat[[2]]))

# DHARMa
testDispersion(models_lat[[2]])
sim <- simulateResiduals(models_lat[[2]])
plot(sim)

legend("topleft",
       legend = c("n = 22",
               expression(R^2 == 0.163),
               expression(F[20]^1 == 3.916),
               "P = 0.06"),
       bty = "n",
       cex = 1.5)

# dev.off()

# N
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_N_vs_LAT.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")
plot(iso$CENTRO_LAT,
     iso$d15N_MEAN,
     xlab = "Latitude of centroids (°)",
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

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(models_lat[[3]])
hist(resid(models_lat[[3]]))
plot(fitted(models_lat[[3]]), resid(models_lat[[3]]))

# DHARMa
testDispersion(models_lat[[3]])
sim <- simulateResiduals(models_lat[[3]])
plot(sim)

legend("topleft",
       legend = c("n = 22",
               expression(R^2 == 0.0004907),
               expression(F[20]^1 == 0.009818),
               "P = 0.9"),
       bty = "n",
       cex = 1.5)

# dev.off()

# Hg
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_Hg_vs_LAT.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")

plot(iso$CENTRO_LAT,
     iso$HG_MEAN,
     xlab = "Latitude of centroids (°)",
     ylab = "Mean feather Hg (µg.g-1)",
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

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(models_lat[[1]])
hist(resid(models_lat[[1]]))
plot(fitted(models_lat[[1]]), resid(models_lat[[1]]))

# DHARMa
testDispersion(models_lat[[1]])
sim <- simulateResiduals(models_lat[[1]])
plot(sim)


legend("bottomleft",
       legend = c("n = 22",
               expression(R^2 == 0.5278),
               expression(F[20]^1 == 22.36),
               "P < 0.001"),
       bty = "n",
       cex = 1.5)

# dev.off()

# SST
# png(
#       "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_SST_vs_LAT.png",
#       # "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_SST_vs_LAT_QUADRA.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")
x11()
col <- ifelse(month(iso$MIN_DATE_K50) < 4,
              "red",
              "black")
plot(x = iso$CENTRO_LAT,
     y = iso$SST_MEAN,
     xlim = c(min(iso$CENTRO_LAT), max(iso$CENTRO_LAT)),
     ylim = c(min(iso$SST_MEAN - iso$SST_SD), max(iso$SST_MEAN + iso$SST_SD)),
    #  bty = "n",
     ylab = "Mean SST per kernel (°C)",
     xlab = "Latitude of centroids (°)",
     cex.axis = 1.5,
     cex.lab = 1.5,
     pch = 19,
    col = col)

segments(x0 = iso$CENTRO_LAT,
         y0 = iso$SST_MEAN + iso$SST_SD,
         x1 = iso$CENTRO_LAT,
         y1 = iso$SST_MEAN - iso$SST_SD)
           
mod1 <- lm(iso$SST_MEAN ~ iso$CENTRO_LAT)
pred_mod <- predict.lm(mod1,
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

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(mod1)
hist(resid(mod1))
plot(fitted(mod1), resid(mod1))

# similar method for a quadratic effect
mod11 <- lm(SST_MEAN ~ poly(CENTRO_LAT, 2, raw = T), data = iso)
ND <- data.frame(CENTRO_LAT = -100:100)
prediction <- predict.lm(mod11,
                         se.fit = T,
                         newdata = ND)
preddd <- data.frame(lat = ND,
                     deg = prediction$fit)
preddd[preddd$deg == max(preddd$deg), ]

mod11b <- lm(iso$SST_MEAN ~ iso$CENTRO_LAT + I(iso$CENTRO_LAT^2))


mod111 <- mgcv::gam(iso$SST_MEAN ~ s(iso$CENTRO_LAT))
x11(); par(mfrow = c(2, 2))
plot(mod11)

x11(); par(mfrow = c(2, 2))
plot(mod111)


# DHARMa
testDispersion(mod)
sim <- simulateResiduals(mod1)
plot(sim)
sim <- simulateResiduals(mod11)
plot(sim)
sim <- simulateResiduals(mod111)
plot(sim)

legend("topleft",
       legend = c("n = 22",
               expression(R^2 == 0.5793),
               expression(F[20]^1 == 27.54),
               "P < 0.0001"),
       bty = "n",
       cex = 1.5)
summary(mod11)
legend("topleft",
       legend = c("n = 22",
               expression(R^2 == 0.786),
               expression(F[19]^2 == 34.9),
               "P < 0.0001"),
       bty = "n",
       cex = 1.5)
# dev.off()

#### LONGITUDE ####
# C
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_C_vs_LON.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")
# x11()
plot(iso$CENTRO_LON,
     iso$d13C_MEAN,
     xlab = "Longitude of centroids (°)",
     ylab = "Mean d13C (per mil)",
     ylim = c(min(iso$d13C_MEAN - iso$d13C_SD, na.rm = T), max(iso$d13C_MEAN + iso$d13C_SD, na.rm = T)),
     cex.axis = 1.5,
     cex.lab = 1.5,
    #  bty = "n",
     pch = 19)
segments(x0 = iso$CENTRO_LON,
         y0 = iso$d13C_MEAN + iso$d13C_SD,
         x1 = iso$CENTRO_LON,
         y1 = iso$d13C_MEAN - iso$d13C_SD)

lines(glob_pred_lon$lon,
      glob_pred_lon$fit_13c,
      col = "red",
      lwd = 2)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_13c - glob_pred_lon$se_fit_13c,
      col = "red",
      lty = 4,
      lwd = 2)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_13c + glob_pred_lon$se_fit_13c,
      col = "red",
      lty = 4,
      lwd = 2)

summary(models_lon[[2]])

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(models_lon[[2]])
hist(resid(models_lon[[2]]))
plot(fitted(models_lon[[2]]), resid(models_lon[[2]]))

# DHARMa
testDispersion(models_lon[[2]])
sim <- simulateResiduals(models_lon[[2]])
plot(sim)

legend("topright",
       legend = c("n = 22",
               expression(R^2 == 0.0003847),
               expression(F[20]^1 == 0.007696),
               "P = 0.931"),
       bty = "n",
       cex = 1.5)

# dev.off()

# N
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_N_vs_LON.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")

# x11()
plot(iso$CENTRO_LON,
     iso$d15N_MEAN,
     xlab = "Longitude of centroids (°)",
     ylab = "Mean d15N (per mil)",
    #  bty = "n",
     ylim = c(min(iso$d15N_MEAN - iso$d15N_SD, na.rm = T),
              max(iso$d15N_MEAN + iso$d15N_SD, na.rm = T)),
     cex.axis = 1.5,
     cex.lab = 1.5,
     pch = 19)
segments(x0 = iso$CENTRO_LON,
         y0 = iso$d15N_MEAN + iso$d15N_SD,
         x1 = iso$CENTRO_LON,
         y1 = iso$d15N_MEAN - iso$d15N_SD)

lines(glob_pred_lon$lon,
      glob_pred_lon$fit_15n,
      col = "red",
      lwd = 2)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_15n - glob_pred_lon$se_fit_15n,
      col = "red",
      lty = 4,
      lwd = 2)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_15n + glob_pred_lon$se_fit_15n,
      col = "red",
      lty = 4,
      lwd = 2)

summary(models_lon[[3]])

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(models_lon[[3]])
hist(resid(models_lon[[3]]))
plot(fitted(models_lon[[3]]), resid(models_lon[[3]]))

# DHARMa
testDispersion(models_lon[[3]])
sim <- simulateResiduals(models_lon[[3]])
plot(sim)

legend("topright",
       legend = c("n = 22",
               expression(R^2 == 0.1154),
               expression(F[20]^1 == 2.60),
               "P = 0.1219"),
       bty = "n",
       cex = 1.5)

# dev.off()

# Hg
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_Hg_vs_LON.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")

# x11()
plot(iso$CENTRO_LON,
     iso$HG_MEAN,
     xlab = "Longitude of centroids (°)",
     ylab = "Mean feather Hg (µg.g-1)",
    #  bty = "n",
     ylim = c(min(iso$HG_MEAN - iso$HG_SD, na.rm = T),
              max(iso$HG_MEAN + iso$HG_SD, na.rm = T)),
     cex.axis = 1.5,
     cex.lab = 1.5,
     pch = 19)

segments(x0 = iso$CENTRO_LON,
         y0 = iso$HG_MEAN + iso$HG_SD,
         x1 = iso$CENTRO_LON,
         y1 = iso$HG_MEAN - iso$HG_SD)

lines(glob_pred_lon$lon,
      glob_pred_lon$fit_hg,
      col = "red",
      lwd = 2)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_hg - glob_pred_lon$se_fit_hg,
      col = "red",
      lty = 4,
      lwd = 2)
lines(glob_pred_lon$lon,
      glob_pred_lon$fit_hg + glob_pred_lon$se_fit_hg,
      col = "red",
      lty = 4,
      lwd = 2)

summary(models_lon[[1]])

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(models_lon[[1]])
hist(resid(models_lon[[1]]))
plot(fitted(models_lon[[1]]), resid(models_lon[[1]]))

# DHARMa
testDispersion(models_lon[[1]])
sim <- simulateResiduals(models_lon[[1]])
plot(sim)

legend("bottomright",
       legend = c("n = 22",
               expression(R^2 == 0.001605),
               expression(F[20]^1 == 0.03215),
               "P = 0.8595"),
       bty = "n",
       cex = 1.5)

# dev.off()

# SST
# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_SST_vs_LON_QUADRA.png",
#     res = 300,
#     width = 50,
#     height = 50,
#     pointsize = 24,
#     unit = "cm",
#     bg = "white")

# x11()
plot(x = iso$CENTRO_LON,
     y = iso$SST_MEAN,
     xlim = c(min(iso$CENTRO_LON), max(iso$CENTRO_LON)),
     ylim = c(min(iso$SST_MEAN - iso$SST_SD), max(iso$SST_MEAN + iso$SST_SD)),
    #  bty = "n",
     ylab = "Mean SST per kernel (°C)",
     xlab = "Longitude of centroids (°)",
     cex.axis = 1.5,
     cex.lab = 1.5,
     pch = 19,
    col = col)

segments(x0 = iso$CENTRO_LON,
         y0 = iso$SST_MEAN + iso$SST_SD,
         x1 = iso$CENTRO_LON,
         y1 = iso$SST_MEAN - iso$SST_SD)
           
mod2 <- lm(iso$SST_MEAN ~ iso$CENTRO_LON)
pred_mod <- predict.lm(mod2,
                       se.fit = TRUE)
pred_df <- data.frame(lon = iso$CENTRO_LON,
                      fit_sst = pred_mod$fit,
                      se_fit_sst = pred_mod$se.fit)
pred_df <- pred_df[order(pred_df$lon), ]

####

lines(preddd$lon,
      preddd$fit_sst,
      col = "red",
      lwd = 2)
lines(preddd$lon,
      preddd$fit_sst - preddd$se_fit_sst,
      col = "red",
      lty = 4,
      lwd = 2)
lines(preddd$lon,
      preddd$fit_sst + preddd$se_fit_sst,
      col = "red",
      lty = 4,
      lwd = 2)
summary(mod2)

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(mod2)
hist(resid(mod2))
plot(fitted(mod2), resid(mod2))

# method for a quadratic effect
mod22 <- lm(SST_MEAN ~ poly(CENTRO_LON, 2, raw = T), data = iso)
summary(mod22)

# Visual verification 
x11(); par(mfrow = c(2, 2))
plot(mod22)

ND <- data.frame(CENTRO_LON = 38:120)
prediction <- predict.lm(mod22,
                         se.fit = T,
                         newdata = ND)
preddd <- data.frame(lon = ND$CENTRO_LON,
                     fit_sst = prediction$fit,
                     se_fit_sst = prediction$se.fit)
preddd[preddd$fit_sst == max(preddd$fit_sst), ]


mod22 <- lm(iso$SST_MEAN ~ poly(iso$CENTRO_LON, 2))
x11(); par(mfrow = c(2, 2))
plot(mod22)


# DHARMa
testDispersion(mod2)
sim <- simulateResiduals(mod2)
plot(sim)

legend("bottomright",
       legend = c("n = 22",
               expression(R^2 == 0.4039),
               expression(F[19]^2 == 6.438),
               "P = 0.007333"),
       bty = "n",
       cex = 1.5)
# dev.off()
# file.show("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/6-PNB_isotopes/Figures_ms/ISOSCAPE_plots/ISOSCAPE_SST_vs_LON.png")