#===============================================================================
# code pour faire des graphiques des données
#-------------------------------------------------------------------------------

# charger les dépendances ------------------------------------------------------
if (!existsFunction("read_excel")) library ("tidyverse")

# load data --------------------------------------------------------------------
if (!exists("d")) source ("0_lire_données.R")
                          
# plot total microbial contamination for early- and late-season sap from sugar 
# and red maple --------------
png(filename = paste0("../fig/fig2_sap_bacterial_counts_",png_width,"x",png_height,".png"), 
    width = png_width, height = png_height, pointsize = png_pointsize)
par(mar = c(4, 6, 1, 1))
plot(x = NULL, y = NULL, 
     axes = FALSE,
     xlim = c(0.5, 2.7), ylim = c(0, 15),
     xlab = "",
     ylab = "")
mtext(side = 2, line = 4, 
      text = paste0(expression("Aerobic bacterial counts [log (CFU/ml)]")))
axis(side = 1, at = c(0.7, 1.0, 1.3, 1.9, 2.2, 2.5), 
     labels = c("Early", "Mid", "Late", "Early", "Mid", "Late"))
axis(side = 2, las = 1)

# Add overall sugar maple sap mean ------
segments(x0 = 0.6, y0 = mean(log(d$CTAM)[d$e == "ERS" & d$m == "EAU"]), 
         x1 = 1.4, y1 = mean(log(d$CTAM)[d$e == "ERS" & d$m == "EAU"]),
         col = "darkgrey", lwd = 4, lty = 2)

# Add overall red maple sap mean ----
segments(x0 = 1.8, y0 = mean(log(d$CTAM)[d$e == "ERR" & d$m == "EAU"]), 
         x1 = 2.6, y1 = mean(log(d$CTAM)[d$e == "ERR" & d$m == "EAU"]),
         col = "slategrey", lwd = 4, lty = 2)

# Add period means for sugar maple sap ------
points(x = 0.7,
       y = mean(log(d$CTAM)[d$s == "D" & d$e == "ERS" & d$m == "EAU"]), 
       pch = 21, cex = 3,
       bg = ACSA_col[1] , col = "darkgrey", lwd = 2)
points(x = 1.0,
       y = mean(log(d$CTAM)[d$s == "M" & d$e == "ERS" & d$m == "EAU"]), 
       pch = 21, cex = 3,
       bg = ACSA_col[2] , col = "darkgrey", lwd = 2)
points(x = 1.3,
       y = mean(log(d$CTAM)[d$s == "F" & d$e == "ERS" & d$m == "EAU"]), 
       pch = 21, cex = 3,
       bg = ACSA_col[3] , col = "darkgrey", lwd = 2)

# Add individual sugar maple data points -----------
points(x = jitter(rep(0.7, 4)),
       y = log(d$CTAM)[ d$s == "D" & d$e == "ERS" & d$m == "EAU"], 
       pch = 21, cex = 1.2,
       bg = ACSA_col[1], col = "darkgrey", lwd = 2)
points(x = jitter(rep(1.0, 4)),
       y = log(d$CTAM)[ d$s == "M" & d$e == "ERS" & d$m == "EAU"], 
       pch = 21, cex = 1.2,
       bg = ACSA_col[2], col = "darkgrey", lwd = 2)
points(x = jitter(rep(1.3, 4)),
       y = log(d$CTAM)[ d$s == "F" & d$e == "ERS" & d$m == "EAU"], 
       pch = 21, cex = 1.2,
       bg = ACSA_col[3], col = "darkgrey", lwd = 2)

# Add period means for red maple sap -----
points(x = 1.9,
       y = mean(log(d$CTAM)[d$s == "D" & d$e == "ERR" & d$m == "EAU"]), 
       pch = 23, cex = 3,
       bg = ACRU_col[1] , col = "slategrey", lwd = 2)
points(x = 2.2,
       y = mean(log(d$CTAM)[d$s == "M" & d$e == "ERR" & d$m == "EAU"]), 
       pch = 23, cex = 3,
       bg = ACRU_col[2] , col = "slategrey", lwd = 2)
points(x = 2.5,
       y = mean(log(d$CTAM)[d$s == "F" & d$e == "ERR" & d$m == "EAU"]), 
       pch = 23, cex = 3,
       bg = ACRU_col[3] , col = "slategrey", lwd = 2)

# Add individual red maple sap data ------
points(x = jitter(rep(1.9, 4)),
       y = log(d$CTAM)[d$s == "D" & d$e == "ERR" & d$m == "EAU"], 
       pch = 23, cex = 1.2,
       bg = ACRU_col[1], col = "slategrey", lwd = 2)
points(x = jitter(rep(2.2, 4)),
       y = log(d$CTAM)[d$s == "M" & d$e == "ERR" & d$m == "EAU"], 
       pch = 23, cex = 1.2,
       bg = ACRU_col[2], col =  "slategrey", lwd = 2)
points(x = jitter(rep(2.5, 4)),
       y = log(d$CTAM)[d$s == "F" & d$e == "ERR" & d$m == "EAU"], 
       pch = 23, cex = 1.2,
       bg = ACRU_col[3], col =  "slategrey", lwd = 2)
dev.off() # close device

# Plot fungal contamination for early-, mid- and late-season syrup form sugar and red maple
png(filename = paste0("../fig/fig2_sap_fungal_counts_",png_width,"x",png_height,".png"), 
    width = png_width, height = png_height, pointsize = png_pointsize)
par(mar = c(4, 6, 1, 1))
plot(x = NULL, y = NULL, 
     axes = FALSE,
     xlim = c(0.5, 2.7), ylim = c(0, 15),
     xlab = "", ylab = "")
mtext(side = 2, line = 4, text = "Fungal counts [log (CFU/ml)]")
axis(side = 1, at = c(0.7, 1.0, 1.3, 1.9, 2.2, 2.5), 
     labels = c("Early", "Mid", "Late", "Early", "Mid", "Late"))
axis(side = 2, las = 1)

# Add overall sugar maple sap mean ------
segments(x0 = 0.6, y0 = mean(log(d$levures)[d$e == "ERS" & d$m == "EAU"]), 
         x1 = 1.4, y1 = mean(log(d$levures)[d$e == "ERS" & d$m == "EAU"]),
         col = "darkgrey", lwd = 4, lty = 2)

# Add overall red maple sap mean ----
segments(x0 = 1.8, y0 = mean(log(d$levures)[d$e == "ERR" & d$m == "EAU"]), 
         x1 = 2.6, y1 = mean(log(d$levures)[d$e == "ERR" & d$m == "EAU"]),
         col = "slategrey", lwd = 4, lty = 2)

# Add period means for sugar maple sap ------
points(x = 0.7,
       y = mean(log(d$levures)[d$s == "D" & d$e == "ERS" & d$m == "EAU"]), 
       pch = 21, cex = 3,
       bg = ACSA_col[1] , col = "darkgrey", lwd = 2)
points(x = 1.0,
       y = mean(log(d$levures)[d$s == "M" & d$e == "ERS" & d$m == "EAU"]), 
       pch = 21, cex = 3,
       bg = ACSA_col[2] , col = "darkgrey", lwd = 2)
points(x = 1.3,
       y = mean(log(d$levures)[d$s == "F" & d$e == "ERS" & d$m == "EAU"]), 
       pch = 21, cex = 3,
       bg = ACSA_col[3] , col = "darkgrey", lwd = 2)

# Add individual sugar maple data ------------
points(x = jitter(rep(0.7, 4)),
       y = log(d$levures)[ d$s == "D" & d$e == "ERS" & d$m == "EAU"], 
       pch = 21, cex = 1.2,
       bg = ACSA_col[1], col = "darkgrey", lwd = 2)
points(x = jitter(rep(1.0, 4)),
       y = log(d$levures)[ d$s == "M" & d$e == "ERS" & d$m == "EAU"], 
       pch = 21, cex = 1.2,
       bg = ACSA_col[2], col = "darkgrey", lwd = 2)
points(x = jitter(rep(1.3, 4)),
       y = log(d$levures)[ d$s == "F" & d$e == "ERS" & d$m == "EAU"], 
       pch = 21, cex = 1.2,
       bg = ACSA_col[3], col = "darkgrey", lwd = 2)

# Add period means for red maple sap -----
points(x = 1.9,
       y = mean(log(d$levures)[d$s == "D" & d$e == "ERR" & d$m == "EAU"]), 
       pch = 23, cex = 3,
       bg = ACRU_col[1] , col = "slategrey", lwd = 2)
points(x = 2.2,
       y = mean(log(d$levures)[d$s == "M" & d$e == "ERR" & d$m == "EAU"]), 
       pch = 23, cex = 3,
       bg = ACRU_col[2] , col = "slategrey", lwd = 2)
points(x = 2.5,
       y = mean(log(d$levures)[d$s == "F" & d$e == "ERR" & d$m == "EAU"]), 
       pch = 23, cex = 3,
       bg = ACRU_col[3] , col = "slategrey", lwd = 2)

# Add red maple sap data ------
points(x = jitter(rep(1.9, 4)),
       y = log(d$levures)[d$s == "D" & d$e == "ERR" & d$m == "EAU"], 
       pch = 23, cex = 1.2,
       bg = ACRU_col[1], col = "slategrey", lwd = 2)
points(x = jitter(rep(2.2, 4)),
       y = log(d$levures)[d$s == "M" & d$e == "ERR" & d$m == "EAU"], 
       pch = 23, cex = 1.2,
       bg = ACRU_col[2], col =  "slategrey", lwd = 2)
points(x = jitter(rep(2.5, 4)),
       y = log(d$levures)[d$s == "F" & d$e == "ERR" & d$m == "EAU"], 
       pch = 23, cex = 1.2,
       bg = ACRU_col[3], col =  "slategrey", lwd = 2)
dev.off() # close device
#===============================================================================