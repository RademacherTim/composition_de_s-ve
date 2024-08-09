#===============================================================================
# code to make two panels of oxalic acid concentrations
#-------------------------------------------------------------------------------

# charger les dépendances ------------------------------------------------------
if (!existsFunction("read_excel")) library ("tidyverse")

# load data --------------------------------------------------------------------
if (!exists("d")) source ("0_lire_données.R")
                          
# loop over the different organic acids and make two plots for sap and syrup ---
for (oa in c("oxalique", "quinique", "pyruvique", "malique", "shikimique", 
             "lactique","acetique", "citrique", "fumarique", "succinique", 
             "tot_OA")){
  
  # select data --------
  d_oa <- d %>% select(s, e, m, oa)
  
  # set parameters --------
  if(oa == "oxalique"){
    y_limits <- c(0, 36)
    y_limits_insert <- c(0, 3)
    y_label <- "Oxalic"
  } else if (oa == "quinique") {
    y_limits <- c(0, 450)
    y_label <- "Quinic"
  } else if (oa == "pyruvique") {
    y_limits <- c(0, 1400)
    y_limits_insert <- c(0, 5)
    y_label <- "Pyruvic"
  } else if (oa == "malique") {
    y_limits <- c(0, 9500)
    y_limits_insert <- c(0, 400)
    y_label <- "Malic"
  } else if (oa == "shikimique") {
    y_limits <- c(0, 6)
    y_label <- "Shikimic"
  } else if (oa == "lactique") {
    y_limits <- c(0, 600)
    y_limits_insert <- c(0, 17)
    y_label <- "Lactic"
  } else if (oa == "acetique") {
    y_limits <- c(0, 2000)
    y_limits_insert <- c(0, 150)
    y_label <- "Acetic"
  } else if (oa == "citrique") {
    y_limits <- c(0, 900)
    y_limits_insert <- c(0, 23)
    y_label <- "Citric"
  } else if (oa == "fumarique") {
    y_limits <- c(0, 380)
    y_limits_insert <- c(0, 11)
    y_label <- "Fumaric"
  } else if (oa == "succinique") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- "Succinic"
  } else if (oa == "tot_OA") {
    y_limits <- c(0, 14000)
    y_limits_insert <- c(0, 460)
    y_label <- "Total organic"
  }
  
  # assemble y-axis label ------
  y_label <- ifelse(oa == "tot_OA", 
                    paste0(y_label, " acids [µg g-1, dry base]"),
                    paste0(y_label, " acid [µg g-1, dry base]"))
  
  # plot organic acid for early- and late-season sap from sugar and red maple --
  png(filename = paste0("../fig/fig4_sap_",oa,"_",png_width,"x",png_height,".png"), 
      width = png_width, height = png_height, pointsize = png_pointsize)
  par(mar = c(4, 6, 1, 1))
  plot(x = NULL, y = NULL, axes = FALSE,
       xlim = c(0.5, 2.7), ylim = y_limits,
       xlab = "", ylab = "")
  mtext(side = 2, line = 4, text = y_label)
  axis(side = 1, at = c(0.7, 1.0, 1.3, 1.9, 2.2, 2.5), 
       labels = c("Early", "Mid", "Late", "Early", "Mid", "Late"))
  axis(side = 2, las = 1)
  
  # Add overall sugar maple sap mean ------
  segments(x0 = 0.6, x1 = 1.4, 
           y0 = mean(unlist(d_oa[d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
           y1 = mean(unlist(d_oa[d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T),
           col = "darkgray", lwd = 4, lty = 2)
  
  # Add overall red maple sap mean ----
  segments(x0 = 1.8, x1 = 2.6, 
           y0 = mean(unlist(d_oa[d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
           y1 = mean(unlist(d_oa[d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T),
           col = "slategray", lwd = 4, lty = 2)
  
  # Add period means for sugar maple sap ------
  points(x = 0.7,
         y = mean(unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = 1.0,
         y = mean(unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = 1.3,
         y = mean(unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add individual sugar maple data ------
  points(x = jitter(rep(0.7, 4)),
         y = unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.0, 4)),
         y = unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.3, 4)),
         y = unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add period means for red maple sap -----
  points(x = 1.9,
         y = mean(unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = 2.2,
         y = mean(unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[2] , col = "slategray", lwd = 2)
  points(x = 2.5,
         y = mean(unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[3] , col = "slategray", lwd = 2)
  
  # Add red maple sap data ------
  points(x = jitter(rep(1.9, 4)),
         y = unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = jitter(rep(2.2, 4)),
         y = unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[2], col =  "slategray", lwd = 2)
  points(x = jitter(rep(2.5, 4)),
         y = unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[3], col =  "slategray", lwd = 2)
  dev.off() # close device
  
  # Create insert for oxalique acid ------
  #=============================================================================
  if (oa %in% c("oxalique", "pyruvique", "malique", "lactique", "acetique", 
                "citrique", "fumarique", "succinique", "tot_OA")){
    # plot oxalique for early- and late-season sap from sugar and red maple --------------
    png(filename = paste0("../fig/fig4_insert_",oa,"_",png_width,"x",png_height,".png"), 
        width = png_width, height = png_height, pointsize = png_pointsize)
    par(mar = c(4, 6, 1, 1))
    plot(x = NULL, y = NULL, 
         axes = FALSE,
         xlim = c(0.5, 2.7), ylim = y_limits_insert,
         xlab = "", ylab = "")
    mtext(side = 2, line = 4, text = y_label)
    axis(side = 1, at = c(0.7, 1.0, 1.3, 1.9, 2.2, 2.5), 
         labels = c("Early", "Mid", "Late", "Early", "Mid", "Late"))
    axis(side = 2, las = 1)
    
    # Add overall sugar maple sap mean ------
    segments(x0 = 0.6,  x1 = 1.4, 
             y0 = mean(unlist(d_oa[d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
             y1 = mean(unlist(d_oa[d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T),
             col = "darkgray", lwd = 4, lty = 2)
    
    # Add overall red maple sap mean ----
    segments(x0 = 1.8, x1 = 2.6, 
             y0 = mean(unlist(d_oa[d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
             y1 = mean(unlist(d_oa[d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T),
             col = "slategray", lwd = 4, lty = 2)
    
    # Add period means for sugar maple sap ------
    points(x = 0.7,
           y = mean(unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
           pch = 21, cex = 3,
           bg = ACSA_col[1], col = "darkgray", lwd = 2)
    points(x = 1.0,
           y = mean(unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
           pch = 21, cex = 3,
           bg = ACSA_col[2], col = "darkgray", lwd = 2)
    points(x = 1.3,
           y = mean(unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), na.rm = T), 
           pch = 21, cex = 3,
           bg = ACSA_col[3], col = "darkgray", lwd = 2)
    
    # Add individual sugar maple data ------
    points(x = jitter(rep(0.7, 4)),
           y = unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), 
           pch = 21, cex = 1.2,
           bg = ACSA_col[1], col = "darkgray", lwd = 2)
    points(x = jitter(rep(1.0, 4)),
           y = unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), 
           pch = 21, cex = 1.2,
           bg = ACSA_col[2], col = "darkgray", lwd = 2)
    points(x = jitter(rep(1.3, 4)),
           y = unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERS" & d_oa$m == "EAU", 4]), 
           pch = 21, cex = 1.2,
           bg = ACSA_col[3], col = "darkgray", lwd = 2)
    
    # Add period means for red maple sap -----
    points(x = 1.9,
           y = mean(unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
           pch = 23, cex = 3,
           bg = ACRU_col[1], col = "slategray", lwd = 2)
    points(x = 2.2,
           y = mean(unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
           pch = 23, cex = 3,
           bg = ACRU_col[2], col = "slategray", lwd = 2)
    points(x = 2.5,
           y = mean(unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), na.rm = T), 
           pch = 23, cex = 3,
           bg = ACRU_col[3], col = "slategray", lwd = 2)
    
    # Add red maple sap data ------
    points(x = jitter(rep(1.9, 4)),
           y = unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), 
           pch = 23, cex = 1.2,
           bg = ACRU_col[1], col = "slategray", lwd = 2)
    points(x = jitter(rep(2.2, 4)),
           y = unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), 
           pch = 23, cex = 1.2,
           bg = ACRU_col[2], col =  "slategray", lwd = 2)
    points(x = jitter(rep(2.5, 4)),
           y = unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERR" & d_oa$m == "EAU", 4]), 
           pch = 23, cex = 1.2,
           bg = ACRU_col[3], col =  "slategray", lwd = 2)
  dev.off() # close device
  }
  
  # Plot maple syrup oxalique for early-, mid- and late-season syrup form sugar and red maple
  png(filename = paste0("../fig/fig4_syrup_",oa,"_",png_width,"x",png_height,".png"), 
      width = png_width, height = png_height, pointsize = png_pointsize)
  par(mar = c(4, 6, 1, 1))
  plot(x = NULL, y = NULL, 
       axes = FALSE,
       xlim = c(0.5, 2.7), ylim = y_limits,
       xlab = "", ylab = "")
  mtext(side = 2, line = 4, text = y_label)
  axis(side = 1, at = c(0.7, 1.0, 1.3, 1.9, 2.2, 2.5), 
       labels = c("Early", "Mid", "Late", "Early", "Mid", "Late"))
  axis(side = 2, las = 1)
  
  # Add overall sugar maple sap mean ------
  segments(x0 = 0.6, x1 = 1.4, 
           y0 = mean(unlist(d_oa[d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), na.rm = T), 
           y1 = mean(unlist(d_oa[d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), na.rm = T),
           col = "darkgray", lwd = 4, lty = 2)
  
  # Add overall red maple sap mean ----
  segments(x0 = 1.8, x1 = 2.6, 
           y0 = mean(unlist(d_oa[d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), na.rm = T), 
           y1 = mean(unlist(d_oa[d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), na.rm = T),
           col = "slategray", lwd = 4, lty = 2)
  
  # Add period means for sugar maple sap ------
  points(x = 0.7,
         y = mean(unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = 1.0,
         y = mean(unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = 1.3,
         y = mean(unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add individual sugar maple data points -------
  points(x = jitter(rep(0.7, 4)),
         y = unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.0, 4)),
         y = unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.3, 4)),
         y = unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERS" & d_oa$m == "SIROP", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add period means for red maple sap -----
  points(x = 1.9,
         y = mean(unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = 2.2,
         y = mean(unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[2], col = "slategray", lwd = 2)
  points(x = 2.5,
         y = mean(unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[3], col = "slategray", lwd = 2)
  
  # Add red maple sap data ------
  points(x = jitter(rep(1.9, 4)),
         y = unlist(d_oa[d_oa$s == "D" & d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = jitter(rep(2.2, 4)),
         y = unlist(d_oa[d_oa$s == "M" & d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[2], col =  "slategray", lwd = 2)
  points(x = jitter(rep(2.5, 4)),
         y = unlist(d_oa[d_oa$s == "F" & d_oa$e == "ERR" & d_oa$m == "SIROP", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[3], col =  "slategray", lwd = 2)
  dev.off() # close device
} # Finish loop over organic acids 
#===============================================================================
