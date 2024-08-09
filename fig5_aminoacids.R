#===============================================================================
# code to make two panels of oxalic acid concentrations
#-------------------------------------------------------------------------------

# charger les dépendances ------------------------------------------------------
if (!existsFunction("read_excel")) library ("tidyverse")

# laad data --------------------------------------------------------------------
if (!exists("d")) source ("0_lire_données.R")

# loop over the different organic acids and make two plots for sap and syrup ---
for (aa in c("glycine", "aspargine", "4-hydroxyproline", "glutamine", 
             "threonine", "1-methyl-histidine","arginine", "citruline", 
             "prolyne-hydroxyprolyne", "serine", "glycine-proline", 
             "3-methyl-histidine", "alanine", "4-aminobutyric acide",
             "methionine", "sarcosine", "proline", "lysine", "aspartic acide", 
             "histidine", "thiaproline", "valine", "glutamic acide", 
             "g-hydroxylysine", "triptophane", "aminopimelic acide", "tyrosine", 
             "leucine", "a-aminoadipic acide", "phenylalanine", "isoleucine", 
             "tot_AA")){
  
  # select data --------
  d_aa <- d %>% select(s, e, m, aa)
  
  # set parameters --------
  if(aa == "glycine"){
    y_limits <- c(0, 50000)
    y_limits_insert <- c(0, 360)
    y_label <- "Glycine"
  } else if (aa == "aspargine") {
    y_limits <- c(0, 1400)
    y_limits_insert <- c(0, 60)
    y_label <- "Aspargine"
  } else if (aa == "4-hydroxyproline") {
    y_limits <- c(0, 4000)
    y_limits_insert <- c(0, 200)
    y_label <- "4-Hydroxyproline"
  } else if (aa == "glutamine") { 
    y_limits <- c(0, 9500)
    y_limits_insert <- c(0, 400)
    y_label <- ""
  } else if (aa == "threonine") {
    y_limits <- c(0, 6)
    y_label <- ""
  } else if (aa == "1-methyl-histidine") {
    y_limits <- c(0, 600)
    y_limits_insert <- c(0, 17)
    y_label <- ""
  } else if (aa == "prolyne-hydroxyprolyne") {
    y_limits <- c(0, 2000)
    y_limits_insert <- c(0, 150)
    y_label <- ""
  } else if (aa == "arginine") {
    y_limits <- c(0, 900)
    y_limits_insert <- c(0, 23)
    y_label <- ""
  } else if (aa == "citruline") {
    y_limits <- c(0, 380)
    y_limits_insert <- c(0, 11)
    y_label <- ""
  } else if (aa == "serine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "glycine-proline") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "3-methyl-histidine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "alanine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "4-aminobutyric acide") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "methionine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "sarcosine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "proline") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "lysine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- "", 
  } else if (aa == "aspartic acide") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "histidine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "thiaproline") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "valine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "glutamic acide") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "g-hydroxylysine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "triptophane") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "aminopimelic acide") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "tyrosine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "leucine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "a-aminoadipic acide") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "phenylalanine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "isoleucine") {
    y_limits <- c(0, 1200)
    y_limits_insert <- c(0, 10)
    y_label <- ""
  } else if (aa == "tot_AA") {
    y_limits <- c(0, 14000)
    y_limits_insert <- c(0, 460)
    y_label <- "Total amino"
  }
  
  # assemble y-axis label ------
  y_label <- ifelse(aa == "tot_AA", 
                    paste0(y_label, " acids [µg g-1, dry base]"),
                    paste0(y_label," [µg g-1, dry base]"))
  
  # plot organic acid for early- and late-season sap from sugar and red maple --
  #png(filename = paste0("../fig/fig5_sap_",aa,"_",png_width,"x",png_height,".png"), 
  #    width = png_width, height = png_height, pointsize = png_pointsize)
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
           y0 = mean(unlist(d_aa[d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
           y1 = mean(unlist(d_aa[d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T),
           col = "darkgray", lwd = 4, lty = 2)
  
  # Add overall red maple sap mean ----
  segments(x0 = 1.8, x1 = 2.6, 
           y0 = mean(unlist(d_aa[d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
           y1 = mean(unlist(d_aa[d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T),
           col = "slategray", lwd = 4, lty = 2)
  
  # Add period means for sugar maple sap ------
  points(x = 0.7,
         y = mean(unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = 1.0,
         y = mean(unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = 1.3,
         y = mean(unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add individual sugar maple data ------
  points(x = jitter(rep(0.7, 4)),
         y = unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.0, 4)),
         y = unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.3, 4)),
         y = unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add period means for red maple sap -----
  points(x = 1.9,
         y = mean(unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = 2.2,
         y = mean(unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[2] , col = "slategray", lwd = 2)
  points(x = 2.5,
         y = mean(unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[3] , col = "slategray", lwd = 2)
  
  # Add red maple sap data ------
  points(x = jitter(rep(1.9, 4)),
         y = unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = jitter(rep(2.2, 4)),
         y = unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[2], col =  "slategray", lwd = 2)
  points(x = jitter(rep(2.5, 4)),
         y = unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[3], col =  "slategray", lwd = 2)
  #dev.off() # close device
  
  # Create insert for oxalique acid ------
  #=============================================================================
  if (aa %in% c("glycine", "", "", "", "", 
                "", "", "", "tot_AA")){
    # plot oxalique for early- and late-season sap from sugar and red maple --------------
    #png(filename = paste0("../fig/fig5_insert_",aa,"_",png_width,"x",png_height,".png"), 
    #    width = png_width, height = png_height, pointsize = png_pointsize)
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
             y0 = mean(unlist(d_aa[d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
             y1 = mean(unlist(d_aa[d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T),
             col = "darkgray", lwd = 4, lty = 2)
    
    # Add overall red maple sap mean ----
    segments(x0 = 1.8, x1 = 2.6, 
             y0 = mean(unlist(d_aa[d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
             y1 = mean(unlist(d_aa[d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T),
             col = "slategray", lwd = 4, lty = 2)
    
    # Add period means for sugar maple sap ------
    points(x = 0.7,
           y = mean(unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
           pch = 21, cex = 3,
           bg = ACSA_col[1], col = "darkgray", lwd = 2)
    points(x = 1.0,
           y = mean(unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
           pch = 21, cex = 3,
           bg = ACSA_col[2], col = "darkgray", lwd = 2)
    points(x = 1.3,
           y = mean(unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), na.rm = T), 
           pch = 21, cex = 3,
           bg = ACSA_col[3], col = "darkgray", lwd = 2)
    
    # Add individual sugar maple data ------
    points(x = jitter(rep(0.7, 4)),
           y = unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), 
           pch = 21, cex = 1.2,
           bg = ACSA_col[1], col = "darkgray", lwd = 2)
    points(x = jitter(rep(1.0, 4)),
           y = unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), 
           pch = 21, cex = 1.2,
           bg = ACSA_col[2], col = "darkgray", lwd = 2)
    points(x = jitter(rep(1.3, 4)),
           y = unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERS" & d_aa$m == "EAU", 4]), 
           pch = 21, cex = 1.2,
           bg = ACSA_col[3], col = "darkgray", lwd = 2)
    
    # Add period means for red maple sap -----
    points(x = 1.9,
           y = mean(unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
           pch = 23, cex = 3,
           bg = ACRU_col[1], col = "slategray", lwd = 2)
    points(x = 2.2,
           y = mean(unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
           pch = 23, cex = 3,
           bg = ACRU_col[2], col = "slategray", lwd = 2)
    points(x = 2.5,
           y = mean(unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), na.rm = T), 
           pch = 23, cex = 3,
           bg = ACRU_col[3], col = "slategray", lwd = 2)
    
    # Add red maple sap data ------
    points(x = jitter(rep(1.9, 4)),
           y = unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), 
           pch = 23, cex = 1.2,
           bg = ACRU_col[1], col = "slategray", lwd = 2)
    points(x = jitter(rep(2.2, 4)),
           y = unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), 
           pch = 23, cex = 1.2,
           bg = ACRU_col[2], col =  "slategray", lwd = 2)
    points(x = jitter(rep(2.5, 4)),
           y = unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERR" & d_aa$m == "EAU", 4]), 
           pch = 23, cex = 1.2,
           bg = ACRU_col[3], col =  "slategray", lwd = 2)
    #dev.off() # close device
  }
  
  # Plot maple syrup oxalique for early-, mid- and late-season syrup form sugar and red maple
  #png(filename = paste0("../fig/fig5_syrup_",aa,"_",png_width,"x",png_height,".png"), 
  #    width = png_width, height = png_height, pointsize = png_pointsize)
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
           y0 = mean(unlist(d_aa[d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), na.rm = T), 
           y1 = mean(unlist(d_aa[d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), na.rm = T),
           col = "darkgray", lwd = 4, lty = 2)
  
  # Add overall red maple sap mean ----
  segments(x0 = 1.8, x1 = 2.6, 
           y0 = mean(unlist(d_aa[d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), na.rm = T), 
           y1 = mean(unlist(d_aa[d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), na.rm = T),
           col = "slategray", lwd = 4, lty = 2)
  
  # Add period means for sugar maple sap ------
  points(x = 0.7,
         y = mean(unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = 1.0,
         y = mean(unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = 1.3,
         y = mean(unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), na.rm = T), 
         pch = 21, cex = 3,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add individual sugar maple data points -------
  points(x = jitter(rep(0.7, 4)),
         y = unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[1], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.0, 4)),
         y = unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[2], col = "darkgray", lwd = 2)
  points(x = jitter(rep(1.3, 4)),
         y = unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERS" & d_aa$m == "SIROP", 4]), 
         pch = 21, cex = 1.2,
         bg = ACSA_col[3], col = "darkgray", lwd = 2)
  
  # Add period means for red maple sap -----
  points(x = 1.9,
         y = mean(unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = 2.2,
         y = mean(unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[2], col = "slategray", lwd = 2)
  points(x = 2.5,
         y = mean(unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), na.rm = T), 
         pch = 23, cex = 3,
         bg = ACRU_col[3], col = "slategray", lwd = 2)
  
  # Add red maple sap data ------
  points(x = jitter(rep(1.9, 4)),
         y = unlist(d_aa[d_aa$s == "D" & d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[1], col = "slategray", lwd = 2)
  points(x = jitter(rep(2.2, 4)),
         y = unlist(d_aa[d_aa$s == "M" & d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[2], col =  "slategray", lwd = 2)
  points(x = jitter(rep(2.5, 4)),
         y = unlist(d_aa[d_aa$s == "F" & d_aa$e == "ERR" & d_aa$m == "SIROP", 4]), 
         pch = 23, cex = 1.2,
         bg = ACRU_col[3], col =  "slategray", lwd = 2)
  #dev.off() # close device
} # Finish loop over amino acids 
#===============================================================================