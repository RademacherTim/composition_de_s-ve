#===============================================================================
# code pour lire les données de l'analyse du laboratoire et de l'analyse 
# organoleptique
#-------------------------------------------------------------------------------

# Questions pour Luc :
# 1) Information pour les érablières (coordonnées, noms, nombre d'entailles, surface, structure de la forêt, etc.)
# 2) Il y a deux valeurs pour RA-F-SIROP-ERS. J'ai gardé celle qui dit CUISINIÈRE, 
# parce que celle pour l'érable rouge existe seulement avec ce qualificatif 
# 3) Qui a publié la théorie que l'érable rouge aurait plus de acides aminés (ligne XXX)?
# 4) Qui a proposé que les cytokinins sont des biomarqueur d'activité métablolique des racines (ligne XXX)?
# 5) What is consensus notation in organoleptic testing?
# 6) Partage une copie de Lagacé et al. (2015) avec moi
# 7) Link to industry inspection standards leads to ADI personnel page, a better reference is needed here. 

# charger les dépendances ------------------------------------------------------
if (!existsFunction("%>%")) library ("tidyverse")
if (!existsFunction("read_excel")) library ("readxl")

# initialise le fichier avec les données ---------------------------------------
dir <- "../Projet 4080189 - Érable Rouge II/Résultats d'analyses/"

# lire les données de l'analyse physio-chimique (p.ex., pH, conductivité et 
# °Brix)------------------------------------------------------------------------
nom_dossier <- paste0(dir, "Rapport_analyses_PC_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A14:E62", 
                       col_names = c("code", "brix", "conductivité", "pH", 
                                     "transmittance"))
données_pc <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) %>%
  slice(-45)  # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
              # n'est pas cuit de la même façon que l'ERR

# lire les données de l'analyse microbienne (UFC / ml) -------------------------
nom_dossier <- paste0(dir,"/Rapport_analyses_Micro_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A14:C37", 
                          col_names = c("code", "CTAM", "levures"))
données_micro <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) 

# lire les données de phytohormones (ABA et cytokinines) -----------------------
nom_dossier <- paste0(dir, "Résultats_analyse_phytohormones_P4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "B2:N50", 
                          col_names = c("code", "hum", "ABA", "PA", "DPA", 
                                        "OH_ABA", "phyto", "zeatin", 
                                        "trans-zeatin riboside", 
                                        "zeatin riboside", "2iP", "IPA", "cyto"),
                          sheet = "Original réorganisé")
données_phyto <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) %>% 
  slice(-47) # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
             # n'est pas cuit de la même façon que l'ERR

# lire les données de l'analyse des sucres (succharose, glucose, fructose) -----
nom_dossier <- paste0(dir, "Rapport_analyses_sucres_2017-P4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A15:D63", 
                          col_names = c("code", "succrose", "glucose", "fructose"),
                          na = "<LQ")
données_sucres <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) %>% 
  slice(-45) %>% # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
                 # n'est pas cuit de la même façon que l'ERR
  mutate(tot_suc = rowSums(across(where(is.numeric))))

# lire les données de l'analyse des polyphenols totaux -------------------------
nom_dossier <- paste0(dir, "Rapport_analyses_phenols_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A13:B61", 
                          col_names = c("code", "phenol"))
données_phenols <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) %>% 
  slice(-45) # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
             # n'est pas cuit de la même façon que l'ERR

# lire les données de l'analyse de 17 minéraux dans la sève --------------------
nom_dossier <- paste0(dir, "Rapport_analyses_minéraux_sève_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A14:R37", 
                          col_names = c("code", "P", "K", "Ca", "Mg", "Na", 
                                        "Al", "B", "Cu", "Fe", "Zn", "Mn", "Mo", 
                                        "Cd", "Cr", "Co", "Pb", "Ni"))
données_sap_minéraux <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) %>%
  mutate(tot_min = rowSums(across(where(is.numeric))))

# lire les données de l'analyse de 17 minéraux dans le sirop -------------------
nom_dossier <- paste0(dir, "Rapport_analyses_minéraux_sirop_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A14:R38", 
                          col_names = c("code", "P", "K", "Ca", "Mg", "Na", 
                                        "Al", "B", "Cu", "Fe", "Zn", "Mn", "Mo", 
                                        "Cd", "Cr", "Co", "Pb", "Ni"))
données_sirop_minéraux <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "e", "m", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) %>%
  slice(-21) %>% # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
                 # n'est pas cuit de la même façon que l'ERR
  mutate(tot_min = rowSums(across(where(is.numeric))))
  
# fusionner les deux jeux de données de minéraux -------------------------------
données_minéraux <- rbind(données_sap_minéraux, données_sirop_minéraux)

# lire les données de l'analyse de 11 acides organiques ------------------------
nom_dossier <- paste0(dir, "Rapport_analyses_AO_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A14:L62", 
                          col_names = c("code", "oxalique", "tartarique", 
                                        "quinique", "pyruvique", "malique", 
                                        "shikimique", "lactique", "acetique", 
                                        "citrique", "fumarique", "succinique"),
                          na = "<LQ")
données_AO <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7, -tartarique) %>% # There were no detectable levels of 
                                        # tartaric acid so I removed the column. 
  slice(-45) %>% # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
                 # n'est pas cuit de la même façon que l'ERR
  mutate(tot_OA = rowSums(across(where(is.numeric)), na.rm = T))

# lire les données de l'analyse de 38 acides aminés ----------------------------
nom_dossier <- paste0(dir, "Rapport_analyses_AA_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A8:AM56", 
                          col_names = c("code", "glycine", "aspargine", 
                                        "4-hydroxyproline", "glutamine", 
                                        "threonine", "1-methyl-histidine", 
                                        "arginine", "citruline", 
                                        "prolyne-hydroxyprolyne", "serine", 
                                        "glycine-proline", "3-methyl-histidine", 
                                        "alanine", "4-aminobutyric acide", 
                                        "ornithinine", "methionine", 
                                        "sarcosine", "proline", "lysine", 
                                        "aspartic acide", "histidine", 
                                        "thiaproline", "valine", 
                                        "glutamic acide", "g-hydroxylysine", 
                                        "b-aminoisobutyric acide", 
                                        "a-aminobutyric acide", "triptophane", 
                                        "aminopimelic acide", "cysthationine", 
                                        "cystine", "homocystine", "tyrosine", 
                                        "leucine", "a-aminoadipic acide", 
                                        "phenylalanine", "cysteine", "isoleucine"),
                          na = "<LQ")
données_AA <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7, -ornithinine, -`b-aminoisobutyric acide`, 
         -`a-aminobutyric acide`, -cysthationine, -cystine, -homocystine, -cysteine) %>% 
  # There were no detectable levels of the following so I removed them:
  #  - ornithinine
  #  - b-aminoisobutyric acide
  #  - a-aminobutyric acide
  #  - cysthationine
  #  - cystine
  #  - homocystine
  #  - cysteine
  slice(-45) %>% # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
                 # n'est pas cuit de la même façon que l'ERR
  mutate(tot_AA = rowSums(across(where(is.numeric)), na.rm = TRUE))

# lire les données de l'analyse de la cpapcité antioxidante (trolox équivalent) 
nom_dossier <- paste0(dir, "Rapport_analyses_ORAC_2017-4080189.xlsx")
données_tmp <- read_excel(path = nom_dossier, range = "A11:B59", 
                          col_names = c("code", "ORAC"))
données_ORAC <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-a, -p1, -p7) %>% 
  slice(-45) # Il y a deux valeur pour RA-F-SIROP-ERS et j'enlève celle qui 
             # n'est pas cuit de la même façon que l'ERR

# fusionner toutes les données dans une seule structure ------------------------
d <- left_join(données_pc, données_micro, by = join_by(p, s, m, e)) %>%
  left_join(données_phyto, by = join_by(p, s, m, e)) %>%
  left_join(données_sucres, by = join_by(p, s, m, e)) %>%
  left_join(données_phenols, by = join_by(p, s, m, e)) %>%
  left_join(données_minéraux, by = join_by(p, s, m, e))  %>%
  left_join(données_AO, by = join_by(p, s, m, e)) %>%
  left_join(données_AA, by = join_by(p, s, m, e)) %>%
  left_join(données_ORAC, by = join_by(p, s, m, e))

# convertir en facteur ---------------------------------------------------------
d <- d %>% mutate(p = factor(p), 
                  s = factor(s, levels = c("D","M","F")),
                  m = factor(m, levels = c("EAU", "SIROP")),
                  e = factor(e, levels = c("ERS", "ERR")))

# initiate colours for early-, mid- and late-seaon sugar and red maple ---------
ACRU_col <- c("#E08989dd", "#D16363dd", "#A41034dd")
ACSA_col <- c("#f5df46dd", "#feb34cdd", "#fd8d3cdd")

# declare width, height, and pointsize of image files --------------------------
png_width <- 660
png_height <- 493
png_pointsize <- 12

# nettoyer l'espace de travail -------------------------------------------------
rm(données_AA, données_AO, données_micro, données_minéraux, 
   données_sap_minéraux, données_sirop_minéraux, données_ORAC, données_pc, 
   données_phenols, données_phyto, données_sucres, données_tmp, dir, 
   nom_dossier)
#===============================================================================