#===============================================================================
# code pour lire les données de l'analyse du laboratoire et de l'analyse 
# organoleptique
#-------------------------------------------------------------------------------

# charger les dépendances ------------------------------------------------------
if (!existsFunction("read_excel")) library ("tidyverse")

# lire les données de l'analyse physiocochemique (p.ex., pH, conductivité et 
# °Brix)------------------------------------------------------------------------
nom_dossier <- "../Projet 4080189 - Érable Rouge II/Résultats d'analyses/Rapport_analyses_PC_2017-4080189.xlsx"
données_tmp <- read_excel(path = nom_dossier, range = "A14:E62", 
                       col_names = c("code", "brix", "conductivité", "pH", 
                                     "transmittance"))
données_pc <- données_tmp %>% 
  separate(code, into = c("p1", "a", "p", "s", "m", "e", "p7"), sep = "-") %>% 
  mutate (p = substr(p, 2, 3)) %>% 
  select(-p1, -p7)



#===============================================================================