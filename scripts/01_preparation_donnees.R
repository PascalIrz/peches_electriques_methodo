rm(list=ls())

# Chargement des librairies

library(tidyverse)

# lecture des données
data_file <- "raw_data/bilan_penny_2021.xlsx"

stations <- readxl::read_xlsx(path = data_file,
                              sheet = "stations")

data <- readxl::read_xlsx(path = data_file,
                          sheet = "mesures annuelles") %>% 
  mutate(intensite = ifelse(is.na(intensite) & !is.na(voltage),
                            yes = 1000 * puissance / voltage,
                            no = intensite)) %>% 
  left_join(y = stations)

data <- data %>% 
  select(station, dept:prof_mes, annee, everything()) %>% 
  mutate(cran = cut(voltage, breaks= c(0, 209, 241, 299, 331, 449, 471, 1000),
                    labels = c("NA", "2: 210-240V", "NA", "3: 300-330V", "NA", "4: 350-470V","NA")),
         cran = ifelse(cran == 'NA', NA, cran),
         cran = as.factor(cran),
         dc = penny_15_centre * intensite
      #  conductivite_insitu = conductivite / (1.023 ^ (25 - temp)), # Calcul de la cond in situ (celle affichée par le conductimètre est convertie en équivalente à 25°C)
      )


rm(data_file)

# Prétraitements

tableau_peches <- data %>%
  mutate(intensite = round(intensite, digits = 2)) %>%
  select(
    station,
    annee,
    cran,
    conductivite:penny_1
  )

# besoin de renommer pour avoir des intitulés courts qui rentrent dans la matrice + suppression lignes incomplètes
data_simp <- data %>%
  select(
    proto = protocole,
    anodes,
    dist = dist_an_cat,
    lar_mo = larg_moy,
    pro_mo = prof_moy,
    lar_me = prof_mes,
    pro_me = prof_mes,
    annee,
    cond = conductivite,
    temp = temperature,
    volt = voltage,
    puiss = puissance,
    intens = intensite,
    pen1c = penny_1,
    pen15c = penny_15_centre,
    pen15b = penny_15_bord,
    dc,
    cran
  )

save(tableau_peches, stations, data, data_simp,
     file = "processed_data/donnees.RData")
