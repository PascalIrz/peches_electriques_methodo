load(file = "processed_data/donnees.RData")
#source(file = "scripts/02b_fonctions_graphiques.R")
#source(file = "scripts/03b_fonctions_non_graphiques.R")

library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(pechelec)
# devtools::install_github('Mikata-Project/ggthemr')
#library(ggthemr)

# ---------------------------------------------------------------------------
# PRODUCTION DES GRAPHIQUES
# ---------------------------------------------------------------------------


# Définition d'une palette de couleurs
couleurs <- brewer.pal(n = 8, name = "Dark2")

# Sur l'ensemble du jeu de données
## Corrélations

cor_tot <- data_simp %>%
  select(cond,
         temp,
         volt,
         puiss,
         intens,
         pen15c,
         dc)

# Récupérer l'ordre des variables de cette matrice pour le garder constant pour la suite
ordre_variables <- data_simp %>%
  select(cond,
         temp,
         volt,
         puiss,
         intens,
         pen15c,
         dc) %>%
  as.matrix() %>%
  Hmisc::rcorr(type = c("spearman")) %>%
  .[[1]] %>%
  corrMatOrder(order = "hclust")

cor_1a <- data_simp %>%
  filter(anodes == 1) %>%
  mef_ordonner_vars(ordre = ordre_variables)

cor_2a <- data_simp %>%
  filter(anodes == 2) %>%
  mef_ordonner_vars(ordre = ordre_variables)

cor_cran2 <- data_simp %>%
  filter(cran == 2) %>%
  mef_ordonner_vars(ordre = ordre_variables)

cor_cran3 <- data_simp %>%
  filter(cran == 3) %>%
  mef_ordonner_vars(ordre = ordre_variables)

cor_cran4 <- data_simp %>%
  filter(cran == 4) %>%
  mef_ordonner_vars(ordre = ordre_variables)

cor_epa_bateau <- data_simp %>%
  filter(proto == "EPA bateau") %>%
  mef_ordonner_vars(ordre = ordre_variables)

cor_epa_pied <- data_simp %>%
  filter(proto == "EPA pied") %>%
  mef_ordonner_vars(ordre = ordre_variables)

cor_inv <- data_simp %>%
  filter(proto == "Inv") %>%
  mef_ordonner_vars(ordre = ordre_variables)

# Intensité - conductivité

prov <- data %>%
  filter(!is.na(conductivite),
         !is.na(intensite),
         !is.na(cran)) %>% 
  mutate(intensite = round(intensite, 2))

graph_intens_cond <- gg_bivar_cran(df = prov,
              x = "conductivite",
              y = "intensite",
              y_lab = "Intensité (A)")

graph_penny_cond <- gg_bivar_cran(df = prov,
              x = "conductivite",
              y = "penny_15_centre",
              y_lab = "Mesure Penny à 1,5m")

graph_dc_cond <- gg_bivar_cran(df = prov,
                                  x = "conductivite",
                                  y = "dc",
                                  y_lab = "Densité de courant")

# -----------------------------------------------------------------------
# Relation conductivité - Penny

data_sel <- data %>% 
  select(conductivite, penny_15_centre, station, cran) %>% 
  drop_na()

# -----------------------------------------------------------------------
# La mesure Penny à 1,50m est-elle corrélée à d'autre variables ?

tab_bivar_penny <-
  map(
    .x = c(
      "dist_an_cat",
      "larg_moy",
      "prof_moy",
      "larg_mes",
      "prof_mes",
      "temperature",
      "voltage",
      "puissance",
      "intensite",
      "conductivite",
      "dc"
    ),
    .f = model_bivar_penny,
    df = data
  ) %>%
  reduce(rbind) %>% 
  arrange(pval)

# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# tableau croisé du cran 2018 vs cran 2019
# xtab_cran <- data_2ans %>% select(station, annee, cran, dept) %>% 
#   pivot_wider(id_cols = c(station, dept), names_from = annee, names_prefix = "annee", values_from = cran) %>% 
#   select(-station) %>% 
#   rpivotTable::rpivotTable(rows="annee2018", cols=c("annee2019"),width="100%", height="400px")




  # data %>% 
  # mutate(cran = as.integer(cran)) %>% 
  # ggplot(aes(x = annee, y = station)) +
  # geom_point(aes(size = cran,
  #                col = conductivite)) +
  # labs(x = "",
  #      y = "",
  #      col = "Conductivité") +
  # scale_radius(range  = c(2, 6),
  #              name   = "Cran",
  #              breaks = c(1, 2, 3),
  #              labels = 1:3) +
  # scale_color_gradient(low = "blue", high = "red") +
  # guides(size = guide_legend(override.aes = list(colour = "red")))







# -----------------------------------------------------------------------
# Sauvegarde pour produire le doc html

rm(ordre_variables)

save.image(file = "processed_data/graphiques.RData")
























