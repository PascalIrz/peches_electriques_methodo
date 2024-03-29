rm(list=ls())

# Chargement des librairies

library(tidyverse)
library(corrplot)
# library(wesanderson)
library(RColorBrewer)

# lecture des données
data_file <- "raw_data/Bilan sonde penny maj_2020.xlsx"

stations <- readxl::read_xlsx(path = data_file, sheet = "Récaptitulatif ", range = "A1:I44") %>% 
  magrittr::set_colnames(c("station", "dept", "type_peche", "anodes", "dist_anode_cathode", "larg_moy", "prof_moy", "larg_mesure",
                           "prof_mesure"))

noms <- c("conductivite_25", "temp", "voltage", "puissance", "intensite", "penny_1m_c", "penny_1.5m_c", "penny_1.5m_b", "annee")

data_2020 <- readxl::read_xlsx(path = data_file, sheet = "Récaptitulatif ", range = "J1:P44") %>% 
  mutate(annee = 2020) %>% 
  rename("penny_1.5m_c" = "Penny à 1,5 m centre 2020",
         "penny_1.5m_b" = "Penny à 1,5 m bord 2020") %>% 
  mutate(penny_1m_c = NA) %>% 
  select(1:5, penny_1m_c, everything()) %>% 
  magrittr::set_names(noms) %>% 
  mutate(intensite = ifelse(is.na(intensite) & !is.na(voltage),
                            yes = 1000 * puissance / voltage,
                            no = intensite)) %>% 
  bind_cols(stations)


data_2019 <- readxl::read_xlsx(path = data_file, sheet = "Récaptitulatif ", range = "S1:Y44") %>% 
  mutate(annee = 2019) %>% 
  rename("penny_1.5m_c" = "Penny à 1,5m 2019",
         "penny_1m_c" = "Penny à 1 m 2019") %>% 
  mutate(penny_1.5m_b = NA) %>% 
  select(1:7, penny_1.5m_b, everything()) %>% 
  magrittr::set_names(noms) %>% 
  bind_cols(stations)
  
  
data_2018 <- readxl::read_xlsx(path = data_file, sheet = "Récaptitulatif ", range = "AB1:AH44") %>% 
  mutate(annee = 2018)%>% 
  rename("penny_1.5m_c" = "Penny à 1,5m 2018",
         "penny_1m_c" = "Penny à 1 m 2018") %>% 
  mutate(penny_1.5m_b = NA) %>% 
  select(1:7, penny_1.5m_b, everything()) %>% 
  magrittr::set_names(noms) %>% 
  bind_cols(stations)

data <- bind_rows(data_2018, data_2019, data_2020) %>% 
  select(station:prof_mesure, annee, everything()) %>% 
  mutate(cran = cut(voltage, breaks= c(0, 209, 241, 299, 331, 449, 471, 1000),
                    labels = c("NA", "2: 210-240V", "NA", "3: 300-330V", "NA", "4: 350-470V","NA")),
         cran = ifelse(cran == 'NA', NA, cran)) %>% 
  filter(!is.na(cran)) %>% 
  mutate(cran = as.factor(cran),
         conductivite = conductivite_25 / (1.023 ^ (25 - temp)), # Calcul de la cond in situ (celle affichée par le conductimètre est convertie en équivalente à 25°C)
         conductivite = round(conductivite))


rm(data_2018, data_2019, data_2020, stations, noms, data_file)

# Prétraitements

tableau_peches <- data %>% 
  mutate(intensite = round(intensite, digits = 2)) %>% 
  select(station, annee, cran, conductivite, conductivite_25, temp, voltage, puissance,
         intensite, penny_1m_c, penny_1.5m_c, penny_1.5m_b)
  
tableau_stations <- data %>% 
  select(station, dept, type_peche, anodes, dist_anode_cathode, larg_moy, larg_mesure, prof_moy, prof_mesure) %>% 
  unique()


# ---------------------------------------------------------------------------
# PRODUCTION DES GRAPHIQUES
# ---------------------------------------------------------------------------


# Définition d'une palette de couleurs
library(ggthemr)
ggthemr("earth", type = "outer", layout = "scientific", spacing = 2)
bg_col <- rgb(red = 67, green = 61, blue = 54, maxColorValue = 255)

cols <- ggthemr::swatch()[c(2,3,5)]

# Sur l'ensemble du jeu de données

## Distribution des variables

graph_histo <- function(x, data, xlab, type = c("pêches", "stations")) {
  
  if (type == "stations") {data <- data %>% select(station, x) %>% unique()}
  if (type == "stations") {ylab <- "Nb de stations"
       } else {
    ylab <- "Nb de pêches"}
  
  moy <- data %>% pull(x) %>% mean(na.rm = TRUE)
  range <- data %>% pull(x) %>% range(na.rm = TRUE)
  amp <- range[2] - range [1]
  label = paste0("moyenne : ", round(moy, digits = 2))

  ggplot(data = data, aes(x = get(x))) +
    geom_histogram(bins = 10) +
    labs(x = paste0(xlab, " - ", label), y = ylab) +
    geom_vline(xintercept = moy, linetype = "longdash", size = 0.7, col = "limegreen") +
    theme(text = element_text(size = 20)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1))
                            
}

# ----------------------------------------------------------------
quantile_penny <- function(data, penny, penny_lab) {
  data %>%
    pull(get(penny)) %>%
    quantile(c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95), na.rm = TRUE) %>% 
    round(2) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    magrittr::set_colnames(c("Pourcentile", penny_lab)) %>% 
    t() %>% 
    knitr::kable() %>%
    kableExtra::kable_styling()
  
                                                  }

# ---------------------------------------------------------------

graph_distri_var_qual <- function(data, var_qual, var_qual_label, type = c("stations", "pêches")) {
  
  if (type == "stations") {data <- data %>% select(station, var_qual) %>% unique()}
  if (type == "stations") {ylab <- "Nb de stations"
  } else {
    ylab <- "Nb de pêches"}
  
  data %>%
    pull(get(var_qual)) %>% 
    table() %>% 
    as.data.frame() %>%
    magrittr::set_colnames(c(var_qual, "n_obs")) %>% 
    ggplot(aes(x = as.factor(get(var_qual)), y = n_obs)) +
        geom_bar(stat = 'Identity', fill = "mediumseagreen") +
        labs(x = var_qual_label, y = ylab) +
        theme(text = element_text(size = 20))
  
}

## Corrélations
# besoin de renommer pour avoir des intitulés courts qui rentrent dans la matrice + suppression lignes incomplètes

data_simp <- data %>%
  select(meth = type_peche, anodes, dist = dist_anode_cathode, lar_mo = larg_moy, pro_mo = prof_moy,
         pro_mo = prof_moy, pro_me = prof_mesure, annee, cond25 = conductivite_25, cond = conductivite,
         temp, volt = voltage, puiss = puissance, intens = intensite, pen1c = penny_1m_c,
         pen1.5c = penny_1.5m_c, pen1.5b = penny_1.5m_b, cran)

# Fonction de production des graphique matrices de corrélation
# L'ordre des variables est soit conservé du dataframe ("original") soit fonction des proximités en CAH ("hclust")
graph_corrplot <- function(df, order = c("hclust", "original")) {
  
  M <- df %>% 
    as.matrix() %>% 
    Hmisc::rcorr(type = c("spearman"))
  
  corrplot.mixed(M$r, p.mat = M$P, sig.level = .05, upper = "ellipse", tl.cex = 0.8,
                           order = order)
}

cor_tot <- data_simp %>%
  select(cond, cond25, temp, volt, puiss, intens, pen1.5c) #%>%
#  graph_corrplot(order = "hclust")

# Récupérer l'ordre des variables de cette matrice pour le garder constant pour la suite
cor_tot_order <- data_simp %>%
  select(cond, cond25, temp, volt, puiss, intens, pen1.5c) %>%
  as.matrix() %>% 
  Hmisc::rcorr(type = c("spearman")) %>% 
  .[[1]] %>% 
  corrMatOrder(order = "hclust")

# par année. Pas de temp en 2018
# fonction de mise en forme

mef <- function(df) {
  df %>% 
    select(cond, cond25, temp, volt, puiss, intens, pen1.5c) %>% 
    select(cor_tot_order)
}

cor_2018 <- data_simp %>%
  filter(annee == 2018) %>%
  mef()

cor_2019 <- data_simp %>%
  filter(annee == 2019) %>%
  mef()

cor_2020 <- data_simp %>%
  filter(annee == 2020) %>% 
  mef()

cor_1a <- data_simp %>%
  filter(anodes == 1) %>%
  mef()

cor_2a <- data_simp %>%
  filter(anodes == 2) %>%
  mef()

cor_cran2 <- data_simp %>%
  filter(cran == 2) %>%
  mef()

cor_cran3 <- data_simp %>%
  filter(cran == 3) %>%
  mef()

cor_cran4 <- data_simp %>%
  filter(cran == 4) %>%
  mef()

cor_epa_bateau <- data_simp %>%
  filter(meth == "EPA bateau") %>%
  mef()

cor_epa_pied <- data_simp %>%
  filter(meth == "EPA pied") %>%
  mef()

cor_inv <- data_simp %>%
  filter(meth == "Inv") %>%
  mef()

# Intensité - conductivité

prov <- data %>%
  filter(!is.na(conductivite), !is.na(intensite), !is.na(cran)) #%>% 
#  filter(annee < 2020)

# Pour afficher les équations
model_simp <- function(df, cond, sel_cran) {
  
  mod <- lm(formula = intensite ~ get(cond),
                               data = df %>% filter(cran == sel_cran))
  
  coef <- mod %>% summary() %>% .$coefficients %>% 
    .[1:2] %>% t() %>% 
    as.data.frame() %>% 
    magrittr::set_names(c("b", "a")) %>% 
    mutate(cran = sel_cran)
  
  pval <- mod %>% anova() %>% .$"Pr(>F)"
  
  rsq <- mod %>% summary() %>% .$"r.squared"
  
  tab <- cbind(coef, pval, rsq) %>% 
    slice(1) %>% 
    mutate_at(vars(a, b), round, 3) %>% 
    mutate(rsq = round(rsq, 2),
           # sig = ifelse(pval>0.05, "NS",
           #              ifelse(pval > 0.01, "*",
           #                     ifelse(pval > 0.001, "**", "***"))),
           sig = case_when(pval > 0.05 ~ "NS",
                           pval <= 0.05 & pval > 0.01 ~ "*",
                           pval <= 0.01 & pval > 0.001 ~ "**",
                           TRUE ~ "***" ))
  
  signe <- ifelse(tab$b > 0, "+", "")
  
  label <- paste0("Cran ",
                  tab$cran,
                  " : y=", tab$a,
                  "x",
                  signe,
                  tab$b,
                  "(",
                  tab$sig,
                  ")  ",
                  "r\U00B2=",
                  round(rsq,2))
}

labels <- map(.x = (2:4),
              .f = model_simp,
              df = prov,
              cond = "conductivite")

prov <- prov %>% 
  mutate(intensite = round(intensite, 2)) %>% 
  rename(`Conductivité` = conductivite, `Intensité` = intensite, Cran = cran, Station = station)


graph_intens_cond <- ggplot(data = prov, aes(x = `Conductivité`, y = `Intensité`,
                                             col = Cran, label = Station)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Conductivité (\U00B5S/cm)", y = "Intensité (A)", col = "Cran") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme(plot.background = element_rect(fill = bg_col),
        panel.background = element_rect(fill = bg_col),
        text = element_text(size = 15)) +
  annotate(geom = "text", x = 200, y = seq(from = 11.7, to = 10.3, by = -0.7), label = labels,
           col = cols) +
  scale_color_manual(values = cols)

graph_intens_cond

graph_intens_cond <- plotly::ggplotly(graph_intens_cond)

# meme chose avec conductivité à 25°C
prov <- data %>%
  filter(!is.na(conductivite_25), !is.na(intensite), !is.na(cran))

labels <- map(.x = (2:4),
              .f = model_simp,
              df = prov,
              cond = "conductivite_25")

prov <- prov %>% 
  mutate(intensite = round(intensite, 2)) %>% 
  rename(`Conductivité à 25°C` = conductivite_25, `Intensité` = intensite, Cran = cran, Station = station)

graph_intens_cond_25 <- ggplot(data = prov, aes(x = `Conductivité à 25°C`, y = `Intensité`, col = Cran, label = Station)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Conductivité à 25°C(\U00B5S/cm)", y = "Intensité (A)", col = "Cran") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme(plot.background = element_rect(fill = bg_col),
        panel.background = element_rect(fill = bg_col),
        text = element_text(size = 15)) +
  annotate(geom = "text", x = 200, y = seq(from = 11.7, to = 10.3, by = -0.7), label = labels,
           col = cols) +
  scale_color_manual(values = cols)

graph_intens_cond_25

graph_intens_cond_25 <- plotly::ggplotly(graph_intens_cond_25)


# -----------------------------------------------------------------------
# Avec la variable qualitative cran

graph_var_qual_var_quant <- function(data, var_quant, var_quant_label, var_qual, var_qual_label) {
  
  data %>% 
    filter(!is.na(get(var_quant)) & !is.na(get(var_qual))) %>% 
    ggplot(aes(x = as.factor(get(var_qual)), y = get(var_quant))) +
    geom_boxplot(
      #fill = brewer.pal(name = "Dark2", n = 3)[2]
      ) +
    labs(x = var_qual_label, y = var_quant_label) +
    theme(text = element_text(size = 20))
  
}


# Lien entre les valeurs d'une année sur l'autre ?

## Liste des stations avec 2 ans de données
sta_2ans <- data %>%
  group_by(station) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  pull(station)

data_2ans <- data %>% 
  filter(station %in% sta_2ans)

# -----------------------------------------------------------------------
# Lien entre mesures penny à 1m et 1.5m

df <- data
annee_sel <- 2018

# Pour afficher les équations
model_simp <- function(df, annee_sel) {
  mod <- lm(formula = penny_1.5m ~ penny_1m,
            data = df %>% filter(annee == annee_sel))
  coef <- mod %>% summary() %>% .$coefficients %>% 
    .[1:2] %>% t() %>% 
    as.data.frame() %>% 
    magrittr::set_names(c("b", "a")) %>% 
    mutate(annee = annee_sel)
  
  pval <- mod %>% anova() %>% .$"Pr(>F)"
  
  rsq <- mod %>% summary() %>% .$"r.squared"
  
  tab <-cbind(coef, pval, rsq) %>% 
    slice(1) %>% 
    mutate_at(vars(a, b), round, 3) %>% 
    mutate(rsq = round(rsq, 2),
           sig = ifelse(pval>0.05, "NS",
                        ifelse(pval > 0.01, "*",
                               ifelse(pval > 0.001, "**", "***"))))
  signe <- ifelse(tab$b>0, "+", "")
  label <- paste0("Année ", tab$annee, " : y=", tab$a, "x", signe, tab$b, "(", tab$sig, ")  ",
                  "r\U00B2=", round(rsq,2))
}

labels <- map(.x = (2018:2019), .f = model_simp, df = data)

penny_1_1.5_scatterplot <- data %>% 
  mutate(annee = as.factor(annee)) %>% 
  ggplot(aes(x = penny_1m, y = penny_1.5m, label = station, color = annee)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.title=element_blank(),
        plot.background = element_rect(fill = bg_col),
        panel.background = element_rect(fill = bg_col),
        text = element_text(size = 15)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(x = "Mesure Penny à 1m", y = "Mesure Penny à 1,5m") +
  annotate(geom = "text", x = 2.5, y = c(4.2, 4.7), label = labels,
           col = cols[1:2]) +
  scale_color_manual(values = cols[1:2])


penny_1_1.5_scatterplot <-  plotly::ggplotly(penny_1_1.5_scatterplot)

# Mise à jour des popups
penny_1_1.5_scatterplot$x$data[[1]]$text <- penny_1_1.5_scatterplot$x$data[[1]]$text %>% 
  str_replace_all(pattern = "station:", replacement = "Station :") %>% 
  str_replace_all(pattern = "penny_1m:", replacement = "Penny à 1m :") %>% 
  str_replace_all(pattern = "penny_1.5m:", replacement = "Penny à 1,5m :") %>% 
  str_replace_all(pattern = "annee", replacement = "Année ")

penny_1_1.5_scatterplot$x$data[[2]]$text <- penny_1_1.5_scatterplot$x$data[[2]]$text %>% 
  str_replace_all(pattern = "station:", replacement = "Station :") %>% 
  str_replace_all(pattern = "penny_1m:", replacement = "Penny à 1m :") %>% 
  str_replace_all(pattern = "penny_1.5m:", replacement = "Penny à 1,5m :") %>% 
  str_replace_all(pattern = "annee", replacement = "Année ")


# -----------------------------------------------------------------------
# Relation conductivité - Penny

data_sel <- data %>% 
  select(conductivite, penny_1.5m, station, cran) %>% 
  drop_na()

# ggplot(data = data_sel, aes(x = conductivite, y = penny_1.5m, col = cran)) +
#   geom_point() +
#   geom_smooth(method = 'lm', se = F)
# 
# df <- data_sel
# cran_sel <- 2

# Pour afficher les équations
model_simp <- function(df, cran_sel) {
  
  mod <- lm(formula = penny_1.5m ~ conductivite, data = df %>% filter(cran == cran_sel))
  
  coef <- mod %>% summary() %>% .$coefficients %>% 
    .[1:2] %>% t() %>% 
    as.data.frame() %>% 
    magrittr::set_names(c("b", "a")) %>% 
    mutate(cran = cran_sel)
  
  pval <- mod %>% anova() %>% .$"Pr(>F)"
  
  rsq <- mod %>% summary() %>% .$"r.squared"
  
  tab <-cbind(coef, pval, rsq) %>% 
    slice(1) %>% 
    mutate_at(vars(a, b), round, 3) %>% 
    mutate(rsq = round(rsq, 2),
           sig = ifelse(pval > 0.05, "NS",
                        ifelse(pval > 0.01, "*",
                               ifelse(pval > 0.001, "**", "***"))))
  
  signe <- ifelse(tab$b > 0, "+", "")
  
  label <- paste0("Cran ", tab$cran, " : y=", tab$a, "x", signe, tab$b, "(", tab$sig, ")  ",
                  "r\U00B2=", round(rsq,2))
}

labels <- map(.x = (2:4), .f = model_simp, df = data_sel)

cond_penny_scatterplot <- data %>% 
  ggplot(aes(x = conductivite_25, y = penny_1.5m, label = station, color = cran)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  labs(x = "Conductivité à 25°C(\U00B5S/cm)", y = "Mesure Penny à 1,5m") +
  annotate(geom = "text", x = 410, y = c(5, 4.5, 4), label = labels,
           col = cols[1:3]) +
  scale_color_manual(values = cols[1:3]) +
  theme(legend.title = element_text("Cran"),
        plot.background = element_rect(fill = bg_col),
        panel.background = element_rect(fill = bg_col),
        text = element_text(size = 15))


cond_penny_scatterplot <-  plotly::ggplotly(cond_penny_scatterplot)

cond_penny_scatterplot$x$data[[1]]$text <- cond_penny_scatterplot$x$data[[1]]$text %>% 
  str_replace_all(pattern = "station:", replacement = "Station :") %>% 
  str_replace_all(pattern = "conductivite_25:", replacement = "Conductivité :") %>% 
  str_replace_all(pattern = "penny_1.5m:", replacement = "Penny à 1,5m :") %>% 
  str_replace_all(pattern = "cran", replacement = "Cran ")

cond_penny_scatterplot$x$data[[2]]$text <- cond_penny_scatterplot$x$data[[2]]$text %>% 
  str_replace_all(pattern = "station:", replacement = "Station :") %>% 
  str_replace_all(pattern = "conductivite_25:", replacement = "Conductivité :") %>% 
  str_replace_all(pattern = "penny_1.5m:", replacement = "Penny à 1,5m :") %>% 
  str_replace_all(pattern = "cran", replacement = "Cran ")

cond_penny_scatterplot$x$data[[3]]$text <- cond_penny_scatterplot$x$data[[3]]$text %>% 
  str_replace_all(pattern = "station:", replacement = "Station :") %>% 
  str_replace_all(pattern = "conductivite_25:", replacement = "Conductivité :") %>% 
  str_replace_all(pattern = "penny_1.5m:", replacement = "Penny à 1,5m :") %>% 
  str_replace_all(pattern = "cran", replacement = "Cran ")


# -----------------------------------------------------------------------
# La mesure Penny à 1,50m est-elle corrélée à d'autre variables ?

model_simp <- function(df, var) {
  mod <- lm(penny_1.5m ~ get(var), data = df)
  coef <- mod %>% summary() %>% .$coefficients %>% 
    .[1:2] %>% t() %>% 
    as.data.frame() %>% 
    magrittr::set_names(c("b", "a"))
  
  pval <- mod %>% anova() %>% .$"Pr(>F)"
  
  rsq <- mod %>% summary() %>% .$"r.squared"
  
  cbind(coef, pval, rsq) %>% 
    slice(1) %>% 
    mutate_at(vars(a, b), round, 3) %>% 
    mutate(rsq = round(rsq, 2),
           sig = ifelse(pval>0.05, "NS",
                        ifelse(pval > 0.01, "*",
                               ifelse(pval > 0.001, "**", "***"))),
           variable = var,
           pval = round(pval, 5)) %>% 
    select(variable, a, everything())
  
}

tab_bivar_penny <- map(.x = c("dist_anode_cathode", "larg_moy", "prof_moy", "larg_mesure", "prof_mesure",
                  "conductivite_25", "temp", "voltage", "puissance", "intensite", "conductivite"),
           .f = model_simp, df = data) %>% 
  reduce(rbind)

# -----------------------------------------------------------------------

comparer_annees_geom_point <- function(variable, df) {
  
  df %>% 
    select(station, dept, annee, variable) %>% 
    pivot_wider(id_cols = c(station, dept), names_from = annee, values_from = variable) %>% 
    ggplot(aes(x = `2018`, y = `2019`, col = as.factor(dept))) +
      geom_point() +
      labs(title = variable) +
      geom_abline(aes(slope = 1, intercept = 0), color = 'red', linetype = 'dotted', size = 1.2) +
 #     scale_color_brewer(palette = "Dark2") +
      scale_x_continuous(limits = c(0, NA)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(color = "Département") +
      theme(legend.position="bottom", text = element_text(size = 20)) 
  
}

# -----------------------------------------------------------------------

comparer_annees_geom_errorbar <- function(variable, df) {
  
  df %>%   
    group_by(annee) %>% 
    summarise(et = sd(get(variable), na.rm = T),
              moy = mean(get(variable), na.rm = T),
              n = n(),
              ymin = moy - et / n^0.5,
              ymax = moy + et / n^0.5) %>% 
    ggplot(aes(x = as.factor(annee), y = moy, fill = as.factor(annee))) +
    geom_bar(stat = "Identity") +
    geom_pointrange(aes(ymin = ymin, ymax = ymax), colour = "white") +
    labs(title = variable, x = "", y = "") +
    geom_hline(yintercept = data_2ans %>% pull(get(variable)) %>%  mean(na.rm =T),
               linetype = "longdash", size = 0.7, col = "limegreen") +
#    scale_fill_brewer(palette = "Dark2") +
    theme(legend.position = "none", text = element_text(size = 20)) +
    labs(title = paste0("Comparaison des moyennes", " (", variable, ")"))
  
#  ggpubr::ggarrange(g1, g2,ncol = 2, nrow = 1)
  
}

# -----------------------------------------------------------------------
# tableau croisé du cran 2018 vs cran 2019
xtab_cran <- data_2ans %>% select(station, annee, cran, dept) %>% 
  pivot_wider(id_cols = c(station, dept), names_from = annee, names_prefix = "annee", values_from = cran) %>% 
  select(-station) %>% 
  rpivotTable::rpivotTable(rows="annee2018", cols=c("annee2019"),width="100%", height="400px")

# -----------------------------------------------------------------------
# Modélisation du cran de réglage

# Sélection des variables candidates d'après les graphiques
data_sel <- data %>% select(cran, larg_moy, conductivite_25, temp) %>%
  drop_na() %>% 
  mutate(cran = as.numeric(as.character(cran)))

# Colinéarite ?
cor(data_sel) %>% round(2)

# Ca passe => modélisation

mod1 <- MASS::polr(as.factor(cran) ~ larg_moy + conductivite_25 + temp, method = "logistic", data = data_sel)
summary(mod1, digits = 3)

mod2 <- update(mod1, method = "probit", Hess = TRUE)
summary(mod2, digits = 3)

mod3 <- update(mod1, method = "loglog", Hess = TRUE)
summary(mod3, digits = 3)

mod4 <- update(mod1, method = "cloglog", Hess = TRUE)
summary(mod4, digits = 3)

### Pourcentage de bien classés


pourcent_bien_classe <- function(table) {
  sum(diag(table)) / sum(table) * 100 %>% round(2)
}

table(predict(mod1, data_sel, type = "class"), data_sel$cran) %>% pourcent_bien_classe

table(predict(mod2, data_sel, type = "class"), data_sel$cran) %>% pourcent_bien_classe

table(predict(mod3, data_sel, type = "class"), data_sel$cran) %>% pourcent_bien_classe

table(predict(mod4, data_sel, type = "class"), data_sel$cran) %>% pourcent_bien_classe


# confirme que mod4 est meilleur en prédiction.

### Possibilité de le simplifier ?


MASS::stepAIC(mod4)

mod5 <- MASS::polr(as.factor(cran) ~ conductivite_25 + temp, method = "cloglog", data = data_sel)

table(predict(mod5, data_sel, type = "class"), data_sel$cran)  %>% pourcent_bien_classe


MASS::stepAIC(mod5)
mod6 <- MASS::polr(as.factor(cran) ~ conductivite_25, method = "cloglog", data = data_sel)
table(predict(mod6, data_sel, type = "class"), data_sel$cran) %>% pourcent_bien_classe

mod_tab <- table(predict(mod5, data_sel, type = "class"), data_sel$cran) %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c('Cran prédit', 'Cran observé', 'Nombre de pêches')) %>% 
  
  rpivotTable::rpivotTable(rows = 'Cran observé',
                           cols = 'Cran prédit',
                           aggregatorName = 'Integer Sum',
                           vals = 'Nombre de pêches',
                           width = "100%", height = "400px")









# -----------------------------------------------------------------------
# Sauvegarde pour produire le doc html

rm(cor_tot_order, prov, labels)

save.image(file = "processed_data/penny_2020.RData")




















# ---------------------------------------------------------------------------
# TESTS
# ---------------------------------------------------------------------------


lm(intensite ~ penny_1m + penny_1.5m + conductivite_25, data = data) %>% 
  summary()

lm(voltage ~ penny_1m + penny_1.5m + conductivite_25, data = data) %>% 
  summary()

lm(puissance ~ penny_1m + penny_1.5m + conductivite_25, data = data) %>% 
  summary()




