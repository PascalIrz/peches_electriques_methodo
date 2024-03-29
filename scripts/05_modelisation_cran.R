load(file = "processed_data/donnees.RData")

library(tidyverse)

# -----------------------------------------------------------------------
# Modélisation du cran de réglage

# Sélection des variables candidates d'après les graphiques
data_sel <- data %>%
  select(cran, larg_moy, conductivite, temperature) %>%
  drop_na() %>% 
  mutate(cran = as.integer(as.character(cran)))

# Colinéarite ?
cor(data_sel) %>% round(2)

# Ca passe => modélisation

mod1 <- MASS::polr(as.factor(cran) ~ larg_moy + conductivite + temperature, method = "logistic", data = data_sel)
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

mod5 <- MASS::polr(as.factor(cran) ~ conductivite + temperature, method = "cloglog", data = data_sel)

table(predict(mod5, data_sel, type = "class"), data_sel$cran)  %>% pourcent_bien_classe


MASS::stepAIC(mod5)
mod6 <- MASS::polr(as.factor(cran) ~ conductivite, method = "cloglog", data = data_sel)
table(predict(mod6, data_sel, type = "class"), data_sel$cran) %>% pourcent_bien_classe

mod_tab <- table(predict(mod5, data_sel, type = "class"), data_sel$cran) %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c('Cran prédit', 'Cran observé', 'Nombre de pêches')) %>% 
  
  rpivotTable::rpivotTable(rows = 'Cran observé',
                           cols = 'Cran prédit',
                           aggregatorName = 'Integer Sum',
                           vals = 'Nombre de pêches',
                           width = "100%", height = "400px")

save.image(file = "processed_data/modeles.RData")

