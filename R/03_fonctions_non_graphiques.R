# fonction de mise en forme du df
mef <- function(df, cor_tot_order) {
  df %>% 
    select(cond, temp, volt, puiss, intens, pen15c) %>% 
    select(all_of(cor_tot_order))
}


# modèle 1
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


model_simp2 <- function(df, annee_sel) {
  mod <- lm(formula = penny_15_centre ~ penny_1,
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


model_simp3 <- function(df, cran_sel) {
  
  mod <- lm(formula = penny_15_centre ~ conductivite, data = df %>% filter(cran == cran_sel))
  
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


model_simp4 <- function(df, var) {
  mod <- lm(penny_15_centre ~ get(var), data = df)
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
