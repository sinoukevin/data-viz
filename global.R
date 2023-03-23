# Package
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png) 
library(png)
library(gifski)
library(DT)




# Chargement des données
data = read_csv("data/bd_marche_final.csv",show_col_types = FALSE)
data = data %>%  mutate(Ch_Aff = str_replace_all(Ch_Aff," ","")) %>% 
                  mutate(Ch_Aff = as.numeric(Ch_Aff)) %>% mutate(ID_SOCIETE = as.factor(ID_SOCIETE))

My_data = data[,-1]

## Chiffres d'affaire 
df1 <- data %>% select(NOM_SOCIETE,ANNEE,Ch_Aff) %>% 
  group_by(ANNEE) %>%
  mutate(rank = rank(-Ch_Aff)) %>%
  group_by(NOM_SOCIETE) %>% 
  filter(rank <=10) %>%
  ungroup()
year1 = df1 %>% select(ANNEE) %>% arrange(ANNEE)
### Prestation de sinistre

df2 = data %>% filter((PRESTATION != 0 | PRESTATION == !(is.na(PRESTATION))),ANNEE<2018) %>%  select(NOM_SOCIETE,TYPE_SOC,ANNEE,PRESTATION) %>% 
  group_by(ANNEE) %>%
  mutate(rank = rank(-PRESTATION)) %>%
  group_by(NOM_SOCIETE) %>% 
  filter(rank <=10) %>%
  ungroup()

year2 = df2 %>% select(ANNEE) %>%  arrange(ANNEE)
# Taux d'évolution du chiffre d'affaire

df1_evolution <- data %>%
  group_by(NOM_SOCIETE,ANNEE) %>%
  summarise(ca = sum(Ch_Aff,na.rm = T)) %>% 
  arrange(ANNEE) %>%
  mutate(CA_precedent = lag(ca),
         taux = round((ca - CA_precedent) / CA_precedent * 100),2) %>%
  select(NOM_SOCIETE, ANNEE, ca,taux) %>% replace(is.na(.),0) %>% ungroup()


 
# Chiffres d'affaire globale

global_ca = function(df){
  value = sum(df$Ch_Aff)
  return (value)
}

percentage_non_vie = function(df){
  ca_global  = global_ca(df)
  ca_nonvie = df %>% filter(TYPE_SOC == "NON VIE") 
  ca_nonvie = sum(ca_nonvie$Ch_Aff)
  percentage = (ca_nonvie/ca_global)*100
  return(round(percentage,2))
}

percentage_vie = function(df){
  ca_global  = global_ca(df)
  ca_vie = df %>% filter(TYPE_SOC == "VIE")
  ca_vie = sum(ca_vie$Ch_Aff)
  percentage = (ca_vie/ca_global)*100
  return(round(percentage,2))
}
# Prestation de sinistres

global_pres = function(df){
  value = sum(df$PRESTATION)
  return (value)
}

pres_non_vie = function(df){
  pres_global  = global_pres(df)
  pres_nonvie = df %>% filter(TYPE_SOC == "NON VIE") 
  pres_nonvie = sum(pres_nonvie$PRESTATION, na.rm=T)
  percentage = (pres_nonvie/pres_global)*100
  return(round(percentage,2))
}

pres_vie = function(df){
  pres_global  = global_pres(df)
  pres_vie = df %>% filter(TYPE_SOC == "VIE")
  pres_vie = sum(pres_vie$PRESTATION,  na.rm =T)
  percentage = (pres_vie/pres_global)*100
  return(round(percentage,2))
}

plot_ca_branche = function(df, id_soc){
  d = data %>% filter(ID_SOCIETE == id_soc) %>% group_by(ANNEE, ID_BRANCHE) %>% 
    ungroup()
  pp <- ggplot(
    d,
    aes(ANNEE, Ch_Aff, group = ID_BRANCHE, color = factor(ID_BRANCHE))
  ) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = "Année", y = "Chiffre d'affaire") +
    theme(legend.position = "top")
  
  pp + 
    geom_point(aes(group = seq_along(ANNEE))) +
    transition_reveal(ANNEE)
}

plot_ca_branche = function(data, nom){
  d = data %>% filter(NOM_SOCIETE == nom)  %>% arrange(ANNEE)
  pp <- ggplot(
    d,
    aes(ANNEE, Ch_Aff, group = ID_BRANCHE, color = factor(ID_BRANCHE))
  ) +
    geom_line() +
    scale_color_viridis_d() +
    labs(x = "Année", y = "Chiffre d'affaire",color = "Branches" ) +
    theme(legend.position = "right",
          legend.title = "Branche",
          legend.text = "Branche") +
    theme_classic()
  pp
}

evolution = function(df, annee){
  res = df %>% filter(ANNEE == annee)
  res = res %>% filter(taux == max(res$taux))
  if (nrow(res) >1)
    res = res %>% slice(1)
  return(res)
}
                 
