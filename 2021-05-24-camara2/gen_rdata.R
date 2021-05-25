library(tidyverse)
source("../../materias/theme.R")

byparty <- read_csv("votos_por_partido.csv") %>%
  mutate(siglaPartido = ifelse(siglaPartido == "PRB", "REPUBLICANOS", siglaPartido)) %>% 
  mutate(maioria = ifelse(Sim == Não, NA, ifelse(Sim > Não, "Sim", "Não")))

bydeputy <- read_csv("votos_por_deputado.csv") %>%
  pivot_longer(cols = c(previdencia, orcamento, fundeb, wilson)) %>% 
  group_by(id) %>% 
  mutate(value = ifelse(value %in% c("Abstenção", "Artigo 17"), NA_character_ , value)) %>% 
  filter(sum(is.na(value)) < 3 ) %>% 
  mutate(untie = is.na(value) & sum(is.na(value)) == 1) %>% 
  ungroup() %>% 
  left_join(byparty, by = c("siglaPartido" = "siglaPartido", "name" = "votacao")) %>% 
  mutate(value = ifelse(untie, maioria, value)) %>% 
  select(id:value) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(grupo = case_when(
    previdencia == "Sim" & orcamento == "Não" & fundeb == "Não" & wilson == "Não" ~ "zambelli",
    previdencia == "Sim" & orcamento == "Não" & fundeb == "Sim" & wilson == "Não" ~ "sampaio",
    previdencia == "Não" & orcamento == "Sim" & fundeb == "Sim" & wilson == "Sim" ~ "gleisi",
    previdencia == "Não" & orcamento == "Sim" & fundeb == "Sim" & wilson == "Não" ~ "freixo",
    previdencia == "Sim" & orcamento == "Sim" & fundeb == "Sim" & wilson == "Não" ~ "tabata",
    is.na(previdencia) | is.na(orcamento) | is.na(fundeb) | is.na(wilson) ~ "abstencao",
    previdencia == "Sim" & orcamento == "Não" &                   wilson == "Sim" ~ "flordelis",
    TRUE ~ "outros"
  ))

save(list = c("bydeputy", "byparty"), file = "camara.RData")
