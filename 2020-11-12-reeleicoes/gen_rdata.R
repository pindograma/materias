library(tidyverse)
library(cepespR)

pref_el_00 = get_candidates(2000, 'Prefeito', only_elected = T, cache = T)
pref_el_04 = get_candidates(2004, 'Prefeito', only_elected = T, cache = T)
pref_el_08 = get_candidates(2008, 'Prefeito', only_elected = T, cache = T)
pref_el_12 = get_candidates(2012, 'Prefeito', only_elected = T, cache = T)

pref_cand_04 = get_candidates(2004, 'Prefeito', cache = T)
pref_cand_08 = get_candidates(2008, 'Prefeito', cache = T)
pref_cand_12 = get_candidates(2012, 'Prefeito', cache = T)
pref_cand_16 = get_candidates(2016, 'Prefeito', cache = T)

pref_votes_04 = get_votes(2004, 'Prefeito', cache = T)
pref_votes_08 = get_votes(2008, 'Prefeito', cache = T)
pref_votes_12 = get_votes(2012, 'Prefeito', cache = T)
pref_votes_16 = get_votes(2016, 'Prefeito', cache = T)

reel_04 = tibble(pref_votes_04) %>%
  left_join(pref_cand_04 %>% select(SIGLA_UE, NUMERO_CANDIDATO, CPF_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'NUMERO_CANDIDATO')) %>%
  left_join(pref_el_00 %>% select(SIGLA_UE, CPF_CANDIDATO, NOME_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'CPF_CANDIDATO')) %>%
  group_by(COD_MUN_IBGE) %>%
  filter(NUM_TURNO == max(NUM_TURNO)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  summarize(has_reel = any(!is.na(NOME_CANDIDATO)), success = !is.na(first(NOME_CANDIDATO))) %>%
  ungroup()

reel_08 = tibble(pref_votes_08) %>%
  left_join(pref_cand_08 %>% select(SIGLA_UE, NUMERO_CANDIDATO, CPF_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'NUMERO_CANDIDATO')) %>%
  left_join(pref_el_04 %>% select(SIGLA_UE, CPF_CANDIDATO, NOME_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'CPF_CANDIDATO')) %>%
  group_by(COD_MUN_IBGE) %>%
  filter(NUM_TURNO == max(NUM_TURNO)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  summarize(has_reel = any(!is.na(NOME_CANDIDATO)), success = !is.na(first(NOME_CANDIDATO))) %>%
  ungroup()

reel_12 = tibble(pref_votes_12) %>%
  left_join(pref_cand_12 %>% select(SIGLA_UE, NUMERO_CANDIDATO, CPF_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'NUMERO_CANDIDATO')) %>%
  left_join(pref_el_08 %>% select(SIGLA_UE, CPF_CANDIDATO, NOME_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'CPF_CANDIDATO')) %>%
  group_by(COD_MUN_IBGE) %>%
  filter(NUM_TURNO == max(NUM_TURNO)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  summarize(has_reel = any(!is.na(NOME_CANDIDATO)), success = !is.na(first(NOME_CANDIDATO))) %>%
  ungroup()

reel_16 = tibble(pref_votes_16) %>%
  left_join(pref_cand_16 %>% select(SIGLA_UE, NUMERO_CANDIDATO, CPF_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'NUMERO_CANDIDATO')) %>%
  left_join(pref_el_12 %>% select(SIGLA_UE, CPF_CANDIDATO, NOME_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'CPF_CANDIDATO')) %>%
  group_by(COD_MUN_IBGE) %>%
  filter(NUM_TURNO == max(NUM_TURNO)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  summarize(has_reel = any(!is.na(NOME_CANDIDATO)), success = !is.na(first(NOME_CANDIDATO))) %>%
  ungroup()

save(list = c('reel_04', 'reel_08', 'reel_12', 'reel_16'), file = 'reel.Rdata')
