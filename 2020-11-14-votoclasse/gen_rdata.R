# devtools::install_github('pindograma/mapalib')

library(tidyverse)
library(mapalib)
library(cepespR)
library(sf)

codigos = readr::read_csv('municipios_brasileiros_tse.csv')

capitais = codigos %>%
  filter(capital == 1) %>%
  filter(codigo_ibge != '5300108') %>%
  pull(codigo_ibge)

capital_names = codigos %>%
  filter(capital == 1) %>%
  filter(codigo_ibge != '5300108') %>%
  pull(nome_municipio)

#mapas_08 = purrr::map(capitais, function(capital) {
#  get_map_data(2008, c(capital), 4326, aggregate_majoritarian, position = 11, turno = 'latest', party_number = 'winner', with_blank_null = F)
#})
#
#mapas_12 = purrr::map(capitais, function(capital) {
#  get_map_data(2012, c(capital), 4326, aggregate_majoritarian, position = 11, turno = 'latest', party_number = 'winner', with_blank_null = F)
#})
#
#mapas_16 = purrr::map(capitais, function(capital) {
#  get_map_data(2016, c(capital), 4326, aggregate_majoritarian, position = 11, turno = 'latest', party_number = 'winner', with_blank_null = F)
#})
#
#save(list = c('mapas_08', 'mapas_12', 'mapas_16'), file = 'renda_voto_bkp.Rdata')
load('renda_voto_bkp.Rdata')

get_low_end = function(mapas) {
  map_dbl(mapas, function(mapa) {
    mapa %>%
      filter(renda <= quantile(renda, .2, na.rm = T)) %>%
      pull(cand) %>%
      mean()
  })
}

get_high_end = function(mapas) {
  map_dbl(mapas, function(mapa) {
    mapa %>%
      filter(renda >= quantile(renda, .8, na.rm = T)) %>%
      pull(cand) %>%
      mean()
  })
}

mapas_08_dist = tibble(
  codigo_ibge = capitais,
  name = capital_names,
  mp = mapas_08,
  low_end = get_low_end(mapas_08),
  high_end = get_high_end(mapas_08),
)

mapas_12_dist = tibble(
  codigo_ibge = capitais,
  name = capital_names,
  mp = mapas_12,
  low_end = get_low_end(mapas_12),
  high_end = get_high_end(mapas_12),
)

mapas_16_dist = tibble(
  codigo_ibge = capitais,
  name = capital_names,
  mp = mapas_16,
  low_end = get_low_end(mapas_16),
  high_end = get_high_end(mapas_16),
)

pref_el_04 = get_candidates(2004, 'Prefeito', only_elected = T, cache = T)
pref_el_08 = get_candidates(2008, 'Prefeito', only_elected = T, cache = T)
pref_el_12 = get_candidates(2012, 'Prefeito', only_elected = T, cache = T)
pref_el_16 = get_candidates(2016, 'Prefeito', only_elected = T, cache = T)

pref_cand_08 = get_candidates(2008, 'Prefeito', cache = T)
pref_cand_12 = get_candidates(2012, 'Prefeito', cache = T)
pref_cand_16 = get_candidates(2016, 'Prefeito', cache = T)

pref_votes_08 = get_votes(2008, 'Prefeito', cache = T)
pref_votes_12 = get_votes(2012, 'Prefeito', cache = T)
pref_votes_16 = get_votes(2016, 'Prefeito', cache = T)

sround_08 = pref_votes_08 %>%
  group_by(COD_MUN_IBGE) %>%
  summarize(turno = max(NUM_TURNO)) %>%
  ungroup()

sround_12 = pref_votes_12 %>%
  group_by(COD_MUN_IBGE) %>%
  summarize(turno = max(NUM_TURNO)) %>%
  ungroup()

sround_16 = pref_votes_16 %>%
  group_by(COD_MUN_IBGE) %>%
  summarize(turno = max(NUM_TURNO)) %>%
  ungroup()

sround = bind_rows(
  sround_08 %>% mutate(year = 'diff08'),
  sround_12 %>% mutate(year = 'diff12'),
  sround_16 %>% mutate(year = 'diff16'),
) %>%
  mutate(has_second_round = (turno == 2))

reel_08 = tibble(pref_votes_08) %>%
  left_join(pref_cand_08 %>% select(SIGLA_UE, NUMERO_CANDIDATO, CPF_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'NUMERO_CANDIDATO')) %>%
  left_join(pref_el_04 %>% select(SIGLA_UE, CPF_CANDIDATO, NOME_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'CPF_CANDIDATO')) %>%
  group_by(COD_MUN_IBGE) %>%
  filter(NUM_TURNO == max(NUM_TURNO)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  filter(row_number() <= 2) %>%
  summarize(has_reel = any(!is.na(NOME_CANDIDATO))) %>%
  ungroup()

reel_12 = tibble(pref_votes_12) %>%
  left_join(pref_cand_12 %>% select(SIGLA_UE, NUMERO_CANDIDATO, CPF_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'NUMERO_CANDIDATO')) %>%
  left_join(pref_el_08 %>% select(SIGLA_UE, CPF_CANDIDATO, NOME_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'CPF_CANDIDATO')) %>%
  group_by(COD_MUN_IBGE) %>%
  filter(NUM_TURNO == max(NUM_TURNO)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  filter(row_number() <= 2) %>%
  summarize(has_reel = any(!is.na(NOME_CANDIDATO))) %>%
  ungroup()

reel_16 = tibble(pref_votes_16) %>%
  left_join(pref_cand_16 %>% select(SIGLA_UE, NUMERO_CANDIDATO, CPF_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'NUMERO_CANDIDATO')) %>%
  left_join(pref_el_12 %>% select(SIGLA_UE, CPF_CANDIDATO, NOME_CANDIDATO), by = c('COD_MUN_TSE' = 'SIGLA_UE', 'CPF_CANDIDATO')) %>%
  group_by(COD_MUN_IBGE) %>%
  filter(NUM_TURNO == max(NUM_TURNO)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  filter(row_number() <= 2) %>%
  summarize(has_reel = any(!is.na(NOME_CANDIDATO))) %>%
  ungroup()

reel = bind_rows(
  reel_08 %>% mutate(year = 'diff08'),
  reel_12 %>% mutate(year = 'diff12'),
  reel_16 %>% mutate(year = 'diff16')
)

save(list = c('mapas_08_dist', 'mapas_12_dist', 'mapas_16_dist', 'sround', 'reel'), file = 'renda_voto.Rdata')
