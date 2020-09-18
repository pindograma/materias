library(sf)
library(geobr)
library(ggplot2)
library(httr)
library(tidyverse)

source('genpoints.R')
load('geocoded_secoes_all_sp.Rdata')

correspondence = read_csv('municipios_brasileiros_tse.csv')

normalize_names = function(x, prefix) {
  paste0(prefix, '_', x)
}

cities = c(3550308)

ibge_data = read_delim('Basico_SP1.csv',
  ";", escape_double = FALSE, col_types = cols(
    Cod_setor = col_character(),
    Cod_UF = col_skip(),
    Cod_RM = col_skip()), 
    locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"), trim_ws = TRUE) %>%
  rename_with(normalize_names, starts_with('V'), 'basico')

tracts_orig = do.call(rbind, map(cities, function (city_id) {
  read_census_tract(code_tract = city_id, simplified = F) %>%
    st_transform(31983) %>%
    mutate(Distrito = as.numeric(str_sub(code_district, start = -2))) %>%
    mutate(Subdistrito = as.numeric(str_sub(code_subdistrict, start = -2))) %>%
    mutate(CodSetor = as.numeric(str_sub(code_tract, start = -4)))
}))

years = c(2012, 2014, 2016, 2018)

votes = map_dfr(years, function(year) {
  map_dfr(cities, function(city_id) {
    tse_id = correspondence %>%
      filter(codigo_ibge == city_id) %>%
      select(codigo_tse) %>%
      unlist()
    
    r = GET(paste0('http://pindograma-dados.s3.amazonaws.com/votos_por_secao/', year, '/', tse_id, '.csv'))
    write(content(r, 'text'), file = paste0('votes_', year, '.csv'))
    read_csv(paste0('votes_', year, '.csv')) %>%
      rename_all(recode, QTD_VOTOS = 'QT_VOTOS', NR_VOTAVEL = 'NUM_VOTAVEL') %>%
      mutate(ano = year)
  })
})

party_ideologies = read_delim('tpower.csv', ';') %>%
  filter(ANO_ELEICAO %in% years)

votes_sum = votes %>%
  filter(CD_CARGO == 6 | CD_CARGO == 7 | CD_CARGO == 13) %>%
  mutate(party = as.numeric(str_sub(NUM_VOTAVEL, end = 2))) %>%
  group_by(ano, NUM_ZONA, NUM_SECAO) %>%
  mutate(total = sum(QT_VOTOS)) %>%
  group_by(party, .add = T) %>%
  summarize(frac = sum(QT_VOTOS) / first(total)) %>%
  ungroup() %>%
  left_join(party_ideologies, by = c('ano' = 'ANO_ELEICAO', 'party' = 'NUM_PART')) %>%
  group_by(ano, NUM_ZONA, NUM_SECAO) %>%
  summarize(ideology = sum(frac * IDEO_IMPUTED, na.rm = T)) %>%
  ungroup()

votes_by_party = votes %>%
  filter(CD_CARGO == 6 | CD_CARGO == 7 | CD_CARGO == 13) %>%
  filter(NUM_VOTAVEL != 95 & NUM_VOTAVEL != 96) %>%
  mutate(party = as.numeric(str_sub(NUM_VOTAVEL, end = 2))) %>%
  group_by(ano, NUM_ZONA, NUM_SECAO) %>%
  mutate(total = sum(QT_VOTOS)) %>%
  group_by(party, .add = T) %>%
  summarize(party_frac = sum(QT_VOTOS) / first(total))

tracts = tracts_orig %>%
  filter(code_muni %in% cities)

points_18 = genpoints(tracts, filter(geocoded_secoes_all_sp, ano == 2018)) %>%
  inner_join(ibge_data, by = c('code_tract' = 'Cod_setor')) %>%
  inner_join(votes_sum, by = c('ano' = 'ano', 'zona' = 'NUM_ZONA', 'secao' = 'NUM_SECAO'))

points_18_parties = points_18 %>%
  st_drop_geometry() %>%
  inner_join(votes_by_party, by = c('ano' = 'ano', 'zona' = 'NUM_ZONA', 'secao' = 'NUM_SECAO'))

save(list = c('points_18_parties', 'party_ideologies'), file = 'points_18_parties.Rdata')