library(tidyverse)
library(sf)

spcidade = geobr::read_municipality(code_muni = 3550308)
spdistritos = geobr::read_census_tract(code_tract = 3550308, simplified = F) %>% group_by(name_district) %>% summarize() %>% nngeo::st_remove_holes()

import_ssp = function(files) {
  map_dfr(files, function(x) {
    read_tsv(x, locale = locale(encoding = 'UTF-8', decimal_mark = ','), col_types = cols(
      NACIONALIDADE = col_character(), NOMEPESSOA = col_character(),
      TIPOPESSOA = col_character(), VITIMAFATAL = col_character(),
      ESTADOCIVIL = col_character(), PROFISSAO = col_character(),
      IDADE = col_number(), RG = col_character(), SEXO = col_character(),
      CORCUTIS = col_character(), NATUREZAVINCULADA = col_character(),
      TIPOVINCULO = col_character(), NATURALIDADE = col_character(),
      DATANASCIMENTO = col_character(), GRAUINSTRUCAO = col_character(),
      RG_UF = col_character()
    )) %>%
      mutate(file = x)
  })
}

sp_roubo_celular = list.files('./raw_crime_data/sp', pattern = '2019.*ROUBO.*CELULAR.*tsv', full.names = T)
sp_furto_celular = list.files('./raw_crime_data/sp', pattern = '2019.*FURTO.*CELULAR.*tsv', full.names = T)

oc_roubo_celular = import_ssp(sp_roubo_celular)
oc_furto_celular = import_ssp(sp_furto_celular)

furto_onibus = oc_furto_celular %>%
  filter(grepl('INTERIOR.*COLETIVO', RUBRICA)) %>%
  distinct(ANO_BO, NUM_BO, CIDADE, DELEGACIA_CIRCUNSCRICAO, DATAOCORRENCIA, HORAOCORRENCIA, BAIRRO, LATITUDE, LONGITUDE, DESCRICAOLOCAL) %>%
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326, na.fail = F)

roubo_onibus = oc_roubo_celular %>%
  filter(grepl('INTERIOR.*COLETIVO', RUBRICA)) %>%
  distinct(ANO_BO, NUM_BO, CIDADE, DELEGACIA_CIRCUNSCRICAO, DATAOCORRENCIA, HORAOCORRENCIA, BAIRRO, LATITUDE, LONGITUDE, DESCRICAOLOCAL) %>%
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326, na.fail = F)

save(list = c('furto_onibus', 'roubo_onibus', 'spcidade', 'spdistritos'), file = 'transporte.Rdata')
