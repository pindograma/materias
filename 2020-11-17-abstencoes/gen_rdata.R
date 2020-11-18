library(tidyverse)
library(lubridate)

# Supplementary Data
mun = read_csv('municipios_brasileiros_tse.csv') %>%
  mutate(codigo_tse = str_pad(codigo_tse, 5, pad = '0'))

tabela1552 = read_csv('tabela1552.csv', col_types = cols(total_idade = col_character())) %>%
  mutate(total_idade = ifelse(total_idade == '-', 0, as.double(total_idade)))
tabela3563 = read_csv('tabela3563.csv')

hist08 = read_csv2('data/hist2008.csv', locale = locale(encoding = 'ISO-8859-1'))
hist09 = read_csv2('data/hist2009.csv', locale = locale(encoding = 'ISO-8859-1'))
hist10 = read_csv2('data/hist2010.csv', locale = locale(encoding = 'ISO-8859-1'))
hist11 = read_csv2('data/hist2011.csv', locale = locale(encoding = 'ISO-8859-1'))
hist12 = read_csv2('data/hist2012.csv', locale = locale(encoding = 'ISO-8859-1'))
hist13 = read_csv2('data/hist2013.csv', locale = locale(encoding = 'ISO-8859-1'))
hist14 = read_csv2('data/hist2014.csv', locale = locale(encoding = 'ISO-8859-1'))
hist15 = read_csv2('data/hist2015.csv', locale = locale(encoding = 'ISO-8859-1'))
hist16 = read_csv2('data/hist2016.csv', locale = locale(encoding = 'ISO-8859-1'))
hist17 = read_csv2('data/hist2017.csv', locale = locale(encoding = 'ISO-8859-1'))
hist18 = read_csv2('data/hist2018.csv', locale = locale(encoding = 'ISO-8859-1'))

hist_all = bind_rows(hist08, hist09, hist10, hist11, hist12, hist13, hist14,
                     hist15, hist16, hist17, hist18) %>%
  mutate(mun = toupper(`Município`)) %>%
  mutate(dt_inicio = dmy(`Data início`), dt_fim = ifelse(is.na(`Data fim prorrogada`), dmy(`Data fim`), dmy(`Data fim prorrogada`))) %>%
  left_join(mun, c('UF' = 'uf', 'mun' = 'nome_municipio'))

hist_up_to_18 = hist_all %>%
  filter(dt_fim <= make_date(2018, 10, 1))

hist_up_to_16 = hist_all %>%
  filter(dt_fim <= make_date(2016, 10, 1))

h17 = hist_all %>%
  filter(dt_fim >= make_date(2016, 11, 1) & dt_fim <= make_date(2018, 9, 1))

# Electorate Data
load('el16.Rdata')
eleitorado_16_por_cidade = el16 %>%
  mutate(SG_UE = str_pad(SG_UE, 5, pad = '0')) %>%
  group_by(SG_UE) %>%
  summarize(eleitores = sum(QTDE)) %>%
  ungroup()
rm(el16)

load('el18.Rdata')
eleitorado_18_por_cidade = el18 %>%
  mutate(SG_UE = str_pad(CD_MUNICIPIO, 5, pad = '0')) %>%
  group_by(SG_UE) %>%
  summarize(eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  ungroup()
rm(el18)

election_results = read_csv('resultados.csv', locale = locale(decimal_mark = ','))

# Abstention Data

abstention_16 = cepespR::get_votes(2016, 'Prefeito', cache = T, blank_votes = T, null_votes = T) %>%
  filter(NUM_TURNO == 1) %>%
  group_by(COD_MUN_TSE, COD_MUN_IBGE) %>%
  summarize(total_votes = sum(QTDE_VOTOS)) %>%
  ungroup() %>%
  left_join(eleitorado_16_por_cidade, by = c('COD_MUN_TSE' = 'SG_UE')) %>%
  mutate(abstention = eleitores - total_votes)

abstention_18 = cepespR::get_votes(2018, 'President', cache = T, blank_votes = T, null_votes = T) %>%
  filter(NUM_TURNO == 1) %>%
  group_by(COD_MUN_TSE, COD_MUN_IBGE) %>%
  summarize(total_votes = sum(QTDE_VOTOS)) %>%
  ungroup() %>%
  left_join(eleitorado_18_por_cidade, by = c('COD_MUN_TSE' = 'SG_UE')) %>%
  mutate(abstention = eleitores - total_votes)

abstention_20 = election_results %>%
  distinct(codigo_tse, eleitorado, abstencoes) %>%
  rename(abstention = abstencoes, eleitores = eleitorado)

abstention_20_with_vars = abstention_20 %>%
  left_join(mun, c('codigo_tse' = 'codigo_tse')) %>%
  left_join(tabela1552, 'codigo_ibge') %>%
  left_join(tabela3563, 'codigo_ibge')

# Basic Abstention Analyses

get_abstention = function(a) {
  sum(a$abstention) / sum(a$eleitores)
}

with_bio_16 = abstention_16 %>%
  semi_join(hist_up_to_16, by = c('COD_MUN_TSE' = 'codigo_tse'))

without_bio_16 = abstention_16 %>%
  anti_join(hist_up_to_16, by = c('COD_MUN_TSE' = 'codigo_tse'))

with_bio_18 = abstention_18 %>%
  semi_join(hist_up_to_18, by = c('COD_MUN_TSE' = 'codigo_tse'))

without_bio_18 = abstention_18 %>%
  anti_join(hist_up_to_18, by = c('COD_MUN_TSE' = 'codigo_tse'))

with_bio_20 = abstention_20 %>%
  semi_join(hist_all, by = c('codigo_tse' = 'codigo_tse'))

without_bio_20 = abstention_20 %>%
  anti_join(hist_all, by = c('codigo_tse' = 'codigo_tse'))

# Summary Functions
age_over_50_summary = function(x) {
  x %>%
    group_by(codigo_tse) %>%
    summarize(over_50 = sum(total_idade[grupo_idade %in% c(
      '50 a 54 anos',
      '55 a 59 anos',
      '60 a 64 anos',
      '65 a 69 anos',
      '70 a 74 anos',
      '75 a 79 anos',
      '80 a 89 anos',
      '90 a 99 anos',
      '100 anos ou mais'
    )]) / sum(total_idade))
}

pop_summary = function(x) {
  x %>%
    group_by(codigo_tse) %>%
    summarize(pop = sum(total_idade))
}

save(list = ls(), file = 'abstencoes.Rdata')
