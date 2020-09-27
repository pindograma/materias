library(sf)
library(geobr)
library(tidyverse)
library(lubridate)

source('genpoints.R')

corr = read_csv('data/municipios_brasileiros_tse.csv') %>%
  mutate(cod_ibge6 = str_sub(codigo_ibge, 1, 6)) %>%
  mutate(codigo_tse = str_pad(codigo_tse, 5, pad = '0'))

regioes = tibble(
  state = unique(corr$uf),
  region = c('Norte', 'Nordeste', 'Norte', 'Nordeste', 'Nordeste',
             'Nordeste', 'Centro-Oeste', 'Sudeste', 'Centro-Oeste', 'Nordeste',
             'Sudeste', 'Centro-Oeste', 'Centro-Oeste', 'Norte', 'Nordeste',
             'Nordeste', 'Nordeste', 'Sul', 'Sudeste', 'Nordeste',
             'Norte', 'Norte', 'Sul', 'Sul', 'Nordeste',
             'Sudeste', 'Norte')
)

tabela = read_csv('tabela.csv', col_types = cols(X = col_skip(), Y = col_skip())) %>%
  mutate(mun = toupper(str_squish(word(Cidade, 1, sep = '\\(')))) %>%
  mutate(state = str_sub(word(Cidade, 2, sep = '\\('), 1, 2))

load('data/el02.Rdata')
el02 = el
load('data/el04.Rdata')
el04 = el
load('data/el06.Rdata')
el06 = el
load('data/el08.Rdata')
load('data/el10.Rdata')
load('data/el12.Rdata')
load('data/el14.Rdata')
load('data/el16.Rdata')
load('data/el18.Rdata')
load('data/el20.Rdata')

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
hist19_20 = read_csv2('data/hist20192020.csv', locale = locale(encoding = 'ISO-8859-1'))

hist_all = bind_rows(hist08, hist09, hist10, hist11, hist12, hist13, hist14,
                     hist15, hist16, hist17, hist18) %>%
  mutate(mun = toupper(`Município`)) %>%
  mutate(dt_inicio = dmy(`Data início`), dt_fim = ifelse(is.na(`Data fim prorrogada`), dmy(`Data fim`), dmy(`Data fim prorrogada`)))

h08 = hist_all %>%
  filter(dt_fim <= make_date(2008, 9, 1))

h09 = hist_all %>%
  filter(dt_fim >= make_date(2008, 11, 1) & dt_fim <= make_date(2010, 9, 1))

h11 = hist_all %>%
  filter(dt_fim >= make_date(2010, 11, 1) & dt_fim <= make_date(2012, 9, 1))

h13 = hist_all %>%
  filter(dt_fim >= make_date(2012, 11, 1) & dt_fim <= make_date(2014, 9, 1))

h15 = hist_all %>%
  filter(dt_fim >= make_date(2014, 11, 1) & dt_fim <= make_date(2016, 9, 1))

h17 = hist_all %>%
  filter(dt_fim >= make_date(2016, 11, 1) & dt_fim <= make_date(2018, 9, 1))

h19 = hist_all %>%
  filter(dt_fim >= make_date(2018, 11, 1) & dt_fim <= make_date(2020, 9, 1))

censo = read_csv('voto_obrigatorio.csv') %>%
  mutate(cod_ibge6 = str_sub(city, 1, 6)) %>%
  left_join(corr, 'cod_ibge6')

gen_obrig = function(el) {
  el %>%
    filter(IDADE != '16 ANOS' &
           IDADE != '17 ANOS' &
           IDADE != '70 A 79 ANOS' &
           IDADE != 'SUPERIOR A 79 ANOS' &
           ESCOLARIDADE != 'ANALFABETO') %>%
    mutate(SG_UE = str_pad(SG_UE, 5, pad = '0')) %>%
    left_join(censo, by = c('SG_UE' = 'codigo_tse')) %>%
    group_by(SG_UF, codigo_ibge, voto_obrig, nome_municipio) %>%
    summarize(eleitores = sum(QTDE)) %>%
    mutate(diff = eleitores - voto_obrig) %>%
    ungroup()
}

gen_obrig_newstyle = function(el) {
  el %>%
    filter(CD_FAIXA_ETARIA >= 1800 & CD_FAIXA_ETARIA <= 6569 &
           DS_GRAU_ESCOLARIDADE != 'ANALFABETO') %>%
    mutate(SG_UE = str_pad(CD_MUNICIPIO, 5, pad = '0')) %>%
    left_join(censo, by = c('SG_UE' = 'codigo_tse')) %>%
    group_by(SG_UF, codigo_ibge, voto_obrig, nome_municipio) %>%
    summarize(eleitores = sum(QT_ELEITORES_PERFIL), bio = sum(QT_ELEITORES_BIOMETRIA)) %>%
    mutate(diff = eleitores - voto_obrig) %>%
    ungroup()
}

eleitorado_obrig_02 = gen_obrig(el02)
eleitorado_obrig_04 = gen_obrig(el04)
eleitorado_obrig_06 = gen_obrig(el06)
eleitorado_obrig_08 = gen_obrig(el08)
eleitorado_obrig_10 = gen_obrig(el10)
eleitorado_obrig_12 = gen_obrig_newstyle(el12)
eleitorado_obrig_14 = gen_obrig(el14)
eleitorado_obrig_16 = gen_obrig(el16)
eleitorado_obrig_18 = gen_obrig_newstyle(el18)
eleitorado_obrig_20 = gen_obrig_newstyle(el_cur)

rm(el02, el04, el06, el08, el10, el12, el14, el16, el18, el_cur)

state_merge_20 = eleitorado_obrig_20 %>%
  group_by(SG_UF) %>%
  summarize(eleitores = sum(eleitores), bio = sum(bio)) %>%
  ungroup() %>%
  mutate(pct = bio / eleitores)

states = read_state() %>%
  left_join(state_merge_20, by = c('abbrev_state' = 'SG_UF'))

gen_smr = function(a, b, h) {
  a %>%
    filter(nome_municipio %in% h$mun & !is.na(nome_municipio)) %>%
    inner_join(b, by = 'codigo_ibge') %>%
    mutate(prop = (eleitores.y - eleitores.x) / eleitores.x) %>%
    select(codigo_ibge, SG_UF.x, nome_municipio.x, eleitores.x, eleitores.y, prop)
}

generate_loss_estimate = function(prev1, prev2, prev3, cur, after, h) {
  average_increase_1 = mean(gen_smr(prev1, prev2, h)$prop, na.rm = T)
  average_increase_2 = mean(gen_smr(prev2, prev3, h)$prop, na.rm = T)
  average_increase = mean(c(average_increase_1, average_increase_2))
  print(average_increase)
  
  print(mean(gen_smr(prev3, cur, h)$prop))
  
  after_smr = gen_smr(cur, after, h)
  
  after_increase = mean(after_smr$prop, na.rm = T)
  print(after_increase)
  
  diff_increase = after_increase - average_increase
  
  sum(after_smr$eleitores.x, na.rm = T) * diff_increase
}

# ---------

# NOTA: Esse arquivo pode ser encontrado em
# https://drive.google.com/file/d/1Z4HXG3fF-uNJQxCa_jZt4uroAv9uEL0r/view
geocoded_secoes_all = read_csv('~/Downloads/geocoded_secoes.csv', col_types = cols(
  approx_ad_CodSetor = col_number(),
  approx_ad_Distrito = col_number(),
  approx_ad_Subdistrito = col_number(),
  google_approx_lat = col_double(),
  google_approx_lon = col_double(),
  ibge_approx_lat = col_double(),
  ibge_approx_lon = col_double(),
  pl_lat = col_character(),
  pl_lon = col_character(),
  places_lat = col_double(),
  places_lon = col_double()
))

queimados_cities = c(3304144)
cb_vz_cities = c(5103403, 5108402)


basico_mt = read_csv2('Basico_MT.csv',
                      locale = locale(encoding = 'ISO-8859-1'),
                      col_types = cols(Cod_setor = col_character()))

basico_rj = read_csv2('Basico_RJ.csv',
                      locale = locale(encoding = 'ISO-8859-1'),
                      col_types = cols(Cod_setor = col_character()))

queimados_tracts = read_census_tract('RJ', simplified = F) %>%
  filter(code_muni %in% queimados_cities) %>%
  st_transform(31983) %>%
  left_join(basico_rj, by = c('code_tract' = 'Cod_setor'))

cb_vz_tracts = read_census_tract('MT', simplified = F) %>%
  filter(code_muni %in% cb_vz_cities) %>%
  st_transform(31981) %>%
  left_join(basico_mt, by = c('code_tract' = 'Cod_setor'))

geocoded_queimados = geocoded_secoes_all %>%
  filter(codigo_ibge %in% queimados_cities)

geocoded_cb_vz = geocoded_secoes_all %>%
  filter(codigo_ibge %in% cb_vz_cities)

# NOTA: Esses arquivos podem ser encontrados no
# Repositório de Dados Eleitorais do TSE.
secoes_queimados_16 = read_csv2('~/Downloads/perfil_eleitor_secao_2016_RJ/perfil_eleitor_secao_2016_RJ.csv', locale = locale(encoding = 'ISO-8859-1')) %>%
  filter(NM_MUNICIPIO == 'QUEIMADOS') %>%
  select(ANO_ELEICAO, NM_MUNICIPIO, SG_UF, NR_ZONA, NR_SECAO, CD_GENERO, CD_ESTADO_CIVIL, CD_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE, QT_ELEITORES_PERFIL, QT_ELEITORES_BIOMETRIA)

secoes_queimados_18 = read_csv2('~/Downloads/perfil_eleitor_secao_2018_RJ/perfil_eleitor_secao_2018_RJ.csv', locale = locale(encoding = 'ISO-8859-1')) %>%
  filter(NM_MUNICIPIO == 'QUEIMADOS') %>%
  select(ANO_ELEICAO, NM_MUNICIPIO, SG_UF, NR_ZONA, NR_SECAO, CD_GENERO, CD_ESTADO_CIVIL, CD_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE, QT_ELEITORES_PERFIL, QT_ELEITORES_BIOMETRIA)

secoes_cb_vz_18 = read_csv2('~/Downloads/perfil_eleitor_secao_2018_MT/perfil_eleitor_secao_2018_MT.csv', locale = locale(encoding = 'ISO-8859-1')) %>%
  filter(NM_MUNICIPIO == 'CUIABÁ' | NM_MUNICIPIO == 'VÁRZEA GRANDE') %>%
  select(ANO_ELEICAO, NM_MUNICIPIO, SG_UF, NR_ZONA, NR_SECAO, CD_GENERO, CD_ESTADO_CIVIL, CD_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE, QT_ELEITORES_PERFIL, QT_ELEITORES_BIOMETRIA)

secoes_cb_vz_20 = read_csv2('~/Downloads/perfil_eleitor_secao_2020_MT/perfil_eleitor_secao_2020_MT.csv', locale = locale(encoding = 'ISO-8859-1')) %>%
  filter(NM_MUNICIPIO == 'CUIABÁ' | NM_MUNICIPIO == 'VÁRZEA GRANDE') %>%
  select(ANO_ELEICAO, NM_MUNICIPIO, SG_UF, NR_ZONA, NR_SECAO, CD_GENERO, CD_ESTADO_CIVIL, CD_FAIXA_ETARIA, DS_GRAU_ESCOLARIDADE, QT_ELEITORES_PERFIL, QT_ELEITORES_BIOMETRIA)

secoes_queimados_16_ag = secoes_queimados_16 %>%
  filter(CD_FAIXA_ETARIA >= 1800 & CD_FAIXA_ETARIA <= 6569 &
         DS_GRAU_ESCOLARIDADE != 'ANALFABETO') %>%
  group_by(NR_ZONA, NR_SECAO) %>%
  summarize(eleitores16 = sum(QT_ELEITORES_PERFIL)) %>%
  ungroup()

secoes_queimados_18_ag = secoes_queimados_18 %>%
  filter(CD_FAIXA_ETARIA >= 1800 & CD_FAIXA_ETARIA <= 6569 &
         DS_GRAU_ESCOLARIDADE != 'ANALFABETO') %>%
  group_by(NR_ZONA, NR_SECAO) %>%
  summarize(eleitores18 = sum(QT_ELEITORES_PERFIL)) %>%
  ungroup()

secoes_cb_vz_18_ag = secoes_cb_vz_18 %>%
  filter(CD_FAIXA_ETARIA >= 1800 & CD_FAIXA_ETARIA <= 6569 &
         DS_GRAU_ESCOLARIDADE != 'ANALFABETO') %>%
  group_by(NR_ZONA, NR_SECAO) %>%
  summarize(eleitores18 = sum(QT_ELEITORES_PERFIL)) %>%
  ungroup()

secoes_cb_vz_20_ag = secoes_cb_vz_20 %>%
  filter(CD_FAIXA_ETARIA >= 1800 & CD_FAIXA_ETARIA <= 6569 &
         DS_GRAU_ESCOLARIDADE != 'ANALFABETO') %>%
  group_by(NR_ZONA, NR_SECAO) %>%
  summarize(eleitores20 = sum(QT_ELEITORES_PERFIL)) %>%
  ungroup()

EPSG = 31983
queimados_points = genpoints(queimados_tracts, geocoded_queimados %>% filter(ano == 2016)) %>%
  left_join(secoes_queimados_16_ag, by = c('zona' = 'NR_ZONA', 'secao' = 'NR_SECAO')) %>%
  left_join(secoes_queimados_18_ag, by = c('zona' = 'NR_ZONA', 'secao' = 'NR_SECAO')) %>%
  mutate(diff = eleitores18 - eleitores16)

EPSG = 31981
cb_vz_points = genpoints(cb_vz_tracts, geocoded_cb_vz %>% filter(ano == 2018)) %>%
  left_join(secoes_cb_vz_18_ag, by = c('zona' = 'NR_ZONA', 'secao' = 'NR_SECAO')) %>%
  left_join(secoes_cb_vz_20_ag, by = c('zona' = 'NR_ZONA', 'secao' = 'NR_SECAO')) %>%
  mutate(diff = eleitores20 - eleitores18)

save(list = c('cb_vz_tracts', 'cb_vz_points', 'queimados_tracts', 'queimados_points', 'tabela', 'eleitorado_obrig_02', 'eleitorado_obrig_04', 'eleitorado_obrig_06', 'eleitorado_obrig_08', 'eleitorado_obrig_10', 'eleitorado_obrig_12', 'eleitorado_obrig_14', 'eleitorado_obrig_16', 'eleitorado_obrig_18', 'eleitorado_obrig_20', 'h08', 'h09', 'h11', 'h13', 'h15', 'h17', 'h19', 'regioes', 'hist_all', 'hist19_20', 'states', 'gen_smr', 'generate_loss_estimate'), file = 'disenfranchisement2.Rdata')


