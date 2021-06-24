library(geobr)
library(sf)
library(tidyverse)
library(readxl)

normalize_simple = function(x) {
    stringi::stri_trans_general(str = x, id = 'Latin-ASCII') %>%
        str_squish() %>%
        toupper()
}

normalize_dp_name = function(x) {
  normalize_simple(x) %>%
    str_replace_all(' SAO ', 'S') %>%
    str_replace_all(' SANTO ', 'S') %>%
    str_replace_all(' SANTA ', 'S') %>%
    str_replace_all('PRESIDENTE', 'PRES') %>%
    str_replace_all('[. º-]', '') %>%
    str_replace('^0+', '')
}

#----

pcsp_corr = read_excel('DP-SECCIONAIS (1).xlsx') %>%
  select(DP, SECCIONAL)

cisp_dscr = read_csv2('de_para_aisp_cisp.csv', locale = locale(encoding = 'Latin1'))

rio_tract_data = read_csv2('Basico_RJ.csv',
                           locale = locale(encoding = 'Latin1'),
                           col_types = cols(Cod_setor = col_character())) %>%
  select(Cod_setor, V002, V009)

sp_tract_data = read_csv2('Basico_SP1.csv',
                           locale = locale(encoding = 'Latin1'),
                           col_types = cols(Cod_setor = col_character())) %>%
  select(Cod_setor, V002, V009)

rio_tracts = read_census_tract(code_tract = 33, simplified = F) %>%
  mutate(code_tract = as.character(code_tract)) %>%
  inner_join(rio_tract_data, c('code_tract' = 'Cod_setor')) %>%
  st_transform(4326)

sp_tracts = read_census_tract(code_tract = 3550308, simplified = F) %>%
  mutate(code_tract = as.character(code_tract)) %>%
  inner_join(sp_tract_data, c('code_tract' = 'Cod_setor')) %>%
  st_transform(4326)

rj_aisp = st_read('shapes/lm_aisp_2019.shp') %>%
  select(aisp) %>%
  left_join(cisp_dscr %>% select(aisp, aisp_dscr) %>% distinct(), c('aisp' = 'aisp'))

sp_seccionais_policiais = st_read('shapes/Distritos_Policiais_SHP.shp',
                                 options = "ENCODING=WINDOWS-1252") %>%
  group_by(DELEGACIA) %>%
  summarize()

population_by_aisp = rj_aisp %>%
  st_join(rio_tracts, st_contains) %>%
  st_drop_geometry() %>%
  group_by(aisp, aisp_dscr) %>%
  summarize(pop = sum(V002, na.rm = T),
            renda = mean(V009, na.rm = T))

population_by_seccional = sp_seccionais_policiais %>%
  st_join(sp_tracts, st_contains) %>%
  st_drop_geometry() %>%
  group_by(DELEGACIA) %>%
  summarize(pop = sum(V002, na.rm = T),
            renda = mean(V009, na.rm = T))

pmrj_data = read_csv('pm-rio.csv') %>%
  rowwise() %>%
  mutate(average_pm = round(mean(c_across(-OPM)))) %>%
  ungroup() %>%
  select(OPM, average_pm) %>%
  mutate(OPM = as.numeric(OPM))

pm_analysis = population_by_aisp %>%
  left_join(pmrj_data, c('aisp' = 'OPM')) %>%
  mutate(habitantes_por_pm = pop / average_pm,
         pm_100k = average_pm / pop * 100000) %>%
  mutate(capital = aisp %in% c(2, 3, 4, 5, 6, 9, 14, 16, 17, 18, 19, 22, 23, 27, 31, 40, 41)) %>%
  ungroup()

pcsp_data = read_excel('DP-SECCIONAIS (1).xlsx') %>%
  distinct(SECCIONAL, DELEGADO, ESCRIVÃO, INVESTIGADOR)

pcsp_analysis = population_by_seccional %>%
  left_join(pcsp_data, c('DELEGACIA' = 'SECCIONAL')) %>%
  mutate(habitantes_por_delegado = pop / DELEGADO,
         habitantes_por_escrivao = pop / ESCRIVÃO,
         habitantes_por_investigador = pop / INVESTIGADOR) %>%
  mutate(del_100k = DELEGADO / pop * 100000)

spcidade = geobr::read_municipality(code_muni = 3550308) %>%
  st_transform(31983)
sp_dps = st_read('shapes/DPS_ESTADO_SP.shp') %>%
  st_transform(31983) %>%
  st_buffer(1) %>%
  mutate(area = normalize_dp_name(DpGeoDes)) %>%
  st_join(st_transform(spcidade, 31983), st_intersects) %>%
  filter(!is.na(name_muni)) %>%
  filter(!(area %in% c('1DPCAJAMAR', 'DELPOLJUQUITIBA', '1DPCAIEIRAS', '2DPBARUERI',
                       '4DPDIADEMA', '2DPTABOAODASERRA', '3DPOSASCO', '4DPSBERNARDODOCAMPO',
                       'DELPOLITAQUAQUECETUBA', 'DELPOLCAIEIRAS', '3DPSVICENTE',
                       '1DPSCAETANODOSUL', '2DPSCAETANODOSUL', 'DELPOLSANTANADEPARNAIBA',
                       'DELPOLFERRAZDEVASCONCELOS', 'DELPOLEMBUGUACU',
                       'DELPOLMAIRIPORA', 'DELPOLITAPECERICADASERRA',
                       '1DPEMBUDASARTES', '1DPFERRAZDEVASCONCELOS',
                       '1DPITAPECERICADASERRA', '1DPDIADEMA', '1DPGUARULHOS',
                       '1DPMAUA', '1DPTABOAODASERRA', '2DPGUARULHOS',
                       '2DPSANDRE', '2DPOSASCO', '3DPITANHAEM', '3DPMAUA', '3DPDIADEMA',
                       '4DPMAUA', '4DPGUARULHOS', '5DPGUARULHOS', '5DPSANDRE',
                       '6DPOSASCO', '7DPOSASCO', '7DPSBERNARDODOCAMPO',
                       '9DPOSASCO', '2DPSBERNARDODOCAMPO'))) %>%
  mutate(area = as.numeric(word(area, 1, sep = 'DP'))) %>%
  left_join(pcsp_corr, c('area' = 'DP')) %>%
  mutate(SECCIONAL = ifelse(is.na(area), 2, SECCIONAL)) %>%
  group_by(SECCIONAL) %>%
  summarize()

save(list = c('pm_analysis', 'pcsp_analysis', 'sp_dps'), file = 'policiais.Rdata')
