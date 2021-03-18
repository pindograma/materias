library(tidyverse)
library(sf)
library(mapalib)
library(stringi)

load('election_maps.Rdata')

# Security Candidates
consulta_cand = read_csv2('data/consulta_cand_2020_BRASIL.csv', locale = locale(encoding = 'Latin1'))

policiais_2020 = read_csv('data/policiais.csv') %>%
  filter(ANO_ELEICAO == 2020)

policiais_rio = policiais_2020 %>%
  filter(MUNICIPIO == 'RIO DE JANEIRO' & CARGO == 'VEREADOR') %>%
  left_join(consulta_cand, c('MUNICIPIO' = 'NM_UE', 'CPF_CANDIDATO' = 'NR_CPF_CANDIDATO')) %>%
  pull(NR_CANDIDATO)

policiais_salvador = policiais_2020 %>%
  filter(MUNICIPIO == 'SALVADOR' & CARGO == 'VEREADOR') %>%
  left_join(consulta_cand, c('MUNICIPIO' = 'NM_UE', 'CPF_CANDIDATO' = 'NR_CPF_CANDIDATO')) %>%
  pull(NR_CANDIDATO)

policiais_sp = policiais_2020 %>%
  filter(MUNICIPIO == 'SAO PAULO' & CARGO == 'VEREADOR') %>%
  mutate(MUNICIPIO = 'SÃO PAULO') %>%
  left_join(consulta_cand, c('MUNICIPIO' = 'NM_UE', 'CPF_CANDIDATO' = 'NR_CPF_CANDIDATO')) %>%
  pull(NR_CANDIDATO)

# Security Maps
load('rio_ba_data.Rdata')

normalize_simple = function(x) {
  stri_trans_general(str = x, id = 'Latin-ASCII') %>%
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

spcidade = geobr::read_municipality(code_muni = 3550308)

sp_shape = st_read('data/DPS_ESTADO_SP/DPS_ESTADO_SP.shp') %>%
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
  mutate(area = word(area, 1, sep = 'DP'))

sp_ag_files = list.files('data/raw_crime_data/sp_aggregated', pattern = '*.csv', full.names = T)
parse_ssp_ag = function(x) {
  read_csv2(x, skip = 1, locale = locale(encoding = 'Latin1')) %>%
    select(Ocorrencia, Total) %>%
    filter(Ocorrencia %in% c('Nº DE VÍTIMAS EM HOMICÍDIO DOLOSO (3)', 'LESÃO CORPORAL DOLOSA',
                             'Nº DE VÍTIMAS EM LATROCÍNIO', 'TOTAL DE ESTUPRO (4)',
                             'ROUBO - OUTROS', 'ROUBO DE VEÍCULO', 'ROUBO DE CARGA',
                             'FURTO - OUTROS', 'FURTO DE VEÍCULO')) %>%
    pivot_wider(names_from = Ocorrencia, values_from = Total) %>%
    janitor::clean_names() %>%
    mutate(area = normalize_dp_name(str_replace(word(x, 2, -1), '\\.csv', ''))) %>%
    mutate(area = word(area, 1, sep = 'DP'))
}

sp_data_fmt = map_dfr(sp_ag_files, parse_ssp_ag) %>%
  mutate(year = 2020)

get_crime_police_data = function(data, shape, policiais, data_fmt, analysis_year) {
  data %>%
    st_join(st_transform(shape, 31983), st_within) %>%
    st_drop_geometry() %>%
    group_by(area) %>%
    summarize(across(contains('abs_votes'), sum, na.rm = T)) %>%
    pivot_longer(-area, names_prefix = 'abs_votes_', names_to = 'NUMERO_CANDIDATO', values_to = 'abs_votes') %>%
    filter(nchar(NUMERO_CANDIDATO) == 5) %>%
    group_by(area) %>%
    summarize(police_abs_votes = sum(abs_votes[NUMERO_CANDIDATO %in% policiais]),
              abs_votes = sum(abs_votes)) %>%
    mutate(police_ratio = police_abs_votes / abs_votes) %>%
    left_join(data_fmt %>% filter(year == analysis_year), 'area')
}

rio_ver_areas = get_crime_police_data(rio_ver, rename(rj_shape, area = dp), policiais_rio, rio_data_fmt, 2019) %>%
  filter(!is.na(area))

salvador_crime = get_crime_police_data(salvador_ver, ba_shape, policiais_salvador, ba_data_fmt, 2019) %>%
  filter(!is.na(area))

sp_crime = get_crime_police_data(sp_ver, sp_shape, policiais_sp, sp_data_fmt, 2020)

save(list = c('rio_ver_areas', 'salvador_crime', 'sp_crime', 'population_by_cisp', 'ba_shape', 'rj_shape', 'sp_shape'), file = 'policiais_output.Rdata')
