library(geobr)
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(httr)
library(nngeo)

source('genpoints.R')

normalize_names = function(x, prefix) {
  paste0(prefix, '_', x)
}

# Constants
bad_tracts = c('330455705240148', '330455705240150', '330455705240151',
               '330455705240152', '330455705240153', '330455705240447',
               '330455705240558', '330455705240567', '330455705240601')

zona_sul = c('Copacabana', 'Lagoa', 'Botafogo', 'Rocinha')
zona_oeste = c('Jacarepaguá', 'Barra Da Tijuca', 'Cidade De Deus', 'Bangu',
               'Campo Grande', 'Santa Cruz', 'Guaratiba', 'Realengo')
zona_norte = c('Ramos', 'Penha', 'Inhaúma', 'Méier', 'Irajá', 'Madureira',
               'Anchieta', 'Pavuna', 'Jacarezinho', 'Complexo Do Alemão',
               'Maré', 'Vigário Geral', 'Tijuca', 'Vila Isabel', 'Ilha Do Governador')
centro = c('Portuária', 'Centro', 'Rio Comprido', 'Santa Teresa', 'São Cristóvão')

cities = c(3304557)
states = c('RJ')
years = c(2014, 2018)

# Datasets
load('geocoded_secoes_all_rio.Rdata')

correspondence = read_csv('municipios_brasileiros_tse.csv')

ap = read_weighting_area(code_weighting = cities[1], simplified = F) %>%
    st_transform(31983)

ev = read_csv('evangelicos_rio.csv') %>%
    mutate(n = as.integer(n))

ocorrencias = st_read('ocorrencias.geojson')

tracts_orig = do.call(rbind, map(cities, function (city_id) {
  read_census_tract(code_tract = city_id, simplified = F) %>%
    st_transform(31983) %>%
    mutate(Distrito = as.numeric(str_sub(code_district, start = -2))) %>%
    mutate(Subdistrito = as.numeric(str_sub(code_subdistrict, start = -2))) %>%
    mutate(CodSetor = as.numeric(str_sub(code_tract, start = -4)))
}))

ibge_data = read_delim(paste0('Basico_RJ.csv'), ";", escape_double = FALSE,
                       locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"),
                       trim_ws = TRUE, col_types = cols(
                         Cod_setor = col_character(),
                         Cod_UF = col_skip(),
                         Cod_RM = col_skip())) %>%
  rename_with(normalize_names, starts_with('V'), 'basico')

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

# Code
tracado = read_municipality(code_muni = cities[1])

votes_pres = votes %>%
  filter(CD_CARGO == 1 & NR_TURNO == 2) %>%
  group_by(ano, NUM_ZONA, NUM_SECAO) %>%
  summarize(pt = sum(QT_VOTOS[NUM_VOTAVEL == 13]) / sum(QT_VOTOS),
            bolsonaro = sum(QT_VOTOS[NUM_VOTAVEL == 17]) / sum(QT_VOTOS)) %>%
  ungroup()

tracts = tracts_orig %>%
  filter(code_muni %in% cities) %>%
  mutate(zona_cidade = case_when(
    name_subdistrict %in% zona_sul ~ 'sul',
    name_subdistrict %in% zona_norte ~ 'norte',
    name_subdistrict %in% centro ~ 'centro',
    name_subdistrict %in% zona_oeste ~ 'oeste',
    T ~ NA_character_
  ))

zonas_cidade = tracts %>%
  group_by(zona_cidade) %>%
  summarize() %>%
  st_remove_holes()

zonas = st_read('zonas-eleitorais/Versao_Final_Shape_CAPITAL.shp') %>%
  st_transform(31983) %>%
  rename(zona = Name)

intersects_without_touching = function(x, y) {
    st_relate(x, y, 'T********')
}

favelas = st_read('shape-favelas/Limite%20Favelas.shp') %>%
  st_set_crs(31983) %>%
  filter(Bairro != 'Cidade de Deus' &
         Complexo != 'Maré' &
         Complexo != 'Complexo do Alemão' &
         Complexo != 'Jacarezinho') %>%
  mutate_if(is.factor, as.character) %>%
  mutate(norm_complexo = ifelse(
      Complexo == 'Isolada',
      paste0('_', Nome),
      Complexo
  )) %>%
  group_by(norm_complexo) %>%
  summarize(pop = sum(Pop_SABREN),
            has_upp = any(UPP != 'N')) %>%
  filter(pop >= 1000) %>%
  ungroup() %>%
  st_join(tracts, intersects_without_touching) %>%
  group_by(norm_complexo) %>%
  summarize(pop = first(pop),
            has_upp = first(has_upp)) %>%
  ungroup()

favelas_with_vote_all = geocoded_secoes_all_rio %>%
  group_split(ano) %>%
  map_dfr(function(geocoded_secoes) {
    points = genpoints(tracts, geocoded_secoes)
    
    favelas_match_1 = st_join(favelas, points, function(x, y) st_contains(st_buffer(x, 350), y))
    not_found = favelas_match_1 %>%
      filter(is.na(secao)) %>%
      pull(norm_complexo) %>%
      unique()
    
    favelas_missing = favelas %>%
      subset(norm_complexo %in% not_found) %>%
      st_join(zonas, st_within)
    
    favelas_match_2 = favelas_missing %>%
      group_split(zona) %>%
      map_dfr(function(x) {
        if (is.na(first(x$zona))) {
          zona_points = points
        } else {
          zona_points = filter(points, zona == first(x$zona))
        }
        
        st_join(x, zona_points, st_nn, k = 2, progress = F)
      }) %>%
      mutate(zona = zona.y) %>%
      select(-zona.x, -zona.y)
    
    favelas_match = rbind(
      favelas_match_1 %>% select(-contains('rn')),
      favelas_match_2 %>% select(-contains('rn'))
    )
      
    favelas_subd = tracts %>%
      inner_join(ibge_data %>% select(Cod_setor, basico_V002, basico_V009),
                 by = c('code_tract' = 'Cod_setor')) %>%
      filter(name_subdistrict == 'Cidade De Deus' |
             name_subdistrict == 'Complexo Do Alemão' |
             name_subdistrict == 'Jacarezinho' |
             name_subdistrict == 'Maré') %>%
      group_by(name_subdistrict) %>%
      summarize(pop = sum(basico_V002, na.rm = T), has_upp = T) %>%
      rename(norm_complexo = name_subdistrict)
    
    favelas_subd_match = favelas_subd %>%
      st_join(points, st_contains) %>%
      rename(geometry = geom)
    
    all = rbind(
      favelas_match %>% select(-contains('rn')),
      favelas_subd_match %>% select(-contains('rn'))
    )
    
    j = c('ano' = 'ano', 'zona' = 'NUM_ZONA', 'secao' = 'NUM_SECAO')
    all %>%
      left_join(votes_pres, by = j)
  })

favelas_grouped = favelas_with_vote_all %>%
  mutate(pt_2014 = ifelse(ano == 2014, pt, NA)) %>%
  mutate(pt_2018 = ifelse(ano == 2018, pt, NA)) %>%
  mutate(bolsonaro_2018 = ifelse(ano == 2018, bolsonaro, NA)) %>%
  group_by(norm_complexo, has_upp, pop) %>%
  summarize(
      bolsonaro_2018 = mean(bolsonaro_2018, na.rm = T),
      pt_2014 = mean(pt_2014, na.rm = T),
      pt_2018 = mean(pt_2018, na.rm = T)
  ) %>%
  mutate(margin = (bolsonaro_2018 - pt_2018) * 100) %>%
  ungroup()

ev_summarized = ev %>%
  group_by(V0011) %>%
  summarize(evangelicos = sum(n[V6121 >= 210 & V6121 <= 499]) / sum(n),
            catolicos = sum(n[V6121 == 110]) / sum(n))

ap_ev = inner_join(ap, ev_summarized, by = c('code_weighting_area' = 'V0011'))

favelas_grouped_ev = st_join(favelas_grouped, ap_ev, st_intersects) %>%
  select(-code_muni, -code_weighting_area, -code_state, -abbrev_state, -code_region, -name_region) %>%
  group_by(norm_complexo) %>%
  summarize_if(is.numeric, mean, na.rm = T) %>%
  ungroup()

tr_mil = st_join(st_buffer(favelas_grouped_ev, 350), st_transform(ocorrencias, 31983), st_contains, T) %>%
  mutate(milicia = ifelse(organizaca == 'milicia', 1, 0)) %>%
  group_by(norm_complexo) %>%
  summarize_if(is.numeric, mean, na.rm = T) %>%
  ungroup()

save(list = c('tracado', 'favelas_grouped', 'ap_ev', 'favelas_grouped_ev',
              'tr_mil', 'zonas_cidade'), file = 'Favelas.Rdata')
