library(geobr)
library(sf)
library(tidyverse)

cisp_dscr = read_csv2('de_para_aisp_cisp.csv', locale = locale(encoding = 'Latin1'))
cisp_mp = read_csv2('de_para_cisp_area_atuacao_familiaTerritorial.csv', locale = locale(encoding = 'Latin1'))

rj_cisp = st_read('shapes//lm_dp_2019.shp') %>%
  select(dp) %>%
  left_join(cisp_dscr %>% select(cisp, cisp_dscr), c('dp' = 'cisp'))

rj_areas_mp = rj_cisp %>%
  left_join(cisp_mp, c('dp' = 'cisp')) %>%
  group_by(area_atuacao_dk, area_atuacao_dscr) %>%
  summarize()

save(list = c('rj_areas_mp'), file = 'rj_areas.Rdata')
