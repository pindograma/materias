library(tidyverse)
library(sf)

cisp_dscr = read_csv2('de_para_aisp_cisp.csv', locale = locale(encoding = 'Latin1'))
aisp_shapes = st_read('shapes/lm_aisp_2019.shp') %>%
  st_transform(4674)

#rio_tracts = st_read('/Users/Daniel/Downloads/rj_setores_censitarios (2)/33SEE250GC_SIR.shp')
rio_tracts = geobr::read_census_tract(code_tract = 33) %>%
  mutate(code_tract = as.character(code_tract))

rio_tract_data = read_csv2('Pessoa03_RJ.csv', col_types = cols(Cod_setor = col_character()), locale = locale(encoding = 'Latin1')) %>%
  mutate_at(vars(matches('V')), function(x) { suppressWarnings(as.numeric(x)) } ) %>%
  select(Cod_setor, V001, V002, V003, V005)

rio_tracts_wd = rio_tracts %>%
  left_join(rio_tract_data, c('code_tract' = 'Cod_setor')) %>%
  select(code_tract, V001, V002, V003, V005)

aisp_demographics = aisp_shapes %>%
  st_join(rio_tracts_wd, st_contains) %>%
  st_drop_geometry() %>%
  group_by(aisp) %>%
  summarize(across(starts_with('V'), sum, na.rm = T)) %>%
  mutate(branco_ibge = V002/V001, pardo_ibge = V005/V001, preto_ibge = V003/V001)

rio_mortes = read_csv2('Recurso_Protocolo_16434.csv', locale = locale(encoding = 'Latin1'))

mortes_por_aisp = rio_mortes %>%
  left_join(select(cisp_dscr, cisp_dscr, aisp), c('cisp' = 'cisp_dscr')) %>%
  mutate(counter = 1) %>%
  group_by(aisp) %>%
  summarize(branco_isp = sum(counter[cor %in% c('albino', 'branca')]),
            pardo_isp = sum(counter[cor == 'parda']),
            preto_isp = sum(counter[cor == 'negra'])) %>%
  mutate(total_isp = branco_isp + pardo_isp + preto_isp) %>%
  mutate(prop_branco_isp = branco_isp / total_isp,
         prop_pardo_isp = pardo_isp / total_isp,
         prop_preto_isp = preto_isp / total_isp)

analysis = aisp_demographics %>%
  left_join(mortes_por_aisp, 'aisp') %>%
  mutate(black_overrepresentation = (prop_preto_isp + prop_pardo_isp) - (pardo_ibge + preto_ibge))

saveRDS(analysis, 'analysis.rds')
