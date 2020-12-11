library(mapalib)
library(sf)
library(tidyverse)

download_abstention_stats_saopaulo = function(year, position, city_id) {
  readr::read_csv('abstention_secao_sp.csv') %>%
    dplyr::rename(NUM_TURNO = NR_TURNO, NUM_ZONA = NR_ZONA, NUM_SECAO = NR_SECAO) %>%
    dplyr::rename(comparecimento = QT_COMPARECIMENTO, abstencao = QT_ABSTENCOES) %>%
    tidyr::pivot_longer(c(comparecimento, abstencao), names_to = 'NUMERO_CANDIDATO', values_to = 'QTDE_VOTOS') %>%
    dplyr::mutate(CODIGO_CARGO = position, ANO_ELEICAO = year)
}

sp20_abs = get_map_data(2020, 11, c(3550308), 4326, aggregate_all_candidates, turno = 2, with_blank_null = F, download_fun = download_abstention_stats_saopaulo)
sp20_all = get_map_data(2020, 11, c(3550308), 4326, aggregate_all_candidates, turno = 2, with_blank_null = F)

sp20_merged = st_drop_geometry(sp20_abs) %>%
  left_join(st_drop_geometry(sp20_all), 'main')

save(sp20_merged, file = 'pesquisas_abs.Rdata')
