library(tidyverse)
library(sf)
library(mapalib)
library(stringi)

# Election Maps
rio_ver = get_map_points(2020, 13, c(3304557), 31983, aggregate_all_candidates, with_blank_null = F, turno = 1)
fort_ver = get_map_points(2020, 13, c(2304400), 31983, aggregate_all_candidates, with_blank_null = F, turno = 1)
salvador_ver = get_map_points(2020, 13, c(2927408), 31983, aggregate_all_candidates, with_blank_null = F, turno = 1)
sp_ver = get_map_points(2020, 13, c(3550308), 31983, aggregate_all_candidates, with_blank_null = F, turno = 1)

save(list = ls(), file = 'election_maps.Rdata')
