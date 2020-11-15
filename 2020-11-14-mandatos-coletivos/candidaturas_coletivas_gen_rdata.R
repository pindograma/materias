library(tidyverse)

cands = read_csv2('../polling/data/tse/consulta_cand_2020_BRASIL.csv')

coletivas = cands %>%
    filter(grepl('COLETIVO|COLETIVA|BANCADA', NM_URNA_CANDIDATO)) %>%
    filter(!grepl('TRANSPORTE COLETIVO', DS_OCUPACAO) & !grepl('TRANSPORTE COLETIVO', NM_URNA_CANDIDATO))

cands_by_state = cands %>%
    count(SG_UF)

save(list = c('coletivas', 'cands_by_state'), file = 'coletivas.Rdata')
