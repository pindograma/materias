library(mapalib)

mourao = get_map_data(2016, 11, c(3541000), 31983, aggregate_majoritarian, turno = 1, party_number = 45, with_blank_null = T)
comin = get_map_data(2016, 13, c(3541000), 31983, aggregate_majoritarian, turno = 1, party_number = 14190, with_blank_null = T)

save(list = c('mourao', 'comin'), file = 'praia.Rdata')
