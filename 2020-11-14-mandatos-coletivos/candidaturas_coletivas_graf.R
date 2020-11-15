library(tidyverse)
library(geobr)

load('./coletivos.Rdata')

party_palette = tibble(
  party = c(45, 17, 10, 11, 14, 55, 43,
            15, 25, 30, 19, 22, 20, 12,
            13, 65, 50, 23, 40, 18, 16,
            29, 21, 77, 28, 33, 36, 51,
            70, 80, 90, 54, 44, 27, 31,
            35, 99),
  party_color = c('#5C88DA', '#003A70', '#41748D', '#56B7E6', '#131E29', '#F6BE00', '#006747',
            '#009A44', '#0857C3', '#FFA400', '#98B6E4', '#287DA1', '#B47E00', '#DB8A06',
            '#F93822', '#C6007E', '#FFCD00', '#F1A7DC', '#FA4616', '#78D64B', '#76232F',
            '#543A3B', '#AF272F', '#F19C49', '#919D9D', '#C1C6C8', '#978C87', '#62685A',
            '#AEA8A5', '#97999B', '#566361', '#7ACC00', '#C4D600', '#BB85AB', '#C964CF',
            '#672146', '#696969'),
  party_name = c('PSDB', 'PSL', 'Republicanos', 'Progressistas', 'PTB', 'PSD', 'PV',
                 'MDB', 'DEM', 'NOVO', 'PODE', 'PL', 'PSC', 'PDT',
                 'PT', 'PCdoB', 'PSOL', 'CIDADANIA', 'PSB', 'REDE', 'PSTU',
                 'PCO', 'PCB', 'SD', 'PRTB', 'PMN', 'PTC', 'PATRI',
                 'Avante', 'UP', 'PROS', 'PPL', 'PRP', 'DC', 'PHS',
                 'PMB', 'Branco/Nulo')
)

coletivas_by_state = coletivas %>%
    count(SG_UF) %>%
    left_join(cands_by_state, 'SG_UF') %>%
    rename(n = n.x) %>%
    mutate(n_prop = n / n.y) %>%
    select(-n.y)

states = read_state() %>%
    left_join(coletivas_by_state, by = c('abbrev_state' = 'SG_UF'))

plot_state_abs = ggplot() +
    geom_sf(data = states, aes(fill = n)) +
    scale_fill_gradient(low = 'white', high = '#6c82a0')

plot_state_rel = ggplot() +
    geom_sf(data = states, aes(fill = n_prop)) +
    scale_fill_gradient(low = 'white', high = '#6c82a0')

coletivas_colored = left_join(coletivas, party_palette, c('NR_PARTIDO' = 'party'))

plot_by_party = ggplot(coletivas_colored, aes(x = fct_infreq(SG_PARTIDO), fill = party_color)) +
    geom_bar() +
    coord_flip() +
    scale_fill_identity()
