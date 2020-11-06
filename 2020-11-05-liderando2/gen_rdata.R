library(tidyverse)
library(lubridate)

Rcpp::sourceCpp('polling_average.cpp')
source('constants.R')

global_polls = read_csv('data/preprocessed_polls.csv')

cities = read_csv('data/cities.csv') %>%
  expand_grid(year = c(2012, 2014, 2016, 2018, 2020)) %>%
  semi_join(global_polls, by = c('SG_UE', 'year')) %>%
  mutate(order = case_when(
    SG_UE == '71072' ~ 1,
    SG_UE == '60011' ~ 2,
    SG_UE == '38490' ~ 3,
    SG_UE == '13897' ~ 4,
    SG_UE == '41238' ~ 5,
    SG_UE == '02550' ~ 6,
    SG_UE == '75353' ~ 7,
    SG_UE == '25313' ~ 8,
    SG_UE == '93734' ~ 9,
    SG_UE == '04278' ~ 10,
    SG_UE == '88013' ~ 11,
    SG_UE == '09210' ~ 12,
    SG_UE == '27855' ~ 13,
    SG_UE == '90514' ~ 14,
    SG_UE == '17612' ~ 15,
    SG_UE == '12190' ~ 16,
    SG_UE == '20516' ~ 17,
    SG_UE == '31054' ~ 18,
    SG_UE == '90670' ~ 19,
    SG_UE == '00035' ~ 20,
    SG_UE == '06050' ~ 21,
    SG_UE == '81051' ~ 22,
    SG_UE == '03018' ~ 23,
    SG_UE == '01392' ~ 24,
    SG_UE == '57053' ~ 25,
    SG_UE == '73440' ~ 26,
    T ~ Inf
  ))

cities = cities %>% arrange(order)

candlist_old = read_csv('data/pindograma_candlist.csv') %>%
  mutate(short = str_replace_all(NOME_URNA_CANDIDATO, 'PROFESSOR', 'PROF. ')) %>%
  mutate(short = str_replace_all(short, '\\(.*?\\)', '')) %>%
  mutate(short = str_squish(short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(NOME_URNA_CANDIDATO = str_squish(short)) %>%
  left_join(party_palette, by = c('NUMERO_CANDIDATO' = 'party'))

candlist_20_tse = read_csv('data/pindograma_candlist_2020.csv') %>%
  mutate(short = str_replace_all(NOME_URNA_CANDIDATO, 'PROFESSOR', 'PROF. ')) %>%
  mutate(short = str_replace_all(short, '\\(.*?\\)', '')) %>%
  mutate(short = str_squish(short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(short = ifelse(nchar(short) >= 21, word(short, start = 1, end = -2), short)) %>%
  mutate(NOME_URNA_CANDIDATO = str_squish(short)) %>%
  left_join(party_palette, by = c('NUMERO_CANDIDATO' = 'party'))

fake_candlist = crossing(
  ANO_ELEICAO = c(2012, 2014, 2016, 2018, 2020),
  SIGLA_UE = cities$SG_UE,
  CODIGO_CARGO = c(1, 3, 11),
  NUM_TURNO = c(1, 2)
) %>%
  mutate(NUMERO_CANDIDATO = 99, NOME_URNA_CANDIDATO = 'BRANCOS / NULOS / OUTROS') %>%
  left_join(party_palette, by = c('NUMERO_CANDIDATO' = 'party'))

candlist = bind_rows(candlist_old, candlist_20_tse, fake_candlist)#,candlist20)

prepare_chart_data = function(yr, city, rnd, cargo = 11, mode) {
  wma_n = 5
  
  if (yr == 2020) {
    if (mode == 1) {
      polls = global_polls %>%
        filter(DT_FIM_PESQUISA >= make_date(2020, 9, 1)) %>%
        group_by(NR_IDENTIFICACAO_PESQUISA, company_id, SG_UE, polled_UE, CD_CARGO) %>%
        filter(n_distinct(estimulada) == 1 | estimulada == 1) %>%
        ungroup()
    } else {
      polls = global_polls %>%
        group_by(NR_IDENTIFICACAO_PESQUISA, company_id, SG_UE, polled_UE, CD_CARGO) %>%
        filter((DT_FIM_PESQUISA <= candidate_registry_date & estimulada == 0) | (DT_FIM_PESQUISA > candidate_registry_date & (n_distinct(estimulada) == 1 | estimulada == 1))) %>%
        ungroup()
    }
  } else {
    polls = global_polls
  }
  
  pollz = polls %>%
    filter(year == yr & polled_UE == city & CD_CARGO == cargo & turno == rnd) %>%
    mutate(applied_weight = 1) %>%
    group_by(NUMERO_CANDIDATO) %>%
    mutate(first_poll_date = min(DT_FIM_PESQUISA)) %>%
    ungroup()
  
  dat = pollz %>%
    group_by(DT_FIM_PESQUISA, NUMERO_CANDIDATO, first_round_date, second_round_date, first_poll_date) %>%
    summarize(
      day_average = weighted.mean(result, applied_weight, na.rm = T),
      day_weight = sum(applied_weight, na.rm = T)
    ) %>%
    ungroup()
  
  start_date = min(if_else(rnd == 1, min(dat$first_poll_date), first(dat$first_round_date)), today())
  end_date = min(if_else(rnd == 1, first(dat$first_round_date), first(dat$second_round_date)), today())
  
  days_df = expand_grid(
    date = seq(start_date, end_date, by = 'days'),
    NUMERO_CANDIDATO = pull(distinct(dat, NUMERO_CANDIDATO))
  ) %>%
    mutate(year = yr, CD_CARGO = cargo, polled_UE = city, turno = rnd) %>%
    left_join(dat %>% select(-first_poll_date), by = c('date' = 'DT_FIM_PESQUISA', 'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO')) %>%
    left_join(dat %>% distinct(NUMERO_CANDIDATO, first_poll_date), by = c('NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO')) %>%
    group_by(NUMERO_CANDIDATO) %>%
    mutate(go_back = case_when(
      as.numeric(date - first_poll_date) < wma_n ~ as.numeric(which.min(is.na(day_average))),
      as.numeric(date - first_poll_date) >= wma_n ~ as.numeric(row_number() - (wma_n - 1))
    )) %>%
    ungroup() %>%
    group_by(date) %>%
    mutate(day_average = ifelse(date < first_poll_date, NA, ifelse(is.na(day_average) & any(!is.na(day_average)), 0, day_average))) %>%
    mutate(day_weight = ifelse(date < first_poll_date, NA, ifelse(is.na(day_weight) & any(!is.na(day_weight)), mean(day_weight, na.rm = T), day_weight))) %>%
    ungroup()
  
  list(
    pollz,
    days_df %>%
      mutate(imputed = is.na(day_average)) %>%
      group_split(NUMERO_CANDIDATO) %>%
      map_dfr(function(x) fillEmptyDays(x, first(x$first_poll_date))) %>%
      tibble() %>%
      group_by(NUMERO_CANDIDATO) %>%
      mutate(final_average = calculateFinalAverage(day_average, day_weight, imputed, wma_n, T, 0.9, 0.8)) %>%
      ungroup()
    )
}

show_city_chart = function(yr, city, rnd, cargo = 11, mode) {
  d = prepare_chart_data(yr, city, rnd, cargo, mode)
  
  d1_with_names = d[[1]] %>%
    left_join(candlist, by = c(
      'year' = 'ANO_ELEICAO',
      'polled_UE' = 'SIGLA_UE',
      'CD_CARGO' = 'CODIGO_CARGO',
      'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO',
      'turno' = 'NUM_TURNO'
    )) %>%
    mutate(NOME_URNA_CANDIDATO = str_to_title(paste0(NOME_URNA_CANDIDATO, '  ')))
  
  d2_with_names = d[[2]] %>%
    left_join(candlist, by = c(
      'year' = 'ANO_ELEICAO',
      'polled_UE' = 'SIGLA_UE',
      'CD_CARGO' = 'CODIGO_CARGO',
      'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO',
      'turno' = 'NUM_TURNO'
    )) %>%
    mutate(NOME_URNA_CANDIDATO = ifelse(NUMERO_CANDIDATO != 99, paste0(str_to_title(NOME_URNA_CANDIDATO), ' (', party_name, ')  '), str_to_title(NOME_URNA_CANDIDATO))) %>%
    group_by(NUMERO_CANDIDATO) %>%
    mutate(NOME_URNA_CANDIDATO = ifelse(n_distinct(NOME_URNA_CANDIDATO) == 1, NOME_URNA_CANDIDATO, paste0('Candidato(a) do ', party_name, '  '))) %>%
    ungroup() %>%
    mutate(party_color = fct_rev(fct_reorder(factor(party_color), final_average, median, na.rm = T)))
  
  lbls = unique(d2_with_names$NOME_URNA_CANDIDATO)
  brks = unique(d2_with_names$party_color)
  
  plot = ggplot(d2_with_names, aes(
    x = date,
    y = final_average,
    color = party_color, group = 1,
    text = factor(paste0('<b>', str_squish(NOME_URNA_CANDIDATO), '</b>: ', round(final_average), '%'))
  )) +
    geom_line(size = 0.5) +
    geom_point(data = d1_with_names, aes(x = DT_FIM_PESQUISA, y = result, text = NA), alpha = 0.35, shape = 19, size = 1) +
    xlab('') + ylab('') +
    scale_color_identity(guide = 'legend', breaks = brks, labels = lbls) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    theme_pindograma() +
    theme(legend.title = element_blank()) +
    theme(legend.position = 'bottom') +
    theme(legend.text = element_text(size = 14))
  
  if (mode == 2) {
    crd = first(d1_with_names$candidate_registry_date)
    plot = plot +
      geom_vline(xintercept = as.numeric(crd), linetype = 'dashed', color = pg_dark_gray)
  }
  
  list(plot, d[[1]])
}

cit = cities %>% filter(year == 2020) %>% pull(SG_UE)

averages = map(cit, function(x) {
  has_recent_polls = global_polls %>%
    filter(year == 2020 & CD_CARGO == 11 & polled_UE == x) %>%
    filter(DT_FIM_PESQUISA >= make_date(2020, 9, 1)) %>%
    nrow() > 0
  
  if (!has_recent_polls) {
    tibble()
  } else {
    prepare_chart_data(2020, x, 1, cargo = 11, mode = 1)
  }
})

leading_party = map_dfr(averages, function(x) {
  if (length(x) != 2) {
    return(tibble())
  }
  
  party = x[[2]] %>%
    filter(date == max(date)) %>%
    filter(final_average >= max(final_average) - 3) %>%
    pull(NUMERO_CANDIDATO)
  
  tibble(city = rep(first(x[[2]]$polled_UE), length(party)), party = party)
})

write.csv(leading_party, 'leading_party.csv', row.names = F)

