library(tidyverse)
library(lubridate)
library(fastDummies)

election_dates = tibble(
  year = c(2012, 2014, 2016, 2018, 2020, 2022),
  first_round_date = c(
    make_date(year = 2012, month = 10, day = 7),
    make_date(year = 2014, month = 10, day = 5),
    make_date(year = 2016, month = 10, day = 2),
    make_date(year = 2018, month = 10, day = 7),
    make_date(2020, 11, 15),
    make_date(2022, 10, 2)
  ),
  second_round_date = c(
    make_date(year = 2012, month = 10, day = 28),
    make_date(year = 2014, month = 10, day = 26),
    make_date(year = 2016, month = 10, day = 30),
    make_date(year = 2018, month = 10, day = 28),
    make_date(2020, 11, 29),
    make_date(2022, 10, 30)
    
  ),
  candidate_registry_date = c(
    make_date(2012, 7, 5),
    make_date(2014, 7, 5),
    make_date(2016, 8, 15),
    make_date(2018, 8, 15),
    make_date(2020, 9, 26),
    make_date(2022, 8, 15)
  )
)

votes = read_csv('resultados_2022.csv') %>%
  rename(SIGLA_UE = ue, NUMERO_CANDIDATO = numero, QTDE_VOTOS = votos_validos, CODIGO_CARGO = cargo) %>%
  mutate(ANO_ELEICAO = 2022, NUM_TURNO = 1) %>%
  select(ANO_ELEICAO, SIGLA_UE, CODIGO_CARGO, NUMERO_CANDIDATO, NUM_TURNO, QTDE_VOTOS) %>%
  filter(NUMERO_CANDIDATO != 95 & NUMERO_CANDIDATO != 96) %>%
  group_by(ANO_ELEICAO, NUM_TURNO, SIGLA_UE, CODIGO_CARGO) %>%
  mutate(qtde_all_valid = sum(QTDE_VOTOS)) %>%
  arrange(desc(QTDE_VOTOS), .by_group = T) %>%
  ungroup()

get_final_poll_list = function(p) {
  p %>%
    group_by(NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, estimulada, CD_CARGO, vv) %>%
    filter(n_distinct(scenario_id) == 1) %>%
    ungroup() %>%
    inner_join(votes, by = c(
      'year' = 'ANO_ELEICAO',
      'turno' = 'NUM_TURNO',
      'polled_UE' = 'SIGLA_UE',
      'CD_CARGO' = 'CODIGO_CARGO',
      'NUMERO_CANDIDATO' = 'NUMERO_CANDIDATO'
    )) %>%
    mutate(state = case_when(
      polled_UE == 'BR' ~ 'BR',
      nchar(polled_UE) == 2 ~ polled_UE,
      T ~ str_sub(NR_IDENTIFICACAO_PESQUISA, start = 1, end = 2)
    )) %>%
    group_by(year, turno,
             NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, CD_CARGO, estimulada, vv, scenario_id, #source,
             DT_INICIO_PESQUISA, DT_FIM_PESQUISA,  is_phone, is_fluxo, QT_ENTREVISTADO,
             first_round_date, second_round_date, is_complete, state,
             confidence_interval_final, error_final) %>%
    mutate(valid_result = result / sum(result) * 100) %>%
    mutate(pct = QTDE_VOTOS / sum(QTDE_VOTOS) * 100) %>%
    filter(sum(QTDE_VOTOS) >= 0.90 * qtde_all_valid) %>%
    mutate(undecided = 100 - sum(result)) %>%
    ungroup()
}

recent_polls_22 = read_csv('pesquisas_ncol.csv')

polls_22 = get_final_poll_list(recent_polls_22) %>%
  select(-first_round_date, -second_round_date, -candidate_registry_date) %>%
  left_join(election_dates, 'year') %>%
  rename(QT_ENTREVISTADOS = QT_ENTREVISTADO)

model_polls = polls_22 %>%
  filter(pct != 0) %>%
  mutate(p_pct = pct/100, p_valid_result = valid_result/100) %>%
  group_by(year, turno,
           NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, company_id, pretty_name,
           SG_UE, polled_UE, CD_CARGO, estimulada, vv, scenario_id,
           DT_FIM_PESQUISA, is_phone, is_fluxo, QT_ENTREVISTADOS,
           first_round_date, second_round_date, is_complete, state) %>%
  arrange(desc(valid_result), .by_group = T) %>%
  summarize(mm3 = mean(abs(valid_result - pct)),
            arzev_bw = weighted.mean(abs(log((p_pct / (1 - p_pct)) * ((1 - p_valid_result) / p_valid_result))), valid_result),
            undecided = first(undecided)) %>%
  ungroup() %>%
  mutate(election_type = case_when(
    polled_UE == 'BR' ~ 1,
    CD_CARGO == 1 & polled_UE != 'BR' ~ 2,
    CD_CARGO == 3 & nchar(polled_UE) == 2 ~ 3,
    CD_CARGO == 11 ~ 4,
    T ~ NA_real_
  )) %>%
  filter(!is.na(election_type)) %>%
  mutate(days_apart = as.numeric(
    abs(difftime(DT_FIM_PESQUISA, if_else(turno == 1, first_round_date, second_round_date), units = 'days'))
  )) %>%
  mutate(one_week_prior = days_apart <= 7) %>%
  filter((turno == 1 & days_apart <= 21) | (turno == 2 & days_apart <= 14)) %>%
  group_by(year, turno, NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, CD_CARGO, estimulada, vv) %>%
  filter(n_distinct(scenario_id) == 1) %>%
  ungroup() %>%
  filter(is_complete) %>%
  dummy_cols(select_columns = c('election_type')) %>%
  dummy_cols(select_columns = c('year')) %>%
  mutate(first_round = turno == 1) %>%
  mutate(n_adj = 1 / sqrt(pmin(QT_ENTREVISTADOS, 6000))) %>%
  dummy_cols(select_columns = c('state'))

lp_old = read_csv('late_polls.csv')
lp_old_playground = lp_old %>%
  filter(pct != 0) %>%
  mutate(p_pct = pct/100, p_valid_result = valid_result/100) %>%
  group_by(year, turno,
           NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, company_id, pretty_name,
           SG_UE, polled_UE, CD_CARGO, estimulada, vv, scenario_id,
           DT_FIM_PESQUISA, is_phone, is_fluxo, QT_ENTREVISTADOS,
           first_round_date, second_round_date, is_complete, state
  ) %>%
  arrange(desc(valid_result), .by_group = T) %>%
  summarize(mm3 = mean(abs(valid_result - pct)),
            arzev_bw = weighted.mean(abs(log((p_pct / (1 - p_pct)) * ((1 - p_valid_result) / p_valid_result))), valid_result),
            undecided = first(undecided)) %>%
  ungroup() %>%
  mutate(election_type = case_when(
    polled_UE == 'BR' ~ 1,
    CD_CARGO == 1 & polled_UE != 'BR' ~ 2,
    CD_CARGO == 3 & nchar(polled_UE) == 2 ~ 3,
    CD_CARGO == 11 ~ 4,
    T ~ NA_real_
  )) %>%
  filter(!is.na(election_type)) %>%
  mutate(days_apart = as.numeric(
    abs(difftime(DT_FIM_PESQUISA, if_else(turno == 1, first_round_date, second_round_date), units = 'days'))
  )) %>%
  mutate(one_week_prior = days_apart <= 7) %>%
  filter((turno == 1 & days_apart <= 21) | (turno == 2 & days_apart <= 14)) %>%
  group_by(year, turno, NR_IDENTIFICACAO_PESQUISA, NR_CNPJ_EMPRESA, SG_UE, polled_UE, CD_CARGO, estimulada, vv) %>%
  filter(n_distinct(scenario_id) == 1) %>%
  ungroup() %>%
  filter(is_complete) %>%
  dummy_cols(select_columns = c('election_type')) %>%
  dummy_cols(select_columns = c('year')) %>%
  mutate(first_round = turno == 1) %>%
  mutate(n_adj = 1 / sqrt(pmin(QT_ENTREVISTADOS, 6000))) %>%
  dummy_cols(select_columns = c('state'))

model_polls_last = model_polls %>%
  filter(days_apart <= 7 & DT_FIM_PESQUISA != first_round_date) %>%
  group_by(year, company_id, pretty_name, election_type, polled_UE) %>%
  arrange(desc(DT_FIM_PESQUISA), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup()

lp_old_last = lp_old_playground %>%
  filter(year %in% c(2014, 2018) & turno == 1) %>%
  filter(days_apart <= 7 & DT_FIM_PESQUISA != first_round_date) %>%
  group_by(year, company_id, pretty_name, election_type, polled_UE) %>%
  arrange(desc(DT_FIM_PESQUISA), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Tabela de Erros

save(list = c('model_polls_last', 'lp_old_last'), file = 'pesquisas.Rdata')
