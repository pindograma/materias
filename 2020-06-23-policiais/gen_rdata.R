library(tidyverse)

# NOTA: O arquivo `filiacao.csv` encontrado foi gerado através dos scripts do
# brasil.io:
# https://github.com/turicas/eleicoes-brasil/blob/master/filiacao.sh.
# 
# Ele pode ser baixado em
# http://pindograma-dados.s3.amazonaws.com/filiacao_2020/filiacao.gz
filiados = read_csv('filiacao.csv')

fpr = filiados %>% filter(uf == 'PR')
fpr2 = fpr %>% filter(is.na(data_desfiliacao) & is.na(data_cancelamento))
fpr3 = fpr2 %>%
  group_by(titulo_eleitoral) %>%
  arrange(desc(data_filiacao), .by_group = T) %>%
  filter(row_number() == 1) %>%
  ungroup()

# NOTA: O arquivo TB_RH.csv pode ser encontrado em
# http://pindograma-dados.s3.amazonaws.com/filiacao_2020/TB_RH.csv
pm = read_csv2('TB_RH.csv')
pm2 = pm %>% filter(quadro_funcional == 'PM' & situacao == 'ATIVO')

join2 = inner_join(pm2, fpr3, by = c('municipio' = 'nome_municipio', 'nome' = 'nome')) %>%
  filter(year(data_filiacao) - 16 >= ano_nasc)

dnames = filiados %>% count(uf, nome_municipio, nome) %>% filter(n > 1)


