library(tidyverse)
library(sidrar)
library(stringi)
library(treemapify)
library(sf)
library(geobr)
#dados novos

normalize_simple = function(x) {
  str_squish(toupper(stri_trans_general(str = x, id = 'Latin-ASCII')))
}

geoloc_states <- read_state() %>% select(abbrev_state, geom) %>% 
  rename(SG_UF = abbrev_state)

party_palette <- tibble(
  party_number = c(45, 17, 10, 11, 14, 55, 43,
            15, 25, 30, 19, 22, 20, 12,
            13, 65, 50, 23, 40, 18, 16,
            29, 21, 77, 28, 33, 36, 51,
            70, 80, 90, 54, 44, 27, 31,
            35, 99),
  party_color = c('#5C88DA', '#003A70', '#41748D', '#56B7E6', '#00B7FF', '#F6BE00', '#006747',
                  '#009A44', '#0857C3', '#FFA400', '#98B6E4', '#287DA1', '#B47E00', '#DB8A06',
                  '#F93822', '#C6007E', '#FFCD00', '#F1A7DC', '#FEA902', '#78D64B', '#76232F',
                  '#543A3B', '#AF272F', '#F19C49', '#919D9D', '#C1C6C8', '#978C87', '#62685A',
                  '#AEA8A5', '#97999B', '#566361', '#7ACC00', '#C4D600', '#BB85AB', '#C964CF',
                  '#672146', '#696969'),
  PARTIDO = c('PSDB', 'PSL', 'REPUBLICANOS', 'PP', 'PTB', 'PSD', 'PV',
                 'MDB', 'DEM', 'NOVO', 'PODE', 'PL', 'PSC', 'PDT',
                 'PT', 'PC do B', 'PSOL', 'CIDADANIA', 'PSB', 'REDE', 'PSTU',
                 'PCO', 'PCB', 'SOLIDARIEDADE', 'PRTB', 'PMN', 'PTC', 'PATRIOTA',
                 'AVANTE', 'UP', 'PROS', 'PPL', 'PRP', 'DC', 'PHS',
                 'PMB', 'Branco/Nulo')
)

tse_to_ibge <- read_csv("municipios_brasileiros_tse.csv",
                        col_types = c(col_character(),
                                      col_character(),
                                      col_logical(),
                                      col_character())) %>% 
  mutate(codigo_tse = str_pad(codigo_tse, 5, pad = '0'),
         codigo_ibge = as.character(codigo_ibge)) %>% 
  select(codigo_tse, codigo_ibge, capital)

pop_ibge <- get_sidra(6579, variable = 9324,
                      period = as.character(2020), geo = "City") %>%
  mutate(UF = str_sub(Município, -2, -1), Município = str_sub(Município, 1, -6)
         %>% normalize_simple()) %>% 
  rename(MUNICIPIO = Município, POPULACAO = Valor, codigo_ibge = `Município (Código)`) %>% 
  select(codigo_ibge, UF, MUNICIPIO, POPULACAO) %>% 
  left_join(tse_to_ibge, by = "codigo_ibge") %>% 
  select(codigo_ibge, codigo_tse, UF, POPULACAO)

pop_uf <- pop_ibge %>% group_by(UF) %>% summarize(pop = sum(POPULACAO))



origin <- read_csv2("./prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_doador_originario_2020_BRASIL.csv", 
                    locale = locale(encoding = "Latin1"), 
                    col_types = cols(DT_GERACAO = col_skip(), 
                                     HH_GERACAO = col_skip(),
                                     ANO_ELEICAO = col_skip(), 
                                     CD_TIPO_ELEICAO = col_skip(),
                                     NM_TIPO_ELEICAO = col_skip(), 
                                     CD_ELEICAO = col_skip(),
                                     DS_ELEICAO = col_skip(), 
                                     DT_ELEICAO = col_skip(),
                                     TP_PRESTACAO_CONTAS = col_skip(), 
                                     DT_PRESTACAO_CONTAS = col_skip(), 
                                     NR_CPF_CNPJ_DOADOR_ORIGINARIO = col_skip(), 
                                     NM_DOADOR_ORIGINARIO = col_skip(), 
                                     NM_DOADOR_ORIGINARIO_RFB = col_skip(), 
                                     TP_DOADOR_ORIGINARIO = col_character(), 
                                     CD_CNAE_DOADOR_ORIGINARIO = col_skip(), 
                                     DS_CNAE_DOADOR_ORIGINARIO = col_skip(), 
                                     DT_RECEITA = col_skip()), trim_ws = TRUE)

receitas <- read_csv2("./prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_2020_BRASIL.csv", 
                      locale = locale(encoding = "Latin1"),
                      col_types = cols(DT_GERACAO = col_skip(), 
                                       HH_GERACAO = col_skip(),
                                       ANO_ELEICAO = col_skip(), 
                                       CD_TIPO_ELEICAO = col_skip(),
                                       NM_TIPO_ELEICAO = col_skip(), 
                                       CD_ELEICAO = col_skip(), DS_ELEICAO = col_skip(), 
                                       DT_ELEICAO = col_skip(),
                                       TP_PRESTACAO_CONTAS = col_skip(), 
                                       DT_PRESTACAO_CONTAS = col_skip(), 
                                       NR_CNPJ_PRESTADOR_CONTA = col_skip(), 
                                       SQ_CANDIDATO = col_skip(), 
                                       NR_CPF_CANDIDATO = col_skip(),
                                       NR_CPF_VICE_CANDIDATO = col_skip(), 
                                       NM_PARTIDO = col_skip(),
                                       CD_FONTE_RECEITA = col_skip(), 
                                       DS_FONTE_RECEITA = col_skip(),
                                       CD_ORIGEM_RECEITA = col_skip(), 
                                       CD_NATUREZA_RECEITA = col_skip(), 
                                       DS_NATUREZA_RECEITA = col_skip(), 
                                       CD_ESPECIE_RECEITA = col_skip(), 
                                       DS_ESPECIE_RECEITA = col_skip(), 
                                       CD_CNAE_DOADOR = col_skip(),
                                       DS_CNAE_DOADOR = col_skip(), 
                                       NR_CPF_CNPJ_DOADOR = col_skip(), 
                                       NM_DOADOR = col_skip(),
                                       NM_DOADOR_RFB = col_skip(), 
                                       CD_ESFERA_PARTIDARIA_DOADOR = col_skip(), 
                                       DS_ESFERA_PARTIDARIA_DOADOR = col_skip(), 
                                       SG_UF_DOADOR = col_skip(),
                                       CD_MUNICIPIO_DOADOR = col_skip(), 
                                       NM_MUNICIPIO_DOADOR = col_skip(), 
                                       SQ_CANDIDATO_DOADOR = col_skip(), 
                                       NR_CANDIDATO_DOADOR = col_skip(), 
                                       CD_CARGO_CANDIDATO_DOADOR = col_skip(), 
                                       DS_CARGO_CANDIDATO_DOADOR = col_skip(), 
                                       NR_PARTIDO_DOADOR = col_skip(),
                                       SG_PARTIDO_DOADOR = col_skip()),
                      trim_ws = TRUE)

candidatos <- receitas %>%
  select(NM_CANDIDATO, SQ_PRESTADOR_CONTAS, NM_UE, SG_UE, SG_PARTIDO) %>% 
  distinct(SQ_PRESTADOR_CONTAS, .keep_all = T)

ori_cand <- candidatos %>%
  right_join(origin, by = "SQ_PRESTADOR_CONTAS") %>%
  filter(DS_RECEITA == "Recursos de Financiamento Coletivo") %>%
  select(-SQ_RECEITA)

finc_med <- ori_cand %>%
  group_by(SG_PARTIDO) %>% 
  summarise(mean(VR_RECEITA)) %>%
  rename("meanFINCOL" = "mean(VR_RECEITA)")

rec <- receitas %>%
  select(-NR_RECIBO_DOACAO, -NR_DOCUMENTO_DOACAO, -DT_RECEITA,
         -DS_RECEITA, -NM_PARTIDO_DOADOR) %>% 
  mutate(
  DS_ORIGEM_RECEITA = ifelse(DS_ORIGEM_RECEITA =="Recursos de pessoas físicas", "OUTROS",
                      ifelse(DS_ORIGEM_RECEITA =="Recursos de outros candidatos", "OUTROS",
                      ifelse(DS_ORIGEM_RECEITA =="Recursos de partido político", "PART", 
                      ifelse(DS_ORIGEM_RECEITA =="Recursos de Financiamento Coletivo", "FINCOL",
                      ifelse(DS_ORIGEM_RECEITA =="Rendimentos de aplicações financeiras", "OUTROS", 
                      ifelse(DS_ORIGEM_RECEITA =="Comercialização de bens ou realização de eventos", "OUTROS", 
                      ifelse(DS_ORIGEM_RECEITA =="Recursos de origens não identificadas", "OUTROS",
                      ifelse(DS_ORIGEM_RECEITA =="Recursos próprios", "OUTROS",
                      ifelse(DS_ORIGEM_RECEITA == "Recursos de pessoas físicas", "OUTROS", 
                      ifelse(DS_ORIGEM_RECEITA == "Doações pela Internet", "OUTROS", DS_ORIGEM_RECEITA))))))))))) %>% 
  group_by(NR_CANDIDATO, NM_CANDIDATO, SG_UE, SQ_PRESTADOR_CONTAS,SG_UF,
           NM_UE, SG_PARTIDO, ST_TURNO, CD_CARGO, DS_CARGO, DS_ORIGEM_RECEITA) %>%
  summarize(valor = sum(VR_RECEITA)) %>% 
  ungroup() %>% 
  pivot_wider(values_from = valor, names_from = DS_ORIGEM_RECEITA, values_fill = 0) %>% 
  mutate(TOTAL = FINCOL + OUTROS + PART) %>% 
  mutate(NÃO_FINCOL = PART + OUTROS) %>% 
  mutate(PROP_FINCOL = FINCOL / TOTAL) %>% 
  mutate(PROP_PART = PART / TOTAL) %>% 
  mutate(PROP_OUTROS = OUTROS / TOTAL) %>% 
  mutate(PROP_NÃO_FINCOL = NÃO_FINCOL / TOTAL) %>% 
  left_join(pop_ibge, by = c("SG_UE" = "codigo_tse"))

cand20_raw <- read_csv2("../partidos_em_numeros/data/consulta_cand_2020_BRASIL.csv",
                        locale = locale(encoding = "Latin1"))

resultados20_raw <- read_csv("../partidos_em_numeros/data/resultados_prefeito.csv") %>%
  mutate(CARGO = "PREFEITO") %>% 
  bind_rows(read_csv("../partidos_em_numeros/data/resultados_vereador.csv") %>% mutate(
    urnas_apuradas = as.character(urnas_apuradas),
    abstencoes_pct = as.character(abstencoes_pct),
    nulos_pct = as.character(nulos_pct),
    CARGO = "VEREADOR"
  )) %>% mutate(SEG_TURNO = F) %>%  
  bind_rows(read_csv("../partidos_em_numeros/data/resultados_prefeito_final.csv") %>%
  mutate(CARGO = "PREFEITO",
         SEG_TURNO = T,
         urnas_apuradas = as.character(urnas_apuradas),
         abstencoes_pct = as.character(abstencoes_pct),
         nulos_pct = as.character(nulos_pct),
         brancos_pct = as.character(brancos_pct),
         pct_votos_validos = as.character(pct_votos_validos)))

resultados20 <- resultados20_raw %>% select(codigo_cidade_tse, CARGO,
                                            nome, numero, votos_validos,
                                            eleito, SEG_TURNO) %>% 
  rename(SG_UE = codigo_cidade_tse, NM_URNA_CANDIDATO = nome)


cand20 <- cand20_raw %>%
  filter(CD_DETALHE_SITUACAO_CAND %in% c(8, 2, 16, 17)) %>% 
  select(DS_CARGO, SG_UF, NM_UE, SG_UE, NM_CANDIDATO, NM_URNA_CANDIDATO, NM_PARTIDO,
         NR_CPF_CANDIDATO, SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DS_GENERO,
         DS_COR_RACA, NR_IDADE_DATA_POSSE) %>%
  mutate(NM_UE = normalize_simple(NM_UE)) %>% 
  rename(CARGO = DS_CARGO, UF = SG_UF, MUNICIPIO = NM_UE, PARTIDO = SG_PARTIDO,
         LEGENDA = DS_COMPOSICAO_COLIGACAO, GENERO = DS_GENERO,
         RACA = DS_COR_RACA, IDADE = NR_IDADE_DATA_POSSE) %>% 
  left_join(resultados20, by = c("SG_UE", "NM_URNA_CANDIDATO", "CARGO")) %>% 
  group_by(SG_UE, NM_URNA_CANDIDATO) %>% 
  arrange(UF, MUNICIPIO, NM_URNA_CANDIDATO, desc(SEG_TURNO)) %>% 
  distinct(NM_URNA_CANDIDATO, .keep_all = T) %>% ungroup()

resumo_partidos <- cand20 %>% filter(CARGO != "VICE-PREFEITO") %>%
  count(PARTIDO) %>% rename(cand_total = n) %>%  
  left_join(rec %>% filter(FINCOL > 0, CD_CARGO != 12) %>%
              group_by(SG_PARTIDO) %>% 
              summarize(cand_fincol = n(), valor = sum(FINCOL)),
            by = c("PARTIDO" = "SG_PARTIDO")) %>% 
  mutate(cand_fincol = ifelse(is.na(cand_fincol), 0, cand_fincol),
         valor = ifelse(is.na(valor), 0, valor)) %>% 
  mutate(prop = cand_fincol / cand_total) %>% 
  left_join(finc_med, by = c("PARTIDO" = "SG_PARTIDO")) %>% 
  left_join(party_palette) %>% 
  mutate(
    PARTIDO = ifelse(PARTIDO == "PATRIOTA", "PATRI", ifelse(
      PARTIDO == "CIDADANIA", "CID", ifelse(
        PARTIDO == "PC do B", "PCdoB", ifelse(
          PARTIDO == "REPUBLICANOS", "REP", ifelse(
            PARTIDO == "SOLIDARIEDADE", "SD", PARTIDO
          ))))),
      meanFINCOL = ifelse(is.na(meanFINCOL), 0, meanFINCOL)
    )

resumo_uf <- rec %>% filter(FINCOL > 0) %>% group_by(SG_UF) %>% summarize(n = n(), valor = sum(FINCOL)) %>%
  left_join(pop_uf, by = c("SG_UF" = "UF")) %>% mutate(apc = valor / pop) %>% 
  left_join(geoloc_states)

save(list = c('resumo_uf', 'resumo_partidos'), file = 'fin.Rdata')

# Candidatos que tiveram financiamento coletivo por partido
ggplot(resumo_partidos, aes(reorder(PARTIDO, cand_fincol), cand_fincol, fill = party_color)) +
  geom_bar(stat="identity") +
  scale_fill_identity()+
  geom_text(aes(label = scales::percent(prop, accuracy = .1)),
            color = "black", position = position_stack(vjust = .5))+
  theme_pindograma()+
  coord_flip()

# Montante originiado de financiamento coletivo por partido
ggplot(resumo_partidos, aes(reorder(PARTIDO, valor), valor, fill = party_color)) +
  geom_hline(aes(yintercept = 4716573), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_hline(aes(yintercept = 2000000), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_hline(aes(yintercept = 1000000), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_hline(aes(yintercept = 500000), color = pg_blue, size = .4, linetype = 'dashed')+
  scale_fill_identity()+
  geom_bar(stat="identity") +
  scale_y_continuous(
    labels = scales::label_number_si(prefix = "R$", big.mark = ".", decimal.mark = ",", accuracy = .1),
    breaks = c(0, 1000000, 2000000, 4716573))+
  labs(title = "Valor arrecadado em financiamento coletivo por partido")+
  theme_pindograma()+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  coord_flip(ylim = c(100000,5000000))

# Doação média por partido
ggplot(resumo_partidos, aes(reorder(PARTIDO, meanFINCOL), meanFINCOL, fill = party_color)) +
  geom_hline(aes(yintercept = 400), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_hline(aes(yintercept = 300), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_hline(aes(yintercept = 200), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_hline(aes(yintercept = 100), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_hline(aes(yintercept = 0), color = pg_blue, size = .4, linetype = 'dashed')+
  geom_bar(stat="identity") +
  geom_hline(aes(yintercept = 131.4742), color = "grey10", size = .8, linetype = 'dashed')+
  annotate('text', label = 'Doação média',
           family = 'Fantasque', color = "grey10",
           x = 4, y = 161, size = 4.5)+
  scale_fill_identity()+
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$", decimal.mark = ",", big.mark = "."))+
  labs(title = "Valor médio das doações por partido")+
  theme_pindograma()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14))+
  coord_flip()

# Candidatos que tiveram financiamento coletivo por estado
ggplot(resumo_uf, aes(reorder(SG_UF, n), n))+
  geom_bar(stat="identity", fill = pg_blue)+
  geom_text(aes(label = n), position = position_stack(vjust = .8))+
  theme_pindograma()+
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
  )

# Montante originado de financiamento coletivo por UF
rec %>% group_by(SG_UF) %>% summarize(valor = sum(FINCOL)) %>% 
  ggplot(aes(reorder(SG_UF, valor), valor)) +
  geom_bar(stat="identity")

# Arrecadação corrigida pela população
  ggplot(resumo_uf, aes(reorder(SG_UF, apc), apc)) +
  geom_bar(stat="identity")

# Scatterplot populacao vs arrecadacao por candidato
rec %>% mutate(has_fincol = FINCOL > 0) %>% 

# Scatterplot populacao vs proporção da arrecadacao originada de fin col
rec %>%
  ggplot(aes(POPULACAO, PROP_FINCOL)) +
  geom_point(alpha = .3) +
  scale_x_log10() +
  #scale_y_log10() +
  geom_smooth()

ggplot(resumo_uf)+
  geom_sf(aes(geometry = geom, fill = n/pop), lwd = 0)+
  scale_fill_distiller(palette = "Oranges", direction = 1)+
  theme_void()+
  theme(legend.position = "none")

ggplot(resumo_uf)+
  geom_sf(aes(geometry = geom, fill = apc), lwd = 0)+
  scale_fill_distiller(
    palette = "YlGn", direction = 1,
    labels = scales::dollar_format(prefix = "R$", decimal.mark = ","),
    breaks = c(0.02, 0.06, 0.1, 0.14))+
  labs(title = "Valor proporcional arrecadado em financiamento coletivo",
       fill = "Doação per capita")+
  theme_pindograma()+
  theme(
    axis.line.x = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.subtitle = element_text(size = 14),
    panel.background = element_blank()
  )

