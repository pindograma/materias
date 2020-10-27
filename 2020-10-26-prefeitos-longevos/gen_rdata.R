library(tidyverse)
library(cepespR)
library(sidrar)
library(stringi)
library(geobr)

normalize_simple = function(x) {
  str_squish(toupper(stri_trans_general(str = x, id = 'Latin-ASCII')))
}

geoloc_states <- read_state() %>% select(abbrev_state, geom) %>% 
  rename(UF = abbrev_state)

geoloc_cities <- read_municipality() %>%
  select(abbrev_state, name_muni, geom) %>% 
  mutate(name_muni = normalize_simple(name_muni)) %>% 
  rename(UF = abbrev_state, MUNICIPIO = name_muni)

party_palette <- tibble(
  party_number = c(45, 17, 10, 11, 14, 55, 43,
            15, 25, 30, 19, 22, 20, 12,
            13, 65, 50, 23, 40, 18, 16,
            29, 21, 77, 28, 33, 36, 51,
            70, 80, 90, 54, 44, 27, 31,
            35, 99),
  party_color = c('#5C88DA', '#003A70', '#41748D', '#56B7E6', '#00B7FF', '#F6BE00', '#006747',
                  '#009A44', '#0857C3', '#FFA400', '#98B6E4', '#287DA1', '#B47E00', '#DB8A06',
                  '#F93822', '#C6007E', '#FFCD00', '#F1A7DC', '#FA4616', '#78D64B', '#76232F',
                  '#543A3B', '#AF272F', '#F19C49', '#919D9D', '#C1C6C8', '#978C87', '#62685A',
                  '#AEA8A5', '#97999B', '#566361', '#7ACC00', '#C4D600', '#BB85AB', '#C964CF',
                  '#672146', '#696969'),
  party_name = c('PSDB', 'PSL', 'REPUBLICANOS', 'PP', 'PTB', 'PSD', 'PV',
                 'MDB', 'DEM', 'NOVO', 'PODE', 'PL', 'PSC', 'PDT',
                 'PT', 'PC do B', 'PSOL', 'CIDADANIA', 'PSB', 'REDE', 'PSTU',
                 'PCO', 'PCB', 'SOLIDARIEDADE', 'PRTB', 'PMN', 'PTC', 'PATRIOTA',
                 'AVANTE', 'UP', 'PROS', 'PPL', 'PRP', 'DC', 'PHS',
                 'PMB', 'Branco/Nulo')
)

#get mayors elected in 1996 (not available in cepespR)
eleicao96 <- read_csv(
  "./data/eleitos_1turno_96.csv", locale = locale(encoding = "Latin1")
) %>%
  bind_rows(read_csv(
    "./data/eleitos_2turno_96.csv", locale = locale(encoding = "Latin1")
  )) %>% 
  mutate(ANO_ELEICAO = 1996) %>%
  rename(
    MUNICIPIO = Município,
    PARTIDO = Partido,
    CANDIDATO = Candidato
  ) %>% 
  select(ANO_ELEICAO, UF, MUNICIPIO, CANDIDATO, PARTIDO)

#get mayors elected since 2000
since2000 <- get_candidates(
  year = '2000, 2004, 2008, 2012, 2016',
  position = 'Prefeito', only_elected = T
) %>% 
  rename(
    MUNICIPIO = DESCRICAO_UE,
    UF = SIGLA_UF,
    PARTIDO = SIGLA_PARTIDO,
    CANDIDATO = NOME_CANDIDATO,
    NOME_URNA = NOME_URNA_CANDIDATO,
    CPF = CPF_CANDIDATO,
    OCUPACAO = DESCRICAO_OCUPACAO,
    SEXO = DESCRICAO_SEXO,
    IDADE = IDADE_DATA_ELEICAO
  ) %>% 
  select(
    ANO_ELEICAO, UF, MUNICIPIO, CANDIDATO, NOME_URNA,
    CPF, PARTIDO, OCUPACAO, SEXO, IDADE, COMPOSICAO_LEGENDA
  ) 
#get 2020 candidates
cand2020 <- read_csv2(
  "./data/consulta_cand_2020_BRASIL.csv",
  locale = locale(encoding = "Latin1")
) %>% 
  filter(
    CD_CARGO == 11,
    CD_DETALHE_SITUACAO_CAND %in% c(8, 2, 16, 17)
  ) %>% 
  select(
    ANO_ELEICAO, SG_UF, NM_UE, NM_CANDIDATO, NM_URNA_CANDIDATO,
    NR_CPF_CANDIDATO, SG_PARTIDO, DS_GENERO, DS_COR_RACA,
    DS_COMPOSICAO_COLIGACAO, DS_OCUPACAO, NR_IDADE_DATA_POSSE
  ) %>% 
  rename(
    MUNICIPIO = NM_UE,
    UF = SG_UF,
    PARTIDO = SG_PARTIDO,
    CANDIDATO = NM_CANDIDATO,
    NOME_URNA = NM_URNA_CANDIDATO,
    CPF = NR_CPF_CANDIDATO,
    OCUPACAO = DS_OCUPACAO,
    SEXO = DS_GENERO,
    RACA = DS_COR_RACA,
    IDADE = NR_IDADE_DATA_POSSE,
    COMPOSICAO_LEGENDA = DS_COMPOSICAO_COLIGACAO
  ) %>% 
  mutate(
    NOME = normalize_simple(CANDIDATO),
    MUNICIPIO = normalize_simple(MUNICIPIO)
  ) 

#compile municipal indicators for analysis (population, GDP per capita, IDEB)
pop_ibge <- get_sidra(
  6579, variable = 9324, period = as.character(2020), geo = "City"
) %>%
  mutate(
    UF = str_sub(Município, -2, -1),
    Município = str_sub(Município, 1, -6) %>% normalize_simple()
  ) %>% 
  select(UF, Município, Valor) %>% 
  rename(MUNICIPIO = Município, POPULACAO = Valor)

pib_municipios <- read_csv("./data/pib_municipios.csv") %>% 
  filter(Ano == 2017) %>%
  select(
    `Nome da Grande Região`, `Sigla da Unidade da Federação`,
    `Nome do Município`, `PIB per capita`, `Principal atividade`
  ) %>% 
  rename(
    UF = `Sigla da Unidade da Federação`,
    REGIAO = `Nome da Grande Região`,
    PPC = `PIB per capita`,
    ATIVIDADE_M = `Principal atividade`,
    MUNICIPIO = `Nome do Município`
  ) %>% 
  mutate(MUNICIPIO = normalize_simple(MUNICIPIO))

firjan_raw <- read_csv("./data/Ranking-IFGF-2019_geral.csv") %>% 
  rename(MUNICIPIO = Município) %>% 
  mutate(MUNICIPIO = normalize_simple(MUNICIPIO))

indicadores <- pop_ibge %>%
  left_join(pib_municipios, by = c("UF", "MUNICIPIO")) %>% 
  left_join(firjan_raw, by = c('UF', 'MUNICIPIO'))

#combine for full list
prefeitos_dup <- since2000 %>% 
  bind_rows(eleicao96) %>% 
  mutate(
    NOME = normalize_simple(CANDIDATO),
    MUNICIPIO = normalize_simple(MUNICIPIO),
    dummy_20 = CPF %in% cand2020$CPF
  ) %>% 
  left_join(indicadores, by = c("UF", "MUNICIPIO")) %>% 
  group_by(UF, MUNICIPIO, NOME) %>% mutate(VEZES_NOME = n()) %>%
  ungroup() %>% group_by(CPF) %>%
  mutate(VEZES_CPF = ifelse(is.na(CPF) | CPF == "#NULO#", 1, n())) %>%
  ungroup() %>%
  mutate(
    VEZES = ifelse(VEZES_CPF > VEZES_NOME, VEZES_CPF, VEZES_NOME),
    target = dummy_20 & VEZES > 2,
    PARTIDO = ifelse(
      PARTIDO == "PMDB", "MDB",
      ifelse(PARTIDO == "PFL", "DEM",
      ifelse(PARTIDO == "PRB", "REPUBLICANOS", 
      ifelse(PARTIDO == "SD", "SOLIDARIEDADE", 
      ifelse(PARTIDO == "PPS", "CIDADANIA",
      ifelse(PARTIDO == "PTN", "PODE", 
      ifelse(PARTIDO == "PEN", "PATRI", 
      ifelse(PARTIDO == "PT do B", "PODE",
      ifelse(PARTIDO == "PR", "PL",
      ifelse(PARTIDO == "PPB", "PP", PARTIDO))))))))))) 

prefeitos <- prefeitos_dup %>% 
  group_by(UF, MUNICIPIO, NOME) %>% 
  filter(ANO_ELEICAO == max(ANO_ELEICAO)) %>% 
  ungroup() %>%
  group_by(CPF) %>% 
  filter(is.na(CPF) | CPF == "#NULO#" | ANO_ELEICAO == max(ANO_ELEICAO))%>% 
  ungroup()

candidatos <- cand2020 %>% left_join(
  select(prefeitos, CPF, target, VEZES_CPF, VEZES_NOME, VEZES), by = "CPF"
  ) %>% 
  mutate(
    VEZES = ifelse(is.na(VEZES), 0, VEZES), 
    target = ifelse(is.na(target), FALSE, target)
    ) %>% 
  left_join(pop_ibge, by=c('UF','MUNICIPIO'))

reeleicao16 <- bind_rows(
  get_candidates(year='2012', position='Prefeito', only_elected=T),
  get_candidates(year='2016', position='Prefeito', only_elected=F) %>% 
    mutate(DESPESA_MAX_CAMPANHA = as.character(DESPESA_MAX_CAMPANHA)),
) %>% 
  filter(DESC_SIT_TOT_TURNO != '2º TURNO') %>% 
  group_by(CPF_CANDIDATO) %>% 
  distinct(CPF_CANDIDATO, ANO_ELEICAO, .keep_all = T) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(SITUACAO = ANO_ELEICAO == 2016 & n > 1) %>% 
  mutate(
    REELEITO = ANO_ELEICAO == 2016 & n > 1 & DESC_SIT_TOT_TURNO == 'ELEITO'
  )

prop_cand_reeleicao <- 
  sum(reeleicao16$SITUACAO)/sum(reeleicao16$ANO_ELEICAO == 2016)
prop_suc_reeleicao <- 
  sum(reeleicao16$REELEITO)/sum(reeleicao16$SITUACAO)
prop_reeleito_eleito <- 
  sum(reeleicao16$REELEITO)/sum(
    reeleicao16$ANO_ELEICAO == 2016 & reeleicao16$DESC_SIT_TOT_TURNO == 'ELEITO')
prop_prefeitas_2016 <- 
  (
    reeleicao16 %>%
     filter(CODIGO_SEXO == 4, ANO_ELEICAO == 2016, DESC_SIT_TOT_TURNO == "ELEITO") %>%
     count()
    ) / (
       reeleicao16 %>%
         filter(ANO_ELEICAO == 2016, DESC_SIT_TOT_TURNO == "ELEITO") %>%
         count()
       )
prop_homens_2016 <- 
  (
    reeleicao16 %>%
     filter(CODIGO_SEXO == 2, ANO_ELEICAO == 2016, DESC_SIT_TOT_TURNO == "ELEITO") %>%
     count()
    ) / (
       reeleicao16 %>%
         filter(ANO_ELEICAO == 2016, DESC_SIT_TOT_TURNO == "ELEITO") %>%
         count()
       )

save(
  list = c( "candidatos", "prefeitos", "prefeitos_dup","prop_homens_2016",
            "prop_prefeitas_2016", "prop_cand_reeleicao", "prop_suc_reeleicao",
            "prop_reeleito_eleito", "geoloc_cities", "geoloc_states", "party_palette"),
  file = "./data/prefeitos_longevos.Rdata")



