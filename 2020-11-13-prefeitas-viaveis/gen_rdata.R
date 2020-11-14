library(tidyverse)
library(cepespR)

load("./data/agregador.Rdata")

all_12_16 <- get_candidates(year = "2012, 2016", position = "Mayor", only_elected = F)
all_20_raw <- read_csv2("./data/consulta_cand_2020_BRASIL.csv",
                    locale = locale(encoding = "Latin1"))

all_12 <- all_12_16 %>% 
  filter(
    ANO_ELEICAO == 2012,
    DESCRICAO_ELEICAO == "ELEIÇÃO MUNICIPAL 2012"
  ) %>% 
  filter(SIGLA_UE %in% viables_12_p$city,
         COD_SITUACAO_CANDIDATURA %in% c(2,16,17)) %>% 
  group_by(CPF_CANDIDATO) %>% filter(NUM_TURNO == max(NUM_TURNO)) %>%
  ungroup() %>%
  mutate(
    viable_p = CPF_CANDIDATO %in% viables_12_p$CPF_CANDIDATO,
    viable_t = CPF_CANDIDATO %in% viables_12$CPF_CANDIDATO
    ) %>%  
  mutate(viable = viable_p | viable_t, elect = DESC_SIT_TOT_TURNO == "ELEITO") %>% 
  distinct(CPF_CANDIDATO, ANO_ELEICAO, .keep_all = T)  

all_16 <- all_12_16 %>%
  filter(
    ANO_ELEICAO == 2016,
    DESCRICAO_ELEICAO == "Eleições Municipais 2016"
  ) %>% 
  filter(SIGLA_UE %in% viables_16$city,
         COD_SITUACAO_CANDIDATURA %in% c(2,16,17)) %>% 
  group_by(CPF_CANDIDATO) %>% filter(NUM_TURNO == max(NUM_TURNO)) %>%
  ungroup() %>% mutate(
    viable_p = CPF_CANDIDATO %in% viables_16_p$CPF_CANDIDATO,
    viable_t = CPF_CANDIDATO %in% viables_16$CPF_CANDIDATO) %>% 
  mutate(viable = viable_p | viable_t, elect = DESC_SIT_TOT_TURNO == "ELEITO") %>% 
  distinct(CPF_CANDIDATO, ANO_ELEICAO, .keep_all = T)

all_20 <- all_20_raw %>% 
  filter(DS_CARGO == "PREFEITO", SG_UE %in% viables_20$city,
         CD_DETALHE_SITUACAO_CAND %in% c(2,16,17) ) %>%
  mutate(
    viable_p = NR_CPF_CANDIDATO %in% viables_20_p$NR_CPF_CANDIDATO,
    viable_t = NR_CPF_CANDIDATO %in% viables_20$NR_CPF_CANDIDATO
  ) %>% 
  mutate(viable = viable_p & viable_t)

compet <- tibble(
  ANO = c(
    2012, 2012, 2012, 2012, 2012, 2012,
    2016, 2016, 2016, 2016, 2016, 2016, 
    2020, 2020, 2020, 2020, 2020, 2020
  ),
  GRUPO = c(
    "Qualquer", "Competitiva", "Eleita", "Qualquer", "Competitiva", "Eleita",
    "Qualquer", "Competitiva", "Eleita", "Qualquer", "Competitiva", "Eleita",
    "Qualquer", "Competitiva", "Eleita", "Qualquer", "Competitiva", "Eleita"
  ),
  SEXO = c(
    "Masculina", "Masculina", "Masculina", "Feminina", "Feminina", "Feminina",
    "Masculina", "Masculina", "Masculina", "Feminina", "Feminina", "Feminina",
    "Masculina", "Masculina", "Masculina", "Feminina", "Feminina", "Feminina"
  ),
  n = c(
    all_12 %>% filter(DESCRICAO_SEXO == "MASCULINO") %>% count() %>% .$n,
    all_12 %>% filter(DESCRICAO_SEXO == "MASCULINO", viable) %>% count() %>% .$n,
    all_12 %>% filter(DESCRICAO_SEXO == "MASCULINO", elect) %>% count() %>% .$n,
    all_12 %>% filter(DESCRICAO_SEXO == "FEMININO") %>% count() %>% .$n,
    all_12 %>% filter(DESCRICAO_SEXO == "FEMININO", viable) %>% count() %>% .$n,
    all_12 %>% filter(DESCRICAO_SEXO == "FEMININO", elect) %>% count() %>% .$n,
    all_16 %>% filter(DESCRICAO_SEXO == "MASCULINO") %>% count() %>% .$n,
    all_16 %>% filter(DESCRICAO_SEXO == "MASCULINO", viable) %>% count() %>% .$n,
    all_16 %>% filter(DESCRICAO_SEXO == "MASCULINO", elect) %>% count() %>% .$n,
    all_16 %>% filter(DESCRICAO_SEXO == "FEMININO") %>% count() %>% .$n,
    all_16 %>% filter(DESCRICAO_SEXO == "FEMININO", viable) %>% count() %>% .$n,
    all_16 %>% filter(DESCRICAO_SEXO == "FEMININO", elect) %>% count() %>% .$n,
    all_20 %>% filter(DS_GENERO == "MASCULINO") %>% count() %>% .$n,
    all_20 %>% filter(DS_GENERO == "MASCULINO", viable) %>% count() %>% .$n,
    0,
    all_20 %>% filter(DS_GENERO == "FEMININO") %>% count() %>% .$n,
    all_20 %>% filter(DS_GENERO == "FEMININO", viable) %>% count() %>% .$n,
    0
  )) 

compet <-  compet %>% mutate(total = ifelse(
  ANO == 2020, all_20 %>% count() %>% .$n,
  ifelse(ANO == 2016, all_16 %>% count() %>% .$n,
         all_12 %>% count() %>% .$n))) %>% 
  mutate(prop = round(100*n/total, digits = 1)) %>% 
  group_by(ANO, GRUPO) %>%
  mutate(prop_g = round(100*n/sum(n), digits = 1)) %>%
  ungroup()

eleitos_sexo <- all_12_16 %>% filter(
  COD_SITUACAO_CANDIDATURA %in% c(2,16,17),
  DESCRICAO_ELEICAO == "Eleições Municipais 2016" | DESCRICAO_ELEICAO == "ELEIÇÃO MUNICIPAL 2012"
) %>% rename(ANO = ANO_ELEICAO, SEXO = DESCRICAO_SEXO) %>% 
  mutate(
    Eleita = DESC_SIT_TOT_TURNO == "ELEITO"
  ) %>%
  distinct(CPF_CANDIDATO, ANO, .keep_all = T) %>%  
  count(ANO, SEXO, Eleita) %>% 
  mutate(
    Eleita = ifelse(is.na(Eleita), "Concorrendo", ifelse(
      Eleita, "Eleita", "Não Eleita")),
    SEXO = ifelse(SEXO == "MASCULINO", "Masculino", "Feminino")
  ) %>% 
  group_by(ANO, SEXO) %>%
  mutate(
    total = sum(n),
  ) 

n_mulheres_2020 <- all_20_raw %>% 
  filter(DS_GENERO == "FEMININO",
         DS_CARGO == "PREFEITO" | DS_CARGO == "VEREADOR") %>% count() %>%
  .$n

prop_2012 <- all_12_16 %>% filter(ANO_ELEICAO == 2012) %>%
  count(DESCRICAO_SEXO) %>% mutate(prop = 100*n / sum(n))

prop_2016 <- all_12_16 %>% filter(ANO_ELEICAO == 2016) %>%
  count(DESCRICAO_SEXO) %>% mutate(prop = 100*n / sum(n))

prop_2020 <- all_20_raw %>% filter(DS_CARGO == "PREFEITO") %>%
  count(DS_GENERO) %>% mutate(prop = 100*n / sum(n))

prop_f_12 <-  prop_2020 %>% filter(DS_GENERO == "FEMININO") %>% .$prop
prop_f_16 <-  prop_2016 %>% filter(DESCRICAO_SEXO == "FEMININO") %>% .$prop
prop_f_20 <-  prop_2012 %>% filter(DESCRICAO_SEXO == "FEMININO") %>% .$prop

n_mun_12 <- all_12 %>% count(SIGLA_UE) %>% count() %>% .$n
n_mun_16 <- all_16 %>% count(SIGLA_UE) %>% count() %>% .$n
n_mun_20 <- all_20 %>% count(SG_UE) %>% count() %>% .$n

n_cand_12 <- all_12 %>% count() %>% .$n
n_cand_16 <- all_16 %>% count() %>% .$n
n_cand_20 <- all_20 %>% count() %>% .$n

n_comp_16 <- all_16 %>% filter(viable) %>% count() %>% .$n 
n_comp_20 <- all_20 %>% filter(viable) %>% count() %>% .$n 

m_comp_12 <- compet %>%
  filter(SEXO == "Masculina", GRUPO == "Competitiva", ANO == 2012)

m_comp_16 <- compet %>%
  filter(SEXO == "Masculina", GRUPO == "Competitiva", ANO == 2016)

m_comp_20 <- compet %>%
  filter(SEXO == "Masculina", GRUPO == "Competitiva", ANO == 2020)

f_comp_12 <- compet %>%
  filter(SEXO == "Feminina", GRUPO == "Competitiva", ANO == 2012)

f_comp_16 <- compet %>%
  filter(SEXO == "Feminina", GRUPO == "Competitiva", ANO == 2016)

f_comp_20 <- compet %>%
  filter(SEXO == "Feminina", GRUPO == "Competitiva", ANO == 2020)

eleitas_12 <- eleitos_sexo %>% filter(SEXO == "Feminino", Eleita == "Eleita", ANO == 2012)
eleitas_16 <- eleitos_sexo %>% filter(SEXO == "Feminino", Eleita == "Eleita", ANO == 2016)

save(list = c('n_mulheres_2020', 'prop_f_20', 'prop_f_16', 'prop_f_12',
              'n_mun_12', 'n_mun_16', 'n_mun_20', 'compet',
              'n_cand_12', 'n_cand_16', 'n_cand_20',
              'm_comp_12', 'm_comp_16', 'm_comp_20', 'n_comp_16',
              'f_comp_12', 'f_comp_16', 'f_comp_20', 'n_comp_20',
              'eleitos_sexo', 'eleitas_12', 'eleitas_16'),
     file = './data/prefeitas-viaveis.Rdata')

