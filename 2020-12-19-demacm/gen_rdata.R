library(tidyverse)
library(cepespR)
library(geobr)
library(stringi)
library(sidrar)

#declare helper function
normalize_simple = function(x) {
  str_squish(toupper(stri_trans_general(str = x, id = 'Latin-ASCII')))
}

#get city populations
pop_ibge <- get_sidra(
  6579, variable = 9324, period = as.character(2020), geo = "City"
) %>%
  mutate(
    UF = str_sub(Município, -2, -1),
    Município = str_sub(Município, 1, -6) %>% normalize_simple()
  ) %>% 
  select(UF, Município, Valor) %>% 
  rename(MUNICIPIO = Município, POPULACAO = Valor)

#get data for city geolocation

geoloc_cities <- read_municipality() %>%
  select(abbrev_state, name_muni, geom) %>% 
  mutate(name_muni = normalize_simple(name_muni)) %>% 
  rename(UF = abbrev_state, MUNICIPIO = name_muni)

#import party color palette
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
  party_name = c('PSDB', 'PSL', 'REPUBLICANOS', 'PP', 'PTB', 'PSD', 'PV',
                 'MDB', 'DEM', 'NOVO', 'PODE', 'PL', 'PSC', 'PDT',
                 'PT', 'PC do B', 'PSOL', 'CIDADANIA', 'PSB', 'REDE', 'PSTU',
                 'PCO', 'PCB', 'SOLIDARIEDADE', 'PRTB', 'PMN', 'PTC', 'PATRIOTA',
                 'AVANTE', 'UP', 'PROS', 'PPL', 'PRP', 'DC', 'PHS',
                 'PMB', 'Branco/Nulo')
)

#get candidates and results for 2020 elections
cand20_raw <- read_csv2("./data/consulta_cand_2020_BRASIL.csv",
                        locale = locale(encoding = "Latin1"))

resultados20_raw <- read_csv("./data/resultados_prefeito.csv") %>%
  mutate(CARGO = "PREFEITO") %>% 
  bind_rows(read_csv("./data/resultados_vereador.csv") %>% mutate(
    urnas_apuradas = as.character(urnas_apuradas),
    abstencoes_pct = as.character(abstencoes_pct),
    nulos_pct = as.character(nulos_pct),
    CARGO = "VEREADOR"
  )) %>% mutate(SEG_TURNO = F) %>%  
  bind_rows(read_csv("./data/resultados_prefeito_final.csv") %>%
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
         SG_PARTIDO, DS_COMPOSICAO_COLIGACAO, DS_GENERO, DS_COR_RACA,
         NR_IDADE_DATA_POSSE) %>%
  mutate(NM_UE = normalize_simple(NM_UE)) %>% 
  rename(CARGO = DS_CARGO, UF = SG_UF, MUNICIPIO = NM_UE, PARTIDO = SG_PARTIDO,
         LEGENDA = DS_COMPOSICAO_COLIGACAO, GENERO = DS_GENERO,
         RACA = DS_COR_RACA, IDADE = NR_IDADE_DATA_POSSE) %>% 
  left_join(resultados20, by = c("SG_UE", "NM_URNA_CANDIDATO", "CARGO")) %>% 
  group_by(SG_UE, NM_URNA_CANDIDATO) %>% 
  arrange(UF, MUNICIPIO, NM_URNA_CANDIDATO, desc(SEG_TURNO)) %>% 
  distinct(NM_URNA_CANDIDATO, .keep_all = T) %>% ungroup()

#get DEM historical mayoral elections performance (2012, 2016, 2020)
dem_hist_pref <- get_candidates(year = "2012, 2016", position = "Mayor", candidate_number = 25 ) %>% 
  select(ANO_ELEICAO, NUM_TURNO, DESCRICAO_ELEICAO, DESCRICAO_CARGO, SIGLA_UF, SIGLA_UE, DESCRICAO_UE, NOME_CANDIDATO, 
         NOME_URNA_CANDIDATO, SIGLA_PARTIDO,
       COMPOSICAO_LEGENDA, DESCRICAO_SEXO, IDADE_DATA_ELEICAO, DESC_SIT_TOT_TURNO) %>%
  rename(SG_UE = SIGLA_UE, MUNICIPIO = DESCRICAO_UE, UF = SIGLA_UF, CARGO = DESCRICAO_CARGO,
         PARTIDO = SIGLA_PARTIDO, LEGENDA = COMPOSICAO_LEGENDA,
         GENERO = DESCRICAO_SEXO, IDADE = IDADE_DATA_ELEICAO, NM_CANDIDATO = NOME_CANDIDATO,
         NM_URNA_CANDIDATO = NOME_URNA_CANDIDATO, eleito = DESC_SIT_TOT_TURNO)
dem_20 <- cand20 %>% filter(PARTIDO == "DEM") %>% mutate(ANO_ELEICAO = 2020) %>%
  mutate(eleito = toupper(eleito), NUM_TURNO = ifelse(SEG_TURNO, 2, 1), DESCRICAO_ELEICAO = "Municipais 2020") %>%
  select(-NM_PARTIDO, -numero, -SEG_TURNO)
dem_pref <- dem_20 %>% filter(CARGO == "PREFEITO") %>% bind_rows(dem_hist_pref)
suplementares <- dem_pref %>%
  count(DESCRICAO_ELEICAO) %>%
  filter(!DESCRICAO_ELEICAO %in% c("ELEIÇÃO MUNICIPAL 2012", "Eleições Municipais 2016", "Municipais 2020"))
dem_resumo <- dem_pref %>% select(SG_UE, UF, MUNICIPIO, ANO_ELEICAO, NUM_TURNO, DESCRICAO_ELEICAO, eleito) %>% 
  group_by(SG_UE, ANO_ELEICAO) %>%
  filter(NUM_TURNO == max(NUM_TURNO) | DESCRICAO_ELEICAO %in% suplementares$DESCRICAO_ELEICAO) %>% 
  mutate(n = n()) %>% filter(n == 1 | DESCRICAO_ELEICAO %in% suplementares$DESCRICAO_ELEICAO) %>% 
  select(-n, -DESCRICAO_ELEICAO, -NUM_TURNO) %>% ungroup()
  
#organize DEM wins by city and year
hist_por_cidade <- dem_resumo %>% 
  pivot_wider(names_from = ANO_ELEICAO, values_from = eleito) %>% 
  mutate(`2012` = ifelse(is.na(`2012`) | `2012` == "#NULO#", "SEM CANDIDATO", `2012`),
         `2016` = ifelse(is.na(`2016`) | `2016` == "#NULO#", "SEM CANDIDATO", `2016`),
         `2020` = ifelse(is.na(`2020`), "SEM CANDIDATO", `2020`)
         ) %>% 
  select(SG_UE, UF, MUNICIPIO, `2012`, `2016`, `2020`)

#organize DEM wins by state and year
hist_por_uf <- dem_resumo %>%
  count(UF, ANO_ELEICAO, eleito) %>%
  filter(eleito == "ELEITO") %>%
  arrange(UF, ANO_ELEICAO) %>%
  pivot_wider(names_from = ANO_ELEICAO, values_from = n) %>% select(UF, `2012`, `2016`, `2020`) %>% 
  right_join(geoloc_states %>% select(UF))

#get all elected mayors in 2016
prefeitos16 <- get_candidates(year = "2016", position = "Mayor", only_elected = T) %>% 
  select(ANO_ELEICAO, NUM_TURNO, DESCRICAO_ELEICAO, DESCRICAO_CARGO, SIGLA_UF, SIGLA_UE, DESCRICAO_UE, NOME_CANDIDATO, 
         NOME_URNA_CANDIDATO, CPF_CANDIDATO, SIGLA_PARTIDO, 
         COMPOSICAO_LEGENDA, DESCRICAO_SEXO, IDADE_DATA_ELEICAO, DESC_SIT_TOT_TURNO) %>%
  rename(SG_UE = SIGLA_UE, MUNICIPIO = DESCRICAO_UE, UF = SIGLA_UF, CARGO = DESCRICAO_CARGO,
         PARTIDO = SIGLA_PARTIDO, LEGENDA = COMPOSICAO_LEGENDA,
         GENERO = DESCRICAO_SEXO, IDADE = IDADE_DATA_ELEICAO, NM_CANDIDATO = NOME_CANDIDATO,
         NM_URNA_CANDIDATO = NOME_URNA_CANDIDATO, eleito = DESC_SIT_TOT_TURNO) %>%
  left_join(pop_ibge, by = c("UF", "MUNICIPIO")) %>% 
  mutate(
    PARTIDO = ifelse(
      PARTIDO == "PMDB", "MDB",
      ifelse(PARTIDO == "PFL", "DEM",
      ifelse(PARTIDO == "PRB", "REPUBLICANOS", 
      ifelse(PARTIDO == "SD", "SOLIDARIEDADE", 
      ifelse(PARTIDO == "PPS", "CIDADANIA",
      ifelse(PARTIDO == "PTN", "PODE", 
      ifelse(PARTIDO == "PEN", "PATRIOTA", 
      ifelse(PARTIDO == "PT do B", "AVANTE",
      ifelse(PARTIDO == "PHS", "PODE",
      ifelse(PARTIDO == "PR", "PL",
      ifelse(PARTIDO == "PPL", "PC do B",
      ifelse(PARTIDO == "PRP", "PATRIOTA",
      ifelse(PARTIDO == "PPB", "PP", PARTIDO))))))))))))),
    MUNICIPIO = normalize_simple(MUNICIPIO))

#get all elected mayors in 2020
prefeitos20 <- cand20 %>% filter(CARGO == "PREFEITO", eleito == "Eleito") %>%
  left_join(pop_ibge, by = c("UF", "MUNICIPIO")) %>% 
  mutate(ANO_ELEICAO = 2020)

pref16dem <- prefeitos16 %>% filter(PARTIDO == "DEM")
pref20dem <- prefeitos20 %>% filter(PARTIDO == "DEM")

#check which parties lost cities to DEM and won cities from DEM
won <- prefeitos16 %>% filter(SG_UE %in% pref20dem$SG_UE, PARTIDO != "DEM") %>% count(PARTIDO) %>% arrange(desc(n))
lost <- prefeitos20 %>% filter(SG_UE %in% pref16dem$SG_UE, PARTIDO != "DEM") %>% count(PARTIDO) %>% arrange(desc(n))
net <- won %>% full_join(lost, by = "PARTIDO") %>%
  mutate(n.x = ifelse(is.na(n.x), 0, n.x), n.y = ifelse(is.na(n.y), 0, n.y)) %>% 
  mutate(net = n.x - n.y) %>% 
  rename(prefeituras_perdidas = n.x, prefeituras_tomadas = n.y)

#generate dataframe with municipal coordinates for map graphics
mapa <- prefeitos16 %>% bind_rows(prefeitos20) %>%
  select(UF, MUNICIPIO, ANO_ELEICAO, PARTIDO) %>%
  left_join(party_palette, by = c("PARTIDO" = "party_name")) %>% 
  left_join(geoloc_cities, by = c("UF", "MUNICIPIO"))

save(list = c('mapa', 'net'),
     file = './data/demacm.Rdata')

