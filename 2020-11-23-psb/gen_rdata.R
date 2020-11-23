library(tidyverse)
library(cepespR)
library(geobr)
library(stringi)
library(lubridate)
library(deflateBR)

#declare helper function
normalize_simple = function(x) {
  str_squish(toupper(stri_trans_general(str = x, id = 'Latin-ASCII')))
}

#get data for city and state geolocation
geoloc_states <- read_state() %>% select(abbrev_state, geom) %>% 
  rename(UF = abbrev_state)

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


#get elected official from 2018 elections
governadores18 <- get_candidates(year = '2018', position = 'Governador', only_elected = 'T')
senadores18 <- get_candidates(year = '2018, 2014', position = 'Senador', only_elected = 'T') %>% 
  mutate(SIGLA_PARTIDO = ifelse(SIGLA_PARTIDO == "PMDB", "MDB", SIGLA_PARTIDO))
depfed18 <- get_candidates(year = '2018', position = 'Deputado Federal', only_elected = 'T')
depest18 <- get_candidates(year = '2018', position = 'Deputado Estadual', only_elected = 'T')

eleicoes18 <- bind_rows(governadores18,senadores18,depfed18,depest18) %>% 
  select(DESCRICAO_CARGO, SIGLA_UF, DESCRICAO_UE, NOME_CANDIDATO, 
         NOME_URNA_CANDIDATO, SIGLA_PARTIDO, NOME_PARTIDO,
         COMPOSICAO_LEGENDA, DESCRICAO_SEXO, IDADE_DATA_ELEICAO) %>%
  rename(ESTADO = DESCRICAO_UE, UF = SIGLA_UF, CARGO = DESCRICAO_CARGO,
         PARTIDO = SIGLA_PARTIDO, LEGENDA = COMPOSICAO_LEGENDA,
         GENERO = DESCRICAO_SEXO, IDADE = IDADE_DATA_ELEICAO) %>% 
  mutate(CARGO = ifelse(CARGO %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL"),
                        "DEPUTADO ESTADUAL", CARGO)) %>% 
  group_by(UF, CARGO) %>% mutate(BANCADA = n()) %>% ungroup() 

#get candidates for 2020 elections
cand20_raw <- read_csv2("./data/consulta_cand_2020_BRASIL.csv",
                        locale = locale(encoding = "Latin1"))

resultados20_raw <- read_csv("./data/resultados_prefeito.csv") %>%
  mutate(CARGO = "PREFEITO") %>% 
  bind_rows(read_csv("./data/resultados_vereador.csv") %>% mutate(
    urnas_apuradas = as.character(urnas_apuradas),
    abstencoes_pct = as.character(abstencoes_pct),
    nulos_pct = as.character(nulos_pct),
    CARGO = "VEREADOR"
  ))

resultados20 <- resultados20_raw %>% select(codigo_cidade_tse, CARGO,
                                            nome, numero, votos_validos,
                                            eleito) %>% 
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
  left_join(resultados20, by = c("SG_UE", "NM_URNA_CANDIDATO", "CARGO"))

#get data about registered party members
filiacoes_raw = read_csv("./data/filiados_sexo_idade.csv") %>% 
  mutate(DIA = 1) %>%
  mutate(DATA = make_date(ANO, MÊS, DIA)) %>% 
  filter(DATA > "2010-03-25" & DATA < "2019-10-01")

filiacoes_idade <- filiacoes_raw %>%
  mutate(FAIXA = ifelse(FAIXA %in% c("16 anos", "17 anos", "18 a 20 anos", 
                                     "21 a 24 anos"), "16 a 24 anos", FAIXA)) %>% 
  mutate(FAIXA = ifelse(FAIXA %in% c("70 a 79 anos", "Superior a 79 anos"),
                        "70 anos ou mais", FAIXA)) %>% 
  filter(!FAIXA %in% c("Inválida", "Não informada")) %>% 
  group_by(DATA, PARTIDO, FAIXA) %>% summarize(n = sum(FILIADOS))
  
filiacoes_total <- filiacoes_raw %>% 
  group_by(DATA, PARTIDO) %>% summarize(n = sum(FILIADOS))

sexo <- read_csv("./data/sexo.csv") %>% 
  mutate(PROP_F_ALL = MULHERES/TOTAL,
         PROP_F_DEC = MULHERES/DECLARANTES)

filiacao_estados <- read_csv2("./data/filiados_estados_2020_04.csv") %>%
  mutate(PROP_100K = round(FILIADOS*100000/ELE_UF, digits = 0))
  

#get party funding data
fundo_partidario <- read_csv2("./data/fundo_formatado.csv") %>% 
  mutate(MES = 12, DIA = 1, DATA = make_date(ANO, MES, DIA)) %>% 
  select(-MES, -DIA) %>% 
  mutate(VALOR_CORRIGIDO = deflate(VALOR, DATA, "09/2020", "igpm") )

#get list of all mayoral party alliances     
coligacoes <- cand20 %>% filter(CARGO == "PREFEITO") %>%
  select(PARTIDO, LEGENDA) %>%
  mutate(LEGENDA = LEGENDA %>% str_split(" / ")) %>%
  unnest(LEGENDA) %>% 
  filter(PARTIDO != LEGENDA, LEGENDA != "#NULO#") %>% 
  count(PARTIDO, LEGENDA)

#create party node list
nodes <- party_palette %>% arrange(party_name) %>% 
  filter(!party_name %in% c("Branco/Nulo", "PRP", "PPL", "PHS") ) %>% 
  rename(value = party_name) %>% 
  mutate(id = 1:33)

save(list = c('eleicoes18', 'filiacoes_total', 'cand20', 'geoloc_cities',
              'geoloc_states', 'party_palette', 'filiacoes_idade', 'sexo',
              'filiacao_estados', 'fundo_partidario', 'coligacoes', 'nodes'),
     file = './data/partidos_em_numeros.Rdata')

