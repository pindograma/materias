library(tidyverse)
library(cepespR)

get_summary <- function(df) {
  df %>%
	  group_by(NR_TURNO, DS_FAIXA_ETARIA) %>%
	  summarise(total = sum(QT_APTOS), comparecimentos = sum(QT_COMPARECIMENTO), abstencoes = sum(QT_ABSTENCAO)) %>%
	  mutate(proporcao = abstencoes/total) %>%
	  filter(NR_TURNO == 1) %>% 
	  arrange(proporcao)
}

df18 <- read_csv2('perfil_comparecimento_abstencao_2018/perfil_comparecimento_abstencao_2018.csv', locale = locale(encoding = 'ISO-8859-1'))
sum18 = get_summary(df18) %>% mutate(ano = 2018)
rm(df18)

df16 <- read_csv2('perfil_comparecimento_abstencao_2016/perfil_comparecimento_abstencao_2016.csv', locale = locale(encoding = 'ISO-8859-1'))
sum16 = get_summary(df16) %>% mutate(ano = 2016)
rm(df16)

df14 <- read_csv2('perfil_comparecimento_abstencao_2014/perfil_comparecimento_abstencao_2014.csv', locale = locale(encoding = 'ISO-8859-1'))
sum14 = get_summary(df14) %>% mutate(ano = 2014)
rm(df14)

invalid = c('16 anos', '17 anos', '70 a 74 anos', '75 a 79 anos', '80 a 84 anos', '85 a 89 anos', '90 a 94 anos', '95 a 99 anos', '100 anos ou mais', 'Inválido')
jovens = c('18 a 20 anos', '21 a 24 anos', '25 a 29 anos', '30 a 34 anos')
levels = rev(c('18 a 20 anos', '21 a 24 anos', '25 a 29 anos', '30 a 34 anos', '35 a 39 anos', '40 a 44 anos', '45 a 49 anos', '50 a 54 anos', '55 a 59 anos', '60 a 64 anos', '65 a 69 anos'))

abstencoes <- bind_rows(sum18, sum16, sum14) %>%
	ungroup() %>%
  select(DS_FAIXA_ETARIA:ano) %>%
	mutate(DS_FAIXA_ETARIA = recode(DS_FAIXA_ETARIA,
		`18 anos` = '18 a 20 anos',
		`19 anos` = '18 a 20 anos',
		`20 anos` = '18 a 20 anos'
	)) %>%
  group_by(ano, DS_FAIXA_ETARIA) %>%
  summarise(total = sum(total), comparecimentos = sum(comparecimentos), abstencoes = sum(abstencoes)) %>%
  mutate(proporcao = abstencoes/total) %>%
	mutate(jovem = ifelse(DS_FAIXA_ETARIA %in% jovens, 'Jovens', 'Não Jovens')) %>%
	ungroup() %>%
	filter(!(DS_FAIXA_ETARIA %in% invalid)) %>%
	mutate(DS_FAIXA_ETARIA = factor(DS_FAIXA_ETARIA, levels = levels))

columns <- list("ANO_ELEICAO", "SIGLA_UF", "NOME_CANDIDATO", "NOME_URNA_CANDIDATO", "DESCRICAO_CARGO", "COD_SITUACAO_CANDIDATURA", "SIGLA_PARTIDO", "DESCRICAO_OCUPACAO","DATA_NASCIMENTO", "IDADE_DATA_ELEICAO", "DESC_SIT_TOT_TURNO", "DESCRICAO_SEXO")

treat_idade <- bind_rows(
	get_candidates(year = "2002, 2006, 2010", position = "Governador", columns_list = columns, only_elected = T),
	get_candidates(year = "2002, 2006, 2010", position = "Deputado Federal", columns_list = columns, only_elected = T),
	get_candidates(year = "2002, 2006, 2010", position = "Deputado Estadual", columns_list = columns, only_elected = T)
) %>%
  mutate(IDADE_DATA_ELEICAO = (ANO_ELEICAO %% 100 + 100) - as.integer(str_sub(DATA_NASCIMENTO, -2, -1)))

elected <- bind_rows(
	treat_idade,
	get_candidates(year = "2014, 2018", position = "Deputado Federal", columns_list = columns, only_elected = T),
  get_candidates(year = "2014, 2018", position = "Deputado Estadual", columns_list = columns, only_elected = T),
  get_candidates(year = "2014, 2018", position = "Governador", columns_list = columns, only_elected = T)
)

elected2 <- elected %>%
  filter(DESC_SIT_TOT_TURNO != "SUPLENTE") %>%
	mutate(jovem_24 = IDADE_DATA_ELEICAO < 25) %>%
	mutate(jovem_34 = IDADE_DATA_ELEICAO < 35) %>%
  filter(ANO_ELEICAO %% 4 != 0)

prop_jovem <- elected2 %>%
	group_by(DESCRICAO_CARGO, ANO_ELEICAO, jovem_34) %>%
	summarize(n_eleitos = n()) %>%
	filter(!is.na(jovem_34))

prop_jovem_chart <- elected2 %>%
	group_by(ANO_ELEICAO, jovem_34) %>%
	summarize(n_eleitos = n()) %>%
	ungroup() %>%
	filter(!is.na(jovem_34)) %>%
	group_by(ANO_ELEICAO) %>%
	mutate(tot = sum(n_eleitos)) %>%
	ungroup() %>%
	mutate(frac = n_eleitos / tot) %>%
	filter(jovem_34)

normalize_simple <- function(x) {
  str_squish(toupper(stri_trans_general(str = x, id = 'Latin-ASCII')))
}

candidatos <- read_csv2("consulta_cand_2020_BRASIL.csv", locale = locale(encoding = 'Latin1')) %>% 
  mutate(NM_CANDIDATO = normalize_simple(NM_CANDIDATO), NM_URNA_CANDIDATO = normalize_simple(NM_URNA_CANDIDATO))

renova <- read_csv("renova_nomes.csv") %>%
  mutate(NOME = normalize_simple(NOME)) %>% 
  distinct()

candidatos_renova <- candidatos %>%
	mutate(joinid = row_number()) %>%
	inner_join(renova, by = c('NM_CANDIDATO' = 'NOME')) %>%
	group_by(joinid) %>%
	filter(n() == 1) %>%
	ungroup() %>%
  select(DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO, DT_NASCIMENTO, NR_IDADE_DATA_POSSE)

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
  label_color = c('white', 'white', 'white', 'black', 'white', 'black', 'white',
                  'white', 'white', 'black', 'black', 'white', 'white', 'black',
                  'black', 'white', 'black', 'black', 'black', 'black', 'black',
                  'black', 'black', 'black', 'white', 'white', 'white', 'black',
                  'black', 'black', 'white', 'black', 'black', 'black', 'black',
                  'black', 'black'),
  party_name = c('PSDB', 'PSL', 'Republicanos', 'Progressistas', 'PTB', 'PSD', 'PV',
                 'MDB', 'DEM', 'NOVO', 'PODE', 'PL', 'PSC', 'PDT',
                 'PT', 'PCdoB', 'PSOL', 'CIDADANIA', 'PSB', 'REDE', 'PSTU',
                 'PCO', 'PCB', 'SD', 'PRTB', 'PMN', 'PTC', 'PATRIOTA',
                 'Avante', 'UP', 'PROS', 'PPL', 'PRP', 'DC', 'PHS',
                 'PMB', 'Branco/Nulo'))

ideologicos = c('NOVO', 'REDE', 'PSL', 'PSOL', 'PATRIOTA')
tradicionais = c('PT', 'PDT', 'PSDB', 'PSB', 'MDB', 'DEM')

filiacoes = read_csv('filiacoes_joao.csv')

filiacoes_joao_raw = filiacoes %>%
	filter(Partido %in% c(ideologicos, tradicionais)) %>%
	mutate(trad = factor(Partido %in% tradicionais)) %>%
	left_join(party_palette, by = c('Partido' = 'party_name'))

filiacoes_medias <- filiacoes %>% filter(Partido %in% c('MEDIA', 'BRASIL')) %>% select(-filiados24, -filiados34, -Total)

all_mean_age = mean(candidatos$NR_IDADE_DATA_POSSE)

save(list = c('candidatos_renova', 'all_mean_age', 'prop_jovem', 'prop_jovem_chart', 'abstencoes', 'filiacoes', 'filiacoes_joao_raw', 'filiacoes_medias'), file = 'jovens.Rdata')
