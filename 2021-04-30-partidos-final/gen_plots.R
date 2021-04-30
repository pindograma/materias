library(sf)
library(tidyverse)
library(treemapify)
library(ggraph)
library(tidygraph)
library(sidrar)
library(cepespR)

load('./data/partidos_em_numeros.Rdata')
source('../theme.R')
shorten_party_names <- function(vt) {
  vt = case_when(vt == "PATRIOTA" ~ "PATRI",
                 vt == "SOLIDARIEDADE" ~ "SD", 
                 vt == "REPUBLICANOS" ~ "REP", 
                 vt == "CIDADANIA" ~ "CID", 
                 vt =="PC do B" ~ "PCdoB",
                 TRUE ~ vt)
}

# FILIADOS PSB, PV, PODEMOS / NOVO PSOL / DEM PT --------------
fil_psbpvpode <- filiacoes_total %>% filter(PARTIDO %in% c("PSB", "PV", "PODE")) %>% 
  ggplot(aes(DATA, n, color = PARTIDO))+
  geom_line(size = 2)+
  geom_point(size = 2.5)+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1, decimal.mark = ",", big.mark = ".")
  )+
  scale_color_manual(
    values = c(party_palette$party_color[party_palette$party_name=="PODE"],
               party_palette$party_color[party_palette$party_name=="PSB"],
               party_palette$party_color[party_palette$party_name=="PV"])
    )+
  labs(
    title = "Número de filiados (2010 - 2019)",
    color = "",
    caption = "Fonte: TSE"
  )+
  theme_pindograma()+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

fil_novopsol <- filiacoes_total %>% filter(PARTIDO %in% c("NOVO", "PSOL"))  %>% 
  ggplot(aes(DATA, n, color = PARTIDO))+
  geom_line(size = 2)+
  geom_point(size = 2.5)+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1, decimal.mark = ",", big.mark = ".")
  )+
  scale_color_manual(
    values = c(party_palette$party_color[party_palette$party_name=="NOVO"],
               party_palette$party_color[party_palette$party_name=="PSOL"])
    )+
  labs(
    title = "Número de filiados (2010 - 2019)",
    color = "",
    caption = "Fonte: TSE"
  )+
  theme_pindograma()+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

fil_dempt <- filiacoes_total %>% filter(PARTIDO %in% c("DEM", "PT")) %>% 
  ggplot(aes(DATA, n, color = PARTIDO))+
  geom_line(size = 2)+
  geom_point(size = 2.5)+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1, decimal.mark = ",", big.mark = ".")
  )+
  scale_color_manual(values = c(
    party_palette$party_color[party_palette$party_name=="DEM"],
    party_palette$party_color[party_palette$party_name=="PT"])
    )+
  labs(
    title = "Número de filiados (2010 - 2019)",
    color = "",
    caption = "Fonte: TSE"
  )+
  theme_pindograma()+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )



# FAIXA ETÁRIA PCDOB DEM / PP PL / PMB PSD REDE -------------
idade_filiados <- filiacoes_idade %>% 
  group_by(PARTIDO,DATA) %>% mutate(total = sum(n), prop = n/total) %>% ungroup()
order_age <- c("16 a 24 anos", "25 a 34 anos","35 a 44 anos", "45 a 59 anos",
               "60 a 69 anos", "70 anos ou mais") %>% rev()

fxeta_pcdobdem <- idade_filiados %>% filter(PARTIDO %in% c("PC do B", "DEM")) %>% 
ggplot(aes(DATA, prop, fill = factor(FAIXA,levels = order_age)))+
  geom_area(stat="identity")+
  scale_fill_manual(values = c(
    pg_orange, pg_blue, pg_green, pg_yellow, pg_light_gray, "#F4Ac90")
    )+
  labs(
    fill = "Faixa etária",
    caption = "Fonte: TSE",
    title = "Composição etária dos filiados (2010 - 2019)"
  )+
  facet_wrap(~PARTIDO)+
  coord_cartesian(xlim = c(as.Date("2010-10-01"), as.Date("2018-12-01")))+
  theme_pindograma()+
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.background = element_blank()
    )

fxeta_pppl <- idade_filiados %>% filter(PARTIDO %in% c("PP", "PL")) %>% 
ggplot(aes(DATA, prop, fill = factor(FAIXA,levels = order_age)))+
  geom_area(stat="identity")+
  scale_fill_manual(values = c(
    pg_orange, pg_blue, pg_green, pg_yellow, pg_light_gray, "#F4Ac90")
    )+
  labs(
    fill = "Faixa etária",
    caption = "Fonte: TSE",
    title = "Composição etária dos filiados (2010 - 2019)"
  )+
  facet_wrap(~PARTIDO)+
  coord_cartesian(xlim = c(as.Date("2010-10-01"), as.Date("2018-12-01")))+
  theme_pindograma()+
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.background = element_blank()
    )

fxeta_pmbredepsd <- idade_filiados %>% filter(PARTIDO %in% c("PMB", "REDE", "PSD")) %>% 
ggplot(aes(DATA, prop, fill = factor(FAIXA,levels = order_age)))+
  geom_area(stat="identity")+
  scale_fill_manual(values = c(
    pg_orange, pg_blue, pg_green, pg_yellow, pg_light_gray, "#F4Ac90")
    )+
  labs(
    fill = "Faixa etária",
    caption = "Fonte: TSE",
    title = "Composição etária dos filiados (2016 - 2019)"
  )+
  facet_wrap(~PARTIDO)+
  coord_cartesian(xlim = c(as.Date("2015-12-01"), as.Date("2019-02-01")))+
  theme_pindograma()+
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.background = element_blank()
    )

# MULHERES FILIADAS ---------
sexo %>%
  group_by(ANO) %>%
  filter(PROP_F_DEC == max(PROP_F_DEC) | PROP_F_DEC == min(PROP_F_DEC)) 

mulheres_filiados <- sexo %>%
  group_by(ANO) %>% 
  summarise(
    dec = sum(DECLARANTES),
    fem = sum(MULHERES)) %>% 
  mutate(prop = fem / dec) %>% 
  ggplot(aes(factor(ANO), prop))+
  geom_col(
    aes(alpha = ANO %>% rank(ties.method = "first")%%2 %>% factor()),
    fill = pg_orange)+
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1, decimal.mark = ",")),
    vjust = 2,
    family = "Fantasque",
    size = 5,
    position = position_dodge(width = .5)
    )+
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  )+
  scale_alpha_manual(values = c(1, .8), guide = F)+ 
  coord_cartesian(ylim = c(.4, .47))+
  labs(
    title = "Proporção de mulheres entre os filiados a partidos políticos (2008-2020)",
    fill = "",
    caption = "Fonte: TSE"
  )+
  theme_pindograma()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# FILIADOS POR 100MIL -----------------
top_filiados <- filiacao_estados %>%
  group_by(UF) %>%
  filter(PROP_100K == max(PROP_100K)) %>% 
  left_join(party_palette, by = c("PARTIDO" = "party_name")) %>% 
  left_join(geoloc_states)

estado_fil <- ggplot(top_filiados) +
  geom_sf(aes(geometry = geom, fill = PARTIDO))+
  labs(title = "Partido com mais filiados por estado", 
       fill = "",
       caption = "Fonte: TSE")+
  scale_fill_manual(
    values = c(
      "#80A0DC",
      "#89C99E",
      "#029590",
      "#DB8A06", 
      "#FFD528",
      "#E92405",
      "#5E899F"
    )
  )+
  theme_pindograma_map()+
  theme(legend.position = "left")

# FINANCIAMENTO ---------
top_fundo <- fundo_partidario %>%
  group_by(ANO) %>% 
  mutate(ranking = n() - rank(VALOR, na.last = F) + 1) %>%
  arrange(ANO, desc(VALOR)) %>% filter(ranking <= 40) %>% ungroup() %>% 
  left_join(party_palette, by = c("PARTIDO" = "party_name"))

fp_top4 <- top_fundo %>% 
  ggplot(aes(factor(ANO), VALOR_CORRIGIDO, fill = reorder(party_color, VALOR_CORRIGIDO)))+
  geom_col(position = "dodge")+
  scale_y_continuous(labels = scales::label_number_si(prefix = "R$", big.mark = ".", decimal.mark = ","))+
  scale_fill_identity(guide = "legend", labels = c("PL", "DEM", "PSD", "PP",
                                                   "PSDB", "MDB", "PT", "PSL"))+
  labs(fill = "Partido",
       title = "Maiores beneficiários do fundo partidário (2008-2019)",
       subtitle = "Valores em milhões de reais, corrigidos pela inflação",
       caption = "Fonte: TSE")+
  theme_pindograma()+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 14))

# PREFEITOS ELEITOS E CANDIDATOS PT MDB / PSDB PCDOB / NOVO -----------
prefeito_pt <- cand20 %>%
  filter(PARTIDO == "PT", CARGO == "PREFEITO") %>% 
  right_join(geoloc_cities, by = c("UF", "MUNICIPIO")) %>% 
  mutate(result = ifelse(is.na(eleito), 0, ifelse(eleito == "Não eleito", -1, 1))) %>% 
  distinct(UF, MUNICIPIO, .keep_all = T) %>% 
ggplot()+
  geom_sf(
          aes(geometry = geom, fill = factor(eleito, levels = c("Eleito", "Não eleito"))),
          lwd = 0)+
  geom_sf(data = geoloc_states, aes(geometry = geom),fill = NA, lwd = .25)+
  scale_fill_manual(values = c(
    party_palette$party_color[party_palette$party_name=="PT"],
    "#E88995"
  ),
                    labels = c("Eleito","Não eleito", "Sem candidato"),
                    na.value="grey90")+
  labs(
    title = "PT",
    fill = ""
  )+
  theme_pindograma()+
  theme(
    axis.line.x = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom"
  )

prefeito_mdb <- cand20 %>%
  filter(PARTIDO == "MDB", CARGO == "PREFEITO") %>% 
  right_join(geoloc_cities, by = c("UF", "MUNICIPIO")) %>% 
  mutate(result = ifelse(is.na(eleito), 0, ifelse(eleito == "Não eleito", -1, 1))) %>% 
  distinct(UF, MUNICIPIO, .keep_all = T) %>% 
ggplot()+
  geom_sf(
          aes(geometry = geom, fill = factor(eleito, levels = c("Eleito", "Não eleito"))),
          lwd = 0)+
  geom_sf(data = geoloc_states, aes(geometry = geom),fill = NA, lwd = .25)+
  scale_fill_manual(values = c(
    party_palette$party_color[party_palette$party_name=="MDB"],
    "#89C79E"
  ),
                    labels = c("Eleito","Não eleito", "Sem candidato"),
                    na.value="grey90")+
  labs(
    title = "MDB",
    fill = ""
  )+
  theme_pindograma()+
  theme(
    axis.line.x = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    legend.position="bottom"
  )

prefeito_psdb <- cand20 %>%
  filter(PARTIDO == "PSDB", CARGO == "PREFEITO") %>% 
  right_join(geoloc_cities, by = c("UF", "MUNICIPIO")) %>% 
  mutate(result = ifelse(is.na(eleito), 0, ifelse(eleito == "Não eleito", -1, 1))) %>% 
  distinct(UF, MUNICIPIO, .keep_all = T) %>% 
ggplot()+
  geom_sf(
          aes(geometry = geom, fill = factor(eleito, levels = c("Eleito", "Não eleito"))),
          lwd = 0)+
  geom_sf(data = geoloc_states, aes(geometry = geom),fill = NA, lwd = .25)+
  scale_fill_manual(values = c(
    party_palette$party_color[party_palette$party_name=="PSDB"],
    "#A9BFEA"
  ),
                    labels = c("Eleito","Não eleito", "Sem candidato"),
                    na.value="grey90")+
  labs(
    title = "PSDB",
    fill = ""
  )+
  theme_pindograma()+
  theme(
    axis.line.x = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom"
  )

prefeito_pcdob <- cand20 %>%
  filter(PARTIDO == "PC do B", CARGO == "PREFEITO") %>% 
  right_join(geoloc_cities, by = c("UF", "MUNICIPIO")) %>% 
  mutate(result = ifelse(is.na(eleito), 0, ifelse(eleito == "Não eleito", -1, 1))) %>% 
  distinct(UF, MUNICIPIO, .keep_all = T) %>% 
ggplot()+
  geom_sf(
          aes(geometry = geom, fill = factor(eleito, levels = c("Eleito", "Não eleito"))),
          lwd = 0)+
  geom_sf(data = geoloc_states, aes(geometry = geom),fill = NA, lwd = .25)+
  scale_fill_manual(values = c(
    party_palette$party_color[party_palette$party_name=="PC do B"],
    "#E086BA"
  ),
                    labels = c("Eleito","Não eleito", "Sem candidato"),
                    na.value="grey90")+
  labs(
    title = "PC do B",
    fill = ""
  )+
  theme_pindograma()+
  theme(
    axis.line.x = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom"
  )

prefeito_novo <- cand20 %>%
  filter(PARTIDO == "NOVO", CARGO == "PREFEITO") %>% 
  right_join(geoloc_cities, by = c("UF", "MUNICIPIO")) %>% 
  mutate(result = ifelse(is.na(eleito), 0, ifelse(eleito == "Não eleito", -1, 1))) %>% 
  distinct(UF, MUNICIPIO, .keep_all = T) %>% 
ggplot()+
  geom_sf(
          aes(geometry = geom, fill = factor(eleito, levels = c("Eleito", "Não eleito"))),
          lwd = 0)+
  geom_sf(data = geoloc_states, aes(geometry = geom),fill = NA, lwd = .25)+
  scale_fill_manual(values = c(
    party_palette$party_color[party_palette$party_name=="NOVO"],
    "#FFCD86"
  ),
                    labels = c("Eleito","Não eleito", "Sem candidato"),
                    na.value="grey90")+
  labs(
    caption = "Fonte: TSE",
    title = str_c("Capilaridade das candidaturas à prefeitura", "NOVO",
                  sep = " - "),
    fill = ""
  )+
  theme_pindograma()+
  theme(
    axis.line.x = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank()
  )

ptmdb <- cowplot::plot_grid(prefeito_pt, prefeito_mdb)+
  labs(title = "Capilaridade das candidaturas à prefeitura", caption = "Fonte: TSE")+
  theme_pindograma_map()

psdbpcdob <- cowplot::plot_grid(prefeito_psdb, prefeito_pcdob)+
  labs(title = "Capilaridade das candidaturas à prefeitura", caption = "Fonte: TSE")+
  theme_pindograma_map()

# COLIGACOES --------------------
ordem_ideo <- c(
                "PCB",
                "PCO",
                "UP",
                "PSTU",
                "PSOL",
                "PC do B",
                "PT",
                "PDT",
                "PSB",
                "REDE",
                "CIDADANIA",
                "PROS",
                "PV",
                "PSDB",
                "MDB",
                "DEM",
                "PSD",
                "SOLIDARIEDADE",
                "PMN",
                "PMB",
                "PODE",
                "AVANTE",
                "PP",
                "PL",
                "DC",
                "PTC",
                "PTB",
                "PRTB",
                "PSL",
                "NOVO",
                "REPUBLICANOS",
                "PSC",
                "PATRIOTA"
                )

PARTIDO <- ordem_ideo
LEGENDA <- ordem_ideo

coligs <- coligacoes %>% right_join(crossing(PARTIDO, LEGENDA)) %>% 
  mutate(PARTIDO = shorten_party_names(PARTIDO), LEGENDA = shorten_party_names(LEGENDA))
  
xadrez <- coligs %>% ggplot()+
  geom_tile(aes(factor(LEGENDA, levels = rev(shorten_party_names(ordem_ideo))), factor(PARTIDO, levels = rev(shorten_party_names(ordem_ideo))), fill = n))+
  scale_fill_distiller(palette = "YlOrBr",
                       direction = 1,
                       guide = "legend",
                       breaks = c(10, 100, 200, 300, 500, 600),
                       na.value = pg_light_gray)+
  labs(
    title = "Frequência de coligação entre partidos",
    fill = "Chapas",
    y = "Líder da coligação",
    x = "Integrante da coligação",
    caption = "Fonte: TSE"
    )+
  theme_pindograma()+
  theme(axis.text.x = element_text(angle = 90, size = 10),
        panel.grid = element_line(colour = pg_dark_gray),
        axis.text.y = element_text(size = 10))

# DIVERSIDADE DE CANDIDATOS E ELEITOS -----------------

div_candidatos <- cand20 %>%
  filter(CARGO %in% c("PREFEITO", "VEREADOR")) %>% 
  count(PARTIDO, RACA, GENERO) %>% 
  group_by(PARTIDO) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  group_by(RACA, GENERO) %>% 
  summarize(total = sum(n),media_das_proporcoes_partidarias = mean(prop)) %>% 
  ungroup() %>% 
  mutate(proporcao_do_total = total/sum(total))

div_eleitos <- cand20 %>%
  filter(CARGO %in% c("PREFEITO", "VEREADOR"), eleito == "Eleito") %>% 
  count(PARTIDO, RACA, GENERO) %>% 
  group_by(PARTIDO) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  group_by(RACA, GENERO) %>% 
  summarize(total = sum(n),media_das_proporcoes_partidarias = mean(prop)) %>% 
  ungroup() %>% 
  mutate(proporcao_do_total = total/sum(total))

genero_candidatos <- cand20 %>% filter(CARGO %in% c("PREFEITO", "VEREADOR")) %>% 
  group_by(PARTIDO, GENERO) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n), pm = min(prop)) %>% 
  left_join(party_palette, by = c("PARTIDO" = "party_name")) %>% 
  mutate(PARTIDO = shorten_party_names(PARTIDO)) %>% 
  ggplot()+
  geom_col(aes(reorder(PARTIDO, -pm), prop, 
               fill = factor(GENERO, levels = c("MASCULINO", "FEMININO"))))+
  geom_hline(yintercept = .347, color = "grey10")+
  geom_hline(yintercept = .3, color = "#C30000")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#7B8FAA", "#D77F1D"))+
  labs(title = "Proporção de gênero entre os candidatos",
       caption = "Fonte: TSE",
       fill = "Gênero")+
  coord_flip()+
  theme_pindograma()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

genero_eleitos <- cand20 %>% filter(CARGO %in% c("PREFEITO", "VEREADOR"), eleito == "Eleito") %>% 
  group_by(PARTIDO, GENERO) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n), pm = min(prop)) %>% 
  left_join(party_palette, by = c("PARTIDO" = "party_name")) %>% 
  mutate(PARTIDO = shorten_party_names(PARTIDO)) %>% 
  ggplot()+
  geom_col(aes(reorder(PARTIDO, -pm), prop, 
               fill = factor(GENERO, levels = c("MASCULINO", "FEMININO"))))+
  geom_hline(aes(yintercept = mean(pm)), color = "grey10")+
  geom_hline(yintercept = .3, color = "#C30000")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#7B8FAA", "#D77F1D"))+
  labs(title = "Proporção de gênero entre os eleitos",
       caption = "Fonte: TSE",
       fill = "Gênero")+
  coord_flip()+
  theme_pindograma()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

raca_candidatos <- cand20 %>% filter(CARGO %in% c("PREFEITO", "VEREADOR")) %>% 
  filter(RACA != "SEM INFORMAÇÃO") %>% 
  group_by(PARTIDO, RACA) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% 
  left_join(
    cand20 %>%
      filter(CARGO %in% c("PREFEITO", "VEREADOR")) %>% 
      group_by(PARTIDO, negros = RACA %in% c("PRETA", "PARDA")) %>% 
      summarise(n = n()) %>% 
      mutate(pn = n / sum(n)) %>% 
      filter(negros) %>% 
      select(PARTIDO, pn)
  ) %>% 
  mutate(PARTIDO = shorten_party_names(PARTIDO)) %>% 
  ggplot()+
  geom_col(aes(reorder(PARTIDO, -pn), prop, 
               fill = RACA))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(
    values = c(pg_dark_gray, pg_green, pg_yellow, pg_blue, pg_orange),
    labels = c("Amarela", "Branca", "Indígena", "Parda", "Preta")
    )+
  labs(title = "Proporção de raça autodeclarada entre os candidatos",
       caption = "Fonte: TSE",
       fill = "Raça")+
  coord_flip()+
  theme_pindograma()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

raca_eleitos <- cand20 %>% filter(CARGO %in% c("PREFEITO", "VEREADOR"), eleito == "Eleito") %>% 
  filter(RACA != "SEM INFORMAÇÃO") %>% 
  group_by(PARTIDO, RACA) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n), pmax = max(prop)) %>% 
 left_join(
   cand20 %>%
     filter(CARGO %in% c("PREFEITO", "VEREADOR"), eleito == "Eleito") %>% 
     group_by(PARTIDO, negros = RACA %in% c("PRETA", "PARDA")) %>% 
     summarise(n = n()) %>% 
     mutate(pn = n / sum(n)) %>% 
     filter(negros) %>% 
     select(PARTIDO, pn)) %>% 
  mutate(PARTIDO = shorten_party_names(PARTIDO)) %>% 
  ggplot()+
  geom_col(aes(reorder(PARTIDO, -pn), prop, 
               fill = RACA))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(
    values = c(pg_dark_gray, pg_green, pg_yellow, pg_blue, pg_orange),
    labels = c("Amarela", "Branca", "Indígena", "Parda", "Preta")
    )+
  labs(title = "Proporção de raça autodeclarada entre os eleitos", fill = "Raça")+
  coord_flip()+
  theme_pindograma()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


raca_comparacao <- cand20 %>% filter(CARGO %in% c("PREFEITO", "VEREADOR")) %>% 
  filter(RACA != "SEM INFORMAÇÃO") %>% 
  group_by(PARTIDO, RACA) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n), gp = "Candidatos") %>% 
  left_join(
    cand20 %>%
      filter(CARGO %in% c("PREFEITO", "VEREADOR")) %>% 
      group_by(PARTIDO, negros = RACA %in% c("PRETA", "PARDA")) %>% 
      summarise(n = n()) %>% 
      mutate(pn = n / sum(n)) %>% 
      filter(negros) %>% 
      select(PARTIDO, pn)
  ) %>% 
  bind_rows(
    cand20 %>% filter(CARGO %in% c("PREFEITO", "VEREADOR"), eleito == "Eleito") %>% 
      filter(RACA != "SEM INFORMAÇÃO") %>% 
      group_by(PARTIDO, RACA) %>% 
      summarise(n = n()) %>% 
      mutate(prop = n / sum(n), gp = "Eleitos") %>% 
      left_join(
        cand20 %>%
          filter(CARGO %in% c("PREFEITO", "VEREADOR"), eleito == "Eleito") %>% 
          group_by(PARTIDO, negros = RACA %in% c("PRETA", "PARDA")) %>% 
          summarise(n = n()) %>% 
          mutate(pn = n / sum(n)) %>% 
          filter(negros) %>% 
          select(PARTIDO, pn))
  ) %>% 
  mutate(PARTIDO = shorten_party_names(PARTIDO)) %>% 
  ggplot()+
  geom_col(aes(reorder(PARTIDO, -pn), prop, 
               fill = RACA))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(
    values = c(pg_dark_gray, pg_green, pg_yellow, pg_blue, pg_orange),
    labels = c("Amarela", "Branca", "Indígena", "Parda", "Preta")
  )+
  labs(title = "Proporção de raça autodeclarada",
       caption = "Fonte: TSE",
       fill = "Raça")+
  facet_wrap(~gp)+
  coord_flip()+
  theme_pindograma()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y.right = element_text(),
        legend.position = "bottom")
 
# REPRESENTANTES E REPRESENTADOS EM 2018 --------------------
pop_ibge <- get_sidra(6579, variable = 9324, period = as.character(2020), geo = "State") %>% 
  mutate(ESTADO = str_to_upper(`Unidade da Federação`)) %>% 
  rename(POPULACAO = Valor) %>% 
  select(ESTADO, POPULACAO)

reprep <- eleicoes18 %>% 
  left_join(pop_ibge) %>%
  mutate(POP_PONDERADA = POPULACAO / BANCADA) %>% 
  group_by(PARTIDO, CARGO) %>%
  summarize(Representantes = n(), Representados = round(sum(POP_PONDERADA))) %>% 
  arrange(CARGO, desc(Representados)) %>% 
  ungroup() %>% 
  mutate(PARTIDO = shorten_party_names(PARTIDO))
  
  write_csv("populacoes_representadas_resumo.csv")


plot_depest <- reprep %>% 
  filter(CARGO == "DEPUTADO ESTADUAL") %>% 
  ggplot(aes(reorder(PARTIDO, Representantes), Representantes))+
  geom_col(
    aes(alpha = reorder(PARTIDO, Representantes) %>% rank(ties.method = "first")%%2 %>% factor()),
    fill = pg_orange
    )+
  scale_alpha_manual(values = c(1, .6), guide = F)+ 
  geom_text(
    aes(label = Representantes),
    hjust = 1,
    family = "Fantasque",
    size = 5,
    )+
  labs(title = "Deputados Estaduais eleitos por partido (2018)", caption = "Fonte: TSE via cepespR")+
  coord_flip()+
  theme_pindograma()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot_depfed <- reprep %>% 
  filter(CARGO == "DEPUTADO FEDERAL") %>% 
  ggplot(aes(reorder(PARTIDO, Representantes), Representantes))+
  geom_col(
    aes(alpha = reorder(PARTIDO, Representantes) %>% rank(ties.method = "first")%%2 %>% factor()),
    fill = pg_blue
    )+
  scale_alpha_manual(values = c(1, .6), guide = F)+ 
  geom_text(
    aes(label = Representantes),
    hjust = 1,
    family = "Fantasque",
    size = 5,
    )+
  labs(title = "Deputados Federais eleitos por partido (2018)", caption = "Fonte: TSE via cepespR")+
  coord_flip()+
  theme_pindograma()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


plot_sen <- reprep %>% 
  filter(CARGO == "SENADOR") %>% 
  ggplot(aes(reorder(PARTIDO, Representantes), Representantes))+
  geom_col(
    aes(alpha = reorder(PARTIDO, Representantes) %>% rank(ties.method = "first")%%2 %>% factor()),
    fill = pg_green)+
  scale_alpha_manual(values = c(1, .6), guide = F)+ 
  geom_text(
    aes(label = Representantes),
    hjust = 2,
    family = "Fantasque",
    size = 5
    )+
  labs(title = "Senadores eleitos por partido (2018)", caption = "Fonte: TSE via cepespR")+
  coord_flip()+
  theme_pindograma()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

mapa_gov <- eleicoes18 %>% filter(CARGO == "GOVERNADOR") %>% select(UF, PARTIDO) %>% 
  left_join(party_palette, by = c("PARTIDO" = "party_name")) %>% 
  right_join(geoloc_states) %>% 
  ggplot()+
  geom_sf(aes(geometry = geom, fill = PARTIDO))+
  theme_pindograma_map()+
  theme(legend.position = "left")+
  labs(title = "Governadores por partido", caption = "Fonte: TSE via cepespR")+
  scale_fill_manual(values = c(
    "#80A0DC",
    "#89C79E",
    "#FFA400",
    "#E086BA",
    "#9C2F04",
    "grey80",
    "#A2D5F0",
    "#FFCD86",
    "#D3B57C",
    party_palette$party_color[party_palette$party_name == "PSD"],
    "#A9BFEA",
    "#7B94AF",
    "#C30000"
  ),
  guide = "legend")


# GRÁFICOS FINALIZADOS -----------

fil_psbpvpode
fil_novopsol
fil_dempt

fxeta_pcdobdem
fxeta_pppl
fxeta_pmbredepsd

mulheres_filiados

estado_fil

fp_top4

ptmdb
psdbpcdob
prefeito_novo

genero_candidatos
genero_eleitos
raca_comparacao

plot_depest
plot_depfed
plot_sen
mapa_gov

save(list = c('fil_psbpvpode', 'fil_novopsol', 'fil_dempt', 'fxeta_pcdobdem',
              'fxeta_pppl', 'fxeta_pmbredepsd', 'mulheres_filiados', 'estado_fil',
              'fp_top4', 'ptmdb', 'psdbpcdob', 'prefeito_novo', 'genero_candidatos',
              'genero_eleitos', 'raca_comparacao', 'plot_depest', 'plot_depfed',
              'plot_sen', 'mapa_gov', 'order_age', 'shorten_party_names', 'xadrez',
              'ordem_ideo'),
     file = './data/plots.Rdata')
