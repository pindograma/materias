library(tidyverse)
library(sf)
library(geobr)
library(aopdata)
library(corrplot)

load("../vacina/tony_esta_irritado.Rdata")
source("../../materias/theme.R")

to_hex <- function(shapefile, hex, var_name, geom_type) {
  if (!geom_type %in% c("point", "area")) {
    print("geom_type must be either \"area\" or \"point\" ")
    return(tibble())
  }
  
  byhex <- hex %>%
    st_join(shapefile %>% st_set_crs(31983) %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry()
  
  if("id_hex" %in% names(hex)){
    oup <- byhex %>% mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
      group_by(id_hex) %>% summarise(n = sum(counter))
    
    print(oup)
    
    colnames(oup) <- c("id_hex", var_name)
  }
  
  else{
    oup <- byhex %>% mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
      group_by(name_district) %>% summarise(n = sum(counter))
    
    print(oup)
    
    colnames(oup) <- c("name_district", var_name)
  }
  
  return(oup)
}

hex_sp <- aopdata::read_access(city = "spo", geometry = T) %>%
  select(id_hex, P001:R003) %>% st_transform(31983)

variaveis <- tibble(
  name = list.files("vars"),
  shp = list.files("vars", recursive = T, pattern = "shp", full.names = T)
) %>%  
  mutate(hex = map2(shp, name, function(x, y) to_hex(st_read(x), hex_sp, y, "point")))

variaveis_by_district <- tibble(
  name = list.files("vars"),
  shp = list.files("vars", recursive = T, pattern = "shp", full.names = T)
) %>%  
  mutate(hex = map2(shp, name, function(x, y) to_hex(st_read(x), sp_by_district %>% st_transform(31983), y, "point")))

# Input ciclovias, parques, favelas ---------

ciclo <- st_read("vars/transp_ciclovia/SIRGAS_SHP_redecicloviaria.shp") %>% 
  st_set_crs(31983)

ciclo_district <- sp_by_district %>% st_transform(31983) %>% 
    st_join(ciclo %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(name_district) %>% summarise(m = sum(rc_ext_t, na.rm = T))

ciclo_hex <- hex_sp %>% st_transform(31983) %>% 
  st_join(ciclo %>% mutate(counter = 1),
          st_intersects, left = T) %>% 
  st_drop_geometry() %>% 
  group_by(rc_nome) %>% 
  mutate(section_ext = rc_ext_t / n()) %>% 
  group_by(id_hex) %>% summarise(m = sum(section_ext, na.rm = T))

parques <- st_read("vars/verde_parquemunicipal/SIRGAS_SHP_parquemunicipal.shp") %>% 
  st_set_crs(31983) %>% mutate(pq_area = as.numeric(pq_area))

pq_district <- sp_by_district %>% st_transform(31983) %>% 
    st_join(parques %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    group_by(pq_id) %>% 
    mutate(section_area = pq_area / n()) %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(name_district) %>% summarise(m2 = sum(section_area, na.rm = T))

pq_hex <- hex_sp %>% st_transform(31983) %>% 
    st_join(parques %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    group_by(pq_id) %>% 
    mutate(section_area = pq_area / n()) %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(id_hex) %>% summarise(m2 = sum(section_area, na.rm = T))

favelas <- st_read("vars/cfln_favela/SIRGAS_SHP_favela.shp") %>% 
  st_set_crs(31983) %>% mutate(area = st_area(.) %>% as.numeric()) 

fv_district <- sp_by_district %>% st_transform(31983) %>% 
    st_join(favelas %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(name_district) %>% summarise(m2 = sum(area, na.rm = T))

fv_hex <- hex_sp %>% st_transform(31983) %>% 
    st_join(favelas %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    group_by(fv_id) %>% 
    mutate(section_area = area / n()) %>% 
    st_drop_geometry() %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(id_hex) %>% summarise(m2 = sum(section_area, na.rm = T))

# Input crimes ---------

sp_roubo_celular = list.files('zip-tony', pattern = '*ROUBO.*CELULAR.*tsv', full.names = T)
sp_furto_celular = list.files('zip-tony', pattern = '*FURTO.*CELULAR.*tsv', full.names = T)
oc_roubo_celular = import_ssp(sp_roubo_celular)
oc_furto_celular = import_ssp(sp_furto_celular)

roubos <- oc_roubo_celular %>% 
  filter(grepl("Roubo", RUBRICA)) %>% 
  distinct(ANO_BO, NUM_BO, CIDADE, DELEGACIA_CIRCUNSCRICAO, DATAOCORRENCIA,
           HORAOCORRENCIA, PERIDOOCORRENCIA, BAIRRO, LATITUDE, LONGITUDE, DESCRICAOLOCAL) %>% 
  filter(CIDADE == "S.PAULO") %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326, na.fail = F) %>% 
  st_transform(31983)
  
furtos <- oc_furto_celular %>% 
  filter(grepl("Furto", RUBRICA)) %>% 
  distinct(ANO_BO, NUM_BO, CIDADE, DELEGACIA_CIRCUNSCRICAO, DATAOCORRENCIA,
           HORAOCORRENCIA, PERIDOOCORRENCIA, BAIRRO, LATITUDE, LONGITUDE, DESCRICAOLOCAL) %>% 
  filter(CIDADE == "S.PAULO") %>% 
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326, na.fail = F) %>% 
  st_transform(31983)

roubo_district <- sp_by_district %>% st_transform(31983) %>% 
    st_join(roubos %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(name_district) %>% summarise(roubos = sum(counter))

roubo_hex <- hex_sp %>% st_transform(31983) %>% 
    st_join(roubos %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(id_hex) %>% summarise(roubos = sum(counter))

furto_district <- sp_by_district %>% st_transform(31983) %>% 
    st_join(furtos %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(name_district) %>% summarise(furtos = sum(counter))

furto_hex <- hex_sp %>% st_transform(31983) %>% 
    st_join(furtos %>% mutate(counter = 1),
            st_intersects, left = T) %>% 
    st_drop_geometry() %>% 
    mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
    group_by(id_hex) %>% summarise(furtos = sum(counter))

# Agregar -----------  

final <- hex_sp %>%
  left_join(plyr::join_all(variaveis$hex, by = "id_hex"), by = "id_hex") %>% 
  mutate(transp_ciclovia = ciclo_hex$m,
         verde_parquemunicipal = pq_hex$m2,
         crime_furto = furto_hex$furtos,
         crime_roubo = roubo_hex$roubos,
         cfln_favela = fv_hex$m2)


final_distritos <- sp_by_district %>% st_transform(31983) %>% 
  left_join(plyr::join_all(variaveis_by_district$hex, by = "name_district"), by = "name_district") %>% 
  nngeo::st_remove_holes() %>% mutate(transp_ciclovia = ciclo_district$m,
                                      verde_parquemunicipal = pq_district$m2,
                                      crime_furto = furto_district$furtos,
                                      crime_roubo = roubo_district$roubos,
                                      cfln_favela = fv_district$m2)


numeric_only_hex <- final %>% st_drop_geometry() %>% select(-id_hex) %>% as_tibble() %>% 
  mutate(
    #dummy_favela = ifelse(cfln_favela >= 1, 1, 0), 
    prop_negros = P003/P001, prop_brancos = P002/P001)
numeric_only_district <- final_distritos %>% st_drop_geometry() %>% select(-name_district) %>% as_tibble()


cor_district <- numeric_only_district %>%
  cor(use="complete.obs")
vars_corplot <- cor_district[,c("renda", "pop_white")] %>% rownames()

cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white)

cor_hex <- numeric_only_hex %>%
  rename(renda = R001) %>% 
  cor(use="complete.obs")
vars_corplot_2 <- cor_hex[,c("renda", "prop_brancos")] %>% rownames()

cor_hex[,c("renda", "prop_brancos")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot_2) %>% 
  select(var, renda, prop_brancos)
%>% 
  write_csv("referencias_fernanda/corplot_filteredhex.csv")

#filled_hexes <- colSums(numeric_only!=0) %>% as.list() %>% as_tibble()
filled_hexes <- read_csv("filled_hexes.csv")

numeric_only_hex %>% select(all_of(filled_hexes %>% filter(one_in_50) %>% .$variable), prop_negros, prop_brancos) %>% 
  cor(use="complete.obs") %>% corrplot::corrplot(method = "circle")

numeric_only_hex %>% select(P001, R001, prop_negros, dummy_favela) %>%
  cor(use="complete.obs") %>% corrplot::corrplot(method = "number")

numeric_only_district %>% cor(use="complete.obs") %>% corrplot::corrplot(method = "circle", type = "lower")


numeric_only_hex %>% select(all_of(filled_hexes %>% filter(one_in_50) %>% .$variable)) %>% View()

# Abastecimento -----------
abs_feira <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/abastecimento_feiraslivres)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = -1, guide = "legend") +
  labs(fill = "Habitantes\npor\nfeira livre") 

abs_bomprato <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = abastecimento_bomprato)) + theme_void() +
  scale_fill_distiller(palette = "Oranges", direction = 1, guide = "legend", breaks = c(0, 1, 2)) +
  labs(fill = "Bom prato") 

abs_sacolao <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = abastecimento_sacolao)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend", breaks = c(0,1,2)) +
  labs(fill = "Sacolao") 

abs_mm <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = abastecimento_mercadosmunicipais)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend") +
  labs(fill = "Mercado Municipal") 

cowplot::plot_grid(abs_feira, abs_bomprato, abs_sacolao, abs_mm)

final_distritos %>% select(name_district, starts_with("abastecimento")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/abastecimento.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, starts_with("abastecimento")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

numeric_only_district %>% mutate(pop_negra = pop_preta+pop_parda) %>% 
  select(pop, renda, pop_negra, starts_with("abastecimento")) %>%
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Cultura --------------
cul_bib <- ggplot() +
  geom_sf(data = final_distritos %>% 
            mutate(bibliotecas = case_when(
              cultura_bibliotecas > 5 ~ 5,
              T ~ cultura_bibliotecas
            )),
          aes(fill = bibliotecas)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend",
                       breaks = c(0, 1, 2, 3, 4, 5),
                       labels = c(0, 1, 2, 3, 4, "5+")
                       ) +
  labs(fill = "Bibliotecas públicas") 

cul_espacocul <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = cultura_espacosculturais)) + theme_void() +
  scale_fill_distiller(palette = "Oranges", direction = 1, guide = "legend",
                       trans = "log10", breaks = c(1, 5, 10, 30, 60)) +
  labs(fill = "Espaços Culturais") 

cul_museu <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = cultura_museus)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend", trans="log10") +
  labs(fill = "Museus") 

cul_teatrocinemashow <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/cultura_teatrocinemashow)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = -1, guide = "legend",
                       trans = "log10", breaks = c(1000, 10000, 100000),
                       labels = c("Mil", "Dez mil", "Cem mil")) +
  labs(fill = "Habitantes para cada \nteatro, cinema ou casa de shows") 

cul_monumento <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = patricultural_monumento)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend", trans="log10") +
  labs(fill = "Monumentos") 

cowplot::plot_grid(cul_bib, cul_espacocul, cul_museu, cul_teatrocinemashow)

final_distritos %>% select(name_district, starts_with("cultura"), patricultural_monumento) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/cultura.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, starts_with("cultura"), patricultural_monumento) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Digital ------------
dig_pracawifi <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = digital_pracawifi)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend",
                       trans = "log10", breaks = c(1, 2, 4, 8)) +
  labs(fill = "Praças com\nwifi público") 

dig_telecentro <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = digital_telecentro)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend",
                      trans = "log10", breaks = c(1, 2, 4, 8)
                       ) +
  labs(fill = "Telecentros") 

cowplot::plot_grid(dig_pracawifi, dig_telecentro)

final_distritos %>% select(name_district, starts_with("digital")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/digital.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, starts_with("digital")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Direitos Humanos --------------
dh_cmcradol <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/dirhum_cmcriancaadol)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = -1, guide = "legend")+
  labs(fill = "Habitantes para cada \ninstituição de apoio\na Criança e Adolescente") 

dh_ctutel <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = dirhum_conselhotutelar)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(fill = "Conselho Tutelar")

dh_protmul <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = dirhum_protecaomulher)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend",
                       breaks = c(0,1,2))+
  labs(fill = "Centro da Rede de\nProteção a Mulher")

dh_redeserv <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = dirhum_redeservicos)) + theme_void() +
  scale_fill_distiller(palette = "Oranges", direction = 1, guide = "legend",
                       breaks = c(0,1,2, 4))+
  labs(fill = "Órgão da rede de serviço\nde direitos humanos")

cowplot::plot_grid(dh_cmcradol, dh_ctutel, dh_protmul, dh_redeserv)

final_distritos %>% select(name_district, starts_with("dirhum")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/dirhum.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, starts_with("dirhum")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Educação -------------
ed_ceu <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_ceu)) + theme_void() +
  scale_fill_distiller(palette = "Purples", direction = 1, guide = "legend",
                       breaks = c(0,1,2))+
  labs(fill = "CEUs") 

ed_eipub <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_edinfantilpublica)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend")+
  labs(fill = "Instituições públicas de\nEd. Infantil por distrito") 

ed_etpub <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_ensinotecnicopublico)) + theme_void() +
  scale_fill_distiller(palette = "Oranges", direction = 1, guide = "legend")+
  labs(fill = "Instituição pública de\nEnsino Técnico") 

ed_fmpub <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_fundmediopublica)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend")+
  labs(fill = "Instituição pública de\nEnsino Fund. e Médio") 

ed_priv <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_redeprivada)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(fill = "Instituição privada de educação") 

ed_sss <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_senaisesisenac)) + theme_void() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, guide = "legend")+
  labs(fill = "Senai Sesi ou Senac") 

ed_outros <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_outros)) + theme_void() +
  scale_fill_distiller(palette = "Greys", direction = 1, guide = "legend")+
  labs(fill = "Educação outros (CASA, etc)") 

cowplot::plot_grid(ed_eipub, ed_fmpub, ed_priv, ed_etpub, ed_sss, ed_ceu, ed_outros)

ed_eipub_pop <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/educacao_edinfantilpublica)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = -1, guide = "legend",
                       breaks = c(2500, 5000, 10000, 20000))+
  labs(fill = "Habitantes por\nInstituição pública de\nEd. Infantil") 

ed_fmpub_pop <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/educacao_fundmediopublica)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = -1, guide = "legend")+
  labs(fill = "Habitantes por\nInstituição pública de\nEnsino Fund. e Médio") 

ed_priv_pop <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/educacao_redeprivada)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = -1, guide = "legend")+
  labs(fill = "Habitantes por\nInstituição privada de educação") 

cowplot::plot_grid(ed_eipub_pop, ed_fmpub_pop, ed_priv_pop)

final_distritos %>% select(name_district, starts_with("educacao")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/educacao.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, starts_with("educacao")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Esporte ------------
esp_centro <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = esporte_centroesportivo)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend")+
  labs(fill = "Centros esportivos") 

esp_clube <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = esporte_clube)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(fill = "Clubes") 

esp_ccm <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = esporte_clubedacomnunidade)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend")+
  labs(fill = "Clubes da comunidade") 

esp_est <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = esporte_estadio)) + theme_void() +
  scale_fill_distiller(palette = "Oranges", direction = 1, guide = "legend",
                       breaks = c(0,1,2))+
  labs(fill = "Estádios") 

cowplot::plot_grid(esp_centro, esp_clube, esp_ccm, esp_est)

final_distritos %>% select(name_district, starts_with("esporte")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/esporte.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, starts_with("esporte")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Proteção e Defesa Civil ----------

pd_ac <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = protdefcivil_areascontaminadas)) + theme_void() +
  scale_fill_distiller(palette = "Oranges", direction = 1, guide = "legend")+
  labs(fill = "Áreas contaminadas") 

pd_rg <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = protdefcivil_riscogeologico)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend")+
  labs(fill = "Pontos de Risco Geológico") 

pd_ro <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = protdefcivil_riscoocorrencia)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(fill = "Ocorrências em 2021\n(Deslizamentos,\n enchentes, etc.)") 

cowplot::plot_grid(pd_ac, pd_rg, pd_ro)

final_distritos %>% select(name_district, starts_with("protdefcivil")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/protdefcivil.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, dummy_favela, starts_with("protdefcivil")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Segurança ----------
sg_bomb <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = seguranca_bombeiro)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend")+
  labs(fill = "Estações de bombeiros") 

sg_pm <- ggplot() +
  geom_sf(data = final_distritos %>% 
            mutate(pm = case_when(
              seguranca_policiamilitar > 9 ~ 10,
              seguranca_policiamilitar == 0 ~ NA_real_,
              T ~ seguranca_policiamilitar 
            )),
          aes(fill = pm)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend",
                       breaks = c(0, 1, 5, 10), labels = c(0, 1, 5, "10+"))+
  labs(fill = "Batalhões da PM") 

sg_pc <- ggplot() +
  geom_sf(data = final_distritos %>% 
            mutate(pm = case_when(
              seguranca_policiacivil > 9 ~ 10,
              seguranca_policiacivil == 0 ~ NA_real_,
              T ~ seguranca_policiacivil 
            )),
          aes(fill = pm)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend",
                       breaks = c(0, 1, 5, 10), labels = c(0, 1, 5, "10+"))+
  labs(fill = "Delegacias de polícia civil") 

sg_cm <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = seguranca_casademediacao)) + theme_void() +
  scale_fill_distiller(palette = "Purples", direction = 1, guide = "legend", breaks = c(0,1))+
  labs(fill = "Casas de mediação") 

cowplot::plot_grid(sg_bomb, sg_pm, sg_pc, sg_cm)

final_distritos %>% select(name_district, starts_with("seguranca")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/seguranca.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, dummy_favela, starts_with("seguranca")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Serviços ---------
sv_cat <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = servico_cat)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend",
                       breaks = c(0,1))+
  labs(fill = "Centros de Apoio\nao Trabalhador") 

sv_cemi <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = servico_cemiterio)) + theme_void() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, guide = "legend")+
  labs(fill = "Cemitérios") 

sv_corr <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = servico_correios)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(fill = "Agências do\nCorreios") 

sv_corr_pop <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/servico_correios)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = -1, guide = "legend")+
  labs(fill = "Habitantes por agência do Correios") 

sv_eco <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = servico_ecopontos)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend")+
  labs(fill = "Ecopontos") 

sv_pt <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = servico_poupatempo)) + theme_void() +
  scale_fill_distiller(palette = "Purples", direction = 1, guide = "legend",
                       breaks = c(0,1))+
  labs(fill = "Poupa Tempo") 

sv_sabesp <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = servico_sabesp)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend",
                       breaks = c(0,1))+
  labs(fill = "Centro de\natendimento\nda SABESP") 

cowplot::plot_grid(sv_cat, sv_cemi, sv_corr, sv_eco, sv_pt, sv_sabesp)

final_distritos %>% select(name_district, starts_with("servico")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/servicos.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, dummy_favela, starts_with("servico")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Transporte ----------
tr_onibus <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_pontoonibus)) + theme_void() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, guide = "legend")+
  labs(fill = "Pontos\nde Ônibus") 

tr_onibus_pop <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_pontoonibus*1000/pop)) + theme_void() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, guide = "legend")+
  labs(fill = "Pontos\nde Ônibus\npor mil habitantes") 

tr_metro <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_estacaometro)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend")+
  labs(fill = "Estações\nde Metrô") 

tr_trem <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_estacaotrem)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(fill = "Estações\nde Trêm") 

tr_ciclo <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_ciclovia)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend",
                       labels = scales::label_number_si(unit = "m"))+
  labs(fill = "Comprimento\nde ciclovias") 

tr_heli <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_rampaheliponto)) + theme_void() +
  scale_fill_distiller(palette = "Purples", direction = 1, guide = "legend",
                       breaks = c(0, 1, 3, 5, 9))+
  labs(fill = "Helipontos") 

cowplot::plot_grid(tr_onibus, tr_metro, tr_trem, tr_ciclo, tr_heli)

final_distritos %>% select(name_district, starts_with("transp")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/transporte.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, dummy_favela, starts_with("transp")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Meio Ambiente ----------

vd_arvores <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/verde_arvores)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = -1, guide = "legend")+
  labs(fill = "Habitantes\npor árvore") 

vd_parque <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = verde_parquemunicipal/pop)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend",
                       trans = "log10", breaks = c(0.1, 1, 10, 50))+
  labs(fill = "Metros quadrados\nde parque por\nhabitante") 

cowplot::plot_grid(vd_arvores, vd_parque)

final_distritos %>% select(name_district, starts_with("verde")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/verde.csv")

numeric_only_hex %>%
  select(P001, R001, prop_negros, dummy_favela, starts_with("verde")) %>%
  rename(populacao = P001, renda = R001) %>% 
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Crimes --------

cr_roubo <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = crime_roubo*100/pop)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend",
                       trans = "log10")+
  labs(fill = "Roubos por 100\nhabitantes (2015-2020)") 

cr_furto <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = crime_furto*100/pop)) + theme_void() +
  scale_fill_distiller(palette = "Oranges", direction = 1, guide = "legend",
                       trans = "log10")+
  labs(fill = "Furtos por 100\nhabitantes (2015-2020)") 

cowplot::plot_grid(cr_furto, cr_roubo)

final_distritos %>% select(name_district, starts_with("crime")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/crime.csv")

numeric_only_district %>%
  select(pop, renda, starts_with("crime")) %>%
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Habitação ---------
cfln_fv <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = cfln_favela/1000000)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend") +
  labs(fill = "Área (km2) ocupada\npor favelas") 

final_distritos %>% select(name_district, starts_with("cfln")) %>%
  st_drop_geometry() %>%
  as_tibble() %>% write_csv("referencias_fernanda/habitacao.csv")

numeric_only_district %>%
  select(pop, renda, pop_parda, pop_preta, starts_with("cfln")) %>%
  cor(use="complete.obs") %>%
  corrplot::corrplot(method = "circle", diag = F, tl.col = "grey30")

# Mapas matéria -------------


ed_eipub <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_edinfantilpublica)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend")+
  labs(title = "Educação Infantil")+
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        plot.caption.position = 'panel',
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  
ed_fmpub <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = educacao_fundmediopublica)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend",
                       breaks = c(15, 30, 45, 60))+
  labs(title = "Fundamental e Médio") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        plot.caption.position = 'panel',
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  
educacao <- cowplot::plot_grid(ed_eipub, ed_fmpub)+
  ggtitle("Escolas públicas por distrito")+
  theme_pindograma_map()

sg_pm <- ggplot() +
  geom_sf(data = final_distritos %>% 
            mutate(pm = case_when(
              seguranca_policiamilitar > 9 ~ 10,
              seguranca_policiamilitar == 0 ~ NA_real_,
              T ~ seguranca_policiamilitar 
            )),
          aes(fill = pm)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend",
                       breaks = c(0, 1, 5, 10), labels = c(0, 1, 5, "10+"))+
  labs(title = "Polícia Militar") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  

sg_pc <- ggplot() +
  geom_sf(data = final_distritos %>% 
            mutate(pm = case_when(
              seguranca_policiacivil > 9 ~ 10,
              seguranca_policiacivil == 0 ~ NA_real_,
              T ~ seguranca_policiacivil 
            )),
          aes(fill = pm)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend",
                       breaks = c(0, 1, 5, 10), labels = c(0, 1, 5, "10+"))+
  labs(title = "Polícia Civil")+
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  
seguranca <- cowplot::plot_grid(sg_pc, sg_pm)+
  ggtitle("Delegacias e batalhões por distrito")+
  theme_pindograma_map()

dh_ctutel <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = dirhum_conselhotutelar)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(title = "Conselho Tutelar")+
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  

dh_protmul <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = dirhum_protecaomulher)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend",
                       breaks = c(0,1,2))+
  labs(title= "Centro de\nProteção à Mulher")+
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  
direitos_humanos <- cowplot::plot_grid(dh_ctutel, dh_protmul)+
  ggtitle("Instituições de direitos humanos por distrito")+
  theme_pindograma_map()

dig_pracawifi <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = digital_pracawifi)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend",
                       trans = "log10", breaks = c(1, 2, 4, 8)) +
  labs(title = "Praças com wifi") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  

dig_telecentro <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = digital_telecentro)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend",
                      trans = "log10", breaks = c(1, 2, 4, 8)
                       ) +
  labs(title = "Telecentros") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
  

inclusao_digital <- cowplot::plot_grid(dig_telecentro, dig_pracawifi)+
  ggtitle("Serviços de inclusão digital por distrito")+
  theme_pindograma_map()

esp_centro <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = esporte_centroesportivo)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend")+
  labs(title = "Centros esportivos") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

esp_ccm <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = esporte_clubedacomnunidade)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend")+
  labs(title = "Clubes da comunidade") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

esportes <- cowplot::plot_grid(esp_centro, esp_ccm)+
  ggtitle("Serviços de esporte e lazer por distrito")+
  theme_pindograma_map()

vd_arvores <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/verde_arvores)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = -1, guide = "legend")+
  labs(title = "Habitantes por árvore") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


vd_parque <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = verde_parquemunicipal/pop)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend",
                       trans = "log10", breaks = c(0.1, 1, 10, 50))+
  labs(title = "Área (m2)\nde parque por habitante") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

verde <- cowplot::plot_grid(vd_arvores, vd_parque)+
  ggtitle("Áreas verdes por distrito")+
  theme_pindograma_map()

sg_bomb <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = seguranca_bombeiro)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend", 
                       trans = "log10")+
  labs(title = "Serviço de bombeiros por distrito",
         fill = "Estações") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

cul_museu <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = cultura_museus)) + theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, guide = "legend", trans="log10") +
  labs(title = "Museus")+ 
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

cul_teatrocinemashow <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/cultura_teatrocinemashow)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = -1, guide = "legend",
                       trans = "log10",
                       breaks = c(1000, 10000, 100000),
                       labels = c("Mil", "Dez mil", "Cem mil")
                       ) +
  labs(title = "Habitantes para cada\nteatro ou cinema")+ 
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

cultura <- cowplot::plot_grid(cul_museu, cul_teatrocinemashow)+
  ggtitle("Ofertas culturais por distrito")+
  theme_pindograma_map()

sv_corr <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = servico_correios)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend")+
  labs(title = "Agências") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


sv_corr_pop <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = pop/servico_correios)) + theme_void() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, guide = "legend",
                       breaks = c(100000,200000,300000),
                       labels = c("100 mil", "200 mil", "300 mil")
                       )+
  labs(title = "Habitantes por agência") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

correios <- cowplot::plot_grid(sv_corr, sv_corr_pop)+
  ggtitle("Serviço dos Correios por distrito")+
  theme_pindograma_map()

tr_onibus <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_pontoonibus)) + theme_void() +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, guide = "legend")+
  labs(title = "Pontos de Ônibus")+ 
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

tr_metro <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = transp_estacaometro+transp_estacaotrem)) + theme_void() +
  scale_fill_distiller(palette = "Greens", direction = 1, guide = "legend")+
  labs(title = "Estações de\nTrem ou Metrô")+ 
  theme_pindograma()+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

transporte <- cowplot::plot_grid(tr_metro, tr_onibus)+
  ggtitle("Transporte público por distrito")+
  theme_pindograma_map()

cfln_fv <- ggplot() +
  geom_sf(data = final_distritos,
          aes(fill = cfln_favela/1000000)) + theme_void() +
  scale_fill_distiller(palette = "Blues", direction = 1, guide = "legend") +
  labs(fill = "Área (km2) ocupada\npor favelas", title="Favelas por distrito") +
  theme_pindograma()+
  theme(panel.background = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# Tabelas matéria --------------
edu_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"edu")) %>% 
  mutate(var = case_when(
    var == "educacao_ceu" ~ "Centro Educacional Unificado",
    var == "educacao_edinfantilpublica" ~ "Educação Infantil",
    var == "educacao_ensinotecnicopublico" ~ "Técnico Público",
    var == "educacao_fundmediopublica" ~ "Fundamental e Médio",
    var == "educacao_senaisesisenac" ~ "Senai, Sesi ou Senac",
  )) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% filter(!is.na(var)) %>% 
  select(-pop_white) %>% 
  rename(`Escola pública` = var, `Correlação com renda média do distrito` = renda)

seg_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"segu") | str_detect(var,"crime")) %>% 
  mutate(var = case_when(
    var == "seguranca_policiacivil" ~ "Delegacia de polícia civil",
    var == "seguranca_policiamilitar" ~ "Batalhão de polícia militar",
    var == "seguranca_policiamilitar" ~ "Batalhão de polícia militar",
    var == "crime_roubo" ~ "Roubos",
    var == "crime_furto" ~ "Furtos"
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Infraestrutura policial ou crime` = var, `Correlação com renda média do distrito` = renda)

dh_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"dirhum")) %>% 
  mutate(var = case_when(
    var == "dirhum_cmcriancaadol" ~ "Instituição de apoio à criança e ao adolescente",
    var == "dirhum_conselhotutelar" ~ "Conselho Tutelar",
    var == "dirhum_protecaomulher" ~ "Centro da Rede de Proteção à Mulher"
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Serviço de direitos humanos` = var, `Correlação com renda média do distrito` = renda)

digi_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"digital")) %>% 
  mutate(var = case_when(
    var == "digital_pracawifi" ~ "Praça com wifi",
    var == "digital_telecentro" ~ "Telecentro"
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Serviço de inclusão digital` = var, `Correlação com renda média do distrito` = renda)

esp_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"espor")) %>% 
  mutate(var = case_when(
    var == "esporte_centroesportivo" ~ "Centro esportivo",
    var == "esporte_clubedacomnunidade" ~ "Clube da comunidade",
    var == "esporte_clube" ~ "Clube privado"
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Serviço de esporte e lazer` = var, `Correlação com renda média do distrito` = renda)

vd_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"verde")) %>% 
  mutate(var = case_when(
    var == "verde_arvores" ~ "Árvores nas vias públicas",
    var == "verde_parquemunicipal" ~ "Parques municipais"
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Áreas verdes` = var, `Correlação com renda média do distrito` = renda)

bombeiro_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"bomb")) %>% 
  mutate(var = case_when(
    var == "seguranca_bombeiro" ~ "Estação do corpo de bombeiros",
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Serviço de bombeiros` = var, `Correlação com renda média do distrito` = renda)

cult_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"cultura_")) %>% 
  mutate(var = case_when(
    var == "cultura_bibliotecas" ~ "Bibliotecas e pontos de leitura",
    var == "cultura_museus" ~ "Museus",
    var == "cultura_espacosculturais" ~ "Espaços culturais",
    var == "cultura_teatrocinemashow" ~ "Teatros, cinemas e casas de shows",
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Ofertas culturais` = var, `Correlação com renda média do distrito` = renda)

correios_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"correio")) %>% 
  mutate(var = case_when(
    var == "servico_correios" ~ "Agências"
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Serviço dos Correios` = var, `Correlação com renda média do distrito` = renda)

transp_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"transp")) %>% 
  mutate(var = case_when(
    var == "transp_estacaometro" ~ "Estações de metrô",
    var == "transp_estacaotrem" ~ "Estações de trem",
    var == "transp_pontoonibus" ~ "Pontos de ônibus"
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Serviço de transporte público` = var, `Correlação com renda média do distrito` = renda)

habit_table <- cor_district[,c("renda", "pop_white")] %>%
  as_tibble() %>%
  mutate(var = vars_corplot) %>% 
  select(var, renda, pop_white) %>% 
  filter(str_detect(var,"favel")) %>% 
  mutate(var = case_when(
    var == "cfln_favela" ~ "Área de favela",
  )) %>% 
  filter(!is.na(var)) %>% 
  mutate(renda = round(renda, digits = 2), pop_white = round(pop_white, digits = 2)) %>% 
  arrange(renda) %>% 
  select(-pop_white) %>% 
  rename(`Arranjo habitacional` = var, `Correlação com renda média do distrito` = renda)

# Save rdata -----------

save(list = c('edu_table', 'educacao', 'seg_table', 'seguranca', 'dh_table',
              'direitos_humanos', 'digi_table', 'inclusao_digital', 'esp_table',
              'esportes', 'vd_table', 'verde', 'bombeiro_table', 'sg_bomb', 
              'cult_table', 'cultura', 'correios_table', 'correios', 
              'transp_table', 'transporte', 'habit_table', 'cfln_fv'),
     file = './sp.Rdata')
