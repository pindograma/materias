st_found = function(x) {
    lengths(x) != 0
}

st_unlist = function(x) {
    is.na(x) = lengths(x) == 0
    unlist(x)
}

make_points_sfc = function(lat, lon) {
    map2(lat,
         lon,
         function(x, y) { st_point(x = c(y, x)) }) %>%
    st_sfc(crs = 4326) %>%
    st_transform(EPSG)
}

gen_tract = function(cid, d, s, c) {
    paste0(
        cid,
        str_pad(d, 2, pad = '0'),
        str_pad(s, 2, pad = '0'),
        str_pad(c, 4, pad = '0'))
}

genpoints_ = function(tracts, geocoded_secoes) {
    tracts = tracts %>% mutate(rn = row_number())
    
    geocoded_secoes = geocoded_secoes %>%
        mutate(
            lat = case_when(
                !is.na(comp_tse_lat) ~ comp_tse_lat,
                !is.na(tse_lat) ~ tse_lat,
                !is.na(inep_lat) ~ inep_lat,
                !is.na(google_lat) ~ google_lat,
                !is.na(places_lat) ~ places_lat,
                T ~ NA_real_),
            lon = case_when(
                !is.na(comp_tse_lon) ~ comp_tse_lon,
                !is.na(tse_lon) ~ tse_lon,
                !is.na(inep_lon) ~ inep_lon,
                !is.na(google_lon) ~ google_lon,
                !is.na(places_lon) ~ places_lon,
                T ~ NA_real_)) %>%
        mutate(code_tract = case_when(
            !is.na(pl_Distrito) & !is.na(pl_Subdistrito) & !is.na(pl_CodSetor) ~
                gen_tract(codigo_ibge, pl_Distrito, pl_Subdistrito, pl_CodSetor),
            !is.na(ad_Distrito) & !is.na(ad_Subdistrito) & !is.na(ad_CodSetor) ~
                gen_tract(codigo_ibge, ad_Distrito, ad_Subdistrito, ad_CodSetor),
            !is.na(rural_Distrito) & !is.na(rural_Subdistrito) & !is.na(rural_CodSetor) ~
                gen_tract(codigo_ibge, rural_Distrito, rural_Subdistrito, rural_CodSetor),
            T ~ NA_character_))
    
    points = make_points_sfc(geocoded_secoes$lat, geocoded_secoes$lon)
    geocoded_secoes$setor_rn = st_within(points, tracts) %>% st_unlist()
    
    tracts_joined_1 = inner_join(tracts, geocoded_secoes, by = c('rn' = 'setor_rn')) %>%
        select(-code_tract.y) %>%
        rename(code_tract = code_tract.x)
    tracts_joined_2 = inner_join(
        tracts,
        geocoded_secoes %>% filter(is.na(lat) | is.na(lon)),
        by = c('code_tract' = 'code_tract'))
    tracts_joined = rbind(tracts_joined_1, tracts_joined_2 %>% select(-setor_rn))
    
    list(rbind(
        st_transform(st_as_sf(
            tracts_joined %>% st_drop_geometry() %>% filter(!is.na(lat)),
            crs = 4326, coords = c('lon', 'lat'), remove = F), EPSG),
        st_centroid(tracts_joined %>% filter(is.na(lat))) %>%
            rename(geometry = geom)
    ), tracts_joined)
}

genpoints = function(tracts, geocoded_secoes) {
    genpoints_(tracts, geocoded_secoes)[[1]]
}