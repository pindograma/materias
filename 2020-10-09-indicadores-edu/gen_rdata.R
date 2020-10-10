library(tidyverse)
library(countrycode)

#read csv containing all OECD educational indicators (last updated 2020)
#source: https://www.oecd-ilibrary.org/education/data/oecd-education-statistics_edu-data-en
#file: http://pindograma-dados.s3.amazonaws.com/reporting/oecd_edu_spending.csv
oecd_raw <- read_csv("oecd_edu_spending.csv")

#read csv containing all PISA scores since 2000
#source: https://en.wikipedia.org/wiki/Programme_for_International_Student_Assessment (pre-processed from two tables)
pisa_raw <- read_csv("pisa_scores.csv") %>%
  mutate(code = countrycode(code, origin = 'iso3c', destination = 'iso2c', nomatch = NULL) %>% tolower()) %>% 
  select(-Country)

#implement function that matches indicator year to nearest PISA
match_year <- function(y){
  pisa_years <- c(2000, 2003, 2006, 2009, 2012, 2015, 2018)
  Find(function(x) x >= y, pisa_years, nomatch = NA)
}
match_year <- Vectorize(match_year)
#paises_pt <- read_csv('traduzir.csv', locale = locale(encoding = 'UTF-8'))

#correct colors for latvia and qatar flags
modify_flags <- function() {
  #for windows users, path may not work
  path = str_c(.libPaths(), "/ggflags/data/lflags.rds", sep = "") 
  data(lflags, package = "ggflags")
  # 15% opacity gives 2B in the alpha channel
  .flaglist[["lv"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#C28FEF2B"
  # 33% opacity gives 54 in the alpha channel
  .flaglist[["qa"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#9450E054"
  save(.flaglist, file=path)
}

#filter OECD data for correct indicators, treat for matching
oecd_treated <- oecd_raw %>%
  filter(ISC11_LEVEL_CAT == 'T',
         REF_SECTOR == 'T',
         COUNTERPART_SECTOR == 'INST_T',
         EXPENDITURE_TYPE == 'T',
         INDICATOR %in% c('FIN_GDP', 'FIN_PERSTUD')) %>%
  rename(indicator_year = Year) %>% 
  mutate(code = countrycode(Country, origin = 'country.name',
                            destination = 'iso2c',
                            nomatch = NULL),
         Country = countrycode(code, origin = 'iso2c',
                               destination = 'country.name',
                               nomatch = NULL),
         code = code %>% tolower(),
         pisa_year = match_year(indicator_year)) %>%
  drop_na(Value) %>% 
  group_by(code, INDICATOR, pisa_year) %>% 
  filter(indicator_year == max(indicator_year)) %>% 
  ungroup() %>%
# mutate(Country = countrycode(Country, origin = 'en', destination = 'pt', custom_dict = paises_pt)) %>% 
  select(Country, code, pisa_year, indicator_year, INDICATOR, Indicator, Value)

#generate tibbles for 2015 GDP percent spending and per student spending by country  
gdp_2015 <- oecd_treated %>% filter(pisa_year == 2015, INDICATOR == 'FIN_GDP') %>% mutate(Value = Value/100)
student_2015 <- oecd_treated %>% filter(pisa_year == 2015, INDICATOR == 'FIN_PERSTUD')

#generate tibble matching PISA scores and indicators
master <- oecd_treated %>% left_join(pisa_raw, by = c('code', 'pisa_year' = 'Year'))
gdp_vs_pisa <- master %>% filter(!is.na(Average), INDICATOR == 'FIN_GDP', pisa_year == 2015) %>% mutate(Value = Value/100)
student_vs_pisa <- master %>% filter(!is.na(Average), INDICATOR == 'FIN_PERSTUD', pisa_year == 2015)

#generate figures for inline citation
pib_br <- gdp_2015 %>% filter(Country == 'Brazil') 
pib_br <- pib_br$Value * 100
pib_ir <- gdp_2015 %>% filter(Country == 'Ireland') 
pib_ir <- pib_ir$Value * 100
stu_br <- student_2015 %>% filter(Country == 'Brazil') 
stu_br <- stu_br$Value
stu_ir <- student_2015 %>% filter(Country == 'Ireland') 
stu_ir <- stu_ir$Value
nota_br <- gdp_vs_pisa %>% filter(Country == 'Brazil')
nota_br <- nota_br$Average
nota_ir <- gdp_vs_pisa %>% filter(Country == 'Ireland')
nota_ir <- nota_ir$Average

save(list = c('gdp_2015','student_2015','master', 'gdp_vs_pisa', 'student_vs_pisa', 'modify_flags',
              'pib_br', 'pib_ir', 'stu_br', 'stu_ir', 'nota_br', 'nota_ir'),
     file='indicadores-edu.Rdata')


