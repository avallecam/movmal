
# contexto ----------------------------------------------------------------

# proyecto: MCIE2018 tesis maestría
# input: data/mcie19_20190522.rds
# output: table/mcie19_(...).xlsx
# estatus: base limpia
# objetivo: tabla 1-2-3
# extra: diccionario de datos
#        kipuformas.com -> consultas -> catálogo de variables de estudio

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(compareGroups)
library(broom)
#skimr,psych,Hmisc

rm(list = ls())
theme_set(theme_bw())


# importar datos ----------------------------------------------------------

mcie19 <- read_rds("data/mcie19_20190522.rds")

mcie19 %>% glimpse()

# identify exposure and outcome -------------------------------------------

mcie19 %>% select(starts_with("sum_1805")) %>% skimr::skim()
mcie19 %>% select(starts_with("malhist_12m")) %>% skimr::skim() #tiempo al último evento diagnosticado?
mcie19 %>% select(oe_fieb1) %>% skimr::skim() # febriles como outcome?

#alternativas de Y
# - tiempo al último evento diagnosticado
# - proporción de febriles

# identify missings -------------------------------------------------------
#http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html

#naniar::gg_miss_var(mcie19)
naniar::miss_var_summary(mcie19)

# distribuciones ----------------------------------------------------------

skimr::skim(mcie19) # visual
psych::describe(mcie19 %>% select_if(is.numeric)) # skewness + kurtosis
Hmisc::describe(mcie19) # smallest, highest, proportion, frequency, type-flexible

mcie19 %>% 
  ggplot(aes(sample=edad)) + 
  stat_qq() +
  stat_qq_line()

mcie19 %>% 
  ggplot(aes(x=edad)) + 
  geom_histogram()

# data dictionary ---------------------------------------------------------

#        kipuformas.com -> consultas -> catálogo de variables de estudio
#        (https://kipuformas.com/consultas.php?Opc=15)

# tabla 1 -----------------------------------------------------------------

#library(compareGroups)

compareGroups(~ .,
              data = mcie19 %>% select(-cod_enrol,-sect_trab_oe) #,byrow=T 
              ,max.xlev = 20
              ,method = c(conv_cant = 2,
                          edad = 2,
                          mosq_cuan = 2,
                          mosq_dusc1 = 2,
                          mosq_ertm1 = 2,
                          mosq_tm1 = 2,
                          mp_numv = 2,
                          mpx_tlv_m = 2,
                          mpxf_tlv_m = 2,
                          sum_0005_f = 2,
                          sum_0005_s = 2,
                          sum_0005_t = 2,
                          sum_0611_f = 2,
                          sum_0611_s = 2,
                          sum_0611_t = 2,
                          sum_0617_f = 2,
                          sum_0617_s = 2,
                          sum_0617_t = 2,
                          sum_1217_f = 2,
                          sum_1217_s = 2,
                          sum_1217_t = 2,
                          sum_1805_f = 2,
                          sum_1805_s = 2,
                          sum_1805_t = 2,
                          sum_1823_f = 2,
                          sum_1823_s = 2,
                          sum_1823_t = 2,
                          tiem_domi = 2,
                          vl_cuantos = 2#,
              )
              ) %>% 
  createTable(show.all = T, show.n = T,digits = 1,sd.type = 2) %>% 
  export2xls("table/mcie19-tab1.xls")

compareGroups(~ sect_trab_oe,
              data = mcie19 #,byrow=T 
              ,max.xlev = 20) %>% 
  createTable()
  
# tabla 2: por grupos -----------------------------------------------------------------

compareGroups(group ~ sexo + edad +
                #num.visita + 
                hto. + leuco. + 
                abaston. + segment. + #neutrofilos
                eosinof. + basofil. +
                monocit. + linfocit. + plaqueta,
              data = mcie19 %>% filter(num.visita=="1") #, byrow=T 
              ,method = c(hto. = 2,
                          leuco. = 2,
                          abaston. = 2,
                          segment. = 2,
                          eosinof. = 2,
                          monocit. = 2,
                          linfocit. = 2, #cercana a normal
                          plaqueta = 2,
                          basofil.= 2, edad = 2
              )
) %>% 
  createTable(show.all = F, show.n = F, show.p.mul = T) #%>% 
#export2xls("table/z0-tab1_ind_r05.xls")

# complete case analysis --------------------------------------------------

#todos los missing están en falciparum!
#mcie19 %>% 
# select(group,x__1) %>% 
#filter(is.na(x__1)) %>% count(group)

mcie19_cc <- mcie19 %>% filter(complete.cases(.))
mcie19_cc %>% dim()

mcie19_cc %>% glimpse()
mcie19_cc %>% count(group)
mcie19_cc_viv <- mcie19_cc %>% 
  filter(num.visita=="1") %>% 
  filter(group!="pfal")
mcie19_cc_fal <- mcie19_cc %>% 
  filter(num.visita=="1") %>% 
  filter(group!="pviv")
mcie19_cc_viv %>% count(group)
mcie19_cc_fal %>% count(group)

# exectute model --------------------------------------------------------

#library(broom)

#mcie19_cc_viv %>% count(hto.)
glm.full <- glm(hto. ~ group + edad + sexo
                , data = mcie19_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()
