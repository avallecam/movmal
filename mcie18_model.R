
# contexto ----------------------------------------------------------------

# proyecto: MCIE2018 tesis maestría
# input: data/mcie19_20190522.rds
# updat: data/mcie19_20190705.rds

# output: table/mcie19_(...).xlsx
# estatus: base limpia
# objetivo: tabla 1-2-3
# extra: diccionario de datos
#        kipuformas.com -> consultas -> catálogo de variables de estudio

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(compareGroups)
library(broom)
library(labelled)
library(skimr)
library(rlang)
library(janitor)
library(avallecam)
#skimr,psych,Hmisc

rm(list = ls())
theme_set(theme_bw())


# importar datos ----------------------------------------------------------

#mcie19 <- read_rds("data/mcie19_20190522.rds")
#mcie19 <- read_rds("data/mcie19_20190705.rds")
#mcie19 <- read_rds("data/mcie19_20190913.rds")
mcie19 <- read_rds("data/mcie19_20191012.rds")

mcie19 %>% glimpse()

# data dictionary ---------------------------------------------------------

mcie19 %>% look_for() %>% as_tibble()

# identify exposure and outcome -------------------------------------------

mcie19 %>% select(starts_with("sum_1805")) %>% skim()
mcie19 %>% select(starts_with("malhist_12m")) %>% skim() #tiempo al último evento diagnosticado?
mcie19 %>% select(oe_fieb1) %>% skim() # febriles como outcome?
mcie19 %>% select(oe_fieb1) %>% look_for()
mcie19 %>% count(oe_fieb1)
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
#Hmisc::describe(mcie19) # smallest, highest, proportion, frequency, type-flexible

mcie19 %>% 
  ggplot(aes(sample=edad)) + 
  stat_qq() +
  stat_qq_line()

# https://stackoverflow.com/a/13609916/6702544
mcie19 %>% 
  ggplot(aes(x=edad)) + 
  geom_histogram(aes(y=..density..)) +
  #geom_density() +
  stat_function(fun = dnorm, 
                args = list(mean = mean(mcie19$edad,na.rm = T), 
                            sd = sd(mcie19$edad,na.rm = T)))

# linealidad --------------------------------------------------------------
# concluimos que si hay linealidad

mcie19 %>% 
  select(edad,malhist_12m) %>% 
  mutate(edad_cat=cut(edad,breaks = 4,include.lowest = T)) %>% 
  tabyl(edad_cat,malhist_12m) %>% 
  adorn_2x2()

mcie19 %>% 
  select(edad,malhist_12m) %>% 
  mutate(edad_cat=cut(edad,breaks = 3,include.lowest = T)) %>% 
  filter(!is.na(edad)) %>% 
  ggplot(aes(edad_cat,fill=malhist_12m)) +
  geom_bar(position = position_fill())

# _EPI-MODEL --------------------------------------------------------------

mcie19_epi <- mcie19 %>% 
  select(cod_enrol,edad,sexo,village,escu_nivel_c,
         casa_tipo,
         tiem_domi,mosq_cuan,insect_sino,
         sect_trab_all_act_prin,sect_trab_all_act_prin_cat,
         vpc_sino,vpl_sino,
         sum_1805_fc,sum_1805_sc,sum_1805_tc,
         mpu_dest1:mpu_dest8,mpuf_dest1:mpuf_dest8,
         mpu_tran1:mpu_tran6,mpuf_tran1:mpuf_tran6,
         mpx_tlv_m,mpxf_tlv_m,
         malhist_12m) %>% 
  mutate(sect_trab_all_act_prin=fct_infreq(sect_trab_all_act_prin)#,
         #sect_trab_all_act_prin=fct_lump_min(sect_trab_all_act_prin,min = 5)
         )

skimr::skim(mcie19_epi) # visual
psych::describe(mcie19_epi %>% select_if(is.numeric)) # skewness + kurtosis

#mcie19_epi %>% glimpse()

# __epi_tabla1 --------------------------------------------------------------

compareGroups(~ .,
              data = mcie19_epi %>% 
                select(-cod_enrol,
                       -(mpu_dest1:mpu_dest8),
                       -(mpuf_dest1:mpuf_dest8),
                       -(mpu_tran1:mpu_tran6),
                       -(mpuf_tran1:mpuf_tran6),
                       -mpx_tlv_m,-mpxf_tlv_m) 
              ,byrow=T 
              ,max.xlev = 20
              ,method = c(
                #conv_cant = 2,
                edad = 2,
                mosq_cuan = 1,
                #mosq_dusc1 = 2,
                #mosq_ertm1 = 2,
                #mosq_tm1 = 2,
                #mp_numv = 2,
                # mpx_tlv_m = 2,
                # mpxf_tlv_m = 2,
                # fin de semana + semana = total
                # sum_0005_f = 2, sum_0005_s = 2, sum_0005_t = 2,
                # sum_0611_f = 2, sum_0611_s = 2, sum_0611_t = 2,
                # sum_0617_f = 2, sum_0617_s = 2, sum_0617_t = 2,
                # sum_1217_f = 2, sum_1217_s = 2, sum_1217_t = 2,
                # sum_1805_f = 2, sum_1805_s = 2, sum_1805_t = 2,
                # sum_1823_f = 2, sum_1823_s = 2, sum_1823_t = 2,
                tiem_domi = 2#,
                #vl_cuantos = 2#,
              )
) %>% 
  createTable(show.all = T, show.n = T,digits = 1,sd.type = 2) %>% 
  export2xls("table/mcie19-epi-tab1.xls")

# __epi_tabla2 ------------------------------------------------------------

compareGroups(malhist_12m ~ .,
              data = mcie19_epi %>% 
                select(-cod_enrol,-sect_trab_all_act_prin,
                       -(mpu_dest1:mpu_dest8),
                       -(mpuf_dest1:mpuf_dest8),
                       -(mpu_tran1:mpu_tran6),
                       -(mpuf_tran1:mpuf_tran6),
                       -mpx_tlv_m,-mpxf_tlv_m) 
              ,byrow=T 
              ,max.xlev = 20
              ,method = c(
                #conv_cant = 2,
                edad = 2,sexo=2,
                mosq_cuan = 2,
                #mosq_dusc1 = 2,
                #mosq_ertm1 = 2,
                #mosq_tm1 = 2,
                #mp_numv = 2,
                mpx_tlv_m = 2,
                mpxf_tlv_m = 2,
                # fin de semana + semana = total
                # sum_0005_f = 2, sum_0005_s = 2, sum_0005_t = 2,
                # sum_0611_f = 2, sum_0611_s = 2, sum_0611_t = 2,
                # sum_0617_f = 2, sum_0617_s = 2, sum_0617_t = 2,
                # sum_1217_f = 2, sum_1217_s = 2, sum_1217_t = 2,
                # sum_1805_f = 2, sum_1805_s = 2, sum_1805_t = 2,
                # sum_1823_f = 2, sum_1823_s = 2, sum_1823_t = 2,
                tiem_domi = 2#,
                #vl_cuantos = 2#,
              )
) %>% 
  createTable(show.all = F, 
              #show.n = T,
              digits = 1,
              show.ratio = TRUE,show.p.ratio = F,
              sd.type = 2) #%>% 
  export2xls("table/mcie19-epi-tab2.xls")

# _BIO-MODEL --------------------------------------------------------------

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


compareGroups(malhist_12m ~ .,
              data = mcie19 %>% select(-cod_enrol,-sect_trab_oe) #,byrow=T 
              ,max.xlev = 20
              ,byrow = T
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
  createTable(show.all = T, show.n = T,
              digits = 1,sd.type = 2,
              show.p.mul = F) %>% 
  export2xls("table/mcie19-tab2.xls")

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
