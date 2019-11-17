
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
naniar::miss_var_summary(mcie19) %>% filter(n_miss>0) %>% print_inf()

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

# plot E x O ---------------------------------------------

mcie19 %>%  
  select(malhist_12m,sum_0617_t,sum_1805_t) %>% 
  filter(!is.na(malhist_12m)) %>% 
  mutate(malhist_12m=fct_recode(malhist_12m,"Yes"="con","No"="sin")) %>% 
  count(malhist_12m,sum_0617_t,sum_1805_t,sort = T) %>% 
  ggplot(aes(sum_0617_t,sum_1805_t,colour=malhist_12m,size=n)) +
  geom_point(alpha=0.5) +
  facet_grid(~malhist_12m) +
  scale_size_continuous(range = c(3,8),breaks = c(1,10,25,50),labels = c(1,10,25,50)) +
  labs(#title = "Hours out of home and Malaria Episodes",
       colour="Malaria\nepisodes",size="# of\nsubjects",
       x="# hours between 06:00h - 17:59h",
       y="# hours between 18:00h - 05:59h")
ggsave("figure/05-mcie-hours_outcome.png",width = 5.6,height = 3,dpi = 600)

# table weekend-weekday ---------------------------------------------------

mcie19 %>% 
  select(cod_enrol,
         mpu_dest1:mpu_dest8,mpuf_dest1:mpuf_dest8, #02
         mpu_tran1:mpu_tran6,mpuf_tran1:mpuf_tran6, #03
         mpx_tlv_m,mpxf_tlv_m, #04
         ) %>% 
  mutate_all(.funs = as.character) %>% 
  pivot_longer(cols = -cod_enrol,names_to = "variable",values_to = "value") %>% 
  mutate(week_x=if_else(str_detect(variable,"f_"),"weekend","weekday")) %>% 
  filter(value!="") %>% 
  mutate(new_value=case_when(
    str_detect(variable,"_m") ~ value,
    TRUE ~ str_c(variable,"-",value)
  ),
  new_variable=if_else(
    str_detect(variable,"f_"),str_replace(variable,"f_","_"),variable
    )
  ) %>%  
  # count(new_variable) %>% print_inf()
  select(-variable,-new_value) %>% 
  pivot_wider(names_from = new_variable,values_from = value) %>% 
  mutate_at(.vars = vars(ends_with("_m")),.funs = as.numeric) %>% 
  select(-cod_enrol#,-week_x,-mpx_tlv_m
         ) %>% #colnames()
  #naniar::vis_miss()
  # # HERE I WOULD NEED TO MAKE SHURE THAT OTHER OPTION WAS TAKEN!
  # select(contains("dest")) %>% 
  # distinct(mpu_dest2,mpu_dest1,mpu_dest5,mpu_dest8,mpu_dest4,mpu_dest3,mpu_dest7,
  #          #mpu_tran6,mpu_tran3,mpu_tran4,mpu_tran2,mpu_tran1,
  #          #mpx_tlv_m,
  #          .keep_all = T) %>% 
  # naniar::vis_miss()
  # mutate_at(.vars = vars(starts_with("mpu_")),.funs = replace_na,"0") %>% 
  compareGroups(formula = week_x ~ .,data = .,byrow = T,method = list(mpx_tlv_m=2)) %>% 
  createTable(show.p.overall = F) %>% 
  export2xls("table/mcie19-epi-tab_week.xls")

# _EPI-MODEL --------------------------------------------------------------

mcie19_epi <- mcie19 %>% 
  select(cod_enrol,edad,sexo,village,escu_nivel_c,
         casa_tipo,casa_tipo_cat,
         tiem_domi,mosq_cuan,insect_sino,
         sect_trab_all_act_prin,sect_trab_all_act_prin_lump,sect_trab_all_act_prin_cat,
         vpc_sino,vpl_sino, #0x
         sum_1805_f,sum_1805_s,sum_1805_t,
         sum_0617_f,sum_0617_s,sum_0617_t,
         sum_1805_fc,sum_1805_sc,sum_1805_tc,
         sum_0617_fc,sum_0617_sc,sum_0617_tc, #01
         mpu_dest1:mpu_dest8,mpuf_dest1:mpuf_dest8, #02
         mpu_tran1:mpu_tran6,mpuf_tran1:mpuf_tran6, #03
         mpx_tlv_m,mpxf_tlv_m, #04
         malhist_12m) %>% 
  mutate(sect_trab_all_act_prin=fct_infreq(sect_trab_all_act_prin), #ordenar por freq
         edad_escala5=edad/5) # escalar edad

skimr::skim(mcie19_epi) # visual
psych::describe(mcie19_epi %>% select_if(is.numeric)) # skewness + kurtosis

#mcie19_epi %>% glimpse()

# __epi_tabla1 --------------------------------------------------------------

compareGroups(~ .,
              data = mcie19_epi %>% 
                select(-cod_enrol,
                       #-(mpu_dest1:mpu_dest8),
                       #-(mpuf_dest1:mpuf_dest8),
                       #-(mpu_tran1:mpu_tran6),
                       #-(mpuf_tran1:mpuf_tran6),
                       #-mpx_tlv_m,-mpxf_tlv_m,
                       -edad_escala5
                       ) 
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
                mpx_tlv_m = 2,
                mpxf_tlv_m = 2,
                #fin de semana + semana = total
                #sum_0005_f = 2, sum_0005_s = 2, 
                sum_0005_t = 2,
                #sum_0611_f = 2, sum_0611_s = 2, 
                sum_0611_t = 2,
                #sum_0617_f = 2, sum_0617_s = 2, 
                sum_0617_t = 2,
                #sum_1217_f = 2, sum_1217_s = 2, 
                sum_1217_t = 2,
                #sum_1805_f = 2, sum_1805_s = 2, 
                sum_1805_t = 2,
                #sum_1823_f = 2, sum_1823_s = 2, 
                sum_1823_t = 2,
                tiem_domi = 2#,
                #vl_cuantos = 2#,
              )
) %>% 
  createTable(show.all = T, show.n = T,digits = 1,sd.type = 2) %>% 
  export2xls("table/mcie19-epi-tab1.xls")

# __epi_tabla2 ------------------------------------------------------------

compareGroups(malhist_12m ~ .,
              data = mcie19_epi %>% 
                select(-cod_enrol,-sect_trab_all_act_prin,-sect_trab_all_act_prin_lump,
                       -(mpu_dest1:mpu_dest8),
                       -(mpuf_dest1:mpuf_dest8),
                       -(mpu_tran1:mpu_tran6),
                       -(mpuf_tran1:mpuf_tran6),
                       -mpx_tlv_m,-mpxf_tlv_m,-edad_escala5) 
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
              #show.ratio = TRUE,
              show.p.ratio = F,
              sd.type = 2) %>% 
  export2xls("table/mcie19-epi-tab2.xls")

# __epi_tabla3 ------------------------------------------------------------

# ___missing analysis --------------------------------------------------------

mcie19_epi %>% naniar::miss_var_summary() %>% filter(n_miss>0) %>% print_inf()
# viajes 15-18% valores perdidos
# mirar cuestionarios!

# ___complete case analysis --------------------------------------------------

mcie19_cc <- mcie19_epi %>% 
  select(edad_escala5,sexo,sect_trab_all_act_prin_cat,village,escu_nivel_c,
         vpc_sino,vpl_sino,sum_1805_tc,sum_0617_tc,malhist_12m) %>% 
  filter(complete.cases(.)) %>% 
  mutate(outcome=as.numeric(malhist_12m)-1)
n_full <- mcie19_epi %>% dim() %>% pluck(1)
n_trim <- mcie19_cc %>% dim() %>% pluck(1)
(1-(n_trim/n_full)) *100 #%perdida de datos ~30%

mcie19_cc %>% glimpse()
mcie19_cc %>% count(outcome,malhist_12m)

# ___simple models --------------------------------------------------------

mcie19_cc %>% pull(outcome) %>% mean()
glm_null <- glm(outcome ~ 1, data = mcie19_cc, family = poisson(link = "log"))
glm_null %>% epi_tidymodel_pr()

simple_models <- mcie19_cc %>%   
  colnames() %>% 
  enframe(name = NULL) %>% 
  filter(!value %in% c("outcome","malhist_12m")) %>% 
  #slice(1:7) %>% 
  mutate(variable=map(value,sym),
         simple_rawm=map(variable,epi_tidymodel_up,reference_model=glm_null),
         simple_tidy=map(simple_rawm,epi_tidymodel_pr)
         ) %>% 
  unnest(cols = c(simple_tidy)) %>% 
  filter(term!="(Intercept)") %>% 
  select(-value,-variable,-simple_rawm)

# ___multiple model --------------------------------------------------------

glm_full <- glm(outcome ~ edad_escala5 + sexo +
                  sect_trab_all_act_prin_cat + village + escu_nivel_c +
                  vpc_sino + vpl_sino + sum_1805_tc + sum_0617_tc
                , data = mcie19_cc, family = poisson(link = "log"))
multiple_model <- glm_full %>% epi_tidymodel_pr()


# ___final table ----------------------------------------------------------

simple_models %>%   
  full_join(multiple_model,by = "term",suffix=c(".s",".m")) %>% 
  select(-contains("log.pr"),-contains("se.")) %>% 
  writexl::write_xlsx("table/mcie19-epi-tab3.xls")


# __epi_tabla4 -----------------------------------------------------------
# type 2 error table

# _BIO-MODEL --------------------------------------------------------------

# __bio_table1 -----------------------------------------------------------------

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
  
# __bio_table2 -----------------------------------------------------------------


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

# __bio_tabla3 -----------------------------------------------------------
# type 1 error table

# ___complete case analysis --------------------------------------------------

# ___simple models --------------------------------------------------------

# ___nested process --------------------------------------------------------

# ___parsimonius model --------------------------------------------------------

# ___final table ---------------------------------------------------------

#' simple models plus 01 nested parsimous model
#' 

# __bio_tabla4 -----------------------------------------------------------
# type 2 error table
# use epi_tidymodel_up

#' simple models plus nested parsimous model adjusted by all other covariates
#' 

