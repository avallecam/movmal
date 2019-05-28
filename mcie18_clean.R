
# contexto ----------------------------------------------------------------

# proyecto: movimiento poblacional humano y malaria asintomática
# data: fase 02 (aplicar cuestionario de 5 secciones a adultos)
# estatus: data digitada con control de calidad por doble digitación
# objetivo: limpiar data a formato analizable (rds y dta) pre-tabla1
# extra: diccionario de datos
#        kipuformas.com -> consultas -> catálogo de variables de estudio
# producto: mcie19_20190522.rds mcie19_20190522.dta

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(haven)
library(naniar)
#skimr,psych,Hmisc

# importar BASAL (base cruda) -----------------------------------------------------

basal <- read_csv("data-raw/encuestas/2da fase_basal/Forma_F02.csv",n_max = 427)
#basal %>% visdat::vis_dat()
basal %>% glimpse()
#basal %>% count(cod_encues)
basal %>% count(lugar_cita,lugar_cita_e,lugar_encu,lugar_encu_e) %>% arrange(n)

# evaluar BASAL -----------------------------------------------------------

#doble reporte de trabajo
basal %>% filter(sect_trab==7 & !is.na(sect_trab_oe)) %>% select(sect_trab,sect_trab_oe)

basal %>% count(par_otr_e)
basal %>% count(mpu_dest8u) %>% print(n=Inf)
basal %>% count(mpv_dest8u) %>% print(n=Inf)
basal %>% count(mpuf_dest8u) %>% print(n=Inf)
basal %>% count(mpvf_dest8u) %>% print(n=Inf)

basal %>% count(apa_tcel)
basal %>% count(apa_smph)

#explorar temporadas de viaje autoreportado
#noviembre-abril:creciente(11-04)
#mayo-octubre:vaciante(05-10)
basal %>% select(cod_enrol,ends_with("_mes")) %>% #select(matches("vr(\\d)_mes")) %>% 
  #rownames_to_column() %>% 
  mutate_at(.vars = vars(matches("v.._mes")),
            .funs = list(~if_else(. %in% 5:10,"vaciante",
                                  if_else(. %in% c(01:04,11:12),
                                          "creciente",
                                          NA_character_)))) %>% #count(vr1_mes)
  replace_with_na_at(vars(matches("vr(\\d)_mes")),~.x %in% c("-1")) #%>% 
  #gather(key,value,-rowname) %>% 
  #arrange(rowname) %>% print(n=500)
  #count(rowname,key,value)
  #mutate(vr_na=if_else(is.na(vr1_mes)|is.na(vr2_mes)|is.na(vr3_mes),TRUE,FALSE),
  #       vl_na=if_else(is.na(vl1_mes)|is.na(vl2_mes)|is.na(vl3_mes),TRUE,FALSE)) %>% count(vl_na)
  #mutate_at(vars(matches("vr(\\d)_mes"),
  #               matches("vl(\\d)_mes")),
  #          str_replace_na,"") %>% 
  #mutate(
  #  vr_c = if_else(vr1_mes==vr2_mes & vr1_mes==vr3_mes & vr2_mes==vr3_mes,
  #                 vr1_mes,
  #                 paste(!!! select(.,matches("vr(\\d)_mes")),sep=".")),
  #  vl_c = paste(!!! select(.,matches("vl(\\d)_mes")),sep=".")
  #) %>% 
  #select(-matches("vr(\\d)_mes"),-matches("vl(\\d)_mes")) %>% 
  #Hmisc::describe()
  #count(vr_c) %>% print(n=Inf)
  #gather(key,value,-rowname) %>% 
  #count(value)

# valor perdido -----------------------------------------------------------

miss_var_summary(basal)
miss_summary(basal)
#visdat::vis_miss(basal)

# corregir valor perdido --------------------------------------------------

basal %>% filter(sexo==77) %>% select(cod_enrol,sexo,edad)

read_rds("data/vivienda_zungarococha_gpsrino_20190318.rds") %>% filter(name=="ll029")

read_rds("data/individuo_zungarococha_2018.rds") %>% 
  filter(str_detect(id,"LL029")) %>% 
  select(id,nacimiento,sexo)

# seleccion MOV1 (covariables) --------------------------------------------

movi1 <- basal %>% 
  select(#Documento,
         cod_enrol,fec_nac,edad,fec_encues,
         
         #seccion_1:socio-demográfico
         #domicilio:zona+tiempo+tiempo_unidades
         tiem_domi,tiem_domi_u,#zona_domi
         #numero de convivientes
         conv_cant,
         sexo,
         #educacion:sino,nivel,grado
         escu_sino,escu_nivel,#escu_inic,
         #trabajo:actual-otro-antes
         act_prin,sect_trab,sect_trab_oe,act_otra,sect_otro_1:sect_otro_11,#tra_antes,act_antes,
         
         #seccion_2:la casa
         #electricidad, agua, agua_otra, sshh, sshh_otro
         casa_ele,
         casa_agua,#casa_agua_e,
         casa_sshh,#casa_sshh_e,
         #vector-niche-distance_to_potencial_biotope: cerca_fuente_agua, cerca_bosque
         casa_fte,casa_bosq,
         #apa_comunicacion+vehiculos_todos
         apa_tcel,apa_smph,#apa_inte,apa_tfijo
         starts_with("veh_"),
         #riesgo-protección:piso,pared,tipo_casa,rociar_insecticida,tener_mosquitero
         piso_parq:casa_tipo,insect_sino,mosq_tiene,mosq_cuan,
         
         #solo sobre ultimo mosquitero - 
         #compra, mantenimiento y condiciones
         Ult3_donde1,#Ult3_don1_e,
         mosq_tm1,#mosq_tm2,mosq_tm3,
         mosq_ins1,mosq_empr1,mosq_ertm1,mosq_agu1,
         #ultima semana - comportamiento: ¿han dormido? si/no, ¿cuantas personas? (sobre ultimo mosquitero)
         mosq_dus1,mosq_dusc1,
         #ayer,¿usted durmio en mosquitero?
         aydu_yo1,#aydu_yo2,aydu_yo3,#starts_with("aydu_")
         
         #seccion_3:info_sobre_enfermedades
         #clave - 1:dengue,2:chikungunya,3:malaria,4:zika
         #conocimiento enfermedad
         #starts_with("oe_conoce"),
         #diagnistico enfermedad 
         #oe_cantd3, #starts_with("oe_cantd"),
         #ultimas-2-semanas: sintomas, fiebre-malestar
         oe_fieb1,oe_males1,
         #ultimos-6-meses: diagnóstico por otras enfermedades, ¿cual?
         #oe_ult6m1, oe_diagtd1,
         
         #seccion_4:historia_malaria 
         #(4 últimos episodios diagnosticados por profesional de la salud)
         #historia: fecha
         #starts_with("hmlr_cmes"),starts_with("hmlr_caño"),
         #historia: especie, diagnistico, tratamiento, oficial?
         #starts_with("hmlr_tipo"),starts_with("hmlr_cdiag"),starts_with("hmlr_trat"),starts_with("hmlr_tnof"),
         #historia: centro salud, finalizó tratamiento? hospitalizado? en UCI?
         #starts_with("hmlr_tcs_"),starts_with("hmlr_tfin_"),starts_with("hmlr_thos_"),starts_with("hmlr_tuci_"),
         
         #seccion_5:movimiento_poblacional
         #comunidad de residencia: si/no, ¿cual?
         mp_comu,#mp_comul,
         #rutina:LV_una_dos_salidas
         #starts_with("mpu_"),starts_with("mpv_"),
         #rutina:SD_una_dos_salidas
         #starts_with("mpuf_"),starts_with("mpvf_"),
         #viaje CORTO (ultimo año) SINO, NUMERO
         vpc_sino,mp_numv,
         #viaje LARGO (ultimo año) SINO, NUMERO
         vpl_sino,vl_cuantos,
         #viaje CORTO (ultimo año) descripcion BREVE + DETALLADA
         #starts_with("vpc1_"),starts_with("vr1_"),
         #viaje LARGO (ultimo año) descripcion BREVE + DETALLADA
         #starts_with("vpl1_"),starts_with("vl1_")
         ends_with("_mes")
  ) %>% 
  replace_with_na_all(condition = ~.x %in% c(-9,-1,"-1")) %>% 
  mutate(village=str_replace(cod_enrol,"(..).+","\\1")) %>% #count(apa_smph)
  
  #ediciones post-primera_tabla_1
  mutate(
    #corregir sexo segun censo (77: prefiere no responder)
    sexo = if_else(sexo==77,2,sexo),
    #colapsar nivel escolar
    escu_nivel_c = case_when(
      escu_nivel %in% 1:2 ~ "primaria",
      escu_nivel %in% 3:4 ~ "secundaria",
      escu_nivel %in% 5:7 ~ "superior"
      ),
    #corregir insect_sino
    insect_sino = case_when(
      insect_sino == 88 ~ NA_real_,
      insect_sino == 2 ~ NA_real_, #ante incertidumbre
      TRUE  ~ insect_sino
      ),
    #corregir insect_sino
    mosq_empr1 = case_when(
      mosq_empr1 == 88 ~ NA_real_,
      TRUE  ~ mosq_empr1
      )
    ) %>% 
  
  #colapsar act_prin, sect_trab
  mutate(
    act_prin=case_when(
      act_prin == 1 ~ "Estudiante",
      act_prin == 2 ~ "Ama de casa",
      act_prin == 3 ~ "Empleado por cuenta propia",
      act_prin == 4 ~ "Empleado para un familiar",
      act_prin == 5 ~ "Empleado para otros",
      act_prin == 6 ~ "Desempleado y buscando trabajo",
      act_prin == 7 ~ "Desempleado y no buscando trabajo",
      act_prin == 8 ~ "Incapaz de trabajar"),
    sect_trab=case_when(
      sect_trab == 1 ~ "Agricultura",
      sect_trab == 2 ~ "Pesca",
      sect_trab == 3 ~ "Tala/Madera",
      sect_trab == 4 ~ "Servicios de salud",
      sect_trab == 5 ~ "Servicios de comida",
      sect_trab == 6 ~ "Construcción",
      sect_trab == 7 ~ "Transporte y almacenamiento",
      sect_trab == 8 ~ "Educación",
      sect_trab == 9 ~ "Limpieza, mantenimiento de edificios, terrenos",
      sect_trab == 10 ~ "Oficina, profesional o personal gerencial",
      sect_trab == 11 ~ "Ventas (negocios)",
      sect_trab == 99 ~ "Otro (especificar)"
    )) %>% 
  mutate(
    sect_trab_oe_c = case_when(
      sect_trab_oe == "-2" ~ NA_character_,
      sect_trab_oe == "ADMINISTRATIVO" ~ "Oficina, profesional o personal gerencial",
      sect_trab_oe == "AVICULTURA" ~ "Ganadería",
      sect_trab_oe == "CARPINTERO" ~ "Tala/Madera",
      sect_trab_oe == "CUIDA LA VIVIENDA" ~ "Limpieza, mantenimiento de edificios, terrenos",
      sect_trab_oe == "GALPONERO GRANJA DE POLLOS" ~ "Ganadería",
      sect_trab_oe == "GRANJA DE POLLOS" ~ "Ganadería",
      sect_trab_oe == "GRANJA LA CHACRA" ~ "Ganadería",
      sect_trab_oe == "GUIA TURISTICA" ~ "Transporte y almacenamiento",
      sect_trab_oe == "INDEPENDIENTE" ~ "Oficina, profesional o personal gerencial",
      sect_trab_oe == "MANIOBRISTA" ~ "Construcción",
      sect_trab_oe == "MATERO EN CC. FORESTAL" ~ "Tala/Madera",
      sect_trab_oe == "MOTOTAXI" ~ "Transporte y almacenamiento",
      sect_trab_oe == "MOTOTAXISTA" ~ "Transporte y almacenamiento",
      sect_trab_oe == "OBRERO" ~ "Construcción",
      sect_trab_oe == "SECRETARIA EN OBRAS" ~ "Construcción",
      sect_trab_oe == "SEGURIDAD" ~ "Limpieza, mantenimiento de edificios, terrenos",
      sect_trab_oe == "TEC ACUICULTOR" ~ "Ganadería",
      sect_trab_oe == "vigilante" ~ "Limpieza, mantenimiento de edificios, terrenos",
      sect_trab_oe == "VIGILANTE" ~ "Limpieza, mantenimiento de edificios, terrenos",
    )
  ) %>% 
  mutate(sect_trab_all=coalesce(!!! select(.,sect_trab_oe_c,sect_trab))) %>% 
  mutate(sect_trab_all=if_else(sect_trab_all=="Otro (especificar)",
                               NA_character_,sect_trab_all)) %>% 
  mutate(
    sect_trab_all_act_prin=coalesce(!!! select(.,sect_trab_all,act_prin)),
    sect_trab_all_act_prin=if_else(str_detect(sect_trab_all_act_prin,"Desempleado"),
                                   "Desempleado",sect_trab_all_act_prin)) %>% 
  mutate(sect_trab_all_act_prin=if_else(str_detect(sect_trab_all_act_prin,"Empleado"),
                                        "Empleado",sect_trab_all_act_prin)) %>% 
  #count(#sect_trab,sect_trab_oe,
  #      #sect_trab_all,
  #      #act_prin,
  #      sect_trab_all_act_prin,
  #      sort = T) %>% print(n=Inf)
  #select(act_prin,sect_trab,act_prin_sect_trab)
  
  #mutate_at(vars(act_prin,sect_trab),
  #          str_replace_na,"") %>% 
  #mutate(act_prin_sect_trab = paste(!!! select(.,act_prin,sect_trab),sep=".")) %>% 
  #replace_with_na_at(vars(act_prin,sect_trab),~.x %in% c("")) %>% 
  #select(act_prin,sect_trab,act_prin_sect_trab) %>% count(act_prin_sect_trab)
  
  #[PENDIENTE]
  #limpiar actividad principal y secundaria
  #limpiar? vehículos (mantenerlo separado)
  #piso (extendido)
  #pared (extendido)
  
  mutate(
    apa_tcel=if_else(apa_tcel==11,"1","0","0"),
    apa_smph=if_else(apa_smph==12,"1","0","0")
  ) %>% 
  
  #drop logical columns with full NA
  select_if(~!is.logical(.)) %>% 
  
  mutate_at(vars(starts_with("par"),starts_with("piso"),
                 starts_with("veh_"),starts_with("sect_otro_")),
            str_replace_na,"") %>% 
  mutate(
    #par_c1 = coalesce(!!! select(.,starts_with("par"),-contains("_otr_e"))),
    par_c2 = paste(!!! select(.,starts_with("par"),-contains("_otr_e")),sep="."),
    #piso_c1 = coalesce(!!! select(.,starts_with("piso"),-contains("_otr_e"))),
    piso_c2 = paste(!!! select(.,starts_with("piso"),-contains("_otr_e")),sep="."),
    veh_c2 = paste(!!! select(.,starts_with("veh"),-contains("_otr_e")),sep="."),
    sect_otro_c2 = paste(!!! select(.,starts_with("sect_otro"),-contains("_otr_e")),sep=".")
  ) %>% 
  #modificación temporal para explorar combinación o como independientes
  select(-(sect_otro_1:sect_otro_11),-(veh_bici:par_otr)) %>% 
  
  #cambiar tipo de variables
  mutate_at(.vars = vars(sexo,escu_sino,escu_nivel,act_prin,sect_trab,act_otra,
                         starts_with("casa_"),insect_sino,mosq_tiene,
                         Ult3_donde1,mosq_ins1,mosq_empr1,mosq_agu1,
                         mosq_dus1,aydu_yo1,oe_fieb1,oe_males1,mp_comu,
                         vpc_sino,vpl_sino,village),
            .funs = as.factor) %>% 
  #transformar a año - tiempo en vivienda actual
  mutate(tiem_domi = if_else(tiem_domi_u == "Meses",tiem_domi/12,tiem_domi)) %>% 
  select(-tiem_domi_u) %>% 
  
  #edad con dos outliers y missing
  #decisión: reemplazar por el dato de fecha encuesta - nacimiento
  mutate(edad=if_else(edad>200 | is.na(edad) | edad < 18, 
                      interval(fec_nac, fec_encues) / years(1), edad )) %>% 
  select(-fec_encues,-fec_nac)

# evaluar MOV1 ------------------------------------------------------------

movi1 %>% glimpse()

movi1 %>% count(sect_trab_oe,sect_trab_oe_c) %>% print(n=Inf)

#revision de edad
#conclusión: en fecha de nacimiento también han puesto fecha de encuesta
hist(movi1$edad)
library(lubridate)
basal %>% select(cod_enrol,fec_nac,fec_encues,edad) %>% #Hmisc::describe()
  #filter(edad>200) %>% 
  mutate(edad_n =interval(fec_nac, fec_encues) / years(1)) %>% 
  filter(edad_n<0 | edad > 200 | is.na(edad) | edad < 18)
  #Hmisc::describe()
  #ggplot(aes(edad,edad_n)) + geom_point()
  #ggplot(aes(edad_n)) + geom_histogram()

movi1 %>% select_if(~!is.logical(.)) %>% select_if(is.logical)
movi1 %>% select_if(is.logical)

movi1 %>% count(sect_otro_c2) %>% arrange(n) %>% mutate(n/sum(n))

movi1 %>% 
  select_if(~!is.logical(.)) %>% 
  select(starts_with("par_")) %>% 
  arrange(!!! select(.,starts_with("par"),-contains("_otr_e"))) %>% 
  count(#par_c1, 
    par_c2) #%>% 
#visdat::vis_miss()

movi1 %>% 
  select_if(~!is.logical(.)) %>% 
  select(starts_with("piso_")) %>% 
  arrange(!!! select(.,starts_with("piso"),-contains("_otr_e"))) %>% 
  count(#piso_c1, 
    piso_c2
  ) #%>% 
#visdat::vis_miss()

movi1 %>% 
  miss_var_summary() %>% 
  print(n=50)

# función rango horas ----------------------------------------------------------

# contar horas (retorno - salida)
# 04 rangos de 06 horas cada uno
# - 06.00 - 11.59 (06, 07, 08, 09, 10, 11)
# - 12.00 - 17.59 (12, 13, 14, 15, 16, 17)
# - 18.00 - 23.59 (18, 19, 20, 21, 22, 23)
# - 00.00 - 05.59 (24, 01, 02, 03, 04, 05)
# probar sensibilidad, desplazando rangos (+1)
# - 07.00 - 12.59 (07, 08, 09, 10, 11, 12)
# - 13.00 - 18.59 (13, 14, 15, 16, 17, 18)
# - 19.00 - 00.59 (19, 20, 21, 22, 23, 24)
# - 01.00 - 06.59 (01, 02, 03, 04, 05, 06)
# probar sensibilidad, desplazando rangos (-1)
# - 05.00 - 10.59 (05, 06, 07, 08, 09, 10)
# - 11.00 - 16.59 (11, 12, 13, 14, 15, 16)
# - 17.00 - 22.59 (17, 18, 19, 20, 21, 22)
# - 23.00 - 04.59 (23, 24, 01, 02, 03, 04)
#
# limite superior - limite inferior
# cuatro escenarios por rango
# 1.- dentro de rangos: R-S
# 2.- cruce con limite inferior: R-Inf
# 3.- cruce con limite superior: Sup-S
# 4.- cruce con ambos límites: Sup-Inf
#
# compartir imagen para explicar
#
#lógica del algoritmo:
# si la hora (salida/retorno) está dentro del rango
# realizar la resta respectiva par acada uno de los cuatro escenarios
sum_range_h <- function(dt, lim_sal_h, lim_ret_h, obs_sal_h, obs_ret_h, prefix, suffix) {
  
  varname <- paste0(prefix, 
                    if_else(lim_sal_h<10,str_c(0,lim_sal_h),str_c(lim_sal_h)), 
                    if_else(lim_ret_h<10,str_c(0,lim_ret_h),str_c(lim_ret_h)), 
                    suffix)
  s_obs <- enquo(obs_sal_h)
  r_obs <- enquo(obs_ret_h)
  
  dt %>% 
    mutate(!!varname := NA_real_,
           !!varname := if_else((!!r_obs<=lim_ret_h & !!r_obs>=lim_sal_h) & 
                                  (!!s_obs>=lim_sal_h & !!s_obs<=lim_ret_h) & (!!r_obs>!!s_obs),
                                (!!r_obs+1)-!!s_obs,
                                if_else((!!r_obs<=lim_ret_h & !!r_obs>=lim_sal_h) & 
                                          (!!s_obs>=lim_sal_h & !!s_obs<=lim_ret_h) & (!!s_obs>!!r_obs),
                                        ((lim_ret_h+1)-!!s_obs) + ((!!r_obs+1)-lim_sal_h),
                                        if_else((!!r_obs>lim_sal_h & !!r_obs<=lim_ret_h) & 
                                                  (!!s_obs<lim_sal_h | !!s_obs>!!r_obs),
                                                (!!r_obs+1)-lim_sal_h,
                                                if_else((!!r_obs>lim_ret_h | !!r_obs<!!s_obs ) & 
                                                          (!!s_obs>=lim_sal_h & !!s_obs<lim_ret_h),
                                                        (lim_ret_h+1)-!!s_obs,
                                                        if_else((!!r_obs>lim_ret_h) & 
                                                                  (!!s_obs<lim_sal_h) & (!!r_obs>!!s_obs),
                                                                (lim_ret_h+1)-lim_sal_h,
                                                                NA_real_)))))
    )
}

# seleccion MCIE (exp y des) ----------------------------------------------------------
#filtrar y limpiar desenlace y exposición

mcie <- basal %>% 
  select(cod_enrol,
         #diagnosticos por malaria
         oe_cantd3,
         #fecha de diagnostico - año y mes - 4 últimos episodios
         starts_with("hmlr_caño"),starts_with("hmlr_cmes"),
         #salida-retorno-LV-1xdia
         starts_with("mpu_"),
         #salida-retorno-LV-2xdia
         starts_with("mpv_"),
         #salida-retorno-SD-1xdia
         starts_with("mpuf_"),
         #salida-retorno-SD-2xdia
         starts_with("mpvf_"),
         #retirar destino (solo texto de ubicación, mantener número)
         -matches("_dest(.)u"),
         #retirar autoreporte de horas promedio fuera de vivienda
         -contains("_hrst"),
         #no_retirar qué tan lejos viaja en tiempo (valor + unidad)
         #-contains("_tl")
  ) %>% #glimpse()
  #reemplazo de missings
  naniar::replace_with_na_all(condition = ~.x %in% c(-1,"-1",9998,98,-9)) %>% 
  #crear nombre de vivienda
  mutate(village=str_replace(cod_enrol,"(..).+","\\1")) %>% 
  
  mutate(
    
    #calcular episodio en el último año: 
    # julio 2016 - julio 2017 
    # ¿sensibilidad?
    malhist_12m=if_else(hmlr_caño1==2017 | hmlr_caño2==2017 | hmlr_caño3==2017 | hmlr_caño4==2017 | 
                          (hmlr_caño1==2016 & hmlr_cmes1 %in% seq(8,12)) | 
                          (hmlr_caño2==2016 & hmlr_cmes2 %in% seq(8,12)) |
                          (hmlr_caño3==2016 & hmlr_cmes3 %in% seq(8,12)) | 
                          (hmlr_caño4==2016 & hmlr_cmes4 %in% seq(8,12)),
                        TRUE,FALSE,FALSE),
    malhist_12m_c=if_else(oe_cantd3>0 & malhist_12m==FALSE,"mas_12m",
                          if_else(malhist_12m==TRUE,"12m",
                                  if_else(oe_cantd3==0,"nunca",NA_character_))),
  ) %>% 
  
  #una vez usada para determinar presencia de HISTORIA, retirarla
  select(-oe_cantd3) %>% 
  
  #salidas (categorizar)
  #objetivo: 
  # -categorizar: 0, 1 o 2 salidas
  # -categorizar: unicamente LV, unicamente SD, ambos LV-SD
  #mutate() %>% 
  
  #salidas (horas)
  #LV: UNA salida x dia
  
  mutate(
    # si 0 -> missing ¿? -> solo una observación sin hora de retorno
    mpu_hsal=if_else(mpu_hsal=="0",NA_character_,mpu_hsal),
    # solo extraer la hora del formato aa:bb
    mpu_hsal_h=str_replace(mpu_hsal,"(..).+","\\1") %>% as.numeric(),
    mpu_hret_h=str_replace(mpu_hret,"(..).+","\\1") %>% as.numeric(),
    # corregir horas de retorno menor que horas de salida
    # máximo valor = 31 (7am del día siguiente)
    #mpu_hret_h=if_else(mpu_hret_h<mpu_hsal_h,24+mpu_hret_h,mpu_hret_h)
  ) %>% #select(mpu_hsal_h,mpu_hret_h) %>% hist()
  
  sum_range_h(lim_sal_h = 6,
              lim_ret_h = 11,
              obs_sal_h = mpu_hsal_h, 
              obs_ret_h = mpu_hret_h,
              prefix = "mpu_",suffix = "_h") %>% 
  sum_range_h(lim_sal_h = 12,
              lim_ret_h = 17,
              obs_sal_h = mpu_hsal_h, 
              obs_ret_h = mpu_hret_h,
              prefix = "mpu_",suffix = "_h") %>% 
  sum_range_h(lim_sal_h = 18,
              lim_ret_h = 23,
              obs_sal_h = mpu_hsal_h, 
              obs_ret_h = mpu_hret_h,
              prefix = "mpu_",suffix = "_h") %>% 
  sum_range_h(lim_sal_h = 00,
              lim_ret_h = 05,
              obs_sal_h = mpu_hsal_h, 
              obs_ret_h = mpu_hret_h,
              prefix = "mpu_",suffix = "_h") %>% 
  
  #LV: DOS salidas x dia
  
  mutate(
    mpv_hsal1_h=str_replace(mpv_hsal1,"(..).+","\\1") %>% as.numeric(),
    mpv_hret1_h=str_replace(mpv_hret1,"(..).+","\\1") %>% as.numeric(),
    mpv_hsal2_h=str_replace(mpv_hsal2,"(..).+","\\1") %>% as.numeric(),
    mpv_hret2_h=str_replace(mpv_hret2,"(..).+","\\1") %>% as.numeric()
  ) %>% 
  
  sum_range_h(lim_sal_h = 6,
              lim_ret_h = 11,
              obs_sal_h = mpv_hsal1_h, 
              obs_ret_h = mpv_hret1_h,
              prefix = "mpv_",suffix = "_h1") %>% 
  sum_range_h(lim_sal_h = 12,
              lim_ret_h = 17,
              obs_sal_h = mpv_hsal1_h, 
              obs_ret_h = mpv_hret1_h,
              prefix = "mpv_",suffix = "_h1") %>% 
  sum_range_h(lim_sal_h = 18,
              lim_ret_h = 23,
              obs_sal_h = mpv_hsal1_h, 
              obs_ret_h = mpv_hret1_h,
              prefix = "mpv_",suffix = "_h1") %>% 
  sum_range_h(lim_sal_h = 00,
              lim_ret_h = 05,
              obs_sal_h = mpv_hsal1_h, 
              obs_ret_h = mpv_hret1_h,
              prefix = "mpv_",suffix = "_h1") %>% 
  
  sum_range_h(lim_sal_h = 6,
              lim_ret_h = 11,
              obs_sal_h = mpv_hsal2_h, 
              obs_ret_h = mpv_hret2_h,
              prefix = "mpv_",suffix = "_h2") %>% 
  sum_range_h(lim_sal_h = 12,
              lim_ret_h = 17,
              obs_sal_h = mpv_hsal2_h, 
              obs_ret_h = mpv_hret2_h,
              prefix = "mpv_",suffix = "_h2") %>% 
  sum_range_h(lim_sal_h = 18,
              lim_ret_h = 23,
              obs_sal_h = mpv_hsal2_h, 
              obs_ret_h = mpv_hret2_h,
              prefix = "mpv_",suffix = "_h2") %>% 
  sum_range_h(lim_sal_h = 00,
              lim_ret_h = 05,
              obs_sal_h = mpv_hsal2_h, 
              obs_ret_h = mpv_hret2_h,
              prefix = "mpv_",suffix = "_h2") %>% 
  
  #SD: UNA salida x semana
  
  mutate(
    mpuf_hsal=if_else(mpuf_hsal=="0",NA_character_,mpuf_hsal),
    mpuf_hsal_h=str_replace(mpuf_hsal,"(..).+","\\1") %>% as.numeric(),
    mpuf_hret_h=str_replace(mpuf_hret,"(..).+","\\1") %>% as.numeric()#,
    #mpuf_hret_h=if_else(mpuf_hret_h<mpuf_hsal_h,24+mpuf_hret_h,mpuf_hret_h),
  ) %>% 
  
  sum_range_h(lim_sal_h = 6,
              lim_ret_h = 11,
              obs_sal_h = mpuf_hsal_h, 
              obs_ret_h = mpuf_hret_h,
              prefix = "mpuf_",suffix = "_h") %>% 
  sum_range_h(lim_sal_h = 12,
              lim_ret_h = 17,
              obs_sal_h = mpuf_hsal_h, 
              obs_ret_h = mpuf_hret_h,
              prefix = "mpuf_",suffix = "_h") %>% 
  sum_range_h(lim_sal_h = 18,
              lim_ret_h = 23,
              obs_sal_h = mpuf_hsal_h, 
              obs_ret_h = mpuf_hret_h,
              prefix = "mpuf_",suffix = "_h") %>% 
  sum_range_h(lim_sal_h = 00,
              lim_ret_h = 05,
              obs_sal_h = mpuf_hsal_h, 
              obs_ret_h = mpuf_hret_h,
              prefix = "mpuf_",suffix = "_h") %>% 
  
  #SD: DOS salidas x semana
  
  mutate(
    mpvf_hsal1_h=str_replace(mpvf_hsal1,"(..).+","\\1") %>% as.numeric(),
    mpvf_hret1_h=str_replace(mpvf_hret1,"(..).+","\\1") %>% as.numeric(),
    mpvf_hsal2_h=str_replace(mpvf_hsal2,"(..).+","\\1") %>% as.numeric(),
    mpvf_hret2_h=str_replace(mpvf_hret2,"(..).+","\\1") %>% as.numeric()
  ) %>% 
  
  sum_range_h(lim_sal_h = 6,
              lim_ret_h = 11,
              obs_sal_h = mpvf_hsal1_h, 
              obs_ret_h = mpvf_hret1_h,
              prefix = "mpvf_",suffix = "_h1") %>% 
  sum_range_h(lim_sal_h = 12,
              lim_ret_h = 17,
              obs_sal_h = mpvf_hsal1_h, 
              obs_ret_h = mpvf_hret1_h,
              prefix = "mpvf_",suffix = "_h1") %>% 
  sum_range_h(lim_sal_h = 18,
              lim_ret_h = 23,
              obs_sal_h = mpvf_hsal1_h, 
              obs_ret_h = mpvf_hret1_h,
              prefix = "mpvf_",suffix = "_h1") %>% 
  sum_range_h(lim_sal_h = 00,
              lim_ret_h = 05,
              obs_sal_h = mpvf_hsal1_h, 
              obs_ret_h = mpvf_hret1_h,
              prefix = "mpvf_",suffix = "_h1") %>% 
  
  sum_range_h(lim_sal_h = 6,
              lim_ret_h = 11,
              obs_sal_h = mpvf_hsal2_h, 
              obs_ret_h = mpvf_hret2_h,
              prefix = "mpvf_",suffix = "_h2") %>% 
  sum_range_h(lim_sal_h = 12,
              lim_ret_h = 17,
              obs_sal_h = mpvf_hsal2_h, 
              obs_ret_h = mpvf_hret2_h,
              prefix = "mpvf_",suffix = "_h2") %>% 
  sum_range_h(lim_sal_h = 18,
              lim_ret_h = 23,
              obs_sal_h = mpvf_hsal2_h, 
              obs_ret_h = mpvf_hret2_h,
              prefix = "mpvf_",suffix = "_h2") %>% 
  sum_range_h(lim_sal_h = 00,
              lim_ret_h = 05,
              obs_sal_h = mpvf_hsal2_h, 
              obs_ret_h = mpvf_hret2_h,
              prefix = "mpvf_",suffix = "_h2") %>% #count(mpu_dest1)
  
  #DENSTINOS
  #LIMPIAR destinos
  mutate_at(vars(contains("_dest1")),str_replace,"Si","1") %>% #count(mpu_dest1)
  mutate_at(vars(contains("_dest2")),str_replace,"Si","2") %>% #count(mpu_dest1)
  mutate_at(vars(contains("_dest3")),str_replace,"Si","3") %>% #count(mpu_dest1)
  mutate_at(vars(contains("_dest4")),str_replace,"Si","4") %>% #count(mpu_dest1)
  mutate_at(vars(contains("_dest5")),str_replace,"Si","5") %>% #count(mpu_dest1)
  mutate_at(vars(contains("_dest6")),str_replace,"Si","6") %>% #count(mpu_dest1)
  mutate_at(vars(contains("_dest7")),str_replace,"Si","7") %>% #count(mpu_dest1)
  mutate_at(vars(contains("_dest8")),str_replace,"Si","8") %>% #count(mpu_dest1)
  # colapse all destinos
  mutate_at(vars(starts_with("mpu_dest"),starts_with("mpv_dest"),
                 starts_with("mpuf_dest"),starts_with("mpvf_dest")),
            str_replace_na,"") %>% 
  mutate(
    #mpu_dest_c1 = coalesce(!!! select(.,starts_with("mpu_dest"))),
    mpu_dest_c2 = paste(!!! select(.,starts_with("mpu_dest")),sep="."),
    #mpv_dest_c1 = coalesce(!!! select(.,starts_with("mpv_dest"))),
    mpv_dest_c2 = paste(!!! select(.,starts_with("mpv_dest")),sep="."),
    mpuf_dest_c2 = paste(!!! select(.,starts_with("mpuf_dest")),sep="."),
    mpvf_dest_c2 = paste(!!! select(.,starts_with("mpvf_dest")),sep=".")
    ) %>% 
  #COALESCE destino de una y dos salidas
  replace_with_na_at(vars(mpu_dest_c2,mpv_dest_c2),~.x %in% c(".......")) %>% 
  mutate(mpx_dest_c2=coalesce(!!! select(.,mpu_dest_c2,mpv_dest_c2))) %>% 
  replace_with_na_at(vars(mpuf_dest_c2,mpvf_dest_c2),~.x %in% c(".......")) %>% 
  mutate(mpxf_dest_c2=coalesce(!!! select(.,mpuf_dest_c2,mpvf_dest_c2))) %>% 
  select(-mpu_dest_c2,-mpv_dest_c2,-mpuf_dest_c2,-mpvf_dest_c2) %>% 
  #DENSTINOS
  
  #TIEMPO
  #tiempos a minutos!
  mutate(
    mpu_tlv_m=if_else(mpu_tlu=="Horas",mpu_tlv*60,if_else(mpu_tlu=="Dias",mpu_tlv*24*60,mpu_tlv*1)),
    mpv_tlv_m=if_else(mpv_tlu=="Horas",mpv_tlv*60,if_else(mpv_tlu=="Dias",mpv_tlv*24*60,mpv_tlv*1)),
    mpuf_tlv_m=if_else(mpuf_tlu=="Horas",mpuf_tlv*60,if_else(mpuf_tlu=="Dias",mpuf_tlv*24*60,mpuf_tlv*1)),
    mpvf_tlv_m=if_else(mpvf_tlu=="Horas",mpvf_tlv*60,if_else(mpvf_tlu=="Dias",mpvf_tlv*24*60,mpvf_tlv*1))
  ) %>% 
  #coalesce tiempo por semana-finde
  mutate(mpx_tlv_m=coalesce(!!! select(.,mpu_tlv_m,mpv_tlv_m))) %>% 
  mutate(mpxf_tlv_m=coalesce(!!! select(.,mpuf_tlv_m,mpvf_tlv_m))) %>% 
  mutate(mpx_tlu=coalesce(!!! select(.,mpu_tlu,mpv_tlu))) %>% 
  mutate(mpxf_tlu=coalesce(!!! select(.,mpuf_tlu,mpvf_tlu))) %>% 
  select(-mpu_tlv_m,-mpv_tlv_m,-mpuf_tlv_m,-mpvf_tlv_m,-mpu_tlu,-mpv_tlu,-mpuf_tlu,-mpvf_tlu) %>% 
  #TIEMPO
  
  #TRANSPORTE
  #LIMPIAR transportes
  mutate_at(vars(contains("_tran1")),str_replace,"Si","1") %>% #count(mpu_tran1)
  mutate_at(vars(contains("_tran2")),str_replace,"Si","2") %>% #count(mpu_tran1)
  mutate_at(vars(contains("_tran3")),str_replace,"Si","3") %>% #count(mpu_tran1)
  mutate_at(vars(contains("_tran4")),str_replace,"Si","4") %>% #count(mpu_tran1)
  mutate_at(vars(contains("_tran5")),str_replace,"Si","5") %>% #count(mpu_tran1)
  mutate_at(vars(contains("_tran6")),str_replace,"Si","6") %>% #count(mpu_tran1)
  # colapse all transportes
  mutate_at(vars(starts_with("mpu_tran"),starts_with("mpv_tran"),
                 starts_with("mpuf_tran"),starts_with("mpvf_tran")),
            str_replace_na,"") %>% 
  mutate(
    #mpu_tran_c1 = coalesce(!!! select(.,starts_with("mpu_tran"))),
    mpu_tran_c2 = paste(!!! select(.,starts_with("mpu_tran")),sep="."),
    #mpv_tran_c1 = coalesce(!!! select(.,starts_with("mpv_tran"))),
    mpv_tran_c2 = paste(!!! select(.,starts_with("mpv_tran")),sep="."),
    mpuf_tran_c2 = paste(!!! select(.,starts_with("mpuf_tran")),sep="."),
    mpvf_tran_c2 = paste(!!! select(.,starts_with("mpvf_tran")),sep=".")
  ) %>% 
  #COALESCE transporte de una y dos salidas
  replace_with_na_at(vars(mpu_tran_c2,mpv_tran_c2),~.x %in% c(".....")) %>% 
  mutate(mpx_tran_c2=coalesce(!!! select(.,mpv_tran_c2,mpu_tran_c2))) %>% 
  replace_with_na_at(vars(mpuf_tran_c2,mpvf_tran_c2),~.x %in% c(".....")) %>% 
  mutate(mpxf_tran_c2=coalesce(!!! select(.,mpuf_tran_c2,mpvf_tran_c2))) %>% 
  select(-mpu_tran_c2,-mpv_tran_c2,-mpuf_tran_c2,-mpvf_tran_c2) %>% 
  #TRANSPORTE
  
  #categorizar transporte: caminar y bote o pequepeque son de mayor proporción
  mutate(#transporte=if_else(!is.na(mpu_tran6) | !is.na(mpv_tran6)| !is.na(mpuf_tran6)| !is.na(mpvf_tran6),"caminando",
          #                if_else(!is.na(mpu_tran2) | !is.na(mpv_tran2)| !is.na(mpuf_tran2)| !is.na(mpvf_tran2),"moto","otro")),
       #caminando=if_else(!is.na(mpu_tran6) | !is.na(mpv_tran6)| !is.na(mpuf_tran6)| !is.na(mpvf_tran6),"caminando","otro"),
       malhist_12m=if_else(malhist_12m_c=="12m","con",
                           if_else(malhist_12m_c=="mas_12m" | malhist_12m_c=="nunca","sin",NA_character_))) %>% 
  
  # sumatoria de horas por sujeto (fila)
  rowwise() %>% 
  #suma total (LV+SD)
  #suma semana (LV)
  #suma findes (SD)
  #no hay denominador medido: pregunta define "en la semana"
  mutate(sum_0611_t=sum(mpu_0611_h,mpv_0611_h1,mpv_0611_h2,mpuf_0611_h,mpvf_0611_h1,mpvf_0611_h2,na.rm = T),
         sum_0611_s=sum(mpu_0611_h,mpv_0611_h1,mpv_0611_h2,na.rm = T),
         sum_0611_f=sum(mpuf_0611_h,mpvf_0611_h1,mpvf_0611_h2,na.rm = T),
         
         sum_1217_t=sum(mpu_1217_h,mpv_1217_h1,mpv_1217_h2,mpuf_1217_h,mpvf_1217_h1,mpvf_1217_h2,na.rm = T),
         sum_1217_s=sum(mpu_1217_h,mpv_1217_h1,mpv_1217_h2,na.rm = T),
         sum_1217_f=sum(mpuf_1217_h,mpvf_1217_h1,mpvf_1217_h2,na.rm = T),
         
         sum_1823_t=sum(mpu_1823_h,mpv_1823_h1,mpv_1823_h2,mpuf_1823_h,mpvf_1823_h1,mpvf_1823_h2,na.rm = T),
         sum_1823_s=sum(mpu_1823_h,mpv_1823_h1,mpv_1823_h2,na.rm = T),
         sum_1823_f=sum(mpuf_1823_h,mpvf_1823_h1,mpvf_1823_h2,na.rm = T),
         
         sum_0005_t=sum(mpu_0005_h,mpv_0005_h1,mpv_0005_h2,mpuf_0005_h,mpvf_0005_h1,mpvf_0005_h2,na.rm = T),
         sum_0005_s=sum(mpu_0005_h,mpv_0005_h1,mpv_0005_h2,na.rm = T),
         sum_0005_f=sum(mpuf_0005_h,mpvf_0005_h1,mpvf_0005_h2,na.rm = T)
  ) %>% 
  mutate(
    sum_0617_t=sum(sum_0611_t,sum_1217_t,na.rm = T),
    sum_1805_t=sum(sum_1823_t,sum_0005_t,na.rm = T),
    
    sum_0617_s=sum(sum_0611_s,sum_1217_s,na.rm = T),
    sum_1805_s=sum(sum_1823_s,sum_0005_s,na.rm = T),
    
    sum_0617_f=sum(sum_0611_f,sum_1217_f,na.rm = T),
    sum_1805_f=sum(sum_1823_f,sum_0005_f,na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    sum_0611_tc=if_else(sum_0611_t>0,"si","no"),
    sum_1217_tc=if_else(sum_1217_t>0,"si","no"),
    sum_1823_tc=if_else(sum_1823_t>0,"si","no"),
    sum_0005_tc=if_else(sum_0005_t>0,"si","no"),
    sum_0617_tc=if_else(sum_0617_t>0,"si","no"),
    sum_1805_tc=if_else(sum_1805_t>0,"si","no"),
    
    sum_0611_sc=if_else(sum_0611_s>0,"si","no"),
    sum_1217_sc=if_else(sum_1217_s>0,"si","no"),
    sum_1823_sc=if_else(sum_1823_s>0,"si","no"),
    sum_0005_sc=if_else(sum_0005_s>0,"si","no"),
    sum_0617_sc=if_else(sum_0617_s>0,"si","no"),
    sum_1805_sc=if_else(sum_1805_s>0,"si","no"),
    
    sum_0611_fc=if_else(sum_0611_f>0,"si","no"),
    sum_1217_fc=if_else(sum_1217_f>0,"si","no"),
    sum_1823_fc=if_else(sum_1823_f>0,"si","no"),
    sum_0005_fc=if_else(sum_0005_f>0,"si","no"),
    sum_0617_fc=if_else(sum_0617_f>0,"si","no"),
    sum_1805_fc=if_else(sum_1805_f>0,"si","no")
  )

# _dest destino ?????? (categorizar, simplificar)

# _tl tiempo ????? (variable continua, ver distribución, variable categórica)

# historia malaria (solo explorar como descriptivo)
# viajes (solo explorar como descriptivo)

# evaluar MCIE -----

mcie %>% glimpse()

mcie %>% 
  mutate(mpx_tlv_m=coalesce(!!! select(.,mpu_tlv_m,mpv_tlv_m))) %>% 
  mutate(mpxf_tlv_m=coalesce(!!! select(.,mpuf_tlv_m,mpvf_tlv_m))) %>% 
  count(mpu_tlv_m,mpv_tlv_m,mpx_tlv_m) %>% 
  #count(mpuf_tlv_m,mpvf_tlv_m,mpxf_tlv_m) %>% 
  print(n=Inf)

#evaluar COALESCE de destino y transporte
mcie %>% 
  count(#mpu_dest_c2,mpv_dest_c2,
        mpx_dest_c2) %>% 
  #count(#mpuf_dest_c2,mpvf_dest_c2,
  #      mpxf_dest_c2) %>% 
  #count(#mpu_tran_c2,mpv_tran_c2,
  #      mpx_tran_c2) %>% 
  #count(#mpuf_tran_c2,mpvf_tran_c2,
  #      mpxf_tran_c2) %>% 
  print(n=Inf)

#mcie %>% count(mpu_dest_c2,mpv_dest_c2) %>% print(n=Inf)
  
mcie %>% select(contains("_tlu")) %>% gather(key,value) %>% count(value)

mcie %>% select(contains("_dest")) %>% select(ends_with("_c2")) %>% mutate_all(as.factor) %>% Hmisc::describe()
mcie %>% select(contains("_tran")) %>% select(ends_with("_c2")) %>% mutate_all(as.factor) %>% Hmisc::describe()

#idea: ver que los reportes de 1 y 2+ salidas son complementarios, no sobrelapantes
mcie %>% select(mpu_0611_h,mpv_0611_h1,mpv_0611_h2,mpuf_0611_h,mpvf_0611_h1,mpvf_0611_h2,sum_0611_t)

#material para categorizar comportamiento reportado, no dependiente de la hora de salida
mcie %>% select(matches("mp(u|v|uf|vf)\\_h(.+)\\_(.+)")) %>% 
  arrange(mpu_hsal_h,mpv_hsal1_h,mpuf_hsal_h,mpvf_hsal1_h) %>% visdat::vis_miss()
mcie %>% select(matches("mp(u|v)\\_h(.+)\\_(.+)")) %>% 
  arrange(mpu_hsal_h,mpv_hsal1_h) %>% visdat::vis_miss()
mcie %>% select(matches("mp(uf|vf)\\_h(.+)\\_(.+)")) %>% 
  arrange(mpuf_hsal_h,mpvf_hsal1_h) %>% visdat::vis_miss()
mcie %>% select(matches("mp(u|uf)\\_h(.+)\\_(.+)")) %>% 
  arrange(mpu_hsal_h,mpuf_hsal_h) %>% visdat::vis_miss()
mcie %>% select(matches("mp(v|vf)\\_h(.+)\\_(.+)")) %>% 
  arrange(mpv_hsal1_h,mpvf_hsal1_h) %>% visdat::vis_miss()

mcie %>% select(matches("mp(.+)\\_(\\d\\d\\d\\d)\\_(.+)"))
mcie %>% select(matches("mp(.+)\\_(....)\\_(.+)"),
                -matches("mp(.+)\\_(\\d\\d\\d\\d)\\_(.+)"))

mcie %>% count(sum_0611_tc) %>% mutate(per=100*n/sum(n))
mcie %>% count(sum_1217_tc) %>% mutate(per=100*n/sum(n))
mcie %>% count(sum_1823_tc) %>% mutate(per=100*n/sum(n))
mcie %>% count(sum_0005_tc) %>% mutate(per=100*n/sum(n))
mcie %>% count(sum_0617_tc) %>% mutate(per=100*n/sum(n))
mcie %>% count(sum_1805_tc) %>% mutate(per=100*n/sum(n))

#mcie %>% count(transporte) %>% mutate(per=100*n/sum(n))
#mcie %>% count(caminando) %>% mutate(per=100*n/sum(n))

#mcie %>% count(out_1805_h) %>% mutate(per=100*n/sum(n))
#mcie %>% count(out_home_c) %>% mutate(per=100*n/sum(n))
mcie %>% count(malhist_12m) %>% mutate(per=100*n/sum(n))

mcie %>% 
  select(cod_enrol,
         sum_0611_t,sum_0611_s,sum_0611_f,
         sum_1217_t,sum_1217_s,sum_1217_f,
         sum_1823_t,sum_1823_s,sum_1823_f,
         sum_0005_t,sum_0005_s,sum_0005_f
  ) %>% 
  gather(key,value,-cod_enrol) %>% 
  mutate(week_t=str_replace(key,"(.+)_(.+)_(.)","\\3")) %>% 
  filter(week_t=="t") %>% 
  group_by(key) %>% 
  summarise(n=sum(value,na.rm = T)) %>% ungroup() %>% mutate(per=100*n/sum(n))

# grabar MCIE (2019) --------------------------------------------------------

mcie_19 <- movi1 %>% 
  full_join(
    mcie %>% select(cod_enrol, #codigo_sujeto=cod_enrol,
                    
                    malhist_12m,malhist_12m_c,
                    #out_1805_h,out_home_c,
                    
                    sum_0611_t,sum_0611_s,sum_0611_f,
                    sum_1217_t,sum_1217_s,sum_1217_f,
                    sum_1823_t,sum_1823_s,sum_1823_f,
                    sum_0005_t,sum_0005_s,sum_0005_f,
                    
                    sum_0617_t,sum_0617_s,sum_0617_f,
                    sum_1805_t,sum_1805_s,sum_1805_f,
                    
                    sum_0611_tc,sum_0611_sc,sum_0611_fc,
                    sum_1217_tc,sum_1217_sc,sum_1217_fc,
                    sum_1823_tc,sum_1823_sc,sum_1823_fc,
                    sum_0005_tc,sum_0005_sc,sum_0005_fc,
                    
                    sum_0617_tc,sum_0617_sc,sum_0617_fc,
                    sum_1805_tc,sum_1805_sc,sum_1805_fc,
                    
                    mpx_dest_c2:mpxf_tran_c2 #,contains("_tlu"),
                    
                    #transporte,
                    #caminando
    )
  ) %>% 
  
  #homogenizar formatos
  mutate_if(is.character,as.factor)

mcie_19 %>% glimpse()

mcie_19 %>% 
  write_rds("data/mcie19_20190522.rds")

mcie_19 %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  #grabar en dta
  haven::write_dta("data/mcie19_20190522.dta")
