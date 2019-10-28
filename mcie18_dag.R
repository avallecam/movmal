
# add packages ------------------------------------------------------------

library(tidyverse)
library(ggdag)
theme_set(new = theme_bw())

# copy daggity output -----------------------------------------------------

dag <- dagitty::dagitty( "dag {
actividad_outdoor -> historia_malaria_anho
actividad_outdoor -> salida_6pm_6am
actividad_outdoor -> viaje_corto
actividad_outdoor -> viaje_largo
destino -> historia_malaria_anho
edad -> actividad_outdoor
edad -> inmunidad
edad -> salida_6pm_6am
inmunidad -> historia_malaria_anho
mosquitero_cama -> historia_malaria_anho
mosquitero_cama -> inmunidad
rocio_insecticida -> historia_malaria_anho
rocio_insecticida -> inmunidad
salida_6pm_6am -> historia_malaria_anho
salida_6pm_6am -> transporte
sexo -> actividad_outdoor
sexo -> salida_6pm_6am
sexo -> historia_malaria_anho
tipo_vivienda -> historia_malaria_anho
tipo_vivienda -> inmunidad
tipo_vivienda -> mosquitero_cama
tipo_vivienda -> rocio_insecticida
transporte -> destino
viaje_corto -> historia_malaria_anho
viaje_corto -> salida_6pm_6am
viaje_largo -> historia_malaria_anho
viaje_largo -> inmunidad
viaje_largo -> salida_6pm_6am
historia_malaria_anho [outcome]
salida_6pm_6am [exposure]
inmunidad [latent]
}
")

# use the coordinates -----------------------------------------------------

dag_loc <- tribble(
  ~name,~x,~y,
  "actividad_outdoor",0.250,0.591,
  "destino",0.362,0.404,
  "edad",0.425,0.710,
  "historia_malaria_anho",0.571,0.497,
  "inmunidad",0.559,0.631,
  "mosquitero_cama",0.913,0.459,
  "rocio_insecticida",0.962,0.628,
  "salida_6pm_6am",-0.034,0.495,
  "sexo",0.354,0.760,
  "tipo_vivienda",0.830,0.753,
  "transporte",0.158,0.406,
  #"viaje_corto",0.253,0.739,
  "viaje_corto",0.175,0.760,
  "viaje_largo",0.045,0.703#,
  #"viaje_largo",0.071,0.683
) %>% 
  mutate(to=name,
         xend=x,
         yend=y)

# explore objects ---------------------------------------------------------

dag
dag_loc
tidy_dag <- tidy_dagitty(dag)
tidy_dag %>% str()
tidy_dag
tidy_dag$data %>% print(n=Inf)
tidy_dag$dag

# reeplace coordinates ----------------------------------------------------

tidy_dag$data <- tidy_dag$data %>% 
  select(-x,-y,-xend,-yend) %>% 
  left_join(dag_loc %>% select(name:y)) %>% 
  left_join(dag_loc %>% select(to:yend)) %>% 
  select(name,x,y,direction,to,xend,yend,circular) %>% 
  mutate(label=name)

# create DAG --------------------------------------------------------------

ggdag(tidy_dag,
      use_labels = "label",
      text = FALSE) +
  theme_dag()
ggsave("figure/01-mcie_dag.png",width = 11,height = 5)

# visualize adjustment set ------------------------------------------------

ggdag_adjustment_set(tidy_dag,
                     use_labels = "label",
                     text = FALSE,
                     node_size = 14) + 
  theme_dag() +
  theme(legend.position = "bottom")
ggsave("figure/02-mcie_dag-adjusted.png",width = 11,height = 6)

# visualize causal paths --------------------------------------------------

ggdag_paths(tidy_dag,adjust_for = c("actividad_outdoor"))
ggdag_paths(tidy_dag,adjust_for = c("actividad_outdoor","edad"))
ggdag_paths(tidy_dag,adjust_for = c("actividad_outdoor","viaje_corto"))
ggdag_paths(tidy_dag,adjust_for = c("actividad_outdoor","viaje_corto","sexo"))
ggdag_paths(tidy_dag,adjust_for = c("actividad_outdoor","viaje_corto","sexo","edad"))
ggdag_paths(tidy_dag,
            use_labels = "label",
            text = FALSE,
            adjust_for = c("actividad_outdoor","viaje_corto","sexo","edad")) +
  theme_dag() +
  theme(legend.position = "bottom")
ggsave("figure/03-mcie_dag-paths.png",width = 11,height = 3)
ggdag_paths(tidy_dag,adjust_for = c("actividad_outdoor","viaje_largo","viaje_corto","sexo","edad","destino"))
ggdag_paths(tidy_dag,adjust_for = c("actividad_outdoor","viaje_largo","viaje_corto","sexo","edad","transporte"))

# visualize d-connected paths ---------------------------------------------

ggdag_dconnected(tidy_dag)
ggdag_dconnected(tidy_dag,controlling_for = c("edad"))
ggdag_dconnected(tidy_dag,controlling_for = c("edad","sexo"))
ggdag_dconnected(tidy_dag,
                 use_labels = "label",
                 text = FALSE,
                 controlling_for = c("actividad_outdoor","viaje_largo","viaje_corto","sexo","edad")) +
  theme_dag() +
  theme(legend.position = "bottom")
ggsave("figure/04-mcie_dag-dconnected_variables.png",width = 11,height = 6)
#' all of adjustments bellow
#' had activated paths due to an adjusted collider 
#' (viajes y actividad outdoor)
# ggdag_dconnected(tidy_dag,controlling_for = c("actividad_outdoor"))
# ggdag_dconnected(tidy_dag,controlling_for = c("edad","sexo","viaje_largo"))
# ggdag_dconnected(tidy_dag,controlling_for = c("edad","sexo","viaje_corto"))
# ggdag_dconnected(tidy_dag,controlling_for = c("edad","sexo","actividad_outdoor"))
# ggdag_dconnected(tidy_dag,controlling_for = c("viaje_corto"))
# ggdag_dconnected(tidy_dag,controlling_for = c("viaje_largo"))
# ggdag_dconnected(tidy_dag,controlling_for = c("viaje_largo","viaje_corto"))
