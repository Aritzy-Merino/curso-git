rm(list = ls())

#### se abren paquetes
library(pacman)
p_load(stringr, dplyr, readr, readxl, tidyverse, data.table, ggplot2, modeest, psych, lubridate, tidyr, haven)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
options(scipen=999)

##########################################################

# abrir base agregada sesnsp y población
setwd("/Users/HP/Documents/IPGC/Desaparecidos")

datos <- read_csv("cenapi.csv")

pob_1 <- read_csv("/Users/HP/Documents/Rizika/Incidencia delictiva/pob_mun 2015-2030/base_municipios_final_datos_01.csv", locale = locale(encoding = "latin1"))

pob_2 <- read_csv("/Users/HP/Documents/Rizika/Incidencia delictiva/pob_mun 2015-2030/base_municipios_final_datos_02.csv", locale = locale(encoding = "latin1"))

pobtot <- rbind(pob_1, pob_2)

pobtot <- pobtot %>% 
  select(2:9)

colnames(pobtot) <- c("Clave_mun", "Clave_ent", "Entidad", "Municipio", "Sexo", "Anual", "Grp_edad", "Pob")

pobtot_nac <- aggregate(Pob ~ Anual + Entidad + Clave_ent + Municipio + Clave_mun, data= pobtot, sum)

pobtot_nac$Anual <- as.factor(pobtot_nac$Anual)

#### Filtrar datos por fecha de interés 2000-2020 ####

datos_2000 <- datos %>% 
  filter(year(`FECHA EVENTO`)>=2000)

datos_2000 <- datos_2000 %>% 
  select(2, 4, 6:12, 14, 23:24, 36, 38:45, 49)

colnames(datos_2000) <- c("fecha_reporte", "fecha_evento", "estado", "clave_estado", "municipio", "clave_municipio", "nacionalidad", "estatus_migratorio", "sexo", "edad", "ocupacion", "rel_grupos_delictivos", "fecha_localizacion", "vivo_muerto", "pos_causa_desaparicion", "condicion_encontrado", "estado_loc", "clave_estado_loc", "municipio_loc", "clave_municipio_loc", "ente_loc", "causas_muerte")

datos_2000 <- datos_2000 %>% 
  mutate(clave_estado = str_pad(datos_2000$clave_estado, 2, pad = 0))

datos_2000 <- datos_2000 %>% 
  mutate(clave_municipio = str_pad(datos_2000$clave_municipio, 3, pad = 0))

datos_2000 <- datos_2000 %>% 
  filter(clave_estado > "00")

datos_2000 <- datos_2000 %>% 
  filter(clave_municipio > "000")

datos_2000 <- datos_2000 %>% 
  mutate(cve_mun = paste0(clave_estado, clave_municipio))

datos_2000 <- datos_2000 %>% 
  select(1:5, 23, 7:22)

### Descriptivos####

datos_tipo <- datos_2000 %>%
  group_by(vivo_muerto) %>% 
  count()

datos_anual <- datos_2000 %>%
  group_by(year(fecha_evento)) %>% 
  count()

datos_estado <- datos_2000 %>%
  group_by(estado) %>% 
  count()

datos_estado_anual <- datos_2000 %>%
  group_by(estado, year(fecha_evento)) %>% 
  count()

#### Filtrar por variables de interés ####

# Encontrados vivos
datos_vivos <- datos_2000 %>% 
  subset(vivo_muerto=="VIVO")

datos_vivos <- datos_vivos %>% 
  group_by(year(fecha_evento), estado, clave_estado, municipio, cve_mun) %>% 
  count() %>% 
  ungroup()

names(datos_vivos)[1] <- "anual"
names(datos_vivos)[6] <- "total_vivos_anual"

datos_vivos <- datos_vivos %>%
  spread(key = anual, value = total_vivos_anual, fill = 0) %>% 
  group_by(cve_mun)

# Encontrados muertos
datos_muertos <- datos_2000 %>% 
  subset(vivo_muerto=="MUERTO")

datos_muertos <- datos_muertos %>% 
  group_by(year(fecha_evento), estado, clave_estado, municipio, cve_mun) %>% 
  count()%>% 
  ungroup()

names(datos_muertos)[1] <- "anual"
names(datos_muertos)[6] <- "total_muertos_anual"

datos_muertos <- datos_muertos %>%
  spread(key = anual, value = total_muertos_anual, fill = 0) %>% 
  group_by(cve_mun)

# Sin encontrar

datos_sin_e <- datos_2000 %>% 
  subset(vivo_muerto =="AUN SIN LOCALIZAR")

datos_sin_e <- datos_sin_e %>% 
  group_by(year(fecha_evento), estado, clave_estado, municipio, cve_mun) %>% 
  count() %>% 
  ungroup

names(datos_sin_e)[1] <- "anual"
names(datos_sin_e)[6] <- "total_desaparecidos_anual"

datos_sin_e <- datos_sin_e %>%
  spread(key = anual, value = total_desaparecidos_anual, fill = 0) %>% 
  group_by(cve_mun)
