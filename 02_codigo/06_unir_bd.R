### Limpiar ambiente ----
remove(list = ls())

### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R")

### Cargar y procesar cada base de datos ----
source("02_codigo/01_importar_procesar_bd_legislaturas.R")
source("02_codigo/02_importar_procesar_bd_indice.R")
source("02_codigo/03_importar_procesar_bd_pibe.R")
source("02_codigo/04_importar_procesar_bd_conapo.R")
source("02_codigo/05_importar_procesar_bd_inegi.R")


### Unir datos de bd_por_mr y bd_por_rp con bd_indice_edos, manteniendo solo las observaciones correspondientes a mujeres, y creando nuevos tibbles bd_mr y bd_rp ----

# Datos de legisladoras de mayoría relativa ----
bd_mr <- 
  bd_prop_mr %>%
  left_join(bd_indice_edos %>% 
              # Filtrar para solo mantener las observaciones de mayoría relativa de bd_indice_edos
              filter(str_detect(principio_de_representacion, "Mayor")),
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año_eleccion", 
                   "estado" = "estado",
                   "principio_de_representacion" = "principio_de_representacion")) %>%   
  # Renombrar variable
  rename(año_publicacion_ley = año_publicacion) %>%  
  # Eliminar algunas columnas
  select(-c(num_norma, entidad, tipo_de_eleccion, id_eleccion)) %>% 
  # Convertir NAs en 0 en las columnas que incluyen subcomponentes del índice y del propio índice. Las NAs corresponden a años en los que la norma electoral no incluía disposiciones relacionadas con equidad/paridad de género. Por ello el valor es 0.
  mutate_at(vars(tamano_de_la_cuota:ifreg),
            ~ ifelse(is.na(.), 0, .))

# Datos de legisladoras de representación proporcional ----
bd_rp <- 
  bd_prop_rp %>% 
  left_join(bd_indice_edos %>% 
              # Filtrar para solo mantener las observaciones de representación proporcional de bd_indice_edos
              filter(str_detect(principio_de_representacion, "Rep")),
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año_eleccion", 
                   "estado" = "estado",
                   "principio_de_representacion" = "principio_de_representacion")) %>% 
  # Renombrar variable
  rename(año_publicacion_ley = año_publicacion) %>% 
  # Eliminar algunas columnas
  select(-c(num_norma, entidad, tipo_de_eleccion, id_eleccion)) %>% 
  # Convertir NAs en 0 en las columnas que incluyen subcomponentes del índice y del propio índice. Las NAs corresponden a años en los que la norma electoral no incluía disposiciones relacionadas con equidad/paridad de género. Por ello el valor es 0.
  mutate_at(vars(tamano_de_la_cuota:ifreg),
            ~ ifelse(is.na(.), 0, .)) 



### Construir variable que registre la antiguedad de la primera norma de género en cada entidad respecto al año de la elección correspondiente---- 

# Mayoría relativa
bd_mr <- 
  bd_mr %>% 
  group_by(estado) %>% 
  mutate(año_primera_norma = min(año_publicacion_ley, na.rm = T),
         año_primera_norma = ifelse(is.na(año_publicacion_ley), NA, año_primera_norma),
         antiguedad_primera_norma = año_inicio - año_primera_norma) %>% 
  ungroup()

# Representación proporcional
bd_rp <- 
  bd_rp %>% 
  group_by(estado) %>% 
  mutate(año_primera_norma = min(año_publicacion_ley, na.rm = T),
         año_primera_norma = ifelse(is.na(año_publicacion_ley), NA, año_primera_norma),
         antiguedad_primera_norma = año_inicio - año_primera_norma) %>% 
  ungroup()

### Unir datos de bd_mr y bd_rp con bd_pibe y guardar resultado en bd_mr y bd_rp ----

# Datos de legisladoras de mayoría relativa ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_pibe,
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "estado")) 

# Datos de legisladoras de representación proporcional ----
bd_rp <- 
  bd_rp %>%
  left_join(bd_pibe,
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "estado"))



### Crear variable alternancia y guardarla en bd_mr y bd_rp, respectivamente ----

# alternancia es una variable dicotómica con valor de 1 *antes* de la elección de la legislatura correspondiente (p. ej, 2013) ya se había registrado una alternancia política en el partido que gobernaba la entidad (p.ej., 2012 o antes), y 0 en otros casos. Este último escenario incluye aquellos casos en los que la alternancia del partido en el gobierno estatal ocurrió en la misma elección en la que se eligió a los miembros de la legislatura (p. ej., Tabasco en 2013). 

# Esta variable es construida con base en los datos de González Ulloa (2017), Alternancia en las elecciones subnacionales en México: ¿síntoma de democratización?, *Estudios Políticos*, Volumen 40, Enero-Abril, páginas 47-69. Url: https://doi.org/10.1016/j.espol.2016.10.010


# Datos de legisladoras de mayoría relativa ----
bd_mr <- 
  bd_mr %>% 
  mutate(alternancia = case_when(estado == "Aguascalientes" & año_inicio == 2001 ~ 1,
                                 estado == "Baja California" & año_inicio == 1992 ~ 1,
                                 estado == "Baja California Sur" & año_inicio == 2002 ~ 1,
                                 estado == "Chiapas" & año_inicio == 2001 ~ 1,
                                 estado == "Chihuahua" & año_inicio == 1995 ~ 1,
                                 estado == "Ciudad de México" & año_inicio == 2000 ~ 1,
                                 estado == "Durango" & año_inicio == 2018 ~ 1,
                                 estado == "Guanajuato" & año_inicio == 1997 ~ 1,
                                 estado == "Guerrero" & año_inicio == 2008 ~ 1,
                                 estado == "Jalisco" & año_inicio == 1997 ~ 1,
                                 estado == "Michoacán" & año_inicio == 2004 ~ 1,
                                 estado == "Morelos" & año_inicio == 2003 ~ 1,
                                 estado == "Nayarit" & año_inicio == 2002 ~ 1,
                                 estado == "Nuevo León" & año_inicio == 2000 ~ 1,
                                 estado == "Oaxaca" & año_inicio == 2013 ~ 1,
                                 estado == "Puebla" & año_inicio == 2014 ~ 1,
                                 estado == "Querétaro" & año_inicio == 2000 ~ 1,
                                 estado == "Quintana Roo" & año_inicio == 2019 ~ 1,
                                 estado == "San Luis Potosí" & año_inicio == 2006 ~ 1,
                                 estado == "Sinaloa" & año_inicio == 2013 ~ 1,
                                 estado == "Sonora" & año_inicio == 2012 ~ 1,
                                 estado == "Tabasco" & año_inicio == 2015 ~ 1,
                                 estado == "Tamaulipas" & año_inicio == 2019 ~ 1,
                                 estado == "Tlaxcala" & año_inicio == 2001 ~ 1,
                                 estado == "Veracruz" & año_inicio == 2018 ~ 1,
                                 estado == "Yucatán" & año_inicio == 2004 ~ 1,
                                 estado == "Zacatecas" & año_inicio == 2001 ~ 1)) %>% 
  # Reemplazar valores posteriores al primer 1 por 1s
  group_by(estado) %>% 
  fill(alternancia, .direction = "down") %>% 
  ungroup() %>% 
  # Reemplazar NAs por 0s
  mutate(alternancia = ifelse(is.na(alternancia), 0, alternancia)) 

# Datos de legisladoras de representación proporcional ----
bd_rp <- 
  bd_rp %>% 
  mutate(alternancia = case_when(estado == "Aguascalientes" & año_inicio == 2001 ~ 1,
                                 estado == "Baja California" & año_inicio == 1992 ~ 1,
                                 estado == "Baja California Sur" & año_inicio == 2002 ~ 1,
                                 estado == "Chiapas" & año_inicio == 2001 ~ 1,
                                 estado == "Chihuahua" & año_inicio == 1995 ~ 1,
                                 estado == "Ciudad de México" & año_inicio == 2000 ~ 1,
                                 estado == "Durango" & año_inicio == 2018 ~ 1,
                                 estado == "Guanajuato" & año_inicio == 1997 ~ 1,
                                 estado == "Guerrero" & año_inicio == 2008 ~ 1,
                                 estado == "Jalisco" & año_inicio == 1997 ~ 1,
                                 estado == "Michoacán" & año_inicio == 2004 ~ 1,
                                 estado == "Morelos" & año_inicio == 2003 ~ 1,
                                 estado == "Nayarit" & año_inicio == 2002 ~ 1,
                                 estado == "Nuevo León" & año_inicio == 2000 ~ 1,
                                 estado == "Oaxaca" & año_inicio == 2013 ~ 1,
                                 estado == "Puebla" & año_inicio == 2014 ~ 1,
                                 estado == "Querétaro" & año_inicio == 2000 ~ 1,
                                 estado == "Quintana Roo" & año_inicio == 2019 ~ 1,
                                 estado == "San Luis Potosí" & año_inicio == 2006 ~ 1,
                                 estado == "Sinaloa" & año_inicio == 2013 ~ 1,
                                 estado == "Sonora" & año_inicio == 2012 ~ 1,
                                 estado == "Tabasco" & año_inicio == 2015 ~ 1,
                                 estado == "Tamaulipas" & año_inicio == 2019 ~ 1,
                                 estado == "Tlaxcala" & año_inicio == 2001 ~ 1,
                                 estado == "Veracruz" & año_inicio == 2018 ~ 1,
                                 estado == "Yucatán" & año_inicio == 2004 ~ 1,
                                 estado == "Zacatecas" & año_inicio == 2001 ~ 1)) %>% 
  # Reemplazar valores posteriores al primer 1 por 1s
  group_by(estado) %>% 
  fill(alternancia, .direction = "down") %>% 
  ungroup() %>% 
  # Reemplazar NAs por 0s
  mutate(alternancia = ifelse(is.na(alternancia), 0, alternancia)) 


### Unir datos de bd_mr y bd_rp con bd_pob y guardar resultado en bd_mr y bd_rp ----

# Datos de legisladoras de mayoría relativa ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_pob,
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "entidad")) 

# Datos de legisladoras de representación proporcional ----
bd_rp <- 
  bd_rp %>%
  left_join(bd_pob,
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "entidad")) 


### Calcular PIB per capita ----

# Datos de legisladoras de mayoría relativa ----
bd_mr <- 
  bd_mr %>% 
  mutate(pibe_per_capita = pibe/poblacion*1e6, 
         pibe_per_capita_rescalado = rescale(pibe_per_capita))

# Datos de legisladoras de representación proporcional ----
bd_rp <- 
  bd_rp %>% 
  mutate(pibe_per_capita = pibe/poblacion*1e6, 
         pibe_per_capita_rescalado = rescale(pibe_per_capita))


### Unir datos del porcentaje de población urbana ----

# Datos de legisladoras de mayoría relativa ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_pob_urbana,
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "entidad"))

# Datos de legisladoras de representación proporcional ----
bd_rp <- 
  bd_rp %>%
  left_join(bd_pob_urbana,
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "entidad")) 


### Unir datos del porcentaje de población con educación media ----

# Datos de legisladoras de mayoría relativa ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_edu_superior %>% select(-año_info_inegi),
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "entidad"))

# Datos de legisladoras de representación proporcional ----
bd_rp <- 
  bd_rp %>%
  left_join(bd_edu_superior %>% select(-año_info_inegi),
            # Defirnir columnas de cada base con las que haremos la unión
            by = c("año_inicio" = "año", 
                   "estado" = "entidad")) 
