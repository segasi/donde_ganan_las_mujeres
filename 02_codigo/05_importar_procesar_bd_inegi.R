### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R")

### Importar datos de censos y conteos de población sobre la población que vive en localidades con más o menos 2,500 habitantes----

# Fuentes: 

# Bases de datos de censos (1990, 2000, 2010) y conteos de población (1995, 2005). Los datos de la encuesta intercensal serán procesados más abajo  

# De acuerdo con el INEGI, una población se considera rural cuando tiene menos de 2,500 habitantes, mientras que la urbana es aquella donde viven más de 2, 500 personas (http://cuentame.inegi.org.mx/poblacion/rur_urb.aspx?tema_P)

# Ligas: 

# Datos de 1990 a 2010: https://www.inegi.org.mx/programas/ccpv/cpvsh/

# Datos de 2015: https://www.inegi.org.mx/programas/intercensal/2015/

# Nota: el formato de los archivos generados originalmente por la plataforma del INEGI es .xls, pero {readxl} no puede abrirlos. Por eso los guardé en formato .xlsx. Los problemas con los acentos son de origen.

# 1990
bd_pob_1990 <- 
  read_excel("01_datos/inegi/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_1990.xlsx")

# 1995
bd_pob_1995 <- 
  read_excel("01_datos/inegi/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_1995.xlsx")

# 2000
bd_pob_2000 <- 
  read_excel("01_datos/inegi/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_2000.xlsx")

# 2005
bd_pob_2005 <- 
  read_excel("01_datos/inegi/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_2005.xlsx")

# 2010
bd_pob_2010 <- 
  read_excel("01_datos/inegi/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_2010.xlsx")


# Limpiar y homogeneizar bases de datos censos y conteos de población ----

# BDs de 1990, 1995, 2005 y 2010
limpiar_bd_pob <- function(bd) {
  bd <- 
    bd %>% 
    select(entidad = `...2`, total = `...3`, menos_2.5K = `...4`, mas_2.5K = `...5`) %>% 
    filter(!is.na(entidad),
           entidad != "Total",
           !str_detect(entidad, "Unidos")) %>% 
    mutate(entidad = str_trim(entidad),
           entidad = case_when(str_detect(entidad, "Coah") ~ "Coahuila",
                               str_detect(entidad, "Distrito") ~ "Ciudad de México",
                               str_detect(entidad, "M鸩co") ~ "México",
                               str_detect(entidad, "Mich") ~ "Michoacán",
                               str_detect(entidad, "Nuevo L") ~ "Nuevo León",
                               str_detect(entidad, "Quer") ~ "Querétaro",
                               str_detect(entidad, "San") ~ "San Luis Potosí",
                               str_detect(entidad, "Veracruz") ~ "Veracruz",
                               str_detect(entidad, "Yuc") ~ "Yucatán",
                               TRUE ~ entidad),
           total = as.numeric(total),
           menos_2.5K = as.numeric(menos_2.5K),
           mas_2.5K = as.numeric(mas_2.5K),
           por_pob_urbana = mas_2.5K/total*100) %>% 
    select(entidad, por_pob_urbana)
}
 

bd_pob_1990 <- limpiar_bd_pob(bd_pob_1990) %>% mutate(año = 1990)
bd_pob_1995 <- limpiar_bd_pob(bd_pob_1995) %>% mutate(año = 1995)
bd_pob_2005 <- limpiar_bd_pob(bd_pob_2005) %>% mutate(año = 2005)
bd_pob_2010 <- limpiar_bd_pob(bd_pob_2010) %>% mutate(año = 2010)


# BD de 2000

# Esta base debe ser limpiada por separado porque su estructura es diferente a las otras

bd_pob_2000 <- 
  bd_pob_2000 %>% 
  select(entidad = `...2`, 
         total = `...3`, 
         menos_2.5K = `...4`, 
         de_2.5_a_14.5K = `...5`,
         de_15_a_99.9K = `...6`,
         mas_99.9K = `...7`) %>% 
  filter(!is.na(entidad),
         entidad != "Total",
         !str_detect(entidad, "Unidos")) %>% 
  mutate(entidad = str_trim(entidad),
         entidad = case_when(str_detect(entidad, "Coah") ~ "Coahuila",
                             str_detect(entidad, "Distrito") ~ "Ciudad de México",
                             str_detect(entidad, "M鸩co") ~ "México",
                             str_detect(entidad, "Mich") ~ "Michoacán",
                             str_detect(entidad, "Nuevo L") ~ "Nuevo León",
                             str_detect(entidad, "Quer") ~ "Querétaro",
                             str_detect(entidad, "San") ~ "San Luis Potosí",
                             str_detect(entidad, "Veracruz") ~ "Veracruz",
                             str_detect(entidad, "Yuc") ~ "Yucatán",
                             TRUE ~ entidad),
         total = as.numeric(total),
         menos_2.5K = as.numeric(menos_2.5K),
         por_pob_urbana = (total - menos_2.5K)/total*100, 
         año = 2000) %>% 
  select(entidad, por_pob_urbana, año)



### Importar archivos de la encuesta intercensal de 2015 del INEGI, sobre la población que vive en localidades con más o menos 2,500 habitantes ----

# El código en los renglones 114 a 133 solo debe ser ejecutado si no de cuenta con los archivos de la encuesta intercensal en el disco local

# Definir sufijos con acronimos de entidades
# edos <- c("ags", "bc", "bcs", "cam",
#           "coah", "col", "chis", "chih",
#           "cdmx", "dgo", "gto", "gro",
#           "hgo", "jal", "mex", "mich",
#           "mor", "nay", "nl", "oax",
#           "pue", "qro", "qroo", "slp",
#           "sin", "son", "tab", "tamps", 
#           "tlax", "ver", "yuc", "zac")

# Definir comienzo del nombre de las variables a descargar 
# variables <- c("01_poblacion_")

# Descargar datos al folder 01_datos/inegi/encuesta_intercensal_2015/ 
# for (i in seq_along(edos)) {
#   for (j in seq_along(variables)) {
#     curl::curl_download(paste("https://www.inegi.org.mx/contenidos/programas/intercensal/2015/tabulados/", variables[j], edos[i], ".xls", sep = ""), destfile = paste("01_datos/inegi/encuesta_intercensal_2015/", variables[j], edos[i], ".xls", sep = ""))
#     
#   }
# }


# Generar lista con archivos
archivos_pob <- 
  list.files(path = "01_datos/inegi/encuesta_intercensal_2015/", pattern = "01_poblacion")

# Juntar archivos en un solo tibble
bd_pob_2015 <- 
  archivos_pob %>% 
  map(function(x) {
    read_excel(paste0("01_datos/inegi/encuesta_intercensal_2015/", x), sheet = "01", skip = 7, col_names = F) %>% 
      clean_names() # "Limpiar" nombres de variables
  })  %>% 
  # Unir todos los data frames en uno solo
  reduce(rbind) 


# Limpiar y homogeneizar bases de datos de la encuesta intercensal ----

# Renombrar, filtrar y seleccionar columnas; editar nombre de entidades 
bd_pob_2015 <- 
  bd_pob_2015 %>% 
  # Renombrar variables
  rename(entidad = x1, 
         tamaño_loc = x2, 
         gpo_edad = x3,
         estimador = x4, 
         pob_tot = x5, 
         pob_hombres = x6,
         pob_mujeres = x7) %>% 
  # Filtrar renglones relevantes 
  filter(tamaño_loc %in% c("Total", "Menos de 2 500 habitantes"), 
         gpo_edad == "Total",
         estimador == "Valor") %>%
  # Seleccionar columnas
  select(entidad, tamaño_loc, pob_tot) %>% 
  # Generar columna de año, quitar clave de nombre de entidad y editar nombres de entidades
  mutate(año = 2015,
         entidad = str_sub(entidad, start = 4, end = 150),
         entidad = str_trim(entidad),
         entidad = case_when(str_detect(entidad, "Coah") ~ "Coahuila",
                             str_detect(entidad, "Distrito") ~ "Ciudad de México",
                             str_detect(entidad, "M鸩co") ~ "México",
                             str_detect(entidad, "Mich") ~ "Michoacán",
                             str_detect(entidad, "Nuevo L") ~ "Nuevo León",
                             str_detect(entidad, "Quer") ~ "Querétaro",
                             str_detect(entidad, "San") ~ "San Luis Potosí",
                             str_detect(entidad, "Veracruz") ~ "Veracruz",
                             str_detect(entidad, "Yuc") ~ "Yucatán",
                             TRUE ~ entidad))  %>% 
  pivot_wider(names_from = "tamaño_loc",
              values_from = "pob_tot") %>% 
  # Calcular el porcentaje de población urbana
  mutate(por_pob_urbana = 100 - (`Menos de 2 500 habitantes`/Total*100)) %>% 
  # Seleccionar columnas finales
  select(entidad, por_pob_urbana, año) 
  
bd_pob_2015 %>% 
  arrange(-por_pob_urbana) %>% 
  print(n = Inf)


### Unir datos de población que vive en localidades con más o menos 2,500 habitantes de los diferentes censos, conteos y encuesta intercensal ---- 
bd_pob_urbana <- 
  bind_rows(bd_pob_1990, bd_pob_1995, bd_pob_2000, bd_pob_2005, bd_pob_2010, bd_pob_2015)

# Verificar visualmente
bd_pob_urbana %>% 
  ggplot(aes(x = año, y = por_pob_urbana)) +
  geom_line() +
  facet_wrap(~ entidad, ncol = 8)


### Importar datos del porcentaje de la población de 15 años y más con educación superior por entidad federativa 2000 a 2015 ----

# Fuente: https://www.inegi.org.mx/app/tabulados/interactivos/?pxq=Educacion_Educacion_04_9209e1cf-72e7-430b-ac83-9c96d3166750

bd_edu_superior <- 
  read_excel("01_datos/inegi/Educacion_04.xlsx", range = "a5:f37") %>% 
  clean_names() %>% 
  select(entidad = entidad_federativa, `2000` = total_3, `2005` = total_4, `2010` = total_5, `2015` = total_6) %>% 
  pivot_longer(-entidad, names_to = "año", values_to = "por_pob_edu_superior") %>% 
  mutate(año = as.numeric(año))



### Generar columna año_info_inegi ---- 

# La columna año_info_inegi registra el año en que el INEGI levantó la información con la que se calculó el porcentaje de población urbana (1990-2015) y el porcentaje de la población con educación superior (2000-2015). 

# Para hacer la unión con otras bases de datos en donde las variables tienen una temporalidad anual, dado que las variables ue miden el porcentaje de población urbana y el porcentaje de la población con educación se levantaron cada cinco años, se adoptó el siguiente criterio: asignar el dato de un levantamiento de información (p. ej., 1990), al año del levantamiento (1990), así como a los dos años previos y posteriores (1988, 1989, 1991 y 1992). La idea es que el valor de la medición sea lo más vigente posible en términos temporales. La única excepción es para las observaciones de 1987, porque el INEGI no llevó a cabo un conteo de población en 1985. Por lo tanto, para este año se usa la información del censo de 1990.

bd_año_info_inegi <- 
  tibble(año = 1987:2019) %>% 
  mutate(año_info_inegi = ifelse(año %in% c(1990, 1995, 2000, 2005, 2010, 2015), año, NA)) %>% 
  fill(año_info_inegi, .direction = "up") %>% 
  fill(año_info_inegi, .direction = "down") %>% 
  mutate(foo = año - año_info_inegi,
         año_info_inegi = ifelse(año - año_info_inegi >= -2, año_info_inegi, año_info_inegi - 5),
         año_info_inegi = ifelse(año_info_inegi == 1985, 1990, año_info_inegi)) %>% 
  select(-foo) %>% 
  print(n = Inf)

### Agregar variable año_info_inegi a bd_pob_urbana y bd_edu_superior ----
bd_pob_urbana <- 
  bd_año_info_inegi %>% 
  left_join(bd_pob_urbana, by = c("año_info_inegi" = "año")) %>% 
  select(entidad, everything()) 

bd_edu_superior <-  
  bd_año_info_inegi %>% 
  left_join(bd_edu_superior, by = c("año_info_inegi" = "año")) %>% 
  select(entidad, everything()) %>% 
  filter(!is.na(entidad)) 


### Homogeneizar el nombre de algunas entidades en bd_pob_urbana y bd_edu_superior ----

# Esto es necesario para unir posteriormente estos datos a las bd's principales 
bd_pob_urbana <- 
  bd_pob_urbana %>% 
  mutate(entidad = ifelse(entidad == "México", "Estado de México", entidad)) 

bd_edu_superior <-  
  bd_edu_superior %>% 
  mutate(entidad = str_trim(entidad),
         entidad = case_when(str_detect(entidad, "Coah") ~ "Coahuila",
                             entidad == "México" ~ "Estado de México",
                             str_detect(entidad, "Mich") ~ "Michoacán",
                             str_detect(entidad, "Veracruz") ~ "Veracruz", 
                             TRUE ~ entidad))


### Gráficas ----

# % población urbana
bd_pob_urbana %>% 
  ggplot(aes(x = año_info_inegi, y = por_pob_urbana)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ entidad, ncol = 8) + 
  labs(title = "% población urbana") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))

# % población con al menos un año de edu. superior
bd_edu_superior %>% 
  ggplot(aes(x = año_info_inegi, y = por_pob_edu_superior)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ entidad, ncol = 8) + 
  labs(title = "% población con al menos un año de edu. superior") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 0.5))