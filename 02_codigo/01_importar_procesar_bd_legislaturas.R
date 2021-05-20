### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R")


### Importar datos ----
ags <- 
  read_excel("01_datos/Electas/Base-Electas-Aguascalientes.xlsx",
             sheet = "Lista Histórica Aguascalientes", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

bc <- 
  read_excel("01_datos/Electas/Base-Electas-Baja-California.xlsx",
             sheet = "Lista Histórico BC", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)


bcs <- 
  read_excel("01_datos/Electas/Base-Electas-Baja-California-Sur.xlsx",
             sheet = "Lista Histórico BCS", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion) %>% 
  mutate(estado = "Baja California Sur") %>% 
  # Esta observación se elimina porque Tuchman renunció al PAN antes de la elección y posteriormente se convocó a una nueva elección para el distrito 2. Fuente: https://es.wikipedia.org/wiki/Anexo:VII_Legislatura_del_Congreso_del_Estado_de_Baja_California_Sur
  filter(!str_detect(apellido, "Tuchman"))



camp <- 
  read_excel("01_datos/Electas/Base-Electas-Campeche.xlsx",
             sheet = "Lista Histórico Campeche", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

chis <- 
  read_excel("01_datos/Electas/Base-Electas-Chiapas.xlsx",
             sheet = "Lista Histórico Chiapas", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

chih <- 
  read_excel("01_datos/Electas/Base-Electas-Chihuahua.xlsx",
             sheet = "Lista Histórico Chihuahua", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

cdmx <- 
  read_excel("01_datos/Electas/Base-Electas-Ciudad de México.xlsx",
             sheet = "Lista Histórico CDMX", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

coah <- 
  read_excel("01_datos/Electas/Base-Electas-Coahuila.xlsx",
             sheet = "Lista Histórica Coahuila", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

col <- 
  read_excel("01_datos/Electas/Base-Electas-Colima.xlsx",
             sheet = "Lista Histórico Colima", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

dgo <- 
  read_excel("01_datos/Electas/Base-Electas-Durango.xlsx",
             sheet = "Lista Histórico Durango", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

mex <- 
  read_excel("01_datos/Electas/Base-Electas-Estado-de-Mexico.xlsx",
             sheet = "Listado Histórico EdoMex", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion) %>% 
  mutate(estado = "Estado de México")

gto <- 
  read_excel("01_datos/Electas/Base-Electas-Guanajuato.xlsx",
             sheet = "Lista Histórico Guanajuato", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

gro <- 
  read_excel("01_datos/Electas/Base-Electas-Guerrero.xlsx",
             sheet = "Lista Histórico Guerrero", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

hgo <- 
  read_excel("01_datos/Electas/Base-Electas-Hidalgo.xlsx",
             sheet = "Lista Histórico Hidalgo", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

jal <- 
  read_excel("01_datos/Electas/Base-Electas-Jalisco.xlsx",
             sheet = "Lista Histórica Jalisco", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion) %>% 
  rename(distrito_electoral = distrito_electoral_local)

mich <- 
  read_excel("01_datos/Electas/Base-Electas-Michoacán.xlsx",
             sheet = "Lista Histórico Michoacán", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

mor <- 
  read_excel("01_datos/Electas/Base-Electas-Morelos.xlsx",
             sheet = "Lista Histórico Morelos", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

nay <- 
  read_excel("01_datos/Electas/Base-Electas-Nayarit.xlsx",
             sheet = "Lista Histórico Nayarit", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

nl <- 
  read_excel("01_datos/Electas/Base-Electas-Nuevo-León.xlsx",
             # sheet = "Lista Histórico Nayarit", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

oax <- 
  read_excel("01_datos/Electas/Base-Electas-Oaxaca.xlsx",
             # sheet = "Lista Histórico Nayarit", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

pue <- 
  read_excel("01_datos/Electas/Base-Electas-Puebla.xlsx",
             sheet = "Lista Histórico Puebla", 
             range = "a2:j494") %>% 
  clean_names() %>% 
  select(-circunscripcion)

qro <- 
  read_excel("01_datos/Electas/Base-Electas-Querétaro.xlsx",
             sheet = "Lista Histórico Querétaro", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

qroo <- 
  read_excel("01_datos/Electas/Base-Electas-Quintana-Roo.xlsx",
             sheet = "Lista Histórico Quintana Roo", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

slp <- 
  read_excel("01_datos/Electas/Base-Electas-San-Luis-Potosi.xlsx",
             sheet = "Lista Histórico SLP", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

sin <- 
  read_excel("01_datos/Electas/Base-Electas-Sinaloa.xlsx",
             sheet = "Lista Histórico Sinaloa", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

son <- 
  read_excel("01_datos/Electas/Base-Electas-Sonora.xlsx",
             sheet = "Lista Histórico Sonora", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

tab <- 
  read_excel("01_datos/Electas/Base-Electas-Tabasco.xlsx",
             sheet = "Lista Histórico Tabasco", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

tamps <- 
  read_excel("01_datos/Electas/Base-Electas-Tamaulipas.xlsx",
             sheet = "Lista Histórico Tamaulipas", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

tlax <- 
  read_excel("01_datos/Electas/Base-Electas-Tlaxcala.xlsx",
             sheet = "Lista Histórico Tlaxcala", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

ver <- 
  read_excel("01_datos/Electas/Base-Electas-Veracruz.xlsx",
             sheet = "Lista Histórico Veracruz", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

yuc <- 
  read_excel("01_datos/Electas/Base-Electas-Yucatán.xlsx", 
             sheet = "Listá Histórico Yucatán", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

zac <- 
  read_excel("01_datos/Electas/Base-Electas-Zacatecas.xlsx", 
             sheet = "Lista Histórico Zacatecas",
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)




### Unir las diversas bases de datos de legisladoras y legisladores ----
bd_legislaturas_inicial <- 
  bind_rows(ags, bc, bcs, camp, chis, chih, cdmx, coah,
            col, dgo, mex, gto, gro, hgo, jal, mich,
            mor, nay, nl, oax, pue, qro, qroo, slp,
            sin, son, tab, tamps, tlax, ver, yuc, zac)

### Homogeneizar valores de sexo, principio de representación y propietario/suplente ----
bd_legislaturas_inicial <- 
  bd_legislaturas_inicial %>% 
  mutate(sexo = str_to_sentence(sexo), 
         sexo = case_when(sexo == "H" ~ "Hombre",
                          sexo == "Masculino" ~ "Hombre",
                          sexo == "M" ~ "Mujer",
                          sexo == "Femenino" ~ "Mujer",
                          TRUE ~ sexo), 
         principio_de_representacion = str_to_title(principio_de_representacion),
         principio_de_representacion = case_when(str_detect(principio_de_representacion, "Rep") ~ "Representación Proporcional",
                                                 str_detect(principio_de_representacion, "Rp") ~ "Representación Proporcional",
                                                 str_detect(principio_de_representacion, "Pluri") ~ "Representación Proporcional",
                                                 str_detect(principio_de_representacion, "May") ~ "Mayoría Relativa",
                                                 str_detect(principio_de_representacion, "Mr") ~ "Mayoría Relativa"
         ),
         propietario_o_suplente = str_to_title(propietario_o_suplente),
         propietario_o_suplente = case_when(str_detect(propietario_o_suplente, "Pr") ~ "Propietaria/o",
                                            TRUE ~ propietario_o_suplente))


### Generar tibble solo con datos de legisladoras y legisladores propietarios ---- 
bd_legislaturas <- 
  bd_legislaturas_inicial %>% 
  filter(propietario_o_suplente == "Propietaria/o")



### Calcular número y porcentaje de legisladores por entidad, género y legislatura ----

# Ambos principios ----
bd_prop_todes <- 
  bd_legislaturas %>% 
  # Calcular el número y proporción de legisladoras y legisladores, así como el número total de curules en cada legislatura
  group_by(estado, periodo, .drop = FALSE) %>% 
  summarise(num_legisladoras = sum(sexo == "Mujer"), 
            num_legisladores = sum(sexo == "Hombre"),
            total_curules = n(),
            prop_legisladoras = num_legisladoras/total_curules,
            prop_legisladores = num_legisladores/total_curules) %>% 
  ungroup() %>% 
  # Construir variable para identifcar en qué año comenzó la legislatura
  mutate(año_inicio = as.numeric(str_sub(periodo, start = 1, end = 4))) %>% 
  select(estado, periodo, año_inicio, everything())


# Mayoría relativa ----
bd_prop_mr <- 
  bd_legislaturas %>% 
  # Filtrar para solo mantener las observaciones de las y los legisladores electos por el principio de mayoría relativa
  filter(principio_de_representacion == "Mayoría Relativa") %>% 
  # Calcular el número y proporción de legisladoras y legisladores, así como el número total de curules en cada legislatura
  group_by(estado, periodo, .drop = FALSE) %>% 
  summarise(num_legisladoras = sum(sexo == "Mujer"), 
            num_legisladores = sum(sexo == "Hombre"),
            total_curules = n(),
            prop_legisladoras = num_legisladoras/total_curules,
            prop_legisladores = num_legisladores/total_curules) %>% 
  ungroup() %>% 
  # Construir versión rezagada de la variable prop_legisladoras
  group_by(estado) %>% 
  mutate(prop_legisladoras_lag = lag(prop_legisladoras)) %>% 
  ungroup() %>% 
  # Construir variables para identifcar el principio de representación mediante el cual fueron electos esteas y estois legisladores y en qué año comenzó la legislatura
  mutate(principio_de_representacion = "Mayoría Relativa",
         año_inicio = as.numeric(str_sub(periodo, start = 1, end = 4))) %>% 
  select(estado, periodo, año_inicio, everything())

# Representación proporcional ----
bd_prop_rp <- 
  bd_legislaturas %>% 
  # Filtrar para solo mantener las observaciones de las y los legisladores electos por el principio de representación proporcional
  filter(principio_de_representacion == "Representación Proporcional") %>% 
  # Calcular el número y proporción de legisladoras y legisladores, así como el número total de curules en cada legislatura
  group_by(estado, periodo, .drop = FALSE) %>% 
  summarise(num_legisladoras = sum(sexo == "Mujer"), 
            num_legisladores = sum(sexo == "Hombre"),
            total_curules = n(),
            prop_legisladoras = num_legisladoras/total_curules,
            prop_legisladores = num_legisladores/total_curules) %>% 
  ungroup() %>% 
  # Construir versión rezagada de la variable prop_legisladoras
  group_by(estado) %>% 
  mutate(prop_legisladoras_lag = lag(prop_legisladoras)) %>% 
  ungroup() %>%
  # Construir variables para identifcar el principio de representación mediante el cual fueron electos esteas y estois legisladores y en qué año comenzó la legislatura
  mutate(principio_de_representacion = "Representación Proporcional") %>% 
  mutate(año_inicio = as.numeric(str_sub(periodo, start = 1, end = 4))) %>% 
  select(estado, periodo, año_inicio, everything())


### Remover objetos que ya no usaremos del ambiente de trabajo -----
remove(list = c("ags", "bc", "bcs", "camp", "chis", "chih",
                "cdmx", "coah", "col", "dgo", "mex", "gto",
                "gro", "hgo", "jal", "mich", "mor", "nay", 
                "nl", "oax", "pue", "qro", "qroo", "slp",
                "sin", "son", "tab", "tamps", "tlax", "ver",
                "yuc", "zac", "bd_legislaturas_inicial"))
