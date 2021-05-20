### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R")

### Importar datos ----

# Proyecciones poblacionales de CONAPO 

# Fuentes: 

# Población a mitad de año

# Población a mitad de año. Para la República Mexicana el periodo es de 1950-2050, para las entidades federativas el periodo es de 1970-2050.

# http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv

bd_pob <- 
  read_delim("01_datos/conapo/pob_mit_proyecciones.csv", "," , locale = locale(encoding = "latin1")) %>% 
  clean_names() %>% 
  rename(año = ano)

### Calcular población por año y entidad ----
bd_pob <- 
  bd_pob %>% 
  filter(entidad != "República Mexicana") %>% 
  mutate(entidad = ifelse(entidad == "México", "Estado de México", entidad)) %>% 
  group_by(año, entidad) %>% 
  summarise(poblacion = sum(poblacion)) %>% 
  ungroup() 
