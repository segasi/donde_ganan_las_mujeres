### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R")

### Importar datos ----
bd_pibe <- 
  read_excel("01_datos/tabulados_pibent/PIBER_2.xlsx",
             skip = 4,
             range = "a5:an43") 


### Eliminar observaciones que no corresponden a entidades ----
bd_pibe <- 
  bd_pibe %>% 
  filter(!is.na(`1980`), 
         !str_detect(Concepto, "___"),
         !str_detect(Concepto, "D.21"),
         !str_detect(Concepto, "Estados")) 

### Generar columna para 2019, que tendrá los mismos valores que la de 2018 ----
bd_pibe <- 
  bd_pibe %>% 
  mutate(`2019` = `2018P`)

### Transformar datos a formato long y hacer algunas transformaciones a la variable pibe ----
bd_pibe <- 
  bd_pibe %>%
  pivot_longer(-Concepto,
               names_to = "año",
               values_to = "pibe") %>% 
  mutate(año = str_replace(año, "R", ""),
         año = str_replace(año, "P", ""),
         año = as.numeric(año)) %>% 
  # Renombrar variable Concepto por estado
  rename(estado = Concepto)

### Generar pibe_reescalado, versión de pibe reescalada al intervalo 0-1 ----

# Esta transformación es necesaria porque las unidades originales de pibe (millones de pesos) son muy diferentes a la del resto de las variables, lo cual genera problemas al momento de estimar los modelos
bd_pibe <- 
  bd_pibe %>% 
  mutate(pibe_reescalado = rescale(pibe))


### Generar versiones rezagadas de pibe_reescalado ----
bd_pibe <- 
  bd_pibe %>% 
  group_by(estado) %>% 
  mutate(pibe_reescalado_lag_1 = lag(pibe_reescalado),
         pibe_reescalado_lag_2 = lag(pibe_reescalado, n = 2),
         pibe_reescalado_lag_3 = lag(pibe_reescalado, n = 3)) %>% 
  ungroup()
  
### Cambiar nombres de tres entidades para después poder unir esta base de datos con otras ----
bd_pibe <- bd_pibe %>% 
  mutate(estado = case_when(estado == "Coahuila de Zaragoza" ~ "Coahuila",
                            estado == "Michoacán de Ocampo" ~ "Michoacán",
                            estado == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                            estado == "México" ~ "Estado de México",
                            TRUE ~ estado))
