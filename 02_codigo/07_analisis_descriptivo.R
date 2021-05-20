### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R")

### Cargar, procesar y unir bases de datos ----
source("02_codigo/06_unir_bd.R")

### Número total de legislaturas analizadas por principio de elección ----

# Datos
bd_mr %>% 
  tally()

# Datos
bd_rp %>% 
  tally()

### Figura 1: Número de legislaturas analizadas por estado ----

# Datos
bd_mr %>% 
  count(año_inicio) %>% 
  print(n = Inf)

# Gráfica
bd_mr %>% 
  count(estado, sort = T) %>% 
  ggplot(aes(x = n, y = fct_reorder(estado, n))) +
  geom_segment(aes(x = 0, xend = n, y = fct_reorder(estado, n), yend = fct_reorder(estado, n)),
               size = 1, color = "grey40") +
  geom_point(size = 3, color = "grey40") +
  tema +
  scale_x_continuous(breaks = 0:12) +
  labs(x = "\nNúmero de legislaturas analizadas",
       y = NULL) +
  theme(panel.grid.major.y = element_blank()) +
  ggsave("03_graficas/figura_1_numero_legislaturas_analizadas_por_estado_1987_2021.png", width = 16, height = 9, dpi = 200)


### Número de cambios en las normas electorales estatales en materia de género, por año ----

# Mayoría relativa
bd_indice %>% 
  filter(!str_detect(entidad, "Fede"),
         str_detect(principio_de_representacion, "Mayor")) %>% 
  count(ano_de_publicacion_de_la_ley) %>% 
  mutate(total = sum(n)) %>% 
  print(n = Inf)

# Representación proporcional
bd_indice %>% 
  filter(!str_detect(entidad, "Fede"),
         str_detect(principio_de_representacion, "Rep")) %>% 
  count(ano_de_publicacion_de_la_ley) %>% 
  mutate(total = sum(n)) %>% 
  print(n = Inf)



### Número de cambios en las normas electorales estatales en materia de género, por estado ----

# Mayoría relativa
bd_indice %>% 
  filter(!str_detect(entidad, "Fede"),
         str_detect(principio_de_representacion, "Mayor")) %>%
  count(entidad) %>% 
  mutate(total = sum(n)) %>% 
  print(n = Inf)

# Representación proporcional
bd_indice %>% 
  filter(!str_detect(entidad, "Fede"),
         !str_detect(principio_de_representacion, "Rep")) %>% 
  count(ano_de_publicacion_de_la_ley) %>% 
  mutate(total = sum(n)) %>% 
  print(n = Inf)


### Figura 2: Promedio porcentual anual de legisladoras y legisladores locales en México, 1991-2019 ---- 

# Datos 
bd_prop_todes %>% 
  # Agregar un par de renglones falsos para lograr que en el siguiente paso cada entidad tenga valores en 2014 y 2020
  add_row(estado = "foo", año_inicio = c(2014)) %>% 
  add_row(estado = "foo", año_inicio = c(2020)) %>% 
  complete(estado, nesting(año_inicio)) %>% 
  # Eliminar las observaciones fictícias del estado "foo"
  filter(estado != "foo") %>% 
  # Reemplazar NAs por el valor de la última celda anterior
  group_by(estado) %>% 
  # glimpse()
  fill(c(periodo, num_legisladoras, num_legisladores, total_curules, prop_legisladoras, prop_legisladores), .direction = "down") %>%
  ungroup() %>% 
  # Eliminar observaciones para los que no existe información en una entidad
  filter(!is.na(periodo)) %>% 
  group_by(año_inicio) %>% 
  summarise(Legisladoras = mean(prop_legisladoras*100),
            Legisladores = mean(prop_legisladores*100)) %>% 
  ungroup() %>% 
  filter(año_inicio > 1990, 
         año_inicio < 2020) %>% 
  mutate(brecha = Legisladores - Legisladoras) %>% 
  print(n = Inf)
  

# Gráfica
bd_prop_todes %>% 
  # Agregar un par de renglones falsos para lograr que en el siguiente paso cada entidad tenga valores en 2014 y 2020
  add_row(estado = "foo", año_inicio = c(2014)) %>% 
  add_row(estado = "foo", año_inicio = c(2020)) %>% 
  complete(estado, nesting(año_inicio)) %>% 
  # Eliminar las observaciones fictícias del estado "foo"
  filter(estado != "foo") %>% 
  # Reemplazar NAs por el valor de la última celda anterior
  group_by(estado) %>% 
  # glimpse()
  fill(c(periodo, num_legisladoras, num_legisladores, total_curules, prop_legisladoras, prop_legisladores), .direction = "down") %>%
  ungroup() %>% 
  # Eliminar observaciones para los que no existe información en una entidad
  filter(!is.na(periodo)) %>% 
  group_by(año_inicio) %>% 
  summarise(Legisladoras = mean(prop_legisladoras*100),
            Legisladores = mean(prop_legisladores*100)) %>% 
  ungroup() %>% 
  filter(año_inicio > 1990, 
         año_inicio < 2020) %>% 
  pivot_longer(-año_inicio,
               names_to = "tipo",
               values_to = "porcentaje") %>% 
  ggplot(aes(x = año_inicio, 
             y = porcentaje,
             color = tipo,
             shape = tipo)) +
  geom_point(size = 5) +
  scale_x_continuous(breaks = c(1987, seq(1991, 2020, 1)), 
                     limits = c(1990.5, 2019.5),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_shape_manual(values = c(Legisladores = 21, Legisladoras = 19),
                     labels = c(Legisladores = "Legisladores",  Legisladoras = "Legisladoras")) +
  scale_color_manual(values = c(Legisladores = "grey40", Legisladoras = "grey40"),
                     labels = c(Legisladores = "Legisladores",  Legisladoras = "Legisladoras")) +
  labs(x = "\nAño   ",
       y = "Promedio porcentual\n",
       color = NULL,
       shape = NULL) +
  tema +
  theme(axis.text = element_text(size = 23),
        axis.text.x = element_text(size = 23, angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 20, lineheight = 1.1),
        legend.key.height = unit(1, "cm"),
        legend.direction = "vertical",
        legend.position = c(0.88, 0.87)) +
  ggsave("03_graficas/figura_2_puntos_promedio_porcentual_anual_de_legisladoras_y_legisladores_locales_en_México_1991_2019.png", width = 16, height = 9, dpi = 200)


# Gráfica para polis paritaria
bd_prop_todes %>% 
  # Agregar un par de renglones falsos para lograr que en el siguiente paso cada entidad tenga valores en 2014 y 2020
  add_row(estado = "foo", año_inicio = c(2014)) %>% 
  add_row(estado = "foo", año_inicio = c(2020)) %>% 
  complete(estado, nesting(año_inicio)) %>% 
  # Eliminar las observaciones fictícias del estado "foo"
  filter(estado != "foo") %>% 
  # Reemplazar NAs por el valor de la última celda anterior
  group_by(estado) %>% 
  # glimpse()
  fill(c(periodo, num_legisladoras, num_legisladores, total_curules, prop_legisladoras, prop_legisladores), .direction = "down") %>%
  ungroup() %>% 
  # Eliminar observaciones para los que no existe información en una entidad
  filter(!is.na(periodo)) %>% 
  group_by(año_inicio) %>% 
  summarise(Legisladoras = mean(prop_legisladoras*100),
            Legisladores = mean(prop_legisladores*100)) %>% 
  ungroup() %>% 
  filter(año_inicio > 1990, 
         año_inicio < 2020) %>% 
  pivot_longer(-año_inicio,
               names_to = "tipo",
               values_to = "porcentaje") %>% 
  ggplot(aes(x = año_inicio, 
             y = porcentaje,
             color = tipo,
             shape = tipo)) +
  geom_point(size = 5) +
  scale_x_continuous(breaks = c(1987, seq(1991, 2020, 1)), 
                     limits = c(1990.5, 2019.5),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_shape_manual(values = c(Legisladores = 21, Legisladoras = 19),
                     labels = c(Legisladores = "Legisladores",  Legisladoras = "Legisladoras")) +
  scale_color_manual(values = c(Legisladores = "grey40", Legisladoras = "grey40"),
                     labels = c(Legisladores = "Legisladores",  Legisladoras = "Legisladoras")) +
  labs(title = "Porcentaje promedio de mujeres y hombres en las legislaturas de las\nentidades federativas, 1991-2019",
       x = "\nAño   ",
       y = "Promedio porcentual\n",
       color = NULL,
       shape = NULL,
       caption = "@PolisParitaria #RepresentaciónParitaria") +
  tema +
  theme(plot.title = element_text(family = "Lato", color = "#c9056f", face = "bold", size = 36),
        plot.caption = element_text(family = "Lato", color = "#c9056f", size = 18),
        axis.text = element_text(size = 23, family = "Open Sans Light"),
        axis.text.x = element_text(size = 23, angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 26, family = "Lato"),
        legend.text = element_text(size = 20, lineheight = 1.1, family = "Open Sans Light"),
        
        legend.key.height = unit(1, "cm"),
        legend.direction = "vertical",
        legend.position = c(0.88, 0.87)) +
  ggsave("03_graficas/figura_2_puntos_promedio_porcentual_anual_de_legisladoras_y_legisladores_locales_en_México_1991_2019_polis.png", width = 16, height = 9, dpi = 200)


### Figura 3: Porcentaje de legisladoras locales por entidad y año de inicio de legislatura ----

# Gráfica no incluida en el artículo
bd_prop_todes %>% 
  ggplot(aes(x = año_inicio, 
             y = prop_legisladoras*100,
             group = estado)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 50, color = "salmon", linetype = 3) +
  scale_x_continuous(breaks = c(seq(1990, 2015, 5), 2019)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  facet_wrap(~ estado, ncol= 8) +
  labs(title = NULL, 
       x = "\n",
       y = "Porcentaje\n") +
  tema +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
        axis.text.y = element_text(size = 11),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2))   +
  ggsave("03_graficas/figura_3_lineas_porcentaje_legisladoras_locales_por_entidad_año_inicio_legislatura_México_1987_2019.png", width = 16, height = 9, dpi = 200)


### Figura XX: Porcentaje de legisladoras locales por entidad y año ----

# Gráfica no incluida en el artículo
bd_prop_todes %>% 
  # Generar variable que registra el año en que inició la legislatura
  mutate(año = as.numeric(str_sub(periodo, start = 1, end = 4))) %>% 
  # Agregar un par de renglones falsos para lograr que en el siguiente paso cada entidad tenga valores en 2014 y 2020
  add_row(estado = "foo", año = c(2014)) %>% 
  add_row(estado = "foo", año = c(2020)) %>% 
  complete(estado, nesting(año)) %>% 
  # Eliminar las observaciones fictícias del estado "foo"
  filter(estado != "foo") %>% 
  # Reemplazar NAs por el valor de la última celda anterior
  group_by(estado) %>% 
  # glimpse()
  fill(c(periodo, num_legisladoras, num_legisladores, total_curules, prop_legisladoras, prop_legisladores), .direction = "down") %>%
  ungroup() %>% 
  # Eliminar observaciones para los que no existe información en una entidad
  filter(!is.na(periodo)) %>% 
  group_by(año) %>% 
  mutate(ranking = rank(-prop_legisladoras, ties.method = "first")) %>% 
  ungroup() %>% 
  select(estado, año, prop_legisladoras, ranking) %>% 
  # arrange(ranking) %>%  
  # filter(str_detect(estado, "San")) %>% 
  ggplot(aes(x = año, 
             y = fct_rev(estado),
             fill = prop_legisladoras*100)) +
  geom_tile(color = "grey80") +
  scale_x_continuous(breaks = 1987:2020, expand = c(0, 0)) +
  scale_fill_gradient(low = "white", high = "grey30", na.value = "grey95",
                      limits = c(0, 70),
                      breaks = seq(0, 70, 10)) +
  labs(title = NULL, 
       x = "\n\n",
       y = NULL,
       caption = "\n",
       fill = "%") +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.position = c(0.82, -0.15),
        legend.direction = "horizontal")  +
  ggsave("03_graficas/figura_XX_heatmap_porcentual_anual_de_legisladoras_locales_en_México_1987_2020.png", width = 16, height = 9, dpi = 200)


### Figura 4: Evolución del Índice de fortaleza del Régimen Electoral de Género ---- 

bd_indice_edos %>% 
  select(estado, año_publicacion, principio_de_representacion, ifreg) %>% 
  distinct() %>% 
  ggplot(aes(x = año_publicacion, 
             y = ifreg, 
             color = principio_de_representacion, 
             group = principio_de_representacion)) +
  geom_vline(xintercept = 2014, color = "salmon", linetype = 2, size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_color_manual(values = c("grey70", "grey30")) +
  facet_wrap(~ estado, ncol= 8) +
  labs(title = NULL, 
       x = "\nAño de publicación\n",
       y = "Valor\n", 
       color = NULL) +
  tema +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        legend.position = c(0.16, -0.11),
        legend.direction = "horizontal", 
        legend.text = element_text(size = 17)) +
  ggsave("03_graficas/figura_4_lineas_evolucion_ifreg_1994_2016.png", width = 16, height = 9, dpi = 200)



### Estadísticas descriptivas de las variables de respuesta y explicativas ----

## Proporciones de legisladoras ----

# Resumen de cinco estadísticas de la proporción de legisladoras de MR y RP
summary(bd_mr$prop_legisladoras)
summary(bd_rp$prop_legisladoras)

# Desviaciones estándar generales de la proporción de legisladoras de MR y RP
sd(bd_mr$prop_legisladoras) 
sd(bd_rp$prop_legisladoras)


# Rango de medias y desviaciones estándar estatales de la proporción de legisladoras de MR
bd_mr %>% 
  group_by(estado) %>% 
  summarise(media_prop_legisladoras = mean(prop_legisladoras),
            des_est_prop_legisladoras = sd(prop_legisladoras)) %>% 
  ungroup() %>% 
  # arrange(-media_prop_legisladoras) %>%
  arrange(-des_est_prop_legisladoras) %>%
  print(n = Inf)


# Rango de medias y desviaciones estándar estatales de la proporción de legisladoras de RP
bd_rp %>% 
  group_by(estado) %>% 
  summarise(media_prop_legisladoras = mean(prop_legisladoras),
            des_est_prop_legisladoras = sd(prop_legisladoras)) %>% 
  ungroup() %>% 
  # arrange(-media_prop_legisladoras) %>%
  arrange(-des_est_prop_legisladoras) %>%
  print(n = Inf)


## IFREG ----

# Resumen de cinco estadísticas del IFREG para MR y RP
summary(bd_mr$ifreg)
summary(bd_rp$ifreg)

# Desviaciones estándar generales del IFREG para MR y RP
sd(bd_mr$ifreg) 
sd(bd_rp$ifreg)


# Rango de medias y desviaciones estándar estatales del IFREG para MR
bd_mr %>% 
  group_by(estado) %>% 
  summarise(media_ifreg = mean(ifreg),
            des_est_ifreg = sd(ifreg)) %>% 
  ungroup() %>% 
  arrange(-media_ifreg) %>%
  # arrange(-des_est_ifreg) %>%
  print(n = Inf)


# Rango de medias y desviaciones estándar estatales del IFREG para RP
bd_rp %>% 
  group_by(estado) %>% 
  summarise(media_ifreg = mean(ifreg),
            des_est_ifreg = sd(ifreg)) %>% 
  ungroup() %>% 
  arrange(-media_ifreg) %>%
  # arrange(-des_est_ifreg) %>%
  print(n = Inf)

## Antigüedad norma de género ----
 
# Resumen de cinco estadísticas para MR y RP
summary(bd_mr$antiguedad_primera_norma)
summary(bd_rp$antiguedad_primera_norma)

# Desviaciones estándar generales para MR y RP
sd(bd_mr$antiguedad_primera_norma, na.rm = T) 
sd(bd_rp$antiguedad_primera_norma, na.rm = T)


# Rango de medias y desviaciones estándar estatales del IFREG para MR
bd_rp %>% 
  group_by(estado) %>% 
  summarise(media_antiguedad_primera_norma = mean(antiguedad_primera_norma, na.rm = T),
            des_est_antiguedad_primera_norma = sd(antiguedad_primera_norma, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(-media_antiguedad_primera_norma) %>%
  # arrange(-des_est_antiguedad_primera_norma) %>%
  print(n = Inf)


## PIB estatal per cápita ----

# Resumen de cinco estadísticas
summary(bd_mr$pibe_per_capita)

# Desviaciones estándar generales para MR y RP
sd(bd_mr$pibe_per_capita, na.rm = T) 
sd(bd_rp$pibe_per_capita, na.rm = T)


# Rango de medias y desviaciones estándar estatales para MR
bd_mr %>% 
  group_by(estado) %>% 
  summarise(media_pibe_per_capita = mean(pibe_per_capita, na.rm = T),
            des_pibe_per_capita = sd(pibe_per_capita, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(-media_pibe_per_capita) %>%
  # arrange(-des_pibe_per_capita) %>%
  print(n = Inf)

## Alternancia ----

# Resumen de cinco estadísticas
summary(bd_mr$alternancia)
summary(bd_rp$alternancia)

# Desviaciones estándar generales para MR y RP
sd(bd_mr$alternancia, na.rm = T) 
sd(bd_rp$alternancia, na.rm = T)


# Rango de medias y desviaciones estándar estatales para MR
bd_mr %>% 
  group_by(estado) %>% 
  summarise(media_alternancia = mean(alternancia, na.rm = T),
            des_alternancia = sd(alternancia, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(-media_alternancia) %>%
  # arrange(-des_alternancia) %>%
  print(n = Inf)
  
## Porcentaje de población urbana ----

# Resumen de cinco estadísticas
summary(bd_mr$por_pob_urbana)

# Desviaciones estándar generales para MR y RP
sd(bd_mr$por_pob_urbana, na.rm = T) 
sd(bd_rp$por_pob_urbana, na.rm = T)


# Rango de medias y desviaciones estándar estatales para MR
bd_mr %>% 
  group_by(estado) %>% 
  summarise(media_por_pob_urbana = mean(por_pob_urbana, na.rm = T),
            des_por_pob_urbana = sd(por_pob_urbana, na.rm = T)) %>% 
  ungroup() %>% 
  # arrange(-media_por_pob_urbana) %>%
  arrange(-des_por_pob_urbana) %>%
  print(n = Inf)


## Porcentaje de población con al menos un año aprobado de estudios de educación superior ----

# Resumen de cinco estadísticas
summary(bd_mr$por_pob_edu_superior)

# Desviaciones estándar generales para MR y RP
sd(bd_mr$por_pob_edu_superior, na.rm = T) 
sd(bd_rp$por_pob_edu_superior, na.rm = T)


# Rango de medias y desviaciones estándar estatales para MR
bd_mr %>% 
  group_by(estado) %>% 
  summarise(media_por_pob_edu_superior = mean(por_pob_edu_superior, na.rm = T),
            des_por_pob_edu_superior = sd(por_pob_edu_superior, na.rm = T)) %>% 
  ungroup() %>% 
  # arrange(-media_por_pob_edu_superior) %>%
  arrange(-des_por_pob_edu_superior) %>%
  print(n = Inf)


### Análisis de correlaciones entre IFREG y Antiguedad ----

# Todas las observaciones de bd_mr ----
bd_mr %>% 
  filter(!is.na(antiguedad_primera_norma)) %>% 
  summarise(correlacion_pearson = cor(antiguedad_primera_norma, ifreg, method = "pearson"),
            correlacion_spearman = cor(antiguedad_primera_norma, ifreg, method = "spearman"),
            correlacion_kendall = cor(antiguedad_primera_norma, ifreg, method = "kendall")) 

# Por entidad, con datos de bd_mr ----
bd_mr %>% 
  filter(!is.na(antiguedad_primera_norma)) %>% 
  group_by(estado) %>% 
  summarise(correlacion_pearson = cor(antiguedad_primera_norma, ifreg, method = "pearson"),
            correlacion_spearman = cor(antiguedad_primera_norma, ifreg, method = "spearman"),
            correlacion_kendall = cor(antiguedad_primera_norma, ifreg, method = "kendall")) %>% 
  ungroup() %>% 
  arrange(-correlacion_spearman) %>% 
  print(n = Inf)


# Todas las observaciones de bd_rp ----
bd_rp %>% 
  filter(!is.na(antiguedad_primera_norma)) %>% 
  summarise(correlacion_pearson = cor(antiguedad_primera_norma, ifreg, method = "pearson"),
            correlacion_spearman = cor(antiguedad_primera_norma, ifreg, method = "spearman"),
            correlacion_kendall = cor(antiguedad_primera_norma, ifreg, method = "kendall")) 

# Por entidad, con datos de bd_rp ----
bd_rp %>% 
  filter(!is.na(antiguedad_primera_norma)) %>% 
  group_by(estado) %>% 
  summarise(correlacion_pearson = cor(antiguedad_primera_norma, ifreg, method = "pearson"),
            correlacion_spearman = cor(antiguedad_primera_norma, ifreg, method = "spearman"),
            correlacion_kendall = cor(antiguedad_primera_norma, ifreg, method = "kendall")) %>% 
  ungroup() %>% 
  arrange(-correlacion_spearman) %>% 
  print(n = Inf)



### Análisis de correlaciones entre PIB per cápita estatal, porcentaje de urbanización y porcentaje de la población de 15 años o más con al menos un año concluido de educación superior ----

# Todas las observaciones de bd_mr ----
foo <- 
  bd_mr %>% 
  select(pibe_per_capita_rescalado, por_pob_urbana, por_pob_edu_superior)


# Pearson
corrplot::corrplot(cor(foo, use = "complete.obs", method = "pearson"), type = "upper", method = "number")

# Spearman
corrplot::corrplot(cor(foo, use = "complete.obs", method = "spearman"), type = "upper", method = "number")

# Kendall
corrplot::corrplot(cor(foo, use = "complete.obs", method = "kendall"), type = "upper", method = "number")


# Todas las observaciones de bd_rp ----
faa <- 
  bd_rp %>% 
  select(pibe_per_capita_rescalado, por_pob_urbana, por_pob_edu_superior)


# Pearson
corrplot::corrplot(cor(faa, use = "complete.obs", method = "pearson"), type = "upper", method = "number")

# Spearman
corrplot::corrplot(cor(faa, use = "complete.obs", method = "spearman"), type = "upper", method = "number")

# Kendall
corrplot::corrplot(cor(faa, use = "complete.obs", method = "kendall"), type = "upper", method = "number")


### Evolución del número de curules en cada legislatura a lo largo del tiempo ----

# Gráficas no incluidas en el artículo
bd_mr %>% 
  ggplot(aes(x = año_inicio, y = total_curules, group = estado)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  facet_wrap(~ estado, ncol = 8)

bd_rp %>% 
  ggplot(aes(x = año_inicio, y = total_curules, group = estado)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  facet_wrap(~ estado, ncol = 8)


### Evolución del % de mujeres electas en cada entidad y legislatura por cada principio respecto al total de curules del tipo correspondiente ----
bd_prop_mr %>% 
  left_join(bd_prop_rp %>% 
              rename(prop_legisladoras_rp = prop_legisladoras) %>% 
              select(estado, periodo, prop_legisladoras_rp),
            by = c("estado", "periodo")) %>% 
  ggplot() +
  geom_line(aes(x = año_inicio, y = prop_legisladoras)) +
  geom_point(aes(x = año_inicio, y = prop_legisladoras)) +
  geom_line(aes(x = año_inicio, y = prop_legisladoras_rp), color = "salmon") +
  geom_point(aes(x = año_inicio, y = prop_legisladoras_rp), color = "salmon") +
  facet_wrap(~ estado, ncol = 8) +
  labs(title = "MR = negro | RP = rojo")