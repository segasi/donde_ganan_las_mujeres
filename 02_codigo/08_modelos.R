### Cargar paquetes, definir setup y tema de gráficas ----
source("02_codigo/00_paquetes_setup_tema.R")

### Cargar, procesar y unir bases de datos ----
source("02_codigo/06_unir_bd.R")


### Modelos 1 y 5 ----

# En esta especificación:

# 1) Las observaciones están anidadas por entidad
# 2) Se calculan tanto efectos fijos como aleatorios
# 3) Se incluyen como variables explicativas en los efectos fijos i) ifreg, ii) prop_legisladoras_lag (una versión rezagada de la variable aleatoria para controlar por posible correlación temporal) y iii) alternancia 
# 4) Se incluye como variable explicativa en los efectos aleatorios i) ifreg

# Mayoría relativa ----
m1_mr <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + prop_legisladoras_lag + alternancia + (1 + ifreg | estado), 
        data = bd_mr,
        family = "binomial")

summary(m1_mr)
fixef(m1_mr) # Efectos fijos
ranef(m1_mr) # Efectos aleatorios


# Representación proporcional ----
m5_rp <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + prop_legisladoras_lag + alternancia + (1 + ifreg | estado), 
        data = bd_rp,
        family = "binomial")

summary(m5_rp)
fixef(m5_rp) # Efectos fijos
ranef(m5_rp) # Efectos aleatorios


### Modelos 2 y 6 ----

# En esta especificación:

# 1) Las observaciones están anidadas por entidad
# 2) Se calculan tanto efectos fijos como aleatorios
# 3) Se incluyen como variables explicativas en los efectos fijos i) ifreg, ii) antiguedad_primera_norma, iii) prop_legisladoras_lag y iv) alternancia 
# 4) Se incluye como variable explicativa en los efectos aleatorios i) ifreg

# Mayoría relativa ----
m2_mr <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + antiguedad_primera_norma + prop_legisladoras_lag + alternancia + (1 + ifreg | estado), 
        data = bd_mr,
        family = "binomial")

summary(m2_mr)
fixef(m2_mr) # Efectos fijos
ranef(m2_mr) # Efectos aleatorios


# Representación proporcional ----
m6_rp <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + antiguedad_primera_norma + prop_legisladoras_lag + alternancia + (1 + ifreg | estado),
        data = bd_rp,
        family = "binomial")

summary(m6_rp)
fixef(m6_rp) # Efectos fijos
ranef(m6_rp) # Efectos aleatorios



### Modelos 3 y 7 ----

# En esta especificación:

# 1) Las observaciones están anidadas por entidad
# 2) Se calculan tanto efectos fijos como aleatorios
# 3) Se incluyen como variables explicativas en los efectos fijos i) ifreg, ii) antiguedad_primera_norma, iii) pibe_per_capita_rescalado, iv) prop_legisladoras_lag y v) alternancia 
# 4) Se incluye como variable explicativa en los efectos aleatorios i) ifreg y ii) pibe_per_capita_rescalado

# Mayoría relativa ----
m3_mr <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + antiguedad_primera_norma + pibe_per_capita_rescalado + prop_legisladoras_lag + alternancia + (1 + ifreg + pibe_per_capita_rescalado | estado), 
        data = bd_mr,
        family = "binomial")

summary(m3_mr)
fixef(m3_mr) # Efectos fijos
ranef(m3_mr) # Efectos aleatorios


# Representación proporcional ----
m7_rp <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + antiguedad_primera_norma + pibe_per_capita_rescalado + prop_legisladoras_lag + alternancia + (1 + ifreg + pibe_per_capita_rescalado | estado), 
        data = bd_rp,
        family = "binomial")

summary(m7_rp)
fixef(m7_rp) # Efectos fijos
ranef(m7_rp) # Efectos aleatorios


### Modelo 4 y 8 ----

# En esta especificación:

# 1) Las observaciones están anidadas por entidad
# 2) Se calculan tanto efectos fijos como aleatorios
# 3) Se incluyen como variables explicativas en los efectos fijos i) ifreg, ii) antiguedad_primera_norma, iii) pibe_per_capita_rescalado, iv) por_pob_urbana, v) por_pob_edu_superior, vi) prop_legisladoras_lag y vii) alternancia 
# 4) Se incluye como variable explicativa en los efectos aleatorios i) ifreg, ii) pibe_per_capita_rescalado, iii) por_pob_urbana y iv) por_pob_edu_superior 



# Mayoría relativa ----
m4_mr <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + antiguedad_primera_norma + pibe_per_capita_rescalado + por_pob_urbana + por_pob_edu_superior + prop_legisladoras_lag + alternancia + (1 + ifreg + pibe_per_capita_rescalado + por_pob_urbana + por_pob_edu_superior | estado), 
      data = bd_mr,
      family = "binomial")

summary(m4_mr)
fixef(m4_mr) # Efectos fijos
ranef(m4_mr) # Efectos aleatorios


# Sin ifreg - modelo NO incluido en el artículo ----
m4_mr_a <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 +  antiguedad_primera_norma + pibe_per_capita_rescalado + por_pob_urbana + por_pob_edu_superior + prop_legisladoras_lag + alternancia + (1 + pibe_per_capita_rescalado + por_pob_urbana + por_pob_edu_superior | estado), 
              data = bd_rp,
              family = "binomial")

summary(m4_mr_a)


# Representación proporcional ----
m8_rp <- 
  glmer(cbind(num_legisladoras, total_curules) ~ 1 + ifreg + antiguedad_primera_norma + pibe_per_capita_rescalado + por_pob_urbana + por_pob_edu_superior + prop_legisladoras_lag + alternancia + (1 + ifreg + pibe_per_capita_rescalado + por_pob_urbana + por_pob_edu_superior | estado), 
        data = bd_rp,
        family = "binomial")

summary(m8_rp)
fixef(m8_rp) # Efectos fijos
ranef(m8_rp) # Efectos aleatorios


### Generar tablas ----

# Modelos para mayoría relativa 
stargazer(m1_mr, m2_mr, m3_mr, m4_mr, 
          title = "Resultados", align = TRUE, 
          type = "text", dep.var.caption = "Mayoría relativa", 
          out = "04_datos_generados/tabla_resultados_modelos_mr_20210128.txt")


# Modelos para representación proporcional
stargazer(m5_rp, m6_rp, m7_rp, m8_rp, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Representación proporcional", 
          out = "04_datos_generados/tabla_resultados_modelos_rp_20210128.txt")


### Figura 5: predicciones a partir de efectos fijos de IFREG y antigüedad norma de género----

# IFREG 

pred_ef_m2_mr_ifreg <- 
  ggpredict(m2_mr, terms = c("ifreg"), type = "fixed") %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "IFREG",
         principio_eleccion = "Mayoría Relativa")

pred_ef_m2_mr_ifreg

pred_ef_m6_rp_ifreg <- 
  ggpredict(m6_rp, terms = c("ifreg"), type = "fixed") %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "IFREG",
         principio_eleccion = "Representación proporcional")

pred_ef_m6_rp_ifreg

# Antigüedad de primera norma de género
pred_ef_m2_mr_antiguedad <- 
  ggpredict(m2_mr, terms = c("antiguedad_primera_norma [all]")) %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "Antigüedad de primera norma de género",
         principio_eleccion = "Mayoría Relativa")

pred_ef_m2_mr_antiguedad %>% print(n = Inf)

pred_ef_m6_rp_antiguedad <- 
  ggpredict(m6_rp, terms = c("antiguedad_primera_norma [all]")) %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "Antigüedad de primera norma de género",
         principio_eleccion = "Representación proporcional")

# Juntar observaciones
pred_ef <- 
  bind_rows(pred_ef_m2_mr_ifreg, 
            pred_ef_m6_rp_ifreg, 
            pred_ef_m2_mr_antiguedad, 
            pred_ef_m6_rp_antiguedad)

# Generar gráficas
g1 <- 
  pred_ef %>% 
  filter(var_explicativa == "IFREG") %>% 
  ggplot(aes(x = x, 
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ principio_eleccion) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.1, 5.1)) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.025)) +
  labs(title = str_to_upper("IFREG"),
       x = "\nValores",
       y = "Proporciones predichas\n") +
  tema +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0, size = 18, face = "italic"),
        strip.text = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing = unit(2, "lines"))


g2 <- 
  pred_ef %>% 
  filter(var_explicativa == "Antigüedad de primera norma de género") %>% 
  ggplot(aes(x = x, 
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ principio_eleccion) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0.9, 24.2),
                     breaks = c(1, seq(5, 20, 5), 24)) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.025)) +
  labs(title = "Antigüedad de la primera norma de género",
       x = "\nAños",
       y = "Proporciones predichas\n") +
  tema +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0, size = 18, face = "italic"),
        strip.text = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing = unit(2, "lines"))


plot_grid(g1, g2, ncol = 1) +
  ggsave("03_graficas/figura_5_predicciones_con_efectos_fijos.png", width = 14, height = 9, dpi = 200)


### Figura 6: Gráficas de predicciones de proporción de legisladoras de MR a partir de efectos aleatorios de IFREG usando el  ----

# Con datos del Modelo 2 ----

# Predicciones con efectos fijos
pred_ef_m2_mr_ifreg <- 
  ggpredict(m2_mr, terms = c("ifreg"), type = "fixed") %>% 
  as_tibble()
  
  
# Predicciones con efectos aleatorios
pred_ea_m2_mr_ifreg <- 
  ggpredict(m2_mr, 
            terms = c("ifreg", "estado"), 
            type = "re") %>% 
  as_tibble()


pred_ea_m2_mr_ifreg %>%
  left_join(pred_ef_m2_mr_ifreg %>% 
              as_tibble() %>% 
              select(x, predicted_fijo = predicted), by = "x") %>%   
  ggplot() +
  geom_line(aes(x = x, y = predicted, group = group)) +
  geom_line(aes(x = x, y = predicted_fijo), color = "salmon") +
  scale_y_continuous(breaks = seq(0, .40, 0.025)) +
  facet_wrap(~ group, ncol = 8) +
  labs(NULL,
       x = "\nValores",
       y = "Proporciones predichas\n") +
  tema +
  theme(panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing = unit(0.5, "lines"),
        strip.text = element_text(size = 17)) +
  ggsave("03_graficas/figura_6_predicciones_con_efectos_aleatorios_modelo_2_mr.png", width = 17, height = 11, dpi = 200)


pred_ea_m2_mr_ifreg %>%
  arrange(group, x) %>% 
  filter(x %in% c(0, 5)) %>% 
  pivot_wider(names_from = x,
              values_from = predicted) %>% 
  mutate(cambio_0_a_5 = `5` - `0`, 
         cambio_por_0_a_5 = (`5` - `0`)/`0`*100) %>% 
  arrange(-cambio_0_a_5) %>% 
  as.data.frame() 
  

# Con datos del Modelo 4 - Gráfica NO incluida en el paper ----

# Predicciones con efectos fijos
pred_ef_m4_mr_ifreg <- 
  ggpredict(m4_mr, terms = c("ifreg"), type = "fixed") %>% 
  as_tibble()


# Predicciones con efectos aleatorios
pred_ea_m4_mr_ifreg <- 
  ggpredict(m4_mr, 
            terms = c("ifreg", "estado"), 
            type = "re") %>% 
  as_tibble()


pred_ea_m4_mr_ifreg %>%
  left_join(pred_ef_m4_mr_ifreg %>% 
              as_tibble() %>% 
              select(x, predicted_fijo = predicted), by = "x") %>%   
  ggplot() +
  geom_line(aes(x = x, y = predicted, group = group)) +
  geom_line(aes(x = x, y = predicted_fijo), color = "salmon") +
  scale_y_continuous(breaks = seq(0, .40, 0.025)) +
  facet_wrap(~ group, ncol = 8) +
  labs(NULL,
       x = "\nValores",
       y = "Proporciones predichas\n") +
  tema +
  theme(panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing = unit(0.5, "lines"),
        strip.text = element_text(size = 17)) +
  ggsave("03_graficas/figura_XX_predicciones_con_efectos_aleatorios_modelo_4_mr.png", width = 17, height = 11, dpi = 200)


pred_ea_m4_mr_ifreg %>%
  arrange(group, x) %>% 
  filter(x %in% c(0, 5)) %>% 
  pivot_wider(names_from = x,
              values_from = predicted) %>% 
  mutate(cambio_0_a_5 = `5` - `0`, 
         cambio_por_0_a_5 = (`5` - `0`)/`0`*100) %>% 
  arrange(-cambio_0_a_5) %>% 
  as.data.frame() 


  