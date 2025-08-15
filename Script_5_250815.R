# Título trabajo: Distribución espacial de la población de Mobula mobular en el Mediterráneo occidental y su relación con variables ambientales.
# Autor: Josep Alba Campins
# Contacto: josep.alba.campins@gmail.com
# Fecha y versión: 15/08/2025, versión 5.

# 0. CREACIÓN TRANSECTOS REGULARES A PARTIR DE DATOS DE VUELO
# Imput: Documento excel con las siguientes columnas: Dia / Año / ID_T / hora0 / horaf / Lat0 / Lon0 / Latf / Lonf
# Columnas opcionales: tiempo de vuelo (tv), Millas náuticas calculadas con excel para comparar resultado R / Velocidad (nm/tv)
# Output: Dataframe con transectos regulares de 15 millas náuticas (nm) con los avistamientos y individuos asociados
# Descripción: 
    # Se calcula la longitud de cada vuelo y se calcula cuantos transectos de 15nm contiene y la longitud del transecto sobrante.
    # Si divide los vuelos en mil puntos para extraer las coordendas y horas iniciales y finales de cada transecto. 
    # Se distribuye el transecto sobrante al intervalo con menos observaciones (menor pérdida de información).
    # Se asginan las coordenadas y horas iniciales y finales a cada transecto


# 0.1 Paquetes utilizados

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(hms)
library(geosphere)
library(purrr)
library(data.table)
library (sf)
library(rnaturalearth)
library(openxlsx)
library(gridExtra)
library(grid)

# 0.2 Creación dataframe y formato variables

ruta <- "C:/Users/Usuario/Desktop/TFG/Datos/Vuelos_ICCAT.xlsx"

vuelo <- read_excel(ruta, sheet = "Transectos")

obs <- read_excel(ruta, sheet = "Avistamientos")

vuelo <- vuelo %>% 
  mutate(Dia = as_date(Dia), 
         hora0 = as_hms(hora0), 
         horaf = as_hms(horaf), 
         across(c(Lat0, Lon0, Latf, Lonf), as.numeric))

obs <- obs %>% 
  mutate(Dia = as_date(Dia), 
         Hora = as_hms(Hora), 
         across(c(Lat, Long), as.numeric))

# 0.3	Generación transectos iniciales y preasignación avistamientos

vuelo <- vuelo %>% 
  rowwise() %>% 
  mutate(
    nm_exact = distHaversine(c(Lon0, Lat0), c(Lonf, Latf)) * 0.000539957,
    n_full = floor(nm_exact / 15),
    resto_nm = nm_exact - n_full * 15
  ) %>% 
  ungroup()

gen_subs <- function(row, seg_len) {
  start <- c(row$Lon0, row$Lat0)
  end <- c(row$Lonf, row$Latf)
  
  prop_fin <- cumsum(seg_len) / row$nm_exact
  prop_ini <- c(0, head(prop_fin, -1))
  
  gc_line <- geosphere::gcIntermediate(start, end, n = 1000, addStartEnd = TRUE)
  s <- seq(0, 1, length.out = nrow(gc_line))
  
  interp <- function(p, col) {
    approx(s, gc_line[, col], xout = p)$y
  }
  
  tot_sec <- as.numeric(row$horaf - row$hora0, units = "secs")
  
  t_interp <- function(p) {
    h0 <- as.numeric(row$hora0, units = "secs")
    as_hms(h0 + round(tot_sec * p))
  }
  
  result <- data.frame(
    ID_T = row$ID_T,
    Dia = row$Dia,
    sub_id = paste0(row$ID_T, "_", seq_along(seg_len)),
    seg_nm = seg_len,
    Lon_ini = interp(prop_ini, 1),
    Lat_ini = interp(prop_ini, 2),
    Lon_fin = interp(prop_fin, 1),
    Lat_fin = interp(prop_fin, 2),
    Hora_ini = t_interp(prop_ini),
    Hora_fin = t_interp(prop_fin)
  )
  
  return(result)
}

subs_init <- rbindlist(
  lapply(seq_len(nrow(vuelo)), function(i) {
    row <- vuelo[i,]
    seg_len <- c(rep(15, row$n_full), if (row$resto_nm > 0) row$resto_nm)
    gen_subs(row, seg_len)
  })
)

obs_ini <- obs %>% 
  inner_join(subs_init %>% select(sub_id, Dia, Hora_ini, Hora_fin),
             by = "Dia") %>% 
  filter(Hora >= Hora_ini, Hora <= Hora_fin) %>% 
  group_by(sub_id) %>% 
  summarise(n_obs = n())

subs_init <- subs_init %>% 
  left_join(obs_ini, by = "sub_id") %>% 
  mutate(n_obs = replace_na(n_obs, 0))

insert_tbl <- tryCatch({
  required_cols <- c("ID_T", "seg_nm", "n_obs")
  if (!all(required_cols %in% names(subs_init))) {
    stop("Faltan columnas necesarias en subs_init: ", 
         paste(setdiff(required_cols, names(subs_init)), collapse = ", "))
  }
  
  if (!all(c("ID_T", "resto_nm", "n_full") %in% names(vuelo))) {
    stop("Faltan columnas necesarias en vuelo: ", 
         paste(setdiff(c("ID_T", "resto_nm", "n_full"), names(vuelo)), collapse = ", "))
  }
  
  combined_data <- subs_init %>% 
    select(any_of(c("ID_T", "seg_nm", "n_obs"))) %>%
    left_join(vuelo %>% select(any_of(c("ID_T", "resto_nm", "n_full"))), by = "ID_T")
  
  combined_data %>% 
    group_by(ID_T) %>% 
    group_modify(~ {
      current_group <- .x
      current_resto <- current_group$resto_nm[1]
      
      insertion_point <- if (!is.na(current_resto) && current_resto > 0) {
        seg_15 <- current_group %>% filter(.data$seg_nm == 15)
        if (nrow(seg_15) > 0) {
          which.min(seg_15$n_obs)
        } else {
          1
        }
      } else {
        NA_integer_
      }
      
      tibble(
        resto_nm = current_resto,
        n_full = current_group$n_full[1],
        idx_ins = insertion_point
      )
    }) %>% 
    ungroup()
}, error = function(e) {
  message("Error al procesar los datos: ", e$message)
  return(NULL)
})

if (!is.null(insert_tbl)) {
  print("Procesamiento completado con éxito:")
  print(head(insert_tbl))
} else {
  print("Hubo un error en el procesamiento. Verifica los mensajes anteriores.")
}

# 0.4 Creación transectos definitivos

subs_final <- rbindlist(
  lapply(seq_len(nrow(vuelo)), function(i) {
    row <- vuelo[i,]
    ins <- insert_tbl$idx_ins[insert_tbl$ID_T == row$ID_T]
    seg_len <- rep(15, row$n_full)
    
    if (row$resto_nm > 0 && !is.na(ins)) {
      seg_len <- append(seg_len, row$resto_nm, after = ins - 1)
    }
    
    gen_subs(row, seg_len)
  })
)

obs_fin <- obs %>% 
  inner_join(subs_final %>% select(sub_id, Dia, Hora_ini, Hora_fin),
             by = "Dia") %>% 
  filter(Hora >= Hora_ini, Hora <= Hora_fin) %>% 
  group_by(sub_id) %>% 
  summarise(
    n_obs = n(),
    ind_sum = sum(Ind., na.rm = TRUE)
  )

transect <- subs_final %>% 
  left_join(obs_fin, by = "sub_id") %>% 
  mutate(across(c(n_obs, ind_sum), ~replace_na(.x, 0))) %>% 
  filter(seg_nm == 15) %>% 
  mutate(
    Hora_avg = as_hms((as.numeric(Hora_ini) + as.numeric(Hora_fin)) / 2),
    Lat_avg = (Lat_ini + Lat_fin) / 2,
    Lon_avg = (Lon_ini + Lon_fin) / 2
  )

study_bbox <- st_bbox(c(xmin = 0.01, xmax = 4.22, 
                        ymin = 37.60, ymax = 41.13), crs = 4326)

coastline <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(4326) %>%
  st_crop(study_bbox)

transect_sf <- st_as_sf(transect, coords = c("Lon_avg", "Lat_avg"), crs = 4326)

transect_marino <- transect_sf %>%
  filter(lengths(st_intersects(., coastline)) == 0)

print(paste("Puntos originales:", nrow(transect)))

print(paste("Puntos marinos:", nrow(transect_marino)))

transect_final <- transect_marino %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# 1. REPRESENTACIÓN PUNTO MEDIO TRANSECTOS SOBRE ZONA DE ESTUDIO
# Imput: Datos de línea de costa y perfil batimétrico de la zona de estudio (obtenidos del Instituto Español de Oceanografía -IEO)
# Outpot: 
    # Mapa de la zona de estudio con la línea de costa de alta resolución.
    # Puntos medios de cada transecto representados y diferencias por presencia negativa (roja) y positiva (verde). Radio circumferencia verde proporcional al nº de inidividuos.

# 1.1 Paquetes utilizados

library(sf)
library(ggplot2)
library(dplyr)
library(scales)

# 1.2 Cargar shapefiles (línea de costa y batimetría)

coastline <- st_read("C:/Users/Usuario/Desktop/TFG/Datos/Batimetria/BAT MALLORCA/country") %>%
  st_transform(4326)

bati <- st_read("C:/Users/Usuario/Desktop/TFG/Datos/Batimetria/BAT MALLORCA/BATI_MALLORCA_GEBCO") %>% st_set_crs(4326) %>% st_transform(4326)

# 1.3 Definir zona de estudio y radio círculos

minx <- 0.01; maxx <- 4.22

miny <- 37.60; maxy <- 41.13

base_size <- 1

min_size <- 1

max_size <- 15

# 1.4 Creación mapa con ggplot

transect_mapa <- transect_final %>%
  mutate(
    obs_status = ifelse(n_obs > 0, "Con avistamientos", "Sin avistamientos"),
    punto_size = ifelse(n_obs > 0, sqrt(ind_sum) * (max_size / sqrt(124)), base_size),
    color_intensity = ifelse(n_obs > 0, rescale(log1p(ind_sum), to = c(0.4, 0.9)), 0.6)
  )

mapa <- ggplot() +
  geom_sf(data = bati, color = alpha("darkblue", 0.2), linewidth = 0.3) +
  geom_sf(data = coastline, fill = "lightgray", color = "black") +
  geom_point(data = transect_mapa,
             aes(x = lon, y = lat, color = obs_status),
             size = base_size, alpha = 0.8) +
  geom_point(data = filter(transect_mapa, n_obs > 0),
             aes(x = lon, y = lat, 
                 size = punto_size, alpha = color_intensity),
             color = "darkgreen", fill = "green3", shape = 21, stroke = 0.5) +
  scale_color_manual(values = c("Con avistamientos" = "green4", 
                                "Sin avistamientos" = "red3")) +
  scale_size_continuous(
    range = c(min_size, max_size),
    breaks = sqrt(c(1, 5, 20, 50, 100)) * (max_size / sqrt(124)),
    labels = c("1", "5", "20", "50", "100+"),
    name = "Número de individuos"
  ) +
  scale_alpha_identity() +
  coord_sf(xlim = c(0.01, 4.22), ylim = c(37.60, 41.13), expand = FALSE) +
  labs(title = "Distribución de Avistamientos Marinos",
       subtitle = "El área de los círculos verdes es proporcional al número de individuos observados",
       x = "Longitud", y = "Latitud") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.background = element_rect(fill = alpha("lightblue", 0.2)),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  )

print(mapa)

ggsave ("C:/Users/Usuario/Desktop/TFG/Resultados/mapa_avistamientos.png", plot = mapa, width = 12, height = 9, dpi = 300, bg = "white")

# 2. ASOCIACIÓN VARIABLES AMBIENTALES A TRANSECTOS
# Imput: Datos de clorofila extraídos de Copernicus (formato .nc) y datos batimétricos obtenidos de EMODnet (formato .csv)
# Output: Transectos con datos de concentración de clorofila y profundidad asociados.

# 2.1 Paquetes utilizados

library(terra)
library (FNN)
library(openxlsx)

# 2.2 Definir ruta (ubicación de los archivos de clorofila y batimetria)

ruta_nc <- "C:/Users/Usuario/Desktop/TFG/Datos/chl_nc"

anios <- c(2013, 2015, 2017, 2018, 2019, 2022, 2023, 2024)

sufijos <- substr(as.character(anios), 3, 4)

nombres_archivos <- paste0("chl_", sufijos, ".nc")

rutas_archivos <- file.path(ruta_nc, nombres_archivos)

batimetria <- read.csv("C:\\Users\\Usuario\\Desktop\\TFG\\Datos\\Batimetria\\Mean depth in multi colour (no land).csv", stringsAsFactors = FALSE)

# 2.3 Extraer concentración de clorofila y añadir columna a transect_final

extraer_clorofila_por_anio <- function(archivo_nc, datos_transecto) {
  r <- rast(archivo_nc)
  fechas_raster <- as.Date(time(r))
  
  datos_filtrados <- subset(datos_transecto, Dia %in% fechas_raster)
  if (nrow(datos_filtrados) == 0) return(NULL)
  
  clorofila <- mapply(function(fecha, lon, lat) {
    idx <- which(fechas_raster == fecha)
    if (length(idx) == 1) {
      extract(r[[idx]], cbind(lon, lat), method = "bilinear")[[1]]
    } else {
      NA
    }
  }, datos_filtrados$Dia, datos_filtrados$lon, datos_filtrados$lat)
  
  datos_filtrados$clorofila <- clorofila
  return(datos_filtrados)
}

resultados <- list()

for (i in seq_along(rutas_archivos)) {
  cat("Procesando año", anios[i], "...\n")
  
  archivo <- rutas_archivos[i]
  anio <- anios[i]
  
  datos_anio <- subset(transect_final, format(Dia, "%Y") == as.character(anio))
  resultado_anio <- extraer_clorofila_por_anio(archivo, datos_anio)
  
  if (!is.null(resultado_anio)) {
    resultados[[as.character(anio)]] <- resultado_anio
  }
}

transect_clorofila <- do.call(rbind, resultados)

transect_final$clorofila <- NA

for (anio in names(resultados)) {
  df_res <- resultados[[anio]]
  
  idx <- with(transect_final, paste(ID_T, Dia))
  idx_res <- with(df_res, paste(ID_T, Dia))
  
  match_idx <- match(idx, idx_res)
  
  transect_final$clorofila[!is.na(match_idx)] <- df_res$clorofila[match_idx[!is.na(match_idx)]]
}

colnames(transect_final)[colnames(transect_final) == "clorofila"] <- "chl"

# 2.4 Extraer datos de batimetría

batimetria$latitude <- as.numeric(batimetria$latitude)

batimetria$longitude <- as.numeric(batimetria$longitude)

batimetria$elevation <- as.numeric(batimetria$elevation)

batimetria_clean <- batimetria[ 
  !is.na(batimetria$latitude) & 
    !is.na(batimetria$longitude) & 
    !is.na(batimetria$elevation), 
]

batimetria_clean <- batimetria_clean[, c("longitude", "latitude", "elevation")]

coords_transect <- as.matrix(transect_final[, c("lon", "lat")])

coords_batimetria <- as.matrix(batimetria_clean[, c("longitude", "latitude")])

nn <- get.knnx(coords_batimetria, coords_transect, k = 1)

transect_final$depth <- batimetria_clean$elevation[nn$nn.index]

sum(is.na(transect_final$depth))

# 2.5 Eliminar filas con valores faltantes de clorofila / profundidad (NaN)

transect_final <- transect_final[complete.cases(transect_final$chl, transect_final$depth), ]

# 3. RESUMEN DE LOS DATOS DISPONIBLES
# Output: 
    # Tabla resumen de los datos de los transectos: nº de transectos por año, millas cubiertas, observaciones y individuos por año, % transectos con observaciones y media ponderada de individuos por observacion
    # Gráficas de distribución valores clorofila / profundidad (observar si hay valores anómalos)

# 3.1 Paquetes utilizados

library(data.table)
library(gt)
library(webshot2)
library(lubridate)
library(ggplot2)

# 3.2 Creación tabla resumen

dt <- copy(transect_final)

dt[, Año := year(Dia)]

calcular_media_eem <- function(ind_sum, n_obs) {
  datos <- data.table(ind_sum, n_obs)
  datos <- datos[n_obs > 0]
  media <- sum(datos$ind_sum) / sum(datos$n_obs)
  ratio <- datos$ind_sum / datos$n_obs
  eem <- sd(ratio) / sqrt(length(ratio))
  sprintf("%.2f ± %.2f", media, eem)
}

resumen <- dt[, .(
  `nº transectos` = .N,
  `nm` = 15 * .N,
  `nº obv` = sum(n_obs, na.rm = TRUE),
  `% obv` = 100 * sum(n_obs > 0) / .N,
  `sum ind` = sum(ind_sum, na.rm = TRUE),
  `Media pond. ind/obs` = calcular_media_eem(ind_sum, n_obs)
), by = Año]

total <- dt[, .(
  Año = "Total",
  `nº transectos` = .N,
  `nm` = 15 * .N,
  `nº obv` = sum(n_obs, na.rm = TRUE),
  `% obv` = 100 * sum(n_obs > 0) / .N,
  `sum ind` = sum(ind_sum, na.rm = TRUE),
  `Media pond. ind/obs` = calcular_media_eem(ind_sum, n_obs)
)]

tabla_final <- rbind(resumen, total, fill = TRUE)

tabla_gt <- gt(tabla_final) %>%
  fmt_number(columns = c(`nº transectos`, `nm`, `nº obv`, `sum ind`), decimals = 0) %>%
  fmt_number(columns = `% obv`, decimals = 1) %>%
  fmt_percent(columns = `% obv`, scale_values = FALSE, decimals = 1) %>%
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(4)
  )

gtsave(
  tabla_gt,
  filename = "C:/Users/Usuario/Desktop/TFG/Resultados/tabla_transectos.png"
)

# 3.3 Buscar valores anómalos en columnas chl y depht

ggplot(transect_final) +
  geom_boxplot(aes(y = chl)) +
  ggtitle("Distribución de Clorofila (chl)")

ggplot(transect_final) +
  geom_boxplot(aes(y = depth)) +
  ggtitle("Distribución de Profundidad (depth)")

# 4. MODELOS GAM
# Imput: Para mapa probabilidad datos de línea de costa y perfil batimétrico de la zona de estudio (obtenidos del IEO)
# Output: 
    # Relación probabilidad presencia respecto las variables estudiadas y representación
    # Validación modelo
    # Mapa de probabilidad calculada según el modelo
    # Valores de cada variable donde la probabilidad es máxima

# 4.1 Paquetes utilizados

library(mgcv)
library(ggplot2) 
library(readxl)    
library(dplyr)
library(pROC)
library(DHARMa)
library(mgcViz)
library(sf)
library(rnaturalearth)
library(ggplot2)

# 4.2 Añadir columna binaria (presencia y ausencia) y comprobación formato variables

transect_final$presencia <- ifelse(transect_final$ind_sum > 0, 1, 0)

transect_final$hora_posix <- as.POSIXct(transect_final$Hora_avg, format = "%H:%M:%S", tz = "UTC")

transect_final$hora <- as.numeric(format(transect_final$hora_posix, "%H")) +
  as.numeric(format(transect_final$hora_posix, "%M")) / 60 +
  as.numeric(format(transect_final$hora_posix, "%S")) / 3600

transect_final$hora <- transect_final$hora %% 24

# 4.3 Creación modelos GAM iniciales

t2=transect_final%>%filter(chl<0.5)

gam <- gam(presencia ~ s(hora, bs = "cc", k = 7) + s(chl, k = 5) + s(depth, k = 5), family = binomial(link = "logit"),data = t2, method = "REML")

summary (gam)

plot (gam)

gam_pos <- gam( presencia ~ s(hora, bs = "cc", k = 7) + s(chl, k = 5) + s(depth, k = 5) + s(lon, lat, k = 40), family = binomial(link = "logit"), data = t2, method = "REML")

summary(gam_pos)

plot (gam_pos)

# 4.4 Ajuste valores k y comparación modelos con AIC

gam.check(gam)

gam.check(gam_pos)

gam_2 <- gam(presencia ~ s(hora, bs = "cc", k = 10) + s(chl, k = 5) + s(depth, k = 3), family = binomial(link = "logit"),data = t2, method = "REML")

gam.check(gam_2)
    
AIC (gam)

AIC (gam_2)

gam_pos_2 <- gam( presencia ~ s(hora, bs = "cc", k = 10) + s(chl, k = 5) + s(depth, k = 3) + s(lon, lat, k = 30), family = binomial(link = "logit"), data = t2, method = "REML")

gam.check(gam_pos_2)
      
AIC (gam_pos)

AIC (gam_pos_2)

summary (gam_2)

summary (gam_pos_2)

plot (gam_2)

plot (gam_pos_2)

# 4.5 GAM con valores k definitivos y visualización efectos

gam <- gam(presencia ~ s(hora, bs = "cc", k = 10) + s(chl, k = 5) + s(depth, k = 3), family = binomial(link = "logit"),data = t2, method = "REML")

gam_pos <- gam( presencia ~ s(hora, bs = "cc", k = 10) + s(chl, k = 5) + s(depth, k = 3) + s(lon, lat, k = 30), family = binomial(link = "logit"), data = t2, method = "REML")

labsize <- 1.2

par(mfrow = c(2, 2))

plot(gam, select = 1, shade = TRUE, scale = 0, rug = TRUE, 
     cex.lab = labsize, cex.axis = labsize, 
     xlab = "Hora del día", ylab = "Efecto parcial", 
     main = "Efecto de la hora (GAM)")

plot(gam, select = 2, shade = TRUE, scale = 0, rug = TRUE,
     cex.lab = labsize, cex.axis = labsize,
     xlab = "Clorofila (chl)", ylab = "Efecto parcial",
     main = "Efecto de clorofila")

plot(gam, select = 3, shade = TRUE, scale = 0, rug = TRUE,
     cex.lab = labsize, cex.axis = labsize,
     xlab = "Profundidad (m)", ylab = "Efecto parcial",
     main = "Efecto de profundidad")

labsize <- 1.2

par(mfrow = c(2, 2))

plot(gam_pos, select = 1, shade = TRUE, scale = 0, rug = TRUE,
     cex.lab = labsize, cex.axis = labsize,
     xlab = "Hora del día", ylab = "Efecto parcial",
     main = "Efecto de la hora (GAM pos)")

plot(gam_pos, select = 2, shade = TRUE, scale = 0, rug = TRUE,
     cex.lab = labsize, cex.axis = labsize,
     xlab = "Clorofila (chl)", ylab = "Efecto parcial",
     main = "Efecto de clorofila")

plot(gam_pos, select = 3, shade = TRUE, scale = 0, rug = TRUE,
     cex.lab = labsize, cex.axis = labsize,
     xlab = "Profundidad (m)", ylab = "Efecto parcial",
     main = "Efecto de profundidad")

# 4.6 Validación modelos con pROC y DHARMa:

t2$fitted_gam <- predict(gam, type = 'response')

t2$fitted_gam_pos <- predict(gam_pos, type = 'response')

roc_gam <- roc(presencia ~ fitted_gam, data = t2)

roc_gam_pos <- roc(presencia ~ fitted_gam_pos, data = t2)

plot(roc_gam, col = "blue", main = "Comparación de modelos")

plot(roc_gam_pos, col = "red", add = TRUE)

legend("bottomright", legend = c("GAM sin posición", "GAM con posición"), 
       col = c("blue", "red"), lwd = 2)

cat("AUC GAM sin posición:", auc(roc_gam), "\n")

cat("AUC GAM con posición:", auc(roc_gam_pos), "\n")

sim_res_gam <- simulateResiduals(gam)

sim_res_gam_pos <- simulateResiduals(gam_pos)
      
testDispersion(sim_res_gam)

par(mfrow = c(1, 2))

testUniformity(sim_res_gam)

testZeroInflation(sim_res_gam)



testDispersion(sim_res_gam_pos)

par(mfrow = c(1, 2))

testUniformity(sim_res_gam_pos)

testZeroInflation(sim_res_gam_pos)

# 4.7 Mapa probabilidad (mediante gam_pos)

coastline <- st_read("C:/Users/Usuario/Desktop/TFG/Datos/Batimetria/BAT MALLORCA/country") %>% st_transform(4326) %>% st_make_valid()

bati <- st_read("C:/Users/Usuario/Desktop/TFG/Datos/Batimetria/BAT MALLORCA/BATI_MALLORCA_GEBCO") %>% st_set_crs(4326) %>% st_transform(4326) %>% st_make_valid()

zona_estudio_coords <- matrix(c(0.01, 37.60, 4.22, 37.60, 4.22, 41.13, 0.01, 41.13, 0.01, 37.60), ncol = 2, byrow = TRUE)

zona_estudio_polygon <- st_polygon(list(zona_estudio_coords))

zona_estudio_sf <- st_sfc(zona_estudio_polygon, crs = 4326)

resolucion_grados <- 0.01

grid_lon <- seq(from = st_bbox(zona_estudio_sf)$xmin, to = st_bbox(zona_estudio_sf)$xmax, by = resolucion_grados)

grid_lat <- seq(from = st_bbox(zona_estudio_sf)$ymin, to = st_bbox(zona_estudio_sf)$ymax, by = resolucion_grados)

pred_grid <- expand.grid(lon = grid_lon, lat = grid_lat)

pred_grid_sf <- st_as_sf(pred_grid, coords = c("lon", "lat"), crs = 4326)

coastline_union <- st_union(coastline)

pred_grid_marine_sf <- pred_grid_sf[!lengths(st_intersects(pred_grid_sf, coastline_union)), ]

pred_data_marine <- data.frame(st_coordinates(pred_grid_marine_sf))

colnames(pred_data_marine) <- c("lon", "lat")

mean_hora <- mean(t2$hora, na.rm = TRUE)

mean_chl <- mean(t2$chl, na.rm = TRUE)

mean_depth <- mean(t2$depth, na.rm = TRUE)

pred_data_marine$hora <- mean_hora

pred_data_marine$chl <- mean_chl

pred_data_marine$depth <- mean_depth

pred_data_marine$prediccion_prob <- predict(gam_pos, newdata = pred_data_marine, type = "response")

prob_min <- min(pred_data_marine$prediccion_prob, na.rm = TRUE)

prob_max <- max(pred_data_marine$prediccion_prob, na.rm = TRUE)

legend_breaks <- round(seq(prob_min, prob_max, length.out = 5), 2)

mapa_probabilidad <- ggplot() + 
  geom_sf(
    data = bati, 
    color = alpha("darkblue", 0.4), 
    linewidth = 0.4, 
    fill = NA
  ) + 
  geom_tile(
    data = pred_data_marine, 
    aes(x = lon, y = lat, fill = prediccion_prob), 
    alpha = 0.7
  ) + 
  scale_fill_gradientn(
    colors = c("navy", "dodgerblue", "cyan", "yellow", "orange", "red", "darkred"),
    name = "Probabilidad de presencia",
    limits = c(0, max(pred_data_marine$prediccion_prob, na.rm = TRUE)),
    values = scales::rescale(c(0, 0.15, 0.3, 0.5, 0.7, 0.85, 1))
  ) + 
  geom_sf(
    data = coastline, 
    fill = "lightgray", 
    color = "black", 
    linewidth = 0.4
  ) + 
  coord_sf(
    xlim = c(0.01, 4.22), 
    ylim = c(37.60, 41.13), 
    expand = FALSE
  ) + 
  labs(
    title = "Mapa de Probabilidad de Presencia (gam_pos)",
    subtitle = paste(
      "Variables fijadas a promedios:", 
      sprintf("Hora = %.1f, Clorofila = %.2f, Profundidad = %.1f", mean_hora, mean_chl, mean_depth),
      sprintf("\nProbabilidad máxima en mar: %.3f", prob_max)
    ),
    x = "Longitud", 
    y = "Latitud"
  ) + 
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(mapa_probabilidad)

# 4.8 Valores de probabilidad máximos para cada variable

mean_hora <- mean(t2$hora, na.rm = TRUE)

mean_chl <- mean(t2$chl, na.rm = TRUE)

mean_depth <- mean(t2$depth, na.rm = TRUE)

hora_range <- seq(min(t2$hora, na.rm = TRUE), 
                  max(t2$hora, na.rm = TRUE), 
                  length.out = 100)

chl_range <- seq(min(t2$chl, na.rm = TRUE), 
                 max(t2$chl, na.rm = TRUE), 
                 length.out = 100)

depth_range <- seq(min(t2$depth, na.rm = TRUE), 
                   max(t2$depth, na.rm = TRUE), 
                   length.out = 100)

optim_grid_gam <- expand.grid(
  hora = hora_range,
  chl = chl_range,
  depth = depth_range
)

optim_grid_gam$prob <- predict(gam, newdata = optim_grid_gam, type = "response")

best_conditions_gam <- optim_grid_gam[which.max(optim_grid_gam$prob), ]

cat("--- Condiciones óptimas (GAM sin posición) ---\n")

cat("  Hora óptima:          ", round(best_conditions_gam$hora, 2), " horas\n")

cat("  Clorofila óptima:     ", round(best_conditions_gam$chl, 4), " mg/m³\n")

cat("  Profundidad óptima:   ", round(best_conditions_gam$depth, 1), " m\n")

cat("  Probabilidad máxima:  ", round(best_conditions_gam$prob, 4), "\n")

fixed_chl <- mean(t2$chl, na.rm = TRUE)

fixed_depth <- mean(t2$depth, na.rm = TRUE)

lon_range <- seq(min(t2$lon, na.rm = TRUE), 
                 max(t2$lon, na.rm = TRUE), 
                 length.out = 100)

lat_range <- seq(min(t2$lat, na.rm = TRUE), 
                 max(t2$lat, na.rm = TRUE), 
                 length.out = 100)

hora_range <- seq(min(t2$hora, na.rm = TRUE), 
                  max(t2$hora, na.rm = TRUE), 
                  length.out = 100)

optim_grid_gam_pos <- expand.grid(lon = lon_range, lat = lat_range, hora = hora_range, chl = fixed_chl, depth = fixed_depth)

optim_grid_gam_pos$prob <- predict(gam_pos, newdata = optim_grid_gam_pos, type = "response")

best_conditions_gam_pos <- optim_grid_gam_pos[which.max(optim_grid_gam_pos$prob), ]

cat("\n--- Condiciones óptimas (GAM con posición) ---\n")

cat("  Longitud óptima:      ", round(best_conditions_gam_pos$lon, 4), "°\n")

cat("  Latitud óptima:       ", round(best_conditions_gam_pos$lat, 4), "°\n")

cat("  Hora óptima:          ", round(best_conditions_gam_pos$hora, 2), " horas\n")

cat("  Probabilidad máxima:  ", round(best_conditions_gam_pos$prob, 4), "\n")





