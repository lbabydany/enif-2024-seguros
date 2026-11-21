#                      Proyecto de Análisis de Regresión 
#                       Análisis de Regresión (2026-I)

################################################################################
###
### 
###                  Universidad Nacional Autónoma de México
###                  Facultad de Estudios Superiores Acatlán
###                        Licenciatura en Actuaria
###   
################################################################################
# Paquetes necesarios:
#install.packages(c("httr", "data.table", "dplyr", "ggplot2", "patchwork", "car", "MASS", "broom"))
library(httr)
library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
library(car)       # vif()
library(MASS)      # glm.nb()
library(broom)     # tidy(), augment()
 
options(scipen = 999)

cat("=== ANALISIS DESCRIPTIVO Y MULTIVARIADO - SEGUROS ===\n")

descargar_enif_inteligente <- function(
    url = "https://www.inegi.org.mx/contenidos/programas/enif/2024/microdatos/enif_2024_bd_csv.zip",
    max_intentos = 3
) {
  # Headers completos simulando un navegador real
  ua <- user_agent("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36")
  hdrs <- add_headers(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "Accept-Language" = "es-MX,es;q=0.9,en;q=0.8",
    "Accept-Encoding" = "gzip, deflate, br",
    "DNT" = "1",
    "Connection" = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "none",
    "Cache-Control" = "max-age=0"
  )
  
  for (intento in seq_len(max_intentos)) {
    tryCatch({
      cat(" Haciendo pre-request para simular navegador...\n")
      Sys.sleep(2)
      
      # HEAD request para verificar disponibilidad del servidor
      head_resp <- HEAD(url, ua, hdrs, timeout(30), config(ssl_verifypeer = 0L))
      status_code <- status_code(head_resp)
      cat(sprintf("Estado del servidor: %d\n", status_code))
      
      if (status_code == 200L) {
        cat("Servidor responde, procediendo con descarga...\n")
        zip_path <- tempfile(fileext = ".zip")
        
        # Descarga directa a disco (evita saturar RAM con archivos grandes)
        get_resp <- GET(
          url,
          ua,
          hdrs,
          timeout(120),
          config(ssl_verifypeer = 0L),
          write_disk(zip_path, overwrite = TRUE)
        )
        stop_for_status(get_resp)
        
        tam_mb <- file.info(zip_path)$size / (1024 * 1024)
        cat(sprintf(" Descarga completada: %.1f MB\n", tam_mb))
        return(zip_path)
      } else {
        cat(sprintf(" Servidor no disponible: %d\n", status_code))
      }
    }, error = function(e) {
      cat(sprintf(" Error en descarga (intento %d/%d): %s\n", intento, max_intentos, conditionMessage(e)))
    })
    
    if (intento < max_intentos) {
      Sys.sleep(2 * intento)
    }
  }
  
  return(NULL)
}

cat(" INICIANDO WEB SCRAPING DEL INEGI...\n")
ruta_zip <- descargar_enif_inteligente()

if (!is.null(ruta_zip) && file.exists(ruta_zip)) {
  cat(" Procesando archivo ZIP descargado...\n")
  exdir_path <- tempfile("enif_2024_")
  dir.create(exdir_path)
  
  tryCatch({
    archivos <- unzip(ruta_zip, list = TRUE)$Name
    cat(" Contenido del ZIP:\n")
    for (a in archivos) {
      cat(sprintf("    %s\n", a))
    }
    
    unzip(ruta_zip, exdir = exdir_path)
    
    # CARGAR TMODULO.CSV
    cat("\n Cargando TMODULO.csv...\n")
    df <- fread(file.path(exdir_path, "TMODULO.csv"), encoding = "Latin-1")
    cat(sprintf("TMODULO cargado: %s registros, %d columnas\n",
                format(nrow(df), big.mark = ","), ncol(df)))
    
    # CARGAR TVIVIENDA.CSV
    cat(" Cargando TVIVIENDA.csv...\n")
    df_vivienda <- fread(file.path(exdir_path, "TVIVIENDA.csv"), encoding = "Latin-1")
    cat(sprintf(" TVIVIENDA cargado: %s registros, %d columnas\n",
                format(nrow(df_vivienda), big.mark = ","), ncol(df_vivienda)))
  }, error = function(e) {
    cat(sprintf(" Error procesando ZIP: %s\n", conditionMessage(e)))
    stop("Fallo al procesar el archivo ZIP de ENIF 2024.", call. = FALSE)
  })
} else {
  # ULTIMO INTENTO: método directo con download.file
  cat(" Último intento con método directo...\n")
  url_directa <- "https://www.inegi.org.mx/contenidos/programas/enif/2024/microdatos/enif_2024_bd_csv.zip"
  zip_directo <- "enif_direct.zip"
  
  tryCatch({
    download.file(
      url_directa,
      destfile = zip_directo,
      mode = "wb",
      timeout = 120,
      extra = "--no-check-certificate"
    )
    exdir_path <- tempfile("enif_direct_")
    dir.create(exdir_path)
    unzip(zip_directo, exdir = exdir_path)
    
    df <- fread(file.path(exdir_path, "TMODULO.csv"), encoding = "Latin-1")
    df_vivienda <- fread(file.path(exdir_path, "TVIVIENDA.csv"), encoding = "Latin-1")
    cat(" Descarga exitosa con método directo!\n")
  }, error = function(e) {
    cat(sprintf("Todos los métodos fallaron: %s\n", conditionMessage(e)))
    cat(" Posibles soluciones:\n")
    cat("   1. El servidor del INEGI puede estar caído temporalmente\n")
    cat("   2. Puede haber restricciones de red en tu entorno\n")
    cat("   3. Intenta ejecutar de nuevo en 5-10 minutos\n")
    stop("No fue posible descargar los microdatos de ENIF 2024.", call. = FALSE)
  })
}

cat("\n WEB SCRAPING EXITOSO!\n")
cat(" Resumen de datos cargados:\n")
cat(sprintf("   • TMODULO: %s registros\n", format(nrow(df), big.mark = ",")))
cat(sprintf("   • TVIVIENDA: %s registros\n", format(nrow(df_vivienda), big.mark = ",")))

# ===================================
# Unir con TVIVIENDA para obtener tamaño del hogar (P1_1)
viv_unicos <- unique(as.data.table(df_vivienda)[, .(LLAVEVIV, P1_1)])
df <- merge(as.data.table(df), viv_unicos, by = "LLAVEVIV", all.x = TRUE)
df[, tamano_hogar := suppressWarnings(as.numeric(P1_1))]

cat("Variables cargadas correctamente\n")

# 1. CREAR VARIABLE DE DIVERSIFICACION DE SEGUROS
cat("\n", strrep("=", 60), "\n", sep = "")
cat("1. CREACION DE VARIABLE DE DIVERSIFICACION DE SEGUROS\n")
cat(strrep("=", 60), "\n")

seguros_columns <- paste0("P8_5_", 1:8)
nombres_seguros <- c("Vida", "Gastos Medicos", "Auto", "Accidentes",
                     "Casa", "Educacion", "Afore", "Otros")

for (col in seguros_columns) {
  df[[paste0(col, "_num")]] <- as.integer(suppressWarnings(as.numeric(df[[col]])) == 1)
}

cols_num <- paste0(seguros_columns, "_num")
df[, num_seguros := rowSums(.SD, na.rm = TRUE), .SDcols = cols_num]

cat("Variable creada: num_seguros\n")
cat(sprintf("Rango observado: %d a %d seguros\n",
            min(df$num_seguros, na.rm = TRUE), max(df$num_seguros, na.rm = TRUE)))
cat("Rango teorico: 0 a 8 seguros\n")

# 2. ANALISIS DE COBERTURA POR TIPO DE SEGURO
cat("\n", strrep("=", 60), "\n", sep = "")
cat("2. COBERTURA POR TIPO DE SEGURO\n")
cat(strrep("=", 60), "\n")

resultados_cobertura <- lapply(seq_along(seguros_columns), function(i) {
  col <- seguros_columns[i]
  vals <- suppressWarnings(as.numeric(df[[col]]))
  total <- sum(!is.na(vals))
  si <- sum(vals == 1, na.rm = TRUE)
  no <- sum(vals == 2, na.rm = TRUE)
  blanco <- sum(is.na(vals))
  cobertura <- if (total > 0) (si / total) * 100 else 0
  
  data.frame(
    Tipo_Seguro = nombres_seguros[i],
    Si = si,
    No = no,
    Blanco_Secuencia = blanco,
    `Cobertura_%` = round(cobertura, 2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows() %>%
  mutate(Ranking = rank(-`Cobertura_%`, ties.method = "min")) %>%
  arrange(Ranking)

print(resultados_cobertura, row.names = FALSE)

# 3. ANALISIS DESCRIPTIVO DE VARIABLES SOCIOECONOMICAS
cat("\n", strrep("=", 60), "\n", sep = "")
cat("3. ESTADISTICOS DESCRIPTIVOS - VARIABLES SOCIOECONOMICAS\n")
cat(strrep("=", 60), "\n")

variables_socioeconomicas <- list(
  num_seguros  = "Numero de Seguros",
  EDAD_V       = "Edad",
  NIV          = "Escolaridad",
  P3_11A       = "Ingreso",
  tamano_hogar = "Tamano Hogar"
)

df_analisis <- df[, names(variables_socioeconomicas), with = FALSE] %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))

cat("Resumen estadistico de variables:\n")
resumen_df <- do.call(rbind, lapply(names(df_analisis), function(v) {
  x <- df_analisis[[v]]
  q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  data.frame(
    variable = v,
    count = sum(!is.na(x)),
    mean = round(mean(x, na.rm = TRUE), 3),
    std = round(sd(x, na.rm = TRUE), 3),
    min = round(min(x, na.rm = TRUE), 3),
    `25%` = round(q[1], 3),
    `50%` = round(q[2], 3),
    `75%` = round(q[3], 3),
    max = round(max(x, na.rm = TRUE), 3),
    check.names = FALSE
  )
}))
print(resumen_df, row.names = FALSE)

cat("\n--- ESTADISTICAS DETALLADAS ---\n")
for (var in names(variables_socioeconomicas)) {
  nombre <- variables_socioeconomicas[[var]]
  data <- na.omit(df_analisis[[var]])
  cat(sprintf("\n%s (%s):\n", nombre, var))
  cat(sprintf("  Media: %.2f\n", mean(data)))
  cat(sprintf("  Mediana: %.2f\n", median(data)))
  cat(sprintf("  Desviacion: %.2f\n", sd(data)))
  cat(sprintf("  Min: %s, Max: %s\n", min(data), max(data)))
  cat(sprintf("  No nulos: %s\n", format(length(data), big.mark = ",")))
}

# 4. DISTRIBUCION DE LA VARIABLE PRINCIPAL
cat("\n", strrep("=", 60), "\n", sep = "")
cat("4. DISTRIBUCION DE NUMERO DE SEGUROS\n")
cat(strrep("=", 60), "\n")

freq_tbl <- table(df_analisis$num_seguros, useNA = "ifany")
dist_nums <- suppressWarnings(as.numeric(names(freq_tbl)))
orden_idx <- order(dist_nums, na.last = TRUE)
freq_tbl <- freq_tbl[orden_idx]
dist_nums <- dist_nums[orden_idx]

frecuencias <- as.integer(freq_tbl)
porcentajes <- (frecuencias / nrow(df_analisis)) * 100
acumulado <- cumsum(porcentajes)

tabla_distribucion <- data.frame(
  Num_Seguros = dist_nums,
  Frecuencia = frecuencias,
  Porcentaje = round(porcentajes, 2),
  Acumulado = round(acumulado, 2)
)
print(tabla_distribucion, row.names = FALSE)

cat(sprintf("\nHogares sin seguros: %s (%.1f%%)\n",
            format(sum(df_analisis$num_seguros == 0, na.rm = TRUE), big.mark = ","),
            mean(df_analisis$num_seguros == 0, na.rm = TRUE) * 100))
cat(sprintf("Hogares con 1 seguro: %s (%.1f%%)\n",
            format(sum(df_analisis$num_seguros == 1, na.rm = TRUE), big.mark = ","),
            mean(df_analisis$num_seguros == 1, na.rm = TRUE) * 100))
cat(sprintf("Hogares con 2+ seguros: %s (%.1f%%)\n",
            format(sum(df_analisis$num_seguros >= 2, na.rm = TRUE), big.mark = ","),
            mean(df_analisis$num_seguros >= 2, na.rm = TRUE) * 100))
cat(sprintf("Hogares con 3+ seguros: %s (%.1f%%)\n",
            format(sum(df_analisis$num_seguros >= 3, na.rm = TRUE), big.mark = ","),
            mean(df_analisis$num_seguros >= 3, na.rm = TRUE) * 100))

# 5. ANALISIS MULTIVARIADO - CORRELACIONES
cat("\n", strrep("=", 60), "\n", sep = "")
cat("5. ANALISIS MULTIVARIADO - MATRIZ DE CORRELACIONES\n")
cat(strrep("=", 60), "\n")

correlaciones <- cor(df_analisis, use = "pairwise.complete.obs")
cat("Matriz de correlaciones:\n")
print(round(correlaciones, 3))

cat("\nCorrelaciones con Numero de Seguros:\n")
corr_seguros <- correlaciones[, "num_seguros"]
corr_seguros <- corr_seguros[names(corr_seguros) != "num_seguros"]
corr_seguros <- sort(corr_seguros, decreasing = TRUE)

for (var in names(corr_seguros)) {
  nombre <- variables_socioeconomicas[[var]]
  cat(sprintf("  %s: %.3f\n", nombre, corr_seguros[[var]]))
}

# 6. ANALISIS MULTIVARIADO - COMPONENTES PRINCIPALES
cat("\n", strrep("=", 60), "\n", sep = "")
cat("6. ANALISIS DE COMPONENTES PRINCIPALES (PCA)\n")
cat(strrep("=", 60), "\n")

variables_pca <- c("EDAD_V", "NIV", "P3_11A", "tamano_hogar")
df_pca <- na.omit(df_analisis[, variables_pca, drop = FALSE])

if (nrow(df_pca) > 0) {
  pca_res <- prcomp(df_pca, center = TRUE, scale. = TRUE)
  var_explicada <- (pca_res$sdev^2) / sum(pca_res$sdev^2)
  var_acumulada <- cumsum(var_explicada)
  
  cat("Varianza explicada por componente:\n")
  for (i in seq_along(var_explicada)) {
    cat(sprintf("  Componente %d: %.1f%% (Acumulado: %.1f%%)\n",
                i, var_explicada[i] * 100, var_acumulada[i] * 100))
  }
  
  cat("\nContribucion de variables a componentes:\n")
  contribuciones <- as.data.frame(round(pca_res$rotation, 3))
  colnames(contribuciones) <- paste0("PC", seq_len(ncol(contribuciones)))
  print(contribuciones)
} else {
  cat("No hay datos suficientes para PCA\n")
}

# 7. VISUALIZACIONES
cat("\n", strrep("=", 60), "\n", sep = "")
cat("7. GENERANDO VISUALIZACIONES\n")
cat(strrep("=", 60), "\n")

g_hist <- ggplot(df_analisis, aes(x = num_seguros)) +
  geom_histogram(breaks = 0:9, fill = "steelblue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = 0:8) +
  labs(title = "Distribucion de Seguros por Hogar",
       x = "Numero de Seguros", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

cob_plot_df <- resultados_cobertura %>%
  mutate(Tipo_Seguro = factor(Tipo_Seguro, levels = rev(Tipo_Seguro)))

g_cob <- ggplot(cob_plot_df, aes(x = Tipo_Seguro, y = `Cobertura_%`)) +
  geom_col(fill = "darkred", alpha = 0.7) +
  coord_flip() +
  labs(title = "Cobertura por Tipo de Seguro",
       x = NULL, y = "Porcentaje de Hogares (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

corr_long <- as.data.frame(as.table(correlaciones)) %>%
  rename(Var1 = Var1, Var2 = Var2, Corr = Freq) %>%
  mutate(
    Var1_lbl = factor(unlist(variables_socioeconomicas[as.character(Var1)]),
                      levels = unlist(variables_socioeconomicas)),
    Var2_lbl = factor(unlist(variables_socioeconomicas[as.character(Var2)]),
                      levels = rev(unlist(variables_socioeconomicas)))
  )

g_corr <- ggplot(corr_long, aes(x = Var1_lbl, y = Var2_lbl, fill = Corr)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Matriz de Correlaciones", x = NULL, y = NULL, fill = "r") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

df_boxplot <- na.omit(df_analisis) %>%
  mutate(
    grupo_edad = cut(EDAD_V,
                     breaks = c(0, 30, 45, 60, 100),
                     labels = c("<30", "30-45", "45-60", ">60"),
                     include.lowest = TRUE),
    num_seguros_fac = factor(num_seguros)
  )

g_box <- ggplot(df_boxplot, aes(x = grupo_edad, y = num_seguros)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7) +
  labs(title = "Seguros por Grupo de Edad",
       x = "Grupo de Edad", y = "Numero de Seguros") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

panel_final <- (g_hist | g_cob) / (g_corr | g_box)
print(panel_final)

# 8. RESUMEN PARA INFORME
cat("\n", strrep("=", 60), "\n", sep = "")
cat("8. RESUMEN PARA INFORME\n")
cat(strrep("=", 60), "\n")

var_mas_corr <- names(corr_seguros)[1]
nombre_mas_corr <- variables_socioeconomicas[[var_mas_corr]]
val_mas_corr <- corr_seguros[[1]]

cat(sprintf("
ANALISIS DESCRIPTIVO COMPLETADO:

DIVERSIFICACION DE SEGUROS:
- Los hogares poseen en promedio %.2f seguros
- %.1f%% de los hogares no tiene seguros
- Solo %.1f%% tiene 2 o mas seguros

COBERTURA POR TIPO:
- Seguro mas comun: %s (%s%%)
- Seguro menos comun: %s (%s%%)

RELACIONES PRINCIPALES:
- La variable que mas correlaciona con numero de seguros es: %s (r = %.3f)
",
            mean(df_analisis$num_seguros, na.rm = TRUE),
            mean(df_analisis$num_seguros == 0, na.rm = TRUE) * 100,
            mean(df_analisis$num_seguros >= 2, na.rm = TRUE) * 100,
            resultados_cobertura$Tipo_Seguro[1], resultados_cobertura$`Cobertura_%`[1],
            resultados_cobertura$Tipo_Seguro[nrow(resultados_cobertura)], resultados_cobertura$`Cobertura_%`[nrow(resultados_cobertura)],
            nombre_mas_corr, val_mas_corr))

cat("Analisis descriptivo y multivariado completado exitosamente\n")

# ============================================================
# 9. MODELO DE REGRESIÓN LINEAL MÚLTIPLE (OLS)
# ============================================================
cat("\n", strrep("=", 60), "\n", sep = "")
cat("9. MODELO DE REGRESIÓN LINEAL MÚLTIPLE\n")
cat(strrep("=", 60), "\n")

vars_modelo <- c("num_seguros", "EDAD_V", "NIV", "P3_11A", "tamano_hogar")
df_regresion <- na.omit(df_analisis[, vars_modelo, drop = FALSE])

cat(sprintf("Registros válidos para el modelo: %s\n",
            format(nrow(df_regresion), big.mark = ",")))

# En R, lm() incluye el intercepto por defecto (no requiere add_constant)
modelo <- lm(num_seguros ~ EDAD_V + NIV + P3_11A + tamano_hogar, data = df_regresion)

print(summary(modelo))

# --- INTERPRETACIÓN BÁSICA DE COEFICIENTES (BETAS) ---
cat("\n--- Interpretación de Coeficientes (Betas) ---\n")

nombres_vars <- c(
  "(Intercept)" = "Intercepto (Beta 0)",
  "EDAD_V"       = "Edad (Beta 1)",
  "NIV"          = "Escolaridad (Beta 2)",
  "P3_11A"       = "Ingreso (Beta 3)",
  "tamano_hogar" = "Tam. Hogar (Beta 4)"
)

tabla_betas <- tidy(modelo, conf.int = TRUE) %>%
  mutate(
    nombre_real = ifelse(term %in% names(nombres_vars), nombres_vars[term], term),
    sig = case_when(
      p.value < 0.01 ~ "*** (Altamente Signif.)",
      p.value < 0.05 ~ "** (Significativo)",
      p.value < 0.10 ~ "* (Poco Signif.)",
      TRUE           ~ "(No Significativo)"
    )
  )

for (i in seq_len(nrow(tabla_betas))) {
  cat(sprintf("%-20s | Coef: %10.5f | P-value: %.4f %s\n",
              tabla_betas$nombre_real[i],
              tabla_betas$estimate[i],
              tabla_betas$p.value[i],
              tabla_betas$sig[i]))
}

r2 <- summary(modelo)$r.squared
cat(sprintf("\nR-cuadrado: %.4f (El modelo explica el %.2f%% de la varianza)\n",
            r2, r2 * 100))

cat(sprintf("Media: %.15f | Varianza: %.15f\n",
            mean(df_regresion$num_seguros),
            var(df_regresion$num_seguros)))
