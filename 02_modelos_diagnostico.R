# ============================================================
# REGRESIÓN LINEAL (OLS), DIAGNÓSTICO Y CONTEO (ENIF 2024)
# ============================================================
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

# ------------------------------------------------------------
# 0. CARGA / PREPARACIÓN AUTOCONTENIDA DE df_analisis
# ------------------------------------------------------------
if (!exists("df_analisis")) {
  cat("Cargando y preparando microdatos ENIF 2024...\n")
  ruta_mod_local <- "data/TMODULO.csv"
  ruta_viv_local <- "data/TVIVIENDA.csv"
  
  if (file.exists(ruta_mod_local) && file.exists(ruta_viv_local)) {
    df <- fread(ruta_mod_local, encoding = "Latin-1")
    df_vivienda <- fread(ruta_viv_local, encoding = "Latin-1")
  } else {
    url_enif <- "https://www.inegi.org.mx/contenidos/programas/enif/2024/microdatos/enif_2024_bd_csv.zip"
    zip_path <- tempfile(fileext = ".zip")
    exdir_path <- tempfile("enif_2024_")
    dir.create(exdir_path)
    
    ua <- user_agent("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36")
    resp <- GET(url_enif, ua, timeout(120), config(ssl_verifypeer = 0L), write_disk(zip_path, overwrite = TRUE))
    stop_for_status(resp)
    unzip(zip_path, exdir = exdir_path)
    
    df <- fread(file.path(exdir_path, "TMODULO.csv"), encoding = "Latin-1")
    df_vivienda <- fread(file.path(exdir_path, "TVIVIENDA.csv"), encoding = "Latin-1")
  }
  
  viv_unicos <- unique(as.data.table(df_vivienda)[, .(LLAVEVIV, P1_1)])
  df <- merge(as.data.table(df), viv_unicos, by = "LLAVEVIV", all.x = TRUE)
  df[, tamano_hogar := suppressWarnings(as.numeric(P1_1))]
  
  seguros_columns <- paste0("P8_5_", 1:8)
  for (col in seguros_columns) {
    df[[paste0(col, "_num")]] <- as.integer(suppressWarnings(as.numeric(df[[col]])) == 1)
  }
  df[, num_seguros := rowSums(.SD, na.rm = TRUE), .SDcols = paste0(seguros_columns, "_num")]
  
  df_analisis <- df[, c("num_seguros", "EDAD_V", "NIV", "P3_11A", "tamano_hogar"), with = FALSE] %>%
    as.data.frame() %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
}

# ------------------------------------------------------------
# 1. PREPARACIÓN Y AJUSTE DEL MODELO OLS (MCO)
# ------------------------------------------------------------
vars_modelo <- c("num_seguros", "EDAD_V", "NIV", "P3_11A", "tamano_hogar")
df_regresion <- na.omit(df_analisis[, vars_modelo, drop = FALSE])

cat(sprintf("Registros válidos para el modelo: %s\n",
            format(nrow(df_regresion), big.mark = ",")))

# Aquí se crea el objeto "modelo"
modelo <- lm(num_seguros ~ EDAD_V + NIV + P3_11A + tamano_hogar, data = df_regresion)
print(summary(modelo))

# ------------------------------------------------------------
# 2. DIAGNÓSTICO DEL MODELO OLS (GRÁFICOS + VIF)
# ------------------------------------------------------------
old_par <- par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 1))
plot(modelo, which = 1:4)
par(old_par)

vif_tbl <- data.frame(
  Features = names(vif(modelo)),
  `VIF Factor` = round(as.numeric(vif(modelo)), 2),
  check.names = FALSE
) %>% arrange(`VIF Factor`)

cat("\n--- Tabla VIF ---\n")
print(vif_tbl, row.names = FALSE)

# ------------------------------------------------------------
# 3. INSPECCIÓN DE PUNTOS EXTREMOS Y LIMPIEZA (COMO EN EL REPORTE)
# ------------------------------------------------------------
aug_reg <- augment(modelo) %>%
  mutate(
    fila_regresion = as.integer(.rownames),
    EDAD_V = df_regresion$EDAD_V,
    NIV = df_regresion$NIV,
    P3_11A = df_regresion$P3_11A,
    tamano_hogar = df_regresion$tamano_hogar
  )

cat("\n--- Top 10 por Apalancamiento (.hat) ---\n")
aug_reg %>%
  arrange(desc(.hat)) %>%
  dplyr::select(fila_regresion, num_seguros, .fitted, .std.resid, .hat, EDAD_V, NIV, P3_11A, tamano_hogar) %>%
  head(10) %>%
  print(row.names = FALSE)

# Limpieza de códigos especiales de INEGI (NIV = 99 y P3_11A = 99888)
df_regresion_limpio <- df_regresion %>%
  filter(NIV < 99, P3_11A < 99888, P3_11A >= 0)

# ------------------------------------------------------------
# 4. MODELO POISSON Y PRUEBA DE SOBREDISPERSIÓN
# ------------------------------------------------------------
modelo_poisson <- glm(
  num_seguros ~ EDAD_V + NIV + P3_11A + tamano_hogar,
  data = df_regresion_limpio,
  family = poisson(link = "log")
)
print(summary(modelo_poisson))

phi <- sum(residuals(modelo_poisson, type = "pearson")^2) / modelo_poisson$df.residual
cat(sprintf("\nValor de Dispersión (Phi): %.4f\n", phi))

# ------------------------------------------------------------
# 5. MODELO BINOMIAL NEGATIVA E IRR
# ------------------------------------------------------------
res_nb <- glm.nb(
  num_seguros ~ EDAD_V + NIV + P3_11A + tamano_hogar,
  data = df_regresion_limpio
)
print(summary(res_nb))

df_resultados <- tidy(res_nb, conf.int = TRUE) %>%
  rename(Coef = estimate, `P-value` = p.value, CI_Bajo = conf.low, CI_Alto = conf.high) %>%
  mutate(IRR = exp(Coef))

cat("\n--- Coeficientes e IRR (Binomial Negativa) ---\n")
print(df_resultados, n = Inf)

