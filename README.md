# Análisis de Diversificación de Seguros en México (ENIF 2024)

Adaptación a **R** del flujo de análisis descriptivo, multivariado, regresión lineal múltiple (OLS), diagnóstico de supuestos y modelos de conteo (Poisson y Binomial Negativa con IRR) sobre microdatos de la Encuesta Nacional de Inclusión Financiera (ENIF) 2024 del INEGI.

## Contenido del repositorio
- `scripts/01_descriptivo_pca.R`: descarga directa de ENIF 2024, unión con `TVIVIENDA`, creación de `num_seguros`, cobertura por ramo, estadísticos descriptivos, matriz de correlaciones, PCA y panel gráfico 2x2.
- `scripts/02_modelos_diagnostico.R`: ajuste OLS (`lm()`), diagnósticos gráficos y VIF, inspección de valores extremos y modelos de conteo (`glm()` y `MASS::glm.nb()`) con cálculo de IRR.
- `notebooks/Proyecto_AR_seguros.ipynb`: notebook original de referencia en Python.
- `docs/Proyecto_RL.pdf`: documento final del proyecto (*¿Qué variables influyen más significativamente en la contratación de ciertos tipos de seguros (Vida, Autos, GMM, Hogar) en México?*).

## Requisitos en R
Paquetes necesarios: `httr`, `data.table`, `dplyr`, `ggplot2`, `patchwork`, `car`, `MASS` y `broom`.
