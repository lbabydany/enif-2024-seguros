# Determinantes de la Diversificación de Seguros en México (ENIF 2024)

Adaptación a **R** de un proyecto de análisis estadístico y econométrico sobre la tenencia de seguros en los hogares mexicanos a partir de los microdatos de la **Encuesta Nacional de Inclusión Financiera (ENIF) 2024** del INEGI.

## Objetivo
Evaluar cómo variables sociodemográficas y económicas —edad (`EDAD_V`), nivel de escolaridad (`NIV`), ingreso (`P3_11A`) y tamaño del hogar (`tamano_hogar` / `P1_1`)— se relacionan con el número de seguros contratados por hogar (`num_seguros`, conteo de 0 a 8 ramos: Vida, Gastos Médicos, Auto, Accidentes, Casa, Educación, Afore y Otros), contrastando la especificación lineal clásica (OLS) con modelos de conteo (**Poisson** y **Binomial Negativa** con *Incidence Rate Ratios*, IRR).

## Estructura del repositorio

```text
enif-2024-seguros-r/
├── .gitignore
├── README.md
├── docs/
│   └── Proyecto_RL.pdf
├── notebooks/
│   └── Proyecto_AR_seguros.ipynb
└── scripts/
    ├── 01_descriptivo_pca.R
    └── 02_modelos_diagnostico.R
