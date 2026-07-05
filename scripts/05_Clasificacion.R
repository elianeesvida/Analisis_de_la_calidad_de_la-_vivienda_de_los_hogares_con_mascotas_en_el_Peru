# ==============================================================================
# Proyecto: Análisis de la calidad de la vivienda de los hogares con
#           mascotas en el Perú
# Script: Clasificación
# Autor: Eliane Caceres
# Fecha: 03-07-2026
# Objetivo: Crear variables analíticas para el análisis de la calidad de
#           la vivienda de los hogares con mascotas en el Perú
# ==============================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN Y CARGA DE DATOS--------------------------------------------
# ------------------------------------------------------------------------------
library(tidyverse)
library(arrow)
library(survey)
library(srvyr)
library(here)
library(gtsummary)
library(flextable)
renv::snapshot()

# Cargamos la base explorada
enaho_explorar <- read_parquet(here("datos", "procesados", "enaho_mascotas_explorar_030726.parquet"))

# ==============================================================================
# 1. PREPARACIÓN DE VARIABLES ANALÍTICAS---------------------------------------
# ==============================================================================

enaho_analitica <- enaho_explorar %>%
  mutate(
    
    # --------------------------------------------------------------------------
    # A. Índice de Calidad de la Vivienda (ICV) ----------- ÍNDICE
    # --------------------------------------------------------------------------
    # Construcción mediante suma simple de las tres NBI.
    # Metodología: INEI (suma de necesidades básicas insatisfechas).
    # Rango: 0 (ninguna NBI) a 3 (las tres NBI).
    # Justificación: En el EDA se verificó que las tres variables presentan
    # variabilidad suficiente (nbi1=5.4%, nbi2=3.2%, nbi3=4.4% ),
    # lo que valida su inclusión en el índice.
    indice_cv = nbi1 + nbi2 + nbi3,

    # --------------------------------------------------------------------------
    # B. Categorización del Índice ----------- RECODIFICACIÓN
    # --------------------------------------------------------------------------
    # La categorización combina dos referentes metodológicos:
    # 1. El INEI, que define como pobre por NBI a todo hogar con al menos
    #    una necesidad básica insatisfecha. 
    # 2. Ponce (2006), quien propone una clasificación ordinal
    #    de la calidad de la vivienda en niveles (buena, regular, mala y muy mala).
    #    Si bien no se replica su metodología (componentes principales),
    #    se adopta su lógica de categorización para enriquecer la
    #    interpretación de los resultados.
    # En consecuencia:
    # - Buena calidad   = 0 NBI (no pobre por NBI, según INEI)
    # - Mala calidad    = 1 NBI (pobre por NBI, según INEI)
    # - Muy mala calidad = 2 o más NBI (pobre por NBI, según INEI)
    categoria_cv = case_when(
      indice_cv == 0 ~ "Buena calidad",
      indice_cv == 1 ~ "Mala calidad",
      indice_cv >= 2 ~ "Muy mala calidad",
      TRUE           ~ NA_character_
    ),
    categoria_cv = factor(categoria_cv,
                          levels = c("Buena calidad",
                                     "Mala calidad",
                                     "Muy mala calidad")),


    # --------------------------------------------------------------------------
    # C. Tipología de Tenencia de Mascotas ----------- TIPOLOGÍA
    # --------------------------------------------------------------------------
    # Clasificación MECE (Mutuamente Excluyente, Colectivamente Exhaustiva)
    # de los hogares según el tipo de mascota que tienen.
    # Justificación: En el EDA se verificó que hay suficientes casos en cada
    # grupo para que la comparación sea estadísticamente válida.
    tipologia_mascota = case_when(
      tiene_perro == TRUE  & tiene_gato == FALSE & tiene_otra_mascota == FALSE ~ "Solo perro",
      tiene_perro == FALSE & tiene_gato == TRUE  & tiene_otra_mascota == FALSE ~ "Solo gato",
      tiene_perro == FALSE & tiene_gato == FALSE & tiene_otra_mascota == TRUE  ~ "Solo otra mascota",
      tiene_perro == TRUE  & tiene_gato == TRUE  & tiene_otra_mascota == FALSE ~ "Perro y gato",
      tiene_perro == TRUE  & tiene_gato == FALSE & tiene_otra_mascota == TRUE  ~ "Perro y otra mascota",
      tiene_perro == FALSE & tiene_gato == TRUE  & tiene_otra_mascota == TRUE  ~ "Gato y otra mascota",
      tiene_perro == TRUE  & tiene_gato == TRUE  & tiene_otra_mascota == TRUE  ~ "Perro, gato y otra mascota",
      TRUE ~ NA_character_
    ),
    tipologia_mascota = factor(tipologia_mascota,
                               levels = c("Solo perro",
                                          "Solo gato",
                                          "Solo otra mascota",
                                          "Perro y gato",
                                          "Perro y otra mascota",
                                          "Gato y otra mascota",
                                          "Perro, gato y otra mascota"))
  )
gc()

# Actualizamos el diseño muestral con la base analítica
# Restringido a hogares CON mascota, que es nuestro universo de análisis
enaho_diseno_analitico <- enaho_analitica %>%
  filter(!is.na(factor_s) & tiene_mascota == TRUE) %>%
  as_survey_design(
    ids     = conglome,
    strata  = estrato,
    weights = factor_s,
    nest    = TRUE
  )

# ==============================================================================
# 2. EXPORTACIÓN DE LA BASE ANALÍTICA------------------------------------------
# ==============================================================================
write_parquet(
  enaho_analitica,
  here::here("datos", "procesados", "enaho_mascotas_analitica_030726.parquet")
)

# ==============================================================================
# 3. REPORTE DE VARIABLES CREADAS----------------------------------------------
# ==============================================================================
reporte_clasificar <- enaho_diseno_analitico %>%
  tbl_svysummary(
    include = c(
      indice_cv,
      categoria_cv,
      tipologia_mascota
    ),
    label = list(
      indice_cv         ~ "Índice de Calidad de Vivienda (0-3)",
      categoria_cv      ~ "Categoría de Calidad de Vivienda",
      tipologia_mascota ~ "Tipología de Tenencia de Mascotas"
    ),
    statistic = list(
      all_categorical() ~ "{n_unweighted} ({p}%)",
      all_continuous()  ~ "{mean} ({sd})"
    ),
    digits       = all_continuous() ~ 2,
    missing_text = "(Casos perdidos / NA)"
  ) %>%
  modify_header(label = "**Variable Construida / Recodificada**") %>%
  modify_caption("**Reporte de Variables Analíticas - Clasificación (ENAHO 2025 - Módulo 118)**") %>%
  bold_labels()

# Imprimir en el visor de RStudio
reporte_clasificar

# Exportar el reporte a HTML
reporte_clasificar %>%
  as_flex_table() %>%
  flextable::save_as_html(
    path = here::here("outputs", "CLASIFICAR_Reporte_VariablesCreadas.html")
  )


