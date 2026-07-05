# ==============================================================================
# Proyecto: Análisis de la calidad de la vivienda de los hogares con
#           mascotas en el Perú
# Script: EDA con variables analíticas
# Autor: Eliane Caceres
# Fecha: 03-07-2026
# Objetivo: Explorar las variables analíticas creadas en el script de
#           clasificación
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
library(flextable)
library(officer)
renv::snapshot()

# Cargamos la base analítica
enaho_analitica <- read_parquet(here("datos", "procesados", "enaho_mascotas_analitica_030726.parquet"))

