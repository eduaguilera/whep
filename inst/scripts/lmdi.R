# ============================================================================
# Ejemplos de uso de la función lmdi_calculation
# ============================================================================

# Cargar paquetes necesarios
library(dplyr)
library(tibble)

# Cargar la función LMDI
source("R/lmdi.r")


# ============================================================================
# EJEMPLO 1: LMDI Simple - Sin estructura de grupos
# ============================================================================
# Este ejemplo muestra un análisis básico de descomposición de energía
# sin variables de agrupación adicionales.

cat("\n=== EJEMPLO 1: Análisis Simple ===\n\n")

# Crear datos de ejemplo: consumo energético a lo largo del tiempo
# IMPORTANTE: Usar solo variables absolutas originales (no ratios pre-calculados)
datos_simples <- tibble::tribble(
  ~year, ~energy,  ~population, ~gdp,
  2000,  1000,     100,         5000,
  2005,  1150,     110,         6600,
  2010,  1300,     120,         8400,
  2015,  1400,     130,         10400,
  2020,  1450,     140,         11900
)

# Mostrar los datos
cat("Datos de entrada (solo variables absolutas):\n")
print(datos_simples)

# Calcular algunos indicadores para referencia
cat("\n\nIndicadores derivados (solo para referencia):\n")
datos_simples |>
  dplyr::mutate(
    gdp_per_capita = gdp / population,
    energy_per_capita = energy / population,
    energy_intensity = energy / gdp
  ) |>
  dplyr::select(year, gdp_per_capita, energy_per_capita, energy_intensity) |>
  print()

# Realizar descomposición LMDI usando identidad con RATIOS
# Identidad matemática: energía como producto de población y ratios
# Factores: Población, PIB per cápita (actividad), Intensidad energética
resultado_simple <- lmdi_calculation(
  data = datos_simples,
  identity = "energy:population*gdp/population*energy/gdp",
  identity_labels = c(
    "Consumo Energético Total",
    "Efecto Población",
    "Efecto PIB per cápita",
    "Efecto Intensidad Energética"
  ),
  time_var = "year",
  periods = NULL, # Genera periodos consecutivos automáticamente
  output_format = "clean",
  verbose = TRUE
)


# Mostrar resultados
cat("\n\nResultados de la descomposición:\n")
print(resultado_simple)

# Filtrar y mostrar solo los factores (sin el target)
cat("\n\nContribuciones de cada factor:\n")
resultado_simple |>
  dplyr::filter(component_type == "factor") |>
  dplyr::select(
    period,
    factor_label,
    additive,
    additive_annual,
    multiplicative
  ) |>
  print()

# Visualizar la contribución acumulada
cat("\n\nContribución acumulada por periodo:\n")
resultado_simple |>
  dplyr::filter(component_type == "factor") |>
  dplyr::group_by(period) |>
  dplyr::summarise(
    total_change = sum(additive, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print()


# ============================================================================
# EJEMPLO 2: LMDI con Múltiples Estructuras (Grupos)
# ============================================================================
# Este ejemplo muestra un análisis más complejo con variables de agrupación
# (país y sector), permitiendo comparar descomposiciones entre diferentes
# combinaciones.

cat("\n\n=== EJEMPLO 2: Análisis Multi-estructura ===\n\n")

# Crear datos de ejemplo más complejos: múltiples países y sectores
# IMPORTANTE: Solo variables absolutas originales
datos_complejos <- tibble::tribble(
  ~year, ~country,  ~sector,      ~emissions, ~energy, ~total_emissions,
  # España - Industria
  2000,  "Spain",   "Industry",   500,        250,     1300,
  2010,  "Spain",   "Industry",   550,        300,     1550,
  2020,  "Spain",   "Industry",   520,        350,     1700,
  # España - Transporte
  2000,  "Spain",   "Transport",  300,        150,     1300,
  2010,  "Spain",   "Transport",  350,        175,     1550,
  2020,  "Spain",   "Transport",  360,        200,     1700,
  # España - Otros
  2000,  "Spain",   "Others",     500,        250,     1300,
  2010,  "Spain",   "Others",     650,        325,     1550,
  2020,  "Spain",   "Others",     820,        410,     1700,
  # Francia - Industria
  2000,  "France",  "Industry",   600,        300,     1500,
  2010,  "France",  "Industry",   650,        325,     1800,
  2020,  "France",  "Industry",   620,        350,     2000,
  # Francia - Transporte
  2000,  "France",  "Transport",  400,        200,     1500,
  2010,  "France",  "Transport",  440,        220,     1800,
  2020,  "France",  "Transport",  450,        250,     2000,
  # Francia - Otros
  2000,  "France",  "Others",     500,        250,     1500,
  2010,  "France",  "Others",     710,        355,     1800,
  2020,  "France",  "Others",     930,        465,     2000,
  # Alemania - Industria
  2000,  "Germany", "Industry",   800,        400,     2000,
  2010,  "Germany", "Industry",   850,        425,     2300,
  2020,  "Germany", "Industry",   800,        450,     2500,
  # Alemania - Transporte
  2000,  "Germany", "Transport",  500,        250,     2000,
  2010,  "Germany", "Transport",  560,        280,     2300,
  2020,  "Germany", "Transport",  570,        320,     2500,
  # Alemania - Otros
  2000,  "Germany", "Others",     700,        350,     2000,
  2010,  "Germany", "Others",     890,        445,     2300,
  2020,  "Germany", "Others",     1130,       565,     2500
)

# Mostrar estructura de los datos
cat("Estructura de los datos (variables absolutas):\n")
print(datos_complejos)

cat("\n\nResumen de países y sectores:\n")
datos_complejos |>
  dplyr::distinct(country, sector) |>
  dplyr::arrange(country, sector) |>
  print()

# Calcular indicadores derivados para referencia
cat("\n\nIndicadores derivados por país y año:\n")
datos_complejos |>
  dplyr::group_by(country, year) |>
  dplyr::summarise(
    total_emissions = first(total_emissions),
    total_energy = sum(energy, na.rm = TRUE),
    emission_intensity = total_emissions / total_energy,
    .groups = "drop"
  ) |>
  print()

# Realizar descomposición LMDI con análisis por país y sector
# Identidad matemática: emisiones por sector como producto de energía y ratio
# Los selectores [sector] indican que se filtra por el sector correspondiente
resultado_complejo <- lmdi_calculation(
  data = datos_complejos,
  identity = "emissions[sector]:energy[sector]*emissions[sector]/energy[sector]",
  identity_labels = c(
    "Emisiones por Sector",
    "Efecto Escala (Energía)",
    "Efecto Intensidad de Emisión"
  ),
  time_var = "year",
  analysis_by = c("country", "sector"), # Agrupar por país y sector
  periods = c(2000, 2010, 2020), # Periodos específicos
  output_format = "clean",
  verbose = FALSE
)

# Mostrar resultados completos
cat("\n\nResultados de la descomposición por país y sector:\n")
print(resultado_complejo)

# Análisis por país
cat("\n\nContribución total por país:\n")
resultado_complejo |>
  dplyr::filter(component_type == "factor") |>
  dplyr::group_by(country, factor_label) |>
  dplyr::summarise(
    total_contribution = sum(additive, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = factor_label,
    values_from = total_contribution
  ) |>
  print()

# Análisis por sector
cat("\n\nContribución total por sector:\n")
resultado_complejo |>
  dplyr::filter(component_type == "factor") |>
  dplyr::group_by(sector, factor_label) |>
  dplyr::summarise(
    total_contribution = sum(additive, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = factor_label,
    values_from = total_contribution
  ) |>
  print()

# Análisis detallado: cambio entre 2000-2010 vs 2010-2020
cat("\n\nComparación de periodos por país:\n")
resultado_complejo |>
  dplyr::filter(component_type == "factor") |>
  dplyr::group_by(country, period, factor_label) |>
  dplyr::summarise(
    contribution = sum(additive, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = period,
    values_from = contribution
  ) |>
  dplyr::arrange(country, factor_label) |>
  print()

# Análisis con media móvil (suavizado)
cat("\n\n=== EJEMPLO 2b: Con media móvil (rolling mean) ===\n")

# Crear datos con más años para el suavizado
# Solo variables absolutas: emissions y energy
datos_extendidos <- tibble::tribble(
  ~year, ~country, ~sector,    ~emissions, ~energy,
  2000,  "Spain",  "Industry", 500,        250,
  2001,  "Spain",  "Industry", 510,        260,
  2002,  "Spain",  "Industry", 520,        270,
  2003,  "Spain",  "Industry", 525,        275,
  2004,  "Spain",  "Industry", 530,        280,
  2005,  "Spain",  "Industry", 535,        285,
  2006,  "Spain",  "Industry", 540,        290,
  2007,  "Spain",  "Industry", 542,        295,
  2008,  "Spain",  "Industry", 538,        300,
  2009,  "Spain",  "Industry", 530,        305,
  2010,  "Spain",  "Industry", 520,        310
)

resultado_suavizado <- lmdi_calculation(
  data = datos_extendidos,
  identity = "emissions:energy*emissions/energy",
  identity_labels = c(
    "Emisiones Totales",
    "Efecto Escala (Energía)",
    "Efecto Factor de Emisión"
  ),
  time_var = "year",
  analysis_by = c("country", "sector"),
  rolling_mean = 3, # Media móvil de 3 años
  output_format = "clean",
  verbose = FALSE
)

cat("\n\nResultados con suavizado (media móvil 3 años):\n")
print(resultado_suavizado)


# ============================================================================
# EJEMPLO 3: LMDI con identidad compleja usando selectores
# ============================================================================

cat("\n\n=== EJEMPLO 3: Identidad con selectores ===\n\n")

# Datos con estructura jerárquica: total y por tipo de combustible
# Variables absolutas: fuel_energy (por tipo), total_energy, gdp
datos_selectores <- tibble::tribble(
  ~year, ~country, ~fuel_type,   ~fuel_energy, ~total_energy, ~gdp,
  2000,  "Spain",  "coal",       400,          1000,          5000,
  2000,  "Spain",  "gas",        300,          1000,          5000,
  2000,  "Spain",  "renewables", 300,          1000,          5000,
  2010,  "Spain",  "coal",       350,          1200,          7000,
  2010,  "Spain",  "gas",        400,          1200,          7000,
  2010,  "Spain",  "renewables", 450,          1200,          7000,
  2020,  "Spain",  "coal",       300,          1400,          9000,
  2020,  "Spain",  "gas",        450,          1400,          9000,
  2020,  "Spain",  "renewables", 650,          1400,          9000
)

cat("Datos con estructura jerárquica (por tipo de combustible):\n")
print(datos_selectores)

cat("\n\nIndicadores derivados:\n")
datos_selectores |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    share_of_total = fuel_energy / total_energy * 100
  ) |>
  dplyr::select(year, fuel_type, fuel_energy, share_of_total) |>
  print()

# Usar selectores en la identidad para filtrar grupos específicos
# Identidad matemática: energía por combustible descompuesta en escala, intensidad y estructura
# Descompone en: Efecto escala (PIB), Efecto intensidad energética, Efecto estructura (share)
resultado_selectores <- lmdi_calculation(
  data = datos_selectores,
  identity = "fuel_energy[fuel_type]:gdp*total_energy/gdp*fuel_energy[fuel_type]/total_energy",
  identity_labels = c(
    "Energía por Combustible",
    "Efecto Escala (PIB)",
    "Efecto Intensidad Energética",
    "Efecto Estructura (Share)"
  ),
  time_var = "year",
  verbose = FALSE
)

cat("\n\nResultados con selectores (descomposición por tipo de combustible):\n")
print(resultado_selectores)

# Análisis de la evolución de la estructura energética
cat("\n\nCambio en la estructura energética por tipo:\n")
resultado_selectores |>
  dplyr::filter(
    component_type == "factor",
    factor_label == "Efecto Estructura (Share)"
  ) |>
  dplyr::select(period, additive) |>
  print()


# ============================================================================
# Resumen de opciones y casos de uso
# ============================================================================

cat("\n\n=== RESUMEN Y GUÍA DE USO ===\n\n")
cat(
  "
═══════════════════════════════════════════════════════════════════════════
REGLA FUNDAMENTAL: USAR VARIABLES ABSOLUTAS ORIGINALES
═══════════════════════════════════════════════════════════════════════════

La función LMDI trabaja con VARIABLES ABSOLUTAS, NO con ratios pre-calculados.
Los ratios se especifican en la identidad y se calculan automáticamente.

✓ CORRECTO:
  identity = 'energy:population*gdp/population*energy/gdp'
  
✗ INCORRECTO:
  identity = 'energy:population*gdp_per_capita*energy_intensity'

═══════════════════════════════════════════════════════════════════════════
EJEMPLOS INCLUIDOS:
═══════════════════════════════════════════════════════════════════════════

1. EJEMPLO SIMPLE (análisis global):
   - Datos: energy, population, gdp (variables absolutas)
   - Identity: energy:population*gdp/population*energy/gdp
   - Sin agrupación, análisis de tendencia general

2. EJEMPLO MULTI-ESTRUCTURA (por grupos):
   - Datos: emissions, energy, total_emissions por sector y país
   - Identity: emissions[sector]:energy[sector]*emissions[sector]/energy[sector]
   - analysis_by = c('country', 'sector')
   - Los selectores [sector] filtran automáticamente

3. EJEMPLO CON SELECTORES JERÁRQUICOS:
   - Datos: fuel_energy, total_energy, gdp por tipo de combustible
   - Identity: fuel_energy[fuel_type]:gdp*total_energy/gdp*fuel_energy[fuel_type]/total_energy
   - Descomposición: Escala + Intensidad + Estructura (share)

═══════════════════════════════════════════════════════════════════════════
SINTAXIS DE IDENTIDADES:
═══════════════════════════════════════════════════════════════════════════

Formato:  'target:factor1*factor2*factor3'

Ejemplos:
  Simple:      'energy:population*energy/population'
  Con ratio:   'energy:population*gdp/population*energy/gdp'
  Selectores:  'emissions[sector]:activity[sector]*emissions[sector]/activity[sector]'
  Complejo:    'fuel[type]:gdp*total/gdp*fuel[type]/total'

═══════════════════════════════════════════════════════════════════════════
PARÁMETROS PRINCIPALES:
═══════════════════════════════════════════════════════════════════════════

data:            Data frame con variables absolutas
identity:        String 'target:factor1*factor2*...' con ratios
identity_labels: Vector de etiquetas (opcional)
time_var:        Nombre de variable temporal (default: 'year')
analysis_by:     Vector de variables de agrupación
periods:         NULL (consecutivos) o vector/data.frame de periodos
rolling_mean:    Número >= 2 para media móvil (opcional)
output_format:   'clean' o 'total'
verbose:         TRUE/FALSE para mensajes

═══════════════════════════════════════════════════════════════════════════
DOCUMENTACIÓN:
?lmdi_calculation
═══════════════════════════════════════════════════════════════════════════
"
)

cat("\n=== Fin de los ejemplos ===\n")
