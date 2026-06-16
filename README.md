# Evaluación del Impacto de la Directiva CGR sobre Obras por Administración Directa

Este proyecto analiza el impacto de la directiva de la Contraloría General de la República (CGR) sobre la ejecución de obras por **Administración Directa (AD)**, vigente desde el 1 de junio de 2024. El análisis se centra en detectar cambios estratégicos en el comportamiento de las unidades ejecutoras, específicamente el fenómeno de *bunching* (acumulación de obras justo debajo del umbral de S/ 5 millones) y quiebres estructurales en la tendencia de uso de esta modalidad.

## Estructura del Proyecto

```text
.
├── data/
│   ├── raw/            # Datos originales de INFOBRAS/MEF (Excel)
│   └── processed/      # Tablas limpias y métricas calculadas (CSV)
├── figures/            # Gráficos generados (PNG)
├── src/
│   └── main.R          # Script principal de análisis
└── README.md           # Documentación del proyecto
```

## Metodología

El análisis implementa tres enfoques econométricos complementarios:

1.  **Análisis de Bunching**: Evaluación de la densidad de montos de inversión alrededor del umbral de S/ 5,000,000. Se estima un polinomio contrafactual para calcular el exceso de masa ($B$) y el hueco de masa ($M$).
2.  **Test de Discontinuidad de Densidad (RD)**: Implementación del estimador de Cattaneo-Jansson-Ma (`rddensity`) para evaluar formalmente si existe un salto en la distribución de montos en el umbral.
3.  **Análisis de Quiebre Estructural**:
    *   **ITS (Interrupted Time Series)**: Evaluación de cambios en el nivel y la pendiente de la proporción de obras por AD antes y después de la vigencia de la norma.
    *   **Bai-Perron & BCP**: Identificación endógena de fechas de quiebre para validar si el cambio de tendencia coincide con el anuncio o vigencia de la directiva.
4.  **Diferencia en Diferencias (DiD)**: Comparación del *share* de obras debajo del umbral entre AD y Contrata (placebo).

## Requisitos

El análisis se realiza en **R** (versión 4.1 o superior recomendada). Se requieren los siguientes paquetes:

*   `tidyverse` (Manipulación de datos y visualización)
*   `readxl` (Lectura de datos Excel)
*   `rddensity`, `rdrobust` (Tests de densidad de Cattaneo)
*   `tidyplots` (Generación de gráficos científicos)
*   `strucchange`, `bcp` (Análisis de quiebres estructurales)
*   `sandwich`, `lmtest` (Inferencia robusta HAC)
*   `lubridate` (Manejo de fechas)

## Instrucciones de Uso

1.  Asegúrese de que el archivo de datos `DataSet-Obras-Publicas 15-06-2026.xlsx` se encuentre en `data/raw/`.
2.  Ejecute el script principal: `source("src/main.R")`.
3.  Los resultados se generarán automáticamente en las carpetas `figures/` y `data/processed/`.

## Resultados Clave

El script genera una tabla resumen en consola con:
- Estimaciones de $B$ y $M$ con intervalos de confianza bootstrap al 95%.
- Estadísticos $T$ y valores $p$ de la discontinuidad de densidad.
- Estimaciones del quiebre estructural y fechas identificadas por los algoritmos.

---
**Autor**: Edison  
**Fecha de última actualización**: Junio 2026
