# RAP CUSUM – Manual de uso (WS5.WP1.2)

Este documento explica, paso a paso:

1. Cómo **configurar los parámetros** del RAP CUSUM antes de correrlo.
2. Cómo **ejecutar** el RAP CUSUM en R / RStudio.

Se asume que ya tenés la estructura del repositorio:

```text
WP1.2_CUSUM_RAP/
  R/
    01_functions_cusum.R
    02_functions_plot.R
    03_functions_io.R
  input/
  output/
  main.R
  README.md
  HELP_EN.md
  HELP_ES.md
```

y que el archivo de entrada agregado desde API-Pop es un CSV ubicado en `input/`.

---

## 1. Configuración – Cómo ajustar los parámetros antes de correr

Todos los parámetros que el usuario puede cambiar están **al inicio del archivo `main.R`**.

Abrí `main.R` en RStudio y buscá este bloque:

```r
## -----------------------------------------------------------
## 1) User-configurable parameters
## -----------------------------------------------------------

# Geographic analysis level
# Options: "country", "province", "department", "censal_fraction"
location_level <- "province"

# CUSUM threshold h (to be calibrated)
cusum_h <- 3

# Baseline length (in weeks):
#   - detection period = last baseline_length_weeks
#   - baseline period  = all weeks before that
baseline_length_weeks <- 52

# Input path: API-Pop aggregated CSV
input_path <- "input/rumor_counts_by_week_unit.csv"
```

Podés editar los valores de la derecha (`"province"`, `3`, `52`, etc.) según las necesidades del análisis.

---

### 1.1 Parámetro: `location_level`

```r
location_level <- "province"
```

Este parámetro controla el **nivel geoespacial de análisis**. El RAP va a agregar los recuentos semanales a este nivel antes de aplicar el CUSUM.

Valores permitidos:

- `"country"`  
  Todos los datos agregados al nivel de **país**.
- `"province"`  
  Datos agregados por `country + province`.
- `"department"`  
  Datos agregados por `country + province + department`.
- `"censal_fraction"`  
  Datos agregados por `country + province + department + censal_fraction`.

Ejemplos:

- `location_level <- "province"` → el CUSUM se corre **por provincia**.
- `location_level <- "censal_fraction"` → el CUSUM se corre **por fracción censal** (nivel más detallado).

Asegurate de que tu CSV de entrada tenga las columnas de ubicación
(`country`, `province`, `department`, `censal_fraction`). Si los nombres
difieren, se pueden adaptar dentro de `prepare_weekly_data_geo()`.

Para la dimensión temporal, el RAP acepta **dos formatos alternativos**:

- Columnas separadas `year` y `week` (por ejemplo `year = 2024`, `week = 5`), o  
- Una única columna combinada `week` con valores en formato `"año-semana"`
  (por ejemplo `"2024-05"` o `"2024-5"`).

Internamente, la función auxiliar `prepare_weekly_data_geo()` interpreta esta
información y construye las variables `year`, `week`, `epi_date` y
`time_index` utilizadas por el análisis CUSUM. La correspondencia entre los
nombres de las columnas de entrada y las variables internas se configura a
través de los argumentos `col_year`, `col_week` y `col_yearweek` de
`prepare_weekly_data_geo()`.

---

### 1.2 Parámetro: `cusum_h`

```r
cusum_h <- 3
```

- `cusum_h` es el **umbral de decisión** del CUSUM.
- Cuando la estadística CUSUM `S_t` para una unidad analítica alcanza o supera `h`, el RAP marca una **alarma** para esa semana.

Este valor se debería **calibrar** según:

- El nivel de falsas alarmas que se considera aceptable.
- La sensibilidad deseada a cambios en la frecuencia de rumores.

Para pruebas y desarrollo se sugiere comenzar con:

- `cusum_h <- 3` o `cusum_h <- 4`

y luego ajustar en función de estudios de simulación o criterio experto.

---

### 1.3 Parámetro: `baseline_length_weeks`

```r
baseline_length_weeks <- 52
```

Este parámetro define cómo el RAP separa la serie temporal en:

- **Período de baseline**: utilizado para estimar la media esperada (modelo de referencia).
- **Período de detección**: donde se aplica el CUSUM.

Para cada unidad analítica:

- Sea `T` el máximo `time_index` (última semana disponible).
- Período de detección = **últimas `baseline_length_weeks` semanas**:  
  semanas con `time_index > T - baseline_length_weeks`
- Período de baseline  = **todas las semanas anteriores**:  
  semanas con `time_index <= T - baseline_length_weeks`

Ejemplo:

- Si `baseline_length_weeks <- 52` y hay 200 semanas de datos:
  - Semanas 1–148 → baseline (para ajustar el modelo Poisson).
  - Semanas 149–200 → detección (CUSUM).

Si cambiás a `baseline_length_weeks <- 26`:

- Semanas 1–174 → baseline.
- Semanas 175–200 → detección.

Podés aumentar la longitud del baseline si contás con series históricas más largas.

---

### 1.4 Parámetro: `input_path`

```r
input_path <- "input/rumor_counts_by_week_unit.csv"
```

Define **de dónde lee el RAP el archivo CSV de entrada**.

- Por defecto, asume un archivo llamado `rumor_counts_by_week_unit.csv` dentro de la carpeta `input/`.
- Podés cambiar el nombre si fuera necesario, por ejemplo:

```r
input_path <- "input/salida_api_pop_2025.csv"
```

El archivo debe incluir las columnas de ubicación geoespacial
(`country`, `province`, `department`, `censal_fraction`), la columna de
recuento semanal (por ejemplo `n_cases`) y la información temporal en alguno
de los dos formatos soportados:

- Columnas separadas `year` y `week`, o  
- Una única columna combinada `week` en formato `"año-semana"`.

---

## 2. Cómo correr el RAP – Paso a paso en RStudio

### 2.1 Abrir el proyecto en RStudio

1. Abrí RStudio.
2. Menú **File → Open Project…**
3. Navegá hasta la carpeta `WP1.2_CUSUM_RAP/` y abrí el archivo `.Rproj` (si existe) o seleccioná la carpeta como proyecto.

Verificá que ves las subcarpetas `R/`, `input/`, `output/` en el panel **Files**.

---

### 2.2 Instalar los paquetes necesarios (solo la primera vez)

En la **Consola**, ejecutá:

```r
install.packages(c(
  "dplyr", "ggplot2", "readr",
  "lubridate", "ISOweek", "tidyr", "rlang"
))
```

Solo hace falta instalar estos paquetes **una vez por computadora**.

---

### 2.3 Colocar el CSV de entrada en `input/`

1. Copiá tu archivo CSV agregado (salida de API-Pop) dentro de la carpeta `input/`.
2. Verificá que el nombre del archivo y las columnas coincidan con lo configurado en:
   - `input_path` en `main.R`, y
   - `prepare_weekly_data_geo()` en `R/03_functions_io.R`.

Ejemplo de ruta:  
`input/rumor_counts_by_week_unit.csv`

---

### 2.4 Configurar los parámetros en `main.R`

1. En el panel **Files**, hacé clic en `main.R` para abrirlo.
2. Al inicio del archivo, revisá y ajustá:

   ```r
   location_level <- "province"
   cusum_h <- 3
   baseline_length_weeks <- 52
   input_path <- "input/rumor_counts_by_week_unit.csv"
   ```

3. Guardá los cambios (**Ctrl+S** o **Cmd+S**).

---

### 2.5 Ejecutar el RAP

Con `main.R` abierto, corré todo el script:

- Opción A: **Ctrl + Shift + Enter** (o Cmd + Shift + Enter en Mac).
- Opción B: botón **Source** en la esquina superior derecha del editor.

R va a:

1. Cargar las funciones (`source("R/…")`).
2. Leer el CSV de entrada desde `input_path`.
3. Preparar la serie semanal y la agregación geográfica al nivel indicado por `location_level`.
4. Ajustar los modelos de baseline y calcular el CUSUM para cada unidad analítica.
5. Guardar las salidas en la carpeta `output/`.

Si aparece un error, leé el mensaje en la **Consola**. Los problemas más habituales son:

- Paquetes de R faltantes → instalarlos.
- `input_path` incorrecto → corregir nombre o ubicación del archivo.
- Columnas faltantes o con nombres distintos en el CSV → adaptar los nombres o la función `prepare_weekly_data_geo()`.

---

### 2.6 Revisar las salidas

Después de correr `main.R`, mirá dentro de la carpeta `output/`. Deberías encontrar:

- Un archivo CSV con los resultados del CUSUM, por ejemplo:  
  `cusum_results_province.csv`  
  (el sufijo coincide con el `location_level` que elegiste).

- Gráficos de ejemplo:
  - `example_province_series.png`  
    → serie observada vs esperada + alarmas para una unidad de ejemplo.
  - `example_province_cusum.png`  
    → proceso CUSUM para esa misma unidad.
  - `cusum_alarms_overview_province.png`  
    → “heatmap” de alarmas por unidad y semana.

Podés abrir los PNG con cualquier visor de imágenes o directamente desde RStudio.

---

## 3. Contacto / soporte

Si encontrás errores que no podés resolver, es útil compartir:

- El mensaje de error completo que aparece en la consola de R.
- Una breve descripción del archivo de entrada (número de filas, columnas).
- Los valores que usaste para `location_level`, `cusum_h`, `baseline_length_weeks`.

Esto ayuda al equipo de desarrollo a diagnosticar y corregir el problema.
