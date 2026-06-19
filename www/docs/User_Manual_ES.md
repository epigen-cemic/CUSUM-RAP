# Manual de Usuario

**CUSUM RAP Tool**  
Análisis de alerta temprana para recuentos semanales de casos  
Revisión de documentación: junio de 2026  
Idioma: Español

---

# 1. Propósito y alcance analítico

La herramienta CUSUM RAP aplica un procedimiento de suma acumulada unilateral superior a recuentos semanales de casos. Está diseñada para señalar aumentos sostenidos por encima de una línea de base esperada para cada unidad de análisis seleccionada.

> **Interpretación.** Una alarma CUSUM es una señal de alerta temprana, no un diagnóstico, una conclusión causal ni la confirmación de un brote. Antes de actuar, revise la calidad de los datos, cambios de notificación, denominadores, contexto local y otras evidencias de vigilancia.

- El análisis se realiza por separado para cada unidad geográfica seleccionada.
- El flujo actual se basa en recuentos. La población puede estar presente, pero no es necesaria para el cálculo CUSUM estándar.
- El período de detección controla la ventana semanal reciente; las reglas de cobertura pueden exigir un historial subyacente más largo.


# 2. Datos de entrada

Cargue uno o más archivos CSV compatibles con API-POP. Se admiten archivos delimitados por coma o punto y coma. Cada fila debe representar una unidad geográfica en una semana epidemiológica.

| Columna interna | Requerida | Significado | Alias aceptados o ejemplos |
| --- | --- | --- | --- |
| year | Sí | Año epidemiológico. | year, anio, año, epiyear |
| week | Sí | Semana epidemiológica, normalmente 1-53. | week, semana, epiweek, se |
| country | Recomendado | País o unidad nacional. | Argentina |
| level1 | Según el nivel elegido | Primer nivel administrativo. | province, region, Level1 |
| level2 | Según el nivel elegido | Segundo nivel administrativo. | department, district, LAD, Level2 |
| level3 / level4 | Según el nivel elegido | Niveles administrativos más detallados. | fraction, MSOA, Level3 |
| n_cases | Sí | Casos semanales observados o recuento de eventos. | n_cases, cases, case_count, count, n |
| population | Opcional | No es necesaria para el CUSUM actual basado en recuentos. | population, pop, denominator |

## Varios archivos cargados

Todos los archivos nuevos quedan activos de forma predeterminada. Use Manage files para incluir o excluir archivos, eliminar los inactivos o limpiar la sesión.

Cuando hay más de un archivo activo, la herramienta solicita cómo resolver registros repetidos de ubicación/semana:

| Opción | Efecto |
| --- | --- |
| Keep Newest File Information | Conserva el registro del archivo activo más reciente. |
| Keep Oldest File Information | Conserva el registro del archivo activo más antiguo. |
| Add Together (Sum) | Suma los recuentos superpuestos. Úselo solo cuando sean datos complementarios y no exportaciones duplicadas. |

> **Seguridad de los datos.** La gestión de archivos y la resolución de solapamientos afectan únicamente la sesión de análisis en memoria. Los archivos originales no se modifican.


# 3. Configurar el análisis

1. Cargue el archivo o los archivos CSV.
2. Seleccione un país cuando config.json habilite más de uno.
3. Elija Geographic Level.
4. Seleccione ubicaciones específicas si corresponde. Deje el campo vacío para incluir todas.
5. Configure Detection Period, el método de frecuencia esperada, ARL0, riesgo relativo y el umbral h.
6. Seleccione Run Analysis.

| Control | Comportamiento actual |
| --- | --- |
| Geographic Level | Define el nivel administrativo usado para construir unidades de análisis independientes. |
| Locations | Limita CUSUM a las unidades elegidas. Sin selección se incluyen todas las unidades disponibles. |
| Detection Period (Weeks) | Rango de interfaz 4-260; valor predeterminado 52. La validación actual exige al menos el mayor valor entre este período y 52 semanas preparadas. |
| Frecuencia esperada automática | Calcula un recuento semanal esperado por separado para cada unidad mediante el flujo actual de modelo Poisson. |
| Frecuencia esperada manual (mu0) | Usa un recuento semanal esperado fijo. Ingrese casos esperados por unidad y por semana, no una tasa de prevalencia. |
| Target ARL0 | Longitud media de corrida deseada bajo condición estable; se usa para mostrar un h recomendado. |
| Relative Risk (RR) | Aumento que se desea detectar. Se usa con el valor esperado para calcular k. |
| Calculated k | Valor de referencia calculado por la aplicación a partir de RR y el recuento esperado. |
| h Threshold | Umbral de decisión. Un valor mayor suele generar menos alarmas y una detección más lenta. |

> **Validación de cobertura.** Las combinaciones ubicación/semana ausentes pueden completarse con cero casos en los datos preparados, pero la herramienta revisa primero la cobertura observada y se detiene si la serie es demasiado corta o depende excesivamente de semanas inferidas.


# 4. Revisar los resultados

## Overview

El mapa de calor resume las alarmas por ubicación y semana reciente. Permite identificar dónde y cuándo el CUSUM alcanzó el umbral.

## Detailed View

Seleccione una ubicación mediante los selectores jerárquicos. La tarjeta de resumen muestra la selección administrativa, nivel, semanas analizadas, rango de fechas, población cuando existe, presencia de alarmas y alarma más reciente.

- Gráfico de serie observada: compara valores semanales observados con la línea esperada.
- Gráfico del proceso CUSUM: muestra la estadística acumulada y el umbral.
- Download Bar Plot y Download Trends exportan los gráficos seleccionados cuando hay resultados.

## Analysis Results

La tabla Unit-level reference muestra los casos semanales esperados y tasas esperadas una vez por unidad. Weekly results contiene la serie, estadística CUSUM, estado de alarma y parámetros. Use Search by para filtrar una columna.

## Prepared Data

Muestra el conjunto limpio, agregado, con solapamientos resueltos y series completadas que se utilizó realmente. Incluye evaluación de cobertura, registro de cambios, resumen, filas agregadas, tabla filtrable y descarga CSV.

> **Configuración visual.** En Help puede aumentar el tamaño de fuente y cambiar los decimales mostrados. Estas opciones no redondean los CSV descargados.


# 5. Guía de interpretación

Para cada semana de detección, la herramienta estandariza la diferencia entre observado y esperado, resta k y acumula evidencia positiva. La estadística nunca baja de cero. Se marca una alarma cuando alcanza o supera h; la aplicación actual reinicia la acumulación después de una alarma, conservando el pico de esa semana.

| Patrón | Interpretación sugerida |
| --- | --- |
| Valores observados repetidamente superiores a lo esperado | El CUSUM tiende a acumularse y puede alcanzar h. |
| Valores cercanos o inferiores a lo esperado | El CUSUM suele mantenerse bajo o volver a cero. |
| Una semana alta aislada | Puede o no generar alarma según la magnitud y la evidencia acumulada. |
| Muchas semanas completadas con cero | Revise las advertencias y confirme que realmente representan semanas sin casos. |
| Alarmas en varias unidades vecinas | Investigue cambios compartidos de notificación, exposiciones comunes, movilidad y otras evidencias epidemiológicas. |

- Confirme que la línea de base sea plausible para cada unidad.
- Revise el registro de preparación para solapamientos y semanas agregadas.
- Compruebe cambios de codificación, geografía o notificación.
- Use el resultado como un componente de una evaluación de vigilancia más amplia.


# 6. Descargas y reproducibilidad

| Descarga | Contenido |
| --- | --- |
| Download Bar Plot | Serie semanal observada y esperada para la ubicación detallada actual. |
| Download Trends | Gráfico del proceso CUSUM para la ubicación actual. |
| Download Results CSV | Salida completa para todas las unidades y semanas analizadas. |
| Download Prepared Data CSV | Entrada limpia y agregada usada por el análisis. |

Los filtros de tabla y los decimales afectan solo la visualización. Las descargas conservan las filas subyacentes y la precisión numérica completa.


# 7. Solución de problemas

| Problema o mensaje | Qué revisar |
| --- | --- |
| Missing required columns | Confirme year, week, n_cases o un alias, y la columna geográfica del nivel seleccionado. |
| No locations available | El nivel elegido puede no existir en los datos activos o tener valores vacíos. |
| Insufficient prepared weeks | Cargue un historial más largo o reduzca el período, considerando que el mínimo configurado actual es 52. |
| Low observed coverage | Confirme si las semanas ausentes son ceros reales; cargue una serie más completa si son faltantes de notificación. |
| Recuentos demasiado altos al combinar archivos | Revise archivos activos y el método de solapamiento, especialmente Add Together (Sum). |
| Línea automática poco plausible | Revise unidades e historial y compare con un recuento esperado manual apropiado. |

> **Información para escalar.** Incluya nombres de archivos activos, país, nivel, ubicaciones, parámetros, registro de preparación y texto exacto del error. No envíe datos identificables o restringidos por canales no aprobados.


# 8. Glosario

| Término | Significado |
| --- | --- |
| mu0 / frecuencia esperada | Recuento semanal esperado para una unidad. |
| k | Valor de referencia que controla cuánto contribuyen las desviaciones. |
| h | Umbral de decisión para generar una alarma. |
| ARL0 | Longitud media de corrida esperada bajo ausencia de cambio. |
| RR | Aumento de riesgo que el diseño pretende detectar. |
| Semana preparada | Fila semanal presente después de agregar y, cuando corresponde, completar ceros. |
| Cobertura observada | Proporción de semanas preparadas provenientes de registros cargados, no de filas de cero insertadas. |

