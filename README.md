# Vulnerabilidad-Judicial-ENPOL-2021
Análisis psicométrico de vulnerabilidad en personas privadas de la libertad en México usando TRI Multidimensional
# Proyecto de Modelamiento Psicométrico Avanzado (ENPOL 2021)
**Autores:** Roberto Cristian Calixto Guevara, Yulia Isabel Baeza Ruffino y Valeria Yamile Garcia Mosco.
**Asesores:** Dr. Ramsés Vásquez-Lira y Lic. Yemil Caleano Becerril

Este repositorio contiene el código fuente, la base de datos limpia y la estructura de organización para el análisis psicométrico del **Rasgo Latente de Vulneración Institucional y Derechos Humanos en la Población Excarcelada en México**, utilizando modelos confirmatorios y de **Teoría de Respuesta al Ítem (TRI)** politómica.

Este proyecto ha sido estructurado siguiendo las normas de entrega solicitadas por el profesor Ramsés y alineado conceptualmente con las necesidades de un diagnóstico para las personas que han salido de prisión.

---

## 📁 Estructura del Repositorio

La carpeta de trabajo se organiza de forma modular y portable de la siguiente manera:

| Carpeta / Archivo | Contenido | Propósito |
| :--- | :--- | :--- |
| **`datos/`** | `datos_enpol.csv` | Almacena exclusivamente la base de datos depurada y filtrada ($N = 490$ observaciones) utilizada para el modelamiento. |
| **`scripts/`** | `analisis_tri.R` | Código fuente de R, completamente documentado y comentado paso a paso. Contiene la preparación, diagnóstico, definición multidimensional, estimación en la librería `mirt` y generación de curvas gráficas. |
| **`resultados/`** | Curvas en PDF y tablas en CSV | Carpeta de destino automatizada donde el script guarda todos los reportes de ajuste y archivos de gráficas sin intervenir la raíz del proyecto. |
| **`README.md`** | Documentación (este archivo) | Guía técnica y metodológica completa para que otros investigadores y docentes puedan reproducir los análisis de forma inmediata. |

---

## 🛠️ Requisitos del Entorno

Para ejecutar este código, asegúrese de tener instalado **R (versión $\ge$ 4.0.0)** y los siguientes paquetes de R:

```R
# Ejecute este comando en su consola de R para instalar las dependencias necesarias:
install.packages(c("mirt", "dplyr", "readr", "ggplot2"))
```

### Paquetes Principales:
- **`mirt`**: Librería especializada de R para la estimación de modelos de Teoría de Respuesta al Ítem Multidimensionales y Unidimensionales (2PL, GRM, etc.).
- **`dplyr`**: Herramientas eficientes de manipulación y recodificación de datos.
- **`readr`**: Lectura y escritura rápida de archivos rectangulares delimitados (.csv).
- **`ggplot2`**: Visualización gráfica avanzada (si bien el script principal emplea el motor gráfico robusto de `mirt` y `lattice` adaptado en PDF).

---

## 🚀 Instrucciones de Ejecución

El script `analisis_tri.R` está diseñado para ser **completamente portable**. Detecta de manera dinámica si se está ejecutando de forma interactiva en **RStudio** o desde la **Terminal**, ajustando el directorio de trabajo (`working directory`) automáticamente a la raíz de la carpeta `Proyecto_Psicometria/`.

### Opción A: Desde RStudio (Recomendada)
1. Abra **RStudio**.
2. Abra el archivo `scripts/analisis_tri.R`.
3. Seleccione todo el código (`Ctrl + A` / `Cmd + A`) y presione **Run** o `Ctrl + Enter`.
4. El script detectará la ruta del archivo y dirigirá todas las lecturas a `datos/` y escrituras a `resultados/` de forma transparente.

### Opción B: Desde la Terminal (Consola de Sistema)
Ejecute el siguiente comando situándose en la carpeta del proyecto:
```bash
Rscript scripts/analisis_tri.R
```
*(Si ejecuta el script directamente parado dentro de la carpeta `scripts/`, el detector dinámico subirá un nivel al root automáticamente para evitar errores de ruta).*

---

## 📊 Descripción de Entregables (Resultados Generados)

Una vez que el script finalice su ejecución (toma aproximadamente de 1 a 2 minutos debido al cálculo de errores estándar robustos en TRI), se poblará automáticamente la carpeta `resultados/` con los siguientes entregables psicométricos:

### 1. Tablas y Parámetros en CSV:
- **`TRI_indices_ajuste.csv`**: Estadísticos globales de bondad de ajuste del modelo multidimensional estimulado (incluye AIC, BIC, Log-likelihood, M2, grados de libertad, p-valor de M2 y RMSEA robusto).
- **`parametros_items_TRI_limpio.csv`**: Parámetros estimados del Modelo de Respuesta Graduada (GRM) y del modelo logístico de 2 parámetros (2PL) para cada reactivo (discriminación $a$, dificultad $b$, e intercepts $d_k$).
- **`ajuste_por_item_limpio.csv`**: Estadísticos de ajuste locales por reactivo ($S-\chi^2$ de Orlando y Thissen), útiles para diagnosticar si algún ítem específico se desvía del comportamiento probabilístico modelado.
- **`scores_theta_limpio.csv`**: Estimaciones de puntuaciones factoriales individuales (habilidades/rasgos latentes $\theta$) para los 490 evaluados en cada una de las 4 dimensiones teóricas bajo el método EAP (Expected A Posteriori).
- **`TRI_scores_descriptivos.csv`**: Tabla de síntesis estadística con la media, desviación estándar, valor mínimo y máximo de los puntajes latentes estimados.

### 2. Reportes Gráficos en PDF:
- **`ICC_[Dimension]_limpio.pdf`**: Curvas Características de los Ítems (ICC) organizadas modularmente en rejillas por dimensión teórica. Muestran la probabilidad de seleccionar cada categoría de respuesta ($P(X=k)$) como una función del rasgo latente de vulneración ($\theta$).
- **`TIF_por_dimension_limpio.pdf`**: Curva de Información del Test (TIF / TIC) y Función del Error Estándar de Medición para cada una de las 4 dimensiones. Permiten identificar visualmente en qué segmentos del rasgo de vulneración (bajo, medio, severo) el test mide con la máxima precisión y con el menor error estándar posible.

---

## 📝 Nota Metodológica
El modelo psicométrico depurado y calibrado representa una estructura multidimensional simple de **4 dimensiones** que superan los criterios de higiene distributiva y consistencia métrica:
1. **Claridad Judicial** (9 ítems)
2. **Satisfacción Institucional** (4 ítems)
3. **Inseguridad Intracarcelaria** (11 ítems)
4. **Expectativas de Salida** (7 ítems)

*Todos los reactivos con discriminación negativa ($a < 0$) o dificultades fuera del límite práctico ($|b| > 6$) fueron purgados del modelo psicométrico en la estimación final para garantizar la convergencia matemática estable y la precisión diagnóstica del instrumento.*

---

  **Ciudad de México, 2026**
