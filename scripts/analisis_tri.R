# ==============================================================================
# PROYECTO: ANÁLISIS TRI MULTIDIMENSIONAL — VULNERABILIDAD EN EL PROCESO JUDICIAL
# Datos: ENPOL 2021  |  N = 490  |  4 dimensiones
# Modelo: TRI Mixto Compensatorio (2PL dicotómicos · GRM politómicos)
# Estimación: EM con errores estándar (SE = TRUE)
# ==============================================================================
# VERSIÓN REVISADA — incluye diagnóstico de parámetros y modelo limpio
# Ítems eliminados por parámetros extremos (|b|>6) o discriminación negativa:
#   P5_2_1, P5_11_01, P6_1_rec, P6_3, P6_15, P6_13, P6_14_3, P7_35, P7_36,
#   P10_7, P10_1
# ==============================================================================

# 0. DETECCIÓN DINÁMICA DEL DIRECTORIO DE TRABAJO --------------------------------
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if (nchar(script_path) > 0) {
    setwd(dirname(script_path))
    # Si estamos dentro de la carpeta 'scripts', subimos un nivel al root del proyecto
    if (basename(getwd()) == "scripts") {
      setwd("..")
    }
  }
} else {
  # Si se corre desde terminal o Rscript, verificamos si estamos en la carpeta scripts
  if (basename(getwd()) == "scripts") {
    setwd("..")
  }
}
cat("[INFO] Directorio de trabajo establecido en:", getwd(), "\n")

# Asegurar la existencia de las carpetas de datos y resultados
if (!dir.exists("datos")) dir.create("datos")
if (!dir.exists("resultados")) dir.create("resultados")

# 1. LIBRERÍAS -------------------------------------------------------------------
library(mirt)
library(dplyr)
library(readr)
library(ggplot2)

# ==============================================================================
# 2. LECTURA DE DATOS
# ==============================================================================
datos <- read_csv("datos/datos_enpol.csv")
cat("[INFO] Filas cargadas:", nrow(datos), "\n")
cat("[INFO] Columnas:      ", ncol(datos), "\n")

# ==============================================================================
# 2b. DIAGNÓSTICO PREVIO — DISTRIBUCIÓN DE ÍTEMS PROBLEMÁTICOS
# ==============================================================================
# Este bloque corre ANTES de la estimación para identificar cuasi-constantes.
# Un ítem cuasi-constante (>90% en una categoría) produce b extremos en TRI.

cat("\n========== DIAGNÓSTICO PREVIO DE DISTRIBUCIONES ==========\n")

items_revisar <- c(
  "P5_2_1", # b=26.4 en modelo original
  "P5_11_01", # b=-14.4, a=-0.13 en modelo original
  "P6_1", # base de P6_1_rec (b1=-31.3)
  "P6_3", # b=9.8
  "P6_15", # b=-19.5
  "P6_13", # umbrales desordenados, a=-0.78
  "P6_14_3", # a=-1.65
  "P7_35", # a=-1.97
  "P7_36", # a=-1.84
  "P10_7", # b1=22.7, b2=18.4, b3=12.5 (todos extremos)
  "P10_1" # b=12.5
)

for (it in items_revisar) {
  if (it %in% colnames(datos)) {
    tbl <- table(datos[[it]], useNA = "always")
    pct <- round(prop.table(tbl) * 100, 1)
    modal <- max(pct, na.rm = TRUE)
    flag <- if (modal > 90) " ⚠ CUASI-CONSTANTE" else ""
    cat(sprintf("\n  %-12s | Modal: %5.1f%%%s\n", it, modal, flag))
    print(data.frame(Cat = names(tbl), N = as.integer(tbl), Pct = as.numeric(pct)),
      row.names = FALSE
    )
  }
}

# ==============================================================================
# 3. LIMPIEZA Y RECODIFICACIÓN
# ==============================================================================

# P5_11_01: valores 0/1 → 1/2 (nota: ítem será excluido del modelo limpio)
datos <- datos %>%
  mutate(P5_11_01 = P5_11_01 + 1)

# P6_1: recodificada para referencia (nota: ítem será excluido del modelo limpio)
datos <- datos %>%
  mutate(P6_1_rec = case_when(
    P6_1 == 0 ~ 1,
    P6_1 <= 4 ~ 2,
    P6_1 <= 7 ~ 3,
    P6_1 >= 8 ~ 4,
    TRUE ~ NA_real_
  ))

cat("\n[INFO] Recodificación completada.\n")

# ==============================================================================
# 4. DEFINICIÓN DE DIMENSIONES — MODELO LIMPIO
# ==============================================================================
# Criterios de exclusión aplicados:
#   (E1) |b| > 6:  dificultad fuera del rango empírico práctico
#   (E2) a < 0:    discriminación negativa (ítem mide dirección opuesta)
#   (E3) GRM con umbrales desordenados y a < 0 simultaneamente
#
# Ítems excluidos y razón:
#   P5_2_1   → (E1) b = 26.4 — cuasi-constante (>95% en cat. 0)
#   P5_11_01 → (E1+E2) b = -14.4, a = -0.13 — cuasi-constante + dir. inversa
#   P6_1_rec → (E1) b1 = -31.3 — colapso de categorías en recodificación
#   P6_3     → (E1) b = 9.8 — cuasi-constante (casi nadie en cat. 1)
#   P6_15    → (E1) b = -19.5 — cuasi-constante (casi todos en cat. 1)
#   P6_13    → (E2+E3) Pregunta: ¿Cuántas veces al día le proporcionan alimentos?
#              Codificación: 1=1 vez (peor) · 2=2 veces · 3=3 veces · 4=4+ veces (mejor)
#              Problema: valores concentrados en cat. 3 (>85% de la muestra), poca varianza.
#              El GRM produce a=-0.78 y umbrales invertidos. La dimensión "Satisfacción"
#              se define por acceso a visitas/servicios, no por frecuencia de comidas
#              (que es casi uniforme). Excluir y mencionar en limitaciones.
#   P6_14_3  → (E2) a = -1.65 — discriminación negativa moderada
#   P7_35    → (E2) a = -1.97 — PERCEPCIÓN SUBJETIVA de seguridad: codificación 1=Seguro, 2=Inseguro
#              La discriminación NEGATIVA es un hallazgo sustantivo, no un error de datos.
#              Personas con mayor victimización objetiva reportan sentirse más seguras
#              (adaptación perceptual / normalización de la violencia). Constructo
#              divergente del resto de Dim 3 (victimización objetiva).
#   P7_36    → (E2) a = -1.84 — Mismo fenómeno que P7_35 (percepción de seguridad en
#              el centro penitenciario). Codificación: 1=Seguro, 2=Inseguro. Excluir.
#   P10_7    → (E1) b1=22.7, b2=18.4, b3=12.5 — todos los umbrales extremos
#              Concentración en cat. 4 ("muy preparado para salir") para ~90% de la muestra
#   P10_1    → (E1) b = 12.5 — Codificación: Sí=¿tiene empleo al salir?
#              >85% responde No. Cuasi-constante: pocas personas tienen empleo asegurado
#
# NOTA METODOLÓGICA: Estos ítems presentan problemas de estimación que hacen
# sus parámetros no interpretables. Su exclusión mejora la validez del modelo
# sin comprometer la representación conceptual de las dimensiones.

# --- DIMENSIÓN 1: Claridad Judicial (9 ítems) ---
# Excluidos: P5_2_1 (E1), P5_11_01 (E1+E2)
dim1_items <- c(
  "P5_17_1", "P5_17_2", "P5_17_3", "P5_17_4", # Likert 4 cats: comprensión del proceso
  "P5_16_1", "P5_16_3", # Ordinal 3 cats: claridad de acusación
  "P5_1", "P5_2_4", # Dicotómicas: acceso a defensa
  "P5_26" # Likert 4 cats: satisfacción con defensa
)

# --- DIMENSIÓN 2: Satisfacción Institucional (4 ítems) ---
# Excluidos: P6_1_rec (E1), P6_3 (E1), P6_15 (E1), P6_13 (E2+E3), P6_14_3 (E2)
# NOTA: La reducción de 9 a 4 ítems refleja que muchos indicadores de esta
# dimensión eran cuasi-constantes en esta muestra. Se recomienda con cautela.
dim2_items <- c(
  "P6_4_1",  "P6_4_2", # Dicotómicas: acceso a servicios
  "P6_14_1", "P6_14_2" # Dicotómicas: visitas y comunicación
)

# --- DIMENSIÓN 3: Inseguridad Intracarcelaria (11 ítems) ---
# Excluidos:
#   P7_35: ¿Se siente seguro/inseguro en su CELDA? (1=Seg, 2=Inseg) → a=-1.97
#   P7_36: ¿Se siente seguro/inseguro en el CENTRO? (1=Seg, 2=Inseg) → a=-1.84
#   Ambos miden percepción SUBJETIVA de seguridad. La discriminación negativa
#   documenta la paradoja de adaptación perceptual: mayor victimización objetiva
#   (medida por P7_40_* y P7_1) → mayor sensación subjetiva de seguridad.
#   Constructo empíricamente divergente; excluir con nota en la discusión.
dim3_items <- c(
  "P7_37",   "P7_38", # Likert 4 cats: percepción de seguridad
  "P7_40_1", "P7_40_2", "P7_40_3", # Dicotómicas: victimización
  "P7_40_4", "P7_40_5", "P7_40_6", # Dicotómicas: victimización
  "P7_1",    "P7_9", # Dicotómicas: contexto de violencia
  "P8_9_12" # Ordinal 3 cats: acceso a salud
)

# --- DIMENSIÓN 4: Expectativas de Salida (7 ítems) ---
# Excluidos: P10_7 (E1), P10_1 (E1)
dim4_items <- c(
  "P10_5_1", # Dicotómica: red de apoyo
  "P10_5_2", "P10_5_3", "P10_5_4", # Ordinal 3 cats: recursos personales
  "P10_3",   "P10_6",   "P10_8" # Dicotómicas: expectativas laborales
)

todos_items <- c(dim1_items, dim2_items, dim3_items, dim4_items)

cat("\n========== ESTRUCTURA DEL MODELO LIMPIO ==========\n")
cat("  Dim 1 — Claridad Judicial       :", length(dim1_items), "ítems:", paste(dim1_items, collapse = ", "), "\n")
cat("  Dim 2 — Satisfacción Inst.      :", length(dim2_items), "ítems:", paste(dim2_items, collapse = ", "), "\n")
cat("  Dim 3 — Inseguridad Intrac.     :", length(dim3_items), "ítems:", paste(dim3_items, collapse = ", "), "\n")
cat("  Dim 4 — Expectativas de Salida  :", length(dim4_items), "ítems:", paste(dim4_items, collapse = ", "), "\n")
cat("  Total de ítems en el modelo     :", length(todos_items), "\n\n")

# ==============================================================================
# 5. PREPARACIÓN DE LA MATRIZ DE DATOS
# ==============================================================================
datos_mirt <- datos %>%
  select(all_of(todos_items)) %>%
  as.data.frame()

cat("Verificación de NAs por ítem:\n")
nas <- colSums(is.na(datos_mirt))
if (any(nas > 0)) {
  print(nas[nas > 0])
} else {
  cat("  Sin NAs en ningún ítem.\n")
}

# ==============================================================================
# 6. TIPO DE MODELO POR ÍTEM (2PL o GRM)
# ==============================================================================
get_itemtype <- function(item_name, data) {
  n_cats <- length(unique(na.omit(data[[item_name]])))
  if (n_cats == 2) {
    return("2PL")
  } else {
    return("graded")
  }
}

tipos_items <- sapply(todos_items, get_itemtype, data = datos_mirt)

cat("\nTipos de modelo por ítem:\n")
print(table(tipos_items))
cat("\nDetalle:\n")
print(tipos_items)

# ==============================================================================
# 7. ESPECIFICACIÓN MULTIDIMENSIONAL (estructura simple)
# ==============================================================================
n_dim1 <- length(dim1_items)
n_dim2 <- length(dim2_items)
n_dim3 <- length(dim3_items)
n_dim4 <- length(dim4_items)
n_total <- length(todos_items)

specs <- matrix(0, nrow = n_total, ncol = 4)
specs[1:n_dim1, 1] <- 1
specs[(n_dim1 + 1):(n_dim1 + n_dim2), 2] <- 1
specs[(n_dim1 + n_dim2 + 1):(n_dim1 + n_dim2 + n_dim3), 3] <- 1
specs[(n_dim1 + n_dim2 + n_dim3 + 1):n_total, 4] <- 1

colnames(specs) <- c(
  "Claridad_Judicial", "Satisfaccion_Inst",
  "Inseguridad", "Expectativas"
)
rownames(specs) <- todos_items

cat("\nEstructura del modelo (primeras 5 filas):\n")
print(head(specs, 5))

# ==============================================================================
# 8. ESTIMACIÓN DEL MODELO TRI MULTIDIMENSIONAL — MODELO LIMPIO
# ==============================================================================
cat("\n--- Estimando modelo TRI limpio (puede tardar unos minutos)... ---\n")

modelo_tri <- mirt(
  data     = datos_mirt,
  model    = mirt.model(specs),
  itemtype = tipos_items,
  method   = "EM",
  SE       = TRUE,
  verbose  = TRUE
)

cat("\n[INFO] Modelo estimado correctamente.\n")

# ==============================================================================
# 8b. VALIDACIÓN AUTOMÁTICA DE PARÁMETROS
# ==============================================================================
cat("\n========== VALIDACIÓN DE PARÁMETROS DEL MODELO LIMPIO ==========\n")

params_raw <- coef(modelo_tri, IRTpars = TRUE, simplify = TRUE)
params_df <- as.data.frame(params_raw$items)

# Detectar a efectiva y b máxima
params_df$item <- rownames(params_df)
params_df$a_efectiva <- apply(
  params_df[, c("a1", "a2", "a3", "a4")], 1,
  function(x) x[x != 0][1]
)
params_df$b_max_abs <- apply(
  params_df[, c("b", "b1", "b2", "b3")], 1,
  function(x) max(abs(x), na.rm = TRUE)
)

items_b_extremo <- params_df$item[params_df$b_max_abs > 6]
items_a_negativa <- params_df$item[!is.na(params_df$a_efectiva) &
  params_df$a_efectiva < 0]

if (length(items_b_extremo) > 0) {
  cat("\n  ⚠ ALERTA — Ítems con |b| > 6 (dificultad extrema):\n")
  print(items_b_extremo)
} else {
  cat("\n  ✓ Sin ítems con dificultad extrema (todos |b| ≤ 6).\n")
}

if (length(items_a_negativa) > 0) {
  cat("\n  ⚠ ALERTA — Ítems con discriminación negativa:\n")
  print(items_a_negativa)
} else {
  cat("  ✓ Sin ítems con discriminación negativa.\n")
}

# ==============================================================================
# 9. ÍNDICES DE AJUSTE DEL MODELO
# ==============================================================================
cat("\n========== ÍNDICES DE AJUSTE ==========\n")
ajuste <- M2(modelo_tri, type = "M2*")
print(ajuste)

cat("\nEstadísticos de información:\n")
info_modelo <- modelo_tri@Fit
cat("  AIC:                 ", info_modelo$AIC, "\n")
cat("  BIC:                 ", info_modelo$BIC, "\n")
cat("  Log-likelihood:      ", info_modelo$logLik, "\n")
# Obtener nestpars de forma robusta para evitar errores de versión
nestpars_val <- tryCatch(
  {
    extract.mirt(modelo_tri, "nestpars")
  },
  error = function(e1) {
    tryCatch(
      {
        modelo_tri@Fit$nestpars
      },
      error = function(e2) {
        # Fallback seguro
        length(unlist(coef(modelo_tri)))
      }
    )
  }
)

cat("  Parámetros estimados:", nestpars_val, "\n")

# Guardar índices de ajuste global a CSV
global_fit <- data.frame(
  AIC = info_modelo$AIC,
  BIC = info_modelo$BIC,
  LogLik = info_modelo$logLik,
  nestpars = nestpars_val,
  M2 = ajuste$M2,
  df_M2 = ajuste$df,
  p_M2 = ajuste$p,
  RMSEA_M2 = ajuste$RMSEA,
  CFI_M2 = ifelse(is.null(ajuste$CFI), NA, ajuste$CFI),
  TLI_M2 = ifelse(is.null(ajuste$TLI), NA, ajuste$TLI)
)
write.csv(global_fit, "resultados/TRI_indices_ajuste.csv", row.names = FALSE)
cat("[INFO] Índices de ajuste guardados en: resultados/TRI_indices_ajuste.csv\n")


# ==============================================================================
# 10. PARÁMETROS DE LOS ÍTEMS
# ==============================================================================
cat("\n========== PARÁMETROS DE LOS ÍTEMS (MODELO LIMPIO) ==========\n")

params_items <- as.data.frame(params_raw$items)
cols_num <- sapply(params_items, is.numeric)
params_items[cols_num] <- round(params_items[cols_num], 3)
print(params_items)

write.csv(params_items, "resultados/parametros_items_TRI_limpio.csv")
cat("\n[INFO] Parámetros guardados en: resultados/parametros_items_TRI_limpio.csv\n")

# ==============================================================================
# 11. ESTADÍSTICOS DE AJUSTE POR ÍTEM (S-X2)
# ==============================================================================
cat("\n========== AJUSTE POR ÍTEM (S-X2) ==========\n")
ajuste_items <- itemfit(modelo_tri, fit_stats = "S_X2")
cols_num2 <- sapply(ajuste_items, is.numeric)
ajuste_items[cols_num2] <- round(ajuste_items[cols_num2], 3)
print(ajuste_items)

items_mal_ajuste <- ajuste_items$item[ajuste_items$p.S_X2 < 0.05]
if (length(items_mal_ajuste) > 0) {
  cat("\n  ⚠ Ítems con ajuste deficiente (p < 0.05):", paste(items_mal_ajuste, collapse = ", "), "\n")
} else {
  cat("\n  ✓ Todos los ítems con ajuste aceptable.\n")
}

write.csv(ajuste_items, "resultados/ajuste_por_item_limpio.csv")
cat("[INFO] Ajuste por ítem guardado en: resultados/ajuste_por_item_limpio.csv\n")

# ==============================================================================
# 12. CURVAS CARACTERÍSTICAS (ICC) POR DIMENSIÓN
# ==============================================================================
cat("\n--- Generando ICCs... ---\n")

# Paleta de colores accesible por número de categorías
paletas <- list(
  "2"  = c("#2166AC", "#D73027"),
  "3"  = c("#2166AC", "#FDAE61", "#D73027"),
  "4"  = c("#2166AC", "#74ADD1", "#F46D43", "#D73027")
)

graficar_icc_dimension <- function(modelo, items_dim, nombre_dim, specs_mat) {
  indices <- which(rownames(specs_mat) %in% items_dim)
  n_items <- length(indices)
  n_cols <- 3
  n_rows <- ceiling(n_items / n_cols)
  d <- which(colnames(specs_mat) == nombre_dim)

  theta_seq <- seq(-3, 3, by = 0.1)
  theta_mat <- matrix(0, nrow = length(theta_seq), ncol = 4)
  theta_mat[, d] <- theta_seq

  archivo_pdf <- paste0("resultados/ICC_", nombre_dim, "_limpio.pdf")
  pdf(archivo_pdf, width = n_cols * 4, height = n_rows * 3.2)
  par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1.5))

  for (i in seq_along(indices)) {
    nombre_item <- items_dim[i]
    tryCatch(
      {
        # extract.item garantiza compatibilidad con modelos multidimensionales
        item_obj <- extract.item(modelo, indices[i])
        probs <- probtrace(item_obj, theta_mat)
        n_cats <- ncol(probs)
        colores <- if (as.character(n_cats) %in% names(paletas)) {
          paletas[[as.character(n_cats)]]
        } else {
          rainbow(n_cats)
        }

        matplot(theta_seq, probs,
          type = "l", lwd = 2.5,
          main = nombre_item,
          xlab = expression(theta),
          ylab = "P(X = k)",
          ylim = c(0, 1),
          col = colores,
          lty = 1,
          cex.main = 1.1, cex.axis = 0.9
        )
        abline(v = 0, col = "gray80", lty = 2)
        legend("topright",
          legend = paste0("Cat ", 1:n_cats),
          col = colores,
          lty = 1, lwd = 2, cex = 0.65,
          bg = "white", box.lty = 0
        )
      },
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste0("Error: ", nombre_item, "\n", conditionMessage(e)),
          cex = 0.75, col = "red3"
        )
      }
    )
  }
  dev.off()
  cat("[INFO] ICC guardadas en:", archivo_pdf, "\n")
}

graficar_icc_dimension(modelo_tri, dim1_items, "Claridad_Judicial", specs)
graficar_icc_dimension(modelo_tri, dim2_items, "Satisfaccion_Inst", specs)
graficar_icc_dimension(modelo_tri, dim3_items, "Inseguridad", specs)
graficar_icc_dimension(modelo_tri, dim4_items, "Expectativas", specs)

# ==============================================================================
# 13. FUNCIÓN DE INFORMACIÓN DEL TEST (TIF) POR DIMENSIÓN
# ==============================================================================
cat("\n--- Generando TIF por dimensión... ---\n")

theta_seq <- seq(-3, 3, by = 0.1)
colores_dim <- c("#1b7837", "#762a83", "#c7522a", "#2166ac")
nombres_dim <- c(
  "Claridad Judicial", "Satisfacción Institucional",
  "Inseguridad Intracarcelaria", "Expectativas de Salida"
)

pdf("resultados/TIF_por_dimension_limpio.pdf", width = 13, height = 9)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1.5))

for (d in 1:4) {
  items_idx <- which(specs[, d] == 1)
  info_total <- rep(0, length(theta_seq))

  for (item_idx in items_idx) {
    tryCatch(
      {
        theta_mat <- matrix(0, nrow = length(theta_seq), ncol = 4)
        theta_mat[, d] <- theta_seq
        info_item <- testinfo(modelo_tri, Theta = theta_mat, which.items = item_idx)
        info_total <- info_total + info_item
      },
      error = function(e) NULL
    )
  }

  # Calcular SEM = 1 / sqrt(I(theta))
  sem <- ifelse(info_total > 0, 1 / sqrt(info_total), NA)

  plot(theta_seq, info_total,
    type = "l", lwd = 2.5,
    col = colores_dim[d],
    main = paste("TIF —", nombres_dim[d]),
    xlab = expression(theta),
    ylab = "Información",
    ylim = c(0, max(info_total, na.rm = TRUE) * 1.15),
    cex.main = 1.05, cex.axis = 0.9
  )
  # Área bajo la curva (shading)
  polygon(c(theta_seq, rev(theta_seq)),
    c(info_total, rep(0, length(theta_seq))),
    col = adjustcolor(colores_dim[d], alpha.f = 0.15), border = NA
  )
  abline(v = 0, col = "gray70", lty = 2)
  # Theta de máxima información
  theta_max_info <- theta_seq[which.max(info_total)]
  abline(v = theta_max_info, col = colores_dim[d], lty = 3)
  legend("topright",
    legend = c(
      sprintf("θ_máx = %.2f", theta_max_info),
      sprintf("I_máx = %.2f", max(info_total, na.rm = TRUE))
    ),
    col = colores_dim[d], lty = c(3, 1), lwd = c(1, 2.5),
    cex = 0.75, bg = "white", box.lty = 0
  )
}

dev.off()
cat("[INFO] TIF guardadas en: resultados/TIF_por_dimension_limpio.pdf\n")

# ==============================================================================
# 14. SCORES DE LOS SUJETOS (theta EAP)
# ==============================================================================
cat("\n--- Estimando scores de los sujetos (EAP)... ---\n")

scores <- fscores(modelo_tri, method = "EAP", full.scores = TRUE)
scores_df <- as.data.frame(scores)
colnames(scores_df) <- c(
  "theta_Claridad", "theta_Satisfaccion",
  "theta_Inseguridad", "theta_Expectativas"
)

cat("\nResumen de scores por dimensión:\n")
print(summary(scores_df))

write.csv(scores_df, "resultados/scores_theta_limpio.csv", row.names = FALSE)
cat("[INFO] Scores guardados en: resultados/scores_theta_limpio.csv\n")

# Guardar descriptivos a CSV
descriptivos <- data.frame(
  Dimension = colnames(scores_df),
  M = round(sapply(scores_df, mean, na.rm = TRUE), 3),
  DE = round(sapply(scores_df, sd, na.rm = TRUE), 3),
  Min = round(sapply(scores_df, min, na.rm = TRUE), 3),
  Max = round(sapply(scores_df, max, na.rm = TRUE), 3)
)
write.csv(descriptivos, "resultados/TRI_scores_descriptivos.csv", row.names = FALSE)
cat("[INFO] Descriptivos de scores guardados en: resultados/TRI_scores_descriptivos.csv\n")


# ==============================================================================
# 15. CORRELACIONES ENTRE DIMENSIONES
# ==============================================================================
cat("\n========== CORRELACIONES ENTRE DIMENSIONES (theta) ==========\n")
cor_thetas <- round(cor(scores_df, use = "complete.obs"), 3)
print(cor_thetas)

# ==============================================================================
# 16. RESUMEN FINAL
# ==============================================================================
cat("\n")
cat("================================================================\n")
cat("  ANÁLISIS TRI COMPLETADO — MODELO LIMPIO\n")
cat("================================================================\n")
cat("  Ítems por dimensión:\n")
cat("    Dim 1 Claridad Judicial:       ", length(dim1_items), "ítems\n")
cat("    Dim 2 Satisfacción Inst.:      ", length(dim2_items), "ítems\n")
cat("    Dim 3 Inseguridad Intrac.:     ", length(dim3_items), "ítems\n")
cat("    Dim 4 Expectativas de Salida:  ", length(dim4_items), "ítems\n")
cat("    Total:                         ", length(todos_items), "ítems\n\n")
cat("  Archivos generados en la carpeta resultados/:\n")
cat("    - parametros_items_TRI_limpio.csv\n")
cat("    - ajuste_por_item_limpio.csv\n")
cat("    - scores_theta_limpio.csv\n")
cat("    - TRI_scores_descriptivos.csv\n")
cat("    - TRI_indices_ajuste.csv\n")
cat("    - ICC_Claridad_Judicial_limpio.pdf\n")
cat("    - ICC_Satisfaccion_Inst_limpio.pdf\n")
cat("    - ICC_Inseguridad_limpio.pdf\n")
cat("    - ICC_Expectativas_limpio.pdf\n")
cat("    - TIF_por_dimension_limpio.pdf\n")
cat("================================================================\n")
