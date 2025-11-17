# ==============================================================================
# TALLER 3 - ESTADÍSTICA II
# ANÁLISIS DE CORRELACIÓN Y REGRESIÓN MÚLTIPLE
# ==============================================================================
# 
# Este script realiza un análisis completo de correlación y regresión múltiple
# utilizando el conjunto de datos trabajado en la unidad de inferencia y 
# pruebas de hipótesis (Taller 2).
#
# Variables del estudio:
# - P3087S1: Valor mensual por prácticas o pasantías
# - P3094S3: Cuánto ahorro por cultivar
# - P3095S3: Valor ahorra por criar animales
# - P3101: ¿Fue a reuniones familiares durante las ultimas 4 semanas? (Sí/No)
# ==============================================================================

# ==============================================================================
# 0. CONFIGURACIÓN INICIAL
# ==============================================================================

# Instalar y cargar librerías necesarias
required_pkgs <- c(
  "tidyverse",    # Manipulación de datos y gráficos
  "readr",        # Lectura de datos
  "stringr",      # Manipulación de strings
  "corrplot",     # Gráficos de correlación
  "Hmisc",        # Correlaciones con valores p
  "ggpubr",       # Gráficos combinados
  "GGally",       # Gráficos de pares
  "lmtest",       # Pruebas de supuestos de regresión
  "car",          # Análisis de regresión
  "nortest",      # Pruebas de normalidad
  "broom",        # Organización de resultados
  "knitr",        # Tablas formateadas
  "kableExtra"    # Tablas mejoradas
)

install_if_needed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_pkgs, install_if_needed))

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(corrplot)
  library(Hmisc)
  library(ggpubr)
  library(GGally)
  library(lmtest)
  library(car)
  library(nortest)
  library(broom)
  library(knitr)
  library(kableExtra)
})

cat("✅ Librerías cargadas correctamente\n")

# Configurar opciones para visualización
options(scipen = 999)  # Evitar notación científica
theme_set(theme_minimal())

# ==============================================================================
# 1. CARGA Y LIMPIEZA DE DATOS
# ==============================================================================

cat("\n📂 Cargando datos...\n")

# Buscar archivo Combinado.csv
find_existing_path <- function(paths) {
  for (p in paths) {
    if (file.exists(p)) return(p)
  }
  stop("No se encontró 'Combinado.csv' en rutas esperadas")
}

paths <- c(
  "Combinado.csv",
  "../Combinado.csv",
  "../Taller 1/Combinado.csv",
  "Taller 1/Combinado.csv",
  "../../Taller 1/Combinado.csv"
)

csv_path <- find_existing_path(paths)
cat("✔ Archivo encontrado en:", csv_path, "\n")

# Cargar datos
df_raw <- readr::read_csv(
  csv_path,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  col_types = cols(
    .default = col_guess(),
    P3094S3 = col_character(),
    P3087S1 = col_character(),
    P3095S3 = col_character(),
    P3101   = col_guess()
  )
)

# Función de limpieza numérica
clean_numeric <- function(x) {
  x_chr <- as.character(x)
  x_chr <- str_trim(x_chr)
  x_chr <- str_replace_all(x_chr, "\\s+", "")
  x_chr <- str_replace_all(x_chr, "[€$£'\"]", "")
  x_chr <- str_replace_all(x_chr, "\\.", "")  # Eliminar puntos de miles
  x_chr <- str_replace_all(x_chr, ",", ".")   # Coma como decimal
  suppressWarnings(as.numeric(x_chr))
}

# Codificación binaria de P3101: 1 = Sí, 2 = No
p3101_raw <- suppressWarnings(as.integer(df_raw$P3101))
p3101_bin <- case_when(
  p3101_raw == 1L ~ 1L,
  p3101_raw == 2L ~ 0L,
  TRUE ~ NA_integer_
)

# Crear dataframe limpio
df <- df_raw %>%
  mutate(
    P3094S3_clean = clean_numeric(P3094S3),  # Ahorro por cultivar
    P3087S1_clean = clean_numeric(P3087S1),  # Valor mensual por prácticas
    P3095S3_clean = clean_numeric(P3095S3),  # Ahorro por criar animales
    P3101_bin     = p3101_bin                 # Reuniones familiares (1=Sí, 0=No)
  )

cat("\n📊 Resumen de datos limpios:\n")
cat("P3087S1_clean (prácticas):", sum(!is.na(df$P3087S1_clean)), "observaciones\n")
cat("P3094S3_clean (ahorro cultivar):", sum(!is.na(df$P3094S3_clean)), "observaciones\n")
cat("P3095S3_clean (ahorro animales):", sum(!is.na(df$P3095S3_clean)), "observaciones\n")

# Filtrar casos con al menos 2 variables cuantitativas para análisis
df_corr <- df %>%
  select(P3087S1_clean, P3094S3_clean, P3095S3_clean) %>%
  filter(complete.cases(.)) %>%
  filter_all(all_vars(. > 0 | is.na(.)))  # Eliminar valores negativos o cero

cat("\n📋 Casos completos para análisis:", nrow(df_corr), "\n")

# ==============================================================================
# 2. ANÁLISIS DE CORRELACIÓN
# ==============================================================================

cat("\n🔗 ANÁLISIS DE CORRELACIÓN\n")
cat(paste0(rep("=", 61), collapse = ""), "\n")

# Definir variables para correlación
variables_corr <- c("P3087S1_clean", "P3094S3_clean", "P3095S3_clean")
nombres_vars <- c(
  "Valor mensual por prácticas",
  "Ahorro por cultivar",
  "Ahorro por criar animales"
)

# Crear matriz de datos para correlación
df_corr_matrix <- df_corr %>%
  select(all_of(variables_corr)) %>%
  set_names(nombres_vars)

# 2.1 Matriz de correlación de Pearson
cat("\n2.1 COEFICIENTES DE CORRELACIÓN DE PEARSON\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cor_pearson <- cor(df_corr_matrix, use = "complete.obs", method = "pearson")
print(round(cor_pearson, 4))

# 2.2 Pruebas de significancia para cada par de variables

cat("\n2.2 PRUEBAS DE SIGNIFICANCIA DE CORRELACIÓN\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

# Función para realizar prueba de correlación con hipótesis formales
test_correlacion <- function(var1, var2, nombre1, nombre2, datos) {
  # Obtener datos completos para el par
  datos_pair <- datos %>%
    select(!!sym(var1), !!sym(var2)) %>%
    drop_na()
  
  if (nrow(datos_pair) < 3) {
    cat("\n⚠️ Insuficientes datos para", nombre1, "vs", nombre2, "\n")
    return(NULL)
  }
  
  # Realizar prueba de correlación
  test_result <- cor.test(datos_pair[[var1]], datos_pair[[var2]], 
                          method = "pearson")
  
  # Plantear hipótesis
  cat("\n📌 Relación:", nombre1, "vs", nombre2, "\n")
  cat("H₀: ρ = 0 (no hay correlación lineal)\n")
  cat("H₁: ρ ≠ 0 (hay correlación lineal)\n")
  
  # Resultados
  cat("\nCoeficiente de correlación (r):", 
      round(test_result$estimate, 4), "\n")
  cat("Estadístico t:", round(test_result$statistic, 4), "\n")
  cat("Grados de libertad:", test_result$parameter, "\n")
  cat("Valor-p:", format(test_result$p.value, scientific = TRUE), "\n")
  
  # Interpretación
  r <- test_result$estimate
  abs_r <- abs(r)
  
  if (abs_r < 0.3) {
    fuerza <- "débil"
  } else if (abs_r < 0.7) {
    fuerza <- "moderada"
  } else {
    fuerza <- "fuerte"
  }
  
  if (r > 0) {
    direccion <- "positiva"
  } else {
    direccion <- "negativa"
  }
  
  cat("\nInterpretación:\n")
  if (test_result$p.value < 0.05) {
    cat("✓ Se rechaza H₀. Hay evidencia de correlación", direccion, 
        fuerza, "\n")
    cat("  (|r| =", round(abs_r, 4), ")\n")
  } else {
    cat("✗ No se rechaza H₀. No hay evidencia suficiente de correlación\n")
  }
  
  # Clasificación de la correlación
  cat("\nClasificación:\n")
  if (abs_r < 0.1) {
    cat("  Correlación prácticamente nula (|r| < 0.1)\n")
  } else if (abs_r < 0.3) {
    cat("  Correlación débil (0.1 ≤ |r| < 0.3)\n")
  } else if (abs_r < 0.7) {
    cat("  Correlación moderada (0.3 ≤ |r| < 0.7)\n")
  } else {
    cat("  Correlación fuerte (|r| ≥ 0.7)\n")
  }
  
  return(list(
    variable1 = nombre1,
    variable2 = nombre2,
    r = r,
    p_value = test_result$p.value,
    estadistico_t = test_result$statistic,
    df = test_result$parameter,
    decision = ifelse(test_result$p.value < 0.05, 
                      "Rechazar H₀", "No rechazar H₀"),
    fuerza = fuerza,
    direccion = direccion
  ))
}

# Aplicar pruebas a todos los pares
resultados_correlacion <- list()

# Par 1: Prácticas vs Ahorro por cultivar
resultados_correlacion[[1]] <- test_correlacion(
  "P3087S1_clean", "P3094S3_clean",
  "Valor mensual por prácticas", "Ahorro por cultivar",
  df_corr
)

# Par 2: Prácticas vs Ahorro por criar animales
resultados_correlacion[[2]] <- test_correlacion(
  "P3087S1_clean", "P3095S3_clean",
  "Valor mensual por prácticas", "Ahorro por criar animales",
  df_corr
)

# Par 3: Ahorro por cultivar vs Ahorro por criar animales
resultados_correlacion[[3]] <- test_correlacion(
  "P3094S3_clean", "P3095S3_clean",
  "Ahorro por cultivar", "Ahorro por criar animales",
  df_corr
)

# 2.3 Matriz de correlación con valores p (Hmisc)
cat("\n\n2.3 MATRIZ DE CORRELACIÓN CON VALORES P\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

rcorr_result <- rcorr(as.matrix(df_corr_matrix))
print(rcorr_result)

# 2.4 Gráficos de correlación

cat("\n2.4 GRÁFICOS DE CORRELACIÓN\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

# Crear directorio para gráficos
dir.create("Taller 3/plots", showWarnings = FALSE, recursive = TRUE)

# Gráfico 1: Matriz de dispersión
png("Taller 3/plots/matriz_dispersion.png", 
    width = 2000, height = 2000, res = 300)
ggpairs(df_corr_matrix,
        title = "Matriz de dispersión y correlaciones",
        lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)),
        upper = list(continuous = wrap("cor", size = 4))) +
  theme_minimal()
dev.off()
cat("✓ Gráfico guardado: plots/matriz_dispersion.png\n")

# Gráfico 2: Correlograma
png("Taller 3/plots/correlograma.png", 
    width = 1200, height = 1000, res = 300)
corrplot(cor_pearson, 
         method = "circle",
         type = "upper",
         order = "original",
         tl.cex = 0.8,
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", 
                                   "#77AADD", "#4477AA"))(200),
         title = "Matriz de correlación de Pearson")
dev.off()
cat("✓ Gráfico guardado: plots/correlograma.png\n")

# ==============================================================================
# 3. ANÁLISIS DE REGRESIÓN MÚLTIPLE
# ==============================================================================

cat("\n\n📈 ANÁLISIS DE REGRESIÓN MÚLTIPLE\n")
cat(paste0(rep("=", 61), collapse = ""), "\n")

# 3.1 ANÁLISIS EXPLORATORIO PREVIO

cat("\n3.1 ANÁLISIS EXPLORATORIO PREVIO\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

# Definir variable respuesta y variables explicativas
# Usaremos P3087S1_clean como variable respuesta (valor mensual por prácticas)
# y P3094S3_clean, P3095S3_clean como variables explicativas

df_reg <- df %>%
  select(P3087S1_clean, P3094S3_clean, P3095S3_clean) %>%
  filter(complete.cases(.)) %>%
  filter_all(all_vars(. > 0 | is.na(.))) %>%
  filter(P3087S1_clean > 0, P3094S3_clean > 0, P3095S3_clean > 0)

cat("\n📋 Datos para regresión: n =", nrow(df_reg), "\n")

# Renombrar para claridad
df_reg <- df_reg %>%
  rename(
    Y = P3087S1_clean,           # Variable respuesta: Valor mensual por prácticas
    X1 = P3094S3_clean,          # Variable explicativa 1: Ahorro por cultivar
    X2 = P3095S3_clean           # Variable explicativa 2: Ahorro por criar animales
  )

cat("\nVariables del modelo:\n")
cat("  Y (respuesta): Valor mensual por prácticas o pasantías\n")
cat("  X1 (explicativa): Ahorro por cultivar\n")
cat("  X2 (explicativa): Ahorro por criar animales\n")

# Estadísticos descriptivos
cat("\n📊 Estadísticos descriptivos:\n")
print(summary(df_reg))

# Gráficos exploratorios
cat("\n📈 Generando gráficos exploratorios...\n")

# Histogramas
png("Taller 3/plots/exploratorio_histogramas.png", 
    width = 1800, height = 1200, res = 300)
par(mfrow = c(1, 3))
hist(df_reg$Y, main = "Y: Valor mensual por prácticas", 
     xlab = "Valor", col = "steelblue", breaks = 30)
hist(df_reg$X1, main = "X1: Ahorro por cultivar", 
     xlab = "Valor", col = "darkgreen", breaks = 30)
hist(df_reg$X2, main = "X2: Ahorro por criar animales", 
     xlab = "Valor", col = "darkorange", breaks = 30)
par(mfrow = c(1, 1))
dev.off()
cat("✓ Gráfico guardado: plots/exploratorio_histogramas.png\n")

# Diagramas de dispersión
png("Taller 3/plots/exploratorio_dispersion.png", 
    width = 1600, height = 800, res = 300)
par(mfrow = c(1, 2))
plot(df_reg$X1, df_reg$Y, 
     main = "Y vs X1: Ahorro por cultivar",
     xlab = "Ahorro por cultivar", ylab = "Valor mensual por prácticas",
     pch = 19, col = alpha("steelblue", 0.3))
abline(lm(Y ~ X1, data = df_reg), col = "red", lwd = 2)

plot(df_reg$X2, df_reg$Y, 
     main = "Y vs X2: Ahorro por criar animales",
     xlab = "Ahorro por criar animales", ylab = "Valor mensual por prácticas",
     pch = 19, col = alpha("darkorange", 0.3))
abline(lm(Y ~ X2, data = df_reg), col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()
cat("✓ Gráfico guardado: plots/exploratorio_dispersion.png\n")

# Análisis de valores atípicos
cat("\n🔍 Análisis de valores atípicos:\n")
Q1_Y <- quantile(df_reg$Y, 0.25)
Q3_Y <- quantile(df_reg$Y, 0.75)
IQR_Y <- Q3_Y - Q1_Y
atipicos_Y <- sum(df_reg$Y < (Q1_Y - 1.5*IQR_Y) | 
                  df_reg$Y > (Q3_Y + 1.5*IQR_Y))
cat("Valores atípicos en Y (método IQR):", atipicos_Y, "\n")

Q1_X1 <- quantile(df_reg$X1, 0.25)
Q3_X1 <- quantile(df_reg$X1, 0.75)
IQR_X1 <- Q3_X1 - Q1_X1
atipicos_X1 <- sum(df_reg$X1 < (Q1_X1 - 1.5*IQR_X1) | 
                   df_reg$X1 > (Q3_X1 + 1.5*IQR_X1))
cat("Valores atípicos en X1 (método IQR):", atipicos_X1, "\n")

Q1_X2 <- quantile(df_reg$X2, 0.25)
Q3_X2 <- quantile(df_reg$X2, 0.75)
IQR_X2 <- Q3_X2 - Q1_X2
atipicos_X2 <- sum(df_reg$X2 < (Q1_X2 - 1.5*IQR_X2) | 
                   df_reg$X2 > (Q3_X2 + 1.5*IQR_X2))
cat("Valores atípicos en X2 (método IQR):", atipicos_X2, "\n")

# 3.2 EXPECTATIVAS DEL MODELO

cat("\n3.2 EXPECTATIVAS DEL MODELO\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nCon base en el análisis exploratorio, se espera:\n")
cat("1. Una relación positiva entre el valor mensual por prácticas (Y) y\n")
cat("   el ahorro por cultivar (X1), dado que ambas representan ingresos\n")
cat("   o recursos económicos.\n")
cat("2. Una relación positiva entre el valor mensual por prácticas (Y) y\n")
cat("   el ahorro por criar animales (X2), por la misma razón.\n")
cat("3. Que ambas variables explicativas aporten información relevante\n")
cat("   para explicar la variabilidad en el valor mensual por prácticas.\n")
cat("4. Posible multicolinealidad entre X1 y X2 si ambas miden conceptos\n")
cat("   económicos similares, aunque representan fuentes diferentes.\n")

# 3.3 MODELO DE REGRESIÓN

cat("\n3.3 MODELO DE REGRESIÓN MÚLTIPLE\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

# Plantear modelo formalmente
cat("\nModelo de regresión múltiple:\n")
cat("Y = β₀ + β₁X₁ + β₂X₂ + ε\n")
cat("\nDonde:\n")
cat("  Y = Valor mensual por prácticas o pasantías\n")
cat("  X₁ = Ahorro por cultivar\n")
cat("  X₂ = Ahorro por criar animales\n")
cat("  β₀ = Intercepto\n")
cat("  β₁ = Coeficiente de regresión para X₁\n")
cat("  β₂ = Coeficiente de regresión para X₂\n")
cat("  ε = Término de error\n")

# Estimar el modelo
cat("\n📊 Estimando el modelo...\n")
modelo <- lm(Y ~ X1 + X2, data = df_reg)
summary_modelo <- summary(modelo)

print(summary_modelo)

# 3.3.1 Significancia de coeficientes individuales

cat("\n3.3.1 PRUEBAS DE SIGNIFICANCIA DE COEFICIENTES INDIVIDUALES\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

coef_table <- summary_modelo$coefficients

cat("\nPara cada coeficiente βᵢ (i = 0, 1, 2):\n")
cat("H₀: βᵢ = 0 (el coeficiente no es significativo)\n")
cat("H₁: βᵢ ≠ 0 (el coeficiente es significativo)\n")

for (i in 1:nrow(coef_table)) {
  coef_name <- rownames(coef_table)[i]
  coef_est <- coef_table[i, "Estimate"]
  std_error <- coef_table[i, "Std. Error"]
  t_stat <- coef_table[i, "t value"]
  p_val <- coef_table[i, "Pr(>|t|)"]
  
  cat("\n", coef_name, ":\n", sep = "")
  cat("  Estimación:", round(coef_est, 4), "\n")
  cat("  Error estándar:", round(std_error, 4), "\n")
  cat("  Estadístico t:", round(t_stat, 4), "\n")
  cat("  Valor-p:", format(p_val, scientific = TRUE), "\n")
  
  if (p_val < 0.05) {
    cat("  Decisión: Rechazar H₀. El coeficiente es significativo.\n")
  } else {
    cat("  Decisión: No rechazar H₀. El coeficiente no es significativo.\n")
  }
}

# 3.3.2 Significancia global del modelo

cat("\n\n3.3.2 PRUEBA DE SIGNIFICANCIA GLOBAL DEL MODELO\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nH₀: β₁ = β₂ = 0 (ninguna variable explicativa es útil)\n")
cat("H₁: Al menos un βᵢ ≠ 0 (al menos una variable explicativa es útil)\n")

F_stat <- summary_modelo$fstatistic[1]
F_df1 <- summary_modelo$fstatistic[2]
F_df2 <- summary_modelo$fstatistic[3]
F_pval <- pf(F_stat, F_df1, F_df2, lower.tail = FALSE)

cat("\nEstadístico F:", round(F_stat, 4), "\n")
cat("Grados de libertad (numerator):", F_df1, "\n")
cat("Grados de libertad (denominator):", F_df2, "\n")
cat("Valor-p:", format(F_pval, scientific = TRUE), "\n")

if (F_pval < 0.05) {
  cat("\nDecisión: Rechazar H₀. El modelo es significativo globalmente.\n")
  cat("Al menos una variable explicativa aporta información relevante.\n")
} else {
  cat("\nDecisión: No rechazar H₀. El modelo no es significativo globalmente.\n")
}

# R² y R² ajustado
cat("\nR² (coeficiente de determinación):", 
    round(summary_modelo$r.squared, 4), "\n")
cat("R² ajustado:", round(summary_modelo$adj.r.squared, 4), "\n")
cat("Error estándar residual:", round(summary_modelo$sigma, 2), "\n")

# 3.3.3 Análisis de varianza (ANOVA)

cat("\n\n3.3.3 ANÁLISIS DE VARIANZA (ANOVA)\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nH₀: Todos los coeficientes de regresión son cero (modelo no útil)\n")
cat("H₁: Al menos un coeficiente de regresión es diferente de cero\n")

anova_table <- anova(modelo)
print(anova_table)

cat("\nInterpretación:\n")
cat("La tabla ANOVA descompone la variabilidad total en:\n")
cat("- Variabilidad explicada por el modelo (Sum Sq Model)\n")
cat("- Variabilidad no explicada (Sum Sq Residual)\n")
cat("Si el valor-p de la prueba F es menor que 0.05, rechazamos H₀.\n")

# 3.4 VALIDACIÓN DE SUPUESTOS

cat("\n\n3.4 VALIDACIÓN DE SUPUESTOS DEL MODELO\n")
cat(paste0(rep("=", 61), collapse = ""), "\n")

# Extraer residuos
residuos <- residuals(modelo)
residuos_estandarizados <- rstandard(modelo)
valores_ajustados <- fitted(modelo)

# 3.4.1 Linealidad

cat("\n3.4.1 SUPUESTO DE LINEALIDAD\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nH₀: La relación entre variables es lineal\n")
cat("H₁: La relación entre variables no es lineal\n")

# Gráfico de residuos vs valores ajustados
png("Taller 3/plots/validacion_linealidad.png", 
    width = 1600, height = 800, res = 300)
par(mfrow = c(1, 2))
plot(valores_ajustados, residuos_estandarizados,
     main = "Residuos estandarizados vs Valores ajustados",
     xlab = "Valores ajustados", ylab = "Residuos estandarizados",
     pch = 19, col = alpha("steelblue", 0.5))
abline(h = 0, col = "red", lwd = 2)
lines(lowess(valores_ajustados, residuos_estandarizados), 
      col = "darkgreen", lwd = 2)

plot(valores_ajustados, residuos,
     main = "Residuos vs Valores ajustados",
     xlab = "Valores ajustados", ylab = "Residuos",
     pch = 19, col = alpha("darkorange", 0.5))
abline(h = 0, col = "red", lwd = 2)
par(mfrow = c(1, 1))
dev.off()
cat("✓ Gráfico guardado: plots/validacion_linealidad.png\n")

cat("\nInterpretación: Si el gráfico muestra un patrón aleatorio alrededor de cero,\n")
cat("no hay evidencia de no linealidad. Si hay un patrón sistemático (curva),\n")
cat("podría indicar no linealidad.\n")

# 3.4.2 Independencia

cat("\n\n3.4.2 SUPUESTO DE INDEPENDENCIA\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nH₀: Los residuos son independientes (no hay autocorrelación)\n")
cat("H₁: Los residuos no son independientes (hay autocorrelación)\n")

# Prueba de Durbin-Watson
dw_test <- durbinWatsonTest(modelo)

cat("\nPrueba de Durbin-Watson:\n")
cat("Estadístico DW:", round(dw_test$dw, 4), "\n")
cat("Valor-p:", format(dw_test$p, scientific = TRUE), "\n")

if (dw_test$p < 0.05) {
  cat("Decisión: Rechazar H₀. Hay evidencia de autocorrelación.\n")
} else {
  cat("Decisión: No rechazar H₀. No hay evidencia de autocorrelación.\n")
}

# Gráfico de residuos vs orden
png("Taller 3/plots/validacion_independencia.png", 
    width = 1200, height = 800, res = 300)
plot(1:length(residuos_estandarizados), residuos_estandarizados,
     type = "l", main = "Residuos estandarizados vs Orden",
     xlab = "Orden de observación", ylab = "Residuos estandarizados",
     col = "steelblue")
abline(h = 0, col = "red", lwd = 2)
points(1:length(residuos_estandarizados), residuos_estandarizados,
       pch = 19, col = alpha("steelblue", 0.5))
dev.off()
cat("✓ Gráfico guardado: plots/validacion_independencia.png\n")

# 3.4.3 Homocedasticidad

cat("\n\n3.4.3 SUPUESTO DE HOMOCEDASTICIDAD\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nH₀: Varianza constante de los errores (homocedasticidad)\n")
cat("H₁: Varianza no constante de los errores (heterocedasticidad)\n")

# Prueba de Breusch-Pagan
bp_test <- bptest(modelo)

cat("\nPrueba de Breusch-Pagan:\n")
cat("Estadístico LM:", round(bp_test$statistic, 4), "\n")
cat("Valor-p:", format(bp_test$p.value, scientific = TRUE), "\n")

if (bp_test$p.value < 0.05) {
  cat("Decisión: Rechazar H₀. Hay evidencia de heterocedasticidad.\n")
} else {
  cat("Decisión: No rechazar H₀. No hay evidencia de heterocedasticidad.\n")
}

# Gráfico para homocedasticidad
png("Taller 3/plots/validacion_homocedasticidad.png", 
    width = 2400, height = 1600, res = 300, bg = "white")

# Configurar márgenes más amplios
par(mar = c(5, 5.5, 4.5, 2) + 0.1,
    cex.axis = 1.2,
    cex.lab = 1.4,
    cex.main = 1.5,
    font.main = 2,
    col.main = "#2c3e50",
    col.lab = "#34495e",
    mgp = c(3.5, 1, 0))

# Crear el gráfico
plot(valores_ajustados, sqrt(abs(residuos_estandarizados)),
     main = "Gráfico de Escala-Localización\n(Validación de Homocedasticidad)",
     xlab = "Valores ajustados (COP)", 
     ylab = "sqrt(|Residuos estandarizados|)",
     pch = 19, 
     col = scales::alpha("#3498db", 0.4),
     cex = 0.8,
     xaxt = "n",
     yaxt = "n")

# Agregar ejes personalizados
axis(1, cex.axis = 1.2, las = 1)
axis(2, cex.axis = 1.2, las = 1)

# Agregar grid
grid(col = "#ecf0f1", lty = "solid", lwd = 0.8)

# Agregar línea de suavizado
lines(lowess(valores_ajustados, sqrt(abs(residuos_estandarizados))), 
      col = "#e74c3c", lwd = 3)

# Restaurar parámetros
par(mgp = c(3, 1, 0))
dev.off()
cat("✓ Gráfico guardado: plots/validacion_homocedasticidad.png\n")

# 3.4.4 Normalidad de los errores

cat("\n\n3.4.4 SUPUESTO DE NORMALIDAD DE LOS ERRORES\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nH₀: Los errores siguen una distribución normal\n")
cat("H₁: Los errores no siguen una distribución normal\n")

# Prueba de Shapiro-Wilk (para muestras pequeñas) o Kolmogorov-Smirnov
n <- length(residuos_estandarizados)
if (n <= 5000) {
  # Shapiro-Wilk para muestras pequeñas
  sw_test <- shapiro.test(residuos_estandarizados)
  cat("\nPrueba de Shapiro-Wilk:\n")
  cat("Estadístico W:", round(sw_test$statistic, 4), "\n")
  cat("Valor-p:", format(sw_test$p.value, scientific = TRUE), "\n")
  
  if (sw_test$p.value < 0.05) {
    cat("Decisión: Rechazar H₀. Los residuos no siguen distribución normal.\n")
  } else {
    cat("Decisión: No rechazar H₀. Los residuos siguen distribución normal.\n")
  }
  test_normalidad <- sw_test
  nombre_test <- "Shapiro-Wilk"
} else {
  # Kolmogorov-Smirnov para muestras grandes
  ks_test <- ks.test(residuos_estandarizados, "pnorm")
  cat("\nPrueba de Kolmogorov-Smirnov:\n")
  cat("Estadístico D:", round(ks_test$statistic, 4), "\n")
  cat("Valor-p:", format(ks_test$p.value, scientific = TRUE), "\n")
  
  if (ks_test$p.value < 0.05) {
    cat("Decisión: Rechazar H₀. Los residuos no siguen distribución normal.\n")
  } else {
    cat("Decisión: No rechazar H₀. Los residuos siguen distribución normal.\n")
  }
  test_normalidad <- ks_test
  nombre_test <- "Kolmogorov-Smirnov"
}

# Gráficos de normalidad
png("Taller 3/plots/validacion_normalidad.png", 
    width = 1600, height = 800, res = 300)
par(mfrow = c(1, 2))
# Q-Q plot
qqnorm(residuos_estandarizados, main = "Q-Q Plot de residuos estandarizados",
       pch = 19, col = alpha("steelblue", 0.5))
qqline(residuos_estandarizados, col = "red", lwd = 2)

# Histograma con curva normal superpuesta
hist(residuos_estandarizados, prob = TRUE,
     main = "Histograma de residuos estandarizados",
     xlab = "Residuos estandarizados",
     col = "lightblue", breaks = 30)
curve(dnorm(x, mean = mean(residuos_estandarizados), 
            sd = sd(residuos_estandarizados)),
      col = "red", lwd = 2, add = TRUE)
par(mfrow = c(1, 1))
dev.off()
cat("✓ Gráfico guardado: plots/validacion_normalidad.png\n")

# 3.4.5 Tabla resumen de validación de supuestos

cat("\n\n3.4.5 TABLA RESUMEN DE VALIDACIÓN DE SUPUESTOS\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

# Crear tabla resumen
tabla_supuestos <- data.frame(
  Supuesto = c(
    "Linealidad",
    "Independencia",
    "Homocedasticidad",
    "Normalidad de errores"
  ),
  Prueba = c(
    "Inspección gráfica",
    "Durbin-Watson",
    "Breusch-Pagan",
    nombre_test
  ),
  Estadistico = c(
    "N/A (gráfico)",
    round(dw_test$dw, 4),
    round(bp_test$statistic, 4),
    round(ifelse(nombre_test == "Shapiro-Wilk", 
                 test_normalidad$statistic, 
                 test_normalidad$statistic), 4)
  ),
  Valor_p = c(
    "N/A",
    format(dw_test$p, scientific = TRUE, digits = 3),
    format(bp_test$p.value, scientific = TRUE, digits = 3),
    format(ifelse(nombre_test == "Shapiro-Wilk",
                  test_normalidad$p.value,
                  test_normalidad$p.value), 
           scientific = TRUE, digits = 3)
  ),
  Decision = c(
    "Evaluar gráficamente",
    ifelse(dw_test$p < 0.05, "Rechazar H₀", "No rechazar H₀"),
    ifelse(bp_test$p.value < 0.05, "Rechazar H₀", "No rechazar H₀"),
    ifelse((ifelse(nombre_test == "Shapiro-Wilk",
                   test_normalidad$p.value,
                   test_normalidad$p.value)) < 0.05,
           "Rechazar H₀", "No rechazar H₀")
  ),
  Cumplimiento = c(
    "Evaluar gráficamente",
    ifelse(dw_test$p >= 0.05, "Sí", "No"),
    ifelse(bp_test$p.value >= 0.05, "Sí", "No"),
    ifelse((ifelse(nombre_test == "Shapiro-Wilk",
                   test_normalidad$p.value,
                   test_normalidad$p.value)) >= 0.05,
           "Sí", "No")
  )
)

print(tabla_supuestos)

# Guardar tabla
write.csv(tabla_supuestos, "Taller 3/tabla_validacion_supuestos.csv", 
          row.names = FALSE)
cat("\n✓ Tabla guardada: tabla_validacion_supuestos.csv\n")

# Comentarios sobre cumplimiento de supuestos
cat("\nComentarios sobre cumplimiento de supuestos:\n")
cat("1. Linealidad: Evaluar visualmente el gráfico de residuos vs valores ajustados.\n")
cat("   Un patrón aleatorio indica cumplimiento del supuesto.\n")
cat("2. Independencia: Prueba Durbin-Watson evaluada con p < 0.05.\n")
cat("3. Homocedasticidad: Prueba Breusch-Pagan evaluada con p < 0.05.\n")
cat("4. Normalidad: Prueba", nombre_test, "evaluada con p < 0.05.\n")

# 3.5 TRANSFORMACIONES (SI APLICA)

cat("\n\n3.5 TRANSFORMACIONES\n")
cat(paste0(rep("-", 61), collapse = ""), "\n")

cat("\nEvaluando necesidad de transformaciones...\n")

# Verificar si hay violaciones de supuestos que justifiquen transformaciones
violaciones <- sum(
  dw_test$p < 0.05,           # Independencia
  bp_test$p.value < 0.05,     # Homocedasticidad
  (ifelse(nombre_test == "Shapiro-Wilk",
          test_normalidad$p.value,
          test_normalidad$p.value)) < 0.05  # Normalidad
)

if (violaciones > 0) {
  cat("\n⚠️ Se detectaron", violaciones, "violaciones de supuestos.\n")
  cat("Considerando transformaciones...\n")
  
  # Transformación logarítmica para normalizar y reducir heterocedasticidad
  cat("\nOpción 1: Transformación logarítmica\n")
  cat("Justificación: Las variables económicas suelen tener distribuciones\n")
  cat("sesgadas que se normalizan con transformación logarítmica.\n")
  
  # Crear variables transformadas (log1p para manejar ceros)
  df_reg_transf <- df_reg %>%
    mutate(
      log_Y = log1p(Y),
      log_X1 = log1p(X1),
      log_X2 = log1p(X2)
    )
  
  # Estimar modelo transformado
  modelo_transf <- lm(log_Y ~ log_X1 + log_X2, data = df_reg_transf)
  
  cat("\nModelo transformado: log(Y+1) = β₀ + β₁log(X₁+1) + β₂log(X₂+1) + ε\n")
  
  # Validar supuestos del modelo transformado
  residuos_transf <- rstandard(modelo_transf)
  
  # Breusch-Pagan en modelo transformado
  bp_transf <- bptest(modelo_transf)
  cat("\nBreusch-Pagan (modelo transformado):\n")
  cat("Valor-p:", format(bp_transf$p.value, scientific = TRUE), "\n")
  
  # Normalidad en modelo transformado
  if (n <= 5000) {
    sw_transf <- shapiro.test(residuos_transf)
    cat("Shapiro-Wilk (modelo transformado):\n")
    cat("Valor-p:", format(sw_transf$p.value, scientific = TRUE), "\n")
  }
  
  cat("\nDecisión técnica:\n")
  if (bp_transf$p.value >= 0.05 && 
      (ifelse(n <= 5000, sw_transf$p.value, ks.test(residuos_transf, "pnorm")$p.value)) >= 0.05) {
    cat("✓ La transformación logarítmica mejora el cumplimiento de supuestos.\n")
    cat("  Se recomienda usar el modelo transformado.\n")
  } else {
    cat("✗ La transformación logarítmica no mejora significativamente los supuestos.\n")
    cat("  Considerar otras transformaciones o técnicas robustas.\n")
  }
  
} else {
  cat("\n✓ No se detectaron violaciones graves de supuestos.\n")
  cat("  No se requiere transformación en este momento.\n")
  cat("  El modelo original es adecuado para el análisis.\n")
}

# ==============================================================================
# 4. EXPORTACIÓN DE RESULTADOS
# ==============================================================================

cat("\n\n💾 EXPORTACIÓN DE RESULTADOS\n")
cat(paste0(rep("=", 61), collapse = ""), "\n")

# Guardar matriz de coeficientes del modelo
coef_matrix <- coef_table
write.csv(coef_matrix, "Taller 3/matriz_coeficientes_modelo.csv")
cat("✓ Matriz de coeficientes guardada: matriz_coeficientes_modelo.csv\n")

# Guardar resumen del modelo
sink("Taller 3/resumen_modelo.txt")
cat("RESUMEN DEL MODELO DE REGRESIÓN MÚLTIPLE\n")
cat(strrep("=", 60), "\n\n")
print(summary_modelo)
sink()
cat("✓ Resumen del modelo guardado: resumen_modelo.txt\n")

# Guardar ANOVA
write.csv(anova_table, "Taller 3/anova_modelo.csv")
cat("✓ Tabla ANOVA guardada: anova_modelo.csv\n")

# Guardar resultados de correlación
if (length(resultados_correlacion) > 0) {
  corr_results_df <- do.call(rbind, lapply(resultados_correlacion, 
                                            function(x) {
                                              if (!is.null(x)) {
                                                data.frame(
                                                  Variable1 = x$variable1,
                                                  Variable2 = x$variable2,
                                                  Coeficiente_r = round(x$r, 4),
                                                  Valor_p = format(x$p_value, 
                                                                   scientific = TRUE),
                                                  Decision = x$decision,
                                                  Fuerza = x$fuerza,
                                                  Direccion = x$direccion
                                                )
                                              }
                                            }))
  write.csv(corr_results_df, "Taller 3/resultados_correlacion.csv", 
            row.names = FALSE)
  cat("✓ Resultados de correlación guardados: resultados_correlacion.csv\n")
}

cat("\n✅ Análisis completo finalizado\n")
cat("Todos los resultados y gráficos han sido guardados en la carpeta 'Taller 3/'\n")

# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================

