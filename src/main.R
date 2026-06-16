# ══════════════════════════════════════════════════════════════════════════════
# bunching_5MM.R
# Análisis de bunching en el notch S/5,000,000
# Directiva CGR sobre obras por Administración Directa (vigencia 1-jun-2024)
#
# Estructura:
#   0. Paquetes y parámetros
#   1. Carga y limpieza
#   2. DiD del share "justo debajo del corte"
#   3. Estimación del contrafactual (polinomio local)
#   4. Exceso de masa B̂ y masa faltante M̂ con bootstrap
#   5. Test de discontinuidad de densidad (rddensity / McCrary-Cattaneo)
#   6. Figura 1 — Histograma + contrafactual (AD pre vs post)
#   7. Figura 2 — Placebo Contrata
#   8. Figura 3 — DiD del share
#   9. Tabla resumen
#
# Paquetes requeridos (CRAN):
#   tidyverse, readxl, rddensity, rdrobust, broom, sysfonts, showtext,
#   strucchange, bcp, sandwich, lmtest
# ══════════════════════════════════════════════════════════════════════════════


# ── 0. PAQUETES Y PARÁMETROS ─────────────────────────────────────────────────

pkgs <- c("tidyverse", "readxl", "rddensity", "rdrobust", "broom", "tidyplots",
          "strucchange", "bcp", "sandwich", "lmtest", "lubridate")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ── Rutas ────────────────────────────────────────────────────────────────────
XLSX_PATH     <- "data/raw/DataSet-Obras-Publicas 15-06-2026.xlsx"
FIG_DIR       <- "figures"
DATA_PROC_DIR <- "data/processed"

dir.create(FIG_DIR,       showWarnings = FALSE)
dir.create(DATA_PROC_DIR, showWarnings = FALSE)

# ── Parámetros del análisis ───────────────────────────────────────────────────
NOTCH    <- 5e6          # S/5,000,000
BIN_W    <- 100e3        # ancho de bin: S/100k
LO       <- 2e6          # límite inferior del análisis
HI       <- 8e6          # límite superior del análisis
EXCL_LO  <- 4.5e6        # banda de exclusión para el polinomio (izquierda)
EXCL_HI  <- 5.5e6        # banda de exclusión para el polinomio (derecha)
POLY_DEG <- 4            # grado del polinomio contrafactual
N_BOOT   <- 2000         # iteraciones bootstrap
SEED     <- 42

DATE_ANN <- as.Date("2023-12-23")   # anuncio de la directiva
DATE_VIG <- as.Date("2024-06-01")   # vigencia efectiva


# ── 1. CARGA Y LIMPIEZA ──────────────────────────────────────────────────────

message("Cargando datos…")

raw <- read_excel(XLSX_PATH, skip = 3, col_names = TRUE)

# Función para parsear montos con separador decimal = espacio
# Ej: "1205287 56" → 1205287.56
parse_monto <- function(x) {
  x <- str_trim(as.character(x))
  # espacio + 1-2 dígitos al final = decimales
  x <- str_replace(x, "\\s+(\\d{1,2})$", ".\\1")
  # eliminar espacios restantes (separadores de miles)
  x <- str_remove_all(x, "\\s")
  suppressWarnings(as.numeric(x))
}

# Seleccionar y renombrar columnas por posición (el header tiene filas de metadata)
df <- raw |>
  select(
    mod    = 5,        # Modalidad de ejecución
    saldo  = 6,        # ¿Corresponde a un saldo de obra?
    rcc    = 13,       # Marca Reconstrucción con Cambios
    react  = 14,       # Marca Reactivación Económica
    et_raw = 42,       # Monto aprobado en soles (col 42, índice base-1)
    f_et   = 38,       # Fecha de aprobación del expediente técnico
    f_ini  = 64,       # Fecha de inicio de obra
    nat    = 8         # Naturaleza de la obra
  ) |>
  mutate(
    et   = parse_monto(et_raw),
    f_et = dmy(f_et),
    f_ini = dmy(f_ini)
  )

# Universo limpio: AD y Contrata, monto > 0, sin saldo/RCC/reactivación
is_yes <- function(x) str_to_upper(str_trim(x)) %in% c("SI", "SÍ", "YES", "S", "1", "TRUE")

base <- df |>
  filter(
    mod %in% c("Administración directa", "Contrata"),
    !is.na(et), et > 0,
    !is_yes(saldo), !is_yes(rcc), !is_yes(react)
  ) |>
  mutate(
    w = case_when(
      f_et <  DATE_ANN                     ~ "pre",
      f_et >= DATE_ANN & f_et < DATE_VIG  ~ "anuncio",
      f_et >= DATE_VIG                     ~ "post",
      TRUE                                 ~ "sinfecha"
    ),
    w = factor(w, levels = c("pre", "anuncio", "post", "sinfecha"))
  )

message(sprintf("Universo limpio: %d obras (AD + Contrata)", nrow(base)))

ad <- filter(base, mod == "Administración directa")
co <- filter(base, mod == "Contrata")


# ── 2. DiD DEL SHARE DEBAJO DEL CORTE ────────────────────────────────────────
# Banda estrecha [4.5, 5.5]MM alrededor del notch

share_below <- function(df, lo = 4.5e6, hi = 5.5e6, notch = NOTCH) {
  band <- filter(df, et >= lo, et <= hi)
  tibble(share = mean(band$et < notch), n = nrow(band))
}

message("\n── DiD share debajo de S/5MM en banda [4.5–5.5MM] ──")
did_tbl <- bind_rows(
  map_dfr(c("pre","anuncio","post"), ~{
    r <- share_below(filter(ad, w == .x))
    tibble(mod = "AD", ventana = .x, share = r$share, n = r$n)
  }),
  map_dfr(c("pre","anuncio","post"), ~{
    r <- share_below(filter(co, w == .x))
    tibble(mod = "Contrata", ventana = .x, share = r$share, n = r$n)
  })
) |>
  mutate(ventana = factor(ventana, levels = c("pre","anuncio","post")))

print(did_tbl)

did_ad  <- share_below(filter(ad, w=="post"))$share - share_below(filter(ad, w=="pre"))$share
did_co  <- share_below(filter(co, w=="post"))$share - share_below(filter(co, w=="pre"))$share
did_did <- did_ad - did_co

message(sprintf("\n  ΔAD (post−pre)       = %+.3f", did_ad))
message(sprintf("  ΔContrata (post−pre) = %+.3f", did_co))
message(sprintf("  DiD (AD − Contrata)  = %+.3f  ← estimador DiD", did_did))


# ── 3. FUNCIONES NÚCLEO ───────────────────────────────────────────────────────

# Genera bins y conteos en [lo, hi]
make_bins <- function(x, lo = LO, hi = HI, bw = BIN_W) {
  breaks <- seq(lo, hi + bw, by = bw)
  counts <- hist(x[x >= lo & x <= hi], breaks = breaks, plot = FALSE)$counts
  centers <- breaks[-length(breaks)] + bw / 2
  tibble(center = centers, count = counts)
}

# Ajusta polinomio contrafactual excluyendo la banda [excl_lo, excl_hi]
fit_counterfactual <- function(bins, excl_lo = EXCL_LO, excl_hi = EXCL_HI,
                               deg = POLY_DEG, notch = NOTCH) {
  fit_data <- filter(bins, center < excl_lo | center > excl_hi)
  x_c <- (fit_data$center - notch) / 1e6    # centrar y escalar
  y   <- fit_data$count
  X   <- poly(x_c, degree = deg, raw = TRUE)
  mod <- lm(y ~ X)
  # Predicción para todos los bins
  x_all <- (bins$center - notch) / 1e6
  X_all <- predict(poly(x_c, degree = deg, raw = TRUE), newdata = x_all)
  fitted <- pmax(as.numeric(cbind(1, X_all) %*% coef(mod)), 0)
  bins |> mutate(fitted = fitted)
}

# Calcula B̂ (exceso debajo) y M̂ (hueco encima)
compute_bunching <- function(x, excl_lo = EXCL_LO, excl_hi = EXCL_HI,
                             deg = POLY_DEG, notch = NOTCH) {
  bins <- make_bins(x)
  bins <- fit_counterfactual(bins, excl_lo, excl_hi, deg, notch)
  bins <- bins |>
    mutate(
      zone = case_when(
        center >= excl_lo & center <  notch ~ "below",
        center >= notch   & center <  excl_hi ~ "above",
        TRUE ~ "out"
      )
    )
  B_hat  <- sum((bins$count - bins$fitted)[bins$zone == "below"])
  M_hat  <- sum((bins$fitted - bins$count)[bins$zone == "above"])
  list(bins = bins, B_hat = B_hat, M_hat = M_hat, n = length(x))
}

# Bootstrap percentil para IC 95% de B̂ y M̂
bootstrap_bunching <- function(x, n_boot = N_BOOT, seed = SEED, ...) {
  set.seed(seed)
  B_vec <- M_vec <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    samp <- sample(x, length(x), replace = TRUE)
    r <- compute_bunching(samp, ...)
    B_vec[i] <- r$B_hat
    M_vec[i] <- r$M_hat
  }
  list(
    B_ci = quantile(B_vec, c(0.025, 0.975)),
    M_ci = quantile(M_vec, c(0.025, 0.975)),
    B_se = sd(B_vec), M_se = sd(M_vec)
  )
}


# ── 4. CÓMPUTO PRINCIPAL ──────────────────────────────────────────────────────

grupos <- list(
  AD_pre    = list(data = filter(ad, w == "pre",  et >= LO, et <= HI)$et, label = "AD · pre-vigencia"),
  AD_post   = list(data = filter(ad, w == "post", et >= LO, et <= HI)$et, label = "AD · post-vigencia"),
  Cont_pre  = list(data = filter(co, w == "pre",  et >= LO, et <= HI)$et, label = "Contrata · pre"),
  Cont_post = list(data = filter(co, w == "post", et >= LO, et <= HI)$et, label = "Contrata · post")
)

message("\n── Estimando bunching (bootstrap ~30s por grupo) ──")
results <- map(grupos, function(g) {
  r  <- compute_bunching(g$data)
  bs <- bootstrap_bunching(g$data)
  c(r, bs, list(label = g$label))
})


# ── 5. ESTIMADOR PRINCIPAL: RD DE DENSIDAD (Cattaneo-Jansson-Ma) ─────────────
#
# JERARQUÍA DE ESTIMADORES:
#   1. [PRINCIPAL]    RD de densidad (rddensity) corrida por separado en pre y
#                     post. El estimador de interés es el cambio en T entre
#                     ventanas: T_post >> T_pre indica que la discontinuidad en
#                     S/5MM surgió (o se amplió) con la directiva.
#                     Identifica si la densidad de montos tiene un salto en el
#                     notch usando toda la distribución, no una banda arbitraria.
#
#   2. [DESCRIPTIVO]  DiD del share (sección 2). Resume la magnitud en términos
#                     porcentuales comunicables, pero NO es el estimador formal.
#                     Limitación: contrata como placebo puede estar contaminada
#                     por sustitución AD→contrata post-directiva, y no captura
#                     sustitución hacia OxI / núcleo ejecutor / otras modalidades
#                     de administración indirecta.
#
#   3. [MECANISMO]    Polinomio contrafactual + B̂/M̂ (sección 3-4). Cuantifica
#                     el exceso de masa y la masa faltante alrededor del notch.
#                     Complementa la RD con una medida de magnitud económica.
#
# SUPUESTO CLAVE de la RD de densidad: la densidad de montos sería suave
# (continua) en S/5MM en ausencia del notch. Se valida comparando pre vs post:
# si T_pre ≈ 0 y T_post >> 0, el redondeo administrativo preexistente no
# explica la discontinuidad post-vigencia.

run_rddensity <- function(x, label) {
  if (length(x) < 30) {
    message(sprintf("  %s: n insuficiente (%d), omitido", label, length(x)))
    return(NULL)
  }
  tryCatch({
    rd <- rddensity(X = x, c = NOTCH)
    list(
      T_stat = rd$test$t_jk,
      p      = rd$test$p_jk,
      bw_l   = rd$h$left,
      bw_r   = rd$h$right,
      n_l    = rd$N$left,
      n_r    = rd$N$right
    )
  }, error = function(e) {
    message(sprintf("  %s: rddensity error — %s", label, conditionMessage(e)))
    NULL
  })
}

message("\n── ESTIMADOR PRINCIPAL: RD de densidad (Cattaneo) por ventana ──")
rd_results <- imap(grupos, \(g, nm) run_rddensity(g$data, g$label))

walk2(rd_results, grupos, function(r, g) {
  if (is.null(r)) return(invisible())
  p_str <- if (r$p < 0.001) "<0.001" else sprintf("%.3f", r$p)
  message(sprintf("  %-28s  T=%6.3f  p=%s  bw=[%.0fk,%.0fk]  n=[%d,%d]",
                  g$label, r$T_stat, p_str,
                  r$bw_l/1e3, r$bw_r/1e3, r$n_l, r$n_r))
})

# Diferencia de T entre post y pre (AD): cambio en la discontinuidad
T_ad_pre  <- rd_results$AD_pre$T_stat  %||% NA_real_
T_ad_post <- rd_results$AD_post$T_stat %||% NA_real_
delta_T   <- T_ad_post - T_ad_pre
message(sprintf(
  "\n  Delta T (AD post − AD pre) = %.3f  [T_pre=%.3f, T_post=%.3f]",
  delta_T, T_ad_pre, T_ad_post
))
message("  Interpretación: un Delta T positivo y grande indica que la")
message("  discontinuidad de densidad en S/5MM es mayor post-vigencia,")
message("  consistente con bunching estratégico inducido por la directiva.")


# ── 6. FIGURA 1 — Histograma + contrafactual (AD pre vs post) ────────────────
# tidyplots no soporta geom_line sobre el mismo panel de barras directamente,
# así que construimos el data.frame de plot y usamos add_bar + add_line.
# El contrafactual y la línea del notch se añaden como capas separadas con
# adjust_*  y los labels con add_title / add_caption.

make_bunching_fig <- function(res, rd_res, titulo, filename, mod_label = "AD") {
  bins <- res$bins |>
    mutate(
      monto_MM = center / 1e6,
      zona = case_when(
        zone == "below" ~ "Exceso (debajo del notch)",
        zone == "above" ~ "Hueco (encima del notch)",
        TRUE            ~ "Fuera de banda"
      )
    )
  
  # Etiqueta rddensity para el caption
  rd_str <- if (!is.null(rd_res)) {
    p_str <- if (rd_res$p < 0.001) "p<0.001" else sprintf("p=%.3f", rd_res$p)
    sprintf("Test discontinuidad densidad (rddensity): T=%.2f, %s", rd_res$T_stat, p_str)
  } else "rddensity: n insuficiente"
  
  b_str <- sprintf("B\u0302 = %+.0f  [IC95%%: %.0f, %.0f]", res$B_hat, res$B_ci[1], res$B_ci[2])
  m_str <- sprintf("M\u0302 = %+.0f  [IC95%%: %.0f, %.0f]", res$M_hat, res$M_ci[1], res$M_ci[2])
  
  # tidyplot con barras coloreadas por zona + línea del contrafactual
  p <- bins |>
    tidyplot(x = monto_MM, y = count, color = zona) |>
    add_sum_bar(
      width       = BIN_W / 1e6 * 0.88,
      alpha       = 0.85
    )
  
  p <- p +
    ggplot2::geom_line(
      data        = bins,
      mapping     = aes(x = monto_MM, y = fitted),
      color       = "firebrick",
      linetype    = "dashed",
      linewidth   = 0.9,
      inherit.aes = FALSE
    )
  
  p <- p |>
    add_reference_lines(x = NOTCH / 1e6, color = "firebrick", linewidth = 0.8) |>
    adjust_colors(
      new_colors = c(
        "Exceso (debajo del notch)" = "#4E8098",
        "Hueco (encima del notch)"  = "#B0C9D3",
        "Fuera de banda"            = "#7A9EB0"
      )
    ) |>
    adjust_x_axis(
      limits = c(LO / 1e6 - 0.1, HI / 1e6 + 0.1),
      labels = \(x) sprintf("S/%gMM", x)
    ) |>
    adjust_y_axis(title = sprintf("N\u00b0 de obras (%s)", mod_label)) |>
    add_title(titulo) |>
    add_caption(paste0(
      "n = ", res$n, " obras  |  ", b_str, "  |  ", m_str, "\n", rd_str
    )) |>
    adjust_legend_title("Zona respecto al notch") |>
    theme_tidyplot()
  
  save_plot(p, filename = file.path(FIG_DIR, filename),
                width = 8, height = 5, units = "in")
  message(sprintf("Guardado: %s", filename))
  return(p)
}

p1a <- make_bunching_fig(
  results$AD_pre,  rd_results$AD_pre,
  titulo   = "Bunching S/5MM — AD pre-vigencia (hasta may-2024)",
  filename = "fig1a_bunching_AD_pre.png"
) %>%
  adjust_size(width = NA, height = NA)

p1b <- make_bunching_fig(
  results$AD_post, rd_results$AD_post,
  titulo   = "Bunching S/5MM — AD post-vigencia (desde jun-2024)",
  filename = "fig1b_bunching_AD_post.png"
) %>%
  adjust_size(width = NA, height = NA)


# ── 7. FIGURA 2 — Placebo Contrata ───────────────────────────────────────────

p2a <- make_bunching_fig(
  results$Cont_pre,  rd_results$Cont_pre,
  titulo    = "Placebo Contrata — pre-vigencia (sin bunching esperado)",
  filename  = "fig2a_bunching_contrata_pre.png",
  mod_label = "Contrata"
) %>%
  adjust_size(width = NA, height = NA)

p2b <- make_bunching_fig(
  results$Cont_post, rd_results$Cont_post,
  titulo    = "Placebo Contrata — post-vigencia (sin bunching esperado)",
  filename  = "fig2b_bunching_contrata_post.png",
  mod_label = "Contrata"
) %>%
  adjust_size(width = NA, height = NA)


# ── PANEL 4-GRAFICOS ─────────────────────────────────────────────────────────

message("\n── Generando panel de 4 gráficos ──")

# Combinar datos de bins para faceting
df_panel <- bind_rows(
  results$AD_pre$bins    |> mutate(mod = "AD",       ventana = "Pre-vigencia"),
  results$AD_post$bins   |> mutate(mod = "AD",       ventana = "Post-vigencia"),
  results$Cont_pre$bins  |> mutate(mod = "Contrata", ventana = "Pre-vigencia"),
  results$Cont_post$bins |> mutate(mod = "Contrata", ventana = "Post-vigencia")
) |>
  mutate(
    monto_MM = center / 1e6,
    zona = case_when(
      zone == "below" ~ "Exceso (debajo del notch)",
      zone == "above" ~ "Hueco (encima del notch)",
      TRUE            ~ "Fuera de banda"
    ),
    # Forzar orden de facetas
    mod = factor(mod, levels = c("AD", "Contrata")),
    ventana = factor(ventana, levels = c("Pre-vigencia", "Post-vigencia")),
    facet_var = interaction(mod, ventana, sep = " · ")
  )

fig_panel <- df_panel |>
  tidyplot(x = monto_MM, y = count, color = zona)

# Añadir línea ANTES de las barras para evitar problemas de dimensiones internas de tidyplots
fig_panel <- fig_panel +
  ggplot2::geom_line(
    mapping     = aes(y = fitted, group = interaction(mod, ventana)),
    color       = "firebrick",
    linetype    = "dashed",
    linewidth   = 0.7
  )

fig_panel <- fig_panel |>
  add_sum_bar(width = BIN_W / 1e6 * 0.88, alpha = 0.85) |>
  split_plot(by = facet_var, ncol = 2) |>
  add_reference_lines(x = NOTCH / 1e6, color = "firebrick", linewidth = 0.8) |>
  adjust_colors(
    new_colors = c(
      "Exceso (debajo del notch)" = "#4E8098",
      "Hueco (encima del notch)"  = "#B0C9D3",
      "Fuera de banda"            = "#7A9EB0"
    )
  ) |>
  adjust_x_axis(
    limits = c(LO / 1e6 - 0.1, HI / 1e6 + 0.1),
    labels = \(x) sprintf("S/%gMM", x)
  ) |>
  adjust_y_axis(title = "N° de obras") |>
  add_title("Análisis de Bunching en S/5MM: AD vs Placebo Contrata") |>
  adjust_legend_title("Zona respecto al notch")

save_plot(fig_panel, filename = file.path(FIG_DIR, "fig4_panel_bunching.png"),
          width = 10, height = 7, units = "in")
message("Panel de 4 gráficos guardado: fig4_panel_bunching.png")


# ── 8. FIGURA 3 — DiD del share debajo del corte ─────────────────────────────

did_tbl_plot <- did_tbl |>
  filter(ventana %in% c("pre", "anuncio", "post")) |>
  mutate(
    ventana = factor(ventana,
                     levels = c("pre", "anuncio", "post"),
                     labels = c("Pre (<dic-2023)",
                                "Anuncio (dic-23 a may-24)",
                                "Post (>=jun-2024)")),
    share_pct = share * 100
  ) 

fig3 <- did_tbl_plot |>
  tidyplot(x = ventana, y = share_pct, color = mod) |>
  add_mean_line(linewidth = 1.1) |>
  add_mean_dot(size = 3) |>
  add_data_labels_repel(
    label = sprintf("%.0f%%\n(n=%d)", share_pct, n),
    size  = 3
  ) |>
  adjust_colors(new_colors = c("AD" = "#2E7D6E", "Contrata" = "#888888")) |>
  adjust_y_axis(
    title  = "Share de obras con monto < S/5MM (%)\n(banda [4.5–5.5MM])",
    limits = c(40, 100)
  ) |>
  adjust_x_axis(title = "Ventana temporal") |>
  add_title("Share de obras < S/5MM por modalidad y ventana [estadístico descriptivo]") |>
  add_caption(sprintf(
    "Banda [4.5-5.5MM]  |  Delta AD = %+.3f  |  Delta Contrata = %+.3f  |  DiD = %+.3f\nNota: DiD es estadístico descriptivo de magnitud, no estimador causal formal.\nEstimador principal: RD de densidad (Cattaneo). Ver figs 1a/1b.\nLimitación: contrata puede estar contaminada por sustitución AD->otras modalidades.",
    did_ad, did_co, did_did
  )) |>
  adjust_legend_title("Modalidad") |>
  theme_tidyplot() %>%
  adjust_size(width = NA, height = NA)

save_plot(fig3, filename = file.path(FIG_DIR, "fig3_DiD_share_debajo.png"),
              width = 8, height = 5, units = "in")
message("Figura 3 guardada")


# ── EXPORTAR TABLAS PROCESADAS ────────────────────────────────────────────────

message("\n── Exportando tablas procesadas ──")

write_csv(base, file.path(DATA_PROC_DIR, "base_limpia_AD_CO.csv"))
write_csv(did_tbl, file.path(DATA_PROC_DIR, "did_share_notch.csv"))

# Resumen de Bunching (Mecanismo)
bunch_summary <- map_dfr(results, \(res) {
  tibble(
    grupo = res$label,
    n     = res$n,
    B_hat = res$B_hat,
    B_lo  = res$B_ci[1],
    B_hi  = res$B_ci[2],
    M_hat = res$M_hat,
    M_lo  = res$M_ci[1],
    M_hi  = res$M_ci[2]
  )
})
write_csv(bunch_summary, file.path(DATA_PROC_DIR, "bunching_metrics.csv"))

# Resumen de RD (Estimador Principal)
rd_summary <- imap_dfr(rd_results, \(r, nm) {
  if (is.null(r)) return(NULL)
  tibble(
    grupo  = nm,
    T_jk   = r$T_stat,
    p_val  = r$p,
    bw_l   = r$bw_l,
    bw_r   = r$bw_r,
    n_l    = r$n_l,
    n_r    = r$n_r
  )
})
write_csv(rd_summary, file.path(DATA_PROC_DIR, "rddensity_results.csv"))

message(sprintf("Tablas guardadas en: %s/", DATA_PROC_DIR))


# ── 10. ANÁLISIS DE QUIEBRE ESTRUCTURAL (ITS) ────────────────────────────────

message("\n── Iniciando análisis de quiebre estructural (ITS) ──")

# Parámetros específicos
inicio_periodo <- as.Date("2015-01-01")
fin_periodo    <- as.Date("2026-05-31")   # excluye junio 2026 (incompleto)
modalidad_target <- "Administración directa"

# Colores (evitamos fuentes externas para evitar problemas de renderizado)
col_teal  <- "#2D6A5E"
col_azul  <- "#1F3D6B"
col_marca <- "#B0413E"

# 1. Preparar serie mensual
serie <- base |>
  mutate(
    mes = floor_date(f_ini, "month"),
    es_ad = as.integer(mod == modalidad_target)
  ) |>
  filter(!is.na(mes), mes >= inicio_periodo, mes <= fin_periodo) |>
  summarise(total = n(), ad = sum(es_ad), .by = mes) |>
  arrange(mes) |>
  complete(mes = seq(min(mes), max(mes), by = "month"),
           fill = list(total = 0, ad = 0)) |>
  mutate(
    prop_ad = if_else(total > 0, ad / total, NA_real_),
    t = row_number(),
    post = as.integer(mes >= DATE_VIG),
    t_post = post * (t - min(t[mes >= DATE_VIG]) + 1)
  )

# 2. Quiebre endógeno: strucchange (Bai-Perron)
y_ts <- ts(serie$prop_ad, start = c(year(min(serie$mes)), month(min(serie$mes))), frequency = 12)
bp <- breakpoints(y_ts ~ 1, h = 0.15, breaks = 5)
fechas_quiebre <- breakdates(bp)

# 3. Series de tiempo interrumpidas (ITS)
its <- lm(prop_ad ~ t + post + t_post, data = serie)
serie <- serie |> mutate(ajuste_its = predict(its, newdata = serie))

# 4. Robustez bayesiana: bcp
set.seed(SEED)
b <- bcp(as.numeric(na.omit(serie$prop_ad)))
serie_bcp <- serie |>
  filter(!is.na(prop_ad)) |>
  mutate(prob_quiebre = b$posterior.prob)
mes_bcp <- serie_bcp$mes[which.max(serie_bcp$prob_quiebre)]

# 5. Gráficos finales
serie_largo <- serie |>
  select(mes, Observado = prop_ad, `Ajuste ITS` = ajuste_its) |>
  pivot_longer(-mes, names_to = "serie", values_to = "valor")

g_final <- serie_largo |>
  tidyplot(x = mes, y = valor, color = serie) |>
  add_line(linewidth = 0.9) |>
  add_reference_lines(x = DATE_VIG, color = col_marca) |>
  add_reference_lines(x = as.Date(date_decimal(fechas_quiebre[1])), color = col_azul) |>
  adjust_colors(c("Observado" = col_teal, "Ajuste ITS" = col_azul)) |>
  add_title("Quiebre estructural en la modalidad de administración directa") |>
  adjust_x_axis_title("Mes de inicio de obra") |>
  adjust_y_axis_title("Proporción AD") |>
  theme_tidyplot() %>%
  adjust_size(width = NA, height = NA)

g_bcp <- serie_bcp |>
  tidyplot(x = mes, y = prob_quiebre) |>
  add_line(color = col_azul, linewidth = 0.8) |>
  add_reference_lines(x = DATE_VIG, color = col_marca) |>
  add_title("Probabilidad posterior de quiebre (bcp)") |>
  adjust_x_axis_title("Mes") |>
  adjust_y_axis_title("Prob. posterior") |>
  theme_tidyplot() %>%
  adjust_size(width = NA, height = NA)

# Guardar con dimensiones adecuadas para evitar cortes
save_plot(g_final, filename = file.path(FIG_DIR, "fig2_quiebre_its.png"),
          width = 8, height = 5, units = "in")

save_plot(g_bcp, filename = file.path(FIG_DIR, "fig5_bcp_posterior.png"),
          width = 8, height = 4, units = "in")

# 6. Tabla resumen de hallazgos de quiebre
resumen_q <- tibble(
  metodo = c("Bai-Perron (BIC)", "bcp (post. máx.)", "ITS vigencia"),
  fecha  = c(paste(format(as.Date(date_decimal(fechas_quiebre)), "%Y-%m"), collapse = "; "),
             as.character(mes_bcp),
             as.character(DATE_VIG))
)
write_csv(resumen_q, file.path(DATA_PROC_DIR, "resumen_quiebres.csv"))

message("Análisis de quiebre completado y tablas exportadas.")


# ── 11. TABLA RESUMEN ─────────────────────────────────────────────────────────
#
# JERARQUÍA DE PRESENTACIÓN:
#   [A] ESTIMADOR PRINCIPAL  — RD de densidad (Cattaneo): T_post vs T_pre en AD
#   [B] MAGNITUD ECONÓMICA   — B̂ / M̂ con IC 95% bootstrap (polinomio contrafactual)
#   [C] ESTADÍSTICO DESC.    — DiD del share (comunicable, con advertencia de
#                              contaminación por sustitución AD→otras modalidades)

W <- 78
message("\n", strrep("═", W))
message("BUNCHING EN S/5MM — TABLA RESUMEN")
message(strrep("═", W))

# ── [A] ESTIMADOR PRINCIPAL: RD de densidad ──────────────────────────────────
message("\n[A] ESTIMADOR PRINCIPAL — RD de densidad (Cattaneo-Jansson-Ma)")
message("    Estimador de interés: cambio en T entre ventanas (T_post - T_pre en AD)")
message("    Supuesto: densidad de montos continua en S/5MM salvo por la directiva")
message(strrep("─", W))
message(sprintf("  %-26s  %6s  %8s  %10s  %9s  %9s",
                "Grupo", "n_izq", "n_der", "T (jk)", "p", "bw [MM]"))
message(strrep("─", W))

walk2(rd_results, grupos, function(r, g) {
  if (is.null(r)) {
    message(sprintf("  %-26s  n insuficiente", g$label)); return(invisible())
  }
  p_str  <- if (r$p < 0.001) "<0.001" else sprintf("%.3f", r$p)
  bw_str <- sprintf("[%.2f, %.2f]", r$bw_l / 1e6, r$bw_r / 1e6)
  message(sprintf("  %-26s  %6d  %8d  %10.3f  %9s  %9s",
                  g$label, r$n_l, r$n_r, r$T_stat, p_str, bw_str))
})

# Diferencia de T: cambio en la discontinuidad pre→post
T_ad_pre  <- rd_results$AD_pre$T_stat   %||% NA_real_
T_ad_post <- rd_results$AD_post$T_stat  %||% NA_real_
T_co_pre  <- rd_results$Cont_pre$T_stat %||% NA_real_
T_co_post <- rd_results$Cont_post$T_stat %||% NA_real_

message(strrep("─", W))
message(sprintf("  Delta T (AD):      T_post - T_pre = %.3f - %.3f = %+.3f",
                T_ad_post, T_ad_pre, T_ad_post - T_ad_pre))
message(sprintf("  Delta T (Contrata): T_post - T_pre = %.3f - %.3f = %+.3f  [placebo redondeo]",
                T_co_post, T_co_pre, T_co_post - T_co_pre))
message(sprintf("  DiD de T (AD - Contrata):  %+.3f",
                (T_ad_post - T_ad_pre) - (T_co_post - T_co_pre)))
message("\n  Lectura: si Delta_T(AD) >> 0 y Delta_T(Contrata) ~ 0,")
message("  la discontinuidad post-vigencia en AD no se explica por redondeo")
message("  administrativo preexistente (que afectaría a ambas modalidades).")
message("\n  ADVERTENCIA: contrata como placebo puede estar contaminada por")
message("  sustitución AD -> contrata o AD -> OxI/nucleo-ejecutor/otras modalidades.")
message("  Interpretar Delta_T(Contrata) solo como control de redondeo, NO como")
message("  contrafactual del efecto de la norma sobre la distribucion de modalidades.")

# ── [B] MAGNITUD ECONÓMICA: B̂ / M̂ ──────────────────────────────────────────
message(sprintf("\n[B] MAGNITUD ECONÓMICA — Exceso (B\u0302) y Hueco (M\u0302) alrededor del notch"))
message("    Polinomio contrafactual grado ", POLY_DEG,
        " | banda exclusion [", EXCL_LO/1e6, ", ", EXCL_HI/1e6, "]MM",
        " | bootstrap n=", N_BOOT, sep="")
message(strrep("─", W))
message(sprintf("  %-26s  %6s  %+8s  %18s  %+8s  %18s",
                "Grupo", "n", "B\u0302", "IC 95% B\u0302", "M\u0302", "IC 95% M\u0302"))
message(strrep("─", W))

walk(results, function(res) {
  message(sprintf("  %-26s  %6d  %+8.1f  [%+7.1f,%+7.1f]  %+8.1f  [%+7.1f,%+7.1f]",
                  res$label, res$n,
                  res$B_hat, res$B_ci[1], res$B_ci[2],
                  res$M_hat, res$M_ci[1], res$M_ci[2]))
})

message(strrep("─", W))
message("  Lectura: B\u0302 > 0 = exceso de obras justo DEBAJO del notch (evasion del tramo).")
message("           M\u0302 > 0 = hueco de obras justo ENCIMA (obras 'desaparecidas' del tramo medio).")
message("  Comparar B\u0302_post vs B\u0302_pre en AD: el incremento es atribuible a la directiva.")

# ── [C] DiD DEL SHARE (estadístico descriptivo) ──────────────────────────────
message(sprintf("\n[C] ESTADÍSTICO DESCRIPTIVO — DiD del share < S/5MM (banda [4.5-5.5MM])"))
message("    Util para comunicar magnitud en %; NO es el estimador formal.")
message(strrep("─", W))

walk(c("pre","anuncio","post"), function(v) {
  ad_r <- filter(did_tbl, mod == "AD",       ventana == v)
  co_r <- filter(did_tbl, mod == "Contrata", ventana == v)
  message(sprintf("  %-10s  AD: %.3f (n=%d)   Contrata: %.3f (n=%d)",
                  v,
                  if(nrow(ad_r)>0) ad_r$share else NA, if(nrow(ad_r)>0) ad_r$n else 0,
                  if(nrow(co_r)>0) co_r$share else NA, if(nrow(co_r)>0) co_r$n else 0))
})

message(strrep("─", W))
message(sprintf("  Delta AD (post-pre):       %+.3f", did_ad))
message(sprintf("  Delta Contrata (post-pre): %+.3f", did_co))
message(sprintf("  DiD (AD - Contrata):       %+.3f", did_did))
message("\n  LIMITACION: contrata no es contrafactual limpio del efecto de la norma.")
message("  Puede haber sustitución AD -> contrata (infla densidad contrata en el notch)")
message("  y sustitución AD -> OxI/otras (invisible en este estimador).")
message("  Usar solo como referencia de magnitud, no para inferencia causal.")

message("\n", strrep("═", W))
message(sprintf("Archivos generados en:"))
message(sprintf("  %s/                         <- Gráficos (PNG)", FIG_DIR))
message(sprintf("  %s/                  <- Datos procesados (CSV)", DATA_PROC_DIR))
message(strrep("═", W))

