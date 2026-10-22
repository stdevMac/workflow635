# ===========================================================================
# Compara los brazos del workflow635 v3 (grupo B: ganancia real en 202107)
#   E0 (WF9920) base con canaritos      E5 (WF9925) base sin canaritos
#   E6 (WF9926) FE evolutivo            E7 (WF9927) evolutivo, seleccion al azar
#   E8 (WF9928) evolutivo sin copias    E9 (WF9929) base con FT_ALFA=0.95
# Metrica principal: la MESETA (promedio de los cortes fijos de 8.000 a 12.000
# de ganancia_202107.txt), pareada por semilla del pipeline, con Wilcoxon de
# rangos con signo. El PR 9 (validacion/) mostro que el maximo es optimista en
# 10-12 M y tiene ~4 veces peor senal/ruido, asi que el MAXIMO de la curva (la
# metrica de la clase, ultima fila) va como secundaria.
#
# uso (en la VM, como ds):  Rscript brazos/analizar_brazos.R [carpeta exp]
# graba brazos_v3/brazos.csv y brazos_v3/evolutivo.csv; el resto va a la salida
# ===========================================================================
suppressMessages(library(data.table))

args <- commandArgs(trailingOnly= TRUE)
EXP <- if (length(args)) args[1] else "/home/ds/buckets/b1/exp"
SALIDA <- file.path(EXP, "brazos_v3")
dir.create(SALIDA, showWarnings= FALSE)
BRAZOS <- c(E0= 9920L, E5= 9925L, E6= 9926L, E7= 9927L, E8= 9928L, E9= 9929L,
            E10= 9933L, E11= 9931L, E12= 9932L, E13= 9934L)
# comparaciones fijadas antes de correr, cada una contesta una pregunta. Holm
# va por tanda: v3 (el codigo del PR 8, ya medida) y v4 (originales fijas,
# historia de las formulas y piso de canaritos)
COMPARACIONES <- list(
  c("E6", "E5", "lo que aporta el evolutivo (los dos sin canaritos)", "v3"),
  c("E6", "E0", "evolutivo contra la base de la catedra", "v3"),
  c("E6", "E7", "si la importancia sirve para elegir padres y sobrevivientes", "v3"),
  c("E6", "E8", "si el operador clon sirve", "v3"),
  c("E5", "E0", "lo que cambia por no correr canaritos", "v3"),
  c("E9", "E0", "pesos temporales 0.95^k en el final train", "v3"),
  c("E11", "E5", "lo que aporta el evolutivo v4 (los dos sin canaritos)", "v4"),
  c("E11", "E12", "si la importancia sirve (contra copias y padres al azar)", "v4"),
  c("E11", "E10", "lo que aportan la historia de las formulas y el piso de canaritos", "v4"),
  c("E10", "E5", "originales fijas sin historia contra la base", "v4"),
  # desglose de E11 - E10 (Holm aparte): que parte es la historia y que parte el piso
  c("E13", "E10", "lo que aporta la historia de las formulas (los dos sin piso)", "v4 desglose"),
  c("E11", "E13", "lo que aporta el piso de canaritos (los dos con historia)", "v4 desglose"))

millones <- function(x) formatC(x / 1e6, format= "f", digits= 2, decimal.mark= ",", big.mark= ".")
p3 <- function(x) formatC(x, format= "f", digits= 3, decimal.mark= ",")

# --- corridas terminadas ----------------------------------------------------
leer_corrida <- function(brazo, carpeta) {
  g <- fread(file.path(carpeta, "ganancia_202107.txt"))
  data.table(brazo= brazo,
    semilla= as.numeric(sub(".*_s", "", basename(carpeta))),
    maximo= g[nrow(g), ganancia], envios_max= g[nrow(g), envios],   # la ultima fila es el maximo
    meseta= mean(g[-nrow(g), ganancia]),
    horas= {
      r <- if (file.exists(file.path(carpeta, "registro.txt"))) readLines(file.path(carpeta, "registro.txt"), warn= FALSE) else ""
      h <- regmatches(r, regexpr("esta sesion: [0-9.]+ h", r))
      if (length(h)) as.numeric(gsub("[^0-9.]", "", h[1])) else NA_real_
    },
    carpeta= carpeta)
}

runs <- rbindlist(lapply(names(BRAZOS), function(b) {
  d <- Sys.glob(file.path(EXP, sprintf("WF%d_s*", BRAZOS[[b]])))
  d <- d[file.exists(file.path(d, "TERMINADO")) & file.exists(file.path(d, "ganancia_202107.txt"))]
  rbindlist(lapply(d, function(x) leer_corrida(b, x)))
}))

cat("Brazos del workflow635 v3, grupo B (ganancia real en 202107)\n")
cat("Generado:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")
if (!nrow(runs)) { cat("Todavia no termino ninguna corrida.\n"); quit(save= "no") }

fwrite(runs, file.path(SALIDA, "brazos.csv"))
cat("Corridas terminadas por brazo:", paste(names(table(runs$brazo)), table(runs$brazo), sep= "=", collapse= "  "), "\n\n")

tabla <- function(metrica) dcast(runs, semilla ~ brazo, value.var= metrica)
for (m in c("meseta", "maximo")) {
  a <- tabla(m)
  cat(if (m == "meseta") "MESETA por semilla (millones, metrica principal):\n"
      else "Maximo de la curva por semilla (millones, secundaria):\n")
  print(a[, c(list(semilla= semilla), lapply(.SD, millones)), .SDcols= setdiff(names(a), "semilla")], row.names= FALSE)
  cat("Promedio por brazo:", paste(runs[, .(v= millones(mean(get(m)))), by= brazo][order(brazo), paste0(brazo, " ", v)], collapse= "   "), "\n\n")
}
cat("Horas por corrida (mediana):",
  paste(runs[, .(h= median(horas, na.rm= TRUE)), by= brazo][order(brazo), paste0(brazo, " ", formatC(h, format= "f", digits= 1, decimal.mark= ","))], collapse= "   "), "\n\n")

# --- Wilcoxon apareado por semilla --------------------------------------------
comparar <- function(a, b, ancho) {
  if (!all(c(a, b) %in% names(ancho))) return(data.table(comparacion= paste(a, "-", b), n= 0L))
  x <- ancho[[a]] - ancho[[b]]
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(data.table(comparacion= paste(a, "-", b), n= n))
  # exacto: con n semillas el p mas chico posible es 2/2^n (6: 0,031; 10: 0,002)
  p <- tryCatch(suppressWarnings(wilcox.test(x, exact= n <= 25)$p.value), error= function(e) NA_real_)
  data.table(comparacion= paste(a, "-", b), n= n, media= mean(x), mediana= median(x),
             gana= sum(x > 0), p_wilcoxon= p)
}
for (m in c("meseta", "maximo")) {
  ancho <- tabla(m)
  tb <- rbindlist(lapply(COMPARACIONES, function(k) cbind(comparar(k[1], k[2], ancho), pregunta= k[3], tanda= k[4])), fill= TRUE)
  # Holm por tanda: v3 y v4 son preguntas fijadas en momentos distintos
  if ("p_wilcoxon" %in% names(tb)) tb[!is.na(p_wilcoxon), p_holm := p.adjust(p_wilcoxon, method= "holm"), by= tanda]
  cat(sprintf("Wilcoxon apareado sobre %s (millones; Holm sobre las comparaciones con pares):\n",
              if (m == "meseta") "la MESETA (principal)" else "el maximo (secundaria)"))
  for (i in seq_len(nrow(tb))) with(tb[i], {
    if (n < 2) cat(sprintf("  %-8s n=%d, faltan pares  (%s)\n", comparacion, n, pregunta))
    else cat(sprintf("  %-8s n=%2d  media %7s  mediana %7s  gana %2d de %2d  p %s  Holm %s%s  (%s)\n",
      comparacion, n, millones(media), millones(mediana), gana, n, p3(p_wilcoxon), p3(p_holm),
      if (isTRUE(p_holm < 0.05)) " <-" else "   ", pregunta))
  })
  cat("\n")
}

# --- el evolutivo por dentro ----------------------------------------------------
ev <- rbindlist(lapply(runs[brazo %in% c("E6", "E7", "E8", "E10", "E11", "E12", "E13")]$carpeta, function(d) {
  f <- file.path(d, "evolutivo_generaciones.txt")
  if (!file.exists(f)) return(NULL)
  g <- fread(f, sep= "\t")
  impo <- if (file.exists(file.path(d, "impo.txt"))) fread(file.path(d, "impo.txt")) else data.table(Feature= character(), Gain= numeric())
  es_ev <- startsWith(impo$Feature, "ev")
  g[, `:=`(brazo= runs[carpeta == d, brazo], semilla= runs[carpeta == d, semilla],
           final_ev_top50= sum(head(es_ev, 50)), final_ev_top100= sum(head(es_ev, 100)),
           final_gain_ev= if (nrow(impo)) sum(impo$Gain[es_ev]) / sum(impo$Gain) else NA_real_)]
  g
}), fill= TRUE)
if (nrow(ev)) {
  fwrite(ev, file.path(SALIDA, "evolutivo.csv"))
  cat("Evolutivo por brazo y generacion (promedio entre semillas):\n")
  print(ev[, .(semillas= .N, poblacion= round(mean(poblacion)), originales_vivas= round(mean(originales_vivas)),
               generadas_vivas= round(mean(generadas_vivas)), prof_max= max(prof_max),
               imp_generadas= paste0(round(100 * mean(importancia_generadas), 1), "%"),
               nacidas_acum= round(mean(nacidas_acumuladas)), max_fitness= millones(mean(ganancia_max))),
           by= .(brazo, generacion)][order(brazo, generacion)], row.names= FALSE)
  cat("\nEn el modelo final (impo.txt): generadas en el top 50 / top 100 y % del gain\n")
  print(unique(ev[, .(brazo, semilla, final_ev_top50, final_ev_top100,
                      final_gain_ev= paste0(round(100 * final_gain_ev, 1), "%"))])[order(brazo, semilla)], row.names= FALSE)
}

# --- tiempos y RAM de la cola ---------------------------------------------------
f <- list.files(SALIDA, "^tiempos.*[.]tsv$", full.names= TRUE)
if (length(f)) {
  t <- rbindlist(lapply(f, fread))
  if (nrow(t)) {
    cat("\nMinutos por corrida (cola):\n")
    print(t[, .(corridas= .N, fallidas= sum(rc != 0), mediana_min= as.numeric(median(minutos))), by= brazo], row.names= FALSE)
  }
}
r <- list.files(SALIDA, "^ram_.*[.]tsv$", full.names= TRUE)
if (length(r)) {
  ram <- rbindlist(lapply(r, function(x) {
    v <- tryCatch(fread(x, header= FALSE)$V2, error= function(e) NA_real_)
    data.table(corrida= sub("^ram_(.*)[.]tsv$", "\\1", basename(x)), gb_max= max(v, na.rm= TRUE))
  }))
  ram[, brazo := names(BRAZOS)[match(as.integer(sub("^WF([0-9]+)_.*", "\\1", corrida)), BRAZOS)]]
  cat("\nRAM usada maxima por brazo (GB, de free cada minuto):\n")
  print(ram[, .(corridas= .N, gb_max= max(gb_max)), by= brazo], row.names= FALSE)
}
