# Prueba chica del FE evolutivo con un dataset sintetico (segundos, no datos reales):
#   Rscript tests/test_evolutivo.R 635_workflow_senior.R
# Saca del script el bloque del evolutivo (entre "ARCH_EV_GEN <-" y el
# comentario de Kaggle) y fganancia_lgbm_meseta (z635 celda 65) a archivos
# temporales, los carga con source() y los corre con stubs de PARAM, loguear
# y mandar_mail. PARAM$CN$train es el de la catedra (celda 67): la importancia
# sale del LightGBM de canaritos.
suppressMessages({ library(data.table); library(lightgbm) })
args <- commandArgs(trailingOnly= TRUE)
script <- normalizePath(if (length(args)) args[1] else "635_workflow_senior.R")
lin <- readLines(script)
desde <- grep("^ARCH_EV_GEN <- ", lin)
hasta <- grep("^# GRUPO A: Kaggle se prepara", lin) - 1
stopifnot(length(desde) == 1, length(hasta) == 1, hasta > desde)

dir <- tempfile("ev_test_"); dir.create(file.path(dir, "checkpoints"), recursive= TRUE); setwd(dir)
bloque <- file.path(dir, "bloque_evolutivo.R")
writeLines(lin[desde:hasta], bloque)
c65 <- grep("^# --- z635 celda 65 ---", lin); c66 <- grep("^# --- z635 celda 66 ---", lin)
stopifnot(length(c65) == 1, length(c66) == 1, c66 > c65)
bloque_cn <- file.path(dir, "celda65.R")
writeLines(lin[c65:(c66 - 1)], bloque_cn)

loguear <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
MAILS <- 0; mandar_mail <- function(...) MAILS <<- MAILS + 1
GRUPO <- "B"
PARAM <- list(semilla_primigenia= 100213, ganancia= list(acierto= 975000, error= -25000),
  BO= list(ht_fijo= ""), GA= list(modo= "evolutivo"),
  EV= list(seleccion= "importancia", generaciones= 4L, tope= 0L, min_hijos= 0.25, presion= 10,
           originales_fijas= FALSE, formulas= 0L, historia= FALSE, piso_canaritos= FALSE,
           semillas_n= 1L, borrar_muertas= TRUE, mail_cada_gen= 1,
           imp= list(training= c(202012, 202101, 202102, 202103), validation= 202105L, undersampling= 1,
                     prueba= 202104L)),
  CN= list(train= list(clase01_valor1= c("BAJA+2", "BAJA+1"), positivos= c("BAJA+2"),
    training= c(202101, 202102, 202103), validation= c(202105), undersampling= 0.1,
    gan1= 0.975, gan0= -0.025), ratio= 0.2, desvios= 2))
source(bloque_cn)
source(bloque)

# dataset: 6 meses x 3000 clientes, 20 variables; la BAJA depende de v1*v2 y v3/v4.
# Incluye los meses de canaritos (202101-202103 y 202105)
set.seed(1)
meses <- c(202012, 202101, 202102, 202103, 202104, 202105)
dataset <- rbindlist(lapply(meses, function(m) {
  d <- data.table(numero_de_cliente= 1:3000, foto_mes= m)
  for (j in 1:20) d[, (paste0("v", j)) := rnorm(.N)]
  d[, v20 := 0]                          # constante: no tiene que romper nada
  d[sample(.N, 50), v5 := NA]            # NA
  z <- d$v1 * d$v2 + d$v3 / (abs(d$v4) + 0.5)
  p <- plogis(-4 + 1.5 * z)
  d[, clase_ternaria := ifelse(runif(.N) < p, ifelse(runif(.N) < 0.5, "BAJA+2", "BAJA+1"), "CONTINUA")]
  d
}))
dataset[, azar := runif(.N)]
# el evolutivo ordena el dataset por cliente y mes (lo necesita la historia)
setorder(dataset, numero_de_cliente, foto_mes)
originales <- paste0("v", 1:20)
dataset0 <- copy(dataset)

res <- correr_evolutivo()
cat(res, "\n")
gen <- fread(ARCH_EV_GEN)
nac <- fread(ARCH_EV_NAC)
pob <- fread(ARCH_EV_POB)
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "OK   " else "FALLA", msg, "\n"); if (!isTRUE(cond)) quit(status= 1) }
ok(nrow(gen) == 5, "una fila por generacion (0..4)")
ok(all(gen$poblacion[-1] == 20), "la poblacion queda en el tope (20) todas las generaciones")
ok(all(gen$copias[-1] + gen$nacidas[-1] == gen$poblacion[-1]), "la generacion es copias X * 1 + hijos")
ok(all(gen$nacidas[-1] >= 5), "siempre hay lugar para hijos (EV_MIN_HIJOS = 0,25 del tope de 20)")
ok(sum(gen$nacidas) == max(gen$nacidas_acumuladas), "conteo de formulas nacidas")
# la regla: pasa igual toda variable que el modelo uso en la generacion anterior
# (importancia > 0), salvo que no entren todas y actue el minimo de hijos
ok(all(gen$copias[-1] == pmin(gen$usadas[-nrow(gen)], 15)), "pasan todas las usadas de la generacion anterior (hasta 15)")
ok(all(gen$uso_minimo_hijos[-1] == (gen$usadas[-nrow(gen)] > 15)), "el minimo de hijos actua solo cuando las usadas no entran")
copias <- nac[padre2 == "uno"]
ok(nrow(copias) == sum(gen$copias) && all(endsWith(copias$formula, " * 1")), "las copias quedan como X * 1 en nacimientos")
ok(any(copias$nacio_padre1 > 0), "tambien pasan formulas, no solo originales")
ok(max(gen$prof_max) >= 2, "hay recombinacion entre generaciones (profundidad >= 2)")
ok(nrow(pob) == 20, "evolutivo_poblacion.txt con 20 filas")
gen_vivas <- pob[original == FALSE, nombre]
ok(all(gen_vivas %in% names(dataset)), "las generadas vivas estan en el dataset")
muertas <- setdiff(originales, pob$nombre)
ok(!any(muertas %in% names(dataset)), "las originales muertas salieron del dataset")
f <- pob[original == FALSE][1]
if (nrow(f)) ok(isTRUE(all.equal(dataset[[f$nombre]], ev_evaluar(f$formula, dataset0))), "formula recalculada en el dataset entero")
ok(MAILS == 4, "un mail por generacion")
ok(all(gen$ganancia_meseta != 0), "la ganancia sale de la validation de la importancia (202105)")
s_imp <- ev_muestra()
ok(length(s_imp$filas) == nrow(dataset0[foto_mes %in% c(202012, 202101, 202102, 202103, 202104, 202105)]),
   "con EV_UNDERSAMPLING = 1 la importancia usa todas las filas de sus meses (y el de prueba)")
ok(sum(s_imp$es_prueba) == 3000 && !any(s_imp$es_prueba & s_imp$es_valid), "el mes de prueba (202104) queda aparte")
ok(all(gen$ganancia_meseta_prueba != 0), "la ganancia en el mes de prueba queda en el registro")
mej <- fread(ARCH_EV_MEJ)
ok(nrow(mej) > 0 && all(mej$importancia > 0) && !is.unsorted(-mej$importancia), "evolutivo_mejores.txt: generadas usadas, por importancia")
ok(inherits(try(ev_evaluar("system('ls')", dataset), silent= TRUE), "try-error"), "ev_evaluar no ejecuta codigo")

# retomar: con el checkpoint en la ultima generacion no reentrena nada
n_gen <- nrow(gen)
dataset <- copy(dataset0)
correr_evolutivo()
ok(nrow(fread(ARCH_EV_GEN)) == n_gen, "retomar desde el checkpoint no repite generaciones")

# seleccion al azar corre igual
unlink(c(ARCH_EV_GEN, ARCH_EV_NAC, ARCH_EV_POB, CK_EV))
PARAM$EV$seleccion <- "azar"; PARAM$EV$generaciones <- 2L
dataset <- copy(dataset0)
correr_evolutivo()
ok(nrow(fread(ARCH_EV_GEN)) == 3, "EV_SELECCION=azar")

# busqueda (EV_SOLO): sin materializar el dataset no cambia, y el control al
# azar escribe sus propios archivos
unlink(c(ARCH_EV_GEN, ARCH_EV_NAC, ARCH_EV_POB, ARCH_EV_MEJ, CK_EV))
PARAM$EV$seleccion <- "importancia"; PARAM$EV$generaciones <- 3L
dataset <- copy(dataset0)
correr_evolutivo(materializar= FALSE)
ok(identical(names(dataset), names(dataset0)) && isTRUE(all.equal(dataset, dataset0)), "EV_SOLO: el dataset no cambia")
pob_busqueda <- fread(ARCH_EV_POB)
rm(list= ls(EV), envir= EV)
PARAM$EV$seleccion <- "azar"
ev_archivos("_azar")
correr_evolutivo(materializar= FALSE)
ok(file.exists("evolutivo_generaciones_azar.txt") && file.exists("checkpoints/evolutivo_azar.rds") &&
   nrow(fread("evolutivo_generaciones.txt")) == 4, "el control al azar escribe sus archivos y no pisa los de la busqueda")
ev_archivos(); PARAM$EV$seleccion <- "importancia"
comp <- ev_comparacion()
ok(grepl("importancia", comp) && grepl("azar", comp) && length(strsplit(comp, "\n")[[1]]) > 8, "la comparacion tiene las dos curvas")

# EV_POBLACION_DESDE: la poblacion de la busqueda entra al dataset sin evolucionar
dataset <- copy(dataset0)
ev_desde(ARCH_EV_POB)
gen_b <- pob_busqueda[original == FALSE]
ok(all(gen_b$nombre %in% names(dataset)), "EV_POBLACION_DESDE: las formulas de la busqueda estan en el dataset")
ok(isTRUE(all.equal(dataset[[gen_b$nombre[1]]], ev_evaluar(gen_b$formula[1], dataset0))), "EV_POBLACION_DESDE: misma formula, mismos valores")
ok(!any(setdiff(originales, pob_busqueda$nombre) %in% names(dataset)), "EV_POBLACION_DESDE: las originales que no pasaron salen")
ok(inherits(try(ev_desde("no_existe.txt"), silent= TRUE), "try-error"), "EV_POBLACION_DESDE con un archivo que no existe corta")

# EV_ORIGINALES_FIJAS: las originales pasan siempre y la genetica agrega hasta
# EV_FORMULAS formulas (10 aca); nunca se borra una original
unlink(c(ARCH_EV_GEN, ARCH_EV_NAC, ARCH_EV_POB, ARCH_EV_MEJ, CK_EV))
PARAM$EV$seleccion <- "importancia"; PARAM$EV$generaciones <- 4L
PARAM$EV$originales_fijas <- TRUE; PARAM$EV$formulas <- 10L
dataset <- copy(dataset0)
correr_evolutivo()
gen_f <- fread(ARCH_EV_GEN); pob_f <- fread(ARCH_EV_POB)
ok(all(gen_f$originales_vivas == 20), "originales fijas: las 20 originales siguen en todas las generaciones")
ok(all(gen_f$poblacion[-1] == 30), "originales fijas: la poblacion es 20 originales + 10 formulas")
ok(all(gen_f$generadas_vivas[-1] <= 10), "originales fijas: las formulas no pasan su cupo")
ok(all(gen_f$nacidas[-1] >= 3), "originales fijas: siempre nacen al menos 3 (EV_MIN_HIJOS = 0,25 de 10)")
ok(all(originales %in% names(dataset)), "originales fijas: ninguna original sale del dataset final")
ok(all(pob_f[original == FALSE, nombre] %in% names(dataset)), "originales fijas: las formulas finales entran al dataset")
unlink(c(ARCH_EV_GEN, ARCH_EV_NAC, ARCH_EV_POB, ARCH_EV_MEJ, CK_EV))
PARAM$EV$seleccion <- "azar"; PARAM$EV$generaciones <- 2L
dataset <- copy(dataset0)
correr_evolutivo(materializar= FALSE)
ok(all(fread(ARCH_EV_GEN)$originales_vivas == 20), "originales fijas con seleccion al azar: las originales tambien quedan")

# EV_HISTORIA: cada formula con su lag1, lag2, delta1, delta2 y tend6
# 1. ev_historia contra un calculo directo: lags por cliente y la pendiente de
#    lm() sobre la fila actual y las 5 anteriores del cliente, sin los NA
set.seed(7)
id <- rep(1:3, times= c(9, 4, 1)); vh <- round(rnorm(14), 2); vh[c(3, 11)] <- NA
h <- ev_historia(vh, id)
ref_lag <- function(k) sapply(seq_along(vh), function(i) if (i - k >= 1 && id[i - k] == id[i]) vh[i - k] else NA)
ref_tend <- sapply(seq_along(vh), function(i) {
  w <- max(1, i - 5):i; w <- w[id[w] == id[i]]; y <- vh[w]; x <- seq_along(w)
  ok <- !is.na(y); if (sum(ok) < 2) NA else unname(coef(lm(y[ok] ~ x[ok]))[2])
})
ok(isTRUE(all.equal(h$lag1, ref_lag(1))) && isTRUE(all.equal(h$lag2, ref_lag(2))), "historia: lag1 y lag2 por cliente (no cruzan clientes)")
ok(isTRUE(all.equal(h$delta1, vh - ref_lag(1))) && isTRUE(all.equal(h$delta2, vh - ref_lag(2))), "historia: delta1 y delta2")
ok(isTRUE(all.equal(h$tend6, ref_tend)), "historia: tend6 igual a la pendiente de lm() de 6 filas del cliente")
# 2. el evolutivo con historia: el LightGBM ve la formula y su historia, y el dataset final tambien
unlink(c(ARCH_EV_GEN, ARCH_EV_NAC, ARCH_EV_POB, ARCH_EV_MEJ, CK_EV))
PARAM$EV$seleccion <- "importancia"; PARAM$EV$generaciones <- 3L
PARAM$EV$originales_fijas <- TRUE; PARAM$EV$formulas <- 5L; PARAM$EV$historia <- TRUE
dataset <- copy(dataset0)
correr_evolutivo()
gen_h <- fread(ARCH_EV_GEN); pob_h <- fread(ARCH_EV_POB)
ok(all(gen_h$columnas[-1] == 20 + 5 * 6), "historia: el LightGBM ve 20 originales + 5 formulas x 6 columnas")
fh <- pob_h[original == FALSE]
cols_h <- c(fh$nombre, as.vector(outer(fh$nombre, EV_SUFIJOS_HIST, paste, sep= "_")))
ok(all(cols_h %in% names(dataset)), "historia: cada formula entra al dataset con lag1, lag2, delta1, delta2 y tend6")
v1 <- ev_evaluar(fh$formula[1], dataset0); h1 <- ev_historia(v1, dataset0$numero_de_cliente)
ok(isTRUE(all.equal(dataset[[paste0(fh$nombre[1], "_tend6")]], h1$tend6)) &&
   isTRUE(all.equal(dataset[[paste0(fh$nombre[1], "_lag1")]], h1$lag1)), "historia: la del dataset final coincide con la formula recalculada")
ok(all(originales %in% names(dataset)), "historia: con originales fijas no sale ninguna original")
# retomar con historia: rearma las formulas y su historia desde el checkpoint
dataset <- copy(dataset0)
correr_evolutivo(materializar= FALSE)
ok(nrow(fread(ARCH_EV_GEN)) == nrow(gen_h), "historia: retomar desde el checkpoint no repite generaciones")

# retomar despues de un corte entre los logs y el checkpoint: se sacan las filas
# de la generacion que se va a repetir
fwrite(data.table(generacion= 99L), ARCH_EV_GEN, sep= "\t", append= TRUE)
fwrite(data.table(generacion= 99L, nombre= "x"), ARCH_EV_NAC, sep= "\t", append= TRUE)
dataset <- copy(dataset0)
correr_evolutivo(materializar= FALSE)
ok(max(fread(ARCH_EV_GEN)$generacion) == 3 && max(fread(ARCH_EV_NAC)$generacion) == 3,
   "retomar saca las filas de una generacion que quedo a medias")

# la poblacion guarda con que historia se hizo, y EV_POBLACION_DESDE la respeta
ok("historia" %in% names(pob_h) && isTRUE(as.logical(pob_h$historia[1])), "la poblacion guarda EV_HISTORIA")
PARAM$EV$historia <- FALSE
dataset <- copy(dataset0)
ev_desde(ARCH_EV_POB)
ok(PARAM$EV$historia && all(cols_h %in% names(dataset)), "EV_POBLACION_DESDE usa la historia de la busqueda aunque la corrida diga otra")

# padres: los empatados en 0 pesan 1 (el minimo), el primero EV_PRESION
w <- ev_pesos(c(5, 4, 3, 0, 0, 0))
ok(w[1] == 10 && all(w[4:6] == 1) && all(diff(w[1:4]) < 0), "padres: importancia 0 pesa 1, el primero 10")

# EV_PISO_CANARITOS: sirve lo que le gana a las canaritos; sin hijos triviales.
# Se agregan v1_lag1 y v2_lag1 (por cliente) para que haya pares de la misma variable
unlink(c(ARCH_EV_GEN, ARCH_EV_NAC, ARCH_EV_POB, ARCH_EV_MEJ, CK_EV))
dataset <- copy(dataset0)
dataset[, v1_lag1 := shift(v1), by= numero_de_cliente]
dataset[, v2_lag1 := shift(v2), by= numero_de_cliente]
dataset_l <- copy(dataset)
PARAM$EV$piso_canaritos <- TRUE; PARAM$EV$historia <- TRUE
PARAM$EV$originales_fijas <- TRUE; PARAM$EV$formulas <- 30L; PARAM$EV$generaciones <- 3L
correr_evolutivo(materializar= FALSE)
gen_c <- fread(ARCH_EV_GEN); nac_c <- fread(ARCH_EV_NAC); pob_c <- fread(ARCH_EV_POB)
ok(all(gen_c$canaritos > 0) && all(is.finite(gen_c$umbral_canaritos)), "piso: el LightGBM de la importancia lleva canaritos y calcula el umbral")
ok(gen_c$usadas[1] < 22, "piso: no todas las originales le ganan a las canaritos (hay 16 de ruido puro)")
ok(all(pob_c[original == FALSE & util == TRUE, importancia] > 0), "piso: lo que sirve tiene importancia")
ok(sum(pob_c[original == FALSE, util], na.rm= TRUE) == nrow(fread(ARCH_EV_MEJ)), "piso: evolutivo_mejores.txt son las formulas que sirven")
hijos_c <- nac_c[padre2 != "uno"]
triviales <- hijos_c[!grepl("^ev", padre1) & !grepl("^ev", padre2) &
                     mapply(ev_trivial, padre1, padre2, sub("^[(].* ([-+*/]) .*[)]$", "\\1", formula))]
ok(nrow(triviales) == 0, "sin hijos triviales: no nace x - x_lag1 ni x_lag1 + x_delta1")
ok(ev_trivial("v1", "v1_lag1", "-") && ev_trivial("v1_lag1", "v1", "-") && ev_trivial("v1_delta2", "v1", "-") &&
   ev_trivial("v1_lag1", "v1_delta1", "+") && ev_trivial("v1", "v1", "+"),
   "ev_trivial: x - x_lagk, x - x_deltak, x_lagk + x_deltak y x + x son triviales")
ok(!ev_trivial("v1_lag1", "v1_lag2", "-") && !ev_trivial("v1_delta1", "v1_delta2", "-") &&
   !ev_trivial("v1", "v1_lag1", "+") && !ev_trivial("v1", "v1_tend6", "+") &&
   !ev_trivial("v1_lag1", "v1_delta2", "+") && !ev_trivial("v1", "v1_lag1", "*") && !ev_trivial("v1", "v2_lag1", "-"),
   "ev_trivial: x_lag1 - x_lag2, x_delta1 - x_delta2, x + x_lag1, x + x_tend6 y los * / pueden nacer")
resta <- hijos_c[grepl("^\\(.* - .*\\)$", formula) & !grepl("[(].*[(]", formula), formula]
partes <- regmatches(resta, regexec("^\\((.*) - (.*)\\)$", resta))
invertidas <- vapply(partes, function(x) paste0("(", x[3], " - ", x[2], ")"), "")
ok(!any(invertidas %in% resta), "sin hijos triviales: si nacio a - b no nace b - a")
ok(all(gen_c$originales_vivas == 22), "piso + originales fijas: las 22 originales siguen")
cat("TODO OK\n")
