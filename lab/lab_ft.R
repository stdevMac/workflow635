# ===========================================================================
# Laboratorio de final train (grupo B: entrena 201901-202105, mide en 202107)
#
# El FE es el de E5 (sin canaritos ni evolutivo): se lee del checkpoint
# dataset_canaritos.fst de una corrida E5. Sobre ese dataset entrena muchos
# modelos por configuracion, cada uno con su semilla, y graba la probabilidad
# de cada cliente de 202107 (en varias iteraciones). Con eso lab_analizar.R
# arma semillerios de cualquier tamanio, como en 0899_ComoElegirMejorModelo.
#
#   Rscript lab_ft.R <dataset_canaritos.fst> <carpeta_salida> "base:10,debil:100,..."
#
# Retoma: un modelo que ya tiene su archivo en <salida>/<config>/ se saltea.
# Los modelos se intercalan entre configuraciones, asi una corrida cortada
# queda pareja.
# ===========================================================================
suppressPackageStartupMessages({ library(data.table); library(fst); library(lightgbm) })
args <- commandArgs(trailingOnly= TRUE)
ARCH_FST <- args[1]; SALIDA <- args[2]
PEDIDO <- strsplit(strsplit(args[3], ",")[[1]], ":")
dir.create(SALIDA, recursive= TRUE, showWarnings= FALSE)
loguear <- function(...) cat(format(Sys.time(), "%F %T"), "|", ..., "\n")

FINAL_TRAIN <- c(201901:201912, 202001:202012, 202101:202105)
FUTURO <- 202107
# LAB_FUTURO=202105: validacion en otro mes, entrena hasta 202103 y mide en 202105
if (nchar(Sys.getenv("LAB_FUTURO"))) {
  FUTURO <- as.integer(Sys.getenv("LAB_FUTURO"))
  FINAL_TRAIN <- FINAL_TRAIN[FINAL_TRAIN <= c(202105L, 202103L, 202101L)[match(FUTURO, c(202107L, 202105L, 202103L))]]
  if (anyNA(FINAL_TRAIN) || !length(FINAL_TRAIN)) stop("LAB_FUTURO no soportado: ", FUTURO)
}
loguear("final train", min(FINAL_TRAIN), "-", max(FINAL_TRAIN), "| mide en", FUTURO)
POSITIVOS <- c("BAJA+1", "BAJA+2")
CORTES <- seq(8000, 12000, 500)

# hiperparametros de E5: mejor fila de la BO de WF9600 (techos 2^10 iteraciones, 2^11 hojas)
HT_E5 <- list(num_iterations= 1023L, num_leaves= 2038L, learning_rate= 0.01579195,
  feature_fraction= 0.8524174, min_sum_hessian_in_leaf= 0.9985839, min_data_in_leaf= 0L)
FIJOS <- list(objective= "binary", boost_from_average= TRUE, feature_pre_filter= FALSE,
  verbosity= -100L, force_row_wise= TRUE, max_bin= 31L, extra_trees= FALSE)

# ds: con que datos entrena. "us02" = todas las BAJA + 20% de los CONTINUA, sorteado
# de nuevo para cada modelo (como E5); "full" = todo; "rec02" = us02 desde 202001
# iters: en que iteraciones se graba la probabilidad (la ultima es num_iterations)
CONFIGS <- list(
  base   = list(ds= "us02", p= HT_E5, iters= c(512L, 1023L)),
  largo  = list(ds= "us02", p= modifyList(HT_E5, list(num_iterations= 2047L)), iters= c(1023L, 1535L, 2047L)),
  ff03   = list(ds= "us02", p= modifyList(HT_E5, list(feature_fraction= 0.3)), iters= c(512L, 1023L)),
  rec02  = list(ds= "rec02", p= HT_E5, iters= c(512L, 1023L)),
  medio  = list(ds= "us02", p= list(num_iterations= 400L, learning_rate= 0.05, num_leaves= 255L,
                  min_data_in_leaf= 100L, feature_fraction= 0.5, bagging_fraction= 0.8, bagging_freq= 1L),
                iters= c(200L, 300L, 400L)),
  # el optimo del semillerio de 100 en 0899_ComoElegirMejorModelo_2025 (learners debiles)
  debil  = list(ds= "full", p= list(num_iterations= 23L, learning_rate= 0.34, num_leaves= 32L,
                  min_data_in_leaf= 711L, feature_fraction= 0.2), iters= 23L),
  debil02= list(ds= "us02", p= list(num_iterations= 23L, learning_rate= 0.34, num_leaves= 32L,
                  min_data_in_leaf= 711L, feature_fraction= 0.2), iters= 23L),
  # todos los CONTINUA, con y sin bagging de negativos por iteracion
  us1    = list(ds= "full", p= HT_E5, iters= c(512L, 1023L)),
  bag    = list(ds= "full", p= modifyList(HT_E5, list(bagging_freq= 1L, pos_bagging_fraction= 1,
                  neg_bagging_fraction= 0.2)), iters= c(512L, 1023L)),
  # derivadas de bag, la mejor del primer lab: con feature_fraction 0.3 (ff03 tambien
  # mejoro a base) y con el doble de arboles (largo tambien mejoro a base)
  bagff  = list(ds= "full", p= modifyList(HT_E5, list(bagging_freq= 1L, pos_bagging_fraction= 1,
                  neg_bagging_fraction= 0.2, feature_fraction= 0.3)), iters= c(512L, 1023L)),
  baglargo= list(ds= "full", p= modifyList(HT_E5, list(bagging_freq= 1L, pos_bagging_fraction= 1,
                  neg_bagging_fraction= 0.2, num_iterations= 2047L)), iters= c(1023L, 1535L, 2047L))
)
desconocidas <- setdiff(sapply(PEDIDO, `[`, 1), names(CONFIGS))
if (length(desconocidas)) stop("configuraciones desconocidas: ", paste(desconocidas, collapse= ", "))

semilla_modelo <- function(i) 100003L + 7919L * (i - 1L)

# agenda intercalada: modelo 1 de cada config, despues el 2, ...
pedido <- data.table(cfg= sapply(PEDIDO, `[`, 1), n= as.integer(sapply(PEDIDO, `[`, 2)))
# LAB_DESDE: corre los modelos desde+1 .. desde+n, para que dos hosts no repitan semillas
desde <- as.integer(Sys.getenv("LAB_DESDE", "0"))
agenda <- pedido[, .(i= desde + seq_len(n)), by= cfg][order(i, match(cfg, pedido$cfg))]
agenda[, semilla := semilla_modelo(i)]
agenda[, archivo := file.path(SALIDA, cfg, paste0("m", semilla, ".fst"))]
agenda <- agenda[!file.exists(archivo)]
loguear("faltan", nrow(agenda), "modelos:", paste(agenda[, .N, cfg][, paste0(cfg, "=", N)], collapse= " "))
if (!nrow(agenda)) quit(status= 0)

loguear("leo", ARCH_FST)
dataset <- read_fst(ARCH_FST, as.data.table= TRUE)
campos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01", "azar"))
loguear("dataset", nrow(dataset), "x", ncol(dataset), "; entreno con", length(campos), "campos")

fut <- dataset[foto_mes == FUTURO]
if (!file.exists(file.path(SALIDA, "clientes.fst")))
  write_fst(fut[, .(numero_de_cliente, clase_ternaria)], file.path(SALIDA, "clientes.fst"))
mat_fut <- data.matrix(fut[, campos, with= FALSE]); rm(fut)

dataset <- dataset[foto_mes %in% FINAL_TRAIN]
dataset[, clase01 := as.integer(clase_ternaria %in% POSITIVOS)]
gc(verbose= FALSE)

# lgb.Dataset construido (libera la matriz); "full" se arma una vez y se reutiliza
armar <- function(filas) {
  d <- lgb.Dataset(data= data.matrix(dataset[filas, campos, with= FALSE]),
    label= dataset[filas, clase01], free_raw_data= TRUE,
    params= list(max_bin= 31L, feature_pre_filter= FALSE, verbosity= -100L))
  lgb.Dataset.construct(d); gc(verbose= FALSE); d
}
cache_full <- NULL
datos <- function(ds, semilla) {
  if (ds == "full") {
    if (is.null(cache_full)) { loguear("armo el dataset full"); cache_full <<- armar(rep(TRUE, nrow(dataset))) }
    return(cache_full)
  }
  set.seed(semilla, kind= "L'Ecuyer-CMRG")
  azar <- runif(nrow(dataset))
  filas <- dataset$clase01 == 1L | azar < 0.2
  if (ds == "rec02") filas <- filas & dataset$foto_mes >= 202001
  armar(filas)
}

tiempos <- file.path(SALIDA, "tiempos.tsv")
if (!file.exists(tiempos)) cat("cfg\tsemilla\tfilas\tminutos\tmeseta_ultima\n", file= tiempos)
clase <- read_fst(file.path(SALIDA, "clientes.fst"))$clase_ternaria
meseta <- function(prob) {
  g <- cumsum(ifelse(clase[order(-prob)] == "BAJA+2", 975000, -25000))
  mean(g[CORTES])
}

for (k in seq_len(nrow(agenda))) {
  a <- agenda[k]; cf <- CONFIGS[[a$cfg]]
  t0 <- Sys.time()
  dtrain <- datos(cf$ds, a$semilla)
  param <- modifyList(modifyList(FIJOS, cf$p), list(seed= a$semilla))
  modelo <- lgb.train(data= dtrain, params= param, verbose= -100L)
  pred <- as.data.table(lapply(setNames(cf$iters, paste0("p", cf$iters)),
    function(it) predict(modelo, mat_fut, num_iteration= it)))
  dir.create(dirname(a$archivo), showWarnings= FALSE)
  write_fst(pred, paste0(a$archivo, ".tmp")); file.rename(paste0(a$archivo, ".tmp"), a$archivo)
  min <- round(as.numeric(difftime(Sys.time(), t0, units= "mins")), 1)
  m <- meseta(pred[[ncol(pred)]])
  cat(paste(a$cfg, a$semilla, dim(dtrain)[1], min, m, sep= "\t"), "\n", sep= "", file= tiempos, append= TRUE)
  loguear(a$cfg, "semilla", a$semilla, ":", min, "min, meseta", round(m / 1e6, 2), "M")
  rm(modelo); if (cf$ds != "full") rm(dtrain); gc(verbose= FALSE)
}
loguear("listo")
