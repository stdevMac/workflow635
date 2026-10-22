# ===========================================================================
# 635_workflow_senior.R
#
# El workflow del z635_WorkFlow_01_senior.ipynb (catedra) como script para
# correr desatendido en una VM Spot:
#
#   Rscript 635_workflow_senior.R .env  >> log.txt 2>&1
#
# Las funciones y celdas de la catedra estan COPIADAS SIN CAMBIOS (cada bloque
# dice de que celda viene). Lo agregado:
#
#   * configuracion desde un .env (ver .env.example): grupo, rutas, mail, kaggle
#   * GRUPO A: valida 202107, final train hasta 202107, predice 202109, Kaggle
#     GRUPO B: valida 202105, final train hasta 202105, predice 202107 y
#              calcula la ganancia contra la clase real de 202107. Sin Kaggle.
#   * semillerio con semillas fijas (PARAM$FT$semillas) en vez de primos al azar
#   * mail a una LISTA de destinatarios: arranque, fin de cada etapa, cada N
#     evaluaciones de la BO, error (con la etapa) y fin
#   * checkpoints en el bucket, para que una VM expropiada no pierda trabajo:
#       - dataset despues del FE historico y despues de canaritos (.fst)
#       - BO: cada evaluacion del diseno inicial y cada iteracion (HT.RDATA)
#       - modelos finales: uno por semilla (ya lo hacia el z635)
#       - Kaggle: una marca por archivo subido, no se resube
#     Relanzar el mismo comando retoma desde lo ultimo guardado.
#
# v2 (todo opcional desde el .env; sin las variables nuevas corre como antes,
# salvo la genetica, que ahora mide en otros meses y decide con un control):
#   * la genetica busca en GA_VALIDA_BUSQUEDA y confirma en GA_VALIDA_CONTROL,
#     meses que ni la BO ni el testing usan, entrenando con todos los meses
#     anteriores (o los GA_VENTANA anteriores). La ganadora de cada ronda queda
#     solo si le gana a GA_N_CONTROL formulas al azar de la misma gramatica
#   * GA_MODO = genetica | azar | nada, para comparar brazos con las mismas
#     semillas; GA_FORZAR=1 deja entrar las ganadoras igual (y registra el control)
#   * HT_FIJO: un BO_log.txt de otra corrida, para no repetir la BO
#   * FT_SEMILLAS_N, FT_UNDERSAMPLING: final train mas barato para comparar brazos
#   * KAGGLE_SUBIR=0: el grupo A no sube nada
#
# v3 (PR 8): la genetica de gramEvol, el control y la transferencia se sacaron.
# En su lugar, FE evolutivo (GA_MODO=evolutivo|nada, ver la seccion EV):
#   * poblacion de tamano fijo (EV_TOPE, por defecto las columnas con las que
#     llega el dataset): nacen hijos de cruzar dos columnas de CUALQUIER
#     generacion, mueren otras tantas; no se busca una super-variable
#   * la importancia sale del LightGBM de canaritos de la catedra: pasa igual
#     (X * 1) toda variable que el modelo uso, original o formula, y los
#     padres de los hijos se sortean por el puesto en la importancia
#   * evolutivo_generaciones.txt: tamano, usadas, copias, nacidas, originales vivas
#   * canaritos apagado por defecto con el evolutivo (CN_RUN)
#   * FT_ALFA: pesos alfa^k por mes en el final train
#   * los brazos se comparan con Wilcoxon apareado sobre el
#     MAXIMO de la curva de ganancia por semilla (brazos/analizar_brazos.R)
# ===========================================================================

format(Sys.time(), "%a %b %d %X %Y")

# ---------------------------------------------------------------------------
# PAQUETES: libreria del usuario (la VM es nueva en cada lanzamiento)
# ---------------------------------------------------------------------------
dir.create(Sys.getenv("R_LIBS_USER"), recursive= TRUE, showWarnings= FALSE)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
options(repos= c(CRAN= "https://cloud.r-project.org"))

# --- z635 celda 9 ---
require("data.table")

if( !require("R.utils")) install.packages("R.utils")
require("R.utils")

if (!require("fst")) install.packages("fst")
require("fst")

# ---------------------------------------------------------------------------
# CONFIGURACION desde el .env
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly= TRUE)
archivo_env <- if (length(args) >= 1) args[1] else ".env"
if (!file.exists(archivo_env)) stop("No existe el archivo de configuracion ", archivo_env,
  " (copiar .env.example y completarlo)")
readRenviron(archivo_env)
# v2: una variable vacia en el .env (VAR=) vale lo mismo que no ponerla
env_o <- function(variable, defecto) { v <- Sys.getenv(variable, ""); if (v == "") defecto else v }

requeridas <- c("GRUPO", "EXPERIMENTO", "BUCKET_DIR", "DATASET_PATH",
  "MAIL_FROM", "MAIL_PASS_FILE", "MAIL_TO_FILE")
GRUPO <- toupper(Sys.getenv("GRUPO"))
KAGGLE_SUBIR <- env_o("KAGGLE_SUBIR", "1") == "1"
if (GRUPO == "A" && KAGGLE_SUBIR) requeridas <- c(requeridas, "KAGGLE_JSON", "KAGGLE_COMPETENCIA")
faltan <- requeridas[Sys.getenv(requeridas) == ""]
if (length(faltan)) stop("Faltan variables en ", archivo_env, ": ", paste(faltan, collapse= ", "))
if (!(GRUPO %in% c("A", "B"))) stop("GRUPO tiene que ser A o B, no '", GRUPO, "'")
if (!file.exists(Sys.getenv("DATASET_PATH"))) stop("No existe DATASET_PATH: ", Sys.getenv("DATASET_PATH"))

PARAM <- list()
PARAM$semilla_primigenia <- 100213
# una corrida completa por semilla: vm/startup.sh pasa cada semilla de
# SEMILLAS_PIPELINE como segundo argumento, y esa semilla es la primigenia de
# todo el pipeline (genetica, canaritos, undersampling, BO). El modelo final
# usa igual las 9 del semillerio.
SEMILLA_CORRIDA <- if (length(args) >= 2) as.numeric(args[2]) else NA
if (!is.na(SEMILLA_CORRIDA)) PARAM$semilla_primigenia <- SEMILLA_CORRIDA
PARAM$experimento <- as.integer(Sys.getenv("EXPERIMENTO"))
# nombre de la corrida: WF<experimento>, o WF<experimento>_s<semilla> en la serie por semilla
NOMBRE_CORRIDA <- paste0("WF", PARAM$experimento,
  if (!is.na(SEMILLA_CORRIDA)) paste0("_s", format(SEMILLA_CORRIDA, scientific= FALSE)) else "")
PARAM$grupo <- GRUPO
PARAM$dataset <- basename(Sys.getenv("DATASET_PATH"))

# semillerio del final training: semillas fijas. Las 5 primeras son del
# usuario; las 4 ultimas son primos sorteados con
#   set.seed(100213); sample(setdiff(primes::generate_primes(100000, 1000000), mias), 4)
PARAM$FT$semillas <- c(100213, 100313, 100613, 100913, 101113,
                       570991, 669287, 832421, 882599)
# v2: para comparar brazos alcanza con menos semillas y un final train con
# undersampling de los CONTINUA (1 = todos, como siempre)
PARAM$FT$semillas <- head(PARAM$FT$semillas,
  as.integer(env_o("FT_SEMILLAS_N", length(PARAM$FT$semillas))))
PARAM$FT$semillerio <- length(PARAM$FT$semillas)
PARAM$FT$undersampling <- as.numeric(env_o("FT_UNDERSAMPLING", "1"))
# v2: HT_FIJO = ruta a un BO_log.txt de otra corrida; se usa su mejor fila y no se corre la BO
PARAM$BO$ht_fijo <- Sys.getenv("HT_FIJO", "")
if (nzchar(PARAM$BO$ht_fijo) && !file.exists(PARAM$BO$ht_fijo)) stop("No existe HT_FIJO: ", PARAM$BO$ht_fijo)

PARAM$mail$desde    <- Sys.getenv("MAIL_FROM")
PARAM$mail$cada_iter <- as.integer(env_o("MAIL_CADA_ITER", "5"))

# ganancia del problema, para el grupo B (testing contra 202107)
PARAM$ganancia$acierto <- 975000    # BAJA+2 enviado
PARAM$ganancia$error   <- -25000    # cualquier otro enviado
PARAM$kaggle$cortes <- seq(8000, 12000, by = 500)

# --- etapas del z635 que corren -------------------------------------------
# Todas las etapas del z635 corren como en el notebook. El FE evolutivo va
# despues del FE de random forest (9.7.1.5), que sigue prendido.
PARAM$FE_rf$run <- TRUE

# --- FE evolutivo (v3, PR 8) -------------------------------------------------
# La poblacion son TODAS las columnas del dataset (despues del FE historico y
# del de random forest). Cada generacion:
#   1. el LightGBM de canaritos de la catedra (z635 celda 66: sus mismos
#      parametros, con early stopping sobre la ganancia meseta) entrena con
#      toda la poblacion y con todo lo posible: todos los meses con clase menos
#      los reservados del grupo y todos los CONTINUA (PARAM$EV$imp). Da la
#      importancia (Gain) de cada columna. No hay un LightGBM propio del
#      evolutivo: el modelo es el de la catedra
#   2. reproduccion (X * 1): pasa igual a la generacion siguiente toda
#      variable que el modelo uso (importancia > 0), sea original o formula.
#      Con EV_ORIGINALES_FIJAS (default) las originales pasan todas, siempre,
#      y solo las formulas compiten, por EV_FORMULAS lugares: la genetica
#      agrega columnas, no reemplaza las que el modelo necesita.
#      Lo que fue bueno sigue sin cambiar; lo que el modelo no uso no pasa.
#      La importancia decide que queda: no hay un corte nuestro. El unico
#      resguardo es EV_MIN_HIJOS: si las usadas no entran todas dejandole ese
#      minimo de lugares a los hijos, pasan las de mas importancia
#   3. cruza: los lugares que quedan hasta EV_TOPE son hijos  padre1 op padre2
#      (op en + - * /). Los padres se sortean con probabilidad segun el puesto
#      en la importancia (ranking lineal: el primero es EV_PRESION veces mas
#      probable que el ultimo; ninguno tiene probabilidad 0). Los padres
#      pueden ser de cualquier generacion: una nacida en la 1 se cruza con una
#      de la 5
#   4. el LightGBM de la generacion siguiente mide a las copias y a los hijos
#      juntos. La poblacion queda siempre en EV_TOPE: no se buscan
#      super-variables
#   El tope por defecto es la cantidad de columnas con la que llega el dataset.
# GA_MODO=evolutivo   lo de arriba
# GA_MODO=nada        sin FE evolutivo: el brazo base
# EV_SELECCION=azar   mismo evolutivo pero las copias (la misma cantidad) y los
#                     padres se eligen al azar, sin importancia: el control
#                     para saber si la importancia sirve para decidir
# Cada generacion queda en evolutivo_generaciones.txt (tamano de la
# poblacion, usadas, copias, nacidas, originales vivas, profundidad, ganancia).
PARAM$GA$modo <- env_o("GA_MODO", "evolutivo")
if (!(PARAM$GA$modo %in% c("evolutivo", "nada")))
  stop("GA_MODO tiene que ser evolutivo o nada (la genetica de gramEvol, azar y transferencia se sacaron en la v3)")
PARAM$EV$seleccion    <- env_o("EV_SELECCION", "importancia")
if (!(PARAM$EV$seleccion %in% c("importancia", "azar"))) stop("EV_SELECCION tiene que ser importancia o azar")
PARAM$EV$generaciones <- as.integer(env_o("EV_GENERACIONES", "10"))
PARAM$EV$tope         <- as.integer(env_o("EV_TOPE", "0"))      # 0 = las columnas con las que llega el dataset
PARAM$EV$min_hijos    <- as.numeric(env_o("EV_MIN_HIJOS", "0.25"))  # lugares minimos para hijos, en fraccion del tope
# EV_ORIGINALES_FIJAS=1 (default): las originales pasan siempre y la genetica
# agrega hasta EV_FORMULAS formulas. 0: compite toda la poblacion por el tope
# y las formulas reemplazan columnas (la regla anterior; en WF9570 y en la
# tanda v3 las originales caian de 1.509 a ~500 y ~220)
PARAM$EV$originales_fijas <- env_o("EV_ORIGINALES_FIJAS", "1") == "1"
PARAM$EV$formulas    <- as.integer(env_o("EV_FORMULAS", "500"))     # lugares para formulas (con originales fijas)
# EV_HISTORIA=1 (default): cada formula tiene su propia historia (lag1, lag2,
# delta1, delta2 y tend6, como las columnas del FE historico), en cada
# generacion y en el dataset final. La importancia de una formula es la suma
# de sus 6 columnas. 0: la formula sola (como en el PR 8)
PARAM$EV$historia    <- env_o("EV_HISTORIA", "1") == "1"
# EV_PISO_CANARITOS=1 (default): una columna "sirve" (y pasa X * 1) solo si le
# gana a columnas al azar en el LightGBM de la importancia, con la regla de la
# catedra (z635 celda 66: puesto < mediana del puesto de las canaritos +
# PARAM$CN$desvios desvios, con PARAM$CN$ratio x columnas de canaritos). Sin
# piso alcanzaba ganar un solo corte (importancia > 0) para pasar
PARAM$EV$piso_canaritos <- env_o("EV_PISO_CANARITOS", "1") == "1"
PARAM$EV$presion      <- as.numeric(env_o("EV_PRESION", "10"))  # el mejor puesto es X veces mas probable que el peor
PARAM$EV$semillas_n   <- as.integer(env_o("EV_SEMILLAS_N", "1"))  # LightGBM por generacion (importancia promedio)
PARAM$EV$borrar_muertas <- env_o("EV_BORRAR_MUERTAS", "1") == "1"   # 1: las originales muertas salen del dataset
# la importancia del evolutivo usa todo lo posible: todas las columnas, todos
# los meses con clase menos los reservados de cada grupo, y todos los CONTINUA.
#   A: entrena 201901-202103, valida 202105 (202104 de gap, como canaritos);
#      nunca 202107 (validacion de la BO) ni 202109 (Kaggle)
#   B: lo mismo corrido dos meses: entrena 201901-202101, valida 202103;
#      nunca 202105 ni 202107
# EV_UNDERSAMPLING: fraccion de los CONTINUA (1 = todos; 0.2 = como la
# training strategy de la catedra, si el tiempo no da)
PARAM$EV$imp <- list(
  training= if (GRUPO == "A") c(201901:201912, 202001:202012, 202101:202103)
            else c(201901:201912, 202001:202012, 202101),
  validation= if (GRUPO == "A") 202105L else 202103L,
  undersampling= as.numeric(env_o("EV_UNDERSAMPLING", "1")))
# mes de prueba: un mes con clase que el evolutivo NUNCA usa para decidir.
# En cada generacion el LightGBM de la importancia lo predice y la ganancia
# queda en el registro: es la medida de si la poblacion mejora fuera de lo
# que vio. A: 202107 (la validacion de la BO, que el evolutivo no toca);
# B: 202105
PARAM$EV$imp$prueba <- if (GRUPO == "A") 202107L else 202105L
# EV_SOLO=1: corrida de busqueda. Hace el FE, corre el evolutivo, graba la
# poblacion y las mejores formulas y termina: sin BO, sin modelo final, sin
# Kaggle. EV_CONTROL=1 (con EV_SOLO): en la misma corrida y sobre el mismo
# dataset corre despues el evolutivo con seleccion al azar (archivos *_azar),
# para comparar las dos curvas en el mes de prueba.
# EV_POBLACION_DESDE: ruta a un evolutivo_poblacion.txt de una busqueda. No
# evoluciona: mete esa poblacion al dataset y sigue con el workflow completo
PARAM$EV$solo <- env_o("EV_SOLO", "0") == "1"
PARAM$EV$control <- env_o("EV_CONTROL", "0") == "1"
PARAM$EV$poblacion_desde <- env_o("EV_POBLACION_DESDE", "")
# canaritos: con el evolutivo no se corre (achica el dataset a ~450 columnas y
# eso es justo lo que no queremos). CN_RUN=1 lo prende igual
PARAM$CN$run <- env_o("CN_RUN", if (PARAM$GA$modo == "evolutivo") "0" else "1") == "1"
# pesos temporales del final training: alfa^k con k = meses antes del ultimo
# mes de final train (1 = sin pesos; la clase sugirio probar 0.95)
PARAM$FT$alfa <- as.numeric(env_o("FT_ALFA", "1"))
PARAM$EV$mail_cada_gen <- 1   # mail de avance cada N generaciones

# ---------------------------------------------------------------------------
# CARPETA DEL EXPERIMENTO (en el bucket: todo lo que se graba sobrevive)
# ---------------------------------------------------------------------------
carpeta_exp <- file.path(Sys.getenv("BUCKET_DIR"), "exp", NOMBRE_CORRIDA)
dir.create(carpeta_exp, recursive= TRUE, showWarnings= FALSE)
setwd(carpeta_exp)
dir.create("checkpoints", showWarnings= FALSE)

loguear <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "|", ..., "\n")
loguear("experimento", PARAM$experimento, "grupo", GRUPO, "en", carpeta_exp)

# ---------------------------------------------------------------------------
# MAIL: Gmail SMTP con curl, a todos los de MAIL_TO_FILE (uno por linea).
# Un mail que falla se loguea y la corrida sigue.
# ---------------------------------------------------------------------------
destinatarios <- function() {
  arch <- Sys.getenv("MAIL_TO_FILE")
  if (!file.exists(arch)) return(character(0))
  d <- trimws(readLines(arch, warn= FALSE))
  d[d != "" & !startsWith(d, "#")]
}

# Dos hilos de Gmail por experimento: "experimento" (arranque, etapas, errores,
# fin) e "iteraciones" (generaciones de la genetica, rondas, avance de la BO).
# Gmail agrupa por asunto igual + References al primer mensaje del hilo. El
# Message-ID del primero es fijo; la marca .mail_hilo_<hilo> dice si ya salio.
# (Sin sample(): el RNG no se puede tocar durante la genetica.)
MAIL_N <- 0L
id_hilo <- function(hilo) sprintf("<%s-%s-%s@workflow635>", tolower(NOMBRE_CORRIDA), tolower(GRUPO), hilo)

mandar_mail <- function(titulo, cuerpo, hilo= "experimento") {
  tryCatch({
    para <- destinatarios()
    pass_file <- Sys.getenv("MAIL_PASS_FILE")
    if (!length(para)) { loguear("SIN MAIL: no hay destinatarios en", Sys.getenv("MAIL_TO_FILE")); return(invisible(FALSE)) }
    if (!file.exists(pass_file)) { loguear("SIN MAIL: no existe", pass_file); return(invisible(FALSE)) }
    pass <- trimws(readLines(pass_file, warn= FALSE)[1])
    asunto <- sprintf("[%s grupo %s] %s", NOMBRE_CORRIDA, GRUPO, hilo)
    raiz <- id_hilo(hilo)
    marca <- paste0(".mail_hilo_", hilo)
    es_primero <- !file.exists(marca)
    MAIL_N <<- MAIL_N + 1L
    msg_id <- if (es_primero) raiz else sprintf("<%s-%s-%s-%s-%d@workflow635>",
      tolower(NOMBRE_CORRIDA), tolower(GRUPO), hilo, format(Sys.time(), "%Y%m%d%H%M%OS6"), MAIL_N)

    archivo <- tempfile(fileext= ".txt")
    writeLines(c(
      paste0("From: ", PARAM$mail$desde),
      paste0("To: ", paste(para, collapse= ", ")),
      paste0("Subject: ", if (es_primero) asunto else paste("Re:", asunto)),
      paste0("Message-ID: ", msg_id),
      if (!es_primero) c(paste0("In-Reply-To: ", raiz), paste0("References: ", raiz)),
      "Content-Type: text/plain; charset=UTF-8",
      "",
      titulo,
      strrep("-", nchar(titulo)),
      cuerpo), archivo)

    rc <- system2("curl", c("-s", "--ssl-reqd",
      "--url", "smtps://smtp.gmail.com:465",
      "--mail-from", PARAM$mail$desde,
      as.vector(rbind("--mail-rcpt", para)),
      "--user", shQuote(paste0(PARAM$mail$desde, ":", pass)),
      "-T", archivo), stdout= FALSE, stderr= FALSE)
    unlink(archivo)
    if (rc == 0 && es_primero) writeLines(raiz, marca)
    if (rc == 0) loguear("mail enviado [", hilo, "]:", titulo) else loguear("FALLO el mail (curl rc =", rc, "):", titulo)
    invisible(rc == 0)
  }, error= function(e) { loguear("FALLO el mail:", conditionMessage(e)); invisible(FALSE) })
}

# ---------------------------------------------------------------------------
# ETAPAS: cada una avisa por mail al terminar; un error avisa en que etapa fue
# ---------------------------------------------------------------------------
ETAPA <- "inicio"
T_ETAPA <- Sys.time()
T_INICIO <- Sys.time()
# duracion de cada etapa en esta sesion, para el registro final
TIEMPOS <- data.table(etapa= character(), minutos= numeric())
WF_VERSION <- Sys.getenv("WF_VERSION", "sin version (no se lanzo con vm/lanzar.sh)")

etapa <- function(nombre) {
  ETAPA <<- nombre
  T_ETAPA <<- Sys.time()
  loguear("=== inicio etapa", nombre)
}

fin_etapa <- function(detalle= "") {
  mins <- as.numeric(difftime(Sys.time(), T_ETAPA, units= "mins"))
  loguear("=== fin etapa", ETAPA, sprintf("(%.1f min)", mins))
  TIEMPOS <<- rbind(TIEMPOS, data.table(etapa= ETAPA, minutos= round(mins, 1)))
  total_h <- as.numeric(difftime(Sys.time(), T_INICIO, units= "hours"))
  mandar_mail(sprintf("termino %s (%.1f min)", ETAPA, mins),
    paste0("Etapa ", ETAPA, " terminada en ", sprintf("%.1f", mins), " min.\n",
           if (exists("dataset")) paste0("dataset: ", nrow(dataset), " filas x ", ncol(dataset), " columnas\n") else "",
           detalle, "\n\n",
           "Tiempos de esta sesion (", sprintf("%.1f", total_h), " h en total):\n",
           paste(sprintf("  %-28s %8.1f min", TIEMPOS$etapa, TIEMPOS$minutos), collapse= "\n")))
}

options(error= function() {
  msg <- geterrmessage()
  loguear("ERROR en la etapa", ETAPA, ":", msg)
  mandar_mail(paste("ERROR en", ETAPA),
    paste0("La corrida se detuvo en la etapa ", ETAPA, ".\n\n", msg,
           "\nRelanzar el mismo comando retoma desde el ultimo checkpoint."))
  quit(save= "no", status= 1)
})

# graba con nombre temporal y renombra: un corte a mitad no deja un archivo roto
grabar_checkpoint <- function(archivo) {
  tmp <- paste0(archivo, ".tmp")
  write_fst(dataset, tmp, compress= 50)
  file.rename(tmp, archivo)
  loguear("checkpoint grabado:", archivo)
}

leer_checkpoint <- function(archivo) {
  loguear("retomo desde checkpoint:", archivo)
  read_fst(archivo, as.data.table= TRUE)
}

# ===========================================================================
# FE EVOLUTIVO (v3, PR 8): una poblacion de variables que evoluciona
#   evolutivo_generaciones.txt  una fila por generacion: tamano de la
#                               poblacion, usadas, copias, nacidas, originales vivas,
#                               profundidad de las formulas, ganancia
#   evolutivo_nacimientos.txt   una fila por hijo nacido y por copia X * 1 (formula,
#                               padres y su generacion): cuantas formulas se generan
#   evolutivo_poblacion.txt     la poblacion final con su importancia
#   checkpoints/evolutivo.rds   el estado al final de cada generacion: relanzar
#                               retoma desde la ultima generacion terminada
# La genetica de la v2 (gramEvol: una formula por ronda, control contra
# formulas al azar, umbral de 2 desvios, transferencia) se saco: buscaba UNA
# super-variable por ronda y la importancia no decidia nada.
# ===========================================================================
if (!require("lightgbm")) install.packages("lightgbm")
require("lightgbm")

ARCH_EV_GEN <- "evolutivo_generaciones.txt"
ARCH_EV_NAC <- "evolutivo_nacimientos.txt"
ARCH_EV_POB <- "evolutivo_poblacion.txt"
CK_EV <- "checkpoints/evolutivo.rds"
ARCH_EV_MEJ <- "evolutivo_mejores.txt"
# los archivos del evolutivo, con un sufijo para el control al azar (EV_CONTROL)
ev_archivos <- function(sufijo= "") {
  ARCH_EV_GEN <<- paste0("evolutivo_generaciones", sufijo, ".txt")
  ARCH_EV_NAC <<- paste0("evolutivo_nacimientos", sufijo, ".txt")
  ARCH_EV_POB <<- paste0("evolutivo_poblacion", sufijo, ".txt")
  ARCH_EV_MEJ <<- paste0("evolutivo_mejores", sufijo, ".txt")
  CK_EV <<- paste0("checkpoints/evolutivo", sufijo, ".rds")
}
# columnas que no son variables (no entran a la poblacion)
EV_NO_VARIABLES <- c("numero_de_cliente", "foto_mes", "clase_ternaria", "clase01",
                     "entrenamiento", "azar", "fold_train", "fold_final_train")

miles <- function(x) formatC(round(x), format= "d", big.mark= ".", decimal.mark= ",")

# Inf -> NA y NaN -> 0 despues de CADA operacion, como la valvula del FE intra-mes
ev_limpiar <- function(v) { v[is.infinite(v)] <- NA; v[is.nan(v)] <- 0; v }
EV_OPS <- list(
  "+"= function(a, b) ev_limpiar(a + b),
  "-"= function(a, b) ev_limpiar(a - b),
  "*"= function(a, b) ev_limpiar(a * b),
  "/"= function(a, b) ev_limpiar(a / b))

# una formula se escribe con los nombres de las columnas ORIGINALES, asi se
# puede recalcular en cualquier tabla (la muestra al retomar, el dataset
# entero). Se calcula recorriendo el arbol de la formula: solo columnas,
# parentesis y las cuatro operaciones
ev_nombre_en_formula <- function(n) ifelse(make.names(n) == n, n, paste0("`", n, "`"))
ev_evaluar <- function(formula, tabla) {
  calcular <- function(x) {
    if (is.name(x)) return(as.numeric(tabla[[as.character(x)]]))
    op <- as.character(x[[1]])
    if (op == "(") return(calcular(x[[2]]))
    if (is.null(EV_OPS[[op]])) stop("operacion desconocida en una formula: ", op)
    EV_OPS[[op]](calcular(x[[2]]), calcular(x[[3]]))
  }
  calcular(str2lang(formula))
}

# la muestra de la importancia: todo lo posible. Entrena con todos los meses
# de PARAM$EV$imp$training (todos los BAJA+1/BAJA+2 y el
# PARAM$EV$imp$undersampling de los CONTINUA: 1 = todos) y valida en
# PARAM$EV$imp$validation, con el peso 1.0000001 a los BAJA+2 de la catedra
# (asi fganancia_lgbm_meseta los reconoce). Las columnas NO se copian aca: van
# como vectores al entorno V (ver correr_evolutivo), para no tener la muestra
# dos veces en RAM. El sorteo del undersampling usa su propia semilla y deja
# el RNG como estaba
ev_muestra <- function() {
  s <- new.env()
  rng <- get0(".Random.seed", envir= globalenv())
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  azar <- runif(nrow(dataset))
  if (!is.null(rng)) assign(".Random.seed", rng, envir= globalenv())
  imp <- PARAM$EV$imp
  uno <- dataset$clase_ternaria %in% PARAM$CN$train$clase01_valor1
  entrena <- dataset$foto_mes %in% imp$training & (uno | azar < imp$undersampling)
  valida <- dataset$foto_mes %in% imp$validation
  prueba <- dataset$foto_mes %in% imp$prueba
  s$filas <- which(entrena | valida | prueba)
  clase <- dataset$clase_ternaria[s$filas]
  s$es_valid <- dataset$foto_mes[s$filas] %in% imp$validation
  s$es_prueba <- dataset$foto_mes[s$filas] %in% imp$prueba
  s$label <- as.integer(clase %in% PARAM$CN$train$clase01_valor1)
  s$peso <- ifelse(clase %in% PARAM$CN$train$positivos, 1.0000001, 1.0)
  s$gan_valid <- ifelse(clase[s$es_valid] == "BAJA+2", PARAM$ganancia$acierto, PARAM$ganancia$error)
  s$gan_prueba <- ifelse(clase[s$es_prueba] == "BAJA+2", PARAM$ganancia$acierto, PARAM$ganancia$error)
  loguear("evolutivo: muestra de la importancia - entrena", paste(range(imp$training), collapse= "-"),
          "con", sum(!s$es_valid), "filas (", imp$undersampling, "de los CONTINUA), valida",
          paste(imp$validation, collapse= ","), "con", sum(s$es_valid), "| prueba",
          paste(imp$prueba, collapse= ","), "con", sum(s$es_prueba))
  s
}

# los parametros del LightGBM de CanaritosAsesinos (z635 celda 66), copiados
# tal cual salvo num_threads: la catedra usa 1 y aca van todos los nucleos de
# la VM (con ~4 M de filas por generacion; cambia la velocidad, no el modelo).
# La semilla se pone en cada entrenamiento
ev_param <- function() list(
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  verbosity = -100,
  max_depth = -1,
  min_gain_to_split = 0.0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  max_bin = 31,
  num_iterations = 9999,
  force_row_wise = TRUE,
  learning_rate = 0.065,
  feature_fraction = 1.0,
  min_data_in_leaf = 260,
  num_leaves = 60,
  early_stopping_rounds = 200,
  num_threads = parallel::detectCores()
)

# la historia de una columna (dataset ordenado por numero_de_cliente, foto_mes):
# lag1, lag2, delta1 y delta2 por cliente como la z635 celda 51, y la
# tendencia de 6 meses como fhistC (celda 49): pendiente por cuadrados minimos
# de la fila actual y las 5 anteriores del mismo cliente, salteando NA, con al
# menos 2 valores. La pendiente no cambia si se corre x, asi que x = -k para la
# fila de k meses antes y se calcula vectorizado. No usa fhistC: esa funcion se
# define en la etapa del FE historico y no existe si la corrida retoma desde su
# checkpoint
EV_SUFIJOS_HIST <- c("lag1", "lag2", "delta1", "delta2", "tend6")
ev_historia <- function(v, id) {
  lagk <- function(k) { l <- shift(v, k); mismo <- id == shift(id, k); l[is.na(mismo) | !mismo] <- NA; l }
  l1 <- lagk(1); l2 <- lagk(2)
  n <- length(v)
  N <- Sx <- Sy <- Sxy <- Sxx <- numeric(n)
  for (k in 0:5) {
    y <- switch(as.character(k), "0"= v, "1"= l1, "2"= l2, lagk(k))
    ok <- !is.na(y)
    x <- -k
    N <- N + ok; Sx <- Sx + ok * x; Sxx <- Sxx + ok * x^2
    Sy[ok] <- Sy[ok] + y[ok]; Sxy[ok] <- Sxy[ok] + x * y[ok]
  }
  tend <- rep(NA_real_, n)
  hay <- N > 1
  tend[hay] <- (N[hay] * Sxy[hay] - Sx[hay] * Sy[hay]) / (N[hay] * Sxx[hay] - Sx[hay]^2)
  list(lag1= l1, lag2= l2, delta1= v - l1, delta2= v - l2, tend6= tend)
}

# las columnas que el LightGBM ve de una poblacion: cada original y cada
# formula, y con EV_HISTORIA la historia de cada formula
ev_columnas <- function(pob) {
  c(pob$nombre, if (PARAM$EV$historia && any(!pob$original))
    as.vector(outer(pob$nombre[!pob$original], EV_SUFIJOS_HIST, paste, sep= "_")))
}

# la importancia de cada fila de la poblacion: la de su columna, mas la de su
# historia si es una formula con EV_HISTORIA
ev_importancia_unidad <- function(pob, imp) {
  u <- unname(imp[pob$nombre])
  if (PARAM$EV$historia && any(!pob$original)) {
    f <- which(!pob$original)
    for (k in EV_SUFIJOS_HIST) u[f] <- u[f] + unname(imp[paste0(pob$nombre[f], "_", k)])
  }
  u[is.na(u)] <- 0
  u
}

# guarda una formula: el vector entero en VF (para hijos y para la historia) y
# la muestra en V (para el LightGBM), con su historia si corresponde
ev_guardar_formula <- function(nombre, v, VF, V, s) {
  assign(nombre, v, envir= VF)
  assign(nombre, v[s$filas], envir= V)
  if (PARAM$EV$historia) {
    h <- ev_historia(v, dataset$numero_de_cliente)
    for (k in EV_SUFIJOS_HIST) assign(paste0(nombre, "_", k), h[[k]][s$filas], envir= V)
  }
}
ev_borrar_formulas <- function(nombres, VF, V) {
  if (!length(nombres)) return(invisible())
  rm(list= intersect(nombres, ls(VF)), envir= VF)
  todas <- c(nombres, as.vector(outer(nombres, EV_SUFIJOS_HIST, paste, sep= "_")))
  rm(list= intersect(todas, ls(V)), envir= V)
}
# el vector entero de un padre: una formula esta en VF, una original en el dataset
ev_completo <- function(nombre, VF) if (exists(nombre, envir= VF, inherits= FALSE)) get(nombre, envir= VF) else as.numeric(dataset[[nombre]])

# arma la matriz de las filas idx: las columnas de V, y las que no estan en V
# (las originales) directo del dataset, sin copias intermedias. Con n_can > 0
# agrega al final n_can canaritos (runif) con su propia semilla, sin tocar el
# RNG del evolutivo
ev_matriz <- function(nombres, V, idx, filas, n_can= 0L, semilla= 1L) {
  canaritos <- if (n_can > 0) paste0("canarito", seq_len(n_can)) else character(0)
  X <- matrix(0, nrow= length(idx), ncol= length(nombres) + n_can, dimnames= list(NULL, c(nombres, canaritos)))
  for (j in seq_along(nombres)) {
    v <- V[[nombres[j]]]
    X[, j] <- if (is.null(v)) dataset[[nombres[j]]][filas[idx]] else v[idx]
  }
  if (n_can > 0) {
    rng <- get0(".Random.seed", envir= globalenv())
    set.seed(semilla, kind = "L'Ecuyer-CMRG")
    for (j in seq_len(n_can)) X[, length(nombres) + j] <- runif(length(idx))
    if (!is.null(rng)) assign(".Random.seed", rng, envir= globalenv())
  }
  X
}

# entrena el LightGBM de canaritos con las columnas de V (env nombre -> vector)
# y devuelve:
#   imp     la importancia (Gain; 0 si no se uso) de cada columna
#   util    si la columna "sirve". Con EV_PISO_CANARITOS (default) es la regla
#           de la catedra (z635 celda 66): se agregan PARAM$CN$ratio x columnas
#           de canaritos al azar y sirve la que queda arriba de la mediana del
#           puesto de las canaritos + PARAM$CN$desvios desvios. Sin piso, sirve
#           si su importancia es > 0
#   meseta / maximo (validation) y meseta_prueba / maximo_prueba (mes de prueba)
# RAM: la matriz de training se arma, LightGBM la pasa a sus bins y se libera
# antes de entrenar
ev_entrenar <- function(nombres, V, s, semillas) {
  tr <- which(!s$es_valid & !s$es_prueba); va <- which(s$es_valid); pr <- which(s$es_prueba)
  n_can <- if (PARAM$EV$piso_canaritos) as.integer(ceiling(PARAM$CN$ratio * length(nombres))) else 0L
  canaritos <- if (n_can > 0) paste0("canarito", seq_len(n_can)) else character(0)
  Xtr <- ev_matriz(nombres, V, tr, s$filas, n_can, PARAM$semilla_primigenia + 1001L)
  dtr <- lgb.Dataset(Xtr, label= s$label[tr], weight= s$peso[tr], free_raw_data= TRUE,
                     params= list(max_bin= EV$param$max_bin, feature_pre_filter= FALSE))
  lgb.Dataset.construct(dtr)
  rm(Xtr); gc(verbose= FALSE)
  Xva <- ev_matriz(nombres, V, va, s$filas, n_can, PARAM$semilla_primigenia + 1002L)
  dva <- lgb.Dataset.create.valid(dtr, Xva, label= s$label[va], weight= s$peso[va])
  Xpr <- ev_matriz(nombres, V, pr, s$filas, n_can, PARAM$semilla_primigenia + 1003L)
  res <- lapply(semillas, function(sem) {
    p <- EV$param
    p$seed <- sem
    m <- lgb.train(data= dtr, valids= list(valid= dva), eval= fganancia_lgbm_meseta,
                   param= p, verbose= -100)
    gan_acum <- cumsum(s$gan_valid[order(-predict(m, Xva))])
    # el mes de prueba: solo se mira, no decide nada
    gan_pr <- cumsum(s$gan_prueba[order(-predict(m, Xpr))])
    im <- as.data.table(lgb.importance(m))
    im[, pos := .I]
    imp <- setNames(rep(0, length(nombres)), nombres)
    im_n <- im[Feature %in% nombres]
    imp[im_n$Feature] <- im_n$Gain
    util <- setNames(rep(FALSE, length(nombres)), nombres)
    if (n_can > 0) {
      pc <- im[Feature %in% canaritos, pos]
      # si ninguna canarito tuvo importancia, sirve todo lo que tuvo alguna
      umbral <- if (length(pc) >= 2) median(pc) + PARAM$CN$desvios * sd(pc) else if (length(pc) == 1) pc else nrow(im) + 1
      util[im_n[pos < umbral, Feature]] <- TRUE
    } else {
      umbral <- NA_real_
      util[im_n$Feature] <- im_n$Gain > 0
    }
    list(imp= imp, util= util, umbral= umbral, con_gain= sum(im_n$Gain > 0),
         meseta= mean(sort(gan_acum, decreasing= TRUE)[1:min(500, length(gan_acum))]),
         maximo= max(gan_acum),
         meseta_prueba= mean(sort(gan_pr, decreasing= TRUE)[1:min(500, length(gan_pr))]),
         maximo_prueba= max(gan_pr))
  })
  rm(dtr, dva, Xva, Xpr); gc(verbose= FALSE)
  list(imp= Reduce(`+`, lapply(res, `[[`, "imp")) / length(res),
       # con varias semillas, sirve si sirve en la mitad o mas
       util= (Reduce(`+`, lapply(res, `[[`, "util")) / length(res)) >= 0.5,
       canaritos= n_can, umbral= mean(sapply(res, `[[`, "umbral")),
       # columnas (sin canaritos) con gain > 0: si "sirven" es casi igual, el piso no corta
       con_gain= mean(sapply(res, `[[`, "con_gain")),
       meseta= mean(sapply(res, `[[`, "meseta")),
       maximo= mean(sapply(res, `[[`, "maximo")),
       meseta_prueba= mean(sapply(res, `[[`, "meseta_prueba")),
       maximo_prueba= mean(sapply(res, `[[`, "maximo_prueba")))
}

# si cada fila de la poblacion sirve: su columna, o con EV_HISTORIA cualquiera
# de las 6 columnas de una formula
ev_util_unidad <- function(pob, util) {
  u <- unname(util[pob$nombre])
  if (PARAM$EV$historia && any(!pob$original)) {
    f <- which(!pob$original)
    for (k in EV_SUFIJOS_HIST) u[f] <- u[f] | unname(util[paste0(pob$nombre[f], "_", k)])
  }
  u[is.na(u)] <- FALSE
  u
}

# la variable original de una columna del FE historico (x_lag1 -> x)
ev_base <- function(n) sub("_(lag1|lag2|delta1|delta2|tend6)$", "", n)

# un hijo a op b que solo reconstruye una columna que ya existe. Con dos
# versiones de la misma variable original: x - x_lagk (= x_deltak), x - x_deltak
# (= x_lagk) y sus inversas, x_lagk + x_deltak (= x), y x + x o x - x. El resto
# (x_lag1 - x_lag2, x_delta1 - x_delta2, x + x_lag1, x + x_tend6) es informacion
# que el FE historico no tiene y puede nacer
ev_trivial <- function(a, b, op) {
  if (!op %in% c("+", "-") || ev_base(a) != ev_base(b)) return(FALSE)
  if (a == b) return(TRUE)
  sufijo <- function(n) if (n == ev_base(n)) "" else substring(n, nchar(ev_base(n)) + 2L)
  par <- paste(sort(c(sufijo(a), sufijo(b))), collapse= "|")
  if (op == "-") par %in% c("|lag1", "|lag2", "|delta1", "|delta2") else par %in% c("delta1|lag1", "delta2|lag2")
}

# probabilidad de ser padre: ranking lineal por importancia. El primero pesa
# EV_PRESION y el ultimo 1; los empatados toman el peso del puesto mas bajo de
# su bloque, asi las de importancia 0 pesan 1. Nadie tiene probabilidad 0
ev_pesos <- function(imp) {
  n <- length(imp)
  if (PARAM$EV$seleccion == "azar" || n < 2) return(rep(1, n))
  r <- rank(-imp, ties.method= "max")
  1 + (PARAM$EV$presion - 1) * (n - r) / (n - 1)
}

ev_constante <- function(v) { u <- v[!is.na(v)]; length(u) < 2 || all(u == u[1]) }

EV <- new.env()   # estado del evolutivo

ev_grabar_estado <- function(g) {
  tmp <- paste0(CK_EV, ".tmp")
  saveRDS(list(g= g, pob= EV$pob, nacidas_acum= EV$nacidas_acum,
               rng= get(".Random.seed", envir= globalenv())), tmp)
  file.rename(tmp, CK_EV)
}

ev_log_generacion <- function(fila) {
  fwrite(fila, ARCH_EV_GEN, sep= "\t", append= file.exists(ARCH_EV_GEN))
  loguear(sprintf("evolutivo gen %d: poblacion %d (%d originales, %d generadas), %d sirven, %d pasan (X * 1)%s, nacen %d, no pasan %d, prof max %d, meseta %s, max %s, prueba meseta %s",
    fila$generacion, fila$poblacion, fila$originales_vivas, fila$generadas_vivas, fila$usadas, fila$copias,
    if (isTRUE(fila$uso_minimo_hijos)) " (minimo de hijos)" else "", fila$nacidas, fila$no_pasan,
    fila$prof_max, miles(fila$ganancia_meseta), miles(fila$ganancia_max), miles(fila$ganancia_meseta_prueba)))
}

correr_evolutivo <- function(materializar= TRUE) {
  originales <- setdiff(names(dataset)[sapply(dataset, is.numeric)], EV_NO_VARIABLES)
  originales <- originales[!startsWith(originales, "canarito")]
  tope <- if (PARAM$EV$tope > 0) PARAM$EV$tope else length(originales)
  EV$param <- ev_param()
  semillas <- PARAM$semilla_primigenia + seq_len(PARAM$EV$semillas_n) - 1
  loguear("evolutivo:", length(originales), "originales,",
          if (PARAM$EV$originales_fijas) paste("originales fijas, lugares para formulas", PARAM$EV$formulas,
            ", minimo de hijos", ceiling(PARAM$EV$min_hijos * PARAM$EV$formulas))
          else paste("tope", tope, ", minimo de hijos", ceiling(PARAM$EV$min_hijos * tope)),
          ", seleccion", PARAM$EV$seleccion,
          if (PARAM$EV$historia) ", formulas con historia (lag1, lag2, delta1, delta2, tend6)" else ", formulas sin historia",
          ", importancia con el LightGBM de canaritos (", EV$param$num_leaves, "hojas, early stopping",
          EV$param$early_stopping_rounds, ")")

  # la historia de las formulas necesita el dataset ordenado por cliente y mes
  setorder(dataset, numero_de_cliente, foto_mes)
  s <- ev_muestra()
  # V: la muestra de cada formula (y su historia); VF: el vector entero de cada
  # formula. Las originales no se copian: se leen del dataset
  V <- new.env(); VF <- new.env()

  if (file.exists(CK_EV)) {
    e <- readRDS(CK_EV)
    EV$pob <- e$pob; EV$nacidas_acum <- e$nacidas_acum
    g0 <- e$g
    assign(".Random.seed", e$rng, envir= globalenv())
    for (i in which(!EV$pob$original))
      ev_guardar_formula(EV$pob$nombre[i], ev_evaluar(EV$pob$formula[i], dataset), VF, V, s)
    loguear("evolutivo: retomo despues de la generacion", g0)
    if (!"util" %in% names(EV$pob)) EV$pob[, util := importancia > 0]
    # si la VM se corto despues de escribir los logs de una generacion y antes
    # de su checkpoint, esa generacion se repite: se sacan sus filas
    for (a in c(ARCH_EV_GEN, ARCH_EV_NAC)) if (file.exists(a)) {
      d <- fread(a, sep= "\t")
      if (any(d$generacion > g0)) fwrite(d[generacion <= g0], a, sep= "\t")
    }
  } else {
    set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
    t0 <- Sys.time()
    # generacion 0: solo las originales. Es tambien la referencia de ganancia
    r <- ev_entrenar(originales, V, s, semillas)
    EV$pob <- data.table(nombre= originales, formula= ev_nombre_en_formula(originales),
                         nacio= 0L, prof= 0L, original= TRUE, importancia= unname(r$imp[originales]),
                         util= unname(r$util[originales]))
    EV$nacidas_acum <- 0L
    g0 <- 0L
    ev_log_generacion(data.table(generacion= 0L, poblacion_inicio= length(originales),
      usadas= sum(r$util[originales]), copias= 0L, uso_minimo_hijos= FALSE,
      canaritos= r$canaritos, umbral_canaritos= round(r$umbral, 1), con_gain= round(r$con_gain),
      columnas= length(originales),
      nacidas= 0L, no_pasan= 0L, poblacion= length(originales),
      originales_vivas= length(originales), generadas_vivas= 0L, prof_max= 0L, prof_media= 0,
      padres_de_generaciones_distintas= NA_real_, nacidas_acumuladas= 0L,
      importancia_generadas= 0, ganancia_meseta= r$meseta, ganancia_max= r$maximo,
      ganancia_meseta_prueba= r$meseta_prueba, ganancia_max_prueba= r$maximo_prueba,
      segundos= round(as.numeric(difftime(Sys.time(), t0, units= "secs")))))
    ev_grabar_estado(0L)
  }

  for (g in seq_len(PARAM$EV$generaciones)[seq_len(PARAM$EV$generaciones) > g0]) {
    t0 <- Sys.time()
    pob <- EV$pob
    pob_inicio <- nrow(pob)
    peso_padres <- ev_pesos(pob$importancia)

    # --- reproduccion (X * 1): pasa igual lo que sirve (le gana a las
    # canaritos, ver ev_entrenar; sin piso, importancia > 0). Con EV_ORIGINALES_FIJAS (default) las originales no compiten: pasan
    # todas, siempre, y la genetica AGREGA formulas en vez de reemplazar
    # columnas. Las formulas compiten entre ellas por EV_FORMULAS lugares: pasan
    # las usadas, por importancia, y el resto de los lugares son hijos. Sin
    # originales fijas compite toda la poblacion por el tope. En los dos casos,
    # si las usadas no entran todas dejandole a los hijos el minimo de
    # EV_MIN_HIJOS, pasan las de mas importancia ------------------------------
    if (PARAM$EV$originales_fijas) {
      compiten <- which(!pob$original)
      cupo <- PARAM$EV$formulas
    } else {
      compiten <- seq_len(nrow(pob))
      cupo <- tope
    }
    max_copias <- cupo - ceiling(PARAM$EV$min_hijos * cupo)
    usadas <- compiten[pob$util[compiten]]
    n_copias <- min(length(usadas), max_copias)
    uso_minimo <- length(usadas) > max_copias
    copias <- if (PARAM$EV$seleccion == "azar") compiten[sample(length(compiten), n_copias)] else
      usadas[order(-pob$importancia[usadas])][seq_len(n_copias)]
    pasa <- seq_len(nrow(pob)) %in% copias | (PARAM$EV$originales_fijas & pob$original)
    sigue <- pob[pasa]
    no_pasa <- pob[!pasa]

    # --- cruza: padre1 op padre2 en los lugares libres, padres de cualquier
    # generacion (tambien las que no pasan: su informacion sigue en el hijo) -
    hijos <- list()
    vistas <- pob$formula
    intentos <- 0
    n_cruzas <- cupo - n_copias
    while (length(hijos) < n_cruzas && intentos < 5 * n_cruzas) {
      intentos <- intentos + 1
      p <- sample(nrow(pob), 2, prob= peso_padres)
      op <- sample(c("+", "-", "*", "/"), 1)
      fa <- pob$formula[p[1]]; fb <- pob$formula[p[2]]
      # + y * conmutan: una sola forma de escribirlas
      if (op %in% c("+", "*") && fb < fa) { p <- rev(p); tmp <- fa; fa <- fb; fb <- tmp }
      formula <- paste0("(", fa, " ", op, " ", fb, ")")
      if (formula %in% vistas) next
      # hijos que no agregan nada: los que reconstruyen una columna del FE
      # historico (ver ev_trivial), y b - a si ya existe a - b (para un arbol
      # son la misma)
      if (all(pob$original[p]) && ev_trivial(pob$nombre[p[1]], pob$nombre[p[2]], op)) next
      if (op == "-" && paste0("(", fb, " - ", fa, ")") %in% vistas) next
      vistas <- c(vistas, formula)
      # el hijo se calcula sobre el dataset entero: su historia necesita los
      # meses que no estan en la muestra (el lag1 de 202105 es 202104)
      v <- EV_OPS[[op]](ev_completo(pob$nombre[p[1]], VF), ev_completo(pob$nombre[p[2]], VF))
      if (ev_constante(v)) next
      nombre <- sprintf("ev%02d_%04d", g, length(hijos) + 1L)
      ev_guardar_formula(nombre, v, VF, V, s)
      hijos[[length(hijos) + 1L]] <- data.table(nombre= nombre, formula= formula, nacio= as.integer(g),
        prof= max(pob$prof[p]) + 1L, original= FALSE, importancia= NA_real_, util= NA,
        padre1= pob$nombre[p[1]], padre2= pob$nombre[p[2]],
        nacio_padre1= pob$nacio[p[1]], nacio_padre2= pob$nacio[p[2]])
    }
    hijos <- rbindlist(hijos)
    cop <- if (PARAM$EV$originales_fijas) sigue[original == FALSE] else sigue
    nac <- rbind(
      if (nrow(hijos)) hijos[, .(generacion= g, nombre, formula, padre1, padre2, nacio_padre1, nacio_padre2)],
      if (nrow(cop)) cop[, .(generacion= g, nombre, formula= paste0(formula, " * 1"),
        padre1= nombre, padre2= "uno", nacio_padre1= nacio, nacio_padre2= NA_integer_)])
    if (!is.null(nac) && nrow(nac)) fwrite(nac, ARCH_EV_NAC, sep= "\t", append= file.exists(ARCH_EV_NAC))
    EV$nacidas_acum <- EV$nacidas_acum + nrow(hijos)

    # --- la generacion nueva: copias + hijos, medidas juntas -------------------
    EV$pob <- rbind(sigue, if (nrow(hijos)) hijos[, names(pob), with= FALSE])
    # lo que no pasa ya no se usa, ni como padre: sale de V antes de entrenar,
    # asi en RAM hay una sola poblacion. Al retomar, V se rearma desde el dataset
    ev_borrar_formulas(no_pasa[original == FALSE, nombre], VF, V)
    cols <- ev_columnas(EV$pob)
    r <- ev_entrenar(cols, V, s, semillas)
    EV$pob[, importancia := ev_importancia_unidad(EV$pob, r$imp)]
    EV$pob[, util := ev_util_unidad(EV$pob, r$util)]

    ev_log_generacion(data.table(generacion= g, poblacion_inicio= pob_inicio,
      usadas= sum(EV$pob$util), copias= nrow(cop), uso_minimo_hijos= uso_minimo,
      canaritos= r$canaritos, umbral_canaritos= round(r$umbral, 1), con_gain= round(r$con_gain),
      columnas= length(cols),
      nacidas= nrow(hijos), no_pasan= nrow(no_pasa), poblacion= nrow(EV$pob),
      originales_vivas= sum(EV$pob$original), generadas_vivas= sum(!EV$pob$original),
      prof_max= max(EV$pob$prof), prof_media= round(mean(EV$pob$prof), 2),
      padres_de_generaciones_distintas= if (nrow(hijos)) round(mean(hijos$nacio_padre1 != hijos$nacio_padre2), 3) else NA_real_,
      nacidas_acumuladas= EV$nacidas_acum,
      importancia_generadas= round(sum(EV$pob[original == FALSE, importancia]) / max(sum(EV$pob$importancia), 1e-12), 4),
      ganancia_meseta= r$meseta, ganancia_max= r$maximo,
      ganancia_meseta_prueba= r$meseta_prueba, ganancia_max_prueba= r$maximo_prueba,
      segundos= round(as.numeric(difftime(Sys.time(), t0, units= "secs")))))
    ev_grabar_estado(g)
    gc(verbose= FALSE)
    if (PARAM$EV$mail_cada_gen > 0 && g %% PARAM$EV$mail_cada_gen == 0)
      mandar_mail(sprintf("evolutivo generacion %d", g), texto_evolutivo(), hilo= "iteraciones")
  }

  # --- la poblacion final ---------------------------------------------------
  pob <- EV$pob
  fwrite(pob[order(-importancia)][, historia := PARAM$EV$historia], ARCH_EV_POB, sep= "\t")
  # las mejores formulas: las generadas que el ultimo modelo uso, por importancia
  fwrite(pob[original == FALSE & util == TRUE][order(-importancia),
             .(nombre, formula, nacio, prof, importancia)], ARCH_EV_MEJ, sep= "\t")
  if (materializar) ev_materializar(pob, originales)
  texto_evolutivo()
}

# mete una poblacion al dataset entero: las formulas se recalculan con los
# nombres de las columnas originales. RAM: cada original muerta se borra
# apenas la calculo la ultima formula que la usa (si no, el pico es el dataset
# con todas las generadas Y todas las muertas)
ev_materializar <- function(pob, originales) {
  muertas_ori <- setdiff(originales, pob$nombre)
  generadas <- which(!pob$original)
  usa <- lapply(pob$formula[generadas], function(f) all.names(str2lang(f), functions= FALSE))
  faltan <- setdiff(c(pob$nombre[pob$original], unlist(usa)), names(dataset))
  if (length(faltan)) stop("la poblacion usa columnas que no estan en el dataset: ",
                           paste(head(faltan, 10), collapse= ", "))
  ultima <- vapply(muertas_ori, function(n) max(c(0L, which(vapply(usa, function(u) n %in% u, logical(1))))), integer(1))
  borrar <- function(cols) if (PARAM$EV$borrar_muertas && length(cols)) dataset[, (cols) := NULL]
  borrar(muertas_ori[ultima == 0L])
  setorder(dataset, numero_de_cliente, foto_mes)
  for (j in seq_along(generadas)) {
    i <- generadas[j]
    v <- ev_evaluar(pob$formula[i], dataset)
    set(dataset, j= pob$nombre[i], value= v)
    if (PARAM$EV$historia) {
      h <- ev_historia(v, dataset$numero_de_cliente)
      for (k in EV_SUFIJOS_HIST) set(dataset, j= paste0(pob$nombre[i], "_", k), value= h[[k]])
    }
    borrar(muertas_ori[ultima == j])
  }
  loguear("evolutivo: el dataset queda con", length(generadas), "generadas",
          if (PARAM$EV$historia) "(cada una con lag1, lag2, delta1, delta2 y tend6)" else "", "y",
          sum(pob$original), "originales de", length(originales),
          if (PARAM$EV$borrar_muertas) "(las originales muertas se borran)" else "(las originales muertas quedan)")
}

# EV_POBLACION_DESDE: la poblacion de una busqueda entra al dataset sin evolucionar
ev_desde <- function(arch) {
  if (!file.exists(arch)) stop("EV_POBLACION_DESDE: no existe ", arch)
  pob <- fread(arch, sep= "\t")
  # la historia de las formulas es la de la busqueda que armo la poblacion
  if ("historia" %in% names(pob)) {
    h <- isTRUE(as.logical(pob$historia[1]))
    if (h != PARAM$EV$historia)
      loguear("evolutivo: la busqueda tenia EV_HISTORIA =", h, "y esta corrida no: uso el de la busqueda")
    PARAM$EV$historia <<- h
  }
  originales <- setdiff(names(dataset)[sapply(dataset, is.numeric)], EV_NO_VARIABLES)
  originales <- originales[!startsWith(originales, "canarito")]
  loguear("evolutivo: poblacion de", arch, "-", nrow(pob), "columnas,", sum(!pob$original), "formulas")
  ev_materializar(pob, originales)
  paste0("FE evolutivo: poblacion de ", arch, " (", nrow(pob), " columnas, ", sum(!pob$original),
         " formulas; no se evoluciono en esta corrida)")
}

# la comparacion de la busqueda: la ganancia del LightGBM de la importancia en
# el mes de prueba, generacion por generacion, con seleccion por importancia y
# (si corrio el control) al azar, y las mejores formulas
ev_comparacion <- function() {
  leer <- function(a) if (file.exists(a)) fread(a, sep= "\t") else NULL
  imp <- leer("evolutivo_generaciones.txt"); azar <- leer("evolutivo_generaciones_azar.txt")
  mejores <- leer("evolutivo_mejores.txt")
  fila <- function(g, d) if (is.null(d) || !(g %in% d$generacion)) "            -" else
    sprintf("%13s", miles(d[generacion == g, ganancia_meseta_prueba]))
  gens <- sort(unique(c(imp$generacion, azar$generacion)))
  base <- imp[generacion == 0, ganancia_meseta_prueba]
  paste0("Busqueda de formulas: ganancia meseta del LightGBM de la importancia en ",
    PARAM$EV$imp$prueba, " (mes que el evolutivo no usa para decidir)\n\n",
    "  gen    importancia          azar   importancia - gen 0\n",
    paste(sprintf("  %3d  %s  %s  %13s", gens, sapply(gens, fila, imp), sapply(gens, fila, azar),
      sapply(gens, function(g) if (g %in% imp$generacion) miles(imp[generacion == g, ganancia_meseta_prueba] - base) else "-")),
      collapse= "\n"),
    "\n\nComo leerlo: si la importancia encuentra formulas que sirven, su curva sube\n",
    "y queda por encima de la del azar. Una diferencia de pocos millones entre dos\n",
    "generaciones puede ser solo el cambio de columnas: lo que cuenta es la tendencia\n",
    "y la distancia con el azar.\n\n",
    if (!is.null(mejores) && nrow(mejores)) paste0("Las 20 mejores formulas (por importancia en la ultima generacion):\n",
      paste(sprintf("  %8.5f  %s", head(mejores$importancia, 20), head(mejores$formula, 20)), collapse= "\n"),
      "\n\nTodas en evolutivo_mejores.txt; la poblacion completa en evolutivo_poblacion.txt\n",
      "(para la corrida completa: EV_POBLACION_DESDE con esa ruta)") else "")
}

# EV_SOLO: termina la corrida despues de la busqueda (y del control al azar)
terminar_busqueda <- function() {
  if (PARAM$EV$control) {
    etapa("FE evolutivo de control (seleccion al azar)")
    PARAM$EV$seleccion <<- "azar"
    rm(list= ls(EV), envir= EV)
    ev_archivos("_azar")
    resumen_azar <- correr_evolutivo(materializar= FALSE)
    ev_archivos()
    PARAM$EV$seleccion <<- "importancia"
    fin_etapa(resumen_azar)
  }
  resumen <- ev_comparacion()
  writeLines(resumen, "evolutivo_busqueda.txt")
  loguear(resumen)
  mandar_mail("TERMINO (busqueda de formulas)", resumen)
  writeLines(format(Sys.time()), "TERMINADO")
  quit(save= "no", status= 0)
}

texto_evolutivo <- function() {
  if (PARAM$GA$modo == "nada") return("Sin FE evolutivo (GA_MODO=nada).")
  if (!file.exists(ARCH_EV_GEN)) return("El evolutivo no termino ninguna generacion.")
  gen <- fread(ARCH_EV_GEN, sep= "\t")
  paste0("FE evolutivo (seleccion ", PARAM$EV$seleccion, ", tope ", max(gen$poblacion), "):\n",
    paste(sprintf("  gen %2d: poblacion %5d | sirven %5d | pasan (X * 1) %5d%s | nacen %4d | originales vivas %5d | generadas %5d | prof max %2d | imp generadas %5.1f%% | meseta %s | max %s | prueba %s",
      gen$generacion, gen$poblacion, gen$usadas, gen$copias, ifelse(gen$uso_minimo_hijos, "*", " "),
      gen$nacidas, gen$originales_vivas, gen$generadas_vivas, gen$prof_max,
      100 * gen$importancia_generadas, miles(gen$ganancia_meseta), miles(gen$ganancia_max),
      miles(gen$ganancia_meseta_prueba)), collapse= "\n"),
    "\nFormulas nacidas en total: ", miles(max(gen$nacidas_acumuladas)),
    if (any(gen$uso_minimo_hijos)) "\n* = las usadas no entraban todas: se uso el minimo de lugares para hijos (EV_MIN_HIJOS)" else "")
}


# GRUPO A: Kaggle se prepara y se prueba al arrancar. Si no anda, avisa por
# mail ahora; la corrida sigue y los archivos para subir quedan en ./kaggle
KAGGLE_OK <- FALSE
preparar_kaggle <- function(avisar) {
  dir.create("~/.kaggle", showWarnings= FALSE)
  if (!file.exists("~/.kaggle/kaggle.json") && file.exists(Sys.getenv("KAGGLE_JSON"))) {
    file.copy(Sys.getenv("KAGGLE_JSON"), "~/.kaggle/kaggle.json")
    Sys.chmod("~/.kaggle/kaggle.json", "600")
  }
  kaggle_bin <<- Sys.getenv("KAGGLE_BIN", "kaggle")
  probar_kaggle <- function() {
    out <- suppressWarnings(tryCatch(system2(kaggle_bin, "--version", stdout= TRUE, stderr= TRUE),
      error= function(e) structure("no se pudo ejecutar", status= 127L)))
    is.null(attr(out, "status"))
  }
  if (!probar_kaggle()) {
    loguear("kaggle no responde: instalo el CLI con pip")
    # la imagen tiene Python "externally managed" (PEP 668): sin este flag pip
    # se niega. Con --user solo toca ~/.local, no el Python del sistema
    system2("python3", c("-m", "pip", "install", "--user", "--quiet", "--break-system-packages", "kaggle"))
    if (kaggle_bin == "kaggle") kaggle_bin <<- path.expand("~/.local/bin/kaggle")
  }
  KAGGLE_OK <<- file.exists("~/.kaggle/kaggle.json") && probar_kaggle()
  if (!KAGGLE_OK && avisar) mandar_mail("AVISO: Kaggle no funciona",
    paste0("No hay ~/.kaggle/kaggle.json (KAGGLE_JSON=", Sys.getenv("KAGGLE_JSON"),
           ") o el CLI no responde. La corrida sigue; al final los archivos quedan en ",
           carpeta_exp, "/kaggle y no se suben."))
}
if (GRUPO == "A" && KAGGLE_SUBIR) preparar_kaggle(avisar= TRUE)

CK_HIST <- "checkpoints/dataset_fe_historico.fst"
CK_CANARITOS <- "checkpoints/dataset_canaritos.fst"

mandar_mail("arranco",
  paste0("Corrida ", NOMBRE_CORRIDA, ", grupo ", GRUPO, ", semilla del pipeline ", PARAM$semilla_primigenia, "\n",
         "Version del script: ", WF_VERSION, "\n",
         "Carpeta: ", carpeta_exp, "\n",
         "Semillas: ", paste(PARAM$FT$semillas, collapse= ", "), "\n",
         "FE evolutivo: modo ", PARAM$GA$modo,
         if (PARAM$GA$modo == "evolutivo") paste0(", seleccion ", PARAM$EV$seleccion,
           ", ", PARAM$EV$generaciones, " generaciones, tope ",
           if (PARAM$EV$tope > 0) PARAM$EV$tope else "columnas iniciales",
           ", minimo de hijos ", PARAM$EV$min_hijos, ", presion ", PARAM$EV$presion,
           if (PARAM$EV$originales_fijas) paste0(", originales fijas, ", PARAM$EV$formulas, " lugares para formulas") else ", originales compiten",
           ", importancia con el LightGBM de canaritos (entrena ",
           paste(range(PARAM$EV$imp$training), collapse= "-"), " con ", PARAM$EV$imp$undersampling,
           " de los CONTINUA, valida ", paste(PARAM$EV$imp$validation, collapse= ","), ")"), "\n",
         "Canaritos: ", if (PARAM$CN$run) "si" else "no", ", FT_ALFA ", PARAM$FT$alfa, "\n",
         if (nzchar(PARAM$BO$ht_fijo)) paste0("HT fijo: ", PARAM$BO$ht_fijo, "\n"),
         "Final train: undersampling ", PARAM$FT$undersampling, " de los CONTINUA\n",
         "Checkpoints existentes: ",
         paste(c(if (file.exists(CK_HIST)) "FE historico", if (file.exists(CK_CANARITOS)) "canaritos",
                 if (file.exists("HT.RDATA")) "BO (HT.RDATA)", if (file.exists("BO_log.txt")) "BO terminada"),
               collapse= ", ")))

# ===========================================================================
# 9.7.1 PREPROCESAMIENTO DEL DATASET
# ===========================================================================

if (!file.exists(CK_CANARITOS)) {

  if (!file.exists(CK_HIST)) {

    etapa("lectura del dataset")
    dataset <- fread(Sys.getenv("DATASET_PATH"))
    fin_etapa()

    etapa("CA catastrophe analysis")
# --- z635 celda 19 ---
if( !require("mice")) install.packages("mice", repos = "http://cran.us.r-project.org")
require("mice")

# --- z635 celda 20 ---
# Escrito por alumnos de  Universidad Austral  Rosario

Corregir_MICE <- function(pcampo, pmeses) {

  meth <- rep("", ncol(dataset))
  names(meth) <- colnames(dataset)
  meth[names(meth) == pcampo] <- "sample"

  # llamada a mice  !
  imputacion <- mice(dataset,
    method = meth,
    maxit = 5,
    m = 1,
    seed = 7)

  tbl <- mice::complete(dataset)

  dataset[, paste0(pcampo) := ifelse(foto_mes %in% pmeses, tbl[, get(pcampo)], get(pcampo))]

}

# --- z635 celda 21 ---
Corregir_interpolar <- function(pcampo, pmeses) {

  tbl <- dataset[, list(
    "v1" = shift(get(pcampo), 1, type = "lag"),
    "v2" = shift(get(pcampo), 1, type = "lead")
  ),
  by = eval(PARAM$dataset_metadata$entity_id)
  ]

  tbl[, paste0(PARAM$dataset_metadata$entity_id) := NULL]
  tbl[, promedio := rowMeans(tbl, na.rm = TRUE)]

  dataset[
    ,
    paste0(pcampo) := ifelse(!(foto_mes %in% pmeses),
      get(pcampo),
      tbl$promedio
    )
  ]
}

# --- z635 celda 22 ---
AsignarNA_campomeses <- function(pcampo, pmeses) {

  if( pcampo %in% colnames( dataset ) ) {

    dataset[ foto_mes %in% pmeses, paste0(pcampo) := NA ]
  }
}

# --- z635 celda 23 ---

Corregir_atributo <- function(pcampo, pmeses, pmetodo)
{
  # si el campo no existe en el dataset, Afuera !
  if( !(pcampo %in% colnames( dataset )) )
    return( 1 )

  # llamo a la funcion especializada que corresponde
  switch( pmetodo,
    "MachineLearning"     = AsignarNA_campomeses(pcampo, pmeses),
    "EstadisticaClasica"  = Corregir_interpolar(pcampo, pmeses),
    "MICE"                = Corregir_MICE(pcampo, pmeses),
  )

  return( 0 )
}

# --- z635 celda 24 ---

Corregir_Rotas <- function(dataset, pmetodo) {
  gc(verbose= FALSE)
  cat( "inicio Corregir_Rotas()\n")
  # acomodo los errores del dataset

  Corregir_atributo("active_quarter", c(202006), pmetodo) # 1
  Corregir_atributo("internet", c(202006), pmetodo) # 2

  Corregir_atributo("mrentabilidad", c(201905, 201910, 202006), pmetodo) # 3
  Corregir_atributo("mrentabilidad_annual", c(201905, 201910, 202006), pmetodo) # 4

  Corregir_atributo("mcomisiones", c(201905, 201910, 202006), pmetodo) # 5

  Corregir_atributo("mactivos_margen", c(201905, 201910, 202006), pmetodo) # 6
  Corregir_atributo("mpasivos_margen", c(201905, 201910, 202006), pmetodo) # 7

  Corregir_atributo("mcuentas_saldo", c(202006), pmetodo) # 8

  Corregir_atributo("ctarjeta_debito_transacciones", c(202006), pmetodo) # 9

  Corregir_atributo("mautoservicio", c(202006), pmetodo) # 10

  Corregir_atributo("ctarjeta_visa_transacciones", c(202006), pmetodo) # 11
  Corregir_atributo("mtarjeta_visa_consumo", c(202006), pmetodo) # 12

  Corregir_atributo("ctarjeta_master_transacciones", c(202006), pmetodo) # 13
  Corregir_atributo("mtarjeta_master_consumo", c(202006), pmetodo) # 14

  Corregir_atributo("ctarjeta_visa_debitos_automaticos", c(201904), pmetodo) # 15
  Corregir_atributo("mttarjeta_visa_debitos_automaticos", c(201904), pmetodo) # 16

  Corregir_atributo("ccajeros_propios_descuentos",
    c(201910, 202002, 202006, 202009, 202010, 202102), pmetodo) # 17

  Corregir_atributo("mcajeros_propios_descuentos",
    c(201910, 202002, 202006, 202009, 202010, 202102), pmetodo) # 18

  Corregir_atributo("ctarjeta_visa_descuentos",
    c(201910, 202002, 202006, 202009, 202010, 202102), pmetodo) # 19

  Corregir_atributo("mtarjeta_visa_descuentos",
    c(201910, 202002, 202006, 202009, 202010, 202102), pmetodo) # 20

  Corregir_atributo("ctarjeta_master_descuentos",
    c(201910, 202002, 202006, 202009, 202010, 202102), pmetodo) # 21

  Corregir_atributo("mtarjeta_master_descuentos",
    c(201910, 202002, 202006, 202009, 202010, 202102), pmetodo) # 22

  Corregir_atributo("ccomisiones_otras", c(201905, 201910, 202006), pmetodo) # 23
  Corregir_atributo("mcomisiones_otras", c(201905, 201910, 202006), pmetodo) # 24

  Corregir_atributo("cextraccion_autoservicio", c(202006), pmetodo) # 25
  Corregir_atributo("mextraccion_autoservicio", c(202006), pmetodo) # 26

  Corregir_atributo("ccheques_depositados", c(202006), pmetodo) # 27
  Corregir_atributo("mcheques_depositados", c(202006), pmetodo) # 28
  Corregir_atributo("ccheques_emitidos", c(202006), pmetodo) # 29
  Corregir_atributo("mcheques_emitidos", c(202006), pmetodo) # 30
  Corregir_atributo("ccheques_depositados_rechazados", c(202006), pmetodo) # 31
  Corregir_atributo("mcheques_depositados_rechazados", c(202006), pmetodo) # 32
  Corregir_atributo("ccheques_emitidos_rechazados", c(202006), pmetodo) # 33
  Corregir_atributo("mcheques_emitidos_rechazados", c(202006), pmetodo) # 34

  Corregir_atributo("tcallcenter", c(202006), pmetodo) # 35
  Corregir_atributo("ccallcenter_transacciones", c(202006), pmetodo) # 36

  Corregir_atributo("thomebanking", c(202006), pmetodo) # 37
  Corregir_atributo("chomebanking_transacciones", c(201910, 202006), pmetodo) # 38

  Corregir_atributo("ccajas_transacciones", c(202006), pmetodo) # 39
  Corregir_atributo("ccajas_consultas", c(202006), pmetodo) # 40

  Corregir_atributo("ccajas_depositos", c(202006, 202105), pmetodo) # 41

  Corregir_atributo("ccajas_extracciones", c(202006), pmetodo) # 41
  Corregir_atributo("ccajas_otras", c(202006), pmetodo) # 43

  Corregir_atributo("catm_trx", c(202006), pmetodo) # 44
  Corregir_atributo("matm", c(202006), pmetodo) # 45
  Corregir_atributo("catm_trx_other", c(202006), pmetodo) # 46
  Corregir_atributo("matm_other", c(202006), pmetodo) # 47

  cat( "fin Corregir_rotas()\n")
}

# --- z635 celda 25 ---
# resuelvo el Catastrophe Analysis

setorder( dataset, numero_de_cliente, foto_mes )

PARAM$CA$metodo= "MachineLearning"

if( PARAM$CA$metodo %in% c("MachineLearning", "EstadisticaClasica", "MICE") )
  Corregir_Rotas(dataset, PARAM$CA$metodo)

    fin_etapa()

    etapa("DR data drifting")
# --- z635 celda 27 ---
# meses que me interesan para el ajuste de variables monetarias
vfoto_mes <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105, 202106,
  202107, 202108, 202109
)

# --- z635 celda 28 ---
# los valores que siguen fueron calculados por alumnos

# momento 1.0  31-dic-2020 a las 23:59
vIPC <- c(
  1.9903030878, 1.9174403544, 1.8296186587,
  1.7728862972, 1.7212488323, 1.6776304408,
  1.6431248196, 1.5814483345, 1.4947526791,
  1.4484037589, 1.3913580777, 1.3404220402,
  1.3154288912, 1.2921698342, 1.2472681797,
  1.2300475145, 1.2118694724, 1.1881073259,
  1.1693969743, 1.1375456949, 1.1065619600,
  1.0681100000, 1.0370000000, 1.0000000000,
  0.9680542110, 0.9344152616, 0.8882274350,
  0.8532444140, 0.8251880213, 0.8003763543,
  0.7763107219, 0.7566381305, 0.7289384687
)

vdolar_blue <- c(
   39.045455,  38.402500,  41.639474,
   44.274737,  46.095455,  45.063333,
   43.983333,  54.842857,  61.059524,
   65.545455,  66.750000,  72.368421,
   77.477273,  78.191667,  82.434211,
  101.087500, 126.236842, 125.857143,
  130.782609, 133.400000, 137.954545,
  170.619048, 160.400000, 153.052632,
  157.900000, 149.780952, 143.615385,
  146.250000, 153.550000, 162.000000,
  178.478261, 180.878788, 184.357143
)

vdolar_oficial <- c(
   38.430000,  39.428000,  42.542105,
   44.354211,  46.088636,  44.955000,
   43.751429,  54.650476,  58.790000,
   61.403182,  63.012632,  63.011579,
   62.983636,  63.580556,  65.200000,
   67.872000,  70.047895,  72.520952,
   75.324286,  77.488500,  79.430909,
   83.134762,  85.484737,  88.181667,
   91.474000,  93.997778,  96.635909,
   98.526000,  99.613158, 100.619048,
  101.619048, 102.569048, 103.781818
)

vUVA <- c(
  2.001408838932958,  1.950325472789153,  1.89323032351521,
  1.8247220405493787, 1.746027787673673,  1.6871348409529485,
  1.6361678865622313, 1.5927529755859773, 1.5549162794128493,
  1.4949100586391746, 1.4197729500774545, 1.3678188186372326,
  1.3136508617223726, 1.2690535173062818, 1.2381595983200178,
  1.211656735577568,  1.1770808941405335, 1.1570338657445522,
  1.1388769475653255, 1.1156993751209352, 1.093638313080772,
  1.0657171590878205, 1.0362173587708712, 1.0,
  0.9669867858358365, 0.9323750098728378, 0.8958202912590305,
  0.8631993702994263, 0.8253893405524657, 0.7928918905364516,
  0.7666323845128089, 0.7428976357662823, 0.721615762047849
)

# --- z635 celda 29 ---
tb_indices <- as.data.table( list(
  "IPC" = vIPC,
  "dolar_blue" = vdolar_blue,
  "dolar_oficial" = vdolar_oficial,
  "UVA" = vUVA
  )
)

tb_indices[[ 'foto_mes' ]] <- vfoto_mes

tb_indices

# --- z635 celda 30 ---
drift_UVA <- function(campos_monetarios) {
  cat( "inicio drift_UVA()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD * i.UVA,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_UVA()\n")
}

# --- z635 celda 31 ---
drift_dolar_oficial <- function(campos_monetarios) {
  cat( "inicio drift_dolar_oficial()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD / i.dolar_oficial,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_dolar_oficial()\n")
}

# --- z635 celda 32 ---
drift_dolar_blue <- function(campos_monetarios) {
  cat( "inicio drift_dolar_blue()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD / i.dolar_blue,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_dolar_blue()\n")
}

# --- z635 celda 33 ---
drift_deflacion <- function(campos_monetarios) {
  cat( "inicio drift_deflacion()\n")

  dataset[tb_indices,
    on = c("foto_mes"),
    (campos_monetarios) := .SD * i.IPC,
    .SDcols = campos_monetarios
  ]

  cat( "fin drift_deflacion()\n")
}

# --- z635 celda 34 ---
drift_rank_simple <- function(campos_drift) {

  cat( "inicio drift_rank_simple()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
      (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = list(foto_mes)]
    dataset[, (campo) := NULL]
  }
  cat( "fin drift_rank_simple()\n")
}

# --- z635 celda 35 ---
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {

  cat( "inicio drift_rank_cero_fijo()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
      frank(get(campo), ties.method = "random") / .N, by = list(foto_mes)]

    dataset[get(campo) < 0, paste0(campo, "_rank") :=
      -frank(-get(campo), ties.method = "random") / .N, by = list(foto_mes)]
    dataset[, (campo) := NULL]
  }
  cat("\n")
  cat( "fin drift_rank_cero_fijo()\n")
}

# --- z635 celda 36 ---
# funcion provista por alumnos amantes de la Estadistica Clasica

drift_estandarizar <- function(campos_drift) {

  cat( "inicio drift_estandarizar()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") :=
      (get(campo) -mean(get(campo), na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
      by = list(foto_mes)]

    dataset[, (campo) := NULL]
  }
  cat( "fin drift_estandarizar()\n")
}

# --- z635 celda 37 ---
# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
  "^(m|Visa_m|Master_m|vm_m)"]

campos_monetarios

# --- z635 celda 38 ---
# ejecuto el Data Drifting
setorder( dataset, numero_de_cliente, foto_mes )


PARAM$DR$metodo <- "deflacion"

switch(PARAM$DR$metodo,
  "ninguno"        = cat("No hay correccion del data drifting"),
  "rank_simple"    = drift_rank_simple(campos_monetarios),
  "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
  "deflacion"      = drift_deflacion(campos_monetarios),
  "dolar_blue"     = drift_dolarblue(campos_monetarios),
  "dolar_oficial"  = drift_dolaroficial(campos_monetarios),
  "UVA"            = drift_UVA(campos_monetarios),
  "estandarizar"   = drift_estandarizar(campos_monetarios)
)

    fin_etapa()

    etapa("FE intra-mes")
# --- z635 celda 41 ---
if( !require("lubridate")) install.packages("lubridate", repos = "http://cran.us.r-project.org")
require("lubridate")

# --- z635 celda 42 ---
# esta funcion atributos presentes existe debido a que las modalidades poseen datasets con distinta cantidad de campos
atributos_presentes <- function( patributos )
{
  atributos <- unique( patributos )
  comun <- intersect( atributos, colnames(dataset) )

  return(  length( atributos ) == length( comun ) )
}

# --- z635 celda 43 ---
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  cat( "inicio AgregarVariables_IntraMes()\n")
  gc(verbose= FALSE)
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # el mes 1,2, ..12
  if( atributos_presentes( c("foto_mes") ))
    dataset[, kmes := foto_mes %% 100]

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  # ya que seria injusto considerar las transacciones medidas en menor tiempo
  if( atributos_presentes( c("ctrx_quarter") ))
    dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]

  if( atributos_presentes( c("ctrx_quarter", "cliente_antiguedad") ))
    dataset[
      cliente_antiguedad == 3,
      ctrx_quarter_normalizado := ctrx_quarter * 1.2
    ]

   if(atributos_presentes(c("foto_mes")))
    dataset[,foto_mes_formato_fecha := as.Date(paste(substr(dataset$foto_mes,1,4),substr(dataset$foto_mes,5,6),"01",sep='-'))]

  #dataset$foto_mes_formato_fecha <<- as.Date(paste(substr(dataset$foto_mes,1,4),substr(dataset$foto_mes,5,6),"01",sep='-'))

  if(atributos_presentes(c("cantidad_total_transacciones"))){
   auxiliarmenos1 <- dataset[,list(numero_de_cliente,foto_mes_formato_fecha, cantidad_total_transacciones)]
   auxiliarmenos2 <- dataset[,list(numero_de_cliente,foto_mes_formato_fecha,cantidad_total_transacciones)]
   # auxiliarmenos1$foto_mes_formato_fecha <- as.Date(auxiliarmenos1$foto_mes_formato_fecha)
   # auxiliarmenos2$foto_mes_formato_fecha <- as.Date(auxiliarmenos2$foto_mes_formato_fecha)
   auxiliarmenos1$foto_mes_formato_fecha <- auxiliarmenos1$foto_mes_formato_fecha  %m-%  months(1)
   auxiliarmenos2$foto_mes_formato_fecha <- auxiliarmenos2$foto_mes_formato_fecha %m-% months(2)
   auxiliarmenos1$codigo <- paste(auxiliarmenos1$numero_de_cliente,auxiliarmenos1$foto_mes_formato_fecha,sep='-')
   auxiliarmenos2$codigo <- paste(auxiliarmenos2$numero_de_cliente,auxiliarmenos2$foto_mes_formato_fecha,sep='-')

   dataset[, codigo := paste(numero_de_cliente, foto_mes_formato_fecha, sep='-') ]

   dataset[ auxiliarmenos1,
            on = "codigo",
            transaccionesmenos1 := i.cantidad_total_transacciones ]

   dataset[ auxiliarmenos2,
            on = "codigo",
            transaccionesmenos2 := i.cantidad_total_transacciones ]

   dataset[, cantidad_total_transacciones_quarter := rowSums(cbind(cantidad_total_transacciones +
    transaccionesmenos1 + transaccionesmenos2),na.rm=T) ]

   dataset[, codigo := NULL ]
   dataset[, transaccionesmenos1 := NULL ]
   dataset[, transaccionesmenos2 := NULL ]
   dataset[, foto_mes_formato_fecha := NULL ]
   rm(auxiliarmenos1)
   rm(auxiliarmenos2)
  }

  if( atributos_presentes( c("cantidad_total_transacciones_quarter") ))
    dataset[, cantidad_total_transacciones_quarter_normalizado := cantidad_total_transacciones_quarter]

  if( atributos_presentes( c("cantidad_total_transacciones_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 1, cantidad_total_transacciones_quarter_normalizado := cantidad_total_transacciones_quarter * 5]

  if( atributos_presentes( c("cantidad_total_transacciones_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 2, cantidad_total_transacciones_quarter_normalizado := cantidad_total_transacciones_quarter * 2]

  if( atributos_presentes( c("cantidad_total_transacciones_quarter", "cliente_antiguedad") ))
    dataset[cliente_antiguedad == 3, cantidad_total_transacciones_quarter_normalizado := cantidad_total_transacciones_quarter * 1.2]

  # variable extraida de una tesis de maestria de Irlanda
  if( atributos_presentes( c("mpayroll", "cliente_edad") ))
    dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  if( atributos_presentes( c("Master_status", "Visa_status") ))
  {
    dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
    dataset[, vm_status02 := Master_status + Visa_status]

    dataset[, vm_status03 := pmax(
      ifelse(is.na(Master_status), 10, Master_status),
      ifelse(is.na(Visa_status), 10, Visa_status)
    )]

    dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
      + ifelse(is.na(Visa_status), 10, Visa_status)]

    dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
      + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

    dataset[, vm_status06 := ifelse(is.na(Visa_status),
      ifelse(is.na(Master_status), 10, Master_status),
      Visa_status
    )]

    dataset[, mv_status07 := ifelse(is.na(Master_status),
      ifelse(is.na(Visa_status), 10, Visa_status),
      Master_status
    )]
  }


  # combino MasterCard y Visa
  if( atributos_presentes( c("Master_mfinanciacion_limite", "Visa_mfinanciacion_limite") ))
    dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  if( atributos_presentes( c("Master_Fvencimiento", "Visa_Fvencimiento") ))
    dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]

  if( atributos_presentes( c("Master_Finiciomora", "Visa_Finiciomora") ))
    dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldototal", "Visa_msaldototal") ))
    dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldopesos", "Visa_msaldopesos") ))
    dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_msaldodolares", "Visa_msaldodolares") ))
    dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumospesos", "Visa_mconsumospesos") ))
    dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumosdolares", "Visa_mconsumosdolares") ))
    dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mlimitecompra", "Visa_mlimitecompra") ))
    dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]

  if( atributos_presentes( c("Master_madelantopesos", "Visa_madelantopesos") ))
    dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_madelantodolares", "Visa_madelantodolares") ))
    dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_fultimo_cierre", "Visa_fultimo_cierre") ))
    dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagado", "Visa_mpagado") ))
    dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagospesos", "Visa_mpagospesos") ))
    dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagosdolares", "Visa_mpagosdolares") ))
    dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]

  if( atributos_presentes( c("Master_fechaalta", "Visa_fechaalta") ))
    dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]

  if( atributos_presentes( c("Master_mconsumototal", "Visa_mconsumototal") ))
    dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]

  if( atributos_presentes( c("Master_cconsumos", "Visa_cconsumos") ))
    dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]

  if( atributos_presentes( c("Master_cadelantosefectivo", "Visa_cadelantosefectivo") ))
    dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]

  if( atributos_presentes( c("Master_mpagominimo", "Visa_mpagominimo") ))
    dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  if( atributos_presentes( c("Master_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]

  if( atributos_presentes( c("Visa_mlimitecompra", "vm_mlimitecompra") ))
    dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldototal", "vm_mlimitecompra") ))
    dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldopesos", "vm_mlimitecompra") ))
    dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldopesos", "vm_msaldototal") ))
    dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]

  if( atributos_presentes( c("vm_msaldodolares", "vm_mlimitecompra") ))
    dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_msaldodolares", "vm_msaldototal") ))
    dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]

  if( atributos_presentes( c("vm_mconsumospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mconsumosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_madelantopesos", "vm_mlimitecompra") ))
    dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_madelantodolares", "vm_mlimitecompra") ))
    dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagado", "vm_mlimitecompra") ))
    dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagospesos", "vm_mlimitecompra") ))
    dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagosdolares", "vm_mlimitecompra") ))
    dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mconsumototal", "vm_mlimitecompra") ))
    dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]

  if( atributos_presentes( c("vm_mpagominimo", "vm_mlimitecompra") ))
    dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

  # Aqui debe usted agregar sus propias nuevas variables

  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  cat( "fin AgregarVariables_IntraMes()\n")
}

# --- z635 celda 44 ---
# agrego las variables intra mes

AgregarVariables_IntraMes(dataset)

    fin_etapa()

    etapa("FE historico")
# --- z635 celda 48 ---
if( !require("Rcpp")) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
require("Rcpp")

# --- z635 celda 49 ---
# se calculan para los 6 meses previos el minimo, maximo y
#  tendencia calculada con cuadrados minimos
# la formula de calculo de la tendencia puede verse en
#  https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
# para la maxíma velocidad esta funcion esta escrita en lenguaje C,
# y no en la porqueria de R o Python

cppFunction("NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde )
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) )
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      {
        xsum  += x[h] ;
        ysum  += y[h] ;
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ;
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}")

# --- z635 celda 50 ---
# calcula la tendencia de las variables cols de los ultimos 6 meses
# la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
# La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas <- function(
    dataset, cols, ventana = 6, tendencia = TRUE,
    minimo = TRUE, maximo = TRUE, promedio = TRUE,
    ratioavg = FALSE, ratiomax = FALSE) {
  gc(verbose= FALSE)
  # Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion <- ventana

  last <- nrow(dataset)

  # creo el vector_desde que indica cada ventana
  # de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids <- dataset[ , numero_de_cliente ]

  vector_desde <- seq(
    -ventana_regresion + 2,
    nrow(dataset) - ventana_regresion + 1
  )

  vector_desde[1:ventana_regresion] <- 1

  for (i in 2:last) {
    if (vector_ids[i - 1] != vector_ids[i]) {
      vector_desde[i] <- i
    }
  }
  for (i in 2:last) {
    if (vector_desde[i] < vector_desde[i - 1]) {
      vector_desde[i] <- vector_desde[i - 1]
    }
  }

  for (campo in cols) {
    nueva_col <- fhistC(dataset[, get(campo)], vector_desde)

    if (tendencia) {
      dataset[, paste0(campo, "_tend", ventana) :=
        nueva_col[(0 * last + 1):(1 * last)]]
    }

    if (minimo) {
      dataset[, paste0(campo, "_min", ventana) :=
        nueva_col[(1 * last + 1):(2 * last)]]
    }

    if (maximo) {
      dataset[, paste0(campo, "_max", ventana) :=
        nueva_col[(2 * last + 1):(3 * last)]]
    }

    if (promedio) {
      dataset[, paste0(campo, "_avg", ventana) :=
        nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratioavg) {
      dataset[, paste0(campo, "_ratioavg", ventana) :=
        get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratiomax) {
      dataset[, paste0(campo, "_ratiomax", ventana) :=
        get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
    }
  }
}

# --- z635 celda 51 ---
# Feature Engineering Historico

setorder(dataset, numero_de_cliente, foto_mes)

# todo es lagueable, menos la primary key y la clase
cols_lagueables <- copy( setdiff(
    colnames(dataset),
    c("numero_de_cliente", "foto_mes", "clase_ternaria")
) )

# https://rdrr.io/cran/data.table/man/shift.html

# lags de orden 1
dataset[,
    paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables
]

# lags de orden 2
dataset[,
    paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
    by = numero_de_cliente,
    .SDcols = cols_lagueables
]

# agrego los delta lags
for (vcol in cols_lagueables)
{
    dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
    dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
}

# --- z635 celda 52 ---
# parametros de Feature Engineering Historico de Tendencias
PARAM$FE_hist$Tendencias$run <- TRUE
PARAM$FE_hist$Tendencias$ventana <- 6
PARAM$FE_hist$Tendencias$tendencia <- TRUE
PARAM$FE_hist$Tendencias$minimo <- FALSE
PARAM$FE_hist$Tendencias$maximo <- FALSE
PARAM$FE_hist$Tendencias$promedio <- FALSE
PARAM$FE_hist$Tendencias$ratioavg <- FALSE
PARAM$FE_hist$Tendencias$ratiomax <- FALSE

# --- z635 celda 53 ---
cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
setorder(dataset, numero_de_cliente, foto_mes)

if( PARAM$FE_hist$Tendencias$run) {
    TendenciaYmuchomas(dataset,
    cols = cols_lagueables,
    ventana = PARAM$FE_hist$Tendencias$ventana, # 6 meses de historia
    tendencia = PARAM$FE_hist$Tendencias$tendencia,
    minimo = PARAM$FE_hist$Tendencias$minimo,
    maximo = PARAM$FE_hist$Tendencias$maximo,
    promedio = PARAM$FE_hist$Tendencias$promedio,
    ratioavg = PARAM$FE_hist$Tendencias$ratioavg,
    ratiomax = PARAM$FE_hist$Tendencias$ratiomax
  )
}

    fin_etapa()

    grabar_checkpoint(CK_HIST)

  } else {
    dataset <- leer_checkpoint(CK_HIST)
  }

  if (PARAM$FE_rf$run) {
    etapa("FE random forest")
# --- z635 celda 57 ---
if( !require("lightgbm")) install.packages("lightgbm")
require("lightgbm")

# --- z635 celda 58 ---
AgregaVarRandomForest <- function() {

  cat( "inicio AgregaVarRandomForest()\n")
  gc(verbose= FALSE)
  dataset[, clase01 := 0L ]
  dataset[ clase_ternaria %in% PARAM$FE_rf$train$clase01_valor1,
      clase01 := 1L ]

  campos_buenos <- setdiff(
    colnames(dataset),
    c( "clase_ternaria", "clase01")
  )

  dataset[, entrenamiento :=
    as.integer( foto_mes %in% PARAM$FE_rf$train$training )]

  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    free_raw_data = FALSE
  )

  modelo <- lgb.train(
     data = dtrain,
     param = PARAM$FE_rf$lgb_param,
     verbose = -100
  )

  cat( "Fin construccion RandomForest\n" )
  # grabo el modelo, achivo .model
  lgb.save(modelo, file="modelo.model" )

  qarbolitos <- copy(PARAM$FE_rf$lgb_param$num_iterations)

  periodos <- dataset[ , unique( foto_mes ) ]

  for( periodo in  periodos )
  {
    cat( "periodo = ", periodo, "\n" )
    datamatrix <- data.matrix(dataset[ foto_mes== periodo, campos_buenos, with = FALSE])

    cat( "Inicio prediccion\n" )
    prediccion <- predict(
        modelo,
        datamatrix,
        type = "leaf"
    )
    cat( "Fin prediccion\n" )

    for( arbolito in 1:qarbolitos )
    {
       cat( arbolito, " " )
       hojas_arbol <- unique(prediccion[ , arbolito])

       for (pos in 1:length(hojas_arbol)) {
         # el numero de nodo de la hoja, estan salteados
         nodo_id <- hojas_arbol[pos]
         dataset[ foto_mes== periodo, paste0(
            "rf_", sprintf("%03d", arbolito),
             "_", sprintf("%03d", nodo_id)
          ) :=  as.integer( nodo_id == prediccion[ , arbolito]) ]

       }

       rm( hojas_arbol )
    }
    cat( "\n" )

    rm( prediccion )
    rm( datamatrix )
    gc(verbose= FALSE)
  }

  gc(verbose= FALSE)

  # borro clase01 , no debe ensuciar el dataset
  dataset[ , clase01 := NULL ]

}

# --- z635 celda 59 ---
# Parametros de Feature Engineering  a partir de hojas de Random Forest

# Estos CUATRO parametros son los que se deben modificar
PARAM$FE_rf$arbolitos= 20
PARAM$FE_rf$hojas_por_arbol= 16
PARAM$FE_rf$datos_por_hoja= 100
PARAM$FE_rf$mtry_ratio= 0.2

# Estos son quasi fijos
PARAM$FE_rf$train$clase01_valor1 <- c( "BAJA+2", "BAJA+1")
PARAM$FE_rf$train$training <- c( 202101, 202102, 202103)

# Estos TAMBIEN son quasi fijos
PARAM$FE_rf$lgb_param <-list(
    # parametros que se pueden cambiar
    num_iterations = PARAM$FE_rf$arbolitos,
    num_leaves  = PARAM$FE_rf$hojas_por_arbol,
    min_data_in_leaf = PARAM$FE_rf$datos_por_hoja,
    feature_fraction_bynode  = PARAM$FE_rf$mtry_ratio,

    # para que LightGBM emule Random Forest
    boosting = "rf",
    bagging_fraction = ( 1.0 - 1.0/exp(1.0) ),
    bagging_freq = 1.0,
    feature_fraction = 1.0,

    # genericos de LightGBM
    max_bin = 31L,
    objective = "binary",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE,
    verbosity = -100,
    max_depth = -1L,
    min_gain_to_split = 0.0,
    min_sum_hessian_in_leaf = 0.001,
    lambda_l1 = 0.0,
    lambda_l2 = 0.0,

    pos_bagging_fraction = 1.0,
    neg_bagging_fraction = 1.0,
    is_unbalance = FALSE,
    scale_pos_weight = 1.0,

    drop_rate = 0.1,
    max_drop = 50,
    skip_drop = 0.5,

    extra_trees = FALSE
  )

# --- z635 celda 60 ---
# Feature Engineering agregando variables de Random Forest
AgregaVarRandomForest()

    fin_etapa()
  } else {
    loguear("FE random forest apagado (PARAM$FE_rf$run = FALSE)")
  }

  # v3: AgregaVarRandomForest deja la marca de sus meses de entrenamiento como
  # columna; sin canaritos llegaria al modelo como una variable mas
  if ("entrenamiento" %in% names(dataset)) dataset[, entrenamiento := NULL]

# --- z635 celda 65 ---
VPOS_CORTE <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")

  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1, PARAM$CN$train$gan1, PARAM$CN$train$gan0)
  ))

  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  setorder(tbl, -gan_acum) # voy por la meseta

  gan <- mean(tbl[1:500, gan_acum]) # meseta de tamaño 500

  pos_meseta <- tbl[1:500, median(posicion)]
  VPOS_CORTE <<- c(VPOS_CORTE, pos_meseta)

  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}

# --- z635 celda 66 ---
# Elimina del dataset las variables que estan por debajo
#  de la capa geologica de canaritos
# se llama varias veces, luego de agregar muchas variables nuevas,
#  para ir reduciendo la cantidad de variables
# y así hacer lugar a nuevas variables importantes

GVEZ <- 1

campitos <- c( "numero_de_cliente", "foto_mes", "clase_ternaria" )

CanaritosAsesinos <- function(
  canaritos_ratio,
  canaritos_desvios,
  canaritos_semilla) {

  cat( "inicio CanaritosAsesinos()\n")
  gc(verbose= FALSE)
  dataset[, clase01 := 0L ]
  dataset[ clase_ternaria %in% PARAM$CN$train$clase01_valor1,
      clase01 := 1L ]

  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  for (i in 1:(ncol(dataset) * canaritos_ratio)) {
    dataset[, paste0("canarito", i) := runif(nrow(dataset))]
  }

  campos_buenos <- setdiff(
    colnames(dataset),
    c( campitos, "clase01")
  )

  azar <- runif(nrow(dataset))

  dataset[, entrenamiento :=
    as.integer( foto_mes %in% PARAM$CN$train$training &
      (clase01 == 1 | azar < PARAM$CN$train$undersampling))]

  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE]),
    label = dataset[entrenamiento == TRUE, clase01],
    weight = dataset[
      entrenamiento == TRUE,
      ifelse(clase_ternaria %in% PARAM$CN$train$positivos, 1.0000001, 1.0)
    ],
    free_raw_data = FALSE
  )

  dvalid <- lgb.Dataset(
    data = data.matrix(dataset[foto_mes %in% PARAM$CN$train$validation, campos_buenos, with = FALSE]),
    label = dataset[foto_mes %in% PARAM$CN$train$validation, clase01],
    weight = dataset[
      foto_mes %in% PARAM$CN$train$validation,
      ifelse( clase_ternaria %in% PARAM$CN$train$positivos, 1.0000001, 1.0)
    ],
    free_raw_data = FALSE
  )


  param <- list(
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    verbosity = -100,
    seed = canaritos_semilla,
    max_depth = -1, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # por ahora, lo dejo fijo
    lambda_l1 = 0.0, # por ahora, lo dejo fijo
    lambda_l2 = 0.0, # por ahora, lo dejo fijo
    max_bin = 31, # por ahora, lo dejo fijo
    num_iterations = 9999, # un numero grande, lo limita early_stopping_rounds
    force_row_wise = TRUE, # para que los alumnos no se atemoricen con  warning
    learning_rate = 0.065,
    feature_fraction = 1.0, # lo seteo en 1
    min_data_in_leaf = 260,
    num_leaves = 60,
    early_stopping_rounds = 200,
    num_threads = 1
  )

  set.seed(canaritos_semilla, kind = "L'Ecuyer-CMRG")
  modelo <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalid),
    eval = fganancia_lgbm_meseta,
    param = param,
    verbose = -100
  )

  tb_importancia <- lgb.importance(model = modelo)
  tb_importancia[, pos := .I]

  fwrite(tb_importancia,
    file = paste0("impo_", GVEZ, ".txt"),
    sep = "\t"
  )

  GVEZ <<- GVEZ + 1

  umbral <- tb_importancia[
    Feature %like% "canarito",
    median(pos) + canaritos_desvios * sd(pos)
  ] # Atencion corto en la mediana mas desvios!!

  col_utiles <- tb_importancia[
    pos < umbral & !(Feature %like% "canarito"),
    Feature
  ]

  col_utiles <- unique(c(
    col_utiles,
    c(campitos, "mes")
  ))

  col_inutiles <- setdiff(colnames(dataset), col_utiles)

  dataset[, (col_inutiles) := NULL]

  cat( "fin CanaritosAsesinos()\n")

  return( tb_importancia )
}

# --- z635 celda 67 ---
# Estos DOS parametros son los que se deben modificar
PARAM$CN$ratio <- 0.2
PARAM$CN$desvios <- 2


# Parametros quasi fijos
# Parametros de un LightGBM que se genera para estimar la column importance
PARAM$CN$train$clase01_valor1 <- c( "BAJA+2", "BAJA+1")
PARAM$CN$train$positivos <- c( "BAJA+2")
PARAM$CN$train$training <- c( 202101, 202102, 202103)
PARAM$CN$train$validation <- c( 202105 )
PARAM$CN$train$undersampling <- 0.1
PARAM$CN$train$gan1 <- 0.975
PARAM$CN$train$gan0 <- -0.025

  # v3: el FE evolutivo trabaja con todo lo que hay (intra-mes, historico y
  # random forest): esa es la poblacion inicial, y su tamano el tope.
  # v4: va despues de las celdas 65 a 67 porque su importancia sale del
  # LightGBM de canaritos (fganancia_lgbm_meseta y PARAM$CN$train), y antes
  # de la llamada de la celda 68
  if (PARAM$GA$modo == "evolutivo") {
    if (nzchar(PARAM$EV$poblacion_desde)) {
      etapa("FE evolutivo (poblacion de una busqueda)")
      resumen_ev <- ev_desde(PARAM$EV$poblacion_desde)
      fin_etapa(resumen_ev)
    } else {
      etapa("FE evolutivo")
      resumen_ev <- correr_evolutivo(materializar= !PARAM$EV$solo)
      fin_etapa(resumen_ev)
      if (PARAM$EV$solo) terminar_busqueda()
    }
  }

  etapa("canaritos")
  # v3: CN_RUN=0 (por defecto con el evolutivo) no corta: el dataset queda
  # del tamano que tiene
  if (PARAM$CN$run) {
# --- z635 celda 68 ---
# la llamada a Canaritos Asesinos
tb_importancia <- CanaritosAsesinos(
  canaritos_ratio = PARAM$CN$ratio,
  canaritos_desvios = PARAM$CN$desvios,
  canaritos_semilla = PARAM$semilla_primigenia
)

# --- z635 celda 69 ---
# grabo la importancia, ver el archivo directamente en la carpeta

fwrite( tb_importancia,
  file="canaritos.txt",
  sep="\t"
)
  } else {
    loguear("canaritos apagado (CN_RUN=0): el dataset queda con", ncol(dataset), "columnas")
  }

  grabar_checkpoint(CK_CANARITOS)
  fin_etapa("Checkpoint grabado: si se corta, retoma desde aca.")

} else {
  dataset <- leer_checkpoint(CK_CANARITOS)
}

# ===========================================================================
# 9.7.2 MODELADO
# ===========================================================================

etapa("training strategy")
# Reemplaza la celda 74 del z635: los meses dependen del grupo
meses <- function(desde, hasta) {
  todos <- c(201901:201912, 202001:202012, 202101:202112)
  todos[todos >= desde & todos <= hasta]
}

if (GRUPO == "A") {
  PARAM$trainingstrategy$validate    <- c(202107)
  PARAM$trainingstrategy$training    <- meses(201901, 202105)
  PARAM$trainingstrategy$final_train <- meses(201901, 202107)
  PARAM$trainingstrategy$future      <- c(202109)
} else {
  # se deja 202104 como gap, igual que el A deja 202106
  PARAM$trainingstrategy$validate    <- c(202105)
  PARAM$trainingstrategy$training    <- meses(201901, 202103)
  PARAM$trainingstrategy$final_train <- meses(201901, 202105)
  PARAM$trainingstrategy$future      <- c(202107)
}
PARAM$trainingstrategy$training_pct <- 0.2
PARAM$trainingstrategy$positivos <- c( "BAJA+1", "BAJA+2")

# --- z635 celda 75 ---
# seteo la clase01   1={BAJA+1, BAJA+2}   0={CONTINUA}
dataset[, clase01 := ifelse( clase_ternaria %in% PARAM$trainingstrategy$positivos, 1, 0 )]

# --- z635 celda 76 ---
# los campos en los que se entrena
campos_buenos <- copy( setdiff(
    colnames(dataset), c("clase_ternaria","clase01","azar"))
)

# --- z635 celda 77 ---
# preparo para que se puede hacer undersampling de los CONTINUA
#  solamente por un tema de VELOCIDAD
set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
dataset[, azar:=runif(nrow(dataset))]

# undersampling de los CONTINUA
dataset[, fold_train :=  foto_mes %in%  PARAM$trainingstrategy$training &
    (clase_ternaria %in% c("BAJA+1", "BAJA+2") |
     azar < PARAM$trainingstrategy$training_pct ) ]


if( !require("lightgbm")) install.packages("lightgbm")
require("lightgbm")

dtrain <- lgb.Dataset(
  data= data.matrix(dataset[fold_train == TRUE, campos_buenos, with = FALSE]),
  label= dataset[fold_train == TRUE, clase01],
  free_raw_data= TRUE
)

# --- z635 celda 78 ---
# datos de validation
dvalidate <- lgb.Dataset(
  data= data.matrix(dataset[foto_mes %in% PARAM$trainingstrategy$validate, campos_buenos, with = FALSE]),
  label= dataset[foto_mes %in% PARAM$trainingstrategy$validate, clase01],
  free_raw_data= TRUE
)

nrow(dvalidate)

fin_etapa(paste0("validate ", paste(PARAM$trainingstrategy$validate, collapse= " "),
  ", future ", paste(PARAM$trainingstrategy$future, collapse= " ")))

# ---------------------------------------------------------------------------
# 9.7.2.2 HYPERPARAMETER TUNING
# ---------------------------------------------------------------------------
etapa("optimizacion bayesiana")

# cada evaluacion queda en BO_iter.txt apenas termina
BO_EVALS <- if (file.exists("BO_iter.txt")) nrow(fread("BO_iter.txt")) else 0L

registrar_eval <- function(x, AUC) {
  BO_EVALS <<- BO_EVALS + 1L
  fila <- data.table(eval= BO_EVALS, fecha= format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    AUC= AUC, as.data.table(lapply(x, as.numeric)))
  fwrite(fila, "BO_iter.txt", sep= "\t", append= file.exists("BO_iter.txt"))

  if (PARAM$mail$cada_iter > 0 && BO_EVALS %% PARAM$mail$cada_iter == 0) {
    tb <- fread("BO_iter.txt")
    mejor <- tb[which.max(AUC)]
    mandar_mail(sprintf("BO: %d evaluaciones", BO_EVALS), hilo= "iteraciones",
      paste0("Mejor AUC hasta ahora: ", round(mejor$AUC, 6), " (evaluacion ", mejor$eval, ")\n",
             "Ultima: ", round(AUC, 6), "\n\n",
             paste(capture.output(print(tail(tb, PARAM$mail$cada_iter))), collapse= "\n")))
  }
}

# --- z635 celda 83 ---
# paquetes necesarios para la Bayesian Optimization
if(!require("DiceKriging")) install.packages("DiceKriging")
require("DiceKriging")

if(!require("mlrMBO")) install.packages("mlrMBO")
require("mlrMBO")

# --- z635 celda 85 ---
# 40 es el minimo razonable,  un buen analista Sr aumentara este valor
PARAM$hipeparametertuning$num_interations <- 30

# parametros fijos del LightGBM
PARAM$lgbm$param_fijos <- list(
  objective= "binary",
  metric= "auc",
  first_metric_only= TRUE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  verbosity= -100,
  force_row_wise= TRUE, # para evitar warning
  seed= PARAM$semilla_primigenia,
  extra_trees= FALSE,

  num_leaves= 128,
  max_depth= -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split= 0.0, # min_gain_to_split >= 0.0
  min_data_in_leaf= 0,
  min_sum_hessian_in_leaf= 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1= 0.0, # lambda_l1 >= 0.0
  lambda_l2= 0.0, # lambda_l2 >= 0.0

  bagging_fraction= 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction= 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction= 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance= FALSE, #
  scale_pos_weight= 1.0, # scale_pos_weight > 0.0

  drop_rate= 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop= 50, # <=0 means no limit
  skip_drop= 0.5, # 0.0 <= skip_drop <= 1.0

  max_bin= 31,
  early_stopping_rounds= 0
)

# --- z635 celda 86 ---

PARAM$hipeparametertuning$hs <- makeParamSet(
  makeNumericParam("num_iterations", lower= 2.0, upper= 10.0, trafo= function(x) round(2^x) ),
  makeNumericParam("num_leaves", lower= 2.0, upper= 11.0, trafo= function(x) round(2^x) ),
  makeNumericParam("learning_rate", lower= -7.0, upper= -0.1, trafo= function(x) 2^x ),
  makeNumericParam("feature_fraction", lower= 0.05, upper= 0.90),
  makeNumericParam("min_sum_hessian_in_leaf", lower= -10.0, upper= 10.0, trafo= function(x) 2^x)
)

# --- rango de la BO (despues de la celda 86) ---------------------------------
# Techo de num_iterations y num_leaves configurable desde el .env, en log2.
# Default = el rango del z635: 2^10 = 1024 arboles y 2^11 = 2048 hojas.
# En WF9501 las 5 mejores evaluaciones quedaron pegadas a esos dos techos.
PARAM$BO$techo_log2_num_iterations <- as.numeric(env_o("BO_TECHO_LOG2_ITERACIONES", "10"))
PARAM$BO$techo_log2_num_leaves     <- as.numeric(env_o("BO_TECHO_LOG2_HOJAS", "11"))
PARAM$hipeparametertuning$hs$pars$num_iterations$upper <- PARAM$BO$techo_log2_num_iterations
PARAM$hipeparametertuning$hs$pars$num_leaves$upper     <- PARAM$BO$techo_log2_num_leaves
loguear("BO: rango num_iterations 2^", PARAM$hipeparametertuning$hs$pars$num_iterations$lower,
        "a 2^", PARAM$hipeparametertuning$hs$pars$num_iterations$upper,
        ", num_leaves 2^", PARAM$hipeparametertuning$hs$pars$num_leaves$lower,
        "a 2^", PARAM$hipeparametertuning$hs$pars$num_leaves$upper)

# --- z635 celda 88 (+ registrar_eval) ---
# En  x llegan los parmaetros de la bayesiana
#  devuelve la AUC en validate del modelo entrenado
#  en el parametro x llegan los hiperparámetros que se estan optimizando

EstimarGanancia_AUC_lightgbm <- function(x) {

  # x pisa (o agrega) a param_fijos
  param_completo <- modifyList(PARAM$lgbm$param_fijos, x)

  # entreno LightGBM
  modelo_train <- lgb.train(
    data= dtrain,
    valids= list(valid = dvalidate),
    eval= "auc",
    param= param_completo,
    verbose= -100
  )

  # recupero la AUC en validation
  AUC <- modelo_train$record_evals$valid$auc$eval[[x$num_iterations]]


  message(format(Sys.time(), "%a %b %d %X %Y  "),
    toString(x),
    " AUC ", AUC
  )

  # hago espacio en la memoria
  rm(modelo_train)
  gc(full= TRUE, verbose= FALSE)

  registrar_eval(x, AUC)
  return(AUC)
}

# --- z635 celda 90 (graba en cada iteracion) ---
configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
    fn= EstimarGanancia_AUC_lightgbm, # la funcion que voy a maximizar
    minimize= FALSE, # estoy Maximizando AUC
    noisy= FALSE,
    par.set= PARAM$hipeparametertuning$hs,
    has.simple.signature= FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
    save.on.disk.at= 0:(PARAM$hipeparametertuning$num_interations + 1), # graba en CADA iteracion
    save.file.path= "HT.RDATA"
)

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
    ctrl,
    iters= PARAM$hipeparametertuning$num_interations  # cantidad de iteraciones inteligentes
)

# defino el método estandar para la creacion de los puntos iniciales
#   los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# mas configuraciones
surr.km <- makeLearner(
    "regr.km",
    predict.type= "se",
    covtype= "matern3_2",
    control= list(trace = TRUE)
)


# Reemplaza la celda 92 del z635. mlrMBO no graba nada hasta terminar el
# diseno inicial (20 entrenamientos), asi que el diseno inicial se evalua
# aca, una fila por vez, grabando cada resultado en BO_init.txt. Con el
# diseno completo, mbo() sigue con las iteraciones inteligentes y graba
# HT.RDATA en cada una.
correr_bayesiana <- function() {
  if (file.exists("HT.RDATA")) {
    loguear("retomo la BO desde HT.RDATA")
    return(mboContinue("HT.RDATA"))
  }
  if (!require("lhs")) install.packages("lhs")
  hs <- PARAM$hipeparametertuning$hs
  set.seed(PARAM$semilla_primigenia, kind = "L'Ecuyer-CMRG")
  diseno <- ParamHelpers::generateDesign(n= 4L * ParamHelpers::getParamNr(hs, devectorize= TRUE), par.set= hs, fun= lhs::maximinLHS)
  diseno$y <- NA_real_
  if (file.exists("BO_init.txt")) {
    hechos <- fread("BO_init.txt")
    diseno$y[hechos$i] <- hechos$y
    loguear("diseno inicial: ya hay", nrow(hechos), "de", nrow(diseno), "puntos evaluados")
  }
  for (i in which(is.na(diseno$y))) {
    x <- ParamHelpers::trafoValue(hs, as.list(diseno[i, ParamHelpers::getParamIds(hs), drop= FALSE]))
    diseno$y[i] <- EstimarGanancia_AUC_lightgbm(x)
    fwrite(data.table(i= i, y= diseno$y[i]), "BO_init.txt", sep= "\t", append= file.exists("BO_init.txt"))
  }
  mbo(obj.fun, design= diseno, learner= surr.km, control= ctrl)
}

# v2: con HT_FIJO no se corre la BO; se usa la mejor fila de ese BO_log.txt
if (nzchar(PARAM$BO$ht_fijo) && !file.exists("BO_log.txt")) {
  loguear("HT fijo: copio", PARAM$BO$ht_fijo, "y no corro la BO")
  file.copy(PARAM$BO$ht_fijo, "BO_log.txt")
}

if (!file.exists("BO_log.txt")) {
  bayesiana_salida <- correr_bayesiana()
# --- z635 celda 94 ---
# almaceno los resultados de la Bayesian Optimization
# y capturo los mejores hiperparametros encontrados

tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)

# ordeno en forma descendente por AUC = y
setorder(tb_bayesiana, -y, -num_iterations)

# grabo para eventualmente poder utilizarlos en OTRA corrida
fwrite( tb_bayesiana,
  file="BO_log.txt",
  sep="\t"
)

# los mejores hiperparámetros son los que quedaron en el registro 1 de la tabla
PARAM$out$lgbm$mejores_hiperparametros <- tb_bayesiana[
  1, # el primero es el de mejor AUC
  setdiff(colnames(tb_bayesiana),
    c("y","dob","eol","error.message","exec.time","ei","error.model",
      "train.time","prop.type","propose.time","se","mean","iter")),
  with= FALSE
]

print(PARAM$out$lgbm$mejores_hiperparametros)

} else {
  loguear("la BO ya habia terminado: leo BO_log.txt")
  tb_bayesiana <- fread("BO_log.txt")
  PARAM$out$lgbm$mejores_hiperparametros <- tb_bayesiana[
    1,
    setdiff(colnames(tb_bayesiana),
      c("y","dob","eol","error.message","exec.time","ei","error.model",
        "train.time","prop.type","propose.time","se","mean","iter")),
    with= FALSE
  ]
}
fin_etapa(paste0("Mejor AUC: ", round(tb_bayesiana[1, y], 6), "\n",
  paste(capture.output(print(PARAM$out$lgbm$mejores_hiperparametros)), collapse= "\n")))

# ===========================================================================
# 9.7.3 PRODUCCION
# ===========================================================================

# RAM: los datasets de la BO ya no se usan (con HT_FIJO ni se construyen y
# free_raw_data no libera la matriz): con miles de columnas son decenas de GB
if (exists("dtrain")) rm(dtrain)
if (exists("dvalidate")) rm(dvalidate)
gc(verbose= FALSE)
etapa("final training")
# --- z635 celda 100 ---
# uno los parametros fijos y los mejores encontrados de los variables
fijos <- copy(PARAM$lgbm$param_fijos)

# quito lo que optimice en la Bayesian Optimization
fijos$num_iterations <- NULL
fijos$early_stopping_rounds <- NULL

# agrego a los hiperparametros fijos los que encontre con la Bayesian Optimization
param_final <- copy(fijos)

x <- PARAM$out$lgbm$mejores_hiperparametros
param_final$num_iterations <- round( 2^x$num_iterations )
param_final$num_leaves <- round( 2^x$num_leaves )
param_final$learning_rate <- 2^x$learning_rate
param_final$feature_fraction <- x$feature_fraction
param_final$min_sum_hessian_in_leaf <- 2^x$min_sum_hessian_in_leaf


# --- z635 celda 106 ---
primero <- TRUE

crear_modelo_final <- function( sem ) {

  nombre_arch <- paste0( "./modelos/modelo_", sem, ".txt")
  if( !file.exists(nombre_arch) )
  {
    param_final$seed <- sem

    set.seed(sem, kind = "L'Ecuyer-CMRG")
    final_model <- lgb.train(
      data= dfinal_train,
      param= param_final,
      verbose= -100
    )

    lgb.save(final_model, nombre_arch) # grabo el modelo"

    # grabo la primer importancia de variables
    #  Natalia : da lo mismo cual se guarda
    if( primero)
    {
      primero <<- FALSE
      # lgb.importance falla si el modelo no usa ninguna variable: no corta la corrida
      tb_importancia <- tryCatch(as.data.table(lgb.importance(final_model)),
        error= function(e) { loguear("impo.txt: lgb.importance fallo:", conditionMessage(e)); NULL })
      archivo_importancia <- "impo.txt"

      if (!is.null(tb_importancia)) fwrite( tb_importancia,
        file= archivo_importancia,
        sep= "\t"
      )
    }

    rm(final_model)
    gc(full = TRUE, verbose=FALSE)
  }
}


dir.create("modelos", showWarnings =FALSE)
faltan_modelos <- PARAM$FT$semillas[!file.exists(paste0("./modelos/modelo_", PARAM$FT$semillas, ".txt"))]
loguear("modelos finales: faltan", length(faltan_modelos), "de", length(PARAM$FT$semillas))

if (length(faltan_modelos)) {
  # reemplaza la celda 98: los meses de final_train ya los fijo el grupo
  # v2: FT_UNDERSAMPLING < 1 deja todas las BAJA y esa fraccion de los CONTINUA
  dataset[, fold_final_train := foto_mes %in% PARAM$trainingstrategy$final_train &
    (clase_ternaria %in% PARAM$trainingstrategy$positivos | azar < PARAM$FT$undersampling) ]
  loguear("final train:", dataset[fold_final_train == TRUE, .N], "filas (undersampling",
          PARAM$FT$undersampling, "de los CONTINUA)")

  # pesos temporales alfa^k, k = meses antes del ultimo mes de final train
  # (FT_ALFA=1: todos pesan 1, igual que antes)
  meses_ft <- sort(unique(dataset[fold_final_train == TRUE, foto_mes]))
  idx_mes <- function(m) (m %/% 100) * 12 + m %% 100
  pesos_ft <- PARAM$FT$alfa ^ (idx_mes(max(meses_ft)) - idx_mes(dataset[fold_final_train == TRUE, foto_mes]))
  if (PARAM$FT$alfa != 1) loguear("final train: pesos alfa^k con alfa", PARAM$FT$alfa,
                                  "(peso minimo", round(min(pesos_ft), 3), ")")

  dfinal_train <- lgb.Dataset(
    data= data.matrix(dataset[fold_final_train == TRUE, campos_buenos, with= FALSE]),
    label= dataset[fold_final_train == TRUE, clase01],
    weight= pesos_ft,
    free_raw_data= TRUE
  )

  gc(full = TRUE, verbose=FALSE)
  primero <- !file.exists("impo.txt")
  for (sem in faltan_modelos) {
    crear_modelo_final(sem)                # graba ./modelos/modelo_<sem>.txt
    loguear("modelo final listo, semilla", sem)
  }
  rm(dfinal_train)
  gc(full = TRUE, verbose=FALSE)
}
fin_etapa(paste0("Semillas: ", paste(PARAM$FT$semillas, collapse= ", ")))

etapa("scoring")
dfuture <- dataset[ foto_mes %in% PARAM$trainingstrategy$future ]
# --- z635 celda 111 ---
# aplico final_model   a dfuture

tb_prediccion <- dfuture[, list(numero_de_cliente)]
tb_prediccion[, prob := 0]

datos_matrix <- data.matrix(dfuture[, campos_buenos, with= FALSE])

# --- z635 celda 112 ---
agregar_a_ensemble <- function( sem ) {

  nombre_arch <- paste0( "./modelos/modelo_", sem, ".txt")
  final_model <- lgb.load(nombre_arch)

  prediccion <- predict(
    final_model,
    datos_matrix
  )

  tb_prediccion[, paste0("prob_", isem) := prediccion]
  tb_prediccion[, prob := prob + prediccion]

  rm(final_model)
  rm(prediccion)
  gc(full = TRUE, verbose=FALSE)
}

# --- z635 celda 113 ---
for( isem in seq(length(PARAM$FT$semillas)) )
{
  sem <- PARAM$FT$semillas[ isem ]
  agregar_a_ensemble( sem )
  gc(full = TRUE, verbose=FALSE)
}

rm( datos_matrix)
gc(full = TRUE, verbose=FALSE)

tb_prediccion[, prob := prob /length(PARAM$FT$semillas) ]

# --- z635 celda 115 ---
# grabo las probabilidad del modelo
#  me va a ser util para hacer Ensembles de modelos
fwrite(tb_prediccion,
  file= "prediccion.txt",
  sep= "\t"
)

# la clase real queda al lado de la probabilidad (el grupo B la usa)
tb_prediccion[, clase_ternaria := dfuture$clase_ternaria]
fin_etapa(paste0("Clientes en ", paste(PARAM$trainingstrategy$future, collapse= " "), ": ", nrow(tb_prediccion)))

# ---------------------------------------------------------------------------
# GRUPO A: Kaggle.  GRUPO B: ganancia contra la clase real de 202107
# ---------------------------------------------------------------------------
resumen <- ""

if (GRUPO == "A") {
  etapa("kaggle")
  # kaggle.json y el CLI se prepararon al arrancar; si no andaban, se reintenta
  # (el kaggle.json pudo haberse subido al bucket durante la corrida)
  if (!KAGGLE_OK && KAGGLE_SUBIR) preparar_kaggle(avisar= FALSE)
  PARAM$kaggle$competencia <- Sys.getenv("KAGGLE_COMPETENCIA")

  setorder(tb_prediccion, -prob)
  dir.create("kaggle", showWarnings= FALSE)
  subidos <- c()

  for (envios in PARAM$kaggle$cortes) {
    tb_prediccion[, Predicted := 0L]
    tb_prediccion[1:envios, Predicted := 1L]

    archivo_kaggle <- paste0("./kaggle/KA", sub("^WF", "", NOMBRE_CORRIDA), "_",
      "s", length(PARAM$FT$semillas), "_", envios, ".csv")
    marca <- paste0(archivo_kaggle, ".subido")
    if (file.exists(marca)) { loguear("ya subido:", archivo_kaggle); next }

    fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)], file= archivo_kaggle, sep= ",")
    if (!KAGGLE_OK) { loguear("sin Kaggle: queda", archivo_kaggle, "para subir a mano"); next }

    linea <- paste(kaggle_bin, "competitions submit",
      "-c", PARAM$kaggle$competencia,
      "-f", archivo_kaggle,
      paste0("-m 'envios=", envios, "  semilla=", PARAM$semilla_primigenia, "'"))
    Sys.sleep(30)
    salida <- suppressWarnings(system(linea, intern= TRUE))
    cat(salida, "\n")
    if (is.null(attr(salida, "status"))) {
      writeLines(c(format(Sys.time()), salida), marca)
      subidos <- c(subidos, envios)
    } else {
      loguear("FALLO la subida de", archivo_kaggle, ":", paste(salida, collapse= " "))
    }
  }
  resumen <- paste0("Subidos a Kaggle en esta corrida: ",
    if (length(subidos)) paste(subidos, collapse= ", ") else "ninguno nuevo")

  # puntaje publico de lo subido: Kaggle tarda un poco en calcularlo
  if (KAGGLE_OK) {
    nuestros <- basename(list.files("kaggle", pattern= "[.]csv$"))
    for (intento in 1:6) {
      Sys.sleep(60)
      tb_sub <- tryCatch(fread(text= paste(suppressWarnings(system2(kaggle_bin,
          c("competitions", "submissions", "-c", PARAM$kaggle$competencia, "--csv"),
          stdout= TRUE, stderr= FALSE)), collapse= "\n")), error= function(e) NULL)
      if (is.null(tb_sub) || !("fileName" %in% names(tb_sub))) next
      tb_sub <- tb_sub[fileName %in% nuestros]
      if (nrow(tb_sub) && all(nchar(as.character(tb_sub$publicScore)) > 0)) break
    }
    if (!is.null(tb_sub) && "fileName" %in% names(tb_sub) && nrow(tb_sub)) {
      fwrite(tb_sub, "kaggle_puntajes.txt", sep= "\t")
      resumen <- paste0(resumen, "\n\nPuntaje publico en Kaggle:\n",
        paste(sprintf("  %-28s %s", tb_sub$fileName, tb_sub$publicScore), collapse= "\n"))
    } else {
      resumen <- paste0(resumen, "\n\n(no se pudo leer el puntaje de Kaggle; ver la pagina de submissions)")
    }
  }
  fin_etapa(resumen)

} else {
  etapa("ganancia en 202107")
  setorder(tb_prediccion, -prob)
  tb_prediccion[, ganancia := ifelse(clase_ternaria == "BAJA+2",
    PARAM$ganancia$acierto, PARAM$ganancia$error)]
  tb_prediccion[, ganancia_acum := cumsum(ganancia)]

  tb_ganancia <- data.table(envios= PARAM$kaggle$cortes,
    ganancia= tb_prediccion$ganancia_acum[PARAM$kaggle$cortes])
  envios_max <- which.max(tb_prediccion$ganancia_acum)
  tb_ganancia <- rbind(tb_ganancia,
    data.table(envios= envios_max, ganancia= tb_prediccion$ganancia_acum[envios_max]))
  fwrite(tb_ganancia, "ganancia_202107.txt", sep= "\t", scipen= 50)
  # v3: la metrica de la clase para comparar brazos es el MAXIMO de la curva
  # por semilla (Wilcoxon apareado); la meseta (promedio de los cortes fijos)
  # queda como metrica secundaria
  meseta <- mean(tb_ganancia$ganancia[seq_along(PARAM$kaggle$cortes)])
  writeLines(format(round(meseta), scientific= FALSE), "ganancia_meseta.txt")
  writeLines(format(round(tb_prediccion$ganancia_acum[envios_max]), scientific= FALSE), "ganancia_maximo.txt")

  resumen <- paste0("Ganancia en 202107 por cantidad de envios:\n",
    paste(sprintf("%6d  %s", tb_ganancia$envios,
      formatC(tb_ganancia$ganancia, format= "d", big.mark= ".")), collapse= "\n"),
    "\n(la ultima fila es el maximo de la curva, que no se conoce de antemano)\n",
    "Promedio de los ", length(PARAM$kaggle$cortes), " cortes (meseta, para comparar corridas): ",
    formatC(meseta, format= "d", big.mark= "."))
  fin_etapa(resumen)
}

etapa("fin")
# --- z635 celda 119 ---
# grabo los parametros
if( !require("yaml")) install.packages("yaml")
require("yaml")

write_yaml( PARAM, file="PARAM.yml")


# REGISTRO de la corrida: lo mismo va al mail final y a registro.txt
# del evolutivo: la historia por generacion y cuantas generadas (ev*) usa el modelo final
texto_registro_evolutivo <- function() {
  t <- texto_evolutivo()
  if (PARAM$GA$modo == "nada" || !file.exists("impo.txt")) return(t)
  impo <- fread("impo.txt")
  es_ev <- startsWith(impo$Feature, "ev")
  paste0(t, "\nEn el modelo final: ", sum(es_ev), " generadas de ", nrow(impo),
    " variables usadas; en el top 50: ", sum(head(es_ev, 50)), ", en el top 100: ", sum(head(es_ev, 100)),
    "; gain de las generadas: ", round(100 * sum(impo$Gain[es_ev]) / sum(impo$Gain), 1), "%",
    if (any(es_ev)) paste0("\nLas 10 generadas mas importantes:\n",
      { top <- head(impo$Feature[es_ev], 10)
        pob <- if (file.exists(ARCH_EV_POB)) fread(ARCH_EV_POB, sep= "\t") else data.table(nombre= character(), formula= character())
        paste0("  ", which(es_ev)[seq_along(top)], ". ", top, "  ", pob$formula[match(top, pob$nombre)], collapse= "\n") }) else "")
}

horas <- as.numeric(difftime(Sys.time(), T_INICIO, units= "hours"))
texto <- function(x) paste(capture.output(print(x)), collapse= "\n")
seccion <- function(titulo, cuerpo) paste0("\n=== ", titulo, " ===\n", cuerpo, "\n")

registro <- paste0(
  "Corrida ", NOMBRE_CORRIDA, ", grupo ", GRUPO, ", semilla del pipeline ", PARAM$semilla_primigenia, "\n",
  "Version del script: ", WF_VERSION, "\n",
  "Carpeta: ", carpeta_exp, "\n",
  "Terminado: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  sprintf(" (esta sesion: %.1f h)", horas), "\n",
  seccion("Resultado", resumen),
  seccion("Linea de tiempo", paste0(
    "training ", min(PARAM$trainingstrategy$training), "-", max(PARAM$trainingstrategy$training),
    ", validate ", paste(PARAM$trainingstrategy$validate, collapse= " "),
    ", final_train ", min(PARAM$trainingstrategy$final_train), "-", max(PARAM$trainingstrategy$final_train),
    ", future ", paste(PARAM$trainingstrategy$future, collapse= " "))),
  seccion("Semillas", paste0("primigenia ", PARAM$semilla_primigenia,
    ", semillerio ", paste(PARAM$FT$semillas, collapse= ", "))),
  seccion("FE evolutivo", texto_registro_evolutivo()),
  seccion("Dataset final", paste0(nrow(dataset), " filas x ", ncol(dataset),
    " columnas; entrena con ", length(campos_buenos), " campos")),
  seccion("Optimizacion bayesiana", paste0("mejor AUC ", round(tb_bayesiana[1, y], 6),
    " de ", nrow(tb_bayesiana), " evaluaciones\n",
    "hiperparametros del modelo final:\n", texto(param_final))),
  seccion("Duracion de cada etapa (esta sesion)", texto(TIEMPOS)),
  seccion("PARAM completo (PARAM.yml)",
    if (file.exists("PARAM.yml")) paste(readLines("PARAM.yml"), collapse= "\n") else "no se grabo PARAM.yml"),
  seccion("Archivos en la carpeta", paste(list.files(".", recursive= TRUE), collapse= "\n"))
)
writeLines(registro, "registro.txt")

writeLines(format(Sys.time()), "TERMINADO")
mandar_mail("TERMINO", registro)
format(Sys.time(), "%a %b %d %X %Y")
