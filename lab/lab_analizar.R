# Analiza las salidas de lab_ft.R: Rscript lab_analizar.R <carpeta con un subdirectorio por host>
# Por host, configuracion e iteracion: meseta de cada modelo y de semillerios de k
# modelos (promedio de probabilidades de k modelos sorteados, 200 sorteos), como
# en 0899_ComoElegirMejorModelo. Al final, mezclas entre configuraciones.
suppressPackageStartupMessages({ library(data.table); library(fst) })
RAIZ <- commandArgs(trailingOnly= TRUE)[1]
CORTES <- seq(8000, 12000, 500)
set.seed(17)

curva <- function(prob, clase) cumsum(ifelse(clase[order(-prob)] == "BAJA+2", 975000, -25000))
meseta <- function(prob, clase) mean(curva(prob, clase)[CORTES]) / 1e6
maximo <- function(prob, clase) { g <- curva(prob, clase); c(max(g) / 1e6, which.max(g)) }

res <- list(); preds <- list()
for (host in list.dirs(RAIZ, recursive= FALSE, full.names= FALSE)) {
  dh <- file.path(RAIZ, host)
  if (!file.exists(file.path(dh, "clientes.fst"))) next
  clase <- read_fst(file.path(dh, "clientes.fst"))$clase_ternaria
  for (cfg in list.dirs(dh, recursive= FALSE, full.names= FALSE)) {
    archs <- list.files(file.path(dh, cfg), pattern= "[.]fst$", full.names= TRUE)
    if (!length(archs)) next
    m <- lapply(archs, read_fst)
    for (it in names(m[[1]])) {
      P <- sapply(m, `[[`, it)                      # clientes x modelos
      n <- ncol(P)
      ks <- unique(c(1, 2, 5, 10, 20, 50, 100, n)); ks <- ks[ks <= n]
      for (k in ks) {
        sorteos <- if (k == n) 1 else if (k == 1) n else 200
        v <- sapply(seq_len(sorteos), function(r) {
          cols <- if (k == 1) r else if (k == n) seq_len(n) else sample(n, k)
          meseta(rowMeans(P[, cols, drop= FALSE]), clase)
        })
        res[[length(res) + 1]] <- data.table(host, cfg, iter= it, n, k, sorteos,
          meseta= mean(v), sd= if (length(v) > 1) sd(v) else NA_real_)
      }
      preds[[paste(host, cfg, it, sep= "|")]] <- list(host= host, prob= rowMeans(P), n= n)
    }
    attr(preds, paste0("clase_", host)) <- clase
  }
  assign(paste0("clase_", host), clase)
}
res <- rbindlist(res)
options(width= 200, datatable.print.nrows= 500)
cat("Meseta (millones) por host, configuracion, iteracion y tamanio del semillerio k\n")
cat("(k=1: promedio de los modelos sueltos; k=n: el semillerio con todos)\n\n")
print(dcast(res, host + cfg + iter + n ~ k, value.var= "meseta", fun.aggregate= function(x) round(x, 1), fill= NA))
cat("\nDesvio entre sorteos\n\n")
print(dcast(res[!is.na(sd)], host + cfg + iter ~ k, value.var= "sd", fun.aggregate= function(x) round(x, 2), fill= NA))

cat("\nSemillerio completo de cada configuracion: meseta y maximo de la curva\n\n")
tot <- rbindlist(lapply(names(preds), function(nm) {
  p <- preds[[nm]]; cl <- get(paste0("clase_", p$host))
  mx <- maximo(p$prob, cl)
  data.table(id= nm, n= p$n, meseta= round(meseta(p$prob, cl), 2), maximo= round(mx[1], 2), envios_max= mx[2])
}))
print(tot[order(-meseta)])

# mezclas: promedio de rankings de los semillerios completos de 2 configuraciones del mismo host
cat("\nMezclas de a 2 (promedio de rangos, mismo host, ultima iteracion de cada config)\n\n")
ult <- tot[, .(id, host= sub("[|].*", "", id), cfg= sub("^[^|]*[|]([^|]*)[|].*", "\\1", id))]
ult <- ult[, .SD[.N], by= .(host, cfg)]
mez <- list()
for (h in unique(ult$host)) {
  ids <- ult[host == h, id]; cl <- get(paste0("clase_", h))
  if (length(ids) < 2) next
  for (par in combn(ids, 2, simplify= FALSE)) {
    pr <- (frank(preds[[par[1]]]$prob) + frank(preds[[par[2]]]$prob)) / 2
    mez[[length(mez) + 1]] <- data.table(host= h, a= par[1], b= par[2], meseta= round(meseta(pr, cl), 2))
  }
}
if (length(mez)) print(rbindlist(mez)[order(-meseta)])
