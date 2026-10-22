#!/bin/bash
# ===========================================================================
# Cola de brazos del workflow635 v3 (PR 8) en la VM (grupo B, 202107 con clase real)
#   E0 = base con canaritos, E5 = base sin canaritos, E6 = FE evolutivo,
#   E7 = evolutivo con seleccion al azar, E8 = evolutivo sin clones,
#   E9 = base con pesos 0.95^k en el final train.
#   Por cada semilla del pipeline corre todos los brazos seguidos: si la cola se
#   corta a mitad, lo que termino ya esta pareado por semilla. Las semillas son
#   las 10 asignadas (Andres y Marcos), intercaladas.
#
# Corre como ds, duena del bucket (en las VM de la catedra los demas usuarios
# no pueden leerlo). Desde un usuario con sudo, para que apague al terminar:
#   cd /tmp && nohup setsid bash -c 'sudo -u ds -H bash /home/ds/buckets/b1/wf635v3/brazos/correr_brazos.sh; sudo poweroff' > ~/cola_brazos.log 2>&1 < /dev/null &
# Opcionales (con sudo -u ds -H env VAR=... bash ...):
#   SEMILLAS="100213 100313"  BRAZOS="E0"  REINTENTOS=1
#   o, por VM, un archivo $EXP/brazos_v3/semillas_<hostname> con sus semillas
#   BORRAR_CHECKPOINTS=1   borra los .fst de checkpoints/ de cada corrida TERMINADA
# Para frenar al terminar la corrida en curso: touch $EXP/brazos_v3/PARAR
# Una corrida con TERMINADO se saltea; una que fallo se relanza y retoma desde
# sus checkpoints.
# ===========================================================================
set -u
DIR=$(cd "$(dirname "$0")/.." && pwd)
EXP=/home/ds/buckets/b1/exp
LOGS=$EXP/brazos_v3
# para repartir la cola entre varias VM: $LOGS/semillas_<hostname> con las semillas de cada una
F=$LOGS/semillas_$(hostname -s)
[ -z "${SEMILLAS:-}" ] && [ -f "$F" ] && SEMILLAS=$(tr '\n' ' ' < "$F")
SEMILLAS=${SEMILLAS:-"100213 271211 100313 353161 100613 487757 100913 641371 101113 823541"}
# v4: E6-E9 se corrieron con el codigo del PR 8; con este codigo esos .env ya no
# lo reproducen. Por defecto, la base y los brazos v4 (los terminados se saltean)
BRAZOS=${BRAZOS:-"E0 E5 E10 E11 E12"}
REINTENTOS=${REINTENTOS:-1}
BORRAR_CHECKPOINTS=${BORRAR_CHECKPOINTS:-1}

if [ "$(id -un)" != ds ]; then echo "correr como ds: sudo -u ds -H bash $0"; exit 1; fi
mkdir -p "$LOGS"
# sudo deja el directorio del usuario que lanzo, que ds no puede leer, y
# install.packages vuelve a el al terminar
cd "$DIR" || exit 1
exec 9>/tmp/correr_brazos.lock
flock -n 9 || { echo "ya hay una cola corriendo"; exit 1; }

log() { echo "$(date '+%F %T') | $*" >> "$LOGS/cola.log"; }
VERSION="v3 $(md5sum "$DIR/635_workflow_senior.R" | cut -c1-8)"
[ -f "$LOGS/tiempos.tsv" ] || printf 'corrida\tbrazo\tsemilla\tintento\trc\tminutos\n' > "$LOGS/tiempos.tsv"
log "cola: semillas [$SEMILLAS] brazos [$BRAZOS] script $VERSION"

# paquetes una sola vez, antes de la primera corrida
Rscript -e '
lib <- Sys.getenv("R_LIBS_USER"); dir.create(lib, recursive= TRUE, showWarnings= FALSE)
.libPaths(c(lib, .libPaths()))
p <- c("data.table", "R.utils", "fst", "lightgbm", "mlrMBO", "DiceKriging",
       "lhs", "lubridate", "mice", "yaml", "Rcpp")
f <- p[!vapply(p, requireNamespace, logical(1), quietly= TRUE)]
if (length(f)) install.packages(f, lib= lib, repos= "https://cloud.r-project.org",
                                Ncpus= parallel::detectCores())
f <- p[!vapply(p, requireNamespace, logical(1), quietly= TRUE)]
if (length(f)) { cat("FALTAN:", f, "\n"); quit(status= 1) }
cat("paquetes OK\n")' >> "$LOGS/paquetes.log" 2>&1 || { log "faltan paquetes: ver paquetes.log"; exit 1; }

for s in $SEMILLAS; do
  for b in $BRAZOS; do
    if [ -f "$LOGS/PARAR" ]; then log "PARAR: freno la cola"; exit 0; fi
    ENV=$DIR/brazos/$b.env
    NUM=$(sed -n 's/^EXPERIMENTO=//p' "$ENV")
    RUN=WF${NUM}_s$s
    if [ -f "$EXP/$RUN/TERMINADO" ]; then log "$RUN ya estaba terminada"; continue; fi
    for intento in $(seq 0 "$REINTENTOS"); do
      log "$RUN arranca (intento $intento, $(free -g | awk '/^Mem/{print $7}') GB libres)"
      # RAM: el pico de cada corrida queda en $LOGS/ram_<corrida>.tsv (lo que htop mostraria)
      ( while sleep 60; do echo "$(date '+%F %T')	$(free -g | awk '/^Mem/{print $3}')"; done ) >> "$LOGS/ram_$RUN.tsv" &
      RAM_PID=$!
      t0=$(date +%s)
      ( cd "$DIR" && WF_VERSION="$VERSION" Rscript 635_workflow_senior.R "$ENV" "$s" ) >> "$LOGS/$RUN.log" 2>&1
      rc=$?
      kill "$RAM_PID" 2>/dev/null
      log "$RUN RAM usada maxima: $(cut -f2 "$LOGS/ram_$RUN.tsv" | sort -n | tail -1) GB"
      min=$(( ($(date +%s) - t0) / 60 ))
      printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$RUN" "$b" "$s" "$intento" "$rc" "$min" >> "$LOGS/tiempos.tsv"
      if [ -f "$EXP/$RUN/TERMINADO" ]; then
        log "$RUN TERMINADO en $min min, meseta $(cat "$EXP/$RUN/ganancia_meseta.txt" 2>/dev/null), maximo $(tail -n 1 "$EXP/$RUN/ganancia_202107.txt" 2>/dev/null | cut -f2)"
        [ "$BORRAR_CHECKPOINTS" = 1 ] && rm -f "$EXP/$RUN"/checkpoints/*.fst
        break
      fi
      log "$RUN fallo (rc $rc, $min min): ver $LOGS/$RUN.log"
    done
    Rscript "$DIR/brazos/analizar_brazos.R" "$EXP" > "$LOGS/analisis.txt" 2>&1 || log "analizar_brazos.R fallo: ver analisis.txt"
  done
done
log "cola terminada"
