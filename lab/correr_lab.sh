#!/bin/bash
# Corre el laboratorio de final train en un host (como ds).
#   LAB_CONFIGS="base:10,debil:100" [LAB_HOST=nombre] [LAB_DESDE=100] bash correr_lab.sh
# 1) si falta, genera el dataset de E5 (semilla 100213) corriendo el workflow con
#    brazos/L0.env hasta que graba checkpoints/dataset_canaritos.fst, y lo corta ahi
# 2) corre lab_ft.R; las salidas quedan en $EXP/lab/<host>/
set -u
DIR=$(cd "$(dirname "$0")/.." && pwd)
EXP=/home/ds/buckets/b1/exp
H=${LAB_HOST:-$(hostname -s)}
OUT=$EXP/lab/$H
mkdir -p "$OUT"
log() { echo "$(date '+%F %T') | $*" >> "$OUT/lab.log"; }
FST=$EXP/WF9940_s100213/checkpoints/dataset_canaritos.fst
exec 9>/tmp/correr_lab.lock
flock -n 9 || { echo "ya hay un lab corriendo"; exit 1; }

if [ ! -f "$FST" ]; then
  log "genero el dataset de E5 (WF9940_s100213)"
  ( cd "$DIR" && WF_VERSION="lab" Rscript 635_workflow_senior.R brazos/L0.env 100213 ) >> "$OUT/fe.log" 2>&1 &
  PID=$!
  while kill -0 $PID 2>/dev/null && [ ! -f "$FST" ]; do sleep 20; done
  sleep 5; pkill -P $PID 2>/dev/null; kill $PID 2>/dev/null
  [ -f "$FST" ] || { log "no se genero $FST: ver fe.log"; exit 1; }
  log "dataset listo: $(du -h "$FST" | cut -f1)"
fi

log "lab: $LAB_CONFIGS"
( while sleep 60; do echo "$(date '+%F %T')	$(free -g | awk '/^Mem/{print $3}')"; done ) >> "$OUT/ram.tsv" &
RAM=$!
Rscript "$DIR/lab/lab_ft.R" "$FST" "$OUT" "$LAB_CONFIGS" >> "$OUT/lab.log" 2>&1
rc=$?
kill $RAM
log "lab_ft.R rc $rc"
exit $rc
