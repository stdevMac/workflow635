#!/bin/bash
# Lanza el workflow de un grupo en una VM Spot que se recrea sola.
#
#   vm/lanzar.sh A        (o B)
#
# Arma un grupo administrado de GCP (MIG) de UNA VM. Si GCP expropia la VM,
# el grupo crea otra igual, que retoma desde los checkpoints del bucket.
# Al terminar (o ante un error), la VM baja el grupo a 0 y no se recrea mas.
#
# Pasos:
#   1. sube 635_workflow_senior.R y .env.<grupo> a gs://$BUCKET/wf635/<grupo>/
#   2. crea un template propio copiando el de la catedra ($TEMPLATE: maquina,
#      disco, imagen, Spot) y agregando vm/startup.sh y vm/shutdown.sh
#   3. crea el grupo, o lo vuelve a 1 si existia en 0
set -euo pipefail
cd "$(dirname "$0")/.."
# Git Bash convierte en rutas de Windows los argumentos con /: el metadata
# bucket-dir=/home/ds/... le llegaba a la VM como C:/Program Files/Git/home/ds/...
# Se excluye solo ese argumento (apagar la conversion entera rompe a gcloud)
export MSYS2_ARG_CONV_EXCL="wf-dir="

G=$(echo "${1:?uso: vm/lanzar.sh A|B}" | tr '[:lower:]' '[:upper:]')
[ "$G" = "A" ] || [ "$G" = "B" ] || { echo "el grupo es A o B"; exit 1; }
g=$(echo "$G" | tr '[:upper:]' '[:lower:]')

[ -f vm/vm.env ] || { echo "falta vm/vm.env (copiar vm/vm.env.example y completarlo)"; exit 1; }
. vm/vm.env
for v in PROJECT REGION ZONE TEMPLATE BUCKET BUCKET_DIR_VM VM_PREFIJO APAGAR_AL_TERMINAR; do
  [ -n "${!v:-}" ] || { echo "falta $v en vm/vm.env"; exit 1; }
done

ENVF=".env.$G"
[ -f "$ENVF" ] || { echo "falta $ENVF (copiar .env.example y completarlo)"; exit 1; }
grep -q "^GRUPO=$G\$" "$ENVF" || { echo "$ENVF no dice GRUPO=$G"; exit 1; }

DIR="$VM_PREFIJO/$G"
MIG="$VM_PREFIJO-$g"

# --- el grupo ya esta corriendo? -------------------------------------------
TAM=$(gcloud compute instance-groups managed describe "$MIG" --zone "$ZONE" --project "$PROJECT" \
  --format="value(targetSize)" 2>/dev/null || true)
if [ -n "$TAM" ] && [ "$TAM" != "0" ]; then
  echo "el grupo $MIG ya esta corriendo (tamano $TAM). Para pararlo: vm/parar.sh $G"
  exit 1
fi

# --- cuota global de vCPUs del proyecto -------------------------------------
CAT="projects/$PROJECT/regions/$REGION/instanceTemplates/$TEMPLATE"
describir() { gcloud compute instance-templates describe "$TEMPLATE" --region "$REGION" \
  --project "$PROJECT" --format="value($1)"; }
MAQUINA=$(describir properties.machineType)
VCPU=$(echo "$MAQUINA" | cut -d- -f3)
read LIMITE USO < <(gcloud compute project-info describe --project "$PROJECT" \
  --flatten=quotas --format="csv[no-heading](quotas.metric,quotas.limit,quotas.usage)" \
  | awk -F, '$1 == "CPUS_ALL_REGIONS" {print $2, $3}')
echo "cuota CPUS_ALL_REGIONS: usadas ${USO%.*} de ${LIMITE%.*}; $TEMPLATE pide $VCPU"
if [ $(( ${USO%.*} + VCPU )) -gt ${LIMITE%.*} ]; then
  echo "NO ENTRA en la cuota: parar otra VM o usar un template mas chico"; exit 1
fi

# --- subir script y .env (sin CRLF: Windows los rompe para bash y R) -------
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
# gcloud en Windows no entiende las rutas /tmp de Git Bash
TMP_GC=$(cygpath -m "$TMP" 2>/dev/null || echo "$TMP")
for f in 635_workflow_senior.R "$ENVF" vm/startup.sh vm/shutdown.sh; do
  tr -d '\r' < "$f" > "$TMP/$(basename "$f")"
done
# que version del script corrio: queda en el registro y en los mails
VERSION=$(git rev-parse --short HEAD 2>/dev/null || echo "sin-git")
git diff --quiet HEAD -- 635_workflow_senior.R 2>/dev/null || VERSION="$VERSION-con-cambios-sin-commitear"
echo "WF_VERSION=$VERSION" >> "$TMP/$ENVF"
gsutil cp "$TMP/635_workflow_senior.R" "gs://$BUCKET/$DIR/635_workflow_senior.R"
gsutil cp "$TMP/$ENVF" "gs://$BUCKET/$DIR/.env"

# --- template propio: copia del de la catedra + nuestros scripts -----------
# Los templates no se pueden modificar: el nombre lleva un hash del contenido,
# asi un cambio en los scripts crea uno nuevo.
IMAGEN=$(describir "properties.disks[0].initializeParams.sourceImage")
IMG_PROYECTO=$(echo "$IMAGEN" | sed -E 's#.*/projects/([^/]+)/.*#\1#')
IMG_FAMILIA=$(echo "$IMAGEN" | sed -E 's#.*/family/([^/]+)$#\1#')
DISCO_GB=$(describir "properties.disks[0].initializeParams.diskSizeGb")
DISCO_TIPO=$(describir "properties.disks[0].initializeParams.diskType")
CUENTA=$(describir "properties.serviceAccounts[0].email")
TAGS=$(describir "properties.tags.items" | tr ';' ',')

METADATA="wf-dir=$DIR,bucket-dir=$BUCKET_DIR_VM,apagar-al-terminar=$APAGAR_AL_TERMINAR,proyecto=$PROJECT,mig=$MIG"
# STOP y no DELETE: GCP no acepta Spot con DELETE en un grupo administrado.
# Con STOP el grupo vuelve a arrancar la VM detenida cuando hay capacidad.
HASH=$( (cat "$TMP/startup.sh" "$TMP/shutdown.sh"; echo "$TEMPLATE $METADATA STOP") | md5sum | cut -c1-8)
PROPIO="$MIG-$HASH"

if gcloud compute instance-templates describe "$PROPIO" --region "$REGION" --project "$PROJECT" >/dev/null 2>&1; then
  echo "template $PROPIO ya existe"
else
  gcloud compute instance-templates create "$PROPIO" --project "$PROJECT" \
    --instance-template-region "$REGION" \
    --machine-type "$MAQUINA" \
    --image-project "$IMG_PROYECTO" --image-family "$IMG_FAMILIA" \
    --boot-disk-size "${DISCO_GB}GB" --boot-disk-type "$DISCO_TIPO" \
    --provisioning-model SPOT --instance-termination-action STOP \
    --maintenance-policy TERMINATE --no-restart-on-failure \
    --service-account "$CUENTA" --scopes cloud-platform \
    --tags "$TAGS" \
    --shielded-secure-boot --shielded-vtpm --shielded-integrity-monitoring \
    --metadata-from-file startup-script="$TMP_GC/startup.sh",shutdown-script="$TMP_GC/shutdown.sh" \
    --metadata "$METADATA"
fi
URL_PROPIO="projects/$PROJECT/regions/$REGION/instanceTemplates/$PROPIO"

# --- el grupo de una VM -----------------------------------------------------
if [ -z "$TAM" ]; then
  gcloud compute instance-groups managed create "$MIG" --project "$PROJECT" --zone "$ZONE" \
    --template "$URL_PROPIO" --size 1
else
  gcloud compute instance-groups managed set-instance-template "$MIG" --project "$PROJECT" \
    --zone "$ZONE" --template "$URL_PROPIO"
  gcloud compute instance-groups managed resize "$MIG" --project "$PROJECT" --zone "$ZONE" --size 1
fi

echo
echo "Grupo $MIG en marcha. Si GCP expropia la VM, la recrea sola."
echo "Log:    gsutil cat gs://$BUCKET/$DIR/log.txt | tail -50"
echo "Parar:  vm/parar.sh $G"
