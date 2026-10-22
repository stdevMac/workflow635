# workflow635

El Workflow 01 senior de la cátedra (`z635_WorkFlow_01_senior.ipynb`, sin
modificar) llevado a un script que corre desatendido en las VM Spot de la
cátedra, para los dos grupos del experimento colaborativo.

| | Grupo A | Grupo B |
|---|---|---|
| Validación de la BO | 202107 | 202105 |
| Final train | 201901 – 202107 | 201901 – 202105 |
| Predice | 202109 | 202107 |
| Testing | Kaggle (sube los cortes 8000 – 12000) | ganancia contra la clase real de 202107 |

> **Resultados finales (grupo B, 2026-10-22):** ver [RESULTADOS.md](RESULTADOS.md): brazos E0–E11, el laboratorio de final train (`lab/`) y cómo reproducir cada número.

## Qué cambia respecto del z635

Las funciones y celdas de la cátedra están copiadas **sin cambios** en
`635_workflow_senior.R`; cada bloque dice de qué celda viene. Lo agregado:

- **FE evolutivo** (v3, PR 8), después del FE histórico y del de random
  forest. Reemplaza a la genética de `gramEvol` de la v1/v2. Ver
  [El FE evolutivo](#el-fe-evolutivo-v3).
- **Configuración en `.env`**: grupo, rutas, mail, Kaggle. Si falta una
  variable, corta al arrancar con su nombre.
- **Semillas fijas**: primigenia 100213, semillerio de 9:
  `100213, 100313, 100613, 100913, 101113` más 4 primos sorteados
  `570991, 669287, 832421, 882599` (en vez de 10 primos al azar).
- **Mail a una lista**: arranque, fin de cada etapa, cada N evaluaciones de la
  BO, error (con la etapa), fin, y si GCP expropia la VM. Un mail que falla no
  corta la corrida.
- **Registro de la corrida**: el mail final (y `registro.txt` en la carpeta)
  trae la versión del script (commit), grupo, línea de tiempo, semillas,
  tamaño del dataset final, mejor AUC y los hiperparámetros del modelo final,
  la duración de cada etapa, el `PARAM.yml` completo y la lista de archivos.
- **Checkpoints en el bucket**, para que una expropiación no pierda trabajo:

  | Qué | Archivo (en `exp/WF<experimento>/`) |
  |---|---|
  | dataset después del FE histórico | `checkpoints/dataset_fe_historico.fst` |
  | dataset después de canaritos | `checkpoints/dataset_canaritos.fst` |
  | cada punto del diseño inicial de la BO | `BO_init.txt` |
  | cada evaluación de la BO | `BO_iter.txt` |
  | estado de la BO en cada iteración | `HT.RDATA` |
  | un modelo final por semilla | `modelos/modelo_<semilla>.txt` |
  | cada archivo subido a Kaggle | `kaggle/*.csv.subido` |

  El z635 solo grababa la BO cada 600 s y nunca durante el diseño inicial (20
  entrenamientos): si se cortaba ahí, se perdía todo.

## El FE evolutivo (v3)

Lo que salió de la clase y del experimento de brazos v2 (la genética de
`gramEvol` no le ganó a sortear fórmulas: ver [brazos](brazos/README.md)):
no buscar **una** super-variable por ronda, sino hacer evolucionar **toda la
población de columnas** con un tamaño fijo, y que la importancia decida
quién se reproduce y quién muere.

Cada generación (`GA_MODO=evolutivo`):

1. **Importancia:** el LightGBM de canaritos de la cátedra (z635 celda 66,
   con sus mismos parámetros y early stopping sobre la ganancia meseta)
   entrena con **todo lo posible**: todas las columnas de la población, todos
   los meses con clase menos los reservados del grupo y todos los CONTINUA.
   En el grupo A entrena 201901-202103 y valida 202105 (nunca 202107 ni
   202109); en el B, 201901-202101 y valida 202103. Usa todos los núcleos de
   la VM (la cátedra usa 1; cambia la velocidad, no el modelo). Da la
   importancia (gain) de cada columna. El evolutivo no tiene un LightGBM
   propio. La ganancia en el mes de validación queda en el registro.
2. **Reproducción (`X * 1`):** pasa igual a la generación siguiente **toda
   variable que sirve**: con `EV_PISO_CANARITOS` (default), la que le gana a
   columnas al azar en el LightGBM de la importancia, con la regla de la
   cátedra (celda 66); sin piso, la que tiene importancia > 0. Con
   `EV_ORIGINALES_FIJAS` (default) las originales pasan siempre y solo
   compiten las fórmulas, por `EV_FORMULAS` lugares. Lo que fue bueno sigue
   sin cambiar y lo que no sirve no pasa: la importancia decide qué queda. El único resguardo es
   `EV_MIN_HIJOS` (25 % del tope): si las usadas no entran todas dejándole
   ese mínimo de lugares a los hijos, pasan las de más importancia. El
   registro marca las generaciones en que actuó.
3. **Cruza:** los lugares que quedan hasta el tope son hijos `padre1 op
   padre2` (`op` en `+ − × /`, con NaN → 0 e Inf → NA en cada operación). Los
   padres se sortean con probabilidad según su puesto en la importancia
   (ranking lineal: el primero es `EV_PRESION` veces más probable que el
   último y nadie tiene probabilidad 0). Los padres pueden ser de **cualquier
   generación**, y también una variable que no pasa puede ser padre: su
   información sigue en el hijo.
4. **Medición:** el LightGBM de la generación siguiente mide a las copias y a
   los hijos juntos. Por defecto el tope es la cantidad de columnas con la que
   llega el dataset. **La población nunca baja.**

Al final la población entra al dataset entero (las fórmulas se escriben con
los nombres de las columnas originales y se recalculan) y las originales
muertas se borran. Canaritos no corre por defecto con el evolutivo: achica el
dataset a ~450 columnas, justo lo contrario de lo que se busca (`CN_RUN=1` lo
prende igual).

**Mes de prueba.** En cada generación el LightGBM de la importancia predice
también un mes con clase que el evolutivo nunca usa para decidir (A: 202107;
B: 202105). Su ganancia queda en el registro (`ganancia_meseta_prueba`): dice si
la población mejora fuera de lo que vio.

**Búsqueda de fórmulas (`EV_SOLO=1`, `EV_CONTROL=1`).** Corre el FE y el
evolutivo por importancia, después el mismo evolutivo con selección al azar
sobre el mismo dataset, y termina. Graba `evolutivo_busqueda.txt` con las dos
curvas en el mes de prueba y las mejores fórmulas, y `evolutivo_mejores.txt`.
Si la curva por importancia sube y queda por encima de la del azar, la
corrida completa usa esa población con `EV_POBLACION_DESDE`, sin volver a
evolucionar.

**Lo que queda registrado:**

| Archivo | Qué |
|---|---|
| `evolutivo_generaciones.txt` | una fila por generación: población, **sirven** (columna `usadas`: le ganan a las canaritos, o importancia > 0 sin piso), canaritos y su umbral, copias `X * 1` de fórmulas, si actuó el mínimo de hijos, nacidas, las que no pasan, **originales vivas**, generadas vivas, profundidad máxima y media, fracción de hijos con padres de generaciones distintas, **fórmulas nacidas acumuladas**, % de la importancia que tienen las generadas, ganancia en la validation de canaritos (meseta y máximo) y segundos |
| `evolutivo_nacimientos.txt` | cada hijo y cada clon: fórmula, padres y generación de cada padre |
| `evolutivo_poblacion.txt` | la población final con su importancia |
| `checkpoints/evolutivo.rds` | el estado al final de cada generación: si la VM se corta, retoma desde ahí |

Un mail por generación con la tabla de arriba, y el registro final dice
cuántas generadas usa el modelo final y en qué puestos.

**Variables del `.env`** (todas opcionales):

| Variable | Default | Qué hace |
|---|---|---|
| `GA_MODO` | `evolutivo` | `evolutivo` o `nada` (sin FE evolutivo, la base) |
| `EV_SELECCION` | `importancia` | `azar`: las copias (la misma cantidad) y los padres al azar (el control de si la importancia sirve) |
| `EV_GENERACIONES` | 10 | |
| `EV_TOPE` | 0 | tamaño de la población; 0 = las columnas con las que llega el dataset |
| `EV_UNDERSAMPLING` | 1 | fracción de los CONTINUA en el LightGBM de la importancia (1 = todos) |
| `EV_ORIGINALES_FIJAS` | 1 | las originales pasan siempre y la genética agrega hasta `EV_FORMULAS` fórmulas; 0: compite toda la población por el tope y las fórmulas reemplazan columnas |
| `EV_FORMULAS` | 500 | lugares para fórmulas, con originales fijas |
| `EV_PISO_CANARITOS` | 1 | sirve (y pasa `X * 1`) solo lo que le gana a columnas al azar en el LightGBM de la importancia, con la regla de la cátedra (celda 66: puesto < mediana de las canaritos + `CN$desvios` desvíos, `CN$ratio` × columnas de canaritos). 0: sirve con importancia > 0 |
| `EV_HISTORIA` | 1 | cada fórmula tiene su propia historia (`_lag1`, `_lag2`, `_delta1`, `_delta2`, `_tend6`, como el FE histórico) en cada generación y en el dataset final; su importancia es la suma de las 6 columnas. 0: la fórmula sola |
| `EV_SOLO` | 0 | 1: corrida de búsqueda. FE + evolutivo, graba la población y las mejores fórmulas y termina (sin BO, modelo final ni Kaggle) |
| `EV_CONTROL` | 0 | 1 (con `EV_SOLO`): en la misma corrida corre después el evolutivo con selección al azar, para comparar |
| `EV_POBLACION_DESDE` | | ruta a un `evolutivo_poblacion.txt` de una búsqueda: no evoluciona, mete esa población y sigue con el workflow completo |
| `EV_MIN_HIJOS` | 0.25 | lugares mínimos para hijos, en fracción del tope (1 = sin copias `X * 1`) |
| `EV_PRESION` | 10 | cuántas veces más probable es el primero de la importancia que el último |
| `EV_SEMILLAS_N` | 1 | LightGBM por generación (importancia promediada) |
| `EV_BORRAR_MUERTAS` | 1 | 0: las originales muertas quedan en el dataset |
| `CN_RUN` | 0 con evolutivo, 1 sin | correr canaritos |
| `FT_ALFA` | 1 | pesos `alfa^k` por mes en el final train (k = meses antes del último); la clase sugirió 0.95 |
| `HT_FIJO` | | ruta a un `BO_log.txt` de otra corrida: se usa su mejor fila y no se corre la BO |
| `FT_SEMILLAS_N` | 9 | cuántas semillas del semillerío final usar |
| `FT_UNDERSAMPLING` | 1 | fracción de CONTINUA en el final train |
| `KAGGLE_SUBIR=0` | | grupo A sin subir a Kaggle (los CSV quedan igual) |

Las variables `GA_FORZAR`, `GA_VALIDA_*`, `GA_VENTANA`, `GA_RONDAS`, `TRF_*`
y los modos `genetica`, `azar` y `transferencia` de la v2 ya no existen.

En grupo B la corrida graba `ganancia_202107.txt` (los 9 cortes y, en la
última fila, el máximo de la curva), `ganancia_maximo.txt` y
`ganancia_meseta.txt`. La comparación entre brazos usa la **meseta** por
semilla con Wilcoxon apareado y el máximo (lo que pidió la clase) como
secundaria, porque el máximo es optimista y más ruidoso (ver PR 9).

**Prueba:** `tests/test_evolutivo.R` corre el evolutivo sobre un dataset
sintético de 20 variables (segundos) y verifica que la población quede en el
tope, que pasen todas las usadas de la generación anterior (originales y
fórmulas), que siempre haya lugar para hijos, recombinación entre generaciones, el conteo de
fórmulas, que se retome desde el checkpoint y que las fórmulas se recalculen
igual en el dataset entero:

```bash
Rscript tests/test_evolutivo.R 635_workflow_senior.R
```

**¿El evolutivo mejora el modelo final?** Para eso está el
[experimento de brazos](brazos/README.md): grupo B, 10 semillas pareadas,
base vs. evolutivo vs. evolutivo con selección al azar vs. sin copias `X * 1`.

## Puesta en marcha

Todo se hace desde **Git Bash** en tu PC, con `gcloud` instalado y logueado
con la cuenta dueña del proyecto de la cátedra (`gcloud auth login`). Nada
secreto va al repo, que es público: la clave del mail, los destinatarios y
el `kaggle.json` viven en tu bucket.

### 1. Tus datos de GCP

```sh
gcloud projects list                          # PROJECT_ID de tu proyecto de la cátedra
gcloud compute instance-templates list        # tienen que aparecer los to-128ram-...
gsutil ls                                      # el nombre de tu bucket (gs://...)
gsutil ls gs://<bucket>/datasets/              # tiene que estar analistasr_competencia_2026.csv.gz
```

Si no ves los templates `to-...`, tu proyecto no tiene la infra de la cátedra
armada: hay que correr primero su script de alta.

### 2. El mail

El script manda los mails desde una cuenta de Gmail con una **app password**
(una clave de 16 letras solo para esto; no es la clave de la cuenta).

1. En la cuenta que va a mandar, activá la **verificación en 2 pasos**:
   https://myaccount.google.com/signinoptions/twosv (sin eso Google no deja
   crear app passwords).
2. Creá la app password en https://myaccount.google.com/apppasswords
   (nombre: `workflow635`). Copiá las 16 letras **sin espacios**.
3. Subila al bucket (reemplazá las x y el bucket):
   ```sh
   printf '%s' 'xxxxxxxxxxxxxxxx' | gsutil cp - gs://<bucket>/secrets/gmail_app_password
   ```
4. Los destinatarios, uno por línea (puede ser la misma cuenta u otras):
   ```sh
   printf 'vos@gmail.com\notro@gmail.com\n' | gsutil cp - gs://<bucket>/wf635/destinatarios.txt
   ```
5. Probá que anda **antes de lanzar**, desde Git Bash (`curl` ya viene):
   ```sh
   DESDE=tu_cuenta@gmail.com; PARA=vos@gmail.com
   printf "From: $DESDE\nTo: $PARA\nSubject: prueba workflow635\n\nanda\n" > /tmp/m.txt
   curl -s --ssl-reqd --url smtps://smtp.gmail.com:465 --mail-from $DESDE --mail-rcpt $PARA \
     --user "$DESDE:$(gsutil cat gs://<bucket>/secrets/gmail_app_password)" -T /tmp/m.txt; echo "codigo: $?"
   ```
   `codigo: 0` = enviado. `67` = Google rechazó el login: la app password no
   es de esa cuenta o tiene espacios.

Los mails llegan en **dos hilos** por experimento: `[WF<exp> grupo A]
experimento` (arranque, fin de cada etapa con sus tiempos, errores, VM
expropiada, registro final) e `[WF<exp> grupo A] iteraciones` (cada
generación del evolutivo y el avance de la BO).

### 3. Kaggle (solo grupo A)

Bajá tu `kaggle.json` de https://www.kaggle.com/settings (API → Create New
Token) y subilo:

```sh
gsutil cp kaggle.json gs://<bucket>/kaggle/kaggle.json
```

El CLI de Kaggle lo instala el script en la VM. Si falta el `kaggle.json`,
llega un mail de aviso al arrancar y al final los archivos quedan en
`exp/WF<exp>/kaggle/` para subir a mano.

### 4. Configuración y lanzamiento

```sh
cp vm/vm.env.example vm/vm.env     # PROJECT, BUCKET, TEMPLATE (to-128ram-12vcpu)
cp .env.example .env.A             # GRUPO=A, EXPERIMENTO, MAIL_FROM=tu_cuenta@gmail.com
vm/lanzar.sh A
```

`EXPERIMENTO` es la carpeta de salida (`exp/WF<EXPERIMENTO>`): uno distinto
por corrida, si no retoma la anterior. Los `.env.*` y `vm/vm.env` no se
commitean (están en el `.gitignore`).

`lanzar.sh`:

1. chequea la cuota de vCPUs;
2. sube el script y el `.env` a `gs://<bucket>/wf635/<grupo>/`;
3. crea un template propio, copia del de la cátedra (misma máquina, disco,
   imagen, Spot) con `vm/startup.sh` y `vm/shutdown.sh`;
4. crea un **grupo administrado de una VM** (`wf635-a` / `wf635-b`).

**Si GCP expropia la VM**, llega un mail y el grupo crea otra sola, que
retoma desde el último checkpoint. No hay que hacer nada.

**Al terminar, o si R falla**, la VM espera 30 minutos y baja el grupo a 0,
así no se recrea (con `APAGAR_AL_TERMINAR=1`). Si R muere sin poder avisar
(por ejemplo, sin memoria), el startup manda el mail.

Para pararlo a mano: `vm/parar.sh A`. Para seguir después: `vm/lanzar.sh A`.

Log en vivo:

```sh
gsutil cat gs://<bucket>/wf635/A/log.txt | tail -50
```

### 5. Ensamble de semillas (grupo A)

`ensamblar.R` promedia la `prob` del `prediccion.txt` de varias corridas
terminadas y escribe los mismos 9 cortes que el workflow, para subir a mano.
En una VM con el bucket montado y el repo copiado en
`/home/ds/buckets/b1/wf635v2` (como en [brazos](brazos/README.md#cómo-correrlo)):

```sh
E=/home/ds/buckets/b1/exp
cd /tmp
sudo -u ds -H Rscript /home/ds/buckets/b1/wf635v2/ensamblar.R $E/ENS_E2A $E/WF9912_s100213 $E/WF9912_s100313 $E/WF9912_s271211
```

Deja `ENS_E2A/kaggle/KAENS_E2A_<envios>.csv` y la `prob` promedio, con la de
cada corrida al lado, en `ENS_E2A/prediccion.txt`. En el LB público, promediar
3 semillas sumó de 2 a 5 M de meseta contra una corrida suelta
([resultados](brazos/README.md#resultados-2026-10-13)).

## Las máquinas

Los templates de la cátedra están en el proyecto
(`gcloud compute instance-templates list`): Spot, n2 custom, de 128 a 512 GB,
con 8 o 12 vCPU, imagen `labo-family`. Los de la cátedra borran la VM al
expropiarse (`DELETE`), pero GCP no acepta eso en un grupo administrado: el
template propio usa `STOP`, y el grupo vuelve a arrancar la VM detenida.

Con una sola corrida a la vez conviene `TEMPLATE=to-128ram-12vcpu` en
`vm/vm.env`: usa los 12 vCPU de la cuota.

Detalles de la imagen que el script ya resuelve:

- el bucket lo monta gcsfuse para el usuario `ds` y **root no lo puede leer**:
  los scripts de la VM acceden como `ds`;
- gcsfuse sube un archivo recién al cerrarlo: el log se escribe en el disco
  local y se copia al bucket cada minuto;
- el Python es "externally managed" (PEP 668): el CLI de Kaggle se instala con
  `pip --user --break-system-packages`;
- desde Git Bash, `lanzar.sh` evita que las rutas del metadata se conviertan en
  rutas de Windows.

La cuota global de este proyecto es de **12 vCPUs**: dos VM de
`to-128ram-08vcpu` (8 + 8) no entran a la vez. `lanzar.sh` lo chequea antes
de crear nada.

## Primera corrida (WF9500, grupo A, 2026-10-10)

Corre en `to-128ram-12vcpu`. Tiempos medidos hasta la BO:

| Etapa | min |
|---|---|
| lectura + CA + drift + FE intra-mes | ~2 |
| FE genético | 11,4 |
| FE histórico | 6,1 |
| FE random forest | 3,1 |
| canaritos | 4,0 |

La genética evaluó 230 fórmulas distintas (~3 s cada una). La mejor,
`vm_mconsumosdolares * vmr_mpagado`, mejoró 2,26 M, por debajo del umbral de
5,7 M (2 desvíos de un ruido de 2,8 M): **ninguna variable genética quedó**.
El fitness barato y la búsqueda chica (600 contra 9.600 del default de
gramEvol) son lo primero a revisar.

Esa corrida usaba para el fitness solo 202101-202105 con el 10% de los
CONTINUA, y cortaba la genética en la primera ronda que no superaba el ruido.
Las dos cosas se cambiaron: el fitness usa los meses de la training strategy
del z635 (201901-202105, 20% de CONTINUA, mide en 202107: 1.037.931 filas,
ruido 3,1 M) y corren todas las rondas. La corrida con eso es **WF9501**.

Lo que la corrida confirmó: el uso de `gramEvol` (la fórmula llega a la
función de costo y el monitor por generación funciona), los checkpoints de
FE, el mail y la VM que retoma desde el bucket.

Pendiente:

- **El fitness de la genética.** Es un LightGBM de 200 árboles sobre una
  muestra sin lags: no está validado que prediga la ganancia del workflow
  completo. El [experimento de brazos](brazos/README.md#resultados-2026-10-13)
  pasó sus ganadoras por el workflow entero: no le ganan a 3 fórmulas
  sorteadas de la misma gramática. Lo que queda es un fitness más fiel
  (dataset completo, lags de la fórmula, varias semillas).
- **El tamaño de la búsqueda** (`PARAM$GA$poblacion`, `generaciones`), que
  con ~3 s por fórmula puede crecer mucho.
