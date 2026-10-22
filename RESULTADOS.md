# Resultados finales del workflow635 (grupo B)

Resumen de lo que corrimos entre el 2026-10-10 y el 2026-10-22: qué se probó,
con qué código, en qué máquina y cómo reproducir cada número. Todas las
ganancias son la **meseta**: el promedio de la ganancia en los cortes
8000, 8500, …, 12000. Es la métrica principal de los brazos; el PR 9 mostró que el
máximo de la curva es optimista en 10–12 M. Cada BAJA+2 suma 975.000 y cualquier otro
cliente resta 25.000. Los montos están en millones.

## TL;DR

| | Meseta 202107 | Corridas | Comentario |
|---|---|---|---|
| E0: base de la cátedra (con canaritos) | 582,3 | 12 semillas | |
| E5: base sin canaritos | 586,9 | 12 semillas | E5 − E0 = +4,6 (gana 12 de 12, Holm 0,003) |
| E6: FE evolutivo v3 | 583,8 | 12 semillas | E6 − E5 = −3,1 (p 0,38) |
| E10: originales fijas + 500 fórmulas | 587,8 | 1 semilla | igual a E5 en esa semilla (588,6) |
| **E11: E10 + historia de cada fórmula + piso de canaritos** | **606,2** | **1 semilla** | **+17,7 sobre E5 en la misma semilla** |

1. **Sacar canaritos ayuda** (E5 > E0, 12 de 12 semillas). Es el único resultado
   que tiene test con 12 semillas.
2. **El evolutivo que reemplaza columnas (E6) no mejora** a E5. Con las originales
   fijas (E10) deja de perder, pero tampoco gana.
3. **E11 es lo mejor que vimos: 606,2.** Supera a las 12 semillas de E5, cuyo
   máximo es 593,3. Pero es **una sola semilla**, medida solo en 202107; ver
   [qué falta](#qué-falta-para-darlo-por-bueno).
4. **Final train** (laboratorio con 202105 como validación): el bagging de negativos
   que en 202107 sumaba +3 a +6 por modelo **no se sostiene en 202105**. Ahí
   la base gana: 260,3 contra 258,2 del mejor bag. Recomendación: mantener el
   final train de la base.

## Código

| Qué | Commit / archivo |
|---|---|
| E0–E9 (v3) | PR 8, `exp/brazos_v3/analisis.txt` en el bucket |
| E10 (`WF9930_s100213`) | `fbf6866` (PR 10). En v4, `brazos/E10.env` es WF9933: el WF9930 no se reproduce con este código (ver el `.env`) |
| E11 (`WF9931_s100213`) | `841047d` (PR 12), `brazos/E11.env`. md5 de `635_workflow_senior.R`: `977bf489` (queda en `registro.txt` y en `cola.log`) |
| Laboratorio de final train | `lab/` (en este PR), `brazos/L0.env` |

## E11 en detalle

Configuración (`brazos/E11.env`):

- **Base:** la de E10, con `EV_ORIGINALES_FIJAS=1`, `EV_FORMULAS=500`,
  `EV_GENERACIONES=10` y selección por importancia.
- **Lo nuevo:**
  - `EV_HISTORIA=1`: cada fórmula entra con lag1, lag2, delta1, delta2 y tend6.
  - `EV_PISO_CANARITOS=1`: una fórmula sirve solo si le gana a las columnas al azar.
  - `EV_UNDERSAMPLING=1`: la importancia se mide con todos los CONTINUA.
- **Fijo, igual que en todos los brazos:**
  - HT de WF9600.
  - Final train con 3 semillas y 20% de los CONTINUA.
  - Sin canaritos.
  - Semilla 100213.
- **Dataset final:** 1509 originales + 500 fórmulas × 6 = **4517 columnas**.

### Ganancia en 202107, semilla 100213

| | E0 | E5 | E6 | E10 | **E11** |
|---|---|---|---|---|---|
| Meseta | 584,7 | 588,6 | 584,8 | 587,8 | **606,2** |
| Máximo | 594,3 | 597,5 | 597,5 | 596,4 | **619,2** |

Para dar escala al ruido: entre las 12 semillas, E5 va de 578,3 a 593,3
(media 586,9). La diferencia E6 − E5 varió unos ±10 M según la semilla.

### Curva del evolutivo (meseta en millones)

| Gen | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Fitness (202103) | 350,0 | 343,9 | 342,5 | 347,7 | 343,7 | 344,9 | 351,4 | 342,8 | 350,5 | 342,6 | 346,3 |
| Prueba (202105) | 238,2 | 243,9 | 248,4 | 242,2 | 238,0 | 238,9 | 237,2 | 241,2 | 247,3 | 237,9 | 243,0 |

Igual que en E10, **la selección no mejora el fitness**: la gen 10 queda por
debajo de la gen 0. La ganancia de E11 no sale de que la población se vuelva
mejor generación a generación. Sale de que el modelo final ve, además de las
originales, 500 fórmulas **con su historia**. La importancia de las generadas
va del 16% al 29% del total según la generación (`evolutivo_generaciones.txt`).

### Costo

| | |
|---|---|
| Máquina | GCP n2-custom, 12 vCPU, 864 GB (`n2-custom-12-884736-ext`), disco de 600 GB |
| RAM | pico ~476 GB en el evolutivo; 399 GB al retomar después de la gen 10 |
| Tiempo | evolutivo: ~12 h de cómputo (suma de las 10 generaciones, en varios tramos por los cortes). Del checkpoint al final: 259 min, de los que el final train de 3 semillas se lleva 210 |
| Disco | `dataset_canaritos.fst` pesa ~61 GB. gcsfuse arma el archivo entero en su `--temp-dir`, que está en el disco local, antes de subirlo |

Problemas que aparecieron, para no repetirlos:

- **Con 512 GB de RAM se cae por OOM** (rc 137) en las generaciones 3 y 8.
  Hace falta más RAM o `EV_UNDERSAMPLING` < 1.
- **Con 58 GB libres de disco falla al grabar el checkpoint** de canaritos:
  `write_fst: There was an error creating the file`. Los dos reintentos fallaron igual.
  Conviene dejar ≥ 150 GB libres. El caché de gcsfuse en `/mnt/gcsfuse` va acumulando
  datasets de corridas viejas y se puede vaciar con el bucket desmontado.
- **En AWS Spot** (r6a.16xlarge, 512 GB) hubo ~10 interrupciones en 6 h y nunca
  terminó la gen 1. Para corridas de muchas horas sin checkpoint intermedio,
  usar on-demand.

### Cómo reproducirlo

En una VM con el bucket montado como `ds` y el repo copiado en
`/home/ds/buckets/b1/wf635v6`:

```bash
cd /tmp
nohup setsid bash -c 'sudo -u ds -H env BRAZOS=E11 SEMILLAS=100213 REINTENTOS=2 bash /home/ds/buckets/b1/wf635v6/brazos/correr_brazos.sh; sudo poweroff' > ~/e11.log 2>&1 < /dev/null &
```

- **Resultados:** quedan en `exp/WF9931_s100213/`:
  - `ganancia_meseta.txt`, `ganancia_maximo.txt` y `ganancia_202107.txt`;
  - `evolutivo_*.txt`, `prediccion.txt` y `registro.txt`.
- **Tiempo y RAM:** en `exp/brazos_v3/tiempos.tsv` y `cola.log`.
- **Si se corta:** relanzar el mismo comando. Retoma desde
  `checkpoints/evolutivo.rds`, que guarda la última generación terminada, o desde el
  `.fst` de la última etapa.

## Laboratorio de final train (`lab/`)

Pregunta: con el FE de E5 fijo, ¿qué final train conviene? Se probó:

- **Undersampling:** cuántos CONTINUA usar.
- **Bagging de negativos por iteración:** `bag` = todos los CONTINUA +
  `bagging_freq=1`, `pos_bagging_fraction=1` y `neg_bagging_fraction=0.2`.
- **Más árboles y `feature_fraction`.**
- **Semillerios de distinto tamaño**, armados en `lab_analizar.R`.

`lab_ft.R` entrena muchos modelos por configuración, cada uno con su
semilla, y graba la probabilidad de cada cliente. `lab_analizar.R` arma
semillerios de cualquier tamaño (k = 1, 2, 5, 10, …) sorteando modelos, y
mezclas de a dos configuraciones por promedio de rangos.

```bash
# 202107 (entrena 201901-202105)
sudo -u ds -H env LAB_CONFIGS="base:8,bag:8,bagff:8,baglargo:6" LAB_DESDE=0 bash lab/correr_lab.sh
# 202105 como validacion (entrena 201901-202103): misma idea, otro mes
sudo -u ds -H env LAB_FUTURO=202105 LAB_CONFIGS="base:8,bag:8,bagff:8,baglargo:6" LAB_DESDE=100 bash lab/correr_lab.sh
# analisis: una subcarpeta por host bajo exp/lab/
Rscript lab/lab_analizar.R /home/ds/buckets/b1/exp/lab
```

`LAB_DESDE` corre los modelos de `desde+1` en adelante. Así dos hosts no
repiten semillas y los resultados se pueden juntar. `correr_lab.sh` genera
primero el dataset de E5 (`brazos/L0.env`, WF9940) si no existe.

### 202107: modelos sueltos (meseta promedio por modelo)

| base | bag | bagff | baglargo |
|---|---|---|---|
| 586,4 (n=15) | 589,7 (n=14) | 592,7 (n=12) | 592,1 (n=8) |

Semillerio completo en 202107: bagff p1023 605,0 (18 modelos), baglargo p1535
605,6 (12) y bag p1023 602,6 (24).

### 202105: la validación que decide

Cuatro hosts (val-a..d, r6a.8xlarge Spot en AWS), 16 a 32 modelos por config:

| Config | Modelos | k=1 | k=10 | Semillerio completo |
|---|---|---|---|---|
| **base p1023** | 16 | 257,7 | **261,0** | **260,3** |
| bag p512 | 32 | 250,8 | 257,4 | 258,2 |
| bag p1023 | 32 | 250,3 | 256,0 | 256,4 |
| baglargo p1023 | 24 | 249,6 | 255,8 | 255,8 |
| bagff p1023 | 32 | 250,1 | 253,9 | 254,8 |

- **Ruido:** el desvío entre sorteos con k=10 es ~1 M. La ventaja de la base sobre
  bag/bagff en 202105 (3 a 6 M) es más grande que ese ruido.
- **Mezcla base + bag:** 260,7. Empata con la base sola.
- **Conclusión:** lo que bag ganaba en 202107 no se repite en 202105.
  **Mantenemos el final train de la base** (us02, 1023 iteraciones). Si se quiere
  un hedge, la mezcla base + bag no pierde.

## Qué falta para darlo por bueno

- **E11 con más semillas.** La diferencia E11 − E5 = +17,7 es muy grande para el
  ruido de una semilla (±10). Pero sin al menos 6 semillas no hay test: con
  Wilcoxon, el p más chico posible con n semillas es 2/2^n. Costo: ~5–15 h por
  semilla en una máquina de ≥ 512 GB.
- **E11 validado en 202105**, como hicimos con el final train.
- **E13 (E11 sin piso de canaritos).** Separa cuánto aporta la historia de las
  fórmulas y cuánto el piso. `E12` es el control al azar de E11.
- Los brazos E10–E13 con 10–12 semillas están definidos en `brazos/` y
  `correr_brazos.sh` los corre en cola: `BRAZOS="E11 E13 E12"`.
