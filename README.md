q# S.I_Proyecto_2

Este repositorio contiene la estructura inicial para un proyecto IoT/DataLogger donde un
ESP32 generó un archivo `data.dat` con registros de 10 estaciones ambientales. El objetivo
es desarrollar clientes y servidores en Lazarus Pascal (modo consola) y un servidor Flask
en Python para procesar, almacenar y graficar los datos.

Este README explica los pasos a seguir, el orden recomendado para crear archivos y las
dependencias para un entorno Arch Linux.

## Estructura del proyecto (actual)
- `server_pascal/`  — servidor HTTP en Pascal (consola)
- `client_pascal/`  — cliente HTTP en Pascal (consola)
- `flask_server/`   — servidor Flask (Python) para recibir datos, guardar en SQLite y generar PNG
- `data/`           — contiene `data.dat` (registro Pascal, actualmente en formato textual)
- `scripts/`        — scripts auxiliares (generadores de datos, utilidades)

## Flujo general (alto nivel)
1. El cliente Pascal lee `data/data.dat` y envía los datos por HTTP POST:
   - Primero como JSON (Content-Type: application/json).
   - Luego como Stream de bytes (Content-Type: application/octet-stream).
2. El servidor (Pascal o Flask) recibe los datos y los guarda en SQLite y opcionalmente en MariaDB.
3. El servidor Flask genera gráficas con matplotlib y guarda PNG en `flask_server/plots/`.

## Orden recomendado: archivos a crear primero
1. `scripts/generate_data.py` — script Python para generar un `data/data.dat` de prueba (binario)
2. `client_pascal/client_main.pas` — cliente consola: lectura de `data.dat`, construcción de JSON y envío POST (JSON + stream)
3. `server_pascal/server_main.pas` — servidor consola: endpoints para recibir JSON y stream e insertar en SQLite
4. `server_pascal/db_helper.pas` — utilidades de acceso a SQLite/MariaDB desde Pascal
5. `flask_server/app.py` — servidor Flask mínimo para recibir POST, guardar en SQLite y generar gráficas PNG
6. `flask_server/plots/` — directorio donde Flask guardará los PNG generados

## Arch Linux — dependencias e instalación

Sistema (paquetes principales):

```bash
# Actualizar sistema y paquetes
sudo pacman -Syu

# Instalar Free Pascal y Lazarus (Lazarus opcional si usarás IDE)
sudo pacman -S fpc lazarus

# SQLite y MariaDB server (si vas a usar MariaDB localmente)
sudo pacman -S sqlite mariadb

# Python3 y pip
sudo pacman -S python python-pip

# (opcional) utilidades
sudo pacman -S git xxd
```

Librerías/paquetes Python (usar un virtualenv recomendado):

```bash
# crear y activar un entorno virtual (recomendado)
python -m venv .venv
source .venv/bin/activate

# instalar dependencias del servidor Flask
pip install --upgrade pip
pip install Flask matplotlib numpy pandas sqlalchemy pymysql
```

Notas sobre MariaDB: si usas MariaDB, inicia el servicio y asegúralo:

```bash
sudo systemctl start mariadb
sudo mysql_secure_installation
```

Para conectarte desde Python a MariaDB puedes usar `pymysql` o `mysql-connector-python`.

## Recomendaciones y notas para Lazarus/Free Pascal
- Para hacer peticiones HTTP desde Free Pascal en consola puedes usar las unidades disponibles
  `fphttpclient` (FPC) para cliente. Para servidor HTTP en modo consola puedes usar `fphttpserver`
  (FCL-Web) o una librería ligera como Synapse/Indy según prefieras.
- Para acceso a SQLite desde Pascal usa los componentes de `SQLDB` con `SQLite3Conn` (incluidos
  en Lazarus/Free Pascal) o SQLite3 dinámico.
- Para MariaDB desde Pascal puedes usar ODBC o bibliotecas específicas; al principio es recomendable
  prototipar con SQLite porque es más simple de desplegar.

## Comandos rápidos de prueba

1) Generar datos de prueba (si creas `scripts/generate_data.py`, ejecuta):

```bash
python scripts/generate_data.py --count 100
```

2) Instalar dependencias Python (si no usas venv):

```bash
pip install -r flask_server/requirements.txt
```

3) Correr el servidor Flask (desde `flask_server/`):

```bash
source .venv/bin/activate   # si usaste venv
cd flask_server
python app.py
```

4) Compilar cliente Pascal (ejemplo con fpc):

```bash
cd client_pascal
fpc client_main.pas -oclient_main
./client_main
```

## Diseño de la gráfica (requisitos)
- El servidor Flask debe crear una figura con `fig, axs = plt.subplots(6, 1, ... )` (6 filas):
  - Filas 1-5: cada una para `te`, `hr`, `mp01`, `mp25`, `mp10`. Cada serie con label, unidad, título y
    grilla activada. Además cada serie mostrará una media móvil (window) de 10 muestras.
  - Fila 6: histograma de `h01`, `h25`, `h50` y `h10` (cada uno en el mismo subplot con leyenda).

Para la media móvil: puedes usar `pandas.Series.rolling(window=10).mean()` o una función de numpy.

## Siguientes tareas inmediatas (priorizadas)
1. Crear `scripts/generate_data.py` para generar `data/data.dat` binario de prueba (N registros).
2. Implementar `flask_server/app.py` mínimo que reciba JSON y guarde en SQLite.
3. Implementar `client_pascal/client_main.pas` que lea `data.dat` de prueba y haga POST JSON al Flask local.

Si quieres, comienzo ahora creando `scripts/generate_data.py` que genere N registros binarios y un ejemplo
de `app.py` mínimo en Flask para recibir JSON e insertar en SQLite. Indica cuántos registros quieres
en el `data.dat` de prueba (por ejemplo: 100).

---
Fecha: 4 de noviembre de 2025
# S.I_Proyecto_2
