# Proyecto #2 - Sistemas Inteligentes (INFO1157)
**DataLogger IoT - Sistema Cliente-Servidor**

## 📋 Descripción del Proyecto

Sistema de recolección y visualización de datos ambientales donde un ESP32 con sensor PMS5003 genera un archivo binario `data.dat` con registros de 10 estaciones ambientales. El sistema incluye:

- **Cliente HTTP (Lazarus Pascal)** - Modo consola/terminal
- **Servidor HTTP (Lazarus Pascal)** - Modo consola/terminal  
- **Servidor Flask (Python)** - Procesamiento, almacenamiento y visualización

### Flujo de Datos
```
ESP32 + PMS5003 → data.dat → HTTP Client (Pascal) → HTTP Server (Pascal/Flask) → SQLite/MariaDB → Gráficas PNG
```

---

## 🏗️ Arquitectura del Proyecto (Clean Architecture)

```
S.I_Proyecto_2/
├── client_pascal/          # Cliente HTTP (Capa de Presentación)
│   ├── src/
│   │   ├── domain/        # Entidades y casos de uso
│   │   ├── infrastructure/ # HTTP client, file reader
│   │   └── presentation/  # CLI interface
│   └── client_main.pas
│
├── server_pascal/          # Servidor HTTP Pascal (Capa de Aplicación)
│   ├── src/
│   │   ├── domain/        # Entidades de negocio
│   │   ├── application/   # Casos de uso
│   │   ├── infrastructure/ # DB, HTTP server
│   │   └── presentation/  # Endpoints
│   └── server_main.pas
│
├── flask_server/           # Servidor Flask (Capa de Servicios)
│   ├── src/
│   │   ├── domain/        # Modelos de datos
│   │   ├── application/   # Lógica de negocio
│   │   ├── infrastructure/ # DB, gráficas
│   │   └── presentation/  # Routes/Controllers
│   ├── plots/             # Gráficas generadas
│   ├── app.py
│   └── requirements.txt
│
├── data/                   # Datos binarios (Capa de Datos)
│   └── data.dat
│
└── scripts/                # Utilidades
    └── generate_data.py
```

---

## ✅ PLAN DE DESARROLLO - TAREAS ORDENADAS

### **FASE 1: Preparación del Entorno y Datos** ⚙️

#### ✅ Tarea 1.1: Configurar estructura de directorios
**Objetivo:** Crear la estructura de carpetas siguiendo Clean Architecture

**Directorios a crear:**
```
client_pascal/src/{domain,infrastructure,presentation}/
server_pascal/src/{domain,application,infrastructure,presentation}/
flask_server/src/{domain,application,infrastructure,presentation}/
flask_server/plots/
```

**Entregable:** Estructura de carpetas completa

---

#### ✅ Tarea 1.2: Crear generador de datos de prueba
**Objetivo:** Script Python que genere `data.dat` binario con estructura Pascal Record

**Archivo:** `scripts/generate_data.py`

**Estructura del Record (Pascal):**
```pascal
TRegistro = Record
  id : Byte;              // ID de estación (1-10)
  te : Byte;              // Temperatura (°C)
  hr : Byte;              // Humedad del aire (%)
  mp01: Word;             // Material Particulado 0.1 um
  mp25: Word;             // Material Particulado 2.5 um
  mp10: Word;             // Material Particulado 10 um
  h01, h25, h50, h10: Word; // Histograma de partículas
end;
```

**Requisitos:**
- Generar 100 registros (10 estaciones × 10 muestras cada una)
- Valores realistas: te (15-35°C), hr (30-80%), mp (valores típicos)
- Formato binario compatible con Pascal (little-endian)
- Script debe aceptar parámetro `--count` para cantidad de registros

**Entregable:** `scripts/generate_data.py` funcional + `data/data.dat` generado

---

### **FASE 2: Servidor HTTP Pascal (Core del Sistema)** 🖥️

#### ✅ Tarea 2.1: Implementar capa de dominio del servidor Pascal
**Objetivo:** Definir entidades y contratos

**Archivos a crear:**
```
server_pascal/src/domain/entities.pas     - TRegistro, TEstacion
server_pascal/src/domain/repositories.pas - Interfaces de DB
```

**Requisitos:**
- Definir `TRegistro` idéntico al generador
- Interfaces para repositorio de datos
- Sin dependencias externas (dominio puro)

**Entregable:** Unidades Pascal de dominio

---

#### ✅ Tarea 2.2: Implementar infraestructura de base de datos
**Objetivo:** Conexión y operaciones con SQLite/MariaDB

**Archivos a crear:**
```
server_pascal/src/infrastructure/database/sqlite_repository.pas
server_pascal/src/infrastructure/database/mariadb_repository.pas
server_pascal/src/infrastructure/database/db_config.pas
```

**Requisitos:**
- Usar componentes `SQLDB` de Free Pascal
- Tabla `estaciones` con todos los campos del registro
- Operaciones: Insert, Select
- Manejo de errores y transacciones
- Soporte para SQLite (prioritario) y MariaDB (opcional)

**Entregable:** Repositorios de datos funcionando

---

#### ✅ Tarea 2.3: Implementar servidor HTTP
**Objetivo:** Servidor HTTP en modo consola que reciba POST

**Archivos a crear:**
```
server_pascal/src/infrastructure/http/http_server.pas
server_pascal/src/presentation/controllers/data_controller.pas
server_pascal/src/application/use_cases/receive_data_usecase.pas
```

**Requisitos:**
- Usar `fphttpserver` (FCL-Web) o librería compatible
- **Endpoint 1:** `POST /api/data/json` - Recibe JSON
- **Endpoint 2:** `POST /api/data/stream` - Recibe stream binario
- Parsear JSON y stream a TRegistro
- Invocar caso de uso para guardar en DB
- Responder con status apropiados (200, 400, 500)
- Modo consola (sin GUI)

**Entregable:** Servidor HTTP funcional en modo consola

---

#### ✅ Tarea 2.4: Integrar y probar servidor Pascal completo
**Objetivo:** Programa principal que inicialice servidor

**Archivo:** `server_pascal/server_main.pas`

**Requisitos:**
- Inicializar base de datos
- Configurar servidor HTTP en puerto 8080
- Logging en consola de requests recibidos
- Manejo de errores y shutdown graceful

**Entregable:** Servidor completo ejecutándose

---

### **FASE 3: Cliente HTTP Pascal** 📤

#### ✅ Tarea 3.1: Implementar capa de dominio del cliente
**Objetivo:** Entidades y casos de uso del cliente

**Archivos a crear:**
```
client_pascal/src/domain/entities.pas       - TRegistro
client_pascal/src/domain/data_sender.pas    - Interface para envío
```

**Entregable:** Entidades del cliente

---

#### ✅ Tarea 3.2: Implementar lectura de archivo binario
**Objetivo:** Leer `data.dat` y convertir a array de TRegistro

**Archivo:** `client_pascal/src/infrastructure/file_reader.pas`

**Requisitos:**
- Abrir archivo binario
- Leer registros usando BlockRead
- Validar estructura
- Retornar lista de registros

**Entregable:** Módulo de lectura funcional

---

#### ✅ Tarea 3.3: Implementar cliente HTTP con envío JSON
**Objetivo:** Enviar datos como JSON al servidor

**Archivo:** `client_pascal/src/infrastructure/http_client.pas`

**Requisitos:**
- Usar `fphttpclient` de FPC
- Convertir TRegistro[] a JSON
- POST a servidor con `Content-Type: application/json`
- Primero enviar a servidor Pascal (puerto 8080)
- Luego enviar a servidor Flask (puerto 5000)
- Manejo de errores de conexión

**Entregable:** Cliente enviando JSON exitosamente

---

#### ✅ Tarea 3.4: Implementar envío como Stream binario
**Objetivo:** Enviar datos como TMemoryStream

**Requisitos:**
- Convertir registros a stream binario
- POST con `Content-Type: application/octet-stream`
- Enviar primero a Pascal, luego a Flask

**Entregable:** Cliente enviando stream exitosamente

---

#### ✅ Tarea 3.5: Programa principal del cliente
**Objetivo:** CLI que ejecute el flujo completo

**Archivo:** `client_pascal/client_main.pas`

**Requisitos:**
- Leer `data.dat`
- Enviar JSON a servidor Pascal
- Enviar Stream a servidor Pascal
- Enviar JSON a servidor Flask
- Enviar Stream a servidor Flask
- Logging en consola de cada paso
- Modo consola (sin GUI)

**Entregable:** Cliente completo funcional

---

### **FASE 4: Servidor Flask con Visualización** 📊

#### ✅ Tarea 4.1: Implementar capa de dominio Flask
**Objetivo:** Modelos de datos

**Archivo:** `flask_server/src/domain/models.py`

**Requisitos:**
- Clase `Registro` con todos los campos
- Clase `Estacion` con metadata
- Validaciones de datos

**Entregable:** Modelos de dominio

---

#### ✅ Tarea 4.2: Implementar repositorio SQLite
**Objetivo:** Persistencia de datos en SQLite

**Archivo:** `flask_server/src/infrastructure/database/sqlite_repository.py`

**Requisitos:**
- Tabla `estaciones` (misma estructura que Pascal)
- Métodos: insert, get_all, get_by_id
- Uso de SQLAlchemy o sqlite3 puro
- Tabla `metadata` para tracking

**Entregable:** Repositorio SQLite funcional

---

#### ✅ Tarea 4.3: Implementar generador de gráficas
**Objetivo:** Crear visualizaciones con Matplotlib/Pyplot

**Archivo:** `flask_server/src/infrastructure/visualization/plot_generator.py`

**Requisitos:**
- Usar `matplotlib.pyplot.subplots(6, 1, ...)`
- **Gráfica 1-5:** Series temporales de `te`, `hr`, `mp01`, `mp25`, `mp10`
  - Cada una con su propio subplot
  - Label, unidades de medida, título y grilla ON
  - Media móvil (window=10) superpuesta
  - Leyenda para serie original y promedio móvil
- **Gráfica 6:** Histograma de `h01`, `h25`, `h50`, `h10`
  - Barras en el mismo subplot con colores distintos
  - Leyenda con unidades
- Guardar en `flask_server/plots/` como PNG
- Nombre de archivo: `estacion_{id}_{timestamp}.png`

**Entregable:** Generador de gráficas funcional

---

#### ✅ Tarea 4.4: Implementar casos de uso de aplicación
**Objetivo:** Lógica de negocio del servidor Flask

**Archivos a crear:**
```
flask_server/src/application/use_cases/receive_json_usecase.py
flask_server/src/application/use_cases/receive_stream_usecase.py
flask_server/src/application/use_cases/generate_plots_usecase.py
```

**Requisitos:**
- Parsear JSON/Stream a modelo de dominio
- Guardar en SQLite y MariaDB
- Generar gráficas automáticamente
- Retornar metadata (archivos generados)

**Entregable:** Casos de uso implementados

---

#### ✅ Tarea 4.5: Implementar endpoints Flask
**Objetivo:** Rutas HTTP del servidor

**Archivo:** `flask_server/src/presentation/routes/data_routes.py`

**Requisitos:**
- `POST /api/data/json` - Recibe JSON
- `POST /api/data/stream` - Recibe stream binario
- `GET /api/plots/<id>` - Retorna PNG de gráfica
- Validación de Content-Type
- Respuestas apropiadas (200, 400, 500)

**Entregable:** Endpoints funcionando

---

#### ✅ Tarea 4.6: Programa principal Flask
**Objetivo:** Aplicación Flask completa

**Archivo:** `flask_server/app.py`

**Requisitos:**
- Inicializar Flask
- Registrar blueprints
- Configurar CORS si es necesario
- Ejecutar en puerto 5000
- Logging de requests

**Entregable:** Servidor Flask completo

---

### **FASE 5: Integración y Pruebas** 🧪

#### ✅ Tarea 5.1: Prueba end-to-end completa
**Objetivo:** Verificar flujo completo

**Pasos:**
1. Generar `data.dat` con script
2. Iniciar servidor Pascal
3. Iniciar servidor Flask
4. Ejecutar cliente Pascal
5. Verificar datos en SQLite/MariaDB
6. Verificar generación de gráficas PNG
7. Validar que gráficas cumplen especificaciones

**Entregable:** Sistema funcionando end-to-end

---

#### ✅ Tarea 5.2: Validación contra rúbrica
**Objetivo:** Asegurar cumplimiento de requisitos

**Checklist:**
- [ ] Cliente HTTP envía datos JSON correctamente
- [ ] Cliente HTTP envía datos Stream correctamente
- [ ] Servidor Pascal recibe y almacena en SQLite
- [ ] Servidor Pascal recibe y almacena en MariaDB (opcional)
- [ ] Servidor Flask recibe y almacena en SQLite
- [ ] Servidor Flask recibe y almacena en MariaDB (opcional)
- [ ] Flask genera 6 gráficas con formato correcto
- [ ] Gráficas incluyen labels, títulos, unidades, grilla
- [ ] Media móvil de 10 muestras calculada correctamente
- [ ] Histograma con 4 series y leyenda
- [ ] Todo en modo consola (sin GUI)
- [ ] Código Lazarus Pascal bien estructurado

**Entregable:** Validación completa

---

## 🛠️ Dependencias y Configuración

### Windows (PowerShell)

#### Free Pascal / Lazarus
```powershell
# Descargar e instalar desde: https://www.lazarus-ide.org/
# O usar Chocolatey:
choco install lazarus
```

#### Python y dependencias
```powershell
# Python 3.10+
python --version

# Crear entorno virtual
python -m venv .venv
.\.venv\Scripts\Activate.ps1

# Instalar dependencias
cd flask_server
pip install -r requirements.txt
```

#### SQLite (incluido en Python)
```powershell
# Verificar
python -c "import sqlite3; print(sqlite3.version)"
```

#### MariaDB (opcional)
```powershell
# Descargar e instalar desde: https://mariadb.org/download/
# Configurar servicio y crear BD
```

---

## 🚀 Comandos de Ejecución

### 1. Generar datos de prueba
```powershell
python scripts/generate_data.py --count 100
```

### 2. Iniciar servidor Pascal
```powershell
cd server_pascal
fpc server_main.pas
.\server_main.exe
```

### 3. Iniciar servidor Flask
```powershell
cd flask_server
.\.venv\Scripts\Activate.ps1
python app.py
```

### 4. Ejecutar cliente Pascal
```powershell
cd client_pascal
fpc client_main.pas
.\client_main.exe
```

---

## 📊 Especificaciones de Gráficas

### Requisitos visuales (6 subplots):
1. **te** (Temperatura) - Línea con promedio móvil
2. **hr** (Humedad) - Línea con promedio móvil
3. **mp01** (PM 0.1) - Línea con promedio móvil
4. **mp25** (PM 2.5) - Línea con promedio móvil
5. **mp10** (PM 10) - Línea con promedio móvil
6. **Histograma** - Barras de h01, h25, h50, h10

Cada gráfica debe tener:
- ✅ Label descriptivo
- ✅ Unidades de medida
- ✅ Título claro
- ✅ Grilla activada (grid ON)
- ✅ Leyenda

---

## 📝 Notas Importantes

1. **NO crear GUI** - Todo en modo consola/terminal
2. **Clean Architecture** - Separación clara de capas
3. **Sin archivos de test** - Solo implementación
4. **Código comentado** - Facilitar comprensión
5. **Manejo de errores** - Try-catch en puntos críticos

---

## 🎯 Estado Actual

**INICIO DEL PROYECTO**
- [ ] Fase 1 completa
- [ ] Fase 2 completa
- [ ] Fase 3 completa
- [ ] Fase 4 completa
- [ ] Fase 5 completa

---

**Fecha de inicio:** 11 de noviembre de 2025  
**Autor:** Alberto Caro  
**Curso:** INFO1157 - Sistemas Inteligentes