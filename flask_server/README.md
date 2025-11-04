Carpeta `flask_server` - aquí se implementará el servidor Flask en Python.
Archivos a crear:
- app.py           -> servidor Flask que reciba POST y almacene en SQLite
- requirements.txt -> dependencias (Flask, matplotlib, sqlite3 es estándar)
- plots/           -> ubicación para PNG generados

Notas:
- El endpoint recibirá JSON y guardará en SQLite; generará una figura con subplots para las series solicitadas y exportará PNG.
