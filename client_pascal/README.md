Carpeta `client_pascal` - aquí se implementará el HTTP Client en Lazarus Pascal (modo consola).
Archivos a crear:
- client_main.pas  -> programa principal del cliente que lea `data/data.dat` y haga POST (JSON y stream).

Notas:
- Enviar primero JSON (Content-Type: application/json) y luego el stream como application/octet-stream.
