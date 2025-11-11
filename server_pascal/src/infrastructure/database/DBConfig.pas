unit DBConfig;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Capa de Infraestructura - Configuración de Base de Datos
  
  Utilidades para crear y gestionar configuraciones de base de datos.
  ============================================================================ }

interface

uses
  Classes, SysUtils, IniFiles,
  Repositories;

{ ============================================================================
  Funciones de configuración predeterminada
  ============================================================================ }

{ Obtener configuración predeterminada de SQLite }
function ObtenerConfigDefaultSQLite: TDatabaseConfig;

{ Obtener configuración predeterminada de MariaDB }
function ObtenerConfigDefaultMariaDB: TDatabaseConfig;

{ ============================================================================
  Funciones para cargar/guardar configuración desde archivos
  ============================================================================ }

{ Cargar configuración desde archivo INI }
function CargarConfigDesdeINI(const ARutaINI: string): TDatabaseConfig;

{ Guardar configuración en archivo INI }
function GuardarConfigEnINI(const AConfig: TDatabaseConfig; const ARutaINI: string): Boolean;

{ ============================================================================
  Funciones de validación y utilidad
  ============================================================================ }

{ Verificar si existe el archivo de base de datos SQLite }
function ExisteArchivoBD(const ARuta: string): Boolean;

{ Crear directorio para base de datos si no existe }
function CrearDirectorioBD(const ARuta: string): Boolean;

{ Obtener ruta absoluta de base de datos relativa al ejecutable }
function ObtenerRutaBDRelativa(const ANombreArchivo: string): string;

implementation

{ ============================================================================
  Implementación de funciones de configuración predeterminada
  ============================================================================ }

function ObtenerConfigDefaultSQLite: TDatabaseConfig;
var
  RutaBD: string;
begin
  // Base de datos en el directorio 'data' relativo al ejecutable
  RutaBD := ObtenerRutaBDRelativa('estaciones.db');
  Result := CrearConfigSQLite(RutaBD);
end;

function ObtenerConfigDefaultMariaDB: TDatabaseConfig;
begin
  Result := CrearConfigMariaDB(
    'localhost',           // Servidor
    'estaciones_db',       // Nombre BD
    'root',                // Usuario
    '',                    // Contraseña (vacía por defecto)
    3306                   // Puerto
  );
end;

{ ============================================================================
  Implementación de funciones de carga/guardado
  ============================================================================ }

function CargarConfigDesdeINI(const ARutaINI: string): TDatabaseConfig;
var
  INI: TIniFile;
  TipoDB: string;
begin
  FillChar(Result, SizeOf(TDatabaseConfig), 0);
  TipoDB := '';
  
  if not FileExists(ARutaINI) then
  begin
    WriteLn('Archivo INI no encontrado: ', ARutaINI);
    WriteLn('Usando configuración predeterminada (SQLite)');
    Result := ObtenerConfigDefaultSQLite;
    Exit;
  end;
  
  INI := TIniFile.Create(ARutaINI);
  try
    try
      TipoDB := LowerCase(INI.ReadString('Database', 'Type', 'sqlite'));
      
      if TipoDB = 'sqlite' then
      begin
        Result.TipoDB := 'sqlite';
        Result.RutaArchivo := INI.ReadString('SQLite', 'FilePath', 
                                             ObtenerRutaBDRelativa('estaciones.db'));
        Result.NombreDB := ExtractFileName(Result.RutaArchivo);
      end
      else if TipoDB = 'mariadb' then
      begin
        Result.TipoDB := 'mariadb';
        Result.Servidor := INI.ReadString('MariaDB', 'Server', 'localhost');
        Result.Puerto := INI.ReadInteger('MariaDB', 'Port', 3306);
        Result.NombreDB := INI.ReadString('MariaDB', 'Database', 'estaciones_db');
        Result.Usuario := INI.ReadString('MariaDB', 'Username', 'root');
        Result.Contrasena := INI.ReadString('MariaDB', 'Password', '');
      end
      else
      begin
        WriteLn('Tipo de BD desconocido: ', TipoDB);
        Result := ObtenerConfigDefaultSQLite;
      end;
    except
      on E: Exception do
      begin
        WriteLn('Error leyendo INI: ', E.Message);
        Result := ObtenerConfigDefaultSQLite;
      end;
    end;
  finally
    INI.Free;
  end;
end;

function GuardarConfigEnINI(const AConfig: TDatabaseConfig; const ARutaINI: string): Boolean;
var
  INI: TIniFile;
begin
  Result := False;
  try
    INI := TIniFile.Create(ARutaINI);
    try
      INI.WriteString('Database', 'Type', AConfig.TipoDB);
      
      if AConfig.TipoDB = 'sqlite' then
      begin
        INI.WriteString('SQLite', 'FilePath', AConfig.RutaArchivo);
      end
      else if AConfig.TipoDB = 'mariadb' then
      begin
        INI.WriteString('MariaDB', 'Server', AConfig.Servidor);
        INI.WriteInteger('MariaDB', 'Port', AConfig.Puerto);
        INI.WriteString('MariaDB', 'Database', AConfig.NombreDB);
        INI.WriteString('MariaDB', 'Username', AConfig.Usuario);
        INI.WriteString('MariaDB', 'Password', AConfig.Contrasena);
      end;
      
      Result := True;
    finally
      INI.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error guardando configuración: ', E.Message);
  end;
end;

{ ============================================================================
  Implementación de funciones de validación y utilidad
  ============================================================================ }

function ExisteArchivoBD(const ARuta: string): Boolean;
begin
  Result := FileExists(ARuta);
end;

function CrearDirectorioBD(const ARuta: string): Boolean;
var
  Dir: string;
begin
  Result := False;
  Dir := ExtractFileDir(ARuta);
  
  if Dir = '' then
    Exit;
  
  try
    if not DirectoryExists(Dir) then
      ForceDirectories(Dir);
    Result := DirectoryExists(Dir);
  except
    on E: Exception do
      WriteLn('Error creando directorio: ', E.Message);
  end;
end;

function ObtenerRutaBDRelativa(const ANombreArchivo: string): string;
var
  RutaExe, RutaData: string;
begin
  // Obtener directorio del ejecutable
  RutaExe := ExtractFilePath(ParamStr(0));
  
  // Crear ruta al directorio 'data'
  RutaData := IncludeTrailingPathDelimiter(RutaExe) + 'data';
  
  // Si no existe, intentar con ruta relativa ..\..\data
  if not DirectoryExists(RutaData) then
    RutaData := IncludeTrailingPathDelimiter(RutaExe) + '..' + PathDelim + '..' + PathDelim + 'data';
  
  // Crear directorio si no existe
  if not DirectoryExists(RutaData) then
    ForceDirectories(RutaData);
  
  // Retornar ruta completa al archivo
  Result := IncludeTrailingPathDelimiter(RutaData) + ANombreArchivo;
end;

end.
