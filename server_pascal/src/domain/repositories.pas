unit Repositories;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Capa de Dominio - Interfaces de Repositorio
  
  Define los contratos (interfaces) para acceso a datos.
  No contiene implementaciones concretas (eso va en Infrastructure).
  Esto permite que el dominio no dependa de detalles técnicos.
  ============================================================================ }

interface

uses
  Classes, SysUtils, Entities;

type
  { ============================================================================
    IRegistroRepository - Contrato para operaciones CRUD de registros
    ============================================================================ }
  
  IRegistroRepository = interface
    ['{A1B2C3D4-E5F6-4789-A1B2-C3D4E5F67890}']
    
    { Guardar un registro en la base de datos }
    function GuardarRegistro(const ARegistro: TRegistro): Boolean;
    
    { Guardar múltiples registros en una transacción }
    function GuardarRegistros(const ARegistros: TRegistroArray): Boolean;
    
    { Obtener todos los registros }
    function ObtenerTodos: TRegistroArray;
    
    { Obtener registros por ID de estación }
    function ObtenerPorEstacion(AEstacionId: Byte): TRegistroArray;
    
    { Obtener cantidad total de registros }
    function ContarRegistros: Integer;
    
    { Obtener cantidad de registros por estación }
    function ContarPorEstacion(AEstacionId: Byte): Integer;
    
    { Eliminar todos los registros (útil para testing) }
    function LimpiarTodo: Boolean;
    
    { Verificar si existe al menos un registro para una estación }
    function ExisteEstacion(AEstacionId: Byte): Boolean;
  end;
  
  { ============================================================================
    IEstacionRepository - Contrato para operaciones con estaciones
    ============================================================================ }
  
  IEstacionRepository = interface
    ['{B2C3D4E5-F6A7-4890-B2C3-D4E5F6A78901}']
    
    { Guardar o actualizar una estación }
    function GuardarEstacion(AEstacion: TEstacion): Boolean;
    
    { Obtener una estación por ID }
    function ObtenerEstacion(AId: Integer): TEstacion;
    
    { Obtener todas las estaciones }
    function ObtenerTodasEstaciones: TEstacionArray;
    
    { Eliminar una estación }
    function EliminarEstacion(AId: Integer): Boolean;
    
    { Verificar si existe una estación }
    function ExisteEstacion(AId: Integer): Boolean;
    
    { Obtener cantidad de estaciones }
    function ContarEstaciones: Integer;
  end;
  
  { ============================================================================
    IDatabaseConnection - Contrato para conexión a base de datos
    ============================================================================ }
  
  IDatabaseConnection = interface
    ['{C3D4E5F6-A7B8-4901-C3D4-E5F6A7B89012}']
    
    { Conectar a la base de datos }
    function Conectar: Boolean;
    
    { Desconectar de la base de datos }
    procedure Desconectar;
    
    { Verificar si está conectado }
    function EstaConectado: Boolean;
    
    { Iniciar transacción }
    function IniciarTransaccion: Boolean;
    
    { Confirmar transacción }
    function CommitTransaccion: Boolean;
    
    { Revertir transacción }
    function RollbackTransaccion: Boolean;
    
    { Obtener mensaje de último error }
    function ObtenerUltimoError: string;
    
    { Crear tablas si no existen }
    function CrearTablas: Boolean;
  end;
  
  { ============================================================================
    TRepositoryError - Excepción personalizada para errores de repositorio
    ============================================================================ }
  
  TRepositoryError = class(Exception);
  
  { ============================================================================
    TDatabaseConfig - Configuración de conexión a base de datos
    Record simple sin lógica (Value Object del dominio)
    ============================================================================ }
  
  TDatabaseConfig = record
    TipoDB: string;        // 'sqlite' o 'mariadb'
    Servidor: string;      // localhost o IP (para MariaDB)
    Puerto: Integer;       // Puerto (3306 para MariaDB)
    NombreDB: string;      // Nombre de la base de datos
    Usuario: string;       // Usuario (para MariaDB)
    Contrasena: string;    // Contraseña (para MariaDB)
    RutaArchivo: string;   // Ruta del archivo (para SQLite)
  end;

{ ============================================================================
  Funciones auxiliares para configuración
  ============================================================================ }

{ Crear configuración para SQLite }
function CrearConfigSQLite(const ARutaArchivo: string): TDatabaseConfig;

{ Crear configuración para MariaDB }
function CrearConfigMariaDB(const AServidor, ANombreDB, AUsuario, AContrasena: string; 
                            APuerto: Integer = 3306): TDatabaseConfig;

{ Validar configuración }
function ValidarConfig(const AConfig: TDatabaseConfig): Boolean;

{ Obtener descripción de la configuración }
function ConfigToString(const AConfig: TDatabaseConfig): string;

implementation

{ ============================================================================
  Implementación de funciones auxiliares
  ============================================================================ }

function CrearConfigSQLite(const ARutaArchivo: string): TDatabaseConfig;
begin
  FillChar(Result, SizeOf(TDatabaseConfig), 0);
  Result.TipoDB := 'sqlite';
  Result.RutaArchivo := ARutaArchivo;
  Result.NombreDB := ExtractFileName(ARutaArchivo);
  Result.Servidor := '';
  Result.Puerto := 0;
  Result.Usuario := '';
  Result.Contrasena := '';
end;

function CrearConfigMariaDB(const AServidor, ANombreDB, AUsuario, AContrasena: string; 
                            APuerto: Integer = 3306): TDatabaseConfig;
begin
  FillChar(Result, SizeOf(TDatabaseConfig), 0);
  Result.TipoDB := 'mariadb';
  Result.Servidor := AServidor;
  Result.Puerto := APuerto;
  Result.NombreDB := ANombreDB;
  Result.Usuario := AUsuario;
  Result.Contrasena := AContrasena;
  Result.RutaArchivo := '';
end;

function ValidarConfig(const AConfig: TDatabaseConfig): Boolean;
begin
  Result := True;
  
  // Validar tipo de base de datos
  if not ((AConfig.TipoDB = 'sqlite') or (AConfig.TipoDB = 'mariadb')) then
  begin
    Result := False;
    Exit;
  end;
  
  // Validar según tipo
  if AConfig.TipoDB = 'sqlite' then
  begin
    // SQLite requiere ruta de archivo
    if AConfig.RutaArchivo = '' then
      Result := False;
  end
  else if AConfig.TipoDB = 'mariadb' then
  begin
    // MariaDB requiere servidor, usuario y nombre de BD
    if (AConfig.Servidor = '') or (AConfig.Usuario = '') or 
       (AConfig.NombreDB = '') or (AConfig.Puerto <= 0) then
      Result := False;
  end;
end;

function ConfigToString(const AConfig: TDatabaseConfig): string;
begin
  if AConfig.TipoDB = 'sqlite' then
    Result := Format('SQLite: %s', [AConfig.RutaArchivo])
  else if AConfig.TipoDB = 'mariadb' then
    Result := Format('MariaDB: %s:%d/%s (usuario: %s)', 
                     [AConfig.Servidor, AConfig.Puerto, 
                      AConfig.NombreDB, AConfig.Usuario])
  else
    Result := 'Configuración inválida';
end;

end.
