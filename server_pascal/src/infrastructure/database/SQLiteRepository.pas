unit SQLiteRepository;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Capa de Infraestructura - Repositorio SQLite
  
  Implementación concreta del repositorio usando SQLite.
  Usa componentes SQLDB de Free Pascal.
  ============================================================================ }

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn, DB,
  Entities, Repositories;

type
  { ============================================================================
    TSQLiteRegistroRepository - Implementación de IRegistroRepository para SQLite
    ============================================================================ }
  
  TSQLiteRegistroRepository = class(TInterfacedObject, IRegistroRepository)
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
    FOwnsConnection: Boolean;
    
    { Crear la tabla si no existe }
    procedure CrearTablaRegistros;
    
    { Convertir DataSet a TRegistro }
    function DataSetToRegistro(ADataSet: TDataSet): TRegistro;
    
  public
    constructor Create(AConnection: TSQLite3Connection; AOwnsConnection: Boolean = False);
    destructor Destroy; override;
    
    { Implementación de IRegistroRepository }
    function GuardarRegistro(const ARegistro: TRegistro): Boolean;
    function GuardarRegistros(const ARegistros: TRegistroArray): Boolean;
    function ObtenerTodos: TRegistroArray;
    function ObtenerPorEstacion(AEstacionId: Byte): TRegistroArray;
    function ContarRegistros: Integer;
    function ContarPorEstacion(AEstacionId: Byte): Integer;
    function LimpiarTodo: Boolean;
    function ExisteEstacion(AEstacionId: Byte): Boolean;
  end;
  
  { ============================================================================
    TSQLiteConnection - Gestión de conexión SQLite
    ============================================================================ }
  
  TSQLiteDatabaseConnection = class(TInterfacedObject, IDatabaseConnection)
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FConfig: TDatabaseConfig;
    FUltimoError: string;
    
  public
    constructor Create(const AConfig: TDatabaseConfig);
    destructor Destroy; override;
    
    { Implementación de IDatabaseConnection }
    function Conectar: Boolean;
    procedure Desconectar;
    function EstaConectado: Boolean;
    function IniciarTransaccion: Boolean;
    function CommitTransaccion: Boolean;
    function RollbackTransaccion: Boolean;
    function ObtenerUltimoError: string;
    function CrearTablas: Boolean;
    
    { Obtener la conexión subyacente }
    property Connection: TSQLite3Connection read FConnection;
  end;

implementation

{ ============================================================================
  Implementación de TSQLiteRegistroRepository
  ============================================================================ }

constructor TSQLiteRegistroRepository.Create(AConnection: TSQLite3Connection; 
                                             AOwnsConnection: Boolean = False);
begin
  inherited Create;
  FConnection := AConnection;
  FOwnsConnection := AOwnsConnection;
  
  // Crear transacción si no existe
  if FConnection.Transaction = nil then
  begin
    FTransaction := TSQLTransaction.Create(nil);
    FTransaction.DataBase := FConnection;
    FConnection.Transaction := FTransaction;
  end
  else
    FTransaction := FConnection.Transaction as TSQLTransaction;
  
  // Crear query
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;
  
  // Crear tabla si no existe
  CrearTablaRegistros;
end;

destructor TSQLiteRegistroRepository.Destroy;
begin
  FQuery.Free;
  if FOwnsConnection then
  begin
    if Assigned(FTransaction) then
      FTransaction.Free;
    if Assigned(FConnection) then
      FConnection.Free;
  end;
  inherited Destroy;
end;

procedure TSQLiteRegistroRepository.CrearTablaRegistros;
const
  SQL_CREATE_TABLE = 
    'CREATE TABLE IF NOT EXISTS estaciones (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  estacion_id INTEGER NOT NULL,' +
    '  temperatura INTEGER NOT NULL,' +
    '  humedad INTEGER NOT NULL,' +
    '  mp01 INTEGER NOT NULL,' +
    '  mp25 INTEGER NOT NULL,' +
    '  mp10 INTEGER NOT NULL,' +
    '  h01 INTEGER NOT NULL,' +
    '  h25 INTEGER NOT NULL,' +
    '  h50 INTEGER NOT NULL,' +
    '  h10 INTEGER NOT NULL,' +
    '  fecha_registro TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
    ')';
    
  SQL_CREATE_INDEX =
    'CREATE INDEX IF NOT EXISTS idx_estacion_id ON estaciones(estacion_id)';
begin
  try
    FQuery.SQL.Text := SQL_CREATE_TABLE;
    FQuery.ExecSQL;
    FTransaction.Commit;
    
    FQuery.SQL.Text := SQL_CREATE_INDEX;
    FQuery.ExecSQL;
    FTransaction.Commit;
  except
    on E: Exception do
      raise TRepositoryError.CreateFmt('Error creando tabla: %s', [E.Message]);
  end;
end;

function TSQLiteRegistroRepository.DataSetToRegistro(ADataSet: TDataSet): TRegistro;
begin
  Result.id := ADataSet.FieldByName('estacion_id').AsInteger;
  Result.te := ADataSet.FieldByName('temperatura').AsInteger;
  Result.hr := ADataSet.FieldByName('humedad').AsInteger;
  Result.mp01 := ADataSet.FieldByName('mp01').AsInteger;
  Result.mp25 := ADataSet.FieldByName('mp25').AsInteger;
  Result.mp10 := ADataSet.FieldByName('mp10').AsInteger;
  Result.h01 := ADataSet.FieldByName('h01').AsInteger;
  Result.h25 := ADataSet.FieldByName('h25').AsInteger;
  Result.h50 := ADataSet.FieldByName('h50').AsInteger;
  Result.h10 := ADataSet.FieldByName('h10').AsInteger;
end;

function TSQLiteRegistroRepository.GuardarRegistro(const ARegistro: TRegistro): Boolean;
const
  SQL_INSERT = 
    'INSERT INTO estaciones (estacion_id, temperatura, humedad, ' +
    'mp01, mp25, mp10, h01, h25, h50, h10) ' +
    'VALUES (:estacion_id, :temperatura, :humedad, ' +
    ':mp01, :mp25, :mp10, :h01, :h25, :h50, :h10)';
begin
  Result := False;
  try
    FQuery.SQL.Text := SQL_INSERT;
    FQuery.ParamByName('estacion_id').AsInteger := ARegistro.id;
    FQuery.ParamByName('temperatura').AsInteger := ARegistro.te;
    FQuery.ParamByName('humedad').AsInteger := ARegistro.hr;
    FQuery.ParamByName('mp01').AsInteger := ARegistro.mp01;
    FQuery.ParamByName('mp25').AsInteger := ARegistro.mp25;
    FQuery.ParamByName('mp10').AsInteger := ARegistro.mp10;
    FQuery.ParamByName('h01').AsInteger := ARegistro.h01;
    FQuery.ParamByName('h25').AsInteger := ARegistro.h25;
    FQuery.ParamByName('h50').AsInteger := ARegistro.h50;
    FQuery.ParamByName('h10').AsInteger := ARegistro.h10;
    
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise TRepositoryError.CreateFmt('Error guardando registro: %s', [E.Message]);
    end;
  end;
end;

function TSQLiteRegistroRepository.GuardarRegistros(const ARegistros: TRegistroArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  try
    FTransaction.StartTransaction;
    
    for I := Low(ARegistros) to High(ARegistros) do
    begin
      // Usar el mismo INSERT pero sin commit individual
      FQuery.SQL.Text := 
        'INSERT INTO estaciones (estacion_id, temperatura, humedad, ' +
        'mp01, mp25, mp10, h01, h25, h50, h10) ' +
        'VALUES (:estacion_id, :temperatura, :humedad, ' +
        ':mp01, :mp25, :mp10, :h01, :h25, :h50, :h10)';
        
      FQuery.ParamByName('estacion_id').AsInteger := ARegistros[I].id;
      FQuery.ParamByName('temperatura').AsInteger := ARegistros[I].te;
      FQuery.ParamByName('humedad').AsInteger := ARegistros[I].hr;
      FQuery.ParamByName('mp01').AsInteger := ARegistros[I].mp01;
      FQuery.ParamByName('mp25').AsInteger := ARegistros[I].mp25;
      FQuery.ParamByName('mp10').AsInteger := ARegistros[I].mp10;
      FQuery.ParamByName('h01').AsInteger := ARegistros[I].h01;
      FQuery.ParamByName('h25').AsInteger := ARegistros[I].h25;
      FQuery.ParamByName('h50').AsInteger := ARegistros[I].h50;
      FQuery.ParamByName('h10').AsInteger := ARegistros[I].h10;
      
      FQuery.ExecSQL;
    end;
    
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise TRepositoryError.CreateFmt('Error guardando registros: %s', [E.Message]);
    end;
  end;
end;

function TSQLiteRegistroRepository.ObtenerTodos: TRegistroArray;
var
  Lista: TRegistroArray;
  Count: Integer;
begin
  SetLength(Lista, 0);
  Count := 0;
  
  try
    FQuery.SQL.Text := 'SELECT * FROM estaciones ORDER BY fecha_registro';
    FQuery.Open;
    
    while not FQuery.EOF do
    begin
      SetLength(Lista, Count + 1);
      Lista[Count] := DataSetToRegistro(FQuery);
      Inc(Count);
      FQuery.Next;
    end;
    
    FQuery.Close;
    Result := Lista;
  except
    on E: Exception do
      raise TRepositoryError.CreateFmt('Error obteniendo registros: %s', [E.Message]);
  end;
end;

function TSQLiteRegistroRepository.ObtenerPorEstacion(AEstacionId: Byte): TRegistroArray;
var
  Lista: TRegistroArray;
  Count: Integer;
begin
  SetLength(Lista, 0);
  Count := 0;
  
  try
    FQuery.SQL.Text := 
      'SELECT * FROM estaciones WHERE estacion_id = :estacion_id ORDER BY fecha_registro';
    FQuery.ParamByName('estacion_id').AsInteger := AEstacionId;
    FQuery.Open;
    
    while not FQuery.EOF do
    begin
      SetLength(Lista, Count + 1);
      Lista[Count] := DataSetToRegistro(FQuery);
      Inc(Count);
      FQuery.Next;
    end;
    
    FQuery.Close;
    Result := Lista;
  except
    on E: Exception do
      raise TRepositoryError.CreateFmt('Error obteniendo registros por estación: %s', [E.Message]);
  end;
end;

function TSQLiteRegistroRepository.ContarRegistros: Integer;
begin
  Result := 0;
  try
    FQuery.SQL.Text := 'SELECT COUNT(*) as total FROM estaciones';
    FQuery.Open;
    Result := FQuery.FieldByName('total').AsInteger;
    FQuery.Close;
  except
    on E: Exception do
      raise TRepositoryError.CreateFmt('Error contando registros: %s', [E.Message]);
  end;
end;

function TSQLiteRegistroRepository.ContarPorEstacion(AEstacionId: Byte): Integer;
begin
  Result := 0;
  try
    FQuery.SQL.Text := 
      'SELECT COUNT(*) as total FROM estaciones WHERE estacion_id = :estacion_id';
    FQuery.ParamByName('estacion_id').AsInteger := AEstacionId;
    FQuery.Open;
    Result := FQuery.FieldByName('total').AsInteger;
    FQuery.Close;
  except
    on E: Exception do
      raise TRepositoryError.CreateFmt('Error contando registros por estación: %s', [E.Message]);
  end;
end;

function TSQLiteRegistroRepository.LimpiarTodo: Boolean;
begin
  Result := False;
  try
    FQuery.SQL.Text := 'DELETE FROM estaciones';
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      FTransaction.Rollback;
      raise TRepositoryError.CreateFmt('Error limpiando registros: %s', [E.Message]);
    end;
  end;
end;

function TSQLiteRegistroRepository.ExisteEstacion(AEstacionId: Byte): Boolean;
begin
  Result := ContarPorEstacion(AEstacionId) > 0;
end;

{ ============================================================================
  Implementación de TSQLiteDatabaseConnection
  ============================================================================ }

constructor TSQLiteDatabaseConnection.Create(const AConfig: TDatabaseConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FUltimoError := '';
  
  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
  FConnection.Transaction := FTransaction;
end;

destructor TSQLiteDatabaseConnection.Destroy;
begin
  Desconectar;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

function TSQLiteDatabaseConnection.Conectar: Boolean;
begin
  Result := False;
  try
    if FConfig.TipoDB <> 'sqlite' then
    begin
      FUltimoError := 'Configuración no es para SQLite';
      Exit;
    end;
    
    FConnection.DatabaseName := FConfig.RutaArchivo;
    FConnection.Open;
    Result := FConnection.Connected;
    
    if Result then
      FUltimoError := ''
    else
      FUltimoError := 'No se pudo conectar a SQLite';
  except
    on E: Exception do
    begin
      FUltimoError := E.Message;
      Result := False;
    end;
  end;
end;

procedure TSQLiteDatabaseConnection.Desconectar;
begin
  try
    if FConnection.Connected then
      FConnection.Close;
  except
    on E: Exception do
      FUltimoError := E.Message;
  end;
end;

function TSQLiteDatabaseConnection.EstaConectado: Boolean;
begin
  Result := FConnection.Connected;
end;

function TSQLiteDatabaseConnection.IniciarTransaccion: Boolean;
begin
  Result := False;
  try
    FTransaction.StartTransaction;
    Result := True;
  except
    on E: Exception do
      FUltimoError := E.Message;
  end;
end;

function TSQLiteDatabaseConnection.CommitTransaccion: Boolean;
begin
  Result := False;
  try
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      FUltimoError := E.Message;
  end;
end;

function TSQLiteDatabaseConnection.RollbackTransaccion: Boolean;
begin
  Result := False;
  try
    FTransaction.Rollback;
    Result := True;
  except
    on E: Exception do
      FUltimoError := E.Message;
  end;
end;

function TSQLiteDatabaseConnection.ObtenerUltimoError: string;
begin
  Result := FUltimoError;
end;

function TSQLiteDatabaseConnection.CrearTablas: Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    Query.Transaction := FTransaction;
    
    // Crear tabla estaciones
    Query.SQL.Text := 
      'CREATE TABLE IF NOT EXISTS estaciones (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  estacion_id INTEGER NOT NULL,' +
      '  temperatura INTEGER NOT NULL,' +
      '  humedad INTEGER NOT NULL,' +
      '  mp01 INTEGER NOT NULL,' +
      '  mp25 INTEGER NOT NULL,' +
      '  mp10 INTEGER NOT NULL,' +
      '  h01 INTEGER NOT NULL,' +
      '  h25 INTEGER NOT NULL,' +
      '  h50 INTEGER NOT NULL,' +
      '  h10 INTEGER NOT NULL,' +
      '  fecha_registro TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
      ')';
    Query.ExecSQL;
    FTransaction.Commit;
    
    // Crear índice
    Query.SQL.Text := 
      'CREATE INDEX IF NOT EXISTS idx_estacion_id ON estaciones(estacion_id)';
    Query.ExecSQL;
    FTransaction.Commit;
    
    Result := True;
  except
    on E: Exception do
    begin
      FUltimoError := E.Message;
      FTransaction.Rollback;
    end;
  end;
  Query.Free;
end;

end.
