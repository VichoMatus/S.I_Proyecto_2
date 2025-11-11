unit Entities;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Capa de Dominio - Entidades de Negocio
  
  Define las entidades core del sistema sin dependencias externas.
  Esta es la capa más interna de Clean Architecture.
  ============================================================================ }

interface

uses
  SysUtils, Classes;

type
  { TRegistro - Entidad que representa un registro de datos ambientales
    Estructura idéntica al formato binario generado por el ESP32
    Tamaño total: 17 bytes (1+1+1+2+2+2+2+2+2+2) }
  
  TRegistro = packed record
    id   : Byte;     // ID de estación (1-10)
    te   : Byte;     // Temperatura en °C (15-35)
    hr   : Byte;     // Humedad relativa en % (30-80)
    mp01 : Word;     // Material Particulado 0.1 µm
    mp25 : Word;     // Material Particulado 2.5 µm
    mp10 : Word;     // Material Particulado 10 µm
    h01  : Word;     // Histograma de partículas > 0.1 µm
    h25  : Word;     // Histograma de partículas > 2.5 µm
    h50  : Word;     // Histograma de partículas > 5.0 µm
    h10  : Word;     // Histograma de partículas > 10 µm
  end;
  
  { Puntero a TRegistro }
  PRegistro = ^TRegistro;
  
  { Array dinámico de registros }
  TRegistroArray = array of TRegistro;
  
  { TEstacion - Entidad que representa una estación ambiental
    Contiene metadata y colección de registros }
  
  TEstacion = class
  private
    FId: Integer;
    FNombre: string;
    FUbicacion: string;
    FActiva: Boolean;
    FRegistros: TRegistroArray;
    function GetCantidadRegistros: Integer;
  public
    constructor Create(AId: Integer; ANombre: string);
    destructor Destroy; override;
    
    { Agregar un registro a la estación }
    procedure AgregarRegistro(const ARegistro: TRegistro);
    
    { Obtener registro por índice }
    function ObtenerRegistro(AIndex: Integer): TRegistro;
    
    { Limpiar todos los registros }
    procedure LimpiarRegistros;
    
    { Propiedades }
    property Id: Integer read FId write FId;
    property Nombre: string read FNombre write FNombre;
    property Ubicacion: string read FUbicacion write FUbicacion;
    property Activa: Boolean read FActiva write FActiva;
    property CantidadRegistros: Integer read GetCantidadRegistros;
    property Registros: TRegistroArray read FRegistros;
  end;
  
  { TEstacionArray - Array de estaciones }
  TEstacionArray = array of TEstacion;

{ ============================================================================
  Funciones auxiliares para trabajar con TRegistro
  ============================================================================ }

{ Crear un registro vacío/inicializado }
function CrearRegistroVacio: TRegistro;

{ Crear un registro con valores específicos }
function CrearRegistro(AId, ATe, AHr: Byte; 
                       AMp01, AMp25, AMp10: Word;
                       AH01, AH25, AH50, AH10: Word): TRegistro;

{ Validar que un registro tenga valores dentro de rangos esperados }
function ValidarRegistro(const ARegistro: TRegistro): Boolean;

{ Obtener representación en texto del registro }
function RegistroToString(const ARegistro: TRegistro): string;

{ Convertir registro a formato JSON (string) }
function RegistroToJSON(const ARegistro: TRegistro): string;

{ Parsear registro desde JSON (string) }
function JSONToRegistro(const AJSON: string): TRegistro;

implementation

uses
  StrUtils;

{ ============================================================================
  Implementación de TEstacion
  ============================================================================ }

constructor TEstacion.Create(AId: Integer; ANombre: string);
begin
  inherited Create;
  FId := AId;
  FNombre := ANombre;
  FUbicacion := '';
  FActiva := True;
  SetLength(FRegistros, 0);
end;

destructor TEstacion.Destroy;
begin
  SetLength(FRegistros, 0);
  inherited Destroy;
end;

function TEstacion.GetCantidadRegistros: Integer;
begin
  Result := Length(FRegistros);
end;

procedure TEstacion.AgregarRegistro(const ARegistro: TRegistro);
var
  Len: Integer;
begin
  Len := Length(FRegistros);
  SetLength(FRegistros, Len + 1);
  FRegistros[Len] := ARegistro;
end;

function TEstacion.ObtenerRegistro(AIndex: Integer): TRegistro;
begin
  if (AIndex >= 0) and (AIndex < Length(FRegistros)) then
    Result := FRegistros[AIndex]
  else
    raise Exception.CreateFmt('Índice fuera de rango: %d', [AIndex]);
end;

procedure TEstacion.LimpiarRegistros;
begin
  SetLength(FRegistros, 0);
end;

{ ============================================================================
  Implementación de funciones auxiliares
  ============================================================================ }

function CrearRegistroVacio: TRegistro;
begin
  FillChar(Result, SizeOf(TRegistro), 0);
end;

function CrearRegistro(AId, ATe, AHr: Byte; 
                       AMp01, AMp25, AMp10: Word;
                       AH01, AH25, AH50, AH10: Word): TRegistro;
begin
  Result.id := AId;
  Result.te := ATe;
  Result.hr := AHr;
  Result.mp01 := AMp01;
  Result.mp25 := AMp25;
  Result.mp10 := AMp10;
  Result.h01 := AH01;
  Result.h25 := AH25;
  Result.h50 := AH50;
  Result.h10 := AH10;
end;

function ValidarRegistro(const ARegistro: TRegistro): Boolean;
begin
  Result := True;
  
  // Validar ID de estación (1-10, aunque podría ser más flexible)
  if (ARegistro.id < 1) or (ARegistro.id > 255) then
    Result := False;
  
  // Validar temperatura (rango típico: 0-50°C, pero aceptamos 0-100)
  if (ARegistro.te > 100) then
    Result := False;
  
  // Validar humedad (0-100%)
  if (ARegistro.hr > 100) then
    Result := False;
  
  // Material particulado y histogramas: aceptamos cualquier valor Word válido
  // No hay validaciones específicas ya que dependen del ambiente
end;

function RegistroToString(const ARegistro: TRegistro): string;
begin
  Result := Format('Estación %d: Temp=%d°C, HR=%d%%, ' +
                   'PM0.1=%d, PM2.5=%d, PM10=%d, ' +
                   'Hist=[%d, %d, %d, %d]',
                   [ARegistro.id, ARegistro.te, ARegistro.hr,
                    ARegistro.mp01, ARegistro.mp25, ARegistro.mp10,
                    ARegistro.h01, ARegistro.h25, ARegistro.h50, ARegistro.h10]);
end;

function RegistroToJSON(const ARegistro: TRegistro): string;
begin
  Result := Format('{"id":%d,"te":%d,"hr":%d,' +
                   '"mp01":%d,"mp25":%d,"mp10":%d,' +
                   '"h01":%d,"h25":%d,"h50":%d,"h10":%d}',
                   [ARegistro.id, ARegistro.te, ARegistro.hr,
                    ARegistro.mp01, ARegistro.mp25, ARegistro.mp10,
                    ARegistro.h01, ARegistro.h25, ARegistro.h50, ARegistro.h10]);
end;

function JSONToRegistro(const AJSON: string): TRegistro;
var
  S: string;
  
  function ExtraerValor(const Campo: string): Integer;
  var
    P1, P2: Integer;
    Valor: string;
  begin
    P1 := Pos('"' + Campo + '":', S);
    if P1 = 0 then
      raise Exception.CreateFmt('Campo no encontrado: %s', [Campo]);
    
    P1 := P1 + Length(Campo) + 3; // Saltar "campo":
    P2 := P1;
    
    // Buscar el siguiente , o }
    while (P2 <= Length(S)) and (S[P2] <> ',') and (S[P2] <> '}') do
      Inc(P2);
    
    Valor := Copy(S, P1, P2 - P1);
    Valor := Trim(Valor);
    Result := StrToIntDef(Valor, 0);
  end;
  
begin
  S := AJSON;
  Result := CrearRegistroVacio;
  
  try
    Result.id := ExtraerValor('id');
    Result.te := ExtraerValor('te');
    Result.hr := ExtraerValor('hr');
    Result.mp01 := ExtraerValor('mp01');
    Result.mp25 := ExtraerValor('mp25');
    Result.mp10 := ExtraerValor('mp10');
    Result.h01 := ExtraerValor('h01');
    Result.h25 := ExtraerValor('h25');
    Result.h50 := ExtraerValor('h50');
    Result.h10 := ExtraerValor('h10');
  except
    on E: Exception do
      raise Exception.CreateFmt('Error parseando JSON: %s', [E.Message]);
  end;
end;

end.
