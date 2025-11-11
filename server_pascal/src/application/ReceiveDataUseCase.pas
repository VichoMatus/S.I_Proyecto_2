unit ReceiveDataUseCase;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Capa de Aplicación - Caso de Uso: Recibir Datos
  
  Implementa la lógica de negocio para recibir y procesar datos.
  ============================================================================ }

interface

uses
  Classes, SysUtils,
  Entities, Repositories;

type
  { ============================================================================
    TReceiveDataUseCase - Caso de uso para recibir y guardar datos
    ============================================================================ }
  
  TReceiveDataUseCase = class
  private
    FRepository: IRegistroRepository;
    FOnLog: TNotifyEvent;
    FLogMessage: string;
    
    procedure Log(const AMessage: string);
    
  public
    constructor Create(ARepository: IRegistroRepository);
    
    { Ejecutar caso de uso: guardar un registro }
    function Execute(const ARegistro: TRegistro): Boolean;
    
    { Ejecutar caso de uso: guardar múltiples registros }
    function ExecuteBatch(const ARegistros: TRegistroArray): Boolean;
    
    { Validar y ejecutar }
    function ExecuteWithValidation(const ARegistro: TRegistro; 
                                   out AError: string): Boolean;
    
    { Obtener último mensaje de log }
    property LastLogMessage: string read FLogMessage;
    
    { Evento de logging }
    property OnLog: TNotifyEvent read FOnLog write FOnLog;
  end;

implementation

{ ============================================================================
  Implementación de TReceiveDataUseCase
  ============================================================================ }

constructor TReceiveDataUseCase.Create(ARepository: IRegistroRepository);
begin
  inherited Create;
  FRepository := ARepository;
  FOnLog := nil;
  FLogMessage := '';
end;

procedure TReceiveDataUseCase.Log(const AMessage: string);
begin
  FLogMessage := AMessage;
  WriteLn('[UseCase] ', AMessage);
  if Assigned(FOnLog) then
    FOnLog(Self);
end;

function TReceiveDataUseCase.Execute(const ARegistro: TRegistro): Boolean;
begin
  Result := False;
  try
    Log(Format('Guardando registro de estación %d', [ARegistro.id]));
    Result := FRepository.GuardarRegistro(ARegistro);
    
    if Result then
      Log('Registro guardado exitosamente')
    else
      Log('Error: no se pudo guardar el registro');
  except
    on E: Exception do
    begin
      Log(Format('Excepción al guardar registro: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TReceiveDataUseCase.ExecuteBatch(const ARegistros: TRegistroArray): Boolean;
begin
  Result := False;
  try
    Log(Format('Guardando lote de %d registros', [Length(ARegistros)]));
    Result := FRepository.GuardarRegistros(ARegistros);
    
    if Result then
      Log(Format('Lote guardado exitosamente (%d registros)', [Length(ARegistros)]))
    else
      Log('Error: no se pudo guardar el lote');
  except
    on E: Exception do
    begin
      Log(Format('Excepción al guardar lote: %s', [E.Message]));
      Result := False;
    end;
  end;
end;

function TReceiveDataUseCase.ExecuteWithValidation(const ARegistro: TRegistro; 
                                                   out AError: string): Boolean;
begin
  Result := False;
  AError := '';
  
  // Validar el registro
  if not ValidarRegistro(ARegistro) then
  begin
    AError := 'Registro contiene datos inválidos';
    Log(Format('Validación fallida para estación %d: %s', [ARegistro.id, AError]));
    Exit;
  end;
  
  // Ejecutar
  Result := Execute(ARegistro);
  
  if not Result then
    AError := 'Error al guardar en la base de datos';
end;

end.
