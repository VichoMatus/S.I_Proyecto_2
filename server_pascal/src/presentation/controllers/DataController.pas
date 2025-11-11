unit DataController;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Capa de Presentación - Controlador de Datos
  
  Coordina las peticiones HTTP con los casos de uso.
  ============================================================================ }

interface

uses
  Classes, SysUtils,
  Entities, ReceiveDataUseCase;

type
  { ============================================================================
    TDataController - Controlador para endpoints de datos
    ============================================================================ }
  
  TDataController = class
  private
    FUseCase: TReceiveDataUseCase;
    
  public
    constructor Create(AUseCase: TReceiveDataUseCase);
    destructor Destroy; override;
    
    { Procesar petición POST con JSON }
    function ProcessJSONRequest(const AJSONContent: string; 
                               out AResponseCode: Integer;
                               out AResponseMessage: string): Boolean;
    
    { Procesar petición POST con Stream }
    function ProcessStreamRequest(AStream: TStream;
                                 out AResponseCode: Integer;
                                 out AResponseMessage: string): Boolean;
    
    { Obtener estadísticas }
    function GetStatistics(out AResponseCode: Integer;
                          out AResponseJSON: string): Boolean;
  end;

implementation

uses
  fpjson, jsonparser;

{ ============================================================================
  Implementación de TDataController
  ============================================================================ }

constructor TDataController.Create(AUseCase: TReceiveDataUseCase);
begin
  inherited Create;
  FUseCase := AUseCase;
end;

destructor TDataController.Destroy;
begin
  inherited Destroy;
end;

function TDataController.ProcessJSONRequest(const AJSONContent: string; 
                                           out AResponseCode: Integer;
                                           out AResponseMessage: string): Boolean;
var
  Registro: TRegistro;
  Registros: TRegistroArray;
  JSONData: TJSONData;
  JSONObj: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  ErrorMsg: string;
begin
  Result := False;
  AResponseCode := 500;
  AResponseMessage := 'Error interno';
  SetLength(Registros, 0);
  ErrorMsg := '';
  
  try
    // Parsear JSON
    JSONData := GetJSON(AJSONContent);
    try
      // Verificar si es array o objeto
      if JSONData is TJSONArray then
      begin
        // Múltiples registros
        JSONArray := TJSONArray(JSONData);
        SetLength(Registros, JSONArray.Count);
        
        for I := 0 to JSONArray.Count - 1 do
        begin
          JSONObj := JSONArray.Objects[I];
          Registros[I] := JSONToRegistro(JSONObj.AsJSON);
        end;
        
        // Guardar batch
        if FUseCase.ExecuteBatch(Registros) then
        begin
          AResponseCode := 200;
          AResponseMessage := Format('Procesados %d registros exitosamente', [Length(Registros)]);
          Result := True;
        end
        else
        begin
          AResponseCode := 500;
          AResponseMessage := 'Error guardando registros';
        end;
      end
      else if JSONData is TJSONObject then
      begin
        // Un solo registro
        Registro := JSONToRegistro(AJSONContent);
        
        if FUseCase.ExecuteWithValidation(Registro, ErrorMsg) then
        begin
          AResponseCode := 200;
          AResponseMessage := 'Registro procesado exitosamente';
          Result := True;
        end
        else
        begin
          AResponseCode := 400;
          AResponseMessage := ErrorMsg;
        end;
      end
      else
      begin
        AResponseCode := 400;
        AResponseMessage := 'Formato JSON inválido';
      end;
    finally
      JSONData.Free;
    end;
  except
    on E: Exception do
    begin
      AResponseCode := 400;
      AResponseMessage := 'Error parseando JSON: ' + E.Message;
    end;
  end;
end;

function TDataController.ProcessStreamRequest(AStream: TStream;
                                             out AResponseCode: Integer;
                                             out AResponseMessage: string): Boolean;
var
  Registros: TRegistroArray;
  Count, I: Integer;
  Registro: TRegistro;
begin
  Result := False;
  AResponseCode := 500;
  AResponseMessage := 'Error interno';
  SetLength(Registros, 0);
  FillChar(Registro, SizeOf(TRegistro), 0);
  
  try
    if AStream = nil then
    begin
      AResponseCode := 400;
      AResponseMessage := 'Stream vacío';
      Exit;
    end;
    
    AStream.Position := 0;
    Count := AStream.Size div SizeOf(TRegistro);
    
    if Count = 0 then
    begin
      AResponseCode := 400;
      AResponseMessage := 'Stream no contiene registros válidos';
      Exit;
    end;
    
    SetLength(Registros, Count);
    
    for I := 0 to Count - 1 do
    begin
      AStream.Read(Registro, SizeOf(TRegistro));
      Registros[I] := Registro;
    end;
    
    // Guardar batch
    if FUseCase.ExecuteBatch(Registros) then
    begin
      AResponseCode := 200;
      AResponseMessage := Format('Procesados %d registros desde stream', [Count]);
      Result := True;
    end
    else
    begin
      AResponseCode := 500;
      AResponseMessage := 'Error guardando registros desde stream';
    end;
  except
    on E: Exception do
    begin
      AResponseCode := 500;
      AResponseMessage := 'Error procesando stream: ' + E.Message;
    end;
  end;
end;

function TDataController.GetStatistics(out AResponseCode: Integer;
                                      out AResponseJSON: string): Boolean;
begin
  Result := False;
  AResponseCode := 500;
  
  try
    // Por ahora solo retornar status OK
    AResponseCode := 200;
    AResponseJSON := '{"status":"ok","message":"Controller activo"}';
    Result := True;
  except
    on E: Exception do
    begin
      AResponseCode := 500;
      AResponseJSON := Format('{"error":"%s"}', [E.Message]);
    end;
  end;
end;

end.
