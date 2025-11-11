unit HTTPServerUnit;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Capa de Infraestructura - Servidor HTTP
  
  Implementa un servidor HTTP usando fphttpserver de Free Pascal.
  Servidor simple para modo consola sin GUI.
  ============================================================================ }

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, httproute,
  Entities, Repositories, ReceiveDataUseCase;

type
  { ============================================================================
    TDataHTTPServer - Servidor HTTP para recibir datos
    ============================================================================ }
  
  TDataHTTPServer = class
  private
    FServer: TFPHTTPServer;
    FPort: Word;
    FRepository: IRegistroRepository;
    FUseCase: TReceiveDataUseCase;
    FRunning: Boolean;
    
    { Manejadores de rutas }
    procedure HandlePostJSON(ARequest: TRequest; AResponse: TResponse);
    procedure HandlePostStream(ARequest: TRequest; AResponse: TResponse);
    procedure HandleGetStatus(ARequest: TRequest; AResponse: TResponse);
    procedure HandleNotFound(ARequest: TRequest; AResponse: TResponse);
    
    { Utilidades }
    function ParseJSONToRegistro(const AJSON: string): TRegistro;
    function ParseStreamToRegistros(AStream: TStream): TRegistroArray;
    procedure SendJSONResponse(AResponse: TResponse; AStatusCode: Integer; 
                              const AMessage: string);
    procedure LogRequest(ARequest: TRequest);
    
  public
    constructor Create(ARepository: IRegistroRepository; APort: Word = 8080);
    destructor Destroy; override;
    
    { Iniciar servidor }
    procedure Start;
    
    { Detener servidor }
    procedure Stop;
    
    { Verificar si está corriendo }
    function IsRunning: Boolean;
    
    property Port: Word read FPort;
  end;

implementation

uses
  fpjson, jsonparser, StrUtils;

{ ============================================================================
  Implementación de TDataHTTPServer
  ============================================================================ }

constructor TDataHTTPServer.Create(ARepository: IRegistroRepository; APort: Word = 8080);
begin
  inherited Create;
  FRepository := ARepository;
  FPort := APort;
  FRunning := False;
  
  // Crear caso de uso
  FUseCase := TReceiveDataUseCase.Create(FRepository);
  
  // Crear servidor HTTP
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := FPort;
  FServer.Threaded := True;
  
  // Registrar rutas usando HTTPRouter
  HTTPRouter.RegisterRoute('/api/data/json', rmPost, @HandlePostJSON);
  HTTPRouter.RegisterRoute('/api/data/stream', rmPost, @HandlePostStream);
  HTTPRouter.RegisterRoute('/api/status', rmGet, @HandleGetStatus);
  HTTPRouter.RegisterRoute('/', rmAll, @HandleNotFound);
end;

destructor TDataHTTPServer.Destroy;
begin
  Stop;
  FUseCase.Free;
  FServer.Free;
  inherited Destroy;
end;

procedure TDataHTTPServer.Start;
begin
  if FRunning then
  begin
    WriteLn('[Server] El servidor ya está corriendo');
    Exit;
  end;
  
  try
    WriteLn('[Server] Iniciando servidor HTTP en puerto ', FPort);
    FServer.Active := True;
    FRunning := True;
    WriteLn('[Server] Servidor iniciado exitosamente');
    WriteLn('[Server] Endpoints disponibles:');
    WriteLn('  POST http://localhost:', FPort, '/api/data/json');
    WriteLn('  POST http://localhost:', FPort, '/api/data/stream');
    WriteLn('  GET  http://localhost:', FPort, '/api/status');
  except
    on E: Exception do
    begin
      WriteLn('[Server] Error al iniciar servidor: ', E.Message);
      FRunning := False;
      raise;
    end;
  end;
end;

procedure TDataHTTPServer.Stop;
begin
  if not FRunning then
    Exit;
  
  try
    WriteLn('[Server] Deteniendo servidor...');
    FServer.Active := False;
    FRunning := False;
    WriteLn('[Server] Servidor detenido');
  except
    on E: Exception do
      WriteLn('[Server] Error al detener servidor: ', E.Message);
  end;
end;

function TDataHTTPServer.IsRunning: Boolean;
begin
  Result := FRunning;
end;

procedure TDataHTTPServer.LogRequest(ARequest: TRequest);
begin
  WriteLn(Format('[Server] %s %s - Content-Type: %s, Content-Length: %d',
    [ARequest.Method, ARequest.URI, ARequest.ContentType, ARequest.ContentLength]));
end;

procedure TDataHTTPServer.HandlePostJSON(ARequest: TRequest; AResponse: TResponse);
var
  JSONData: string;
  Registro: TRegistro;
  ErrorMsg: string;
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  Registros: TRegistroArray;
  I: Integer;
begin
  LogRequest(ARequest);
  
  // Inicializar variables
  SetLength(Registros, 0);
  FillChar(Registro, SizeOf(TRegistro), 0);
  ErrorMsg := '';
  JSONArray := nil;
  JSONObj := nil;
  JSONData := '';
  I := 0;
  
  // Verificar Content-Type
  if Pos('application/json', LowerCase(ARequest.ContentType)) = 0 then
  begin
    SendJSONResponse(AResponse, 400, 'Content-Type debe ser application/json');
    Exit;
  end;
  
  try
    // Leer contenido JSON
    JSONData := ARequest.Content;
    
    if JSONData = '' then
    begin
      SendJSONResponse(AResponse, 400, 'Cuerpo JSON vacío');
      Exit;
    end;
    
    WriteLn('[Server] JSON recibido (primeros 200 chars): ', Copy(JSONData, 1, 200));
    
    // Parsear JSON - puede ser un objeto o array
    if (Length(JSONData) > 0) and (JSONData[1] = '[') then
    begin
      // Es un array de registros
      JSONArray := TJSONArray(GetJSON(JSONData));
      try
        SetLength(Registros, JSONArray.Count);
        
        for I := 0 to JSONArray.Count - 1 do
        begin
          JSONObj := JSONArray.Objects[I];
          Registros[I].id := JSONObj.Get('id', 0);
          Registros[I].te := JSONObj.Get('te', 0);
          Registros[I].hr := JSONObj.Get('hr', 0);
          Registros[I].mp01 := JSONObj.Get('mp01', 0);
          Registros[I].mp25 := JSONObj.Get('mp25', 0);
          Registros[I].mp10 := JSONObj.Get('mp10', 0);
          Registros[I].h01 := JSONObj.Get('h01', 0);
          Registros[I].h25 := JSONObj.Get('h25', 0);
          Registros[I].h50 := JSONObj.Get('h50', 0);
          Registros[I].h10 := JSONObj.Get('h10', 0);
        end;
        
        // Guardar batch
        if FUseCase.ExecuteBatch(Registros) then
          SendJSONResponse(AResponse, 200, Format('Guardados %d registros exitosamente', [Length(Registros)]))
        else
          SendJSONResponse(AResponse, 500, 'Error al guardar registros');
      finally
        JSONArray.Free;
      end;
    end
    else
    begin
      // Es un solo registro
      Registro := ParseJSONToRegistro(JSONData);
      
      // Validar y guardar
      if FUseCase.ExecuteWithValidation(Registro, ErrorMsg) then
        SendJSONResponse(AResponse, 200, 'Registro guardado exitosamente')
      else
        SendJSONResponse(AResponse, 400, 'Error: ' + ErrorMsg);
    end;
  except
    on E: Exception do
    begin
      WriteLn('[Server] Error procesando JSON: ', E.Message);
      SendJSONResponse(AResponse, 500, 'Error interno: ' + E.Message);
    end;
  end;
end;

procedure TDataHTTPServer.HandlePostStream(ARequest: TRequest; AResponse: TResponse);
var
  Registros: TRegistroArray;
  ErrorMsg: string;
  MemStream: TMemoryStream;
begin
  LogRequest(ARequest);
  
  // Inicializar variables
  SetLength(Registros, 0);
  ErrorMsg := '';
  
  // Verificar Content-Type
  if Pos('application/octet-stream', LowerCase(ARequest.ContentType)) = 0 then
  begin
    SendJSONResponse(AResponse, 400, 'Content-Type debe ser application/octet-stream');
    Exit;
  end;
  
  try
    // Crear stream desde el contenido
    MemStream := TMemoryStream.Create;
    try
      // Escribir el contenido en el stream
      if Length(ARequest.Content) > 0 then
      begin
        MemStream.Write(ARequest.Content[1], Length(ARequest.Content));
        MemStream.Position := 0;
        
        // Parsear stream binario
        Registros := ParseStreamToRegistros(MemStream);
        
        WriteLn(Format('[Server] Stream recibido: %d bytes, %d registros', 
          [ARequest.ContentLength, Length(Registros)]));
        
        // Guardar usando batch
        if FUseCase.ExecuteBatch(Registros) then
          SendJSONResponse(AResponse, 200, Format('Guardados %d registros desde stream', [Length(Registros)]))
        else
          SendJSONResponse(AResponse, 500, 'Error al guardar registros desde stream');
      end
      else
      begin
        SendJSONResponse(AResponse, 400, 'Stream vacío');
      end;
    finally
      MemStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('[Server] Error procesando stream: ', E.Message);
      SendJSONResponse(AResponse, 500, 'Error interno: ' + E.Message);
    end;
  end;
end;

procedure TDataHTTPServer.HandleGetStatus(ARequest: TRequest; AResponse: TResponse);
var
  TotalRegistros: Integer;
begin
  LogRequest(ARequest);
  
  try
    TotalRegistros := FRepository.ContarRegistros;
    SendJSONResponse(AResponse, 200, 
      Format('{"status":"running","port":%d,"total_registros":%d}', 
        [FPort, TotalRegistros]));
  except
    on E: Exception do
      SendJSONResponse(AResponse, 500, 'Error obteniendo status: ' + E.Message);
  end;
end;

procedure TDataHTTPServer.HandleNotFound(ARequest: TRequest; AResponse: TResponse);
begin
  LogRequest(ARequest);
  SendJSONResponse(AResponse, 404, 'Endpoint no encontrado');
end;

function TDataHTTPServer.ParseJSONToRegistro(const AJSON: string): TRegistro;
var
  JSONData: TJSONData;
  JSONObj: TJSONObject;
begin
  Result := CrearRegistroVacio;
  
  JSONData := GetJSON(AJSON);
  try
    if JSONData is TJSONObject then
    begin
      JSONObj := TJSONObject(JSONData);
      Result.id := JSONObj.Get('id', 0);
      Result.te := JSONObj.Get('te', 0);
      Result.hr := JSONObj.Get('hr', 0);
      Result.mp01 := JSONObj.Get('mp01', 0);
      Result.mp25 := JSONObj.Get('mp25', 0);
      Result.mp10 := JSONObj.Get('mp10', 0);
      Result.h01 := JSONObj.Get('h01', 0);
      Result.h25 := JSONObj.Get('h25', 0);
      Result.h50 := JSONObj.Get('h50', 0);
      Result.h10 := JSONObj.Get('h10', 0);
    end;
  finally
    JSONData.Free;
  end;
end;

function TDataHTTPServer.ParseStreamToRegistros(AStream: TStream): TRegistroArray;
var
  Count, I: Integer;
  Registro: TRegistro;
begin
  SetLength(Result, 0);
  FillChar(Registro, SizeOf(TRegistro), 0);
  Count := 0;
  I := 0;
  
  if AStream = nil then
    Exit;
  
  AStream.Position := 0;
  Count := AStream.Size div SizeOf(TRegistro);
  
  SetLength(Result, Count);
  
  for I := 0 to Count - 1 do
  begin
    AStream.Read(Registro, SizeOf(TRegistro));
    Result[I] := Registro;
  end;
end;

procedure TDataHTTPServer.SendJSONResponse(AResponse: TResponse; AStatusCode: Integer; 
                                          const AMessage: string);
var
  JSONResponse: string;
begin
  AResponse.Code := AStatusCode;
  AResponse.ContentType := 'application/json';
  
  // Si el mensaje ya es JSON válido, usarlo directamente
  if (Length(AMessage) > 0) and (AMessage[1] = '{') then
    JSONResponse := AMessage
  else
    JSONResponse := Format('{"message":"%s","status":%d}', 
      [StringReplace(AMessage, '"', '\"', [rfReplaceAll]), AStatusCode]);
  
  AResponse.Content := JSONResponse;
  AResponse.ContentLength := Length(JSONResponse);
  AResponse.SendContent;
end;

end.
