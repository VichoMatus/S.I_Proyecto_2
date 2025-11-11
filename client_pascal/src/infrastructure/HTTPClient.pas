unit HTTPClient;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Cliente Pascal - Cliente HTTP
  
  Envía datos al servidor usando fphttpclient
  ============================================================================ }

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets,
  Entities;

type
  { TDataHTTPClient - Cliente HTTP para enviar datos }
  
  TDataHTTPClient = class
  private
    FClient: TFPHTTPClient;
    FServerURL: string;
    FLastError: string;
    FLastResponseCode: Integer;
    
  public
    constructor Create(const AServerURL: string);
    destructor Destroy; override;
    
    { Enviar registros como JSON }
    function SendJSON(const ARegistros: TRegistroArray): Boolean;
    
    { Enviar registros como Stream binario }
    function SendStream(const ARegistros: TRegistroArray): Boolean;
    
    { Verificar estado del servidor }
    function CheckServerStatus: Boolean;
    
    { Último error }
    property LastError: string read FLastError;
    property LastResponseCode: Integer read FLastResponseCode;
    property ServerURL: string read FServerURL write FServerURL;
  end;

implementation

constructor TDataHTTPClient.Create(const AServerURL: string);
begin
  inherited Create;
  FServerURL := AServerURL;
  FLastError := '';
  FLastResponseCode := 0;
  
  FClient := TFPHTTPClient.Create(nil);
  FClient.AllowRedirect := True;
end;

destructor TDataHTTPClient.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

function TDataHTTPClient.SendJSON(const ARegistros: TRegistroArray): Boolean;
var
  JSONData: string;
  Response: string;
  RequestStream: TStringStream;
begin
  Result := False;
  FLastError := '';
  FLastResponseCode := 0;
  JSONData := '';
  Response := '';
  
  if Length(ARegistros) = 0 then
  begin
    FLastError := 'No hay registros para enviar';
    Exit;
  end;
  
  try
    // Convertir a JSON
    JSONData := RegistrosToJSONArray(ARegistros);
    
    WriteLn('[HTTPClient] Enviando JSON...');
    WriteLn('[HTTPClient] URL: ', FServerURL, '/api/data/json');
    WriteLn('[HTTPClient] Registros: ', Length(ARegistros));
    WriteLn('[HTTPClient] Tamaño JSON: ', Length(JSONData), ' bytes');
    
    // Crear stream con el JSON
    RequestStream := TStringStream.Create(JSONData);
    try
      // Configurar headers
      FClient.RequestHeaders.Clear;
      FClient.AddHeader('Content-Type', 'application/json');
      FClient.AddHeader('User-Agent', 'PascalDataLogger/1.0');
      
      // Hacer POST
      FClient.RequestBody := RequestStream;
      Response := FClient.Post(FServerURL + '/api/data/json');
      FLastResponseCode := FClient.ResponseStatusCode;
      
      WriteLn('[HTTPClient] Respuesta: HTTP ', FLastResponseCode);
      WriteLn('[HTTPClient] ', Response);
      
      Result := (FLastResponseCode >= 200) and (FLastResponseCode < 300);
      
      if Result then
        WriteLn('[HTTPClient] ✓ JSON enviado exitosamente')
      else
        FLastError := Format('Error HTTP %d: %s', [FLastResponseCode, Response]);
        
    finally
      RequestStream.Free;
    end;
  except
    on E: Exception do
    begin
      FLastError := 'Excepción enviando JSON: ' + E.Message;
      WriteLn('[HTTPClient] ✗ Error: ', FLastError);
    end;
  end;
end;

function TDataHTTPClient.SendStream(const ARegistros: TRegistroArray): Boolean;
var
  DataStream: TMemoryStream;
  Response: string;
  I: Integer;
begin
  Result := False;
  FLastError := '';
  FLastResponseCode := 0;
  Response := '';
  I := 0;
  
  if Length(ARegistros) = 0 then
  begin
    FLastError := 'No hay registros para enviar';
    Exit;
  end;
  
  try
    // Crear stream binario
    DataStream := TMemoryStream.Create;
    try
      // Escribir todos los registros al stream
      for I := Low(ARegistros) to High(ARegistros) do
        DataStream.Write(ARegistros[I], SizeOf(TRegistro));
      
      DataStream.Position := 0;
      
      WriteLn('[HTTPClient] Enviando Stream...');
      WriteLn('[HTTPClient] URL: ', FServerURL, '/api/data/stream');
      WriteLn('[HTTPClient] Registros: ', Length(ARegistros));
      WriteLn('[HTTPClient] Tamaño Stream: ', DataStream.Size, ' bytes');
      
      // Configurar headers
      FClient.RequestHeaders.Clear;
      FClient.AddHeader('Content-Type', 'application/octet-stream');
      FClient.AddHeader('User-Agent', 'PascalDataLogger/1.0');
      
      // Hacer POST
      FClient.RequestBody := DataStream;
      Response := FClient.Post(FServerURL + '/api/data/stream');
      FLastResponseCode := FClient.ResponseStatusCode;
      
      WriteLn('[HTTPClient] Respuesta: HTTP ', FLastResponseCode);
      WriteLn('[HTTPClient] ', Response);
      
      Result := (FLastResponseCode >= 200) and (FLastResponseCode < 300);
      
      if Result then
        WriteLn('[HTTPClient] ✓ Stream enviado exitosamente')
      else
        FLastError := Format('Error HTTP %d: %s', [FLastResponseCode, Response]);
        
    finally
      DataStream.Free;
    end;
  except
    on E: Exception do
    begin
      FLastError := 'Excepción enviando Stream: ' + E.Message;
      WriteLn('[HTTPClient] ✗ Error: ', FLastError);
    end;
  end;
end;

function TDataHTTPClient.CheckServerStatus: Boolean;
var
  Response: string;
begin
  Result := False;
  FLastError := '';
  FLastResponseCode := 0;
  
  try
    WriteLn('[HTTPClient] Verificando servidor...');
    WriteLn('[HTTPClient] URL: ', FServerURL, '/api/status');
    
    Response := FClient.Get(FServerURL + '/api/status');
    FLastResponseCode := FClient.ResponseStatusCode;
    
    WriteLn('[HTTPClient] Respuesta: HTTP ', FLastResponseCode);
    
    Result := (FLastResponseCode >= 200) and (FLastResponseCode < 300);
    
    if Result then
      WriteLn('[HTTPClient] ✓ Servidor activo')
    else
    begin
      FLastError := Format('Servidor respondió con HTTP %d', [FLastResponseCode]);
      WriteLn('[HTTPClient] ✗ ', FLastError);
    end;
  except
    on E: Exception do
    begin
      FLastError := 'No se pudo conectar al servidor: ' + E.Message;
      WriteLn('[HTTPClient] ✗ Error: ', FLastError);
    end;
  end;
end;

end.
