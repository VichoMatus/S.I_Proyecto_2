program client_main;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Cliente HTTP Pascal - Programa Principal
  
  Lee data.dat y envía los datos al servidor Pascal (puerto 8080)
  ============================================================================ }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  Entities, FileReader, HTTPClient;

type
  { TClientApplication }
  
  TClientApplication = class(TCustomApplication)
  private
    FDataFile: string;
    FServerURL: string;
    FFileReader: TDataFileReader;
    FHTTPClient: TDataHTTPClient;
    
    procedure PrintHeader;
    procedure PrintUsage;
    function ProcessData: Boolean;
    
  protected
    procedure DoRun; override;
    
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure WriteHelp; virtual;
  end;

{ TClientApplication }

constructor TClientApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  
  // Valores por defecto
  FDataFile := '';
  FServerURL := 'http://localhost:8080';
  FFileReader := nil;
  FHTTPClient := nil;
end;

destructor TClientApplication.Destroy;
begin
  if Assigned(FHTTPClient) then
    FHTTPClient.Free;
  if Assigned(FFileReader) then
    FFileReader.Free;
  inherited Destroy;
end;

procedure TClientApplication.PrintHeader;
begin
  WriteLn('');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Cliente HTTP Pascal - DataLogger IoT');
  WriteLn('  INFO1157 - Sistemas Inteligentes');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('');
end;

procedure TClientApplication.PrintUsage;
begin
  WriteLn('Uso:');
  WriteLn('  client_main --file=<archivo> [--server=<url>]');
  WriteLn('');
  WriteLn('Opciones:');
  WriteLn('  --file=<archivo>   Ruta al archivo data.dat (requerido)');
  WriteLn('  --server=<url>     URL del servidor (default: http://localhost:8080)');
  WriteLn('  --help             Muestra esta ayuda');
  WriteLn('');
  WriteLn('Ejemplos:');
  WriteLn('  client_main --file=data.dat');
  WriteLn('  client_main --file=..\..\data\data.dat --server=http://localhost:8080');
  WriteLn('');
end;

procedure TClientApplication.WriteHelp;
begin
  PrintHeader;
  PrintUsage;
end;

function TClientApplication.ProcessData: Boolean;
var
  Registros: TRegistroArray;
  I: Integer;
begin
  Result := False;
  
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('[Cliente] PASO 1: Lectura de archivo binario');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('');
  
  // Leer archivo
  Registros := FFileReader.ReadAllRegistros;
  
  if Length(Registros) = 0 then
  begin
    WriteLn('[Cliente] ✗ Error: ', FFileReader.LastError);
    Exit;
  end;
  
  // Mostrar primeros 3 registros
  WriteLn('');
  WriteLn('[Cliente] Primeros registros:');
  for I := 0 to Min(2, Length(Registros) - 1) do
    WriteLn('  ', I + 1, '. ', RegistroToString(Registros[I]));
  
  if Length(Registros) > 3 then
    WriteLn('  ... (', Length(Registros) - 3, ' registros más)');
  
  WriteLn('');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('[Cliente] PASO 2: Verificar conexión con servidor');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('');
  
  // Verificar servidor
  if not FHTTPClient.CheckServerStatus then
  begin
    WriteLn('[Cliente] ✗ Servidor no disponible: ', FHTTPClient.LastError);
    WriteLn('[Cliente] Por favor inicia el servidor Pascal primero.');
    Exit;
  end;
  
  WriteLn('');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('[Cliente] PASO 3: Enviar datos como JSON');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('');
  
  // Enviar como JSON
  if not FHTTPClient.SendJSON(Registros) then
  begin
    WriteLn('[Cliente] ✗ Error enviando JSON: ', FHTTPClient.LastError);
    Exit;
  end;
  
  WriteLn('');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('[Cliente] PASO 4: Enviar datos como Stream binario');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('');
  
  // Enviar como Stream
  if not FHTTPClient.SendStream(Registros) then
  begin
    WriteLn('[Cliente] ✗ Error enviando Stream: ', FHTTPClient.LastError);
    Exit;
  end;
  
  WriteLn('');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('[Cliente] ✓ PROCESO COMPLETADO EXITOSAMENTE');
  WriteLn('[Cliente] ════════════════════════════════════════════════');
  WriteLn('');
  WriteLn('[Cliente] Resumen:');
  WriteLn('  - Archivo leído: ', FDataFile);
  WriteLn('  - Registros procesados: ', Length(Registros));
  WriteLn('  - Servidor: ', FServerURL);
  WriteLn('  - Formatos enviados: JSON + Stream');
  WriteLn('');
  
  Result := True;
end;

procedure TClientApplication.DoRun;
var
  ErrorMsg: string;
begin
  ErrorMsg := '';
  
  // Parse command line
  ErrorMsg := CheckOptions('hf:s:', ['help', 'file:', 'server:']);
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // Help
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  
  PrintHeader;
  
  // Obtener parámetros
  if HasOption('f', 'file') then
    FDataFile := GetOptionValue('f', 'file')
  else
  begin
    WriteLn('[Cliente] ✗ Error: Falta el parámetro --file');
    WriteLn('');
    PrintUsage;
    Terminate;
    Exit;
  end;
  
  if HasOption('s', 'server') then
    FServerURL := GetOptionValue('s', 'server');
  
  WriteLn('[Cliente] Configuración:');
  WriteLn('  Archivo: ', FDataFile);
  WriteLn('  Servidor: ', FServerURL);
  WriteLn('');
  
  // Inicializar componentes
  try
    FFileReader := TDataFileReader.Create(FDataFile);
    FHTTPClient := TDataHTTPClient.Create(FServerURL);
    
    // Verificar que el archivo existe
    if not FFileReader.FileExists then
    begin
      WriteLn('[Cliente] ✗ Error: Archivo no encontrado: ', FDataFile);
      Terminate;
      Exit;
    end;
    
    // Procesar datos
    if ProcessData then
      ExitCode := 0
    else
      ExitCode := 1;
      
  except
    on E: Exception do
    begin
      WriteLn('[Cliente] ✗ Excepción: ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  // Terminar
  Terminate;
end;

var
  Application: TClientApplication;
  
begin
  Application := TClientApplication.Create(nil);
  Application.Title := 'Cliente DataLogger';
  Application.Run;
  Application.Free;
end.
