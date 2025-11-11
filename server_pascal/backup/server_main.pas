program ServerMain;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Servidor HTTP Pascal - Programa Principal
  
  Servidor HTTP en modo consola que recibe datos ambientales y los almacena
  en SQLite. Implementa Clean Architecture.
  
  Uso:
    server_main.exe [puerto]
  
  Ejemplo:
    server_main.exe 8080
  ============================================================================ }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  // Capa de Dominio
  Entities, Repositories,
  // Capa de Aplicación
  ReceiveDataUseCase,
  // Capa de Infraestructura
  SQLiteRepository, DBConfig,
  // Capa de Presentación
  HTTPServerUnit;

type
  { TServerApplication - Aplicación principal del servidor }
  TServerApplication = class(TCustomApplication)
  private
    FServer: TDataHTTPServer;
    FDBConnection: TSQLiteDatabaseConnection;
    FRepository: IRegistroRepository;
    FPort: Word;
    
    procedure InitializeDatabase;
    procedure InitializeServer;
    procedure ShowWelcomeMessage;
    procedure ShowHelpMessage;
    
  protected
    procedure DoRun; override;
    
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ ============================================================================
  Implementación de TServerApplication
  ============================================================================ }

constructor TServerApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FPort := 8080; // Puerto por defecto
  FServer := nil;
  FDBConnection := nil;
end;

destructor TServerApplication.Destroy;
begin
  if Assigned(FServer) then
    FServer.Free;
  if Assigned(FDBConnection) then
    FDBConnection.Free;
  inherited Destroy;
end;

procedure TServerApplication.ShowWelcomeMessage;
begin
  WriteLn('');
  WriteLn('╔═══════════════════════════════════════════════════════════════════╗');
  WriteLn('║  Proyecto #2 - Sistemas Inteligentes (INFO1157)                  ║');
  WriteLn('║  Servidor HTTP Pascal - DataLogger IoT                           ║');
  WriteLn('║  By Alberto Caro                                                  ║');
  WriteLn('╚═══════════════════════════════════════════════════════════════════╝');
  WriteLn('');
end;

procedure TServerApplication.ShowHelpMessage;
begin
  WriteLn('Uso: ', ExeName, ' [opciones]');
  WriteLn('');
  WriteLn('Opciones:');
  WriteLn('  -p, --port=PUERTO    Puerto del servidor (default: 8080)');
  WriteLn('  -h, --help           Mostrar esta ayuda');
  WriteLn('');
  WriteLn('Ejemplos:');
  WriteLn('  ', ExeName);
  WriteLn('  ', ExeName, ' --port=9090');
  WriteLn('');
end;

procedure TServerApplication.InitializeDatabase;
var
  Config: TDatabaseConfig;
begin
  WriteLn('[Init] Inicializando base de datos...');
  
  try
    // Obtener configuración por defecto (SQLite)
    Config := ObtenerConfigDefaultSQLite;
    WriteLn('[Init] Configuración: ', ConfigToString(Config));
    
    // Crear directorio si no existe
    if not CrearDirectorioBD(Config.RutaArchivo) then
      raise Exception.Create('No se pudo crear directorio para base de datos');
    
    // Crear conexión
    FDBConnection := TSQLiteDatabaseConnection.Create(Config);
    
    // Conectar
    if not FDBConnection.Conectar then
      raise Exception.Create('Error conectando a BD: ' + FDBConnection.ObtenerUltimoError);
    
    WriteLn('[Init] Conectado a: ', Config.RutaArchivo);
    
    // Crear tablas si no existen
    if not FDBConnection.CrearTablas then
      raise Exception.Create('Error creando tablas: ' + FDBConnection.ObtenerUltimoError);
    
    WriteLn('[Init] Tablas verificadas/creadas correctamente');
    
    // Crear repositorio
    FRepository := TSQLiteRegistroRepository.Create(FDBConnection.Connection, False);
    
    WriteLn('[Init] Base de datos lista');
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] Error inicializando base de datos: ', E.Message);
      raise;
    end;
  end;
end;

procedure TServerApplication.InitializeServer;
begin
  WriteLn('[Init] Inicializando servidor HTTP...');
  
  try
    // Crear servidor
    FServer := TDataHTTPServer.Create(FRepository, FPort);
    
    WriteLn('[Init] Servidor HTTP configurado en puerto ', FPort);
  except
    on E: Exception do
    begin
      WriteLn('[ERROR] Error inicializando servidor: ', E.Message);
      raise;
    end;
  end;
end;

procedure TServerApplication.DoRun;
var
  ErrorMsg: String;
  Comando: string;
begin
  // Parsear parámetros
  ErrorMsg := CheckOptions('hp:', 'help port:');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // Mostrar ayuda si se solicita
  if HasOption('h', 'help') then
  begin
    ShowHelpMessage;
    Terminate;
    Exit;
  end;

  // Obtener puerto si se especificó
  if HasOption('p', 'port') then
  begin
    FPort := StrToIntDef(GetOptionValue('p', 'port'), 8080);
    if (FPort < 1024) or (FPort > 65535) then
    begin
      WriteLn('[ERROR] Puerto inválido. Debe estar entre 1024 y 65535');
      Terminate;
      Exit;
    end;
  end;

  // Mostrar mensaje de bienvenida
  ShowWelcomeMessage;
  
  try
    // Inicializar componentes
    InitializeDatabase;
    InitializeServer;
    
    // Iniciar servidor
    FServer.Start;
    
    WriteLn('');
    WriteLn('╔═══════════════════════════════════════════════════════════════════╗');
    WriteLn('║  SERVIDOR ACTIVO                                                  ║');
    WriteLn('╚═══════════════════════════════════════════════════════════════════╝');
    WriteLn('');
    WriteLn('Endpoints disponibles:');
    WriteLn('  • POST http://localhost:', FPort, '/api/data/json    - Recibir datos JSON');
    WriteLn('  • POST http://localhost:', FPort, '/api/data/stream  - Recibir datos Stream');
    WriteLn('  • GET  http://localhost:', FPort, '/api/status       - Estado del servidor');
    WriteLn('');
    WriteLn('Comandos:');
    WriteLn('  status  - Mostrar estadísticas');
    WriteLn('  help    - Mostrar ayuda');
    WriteLn('  quit    - Detener servidor y salir');
    WriteLn('');
    
    // Loop principal - esperar comandos
    repeat
      Write('> ');
      ReadLn(Comando);
      Comando := LowerCase(Trim(Comando));
      
      if Comando = 'status' then
      begin
        try
          WriteLn('');
          WriteLn('═══ ESTADÍSTICAS DEL SERVIDOR ═══');
          WriteLn('Puerto: ', FPort);
          WriteLn('Estado: ', IfThen(FServer.IsRunning, 'Activo', 'Inactivo'));
          WriteLn('Total registros: ', FRepository.ContarRegistros);
          WriteLn('Base de datos: ', IfThen(FDBConnection.EstaConectado, 'Conectada', 'Desconectada'));
          WriteLn('');
        except
          on E: Exception do
            WriteLn('[ERROR] ', E.Message);
        end;
      end
      else if Comando = 'help' then
      begin
        WriteLn('');
        WriteLn('Comandos disponibles:');
        WriteLn('  status  - Mostrar estadísticas del servidor');
        WriteLn('  help    - Mostrar esta ayuda');
        WriteLn('  quit    - Detener servidor y salir');
        WriteLn('');
      end
      else if (Comando <> 'quit') and (Comando <> 'exit') and (Comando <> '') then
      begin
        WriteLn('[INFO] Comando desconocido. Escriba "help" para ver comandos disponibles.');
      end;
      
    until (Comando = 'quit') or (Comando = 'exit');
    
    // Detener servidor
    WriteLn('');
    WriteLn('[Shutdown] Deteniendo servidor...');
    FServer.Stop;
    
    WriteLn('[Shutdown] Cerrando base de datos...');
    FDBConnection.Desconectar;
    
    WriteLn('[Shutdown] Servidor detenido correctamente');
    WriteLn('');
    
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('[ERROR FATAL] ', E.Message);
      WriteLn('');
      ExitCode := 1;
    end;
  end;

  // Terminar aplicación
  Terminate;
end;

{ ============================================================================
  Programa Principal
  ============================================================================ }

var
  Application: TServerApplication;

begin
  Application := TServerApplication.Create(nil);
  Application.Title := 'Servidor HTTP Pascal - DataLogger';
  Application.Run;
  Application.Free;
end.
