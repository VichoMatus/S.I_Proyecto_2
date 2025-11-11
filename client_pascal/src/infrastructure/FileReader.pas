unit FileReader;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Cliente Pascal - Lector de Archivos Binarios
  
  Lee el archivo data.dat y lo convierte en array de TRegistro
  ============================================================================ }

interface

uses
  Classes, SysUtils, Entities;

type
  { TDataFileReader - Lector de archivo data.dat }
  
  TDataFileReader = class
  private
    FFileName: string;
    FLastError: string;
    
  public
    constructor Create(const AFileName: string);
    
    { Leer todos los registros del archivo }
    function ReadAllRegistros: TRegistroArray;
    
    { Verificar si el archivo existe }
    function FileExists: Boolean;
    
    { Obtener tamaño del archivo }
    function GetFileSize: Int64;
    
    { Obtener cantidad estimada de registros }
    function GetRecordCount: Integer;
    
    { Último error ocurrido }
    property LastError: string read FLastError;
    property FileName: string read FFileName;
  end;

implementation

constructor TDataFileReader.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FLastError := '';
end;

function TDataFileReader.FileExists: Boolean;
begin
  Result := SysUtils.FileExists(FFileName);
end;

function TDataFileReader.GetFileSize: Int64;
var
  F: TFileStream;
begin
  Result := 0;
  if not FileExists then
  begin
    FLastError := 'Archivo no existe: ' + FFileName;
    Exit;
  end;
  
  try
    F := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := F.Size;
    finally
      F.Free;
    end;
  except
    on E: Exception do
    begin
      FLastError := 'Error obteniendo tamaño: ' + E.Message;
      Result := 0;
    end;
  end;
end;

function TDataFileReader.GetRecordCount: Integer;
var
  FileSize: Int64;
begin
  FileSize := GetFileSize;
  if FileSize > 0 then
    Result := FileSize div SizeOf(TRegistro)
  else
    Result := 0;
end;

function TDataFileReader.ReadAllRegistros: TRegistroArray;
var
  F: TFileStream;
  Count, I: Integer;
  Registro: TRegistro;
  BytesRead: Integer;
begin
  SetLength(Result, 0);
  FLastError := '';
  F := nil;
  Count := 0;
  I := 0;
  BytesRead := 0;
  FillChar(Registro, SizeOf(TRegistro), 0);
  
  // Verificar que el archivo existe
  if not FileExists then
  begin
    FLastError := 'Archivo no existe: ' + FFileName;
    Exit;
  end;
  
  try
    // Abrir archivo en modo lectura
    F := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
      // Calcular cantidad de registros
      Count := F.Size div SizeOf(TRegistro);
      
      if Count = 0 then
      begin
        FLastError := 'Archivo vacío o tamaño inválido';
        Exit;
      end;
      
      WriteLn('[FileReader] Archivo: ', FFileName);
      WriteLn('[FileReader] Tamaño: ', F.Size, ' bytes');
      WriteLn('[FileReader] Registros: ', Count);
      
      // Redimensionar array
      SetLength(Result, Count);
      
      // Leer registros
      for I := 0 to Count - 1 do
      begin
        FillChar(Registro, SizeOf(TRegistro), 0);
        BytesRead := F.Read(Registro, SizeOf(TRegistro));
        
        if BytesRead <> SizeOf(TRegistro) then
        begin
          FLastError := Format('Error leyendo registro %d: bytes leídos=%d, esperados=%d',
                              [I, BytesRead, SizeOf(TRegistro)]);
          SetLength(Result, I); // Truncar array
          Break;
        end;
        
        Result[I] := Registro;
        
        // Mostrar progreso cada 10 registros
        if (I + 1) mod 10 = 0 then
          Write(Format(#13'[FileReader] Leídos: %d/%d registros', [I + 1, Count]));
      end;
      
      WriteLn('');
      WriteLn('[FileReader] ✓ Lectura completada: ', Length(Result), ' registros');
      
    finally
      F.Free;
    end;
  except
    on E: Exception do
    begin
      FLastError := 'Error leyendo archivo: ' + E.Message;
      SetLength(Result, 0);
    end;
  end;
end;

end.
