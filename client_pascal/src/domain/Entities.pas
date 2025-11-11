unit Entities;

{$mode objfpc}{$H+}

{ ============================================================================
  Proyecto #2 - Sistemas Inteligentes
  Cliente Pascal - Capa de Dominio - Entidades
  
  Define las entidades del cliente (idénticas al servidor para compatibilidad)
  ============================================================================ }

interface

uses
  SysUtils, Classes;

type
  { TRegistro - Estructura de registro de datos ambientales
    Debe coincidir exactamente con el formato del servidor y data.dat }
  
  TRegistro = packed record
    id   : Byte;     // ID de estación (1-10)
    te   : Byte;     // Temperatura en °C
    hr   : Byte;     // Humedad relativa en %
    mp01 : Word;     // Material Particulado 0.1 µm
    mp25 : Word;     // Material Particulado 2.5 µm
    mp10 : Word;     // Material Particulado 10 µm
    h01  : Word;     // Histograma de partículas > 0.1 µm
    h25  : Word;     // Histograma de partículas > 2.5 µm
    h50  : Word;     // Histograma de partículas > 5.0 µm
    h10  : Word;     // Histograma de partículas > 10 µm
  end;
  
  { Array dinámico de registros }
  TRegistroArray = array of TRegistro;

{ Funciones auxiliares }

{ Crear registro vacío }
function CrearRegistroVacio: TRegistro;

{ Convertir registro a JSON }
function RegistroToJSON(const ARegistro: TRegistro): string;

{ Convertir array de registros a JSON array }
function RegistrosToJSONArray(const ARegistros: TRegistroArray): string;

{ Obtener representación en texto del registro }
function RegistroToString(const ARegistro: TRegistro): string;

implementation

function CrearRegistroVacio: TRegistro;
begin
  FillChar(Result, SizeOf(TRegistro), 0);
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

function RegistrosToJSONArray(const ARegistros: TRegistroArray): string;
var
  I: Integer;
  JSONList: TStringList;
begin
  Result := '';
  JSONList := TStringList.Create;
  try
    JSONList.Add('[');
    
    for I := Low(ARegistros) to High(ARegistros) do
    begin
      if I > Low(ARegistros) then
        JSONList.Add(',');
      JSONList.Add(RegistroToJSON(ARegistros[I]));
    end;
    
    JSONList.Add(']');
    Result := JSONList.Text;
    // Eliminar saltos de línea
    Result := StringReplace(Result, #13#10, '', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '', [rfReplaceAll]);
    Result := StringReplace(Result, #13, '', [rfReplaceAll]);
  finally
    JSONList.Free;
  end;
end;

function RegistroToString(const ARegistro: TRegistro): string;
begin
  Result := Format('Est:%d T:%d°C HR:%d%% PM[%.1:%.2:%.10] H[%d,%d,%d,%d]',
                   [ARegistro.id, ARegistro.te, ARegistro.hr,
                    ARegistro.mp01, ARegistro.mp25, ARegistro.mp10,
                    ARegistro.h01, ARegistro.h25, ARegistro.h50, ARegistro.h10]);
end;

end.
