unit Lazy.NATO;

interface

uses
  Lazy.Types, System.Classes;

type
  TLZNATO = class(TLZObject)
  private
    function GetNatoChar(AChar: char): string;
    function GetNATOString(AValue: string): string;
  public
    class function AsNATO(AValue: string): string;
  public

  end;

implementation

uses
  System.StrUtils, System.SysUtils, Winapi.Windows, Lazy.Utils;

const
  NATOUpper: array [65 .. 90] of string = ('ALPHA', 'BRAVO', 'CHARLIE', 'DELTA',
    'ECHO', 'FOXTROT', 'GOLF', 'HOTEL', 'INDIA', 'JULIET', 'KILO', 'LIMA',
    'MIKE', 'NOVEMBER', 'OSCAR', 'PAPA', 'QUEBEC', 'ROMEO', 'SIERRA', 'TANGO',
    'UNIFORM', 'VICTOR', 'WHISKEY', 'XRAY', 'YANKEE', 'ZULU');
  NATOLower: array [97 .. 122] of string = ('alpha', 'bravo', 'charlie',
    'delta', 'echo', 'foxtrot', 'golf', 'hotel', 'india', 'juliet', 'kilo',
    'lima', 'mike', 'november', 'oscar', 'papa', 'quebec', 'romeo', 'sierra',
    'tango', 'uniform', 'victor', 'whiskey', 'xray', 'yankee', 'zulu');
  NATONumeric: array [48 .. 57] of string = ('Zero', 'One', 'Two', 'Three',
    'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine');

class function TLZNATO.AsNATO(AValue: string): string;
var
  LNATO: TLZNATO;
begin
  LNATO := TLZNATO.Create;
  try
    Result := LNATO.GetNATOString(AValue);
  finally
    FreeAndNil(LNATO);
  end;
end;

function TLZNATO.GetNatoChar(AChar: char): string;
var
  Index: integer;
begin
  Result := AChar;

  Index := Ord(AChar);

  if Index in [48 .. 57] then
  begin
    Result := NATONumeric[Index];
  end;
  if Index in [65 .. 90] then
  begin
    Result := NATOUpper[Index];
  end;
  if Index in [97 .. 122] then
  begin
    Result := NATOLower[Index];
  end;

end;

function TLZNATO.GetNATOString(AValue: string): string;
var
  Idx: integer;
  NATO: string;
begin

  for Idx := 1 to Length(AValue) do
  begin

    if not TLZString.IsEmptyString(Result) then
      Result := Result + ' ';

    NATO := GetNatoChar(AValue[Idx]);

    Result := Result + format('%s', [NATO]);

  end;

end;

end.
