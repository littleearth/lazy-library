unit VCL.Lazy.Compare;

interface

uses Classes, Windows, SysUtils, VCL.Lazy.Utils.Windows, Lazy.Types,
  Lazy.Compare;

type
  TLZCompare = class(TLZCompareBase)
  public
    class function GetFileVersion(AFilename: TFileName): string;
    class function GetFileProductVersion(AFilename: TFileName): string;
  end;

  TLZCompareConsole = class(TLZCompare)
  protected
    function GetUsage: string; virtual;
    function CheckCommandParameter(var AMessage: string;
      var AMode: TLZCompareMode; var AValue1: string; var AValue2: string)
      : boolean; virtual;
  public
    function Execute(var AMessage: string): integer; virtual;
  end;

implementation

uses
  WinApi.FileVersionInformation;

{ TCompare }

class function TLZCompare.GetFileVersion(AFilename: TFileName): string;
var
  LFileVersionInformation: TLZFileVersionInformation;
begin
  LFileVersionInformation := TLZFileVersionInformation.Create;
  try
    LFileVersionInformation.FileName := AFilename;
    Result := LFileVersionInformation.FileVersion;
  finally
    FreeAndNil(LFileVersionInformation);
  end;
end;

class function TLZCompare.GetFileProductVersion(AFilename: TFileName): string;
var
  LFileVersionInformation: TLZFileVersionInformation;
begin
  LFileVersionInformation := TLZFileVersionInformation.Create;
  try
    LFileVersionInformation.FileName := AFilename;
    Result := LFileVersionInformation.ProductVersion;
  finally
    FreeAndNil(LFileVersionInformation);
  end;
end;

{ TLZCompareConsole }

function TLZCompareConsole.CheckCommandParameter(var AMessage: string;
  var AMode: TLZCompareMode; var AValue1, AValue2: string): boolean;
var
  LValue: string;
begin
  Result := True;
  AMessage := '';
  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/MODE', LValue) then
    begin
      AMode := GetCompareMode(LValue);
    end
    else
    begin
      Result := False;
      AMessage := GetUsage;
    end;
  end;
  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/VALUE1', AValue1) then
    begin
      Result := False;
      AMessage := GetUsage;
    end;
  end;
  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/VALUE2', AValue2) then
    begin
      case AMode of
        vcGetMD5, vcGetVersionProduct, vcGetVersionFile:
          begin
            Result := True;
          end;
      else
        begin
          Result := False;
          AMessage := GetUsage;
        end;
      end;
    end;
  end;
end;

function TLZCompareConsole.Execute(var AMessage: string): integer;
var
  LMode: TLZCompareMode;
  LValue1, LValue2: string;
begin
  Result := -9999;
  if CheckCommandParameter(AMessage, LMode, LValue1, LValue2) then
  begin
    case LMode of
      vcVersionString:
        begin
          Result := CompareVersion(LValue1, LValue2);
          AMessage := Format('Compare version: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcVersionFile:
        begin
          LValue1 := GetFileVersion(LValue1);
          if TLZFile.IsValidFileName(LValue2) and FileExists(LValue2) then
          begin
            LValue2 := GetFileVersion(LValue2);
          end;
          Result := CompareVersion(LValue1, LValue2);
          AMessage := Format('Compare file: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcMD5File:
        begin
          LValue1 := GenerateMD5(LValue1);
          if TLZFile.IsValidFileName(LValue2) and FileExists(LValue2) then
          begin
            LValue2 := GenerateMD5(LValue2);
          end;
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare MD5: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcMD5String, vcString:
        begin
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare string: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcInteger:
        begin
          Result := CompareInteger(LValue1, LValue2);
          AMessage := Format('Compare integer: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcFloat:
        begin
          Result := CompareFloat(LValue1, LValue2);
          AMessage := Format('Compare float: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcDate:
        begin
          Result := CompareDate(LValue1, LValue2);
          AMessage := Format('Compare date: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcTime:
        begin
          Result := CompareTime(LValue1, LValue2);
          AMessage := Format('Compare time: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcDateTime:
        begin
          Result := CompareDateTime(LValue1, LValue2);
          AMessage := Format('Compare date and time: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcGetMD5:
        begin
          AMessage := GenerateMD5(LValue1);
          Result := 0;
        end;
      vcGetVersionFile:
        begin
          AMessage := GetFileVersion(LValue1);
          Result := 0;
        end;
      vcGetVersionProduct:
        begin
          AMessage := GetFileProductVersion(LValue1);
          Result := 0;
        end;
    end;
  end;
end;

function TLZCompareConsole.GetUsage: string;
var
  LUsage: TStringList;
begin
  LUsage := TStringList.Create;
  try
    LUsage.Add('Compare 2 values with the comparison both');
    LUsage.Add('displayed and returned in %ERRORLEVEL%');
    LUsage.Add('');
    LUsage.Add('   0 if Value1 = Value2');
    LUsage.Add('   value less than 0 if Value1 < Value2');
    LUsage.Add('   value greater than 0 if Value1 > Value2');
    LUsage.Add('');
    LUsage.Add('Usage instruction:');
    LUsage.Add('');
    LUsage.Add('/MODE:{mode} /VALUE1:{filename|text} /VALUE2:{filename|text}');
    LUsage.Add('');
    LUsage.Add('- /MODE (default STRING).');
    LUsage.Add('   Options: STRING, INTEGER, FLOAT, DATE, TIME,');
    LUsage.Add('   DATETIME, VERSIONSTRING, VERSIONFILE, VERSIONGET,');
    LUsage.Add('   MD5STRING, MD5FILE, GETMD5, GETVERSIONFILE,');
    LUsage.Add('   GETVERSIONPRODUCT');
    LUsage.Add('');
    LUsage.Add('- /VALUE1 is required for all options');
    LUsage.Add('');
    LUsage.Add('- /VALUE2 is required for all except');
    LUsage.Add('   GETMD5, GETVERSIONFILE, GETVERSIONPRODUCT');
    LUsage.Add('');
    LUsage.Add('Examples:');
    LUsage.Add('   /MODE:VERSIONSTRING /VALUE1:1.1.1.0 /VALUE2:1.1.1');
    LUsage.Add('   /MODE:DATE /VALUE1:2022-01-25 /VALUE2:2022/01/25');
    LUsage.Add('   /MODE:GETMD5 /VALUE1:"C:\MyFile.exe"');
    Result := LUsage.Text;
  finally
    FreeAndNil(LUsage);
  end;
end;

end.
