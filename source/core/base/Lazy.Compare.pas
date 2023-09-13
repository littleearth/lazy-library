unit Lazy.Compare;

interface

uses Classes, Windows, SysUtils, Lazy.Utils, Lazy.Types;

type
  TLZCompareMode = (vcString, vcInteger, vcFloat, vcDate, vcTime, vcDateTime,
    vcVersionString, vcVersionFile, vcMD5String, vcMD5File, vcGetMD5,
    vcGetVersionFile, vcGetVersionProduct);

  TLZCompareBase = class(TLZObject)
  protected
    function GetCompareMode(AVersionCompareMode: string): TLZCompareMode;
  public
    function CompareVersion(AVersion1: string; AVersion2: string): integer;
    function CompareInteger(AValue1, AValue2: string): integer;
    function CompareFloat(AValue1, AValue2: string): integer;
    function CompareDate(AValue1, AValue2: string): integer;
    function CompareTime(AValue1, AValue2: string): integer;
    function CompareDateTime(AValue1, AValue2: string): integer;
    function GenerateMD5(AFilename: TFileName): string;
  end;

implementation

uses
  IdHashMessageDigest, idHash, System.Math, System.DateUtils;

function TLZCompareBase.GetCompareMode(AVersionCompareMode: string)
  : TLZCompareMode;
begin
  Result := vcString;
  if SameText(AVersionCompareMode, 'INTEGER') then
    Result := vcInteger;
  if SameText(AVersionCompareMode, 'FLOAT') then
    Result := vcFloat;
  if SameText(AVersionCompareMode, 'DATE') then
    Result := vcDate;
  if SameText(AVersionCompareMode, 'TIME') then
    Result := vcTime;
  if SameText(AVersionCompareMode, 'DATETIME') then
    Result := vcDateTime;
  if SameText(AVersionCompareMode, 'VERSIONFILE') then
    Result := vcVersionFile;
  if SameText(AVersionCompareMode, 'VERSIONSTRING') then
    Result := vcVersionString;
  if SameText(AVersionCompareMode, 'MD5STRING') then
    Result := vcMD5String;
  if SameText(AVersionCompareMode, 'MD5FILE') then
    Result := vcMD5File;
  if SameText(AVersionCompareMode, 'GETMD5') then
    Result := vcGetMD5;
  if SameText(AVersionCompareMode, 'GETVERSIONFILE') then
    Result := vcGetVersionFile;
  if SameText(AVersionCompareMode, 'GETVERSIONPRODUCT') then
    Result := vcGetVersionProduct
end;

function TLZCompareBase.CompareVersion(AVersion1, AVersion2: string): integer;
var
  LVersionDetails: TApplicationVersionDetails;
  LVersion1, LVersion2: string;
  LVersion1Current, LVersion2Current: boolean;
begin
  Result := -9999;
  LVersion1 := AVersion1;
  LVersion2 := AVersion2;
  Log('Version1: ' + LVersion1 + ', Version2: ' + LVersion2);
  LVersionDetails := TApplicationVersionDetails.Create(nil);
  try

    LVersionDetails.Version := LVersion1;
    LVersion1 := LVersionDetails.AsString;

    LVersionDetails.Reset;
    LVersionDetails.Version := LVersion2;
    LVersion2 := LVersionDetails.AsString;

    LVersionDetails.Reset;
    LVersionDetails.Version := LVersion2;
    LVersion2Current := LVersionDetails.IsCurrent(LVersion1);

    LVersionDetails.Reset;
    LVersionDetails.Version := LVersion1;
    LVersion1Current := LVersionDetails.IsCurrent(LVersion2);

    Log('Version1: ' + LVersion1 + ', Version2: ' + LVersion2);

    if LVersion1Current and LVersion2Current then
    begin
      Result := 0;
    end
    else
    begin
      if LVersion1Current and (not LVersion2Current) then
      begin
        Result := -1;
      end;
      if LVersion2Current and (not LVersion1Current) then
      begin
        Result := 1;
      end;
    end;
  finally
    FreeAndNil(LVersionDetails);
  end;
end;

function TLZCompareBase.CompareDate(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TDate;
begin
  LValue1 := TLZDateTime.StringToDate(AValue1);
  LValue2 := TLZDateTime.StringToDate(AValue2);
  Result := System.DateUtils.CompareDate(LValue1, LValue2);
end;

function TLZCompareBase.CompareDateTime(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TDateTime;
begin
  LValue1 := TLZDateTime.StringToDateTime(AValue1);
  LValue2 := TLZDateTime.StringToDateTime(AValue2);
  Result := System.DateUtils.CompareDateTime(LValue1, LValue2);
end;

function TLZCompareBase.CompareFloat(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: Double;
begin
  LValue1 := StrToFloat(AValue1);
  LValue2 := StrToFloat(AValue2);
  Result := CompareValue(LValue1, LValue2);
end;

function TLZCompareBase.CompareInteger(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: integer;
begin
  LValue1 := StrToInt(AValue1);
  LValue2 := StrToInt(AValue2);
  Result := CompareValue(LValue1, LValue2);
end;

function TLZCompareBase.CompareTime(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TTime;
begin
  LValue1 := TLZDateTime.StringToTime(AValue1);
  LValue2 := TLZDateTime.StringToTime(AValue2);
  Result := System.DateUtils.CompareTime(LValue1, LValue2);
end;

function TLZCompareBase.GenerateMD5(AFilename: TFileName): string;
var
  LIdMD5: TIdHashMessageDigest5;
  LFileStream: TFileStream;
begin
  LIdMD5 := TIdHashMessageDigest5.Create;
  LFileStream := TFileStream.Create(AFilename, fmOpenRead OR fmShareDenyWrite);
  try
    Result := LIdMD5.HashBytesAsHex(LIdMD5.HashStream(LFileStream));
  finally
    LFileStream.Free;
    LIdMD5.Free;
  end;
end;

end.
