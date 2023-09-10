unit Lazy.Compare;

interface

uses Classes, Windows, SysUtils, Lazy.Utils, Lazy.Types;

type
  TCompareMode = (vcString, vcInteger, vcFloat, vcDate, vcTime, vcDateTime,
    vcVersionString, vcVersionFile, vcMD5String, vcMD5File, vcGetMD5,
    vcGetVersionFile, vcGetVersionProduct);

  TCompareBase = class(TLazyObject)
  private
  protected
    function GetCompareMode(AVersionCompareMode: string): TCompareMode;
  public
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

function TCompareBase.GetCompareMode(AVersionCompareMode: string): TCompareMode;
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

function TCompareBase.CompareDate(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TDate;
begin
  LValue1 := StringToDate(AValue1);
  LValue2 := StringToDate(AValue2);
  Result := System.DateUtils.CompareDate(LValue1, LValue2);
end;

function TCompareBase.CompareDateTime(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TDateTime;
begin
  LValue1 := StringToDateTime(AValue1);
  LValue2 := StringToDateTime(AValue2);
  Result := System.DateUtils.CompareDateTime(LValue1, LValue2);
end;

function TCompareBase.CompareFloat(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: Double;
begin
  LValue1 := StrToFloat(AValue1);
  LValue2 := StrToFloat(AValue2);
  Result := CompareValue(LValue1, LValue2);
end;

function TCompareBase.CompareInteger(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: integer;
begin
  LValue1 := StrToInt(AValue1);
  LValue2 := StrToInt(AValue2);
  Result := CompareValue(LValue1, LValue2);
end;

function TCompareBase.CompareTime(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TTime;
begin
  LValue1 := StringToTime(AValue1);
  LValue2 := StringToTime(AValue2);
  Result := System.DateUtils.CompareTime(LValue1, LValue2);
end;


function TCompareBase.GenerateMD5(AFilename: TFileName): string;
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
