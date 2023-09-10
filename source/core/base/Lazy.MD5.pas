unit Lazy.MD5;

interface

uses Classes, Windows, SysUtils, Lazy.Utils, Lazy.Types;

type
  TMD5Mode = (md5Generate, md5Compare);

  TMD5 = class(TLazyObject)
  protected
    function GetMD5Mode(AVersionCompareMode: string): TMD5Mode;
    function GetMD5FileName(AFileName: TFileName): TFileName;
  public
    function LoadMD5(AFileName: TFileName): string;
    function SaveMD5(AFileName: TFileName; AMD5: string): Boolean;
    function GenerateMD5(AFileName: TFileName): string; overload;
    function CompareMD5(AFileName: TFileName): Boolean; overload;
  end;

implementation

uses
  IdHashMessageDigest, idHash, System.Math, System.DateUtils, Lazy.Token;

function TMD5.GetMD5Mode(AVersionCompareMode: string): TMD5Mode;
begin
  Result := md5Generate;
  if SameText(AVersionCompareMode, 'COMPARE') then
    Result := md5Compare;
end;

function TMD5.LoadMD5(AFileName: TFileName): string;
var
  LFile: TStringList;
begin
  Result := '';
  if FileExists(AFileName) then
  begin
    LFile := TStringList.Create;
    try
      LFile.LoadFromFile(AFileName);
      Result := Trim(LFile.Text);
    finally
      FreeAndNil(LFile);
    end;
  end;
end;

function TMD5.SaveMD5(AFileName: TFileName; AMD5: string): Boolean;
var
  LFile: TStringList;
begin
  if FileExists(AFileName) then
    DeleteFile(AFileName);
  if not IsEmptyString(AMD5) then
  begin
    LFile := TStringList.Create;
    try
      LFile.Add(AMD5);
      LFile.SaveToFile(AFileName);
    finally
      FreeAndNil(LFile);
    end;
  end;
  Result := FileExists(AFileName);
end;

function TMD5.GetMD5FileName(AFileName: TFileName): TFileName;
begin
  Result := ChangeFileExt(AFileName, '.md5');
end;

function TMD5.GenerateMD5(AFileName: TFileName): string;
var
  LIdMD5: TIdHashMessageDigest5;
  LFileStream: TFileStream;
begin
  LIdMD5 := TIdHashMessageDigest5.Create;
  LFileStream := TFileStream.Create(AFileName, fmOpenRead OR fmShareDenyWrite);
  try
    Result := LIdMD5.HashBytesAsHex(LIdMD5.HashStream(LFileStream));
  finally
    LFileStream.Free;
    LIdMD5.Free;
  end;
end;

function TMD5.CompareMD5(AFileName: TFileName): Boolean;
var
  LMD5FileName: TFileName;
  LMD51, LMD52: string;
begin
  Result := False;
  LMD5FileName := GetMD5FileName(AFileName);
  LMD51 := LoadMD5(LMD5FileName);
  if not IsEmptyString(LMD51) then
  begin
    LMD52 := GenerateMD5(AFileName);
    Result := SameText(LMD51, LMD52);
  end;
end;

end.
