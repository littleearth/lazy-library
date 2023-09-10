unit Lazy.Base64;

interface

uses
  SysUtils, Classes, DB;

type
  TBase64 = class(TPersistent)
  public
    function Base64ToFileName(ABase64: string; AFileName: string): boolean;
    function FileNameToBase64(AFileName: string; var ABase64: string): boolean;
    function Base64ToField(ABase64: string; AField: TField): boolean;
    function FieldToBase64(AField: TField; var ABase64: string): boolean;
  end;

implementation

uses
  IdCoderMIME, System.NetEncoding;

function TBase64.Base64ToFileName(ABase64: string; AFileName: string): boolean;
var
  IdDecoderMIME: TIdDecoderMIME;
  FileStream: TFileStream;
  StringStream : TStringStream;
begin
  if FileExists(AFileName) then
    DeleteFile(PChar(AFileName));

  IdDecoderMIME := TIdDecoderMIME.Create(nil);
  StringStream := TStringStream.Create;
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    // IdDecoderMIME.DecodeBegin(FileStream);
    // IdDecoderMIME.Decode(ABase64);
    // IdDecoderMIME.DecodeEnd;
    StringStream.WriteString(ABase64);
    StringStream.Position := 0;
    TNetEncoding.Base64.Decode(StringStream, FileStream);
    Result := FileExists(AFileName);
  finally
    FreeAndNil(IdDecoderMIME);
    FreeAndNil(FileStream);
    FreeAndNil(StringStream);
  end;
end;

function TBase64.Base64ToField(ABase64: string; AField: TField): boolean;
var
  IdDecoderMIME: TIdDecoderMIME;
  Blob: TStream;

begin
  IdDecoderMIME := TIdDecoderMIME.Create(nil);
  Blob := AField.DataSet.CreateBlobStream(AField, bmWrite);
  try
    IdDecoderMIME.DecodeBegin(Blob);
    IdDecoderMIME.Decode(ABase64);
    IdDecoderMIME.DecodeEnd;
    Result := True;
  finally
    FreeAndNil(IdDecoderMIME);
    FreeAndNil(Blob);
  end;
end;

function TBase64.FileNameToBase64(AFileName: string;
  var ABase64: string): boolean;
var
  IdEncoderMIME: TIdEncoderMIME;
  FileStream: TFileStream;
begin
  Result := False;
  if FileExists(AFileName) then
  begin
    IdEncoderMIME := TIdEncoderMIME.Create(nil);
    FileStream := TFileStream.Create(AFileName, fmOpenRead);
    try
      ABase64 := IdEncoderMIME.Encode(FileStream);
      Result := Trim(ABase64) <> '';
    finally
      FreeAndNil(IdEncoderMIME);
      FreeAndNil(FileStream);
    end;
  end;
end;

function TBase64.FieldToBase64(AField: TField; var ABase64: string): boolean;
var
  MemoryStream: TMemoryStream;
  IdEncoderMIME: TIdEncoderMIME;

  function LoadFromBlob(const AField: TField; const Stream: TStream): boolean;
  var
    Blob: TStream;
  begin
    Blob := AField.DataSet.CreateBlobStream(AField, bmRead);
    try
      Blob.Seek(0, TSeekOrigin.soBeginning);
      Stream.CopyFrom(Blob, Blob.Size);
      Result := True;
    finally
      Blob.Free
    end;
  end;

begin
  Result := False;
  IdEncoderMIME := TIdEncoderMIME.Create(nil);
  MemoryStream := TMemoryStream.Create;
  try
    if LoadFromBlob(AField, MemoryStream) then
    begin
      MemoryStream.Position := 0;
      ABase64 := IdEncoderMIME.Encode(MemoryStream);
      Result := Trim(ABase64) <> '';
    end;
  finally
    FreeAndNil(IdEncoderMIME);
    FreeAndNil(MemoryStream);
  end;
end;

end.
