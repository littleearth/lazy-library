unit Lazy.Base64;

interface

uses
  Lazy.Types, SysUtils, Classes, DB;

type
  TLZBase64 = class(TLZPersistent)
  private
    function LoadFromBlob(
      const AField: TField;
      const Stream: TStream): boolean;
  protected
    function Base64ToFileName(
      ABase64: string;
      AFileName: string): boolean;
    function FileNameToBase64(
      AFileName: string;
      var ABase64: string): boolean;
    function Base64ToField(
      ABase64: string;
      AField: TField): boolean;
    function FieldToBase64(
      AField: TField;
      var ABase64: string): boolean;
  public
    class function ToFile(
      ABase64: string;
      AFileName: string): boolean;
    class function FromFile(
      AFileName: string;
      var ABase64: string): boolean;
    class function ToField(
      ABase64: string;
      AField: TField): boolean;
    class function FromField(
      AField: TField;
      var ABase64: string): boolean;
  end;

implementation

uses
  IdCoderMIME, System.NetEncoding;

function TLZBase64.Base64ToFileName(
  ABase64: string;
  AFileName: string): boolean;
var
  IdDecoderMIME: TIdDecoderMIME;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  if FileExists(AFileName) then
    DeleteFile(PChar(AFileName));

  IdDecoderMIME := TIdDecoderMIME.Create(nil);
  StringStream := TStringStream.Create;
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
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

function TLZBase64.Base64ToField(
  ABase64: string;
  AField: TField): boolean;
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

function TLZBase64.FileNameToBase64(
  AFileName: string;
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

class function TLZBase64.FromField(
  AField: TField;
  var ABase64: string): boolean;
var
  LBase64: TLZBase64;
begin
  LBase64 := TLZBase64.Create;
  try
    Result := LBase64.FieldToBase64(AField, ABase64);
  finally
    FreeAndNil(LBase64);
  end;
end;

class function TLZBase64.FromFile(
  AFileName: string;
  var ABase64: string): boolean;
var
  LBase64: TLZBase64;
begin
  LBase64 := TLZBase64.Create;
  try
    Result := LBase64.FileNameToBase64(AFileName, ABase64);
  finally
    FreeAndNil(LBase64);
  end;
end;

function TLZBase64.LoadFromBlob(
  const AField: TField;
  const Stream: TStream): boolean;
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

class function TLZBase64.ToField(
  ABase64: string;
  AField: TField): boolean;
var
  LBase64: TLZBase64;
begin
  LBase64 := TLZBase64.Create;
  try
    Result := LBase64.Base64ToField(ABase64, AField);
  finally
    FreeAndNil(LBase64);
  end;
end;

class function TLZBase64.ToFile(ABase64, AFileName: string): boolean;
var
  LBase64: TLZBase64;
begin
  LBase64 := TLZBase64.Create;
  try
    Result := LBase64.Base64ToFileName(ABase64, AFileName);
  finally
    FreeAndNil(LBase64);
  end;
end;

function TLZBase64.FieldToBase64(
  AField: TField;
  var ABase64: string): boolean;
var
  MemoryStream: TMemoryStream;
  IdEncoderMIME: TIdEncoderMIME;
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
