unit Lazy.CryptINI;

interface

uses
  SysUtils, Variants, Classes, inifiles;

const
  CRYPTINI_DEFAULT_KEY = '9ZehKeUpvGy3';

type
  TLZCryptINI = Class(TMemIniFile)
  private
    FBusy: Boolean;
    FCryptFileName: TFileName;
    FKey: string;
    FSaveOnDestroy: Boolean;
    function CheckDirectoryExists(
      ADirectory: string;
      ACreate: Boolean): Boolean;
  protected
    function SuperCipher(const S, Key: string): string;
    procedure LoadFile(AFileName: TFileName); overload;
    procedure SaveFile(AFileName: TFileName);
  public
    constructor Create(
      const AFileName: TFileName;
      AKey: string = CRYPTINI_DEFAULT_KEY;
      ASaveOnDestroy: Boolean = False); reintroduce;
    destructor Destroy; override;
    procedure UpdateFile; override;
    procedure LoadFile; overload;
    procedure WriteStrings(
      ASection: string;
      AStrings: TStrings);
    procedure ReadStrings(
      ASection: string;
      AStrings: TStrings);
    property Key: string read FKey write FKey;
    property SaveOnDestroy: Boolean read FSaveOnDestroy write FSaveOnDestroy;
  end;

implementation

constructor TLZCryptINI.Create(
  const AFileName: TFileName;
  AKey: string;
  ASaveOnDestroy: Boolean);
begin
  inherited Create('');
  FCryptFileName := AFileName;
  FKey := AKey;
  FSaveOnDestroy := ASaveOnDestroy;
  LoadFile;
end;

destructor TLZCryptINI.Destroy;
begin
  try
    if FSaveOnDestroy then
    begin
      UpdateFile;
    end;
  finally
    inherited Destroy;
  end;
end;

function TLZCryptINI.CheckDirectoryExists(
  ADirectory: string;
  ACreate: Boolean): Boolean;
begin
  try
    if ACreate then
    begin
      if not DirectoryExists(ADirectory) then
      begin
        ForceDirectories(ADirectory);
      end;
    end;
  finally
    Result := DirectoryExists(ADirectory);
  end;
end;

procedure TLZCryptINI.UpdateFile;
begin
  if Trim(FCryptFileName) <> '' then
  begin
    SaveFile(FCryptFileName);
  end;
end;

procedure TLZCryptINI.WriteStrings(
  ASection: string;
  AStrings: TStrings);
var
  LIdx: integer;
begin
  EraseSection(ASection);
  for LIdx := 0 to Pred(AStrings.Count) do
  begin
    WriteString(ASection, IntToStr(LIdx), AStrings[LIdx]);
  end;
end;

procedure TLZCryptINI.LoadFile;
begin
  LoadFile(FCryptFileName);
end;

procedure TLZCryptINI.ReadStrings(
  ASection: string;
  AStrings: TStrings);
var
  LLines: TStringList;
  LIdx: integer;
begin
  LLines := TStringList.Create;
  try
    AStrings.Clear;
    ReadSection(ASection, LLines);
    for LIdx := 0 to Pred(LLines.Count) do
    begin
      AStrings.Add(ReadString(ASection, LLines[LIdx], ''));
    end;
  finally
    FreeAndNil(LLines);
  end;
end;

function TLZCryptINI.SuperCipher(const S, Key: string): string;
var
  i, Z: integer;
  C: char;
  Code: byte;
begin
  Result := '';
  Z := length(Key);
  if (Z > 0) and (length(S) > 0) then
    for i := 1 to length(S) do
    begin
      Code := Ord(Key[(i - 1) mod Z + 1]);
      if S[i] >= #128 then
        C := Chr(Ord(S[i]) xor (Code and $7F))
      else if S[i] >= #64 then
        C := Chr(Ord(S[i]) xor (Code and $3F))
      else if S[i] >= #32 then
        C := Chr(Ord(S[i]) xor (Code and $1F))
      else
        C := S[i];
      Result := Result + C;
    end;
end;

procedure TLZCryptINI.LoadFile(AFileName: TFileName);
var
  FileData: TStringList;
  LTextFile: TextFile;
  LLine: string;
begin
  if FBusy then
    Exit;
  if not FBusy then
  begin
    FileData := TStringList.Create;
    FBusy := True;
    try
      try
        if FileExists(AFileName) then
        begin
          AssignFile(LTextFile, AFileName);
          try
            Reset(LTextFile);
            while not Eof(LTextFile) do
            begin
              Readln(LTextFile, LLine);
              FileData.Add(SuperCipher(LLine, FKey));
            end;
          finally
            CloseFile(LTextFile);
          end;
        end;
        SetStrings(FileData);
      except
      end;
    finally
      FBusy := False;
      FreeAndNil(FileData);
    end;
  end;
end;

procedure TLZCryptINI.SaveFile(AFileName: TFileName);
var
  LFileData: TStringList;
  LTextFile: TextFile;
  Idx: integer;
begin
  if FBusy then
    Exit;
  if not FBusy then
  begin
    FBusy := True;
    LFileData := TStringList.Create;
    try
      try
        if CheckDirectoryExists(ExtractFilePath(AFileName), True) then
        begin
          AssignFile(LTextFile, AFileName);
          try
            GetStrings(LFileData);
            Rewrite(LTextFile);
            for Idx := 0 to Pred(LFileData.Count) do
            begin
              Writeln(LTextFile, SuperCipher(LFileData[Idx], FKey));
            end;
          finally
            CloseFile(LTextFile);
          end;
        end;
      except
      end;
    finally
      FBusy := False;
      FreeAndNil(LFileData);
    end;
  end;
end;

end.
