unit Lazy.Token;

interface

uses Classes;

const
  LZTokenDefaultSeperators: TArray<Char> = [';', ','];

type
  TLZTokenSeperators = TArray<Char>;

  TLZToken = class(TPersistent)
  private
    FTokens: TStringList;
    FSource: string;
    FSeperator: Char;
    FOutOfBoundsException: Boolean;
    FOutOfBoundsValue: string;
  protected
    procedure SetSource(AValue: string);
    procedure SetSeperator(AValue: Char);
    function GetNextToken(
      const S: string;
      Separator: Char;
      var StartPos: integer): string;
    procedure Split;
    function GetToken(AIndex: integer): string;
    procedure SetToken(
      AIndex: integer;
      AValue: string);
    function GetText: string;
    function GetCount: integer;
  public
    constructor Create(
      ASource: string;
      ASeperator: Char); reintroduce; overload;
    constructor Create(
      ASource: string;
      ASeperators: TLZTokenSeperators = []); overload;
    constructor Create; overload;
    destructor Destroy; override;
    property Tokens[AIndex: integer]: string read GetToken write SetToken;
    function TokenExists(
      AValue: string;
      ACaseSensitive: Boolean = false): Boolean;
    class function GetTokens(
      ASource: string;
      ASeperators: TLZTokenSeperators): TStrings; overload;
    class function GetTokens(ASource: string): TStrings; overload;
    property Strings: TStringList read FTokens;
    property Source: string Read FSource Write SetSource;
    property Seperator: Char Read FSeperator Write SetSeperator;
    property Text: string read GetText;
    property OutOfBoundsException: Boolean read FOutOfBoundsException
      write FOutOfBoundsException;
    property OutOfBoundsValue: string read FOutOfBoundsValue
      write FOutOfBoundsValue;
    property Count: integer read GetCount;
  end;

implementation

uses SysUtils;

constructor TLZToken.Create(
  ASource: string;
  ASeperator: Char);
begin
  inherited Create;
  FTokens := TStringList.Create;
  FTokens.Duplicates := dupAccept;
  Seperator := ASeperator;
  Source := ASource;
  FOutOfBoundsException := false;
  FOutOfBoundsValue := '';
end;

constructor TLZToken.Create;
begin
  Create('', ',');
end;

constructor TLZToken.Create(
  ASource: string;
  ASeperators: TLZTokenSeperators);
var
  LSource: String;
  LSeperator: Char;
  LSeperators: TLZTokenSeperators;
begin
  LSource := ASource;
  LSeperators := ASeperators;
  if Length(LSeperators) > 0 then
  begin
    LSeperators := LZTokenDefaultSeperators;
  end;

  FSeperator := LSeperators[0];
  for LSeperator in LSeperators do
  begin
    LSource := StringReplace(LSource, LSeperator, FSeperator,
      [rfReplaceAll, rfIgnoreCase]);
  end;

  Create(LSource, FSeperator);
end;

destructor TLZToken.Destroy;
begin
  FreeAndNil(FTokens);
  inherited Destroy;
end;

procedure TLZToken.SetSource(AValue: string);
begin
  FSource := Trim(AValue);
  if FSource <> '' then
  begin
    if FSource[Length(FSource)] = Seperator then
    begin
      // FSource := FSource + FSeperator;
      Delete(FSource, Length(FSource), 1);
    end;
    Split;
  end;
end;

procedure TLZToken.SetSeperator(AValue: Char);
begin
  FSeperator := AValue;
  Split;
end;

function TLZToken.GetNextToken(
  const S: string;
  Separator: Char;
  var StartPos: integer): string;
var
  Index: integer;
begin
  Result := '';

  { Step over repeated separators }
  // while (S[StartPos] = Separator) and (StartPos <= length(S)) do
  // StartPos := StartPos + 1;

  if (StartPos > Length(S)) or (Length(S) = 0) then
    Exit;

  { Set Index to StartPos }
  Index := StartPos;

  { Find the next Separator }
  while (S[Index] <> Separator) and (Index <= Length(S)) do
    Index := Index + 1;

  { Copy the token to the Result }
  Result := Copy(S, StartPos, Index - StartPos);

  { SetStartPos to next Character after the Separator }
  StartPos := Index + 1;
end;

procedure TLZToken.Split;
var
  // Idx: integer;
  SourceArray: TArray<string>;
  TokenStr: string;
begin
  FTokens.Clear;
  // Value := '';
  if Length(FSource) > 0 then
  begin
    // while Start <= Length(FSource) do
    // FTokens.Add(GetNextToken(FSource, FSeperator, Start));
    // for Idx := Start to Length(FSource) do
    // begin
    // if FSource[Idx] = FSeperator then
    // begin
    // FTokens.Add(Value);
    // Value := '';
    // end
    // else
    // begin
    // Value := Value + FSource[Idx];
    // end;
    // end;

    if FSource.Contains(FSeperator) then
    begin

      SourceArray := FSource.Split([FSeperator]);

      for TokenStr in SourceArray do
      begin
        FTokens.Add(TokenStr);
      end;
    end
    else
    begin
      FTokens.Add(FSource);
    end;

  end;
end;

function TLZToken.TokenExists(
  AValue: string;
  ACaseSensitive: Boolean): Boolean;
var
  Idx: integer;
begin
  Result := false;
  Idx := 0;
  while (Idx < Count) and (not Result) do
  begin
    if ACaseSensitive then
    begin
      Result := CompareStr(Tokens[Idx], AValue) = 0;
    end
    else
    begin
      Result := CompareText(Tokens[Idx], AValue) = 0;
    end;
    Inc(Idx);
  end;
end;

function TLZToken.GetToken(AIndex: integer): string;
begin
  if (AIndex >= 0) and (AIndex < FTokens.Count) then
  begin
    Result := FTokens[AIndex];
  end
  else
  begin
    if FOutOfBoundsException then
    begin
      raise ERangeError.Create(Format('Index out of bouds (%d)', [AIndex]));
    end
    else
    begin
      Result := FOutOfBoundsValue;
    end;
  end;
end;

class function TLZToken.GetTokens(ASource: string): TStrings;
begin
  Result := GetTokens(ASource, LZTokenDefaultSeperators);
end;

class function TLZToken.GetTokens(
  ASource: string;
  ASeperators: TLZTokenSeperators): TStrings;
var
  LLZToken: TLZToken;
begin
  Result := TStringList.Create;
  LLZToken := TLZToken.Create(ASource, ASeperators);
  try
    Result.Assign(LLZToken.Strings);
  finally
    FreeAndNil(LLZToken);
  end;

end;

procedure TLZToken.SetToken(
  AIndex: integer;
  AValue: string);
begin
  FTokens[AIndex] := AValue;
end;

function TLZToken.GetText: string;
begin
  Result := FTokens.Text;
end;

function TLZToken.GetCount: integer;
begin
  Result := FTokens.Count;
end;

end.
