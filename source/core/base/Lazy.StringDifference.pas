unit Lazy.StringDifference;

interface

uses
  Lazy.Types,
  System.SysUtils, System.Classes, System.RegularExpressions,
  System.Generics.Collections, System.StrUtils;

type
  TLZStringDifferenceType = (dtNone, dtAdded, dtRemoved, dtModified);

  TLZStringDifferenceInfo = record
    DiffType: TLZStringDifferenceType;
    Position: Integer;
    OriginalText: string;
    ModifiedText: string;
  end;

  TLZStringDifferenceList = TList<TLZStringDifferenceInfo>;

  TLZStringDifference = class(TLZObject)
  private
    FDifferences: TLZStringDifferenceList;
    FIgnoreCase: Boolean;
    FIgnoreCRLF: Boolean;
    FIgnoreWhitespace: Boolean;

    function CleanString(const Str: string): string;
    function RemoveWhitespaceExceptQuoted(const Str: string): string;
    function CompareSubstring(const Str1, Str2: string;
      StartPos, Length: Integer): Boolean;
    procedure AddDifference(DiffType: TLZStringDifferenceType;
      Position: Integer; const Original, Modified: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Main comparison method
    function Compare(const Str1, Str2: string): Boolean;

    // Get detailed report of differences
    function GetDifferenceReport: string;

    // Clear all stored differences
    procedure Clear;

    // Properties
    property Differences: TLZStringDifferenceList read FDifferences;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase;
    property IgnoreCRLF: Boolean read FIgnoreCRLF write FIgnoreCRLF;
    property IgnoreWhitespace: Boolean read FIgnoreWhitespace
      write FIgnoreWhitespace;
  end;

implementation

constructor TLZStringDifference.Create;
begin
  inherited;
  FDifferences := TLZStringDifferenceList.Create;
  FIgnoreCase := False;
  FIgnoreCRLF := False;
  FIgnoreWhitespace := False;
end;

destructor TLZStringDifference.Destroy;
begin
  FDifferences.Free;
  inherited;
end;

function TLZStringDifference.RemoveWhitespaceExceptQuoted
  (const Str: string): string;
var
  i: Integer;
  InSingleQuote, InDoubleQuote: Boolean;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    InSingleQuote := False;
    InDoubleQuote := False;
    i := 1;

    while i <= Length(Str) do
    begin
      case Str[i] of
        '''':
          if not InDoubleQuote then
          begin
            InSingleQuote := not InSingleQuote;
            SB.Append(Str[i]);
          end
          else
            SB.Append(Str[i]);

        '"':
          if not InSingleQuote then
          begin
            InDoubleQuote := not InDoubleQuote;
            SB.Append(Str[i]);
          end
          else
            SB.Append(Str[i]);

        #9, #32: // Tab or Space
          if InSingleQuote or InDoubleQuote then
            SB.Append(Str[i]);

      else
        SB.Append(Str[i]);
      end;
      Inc(i);
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TLZStringDifference.CleanString(const Str: string): string;
begin
  Result := Str;

  if FIgnoreWhitespace then
    Result := RemoveWhitespaceExceptQuoted(Result);

  if FIgnoreCRLF then
  begin
    Result := StringReplace(Result, #13#10, '', [rfReplaceAll]);
    Result := StringReplace(Result, #13, '', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  end;

  if FIgnoreCase then
    Result := AnsiUpperCase(Result);
end;

function TLZStringDifference.CompareSubstring(const Str1, Str2: string;
  StartPos, Length: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length - 1 do
  begin
    if (StartPos + i > System.Length(Str1)) or
      (StartPos + i > System.Length(Str2)) or
      (Str1[StartPos + i] <> Str2[StartPos + i]) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TLZStringDifference.AddDifference(DiffType: TLZStringDifferenceType;
  Position: Integer; const Original, Modified: string);
var
  DiffInfo: TLZStringDifferenceInfo;
begin
  DiffInfo.DiffType := DiffType;
  DiffInfo.Position := Position;
  DiffInfo.OriginalText := Original;
  DiffInfo.ModifiedText := Modified;
  FDifferences.Add(DiffInfo);
end;

function TLZStringDifference.Compare(const Str1, Str2: string): Boolean;
var
  Clean1, Clean2: string;
  i, j, LenStr1, LenStr2: Integer;
begin
  Clear;

  Clean1 := CleanString(Str1);
  Clean2 := CleanString(Str2);

  LenStr1 := Length(Clean1);
  LenStr2 := Length(Clean2);

  i := 1;
  j := 1;

  while (i <= LenStr1) or (j <= LenStr2) do
  begin
    if (i <= LenStr1) and (j <= LenStr2) and (Clean1[i] = Clean2[j]) then
    begin
      Inc(i);
      Inc(j);
      Continue;
    end;

    // Check for potential substring match
    if (i <= LenStr1) and (j <= LenStr2) and CompareSubstring(Clean1, Clean2,
      i, 3) then
    begin
      if i <> j then
        AddDifference(dtModified, i, Copy(Clean1, i, j - i),
          Copy(Clean2, i, j - i));
      Inc(i, 3);
      Inc(j, 3);
      Continue;
    end;

    if (i <= LenStr1) and (j <= LenStr2) then
    begin
      AddDifference(dtModified, i, Clean1[i], Clean2[j]);
      Inc(i);
      Inc(j);
    end
    else if i <= LenStr1 then
    begin
      AddDifference(dtRemoved, i, Clean1[i], '');
      Inc(i);
    end
    else if j <= LenStr2 then
    begin
      AddDifference(dtAdded, j, '', Clean2[j]);
      Inc(j);
    end;
  end;

  Result := FDifferences.Count = 0;
end;

function TLZStringDifference.GetDifferenceReport: string;
var
  DiffInfo: TLZStringDifferenceInfo;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    if FDifferences.Count = 0 then
      SB.Append('No differences found.')
    else
    begin
      SB.AppendLine('Differences found:');
      for DiffInfo in FDifferences do
      begin
        case DiffInfo.DiffType of
          dtAdded:
            SB.AppendFormat('Position %d: Added "%s"',
              [DiffInfo.Position, DiffInfo.ModifiedText]);
          dtRemoved:
            SB.AppendFormat('Position %d: Removed "%s"',
              [DiffInfo.Position, DiffInfo.OriginalText]);
          dtModified:
            SB.AppendFormat('Position %d: Changed from "%s" to "%s"',
              [DiffInfo.Position, DiffInfo.OriginalText,
              DiffInfo.ModifiedText]);
        end;
        SB.AppendLine;
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TLZStringDifference.Clear;
begin
  FDifferences.Clear;
end;

end.
