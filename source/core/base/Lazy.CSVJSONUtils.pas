unit Lazy.CSVJSONUtils;

interface

uses
  Lazy.Types,
  SysUtils, Classes;

type
  TLZCSVQuoteMode = (qmMinimal, qmAll, qmNonNumeric, qmNone);

  TFieldNameConflictEvent = procedure(
    ASender: TObject;
    var AFieldName: string;
    AConflicts: Boolean) of object;

  TLZDetectedCaseType = (dctUnknown, dctPascalCase, dctCamelCase, dctSnakeCase,
    dctKebabCase, dctSpaceCase, dctMixed);

  TLZFieldNameProcessor = class(TLZObject)
  private
    FFieldNameCase: TLZFieldNameCase;
    FOnFieldNameConflict: TFieldNameConflictEvent;
    FFieldNames: TStringList;

    function IsLetter(AChar: Char): Boolean;

    function DetectCaseType(const AFieldName: string): TLZDetectedCaseType;
    function NormalizeToWords(
      const AFieldName: string;
      ACaseType: TLZDetectedCaseType): string;
    function ConvertFromNormalized(
      const ANormalizedName: string;
      ATargetCase: TLZFieldNameCase): string;

  public
    constructor Create;
    destructor Destroy; override;

    function ConvertFieldNameCase(
      const AFieldName: string;
      ACaseType: TLZFieldNameCase): string;
    function ProcessFieldName(const AOriginalName: string): string;
    procedure ClearFieldNames;

    property FieldNameCase: TLZFieldNameCase read FFieldNameCase
      write FFieldNameCase;
    property OnFieldNameConflict: TFieldNameConflictEvent
      read FOnFieldNameConflict write FOnFieldNameConflict;
    property FieldNames: TStringList read FFieldNames;
  end;

  TLZCSVParser = class(TLZObject)
  public
    class function ParseCSVLine(
      const ALine: string;
      ADelimiter: Char = ','): TStringList;
  end;

  TLZCSVFormatter = class(TLZObject)
  private
    class function EscapeCSVField(
      const AValue: string;
      ADelimiter: Char;
      AQuoteChar: Char;
      AQuoteMode: TLZCSVQuoteMode = qmMinimal): string;
  public
    class function FormatCSVLine(
      AFields: TStringList;
      ADelimiter: Char = ',';
      AQuoteChar: Char = '"';
      AQuoteMode: TLZCSVQuoteMode = qmMinimal): string;
  end;

implementation

{ TLZFieldNameProcessor }

constructor TLZFieldNameProcessor.Create;
begin
  inherited Create;
  FFieldNameCase := fncPascalCase;
  FFieldNames := TStringList.Create;
  FFieldNames.Sorted := True;
  FFieldNames.Duplicates := dupIgnore;
end;

destructor TLZFieldNameProcessor.Destroy;
begin
  FFieldNames.Free;
  inherited Destroy;
end;

procedure TLZFieldNameProcessor.ClearFieldNames;
begin
  FFieldNames.Clear;
end;

function TLZFieldNameProcessor.DetectCaseType(const AFieldName: string)
  : TLZDetectedCaseType;
var
  LIndex: Integer;
  LChar: Char;
  LHasUnderscore: Boolean;
  LHasHyphen: Boolean;
  LHasSpace: Boolean;
  LHasUpperCase: Boolean;
  LHasLowerCase: Boolean;
  LStartsWithUpper: Boolean;
  LStartsWithLower: Boolean;
begin
  Result := dctUnknown;

  if Length(AFieldName) = 0 then
    Exit;

  LHasUnderscore := False;
  LHasHyphen := False;
  LHasSpace := False;
  LHasUpperCase := False;
  LHasLowerCase := False;
  LStartsWithUpper := (AFieldName[1] >= 'A') and (AFieldName[1] <= 'Z');
  LStartsWithLower := (AFieldName[1] >= 'a') and (AFieldName[1] <= 'z');

  // Scan the string to detect patterns
  for LIndex := 1 to Length(AFieldName) do
  begin
    LChar := AFieldName[LIndex];

    if LChar = '_' then
      LHasUnderscore := True
    else if LChar = '-' then
      LHasHyphen := True
    else if LChar = ' ' then
      LHasSpace := True
    else if (LChar >= 'A') and (LChar <= 'Z') then
      LHasUpperCase := True
    else if (LChar >= 'a') and (LChar <= 'z') then
      LHasLowerCase := True;
  end;

  // Determine case type based on patterns (order matters - check most specific first)
  if LHasSpace and not LHasUnderscore and not LHasHyphen then
    Result := dctSpaceCase
  else if LHasUnderscore and not LHasHyphen and not LHasSpace and not LHasUpperCase
  then
    Result := dctSnakeCase
  else if LHasHyphen and not LHasUnderscore and not LHasSpace and not LHasUpperCase
  then
    Result := dctKebabCase
  else if not LHasUnderscore and not LHasHyphen and not LHasSpace and
    LHasUpperCase and LHasLowerCase then
  begin
    if LStartsWithUpper then
      Result := dctPascalCase
    else if LStartsWithLower then
      Result := dctCamelCase
    else
      Result := dctMixed;
  end
  else if not LHasUnderscore and not LHasHyphen and not LHasSpace and
    not LHasUpperCase and LHasLowerCase then
    Result := dctSnakeCase // All lowercase, treat as snake_case
  else if not LHasUnderscore and not LHasHyphen and not LHasSpace and
    LHasUpperCase and not LHasLowerCase then
    Result := dctPascalCase // All uppercase, treat as PascalCase
  else
    Result := dctMixed;
end;

function TLZFieldNameProcessor.NormalizeToWords(
  const AFieldName: string;
  ACaseType: TLZDetectedCaseType): string;
var
  LIndex: Integer;
  LChar: Char;
  LResult: string;
  LPrevWasLower: Boolean;
begin
  LResult := '';
  LPrevWasLower := False;

  case ACaseType of
    dctSnakeCase, dctKebabCase, dctSpaceCase:
      begin
        // Convert underscores/hyphens/spaces to pipe separators
        for LIndex := 1 to Length(AFieldName) do
        begin
          LChar := AFieldName[LIndex];
          if (LChar = '_') or (LChar = '-') or (LChar = ' ') then
            LResult := LResult + '|'
          else
            LResult := LResult + LowerCase(LChar);
        end;
      end;

    dctPascalCase, dctCamelCase:
      begin
        // Split on uppercase letters (except first)
        for LIndex := 1 to Length(AFieldName) do
        begin
          LChar := AFieldName[LIndex];

          if IsLetter(LChar) then
          begin
            if (LChar >= 'A') and (LChar <= 'Z') then
            begin
              // Add separator before uppercase if previous was lowercase and not first char
              if LPrevWasLower and (LIndex > 1) then
                LResult := LResult + '|';
              LResult := LResult + LowerCase(LChar);
              LPrevWasLower := False;
            end
            else
            begin
              LResult := LResult + LChar;
              LPrevWasLower := True;
            end;
          end
          else
          begin
            LResult := LResult + LChar;
            LPrevWasLower := False;
          end;
        end;
      end;

  else
    begin
      // Unknown or mixed case - try to intelligently split
      for LIndex := 1 to Length(AFieldName) do
      begin
        LChar := AFieldName[LIndex];

        if (LChar = '_') or (LChar = '-') or (LChar = ' ') then
          LResult := LResult + '|'
        else if IsLetter(LChar) then
        begin
          if (LChar >= 'A') and (LChar <= 'Z') then
          begin
            if LPrevWasLower and (LIndex > 1) then
              LResult := LResult + '|';
            LResult := LResult + LowerCase(LChar);
            LPrevWasLower := False;
          end
          else
          begin
            LResult := LResult + LChar;
            LPrevWasLower := True;
          end;
        end
        else
          LResult := LResult + LChar;
      end;
    end;
  end;

  Result := LResult;
end;

function TLZFieldNameProcessor.ConvertFromNormalized(
  const ANormalizedName: string;
  ATargetCase: TLZFieldNameCase): string;
var
  LWords: TStringList;
  LIndex: Integer;
  LWord: string;
  LResult: string;
begin
  LWords := TStringList.Create;
  try
    // Split on pipe separator
    LWords.Delimiter := '|';
    LWords.StrictDelimiter := True;
    LWords.DelimitedText := ANormalizedName;

    LResult := '';

    case ATargetCase of
      fncSnakeCase:
        begin
          for LIndex := 0 to LWords.Count - 1 do
          begin
            if LIndex > 0 then
              LResult := LResult + '_';
            LResult := LResult + LowerCase(Trim(LWords[LIndex]));
          end;
        end;

      fncKebabCase:
        begin
          for LIndex := 0 to LWords.Count - 1 do
          begin
            if LIndex > 0 then
              LResult := LResult + '-';
            LResult := LResult + LowerCase(Trim(LWords[LIndex]));
          end;
        end;

      fncCamelCase:
        begin
          for LIndex := 0 to LWords.Count - 1 do
          begin
            LWord := Trim(LWords[LIndex]);
            if LWord <> '' then
            begin
              if LIndex = 0 then
                LResult := LResult + LowerCase(LWord)
              else
                LResult := LResult + UpCase(LWord[1]) +
                  LowerCase(Copy(LWord, 2, Length(LWord) - 1));
            end;
          end;
        end;

      fncPascalCase:
        begin
          for LIndex := 0 to LWords.Count - 1 do
          begin
            LWord := Trim(LWords[LIndex]);
            if LWord <> '' then
              LResult := LResult + UpCase(LWord[1]) +
                LowerCase(Copy(LWord, 2, Length(LWord) - 1));
          end;
        end;
    end;

    Result := LResult;
  finally
    LWords.Free;
  end;
end;

function TLZFieldNameProcessor.ConvertFieldNameCase(
  const AFieldName: string;
  ACaseType: TLZFieldNameCase): string;
var
  LDetectedCase: TLZDetectedCaseType;
  LNormalizedName: string;
begin
  // Step 1: Detect current case type
  LDetectedCase := DetectCaseType(AFieldName);

  // Step 2: Normalize to pipe-separated words
  LNormalizedName := NormalizeToWords(AFieldName, LDetectedCase);

  // Step 3: Convert to target case
  Result := ConvertFromNormalized(LNormalizedName, ACaseType);

  // Fallback if result is empty
  if Result = '' then
    Result := AFieldName;
end;

function TLZFieldNameProcessor.ProcessFieldName(const AOriginalName
  : string): string;
var
  LProcessedName: string;
  LConflicts: Boolean;
  LCounter: Integer;
  LTestName: string;
begin
  // Convert to desired case
  LProcessedName := ConvertFieldNameCase(AOriginalName, FFieldNameCase);

  // Ensure it's not empty
  if LProcessedName = '' then
    LProcessedName := 'Field';

  // Check for conflicts
  LConflicts := FFieldNames.IndexOf(LProcessedName) >= 0;

  // Fire event if assigned
  if Assigned(FOnFieldNameConflict) then
  begin
    LTestName := LProcessedName;
    FOnFieldNameConflict(Self, LTestName, LConflicts);
    LProcessedName := LTestName;
  end;

  // Handle conflicts by appending numbers
  if FFieldNames.IndexOf(LProcessedName) >= 0 then
  begin
    LCounter := 1;
    repeat
      LTestName := LProcessedName + IntToStr(LCounter);
      Inc(LCounter);
    until FFieldNames.IndexOf(LTestName) < 0;
    LProcessedName := LTestName;
  end;

  // Add to list to track conflicts
  FFieldNames.Add(LProcessedName);
  Result := LProcessedName;
end;

function TLZFieldNameProcessor.IsLetter(AChar: Char): Boolean;
begin
  Result := ((AChar >= 'A') and (AChar <= 'Z')) or
    ((AChar >= 'a') and (AChar <= 'z'));
end;

{ TLZCSVParser }

class function TLZCSVParser.ParseCSVLine(
  const ALine: string;
  ADelimiter: Char): TStringList;
var
  LResult: TStringList;
  LCurrentField: string;
  LInQuotes: Boolean;
  LCharIndex: Integer;
  LCurrentChar: Char;
begin
  LResult := TStringList.Create;
  LCurrentField := '';
  LInQuotes := False;
  LCharIndex := 1;

  while LCharIndex <= Length(ALine) do
  begin
    LCurrentChar := ALine[LCharIndex];

    case LCurrentChar of
      '"':
        begin
          if LInQuotes and (LCharIndex < Length(ALine)) and
            (ALine[LCharIndex + 1] = '"') then
          begin
            // Handle escaped quotes ("")
            LCurrentField := LCurrentField + '"';
            Inc(LCharIndex); // Skip next quote
          end
          else
            LInQuotes := not LInQuotes;
        end;
    else
      begin
        if LCurrentChar = ADelimiter then
        begin
          if not LInQuotes then
          begin
            // Remove surrounding quotes if present
            if (Length(LCurrentField) >= 2) and (LCurrentField[1] = '"') and
              (LCurrentField[Length(LCurrentField)] = '"') then
            begin
              LCurrentField := Copy(LCurrentField, 2,
                Length(LCurrentField) - 2);
            end;
            LResult.Add(LCurrentField);
            LCurrentField := '';
          end
          else
            LCurrentField := LCurrentField + LCurrentChar;
        end
        else
          LCurrentField := LCurrentField + LCurrentChar;
      end;
    end;

    Inc(LCharIndex);
  end;

  // Add the last field and remove quotes if present
  if (Length(LCurrentField) >= 2) and (LCurrentField[1] = '"') and
    (LCurrentField[Length(LCurrentField)] = '"') then
  begin
    LCurrentField := Copy(LCurrentField, 2, Length(LCurrentField) - 2);
  end;
  LResult.Add(LCurrentField);
  Result := LResult;
end;

{ TLZCSVFormatter }

class function TLZCSVFormatter.EscapeCSVField(
  const AValue: string;
  ADelimiter: Char;
  AQuoteChar: Char;
  AQuoteMode: TLZCSVQuoteMode): string;
var
  LNeedsQuoting: Boolean;
  LIsNumeric: Boolean;
  LTrimmedValue: string;
  LDummy: Double;
begin
  Result := AValue;
  LTrimmedValue := Trim(AValue);
  LNeedsQuoting := False;
  // Check if value is purely numeric
  LIsNumeric := (LTrimmedValue <> '') and TryStrToFloat(LTrimmedValue, LDummy);

  case AQuoteMode of
    qmAll:
      LNeedsQuoting := True;
    qmNonNumeric:
      LNeedsQuoting := not LIsNumeric and (LTrimmedValue <> '');
    qmMinimal:
      begin
        // Only quote when necessary
        LNeedsQuoting := (Pos(ADelimiter, Result) > 0) or
          (Pos(AQuoteChar, Result) > 0) or (Pos(#13, Result) > 0) or
          (Pos(#10, Result) > 0) or (LTrimmedValue <> AValue);
        // Has leading/trailing spaces

        // Special cases for text that could be misinterpreted
        if not LNeedsQuoting and not LIsNumeric and (LTrimmedValue <> '') then
        begin
          LNeedsQuoting := (LowerCase(LTrimmedValue) = 'true') or
            (LowerCase(LTrimmedValue) = 'false') or
            (LowerCase(LTrimmedValue) = 'null');
        end;
      end;
  end;

  if LNeedsQuoting then
  begin
    // Escape existing quote characters by doubling them
    Result := StringReplace(Result, AQuoteChar, AQuoteChar + AQuoteChar,
      [rfReplaceAll]);
    // Wrap in quotes
    Result := AQuoteChar + Result + AQuoteChar;
  end;
end;

class function TLZCSVFormatter.FormatCSVLine(
  AFields: TStringList;
  ADelimiter: Char;
  AQuoteChar: Char;
  AQuoteMode: TLZCSVQuoteMode): string;
var
  LFieldIndex: Integer;
begin
  Result := '';

  for LFieldIndex := 0 to AFields.Count - 1 do
  begin
    if LFieldIndex > 0 then
      Result := Result + ADelimiter;
    Result := Result + EscapeCSVField(AFields[LFieldIndex], ADelimiter,
      AQuoteChar, AQuoteMode);
  end;
end;

end.
