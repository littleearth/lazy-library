unit Lazy.JSONtoCSV;

interface

uses
  Lazy.Types, Lazy.CSVJSONUtils,
  SysUtils, Classes, JSON, Generics.Collections;

type
  TLZJSONToCSV = class;

  TLZJSONToCSV = class(TLZObject)
  private
    FFieldNameProcessor: TLZFieldNameProcessor;
    FDelimiter: Char;
    FQuoteChar: Char;
    FQuoteMode: TLZCSVQuoteMode;
    FIncludeHeaders: Boolean;

    function JSONValueToString(AValue: TJSONValue): string;
    function ExtractArrayFromJSON(AJSONValue: TJSONValue): TJSONArray;
    procedure CollectFieldNames(
      AJSONArray: TJSONArray;
      AFieldList: TStringList);
    function GetFieldNameCase: TLZFieldNameCase;
    procedure SetFieldNameCase(const AValue: TLZFieldNameCase);
    function GetOnFieldNameConflict: TFieldNameConflictEvent;
    procedure SetOnFieldNameConflict(const AValue: TFieldNameConflictEvent);

  public
    constructor Create;
    destructor Destroy; override;
    class function Convert(
      AJSONData: TJSONValue;
      AFieldNameCase: TLZFieldNameCase = fncPascalCase;
      ADelimiter: Char = ',';
      AIncludeHeaders: Boolean = True;
      AQuoteMode: TLZCSVQuoteMode = qmMinimal): string; overload;
    class function Convert(
      const AJSONString: string;
      AFieldNameCase: TLZFieldNameCase = fncPascalCase;
      ADelimiter: Char = ',';
      AIncludeHeaders: Boolean = True;
      AQuoteMode: TLZCSVQuoteMode = qmMinimal): string; overload;
    function ConvertJSONToCSV(AJSONData: TJSONValue): string; overload;
    function ConvertJSONToCSV(const AJSONString: string): string; overload;
    property FieldNameCase: TLZFieldNameCase read GetFieldNameCase
      write SetFieldNameCase;
    property OnFieldNameConflict: TFieldNameConflictEvent
      read GetOnFieldNameConflict write SetOnFieldNameConflict;
    property Delimiter: Char read FDelimiter write FDelimiter;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property QuoteMode: TLZCSVQuoteMode read FQuoteMode write FQuoteMode;
    property IncludeHeaders: Boolean read FIncludeHeaders write FIncludeHeaders;
  end;

implementation

{ TLZJSONToCSV }

constructor TLZJSONToCSV.Create;
begin
  inherited Create;
  FFieldNameProcessor := TLZFieldNameProcessor.Create;
  FFieldNameProcessor.FieldNameCase := fncPascalCase;
  FDelimiter := ',';
  FQuoteChar := '"';
  FQuoteMode := qmMinimal;
  FIncludeHeaders := True;
end;

destructor TLZJSONToCSV.Destroy;
begin
  FFieldNameProcessor.Free;
  inherited Destroy;
end;

class function TLZJSONToCSV.Convert(
  AJSONData: TJSONValue;
  AFieldNameCase: TLZFieldNameCase;
  ADelimiter: Char;
  AIncludeHeaders: Boolean;
  AQuoteMode: TLZCSVQuoteMode): string;
var
  LJSON: TLZJSONToCSV;
begin
  LJSON := TLZJSONToCSV.Create;
  try
    LJSON.FieldNameCase := AFieldNameCase;
    LJSON.Delimiter := ADelimiter;
    LJSON.IncludeHeaders := AIncludeHeaders;
    LJSON.QuoteMode := AQuoteMode;
    Result := LJSON.ConvertJSONToCSV(AJSONData);
  finally
    FreeAndNil(LJSON);
  end;
end;

class function TLZJSONToCSV.Convert(
  const AJSONString: string;
  AFieldNameCase: TLZFieldNameCase;
  ADelimiter: Char;
  AIncludeHeaders: Boolean;
  AQuoteMode: TLZCSVQuoteMode): string;
var
  LJSON: TLZJSONToCSV;
begin
  LJSON := TLZJSONToCSV.Create;
  try
    LJSON.FieldNameCase := AFieldNameCase;
    LJSON.Delimiter := ADelimiter;
    LJSON.IncludeHeaders := AIncludeHeaders;
    LJSON.QuoteMode := AQuoteMode;
    Result := LJSON.ConvertJSONToCSV(AJSONString);
  finally
    FreeAndNil(LJSON);
  end;
end;

function TLZJSONToCSV.ConvertJSONToCSV(const AJSONString: string): string;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := TJSONObject.ParseJSONValue(AJSONString);
  try
    if LJSONValue = nil then
      raise Exception.Create('Invalid JSON string');
    Result := ConvertJSONToCSV(LJSONValue);
  finally
    LJSONValue.Free;
  end;
end;

function TLZJSONToCSV.ConvertJSONToCSV(AJSONData: TJSONValue): string;
var
  LJSONArray: TJSONArray;
  LAllFields: TStringList;
  LProcessedFields: TStringList;
  LCSVLines: TStringList;
  LCurrentLine: string;
  LRowIndex, LFieldIndex: Integer;
  LJSONObject: TJSONObject;
  LFieldValue: TJSONValue;
  LFieldName: string;
  LFieldValues: TStringList;
begin
  LJSONArray := ExtractArrayFromJSON(AJSONData);
  if LJSONArray = nil then
    raise Exception.Create('JSON data must contain an array of objects');

  LAllFields := TStringList.Create;
  LProcessedFields := TStringList.Create;
  LCSVLines := TStringList.Create;
  try
    FFieldNameProcessor.ClearFieldNames;

    // Collect all unique field names from all objects in the array
    CollectFieldNames(LJSONArray, LAllFields);

    // Process field names according to naming convention
    for LFieldIndex := 0 to LAllFields.Count - 1 do
    begin
      LProcessedFields.Add(FFieldNameProcessor.ProcessFieldName
        (LAllFields[LFieldIndex]));
    end;

    // Add header row if requested
    if FIncludeHeaders then
    begin
      LCurrentLine := TLZCSVFormatter.FormatCSVLine(LProcessedFields,
        FDelimiter, FQuoteChar, FQuoteMode);
      LCSVLines.Add(LCurrentLine);
    end;

    // Process each object in the array
    for LRowIndex := 0 to LJSONArray.Count - 1 do
    begin
      if not(LJSONArray.Items[LRowIndex] is TJSONObject) then
        Continue;

      LJSONObject := LJSONArray.Items[LRowIndex] as TJSONObject;

      // Create field values list
      LFieldValues := TStringList.Create;
      try
        // Process each field
        for LFieldIndex := 0 to LAllFields.Count - 1 do
        begin
          LFieldName := LAllFields[LFieldIndex];
          LFieldValue := LJSONObject.GetValue(LFieldName);

          if LFieldValue <> nil then
            LFieldValues.Add(JSONValueToString(LFieldValue))
          else
            LFieldValues.Add('');
        end;

        LCurrentLine := TLZCSVFormatter.FormatCSVLine(LFieldValues, FDelimiter,
          FQuoteChar, FQuoteMode);
        LCSVLines.Add(LCurrentLine);
      finally
        LFieldValues.Free;
      end;
    end;

    Result := LCSVLines.Text;

  finally
    LAllFields.Free;
    LProcessedFields.Free;
    LCSVLines.Free;
  end;
end;

function TLZJSONToCSV.ExtractArrayFromJSON(AJSONValue: TJSONValue): TJSONArray;
begin
  Result := nil;

  if AJSONValue is TJSONArray then
    Result := AJSONValue as TJSONArray
  else if AJSONValue is TJSONObject then
  begin
    // Look for the first array property in the object
    var
    LJSONObject := AJSONValue as TJSONObject;
    for var LPair in LJSONObject do
    begin
      if LPair.JsonValue is TJSONArray then
      begin
        Result := LPair.JsonValue as TJSONArray;
        Break;
      end;
    end;
  end;
end;

procedure TLZJSONToCSV.CollectFieldNames(
  AJSONArray: TJSONArray;
  AFieldList: TStringList);
var
  LRowIndex: Integer;
  LJSONObject: TJSONObject;
  LPair: TJSONPair;
begin
  AFieldList.Clear;
  AFieldList.Sorted := True;
  AFieldList.Duplicates := dupIgnore;

  for LRowIndex := 0 to AJSONArray.Count - 1 do
  begin
    if AJSONArray.Items[LRowIndex] is TJSONObject then
    begin
      LJSONObject := AJSONArray.Items[LRowIndex] as TJSONObject;
      for LPair in LJSONObject do
      begin
        AFieldList.Add(LPair.JsonString.Value);
      end;
    end;
  end;
end;

function TLZJSONToCSV.JSONValueToString(AValue: TJSONValue): string;
begin
  if AValue is TJSONString then
    Result := (AValue as TJSONString).Value
  else if AValue is TJSONNumber then
    Result := (AValue as TJSONNumber).ToString
  else if AValue is TJSONBool then
  begin
    if (AValue as TJSONBool).AsBoolean then
      Result := 'true'
    else
      Result := 'false';
  end
  else if AValue is TJSONNull then
    Result := ''
  else if (AValue is TJSONArray) or (AValue is TJSONObject) then
    // Keep arrays and objects as raw JSON
    Result := AValue.ToString
  else
    Result := AValue.ToString;
end;

function TLZJSONToCSV.GetFieldNameCase: TLZFieldNameCase;
begin
  Result := FFieldNameProcessor.FieldNameCase;
end;

procedure TLZJSONToCSV.SetFieldNameCase(const AValue: TLZFieldNameCase);
begin
  FFieldNameProcessor.FieldNameCase := AValue;
end;

function TLZJSONToCSV.GetOnFieldNameConflict: TFieldNameConflictEvent;
begin
  Result := FFieldNameProcessor.OnFieldNameConflict;
end;

procedure TLZJSONToCSV.SetOnFieldNameConflict(const AValue
  : TFieldNameConflictEvent);
begin
  FFieldNameProcessor.OnFieldNameConflict := AValue;
end;

end.
