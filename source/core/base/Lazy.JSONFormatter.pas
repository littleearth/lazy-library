unit Lazy.JSONFormatter;

interface

uses
  Lazy.Types, Lazy.CSVJSONUtils,
  SysUtils, Classes, JSON, Generics.Collections;

type
  TJSONFormatMode = (jfmCompact, jfmFormatted);

  TLZJSONFormatter = class(TLZObject)
  private
    FFieldNameProcessor: TLZFieldNameProcessor;
    FArrayName: string;
    FRemoveArrayWrapper: Boolean;
    FFormatMode: TJSONFormatMode;
    FIndentSize: Integer;

  protected
    function ProcessJSONValue(AValue: TJSONValue): TJSONValue;
    function ProcessJSONObject(AObject: TJSONObject): TJSONObject;
    function ProcessJSONArray(AArray: TJSONArray): TJSONArray;
    function ExtractArrayFromJSON(AJSONValue: TJSONValue): TJSONArray;
    function WrapArrayInObject(
      AArray: TJSONArray;
      const AArrayName: string): TJSONObject;
    function FormatJSONOutput(AValue: TJSONValue): string;
    function GetFieldNameCase: TLZFieldNameCase;
    procedure SetFieldNameCase(const AValue: TLZFieldNameCase);
    function GetOnFieldNameConflict: TFieldNameConflictEvent;
    procedure SetOnFieldNameConflict(const AValue: TFieldNameConflictEvent);

  public
    constructor Create;
    destructor Destroy; override;

    class function Format(
      const AJSONString: string;
      AFieldNameCase: TLZFieldNameCase = fncPascalCase;
      const AArrayName: string = '';
      ARemoveArrayWrapper: Boolean = False;
      AFormatMode: TJSONFormatMode = jfmFormatted): string; overload;
    class function Format(
      AJSONValue: TJSONValue;
      AFieldNameCase: TLZFieldNameCase = fncPascalCase;
      const AArrayName: string = '';
      ARemoveArrayWrapper: Boolean = False;
      AFormatMode: TJSONFormatMode = jfmFormatted): string; overload;
    class function Format(
      AJSONValue: TJSONValue;
      AFormatMode: TJSONFormatMode): string; overload;

    function FormatJSON(const AJSONString: string): string; overload;
    function FormatJSON(AJSONValue: TJSONValue): string; overload;

    property FieldNameCase: TLZFieldNameCase read GetFieldNameCase
      write SetFieldNameCase;
    property OnFieldNameConflict: TFieldNameConflictEvent
      read GetOnFieldNameConflict write SetOnFieldNameConflict;
    property ArrayName: string read FArrayName write FArrayName;
    property RemoveArrayWrapper: Boolean read FRemoveArrayWrapper
      write FRemoveArrayWrapper;
    property FormatMode: TJSONFormatMode read FFormatMode write FFormatMode;
    property IndentSize: Integer read FIndentSize write FIndentSize;
  end;

implementation

{ TLZJSONFormatter }

constructor TLZJSONFormatter.Create;
begin
  inherited Create;
  FFieldNameProcessor := TLZFieldNameProcessor.Create;
  FFieldNameProcessor.FieldNameCase := fncPascalCase;
  FArrayName := '';
  FRemoveArrayWrapper := False;
  FFormatMode := jfmFormatted;
  FIndentSize := 2;
end;

destructor TLZJSONFormatter.Destroy;
begin
  FFieldNameProcessor.Free;
  inherited Destroy;
end;

class function TLZJSONFormatter.Format(
  const AJSONString: string;
  AFieldNameCase: TLZFieldNameCase;
  const AArrayName: string;
  ARemoveArrayWrapper: Boolean;
  AFormatMode: TJSONFormatMode): string;
var
  LFormatter: TLZJSONFormatter;
begin
  LFormatter := TLZJSONFormatter.Create;
  try
    LFormatter.FieldNameCase := AFieldNameCase;
    LFormatter.ArrayName := AArrayName;
    LFormatter.RemoveArrayWrapper := ARemoveArrayWrapper;
    LFormatter.FormatMode := AFormatMode;
    Result := LFormatter.FormatJSON(AJSONString);
  finally
    FreeAndNil(LFormatter);
  end;
end;

class function TLZJSONFormatter.Format(
  AJSONValue: TJSONValue;
  AFieldNameCase: TLZFieldNameCase;
  const AArrayName: string;
  ARemoveArrayWrapper: Boolean;
  AFormatMode: TJSONFormatMode): string;
var
  LFormatter: TLZJSONFormatter;
begin
  LFormatter := TLZJSONFormatter.Create;
  try
    LFormatter.FieldNameCase := AFieldNameCase;
    LFormatter.ArrayName := AArrayName;
    LFormatter.RemoveArrayWrapper := ARemoveArrayWrapper;
    LFormatter.FormatMode := AFormatMode;
    Result := LFormatter.FormatJSON(AJSONValue);
  finally
    FreeAndNil(LFormatter);
  end;
end;

class function TLZJSONFormatter.Format(
  AJSONValue: TJSONValue;
  AFormatMode: TJSONFormatMode): string;
var
  LFormatter: TLZJSONFormatter;
begin
  LFormatter := TLZJSONFormatter.Create;
  try
    LFormatter.FormatMode := AFormatMode;
    Result := LFormatter.FormatJSONOutput(AJSONValue);
  finally
    FreeAndNil(LFormatter);
  end;
end;

function TLZJSONFormatter.FormatJSON(const AJSONString: string): string;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := TJSONObject.ParseJSONValue(AJSONString);
  try
    if LJSONValue = nil then
      raise Exception.Create('Invalid JSON string');
    Result := FormatJSON(LJSONValue);
  finally
    LJSONValue.Free;
  end;
end;

function TLZJSONFormatter.FormatJSON(AJSONValue: TJSONValue): string;
var
  LProcessedValue: TJSONValue;
  LArray: TJSONArray;
  LWrappedObject: TJSONObject;
  LRemoveArrayWrapper: Boolean;
  LExtractedArray: TJSONArray;
  LArrayName: string;
begin
  // Don't clear field names globally - this was causing the numbering issue
  // FFieldNameProcessor.ClearFieldNames;

  // Process the JSON structure
  LProcessedValue := ProcessJSONValue(AJSONValue);
  try
    LArrayName := FArrayName;
    if FRemoveArrayWrapper then
      LArrayName := '';
    LRemoveArrayWrapper := FRemoveArrayWrapper or (LArrayName <> '');

    // Handle array wrapper operations
    if LRemoveArrayWrapper then
    begin
      // Try to extract array from wrapper object
      LArray := ExtractArrayFromJSON(LProcessedValue);
      if LArray <> nil then
      begin
        // Clone the array since we're changing the structure
        LArray := ProcessJSONArray(LArray);
        LProcessedValue.Free;
        LProcessedValue := LArray;
      end;
    end;

    if LArrayName <> '' then
    begin
      // Check if we need to change an existing wrapper name or add a new wrapper
      if LProcessedValue is TJSONObject then
      begin
        // JSON is already wrapped in an object - extract the array and rewrap with new name
        LExtractedArray := ExtractArrayFromJSON(LProcessedValue);
        if LExtractedArray <> nil then
        begin
          // Clone the extracted array and wrap with new name
          LArray := ProcessJSONArray(LExtractedArray);
          LProcessedValue.Free;
          LProcessedValue := WrapArrayInObject(LArray, LArrayName);
        end;
        // If object doesn't contain an array, keep as is
      end
      else if LProcessedValue is TJSONArray then
      begin
        // JSON is a raw array - wrap it with the specified name
        LWrappedObject := WrapArrayInObject(LProcessedValue as TJSONArray,
          LArrayName);
        LProcessedValue := LWrappedObject;
      end;
    end;

    Result := FormatJSONOutput(LProcessedValue);
  finally
    LProcessedValue.Free;
  end;
end;

function TLZJSONFormatter.ProcessJSONValue(AValue: TJSONValue): TJSONValue;
begin
  if AValue is TJSONObject then
    Result := ProcessJSONObject(AValue as TJSONObject)
  else if AValue is TJSONArray then
    Result := ProcessJSONArray(AValue as TJSONArray)
  else
    Result := AValue.Clone as TJSONValue;
end;

function TLZJSONFormatter.ProcessJSONObject(AObject: TJSONObject): TJSONObject;
var
  LPair: TJSONPair;
  LNewFieldName: string;
  LProcessedValue: TJSONValue;
  LLocalFieldNames: TStringList;
  LCounter: Integer;
  LTestName: string;
  LOriginalFieldName: string;
begin
  Result := TJSONObject.Create;

  // Use local field names tracking for this object only
  LLocalFieldNames := TStringList.Create;
  try
    LLocalFieldNames.Sorted := True;
    LLocalFieldNames.Duplicates := dupIgnore;

    for LPair in AObject do
    begin
      LOriginalFieldName := LPair.JsonString.Value;

      // Convert using the field name processor (now handles PascalCase preservation internally)
      LNewFieldName := FFieldNameProcessor.ConvertFieldNameCase
        (LOriginalFieldName, FFieldNameProcessor.FieldNameCase);

      // Ensure it's not empty
      if LNewFieldName = '' then
        LNewFieldName := 'Field';

      // Handle conflicts within this object only
      if LLocalFieldNames.IndexOf(LNewFieldName) >= 0 then
      begin
        LCounter := 1;
        repeat
          LTestName := LNewFieldName + IntToStr(LCounter);
          Inc(LCounter);
        until LLocalFieldNames.IndexOf(LTestName) < 0;
        LNewFieldName := LTestName;
      end;

      // Add to local tracking
      LLocalFieldNames.Add(LNewFieldName);

      // Recursively process the value
      LProcessedValue := ProcessJSONValue(LPair.JsonValue);

      Result.AddPair(LNewFieldName, LProcessedValue);
    end;
  finally
    LLocalFieldNames.Free;
  end;
end;

function TLZJSONFormatter.ProcessJSONArray(AArray: TJSONArray): TJSONArray;
var
  LIndex: Integer;
  LProcessedValue: TJSONValue;
begin
  Result := TJSONArray.Create;

  for LIndex := 0 to AArray.Count - 1 do
  begin
    LProcessedValue := ProcessJSONValue(AArray.Items[LIndex]);
    Result.AddElement(LProcessedValue);
  end;
end;

function TLZJSONFormatter.ExtractArrayFromJSON(AJSONValue: TJSONValue)
  : TJSONArray;
var
  LJSONObject: TJSONObject;
  LPair: TJSONPair;
begin
  Result := nil;

  if AJSONValue is TJSONArray then
    Result := AJSONValue as TJSONArray
  else if AJSONValue is TJSONObject then
  begin
    LJSONObject := AJSONValue as TJSONObject;
    // Look for the first array property in the object
    for LPair in LJSONObject do
    begin
      if LPair.JsonValue is TJSONArray then
      begin
        Result := LPair.JsonValue as TJSONArray;
        Break;
      end;
    end;
  end;
end;

function TLZJSONFormatter.WrapArrayInObject(
  AArray: TJSONArray;
  const AArrayName: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair(AArrayName, AArray.Clone as TJSONArray);
end;

function TLZJSONFormatter.FormatJSONOutput(AValue: TJSONValue): string;
begin
  case FFormatMode of
    jfmCompact:
      Result := AValue.ToString;
  else
    begin
      Result := AValue.Format(FIndentSize);
    end;
  end;
end;

function TLZJSONFormatter.GetFieldNameCase: TLZFieldNameCase;
begin
  Result := FFieldNameProcessor.FieldNameCase;
end;

procedure TLZJSONFormatter.SetFieldNameCase(const AValue: TLZFieldNameCase);
begin
  FFieldNameProcessor.FieldNameCase := AValue;
end;

function TLZJSONFormatter.GetOnFieldNameConflict: TFieldNameConflictEvent;
begin
  Result := FFieldNameProcessor.OnFieldNameConflict;
end;

procedure TLZJSONFormatter.SetOnFieldNameConflict(const AValue
  : TFieldNameConflictEvent);
begin
  FFieldNameProcessor.OnFieldNameConflict := AValue;
end;

end.
