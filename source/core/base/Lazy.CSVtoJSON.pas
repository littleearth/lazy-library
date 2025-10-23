unit Lazy.CSVtoJSON;

interface

uses
  Lazy.Types, Lazy.CSVJSONUtils,
  SysUtils, Classes, JSON;

type
  TLZCSVToJSON = class(TLZObject)
  private
    FFieldNameProcessor: TLZFieldNameProcessor;
    FArrayName: string;

    function ParseCSVLine(const ALine: string): TStringList;
    function GetFieldNameCase: TLZFieldNameCase;
    procedure SetFieldNameCase(const AValue: TLZFieldNameCase);
    function GetOnFieldNameConflict: TFieldNameConflictEvent;
    procedure SetOnFieldNameConflict(const AValue: TFieldNameConflictEvent);

  public
    constructor Create;
    destructor Destroy; override;
    class function Convert(
      ACSVData: string;
      AFieldNameCase: TLZFieldNameCase = fncPascalCase;
      const AArrayName: string = ''): TJSONValue;
    function ConvertCSVToJSON(const ACSVData: string): TJSONValue;
    property FieldNameCase: TLZFieldNameCase read GetFieldNameCase
      write SetFieldNameCase;
    property OnFieldNameConflict: TFieldNameConflictEvent
      read GetOnFieldNameConflict write SetOnFieldNameConflict;
    property ArrayName: string read FArrayName write FArrayName;
  end;

implementation

{ TLZCSVToJSON }

constructor TLZCSVToJSON.Create;
begin
  inherited Create;
  FFieldNameProcessor := TLZFieldNameProcessor.Create;
  FFieldNameProcessor.FieldNameCase := fncPascalCase;
  FArrayName := '';
end;

destructor TLZCSVToJSON.Destroy;
begin
  FFieldNameProcessor.Free;
  inherited Destroy;
end;

class function TLZCSVToJSON.Convert(
  ACSVData: string;
  AFieldNameCase: TLZFieldNameCase;
  const AArrayName: string): TJSONValue;
var
  LJSON: TLZCSVToJSON;
begin
  LJSON := TLZCSVToJSON.Create;
  try
    LJSON.FieldNameCase := AFieldNameCase;
    LJSON.ArrayName := AArrayName;
    Result := LJSON.ConvertCSVToJSON(ACSVData);
  finally
    FreeAndNil(LJSON);
  end;
end;

function TLZCSVToJSON.ConvertCSVToJSON(const ACSVData: string): TJSONValue;
var
  LLines: TStringList;
  LHeaders: TStringList;
  LProcessedHeaders: TStringList;
  LFields: TStringList;
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  LRootObject: TJSONObject;
  LLineIndex, LFieldIndex: Integer;
  LFieldValue: string;
begin
  LLines := TStringList.Create;
  LProcessedHeaders := TStringList.Create;
  try
    LLines.Text := ACSVData;

    LJSONArray := TJSONArray.Create;

    // If array name is specified, wrap in an object
    if FArrayName <> '' then
    begin
      LRootObject := TJSONObject.Create;
      LRootObject.AddPair(FArrayName, LJSONArray);
      Result := LRootObject;
    end
    else
      Result := LJSONArray;

    if LLines.Count = 0 then
      Exit;

    // Parse and process headers from first line
    LHeaders := ParseCSVLine(LLines[0]);
    try
      FFieldNameProcessor.ClearFieldNames;

      // Process each header name
      for LFieldIndex := 0 to LHeaders.Count - 1 do
      begin
        LProcessedHeaders.Add(FFieldNameProcessor.ProcessFieldName
          (LHeaders[LFieldIndex]));
      end;

      // Process data rows
      for LLineIndex := 1 to LLines.Count - 1 do
      begin
        if Trim(LLines[LLineIndex]) = '' then
          Continue;

        LFields := ParseCSVLine(LLines[LLineIndex]);
        try
          LJSONObject := TJSONObject.Create;

          // Add each field as JSON property
          for LFieldIndex := 0 to LProcessedHeaders.Count - 1 do
          begin
            if LFieldIndex < LFields.Count then
              LFieldValue := LFields[LFieldIndex]
            else
              LFieldValue := '';

            LJSONObject.AddPair(LProcessedHeaders[LFieldIndex], LFieldValue);
          end;

          LJSONArray.AddElement(LJSONObject);

        finally
          LFields.Free;
        end;
      end;

    finally
      LHeaders.Free;
    end;
  finally
    LLines.Free;
    LProcessedHeaders.Free;
  end;
end;

function TLZCSVToJSON.ParseCSVLine(const ALine: string): TStringList;
begin
  Result := TLZCSVParser.ParseCSVLine(ALine, ',');
end;

function TLZCSVToJSON.GetFieldNameCase: TLZFieldNameCase;
begin
  Result := FFieldNameProcessor.FieldNameCase;
end;

procedure TLZCSVToJSON.SetFieldNameCase(const AValue: TLZFieldNameCase);
begin
  FFieldNameProcessor.FieldNameCase := AValue;
end;

function TLZCSVToJSON.GetOnFieldNameConflict: TFieldNameConflictEvent;
begin
  Result := FFieldNameProcessor.OnFieldNameConflict;
end;

procedure TLZCSVToJSON.SetOnFieldNameConflict(const AValue
  : TFieldNameConflictEvent);
begin
  FFieldNameProcessor.OnFieldNameConflict := AValue;
end;

end.
