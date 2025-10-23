unit Lazy.Model;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  REST.Authenticator.Basic, System.JSON, System.Generics.Collections,
  Lazy.Types, Lazy.Log, Rtti, TypInfo;

type

  TLZModel = class(TLZObject)
  protected
    procedure SetDefaults; virtual;
    procedure AfterModelCreated; virtual;
    procedure BeforeModelDestroyed; virtual;
    function JSONArrayToDelimitedString(AJSONArray: TJSONArray;
      ADelimeter: char = ','): string;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    constructor CreateClone(ASource: TLZModel); virtual;
    class function CreateModel<T: TLZModel>: T;
    procedure Clone(ASource: TLZModel);
    procedure Assign(ASource: TLZModel); virtual;
    procedure FromJSONValue(AJSONValue: TJSONValue); virtual; abstract;
    function ToJSONValue: TJSONValue; virtual; abstract;
    procedure FromJSON(AJSON: string);
    function ToJSON(AFormat: boolean = false): string;
  end;

  TLZModelList<T: TLZModel> = class(TLZObject)
  private
    FItems: TObjectList<T>;
    function GetCount: integer;
    function GetItems: TObjectList<T>;
    function GetModel(AIndex: integer): T;
  protected
    function GetDefaultArrayName: string; virtual;
  public
    constructor Create; reintroduce; virtual;
    constructor CreateClone(ASource: TLZModelList<T>); virtual;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<T>;
    procedure Assign(ASource: TLZModelList<T>); virtual;
    procedure Add(ASource: T); overload;
    function Add: T; overload;
    procedure Clear;
    procedure FromJSONValue(AJSONValue: TJSONValue; AClear: boolean = true;
      AArrayName: string = ''; AFreeJSONValue: boolean = false); virtual;
    procedure FromJSON(AJSON: string);
    function ToJSONValue(AArrayName: string = ''): TJSONValue; virtual;
    function ToJSON(AFormat: boolean = false): string; overload;
    property Count: integer read GetCount;
    property Model[AIndex: integer]: T read GetModel; default;
    property Models: TObjectList<T> read GetItems;
  end;

implementation

uses
  Lazy.Utils;

{ TDuoModelList<T> }

procedure TLZModelList<T>.Add(ASource: T);
begin
  FItems.Add(ASource);
end;

function TLZModelList<T>.Add: T;
begin
  Result := TLZModel.CreateModel<T>;
  Add(Result);
end;

procedure TLZModelList<T>.Assign(ASource: TLZModelList<T>);
var
  LSourceDevice, LDestDevice: T;
begin
  Clear;
  for LSourceDevice in ASource.Models do
  begin
    LDestDevice := T.CreateClone(LSourceDevice);
    FItems.Add(LDestDevice);
  end;
end;

procedure TLZModelList<T>.Clear;
begin
  FItems.Clear;
end;

constructor TLZModelList<T>.Create;
begin
  FItems := TObjectList<T>.Create;
end;

constructor TLZModelList<T>.CreateClone(ASource: TLZModelList<T>);
begin
  inherited Create;
  Assign(ASource);
end;

destructor TLZModelList<T>.Destroy;
begin
  try
    FreeAndNil(FItems);
  finally
    inherited;
  end;
end;

procedure TLZModelList<T>.FromJSON(AJSON: string);
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ParseJSONValue(AJSON, true, true);
  try
    FromJSONValue(LJSON);
  finally
    LJSON.Free;
  end;
end;

procedure TLZModelList<T>.FromJSONValue(AJSONValue: TJSONValue; AClear: boolean;
  AArrayName: string; AFreeJSONValue: boolean);
var
  LArray: TJSONArray;
  LIdx: integer;
  LModel: T;
  LArrayName: string;
begin
  if Assigned(AJSONValue) then
  begin
    LArrayName := AArrayName;
    if TLZString.IsEmptyString(LArrayName) then
      LArrayName := GetDefaultArrayName;
    if AClear then
      Clear;
    LArray := AJSONValue.GetValue<TJSONArray>(LArrayName, nil);
    if Assigned(LArray) then
    begin
      for LIdx := 0 to Pred(LArray.Count) do
      begin
        LModel := TLZModel.CreateModel<T>;
        LModel.FromJSONValue(LArray.Items[LIdx]);
        Add(LModel);
      end;
    end;
    if AFreeJSONValue then
    begin
      AJSONValue.Free;
    end;
  end;
end;

function TLZModelList<T>.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TLZModelList<T>.GetDefaultArrayName: string;
begin
  Result := '';
end;

function TLZModelList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FItems.GetEnumerator;
end;

function TLZModelList<T>.GetItems: TObjectList<T>;
begin
  Result := FItems;
end;

function TLZModelList<T>.GetModel(AIndex: integer): T;
begin
  Result := FItems[AIndex];
end;

function TLZModelList<T>.ToJSON(AFormat: boolean): string;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := ToJSONValue(GetDefaultArrayName);
  if Assigned(LJSONValue) then
  begin
    try
      if AFormat then
      begin
        Result := LJSONValue.Format;
      end
      else
      begin
        Result := LJSONValue.ToJSON;
      end;
    finally
      LJSONValue.Free;
    end;
  end;
end;

function TLZModelList<T>.ToJSONValue(AArrayName: string = ''): TJSONValue;
var
  LModel: T;
  LJSONValue: TJSONValue;
  LJSONArray: TJSONArray;
begin
  Result := TJSONObject.Create;
  LJSONArray := TJSONArray.Create;
  For LModel in Models do
  begin
    LJSONValue := LModel.ToJSONValue;
    if Assigned(LJSONValue) then
    begin
      LJSONArray.Add(LJSONValue as TJSONObject);
    end;
  end;
  (Result as TJSONObject).AddPair(AArrayName, LJSONArray);
end;

{ TDuoModel }

procedure TLZModel.AfterModelCreated;
begin

end;

procedure TLZModel.Assign(ASource: TLZModel);
begin
  Clone(ASource);
end;

procedure TLZModel.BeforeModelDestroyed;
begin

end;

procedure TLZModel.Clone(ASource: TLZModel);
var
  Context: TRttiContext;
  RttiType: TRttiType;
  MinVisibility: TMemberVisibility;
  Prop: TRttiProperty;
  SourceAsPointer, ResultAsPointer: Pointer;
begin
  RttiType := Context.GetType(Self.ClassType);
  Move(ASource, SourceAsPointer, SizeOf(Pointer));
  Move(Self, ResultAsPointer, SizeOf(Pointer));
  MinVisibility := mvPublic;

  for Prop in RttiType.GetProperties do
  begin
    if (Prop.Visibility >= MinVisibility) and Prop.IsReadable and Prop.IsWritable
    then
    Begin
      if not SameText(Prop.Name, 'Name') then
      begin
        Prop.SetValue(ResultAsPointer, Prop.GetValue(SourceAsPointer));
      end;
    end;
  end;
end;

constructor TLZModel.Create;
begin
  inherited Create;
  AfterModelCreated;
  SetDefaults;
end;

constructor TLZModel.CreateClone(ASource: TLZModel);
begin
  Create;
  Clone(ASource);
end;

class function TLZModel.CreateModel<T>: T;
var
  LRttiContext: TRttiContext;
  AValue: TValue;
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  Result := nil;
  rType := LRttiContext.GetType(TypeInfo(T));
  AMethCreate := rType.GetMethod('Create');

  if Assigned(AMethCreate) and rType.IsInstance then
  begin
    instanceType := rType.AsInstance;
    AValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
    Result := AValue.AsType<T>;
  end;
end;

destructor TLZModel.Destroy;
begin
  try
    BeforeModelDestroyed;
  finally
    inherited;
  end;
end;

procedure TLZModel.FromJSON(AJSON: string);
var
  LJSON: TJSONValue;
begin
  LJSON := TJSONObject.ParseJSONValue(AJSON);
  try
    FromJSONValue(LJSON);
  finally
    LJSON.Free;
  end;
end;

function TLZModel.JSONArrayToDelimitedString(AJSONArray: TJSONArray;
  ADelimeter: char): string;
var
  LJSONValue: TJSONValue;
  LValue: string;
begin
  Result := '';
  if Assigned(AJSONArray) then
  begin
    for LJSONValue in AJSONArray do
    begin
      if not TLZString.IsEmptyString(Result) then
      begin
        Result := Result + ADelimeter;
      end;
      LValue := LJSONValue.AsType<string>;
      LValue := AnsiDequotedStr(LValue, '"');
      Result := Result + LValue;
    end;
  end;
end;

procedure TLZModel.SetDefaults;
begin

end;

function TLZModel.ToJSON(AFormat: boolean): string;
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := ToJSONValue;
  if Assigned(LJSONValue) then
  begin
    try
      if AFormat then
      begin
        Result := LJSONValue.Format;
      end
      else
      begin
        Result := LJSONValue.ToJSON;
      end;
    finally
      LJSONValue.Free;
    end;
  end;
end;

end.
