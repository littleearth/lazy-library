unit Lazy.Nullable;

interface

uses Lazy.Types, Generics.Defaults, SysUtils, System.Rtti, System.JSON;

type
  TLZNullable<T> = record
  private
    FValue: T;
    FHasValue: IInterface;
    function GetValue: T; inline;
    function GetHasValue: Boolean;
  public
    constructor Create(AValue: T);
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(Default: T): T; overload;
    procedure SetValue(AValue: T);
    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue write SetValue;
    function ToString: string;

    function IsNull: Boolean;

    procedure Clear;
    class operator NotEqual(const ALeft, ARight: TLZNullable<T>): Boolean;
    class operator Equal(ALeft, ARight: TLZNullable<T>): Boolean;
    class operator Implicit(Value: TLZNullable<T>): T;
    class operator Implicit(Value: T): TLZNullable<T>;
    class operator Implicit(Value: TLZNullable<T>): string;
  end;

  TLZNullableInteger = TLZNullable<Integer>;
  TLZNullableString = TLZNullable<String>;
  TLZNullableDate = TLZNullable<Tdate>;
  TLZNullableTime = TLZNullable<TTime>;
  TLZNullableDateTime = TLZNullable<TDateTime>;
  TLZNullableDouble = TLZNullable<Double>;
  TLZNullableSingle = TLZNullable<Single>;
  TLZNullableCurrency = TLZNullable<Currency>;
  TLZNullableBoolean = TLZNullable<Boolean>;

  TTLZNullableTimeHelper = record helper for TLZNullableTime
    procedure FromString(AValue: string);
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
    function ToString: string;
  end;

  TTLZNullableDateHelper = record helper for TLZNullableDate
    procedure FromString(AValue: string);
    function ToString: string;
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
  end;

  TTLZNullableDateTimeHelper = record helper for TLZNullableDateTime
    procedure FromString(AValue: string);
    function ToString: string;
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
  end;

  TTLZNullableBooleanHelper = record helper for TLZNullableBoolean
    procedure FromString(AValue: string);
    procedure FromInteger(AValue: Integer);
    function ToString: string;
    function ToInteger: Integer;
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
  end;

  TDateTimeHelper = record helper for TDateTime
    procedure FromString(AValue: string);
    function ToString: string;
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
  end;

  TDateHelper = record helper for Tdate
    procedure FromString(AValue: string);
    function ToString: string;
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
  end;

  TTimeHelper = record helper for TTime
    procedure FromString(AValue: string);
    function ToString: string;
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
  end;

  TBooleanHelper = record helper for
    Boolean
    procedure FromInteger(AValue: Integer);
    function ToInteger: Integer;
    procedure FromString(AValue: string);
    function ToString: string;
    procedure FromJSON(AJSONValue: TJSONValue; const APath: string);
  end;

procedure SetFlagInterface(var Intf: IInterface);

implementation

uses
  Lazy.Utils;

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj)
  : HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

const
  FlagInterfaceVTable: array [0 .. 2] of Pointer = (@NopQueryInterface,
    @NopAddref, @NopRelease);

  FlagInterfaceInstance: Pointer = @FlagInterfaceVTable;

procedure SetFlagInterface(var Intf: IInterface);
begin
  Intf := IInterface(@FlagInterfaceInstance);
end;

{ TLZNullable<T> }

procedure TLZNullable<T>.Clear;
begin
  FHasValue := nil;
end;

constructor TLZNullable<T>.Create(AValue: T);
begin
  FValue := AValue;
  SetFlagInterface(FHasValue);
end;

class operator TLZNullable<T>.Equal(ALeft, ARight: TLZNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := Comparer.Equals(ALeft.Value, ARight.Value);
  end
  else
    Result := ALeft.HasValue = ARight.HasValue;
end;

function TLZNullable<T>.GetHasValue: Boolean;
begin
  Result := FHasValue <> nil;
end;

function TLZNullable<T>.GetValue: T;
begin
  if not HasValue then
    raise Exception.Create('Invalid operation, TLZNullable type has no value');
  Result := FValue;
end;

function TLZNullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default (T);
end;

function TLZNullable<T>.GetValueOrDefault(Default: T): T;
begin
  if not HasValue then
    Result := Default
  else
    Result := FValue;
end;

function TLZNullable<T>.ToString: string;
begin
  if HasValue then
  begin
    if TypeInfo(T) = TypeInfo(TDateTime) then
      Result := DateTimeToStr(PDateTime(@FValue)^)
    else if TypeInfo(T) = TypeInfo(Tdate) then
      Result := DateToStr(PDateTime(@FValue)^)
    else if TypeInfo(T) = TypeInfo(TTime) then
      Result := TimeToStr(PDateTime(@FValue)^)
    else
      Result := TValue.From<T>(FValue).ToString;
  end
  else
    Result := 'null';
end;

class operator TLZNullable<T>.Implicit(Value: TLZNullable<T>): T;
begin
  Result := Value.Value;
end;

class operator TLZNullable<T>.Implicit(Value: T): TLZNullable<T>;
begin
  Result := TLZNullable<T>.Create(Value);
end;

class operator TLZNullable<T>.Implicit(Value: TLZNullable<T>): string;
begin
  Result := Value.ToString;
end;

function TLZNullable<T>.IsNull: Boolean;
begin
  Result := not GetHasValue;
end;

class operator TLZNullable<T>.NotEqual(const ALeft,
  ARight: TLZNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := not Comparer.Equals(ALeft.Value, ARight.Value);
  end
  else
    Result := ALeft.HasValue <> ARight.HasValue;
end;

procedure TLZNullable<T>.SetValue(AValue: T);
begin
  FValue := AValue;
  SetFlagInterface(FHasValue);
end;

{ TTLZNullableTimeHelper }

procedure TTLZNullableTimeHelper.FromJSON(AJSONValue: TJSONValue;
  const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TTLZNullableTimeHelper.FromString(AValue: string);
var
  LValue: TTime;
begin
  LValue := TLZDateTime.StringToTime(AValue);
  if LValue <> 0 then
  begin
    SetValue(LValue);
  end
  else
  begin
    Clear;
  end;
end;

function TTLZNullableTimeHelper.ToString: string;
begin
  Result := '';
  if HasValue then
  begin
    Result := TLZDateTime.TimeToString(Value);
  end;
end;

{ TTLZNullableDateHelper }

procedure TTLZNullableDateHelper.FromJSON(AJSONValue: TJSONValue;
  const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TTLZNullableDateHelper.FromString(AValue: string);
var
  LValue: Tdate;
begin
  LValue := TLZDateTime.StringToDate(AValue);
  if LValue <> 0 then
  begin
    SetValue(LValue);
  end
  else
  begin
    Clear;
  end;
end;

function TTLZNullableDateHelper.ToString: string;
begin
  Result := '';
  if HasValue then
  begin
    Result := TLZDateTime.DateToString(Value);
  end;
end;

{ TTLZNullableDateTimeHelper }

procedure TTLZNullableDateTimeHelper.FromJSON(AJSONValue: TJSONValue;
  const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TTLZNullableDateTimeHelper.FromString(AValue: string);
var
  LValue: TDateTime;
begin
  LValue := TLZDateTime.StringToDateTime(AValue);
  if LValue <> 0 then
  begin
    SetValue(LValue);
  end
  else
  begin
    Clear;
  end;
end;

function TTLZNullableDateTimeHelper.ToString: string;
begin
  Result := '';
  if HasValue then
  begin
    Result := TLZDateTime.DateTimeToString(Value);
  end;
end;

{ TDateTimeHelper }

procedure TDateTimeHelper.FromJSON(AJSONValue: TJSONValue; const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TDateTimeHelper.FromString(AValue: string);
begin
  Self := TLZDateTime.StringToDateTime(AValue);
end;

function TDateTimeHelper.ToString: string;
begin
  Result := TLZDateTime.DateTimeToString(Self);
end;

{ TDateHelper }

procedure TDateHelper.FromJSON(AJSONValue: TJSONValue; const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TDateHelper.FromString(AValue: string);
begin
  Self := TLZDateTime.StringToDate(AValue);
end;

function TDateHelper.ToString: string;
begin
  Result := TLZDateTime.DateToString(Self);
end;

{ TTimeHelper }

procedure TTimeHelper.FromJSON(AJSONValue: TJSONValue; const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TTimeHelper.FromString(AValue: string);
begin
  Self := TLZDateTime.StringToTime(AValue);
end;

function TTimeHelper.ToString: string;
begin
  Result := TLZDateTime.TimeToString(Self);
end;

{ TBooleanHelper }

procedure TBooleanHelper.FromInteger(AValue: Integer);
begin
  Self := TLZBoolean.IntegerToBool(AValue);
end;

procedure TBooleanHelper.FromJSON(AJSONValue: TJSONValue; const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TBooleanHelper.FromString(AValue: string);
begin
  Self := TLZBoolean.StringToBool(AValue);
end;

function TBooleanHelper.ToInteger: Integer;
begin
  Result := TLZBoolean.BoolToInteger(Self);
end;

function TBooleanHelper.ToString: string;
begin
  Result := BoolToStr(Self, true);
end;

{ TTLZNullableBooleanHelper }

procedure TTLZNullableBooleanHelper.FromInteger(AValue: Integer);
begin
  Self := TLZBoolean.IntegerToBool(AValue);
end;

procedure TTLZNullableBooleanHelper.FromJSON(AJSONValue: TJSONValue;
  const APath: string);
var
  LValue: string;
begin
  if AJSONValue.TryGetValue<string>(APath, LValue) then
  begin
    FromString(LValue);
  end;
end;

procedure TTLZNullableBooleanHelper.FromString(AValue: string);
begin
  if not TLZString.IsEmptyString(AValue) then
  begin
    Self := TLZBoolean.StringToBool(AValue);
  end
  else
  begin
    Clear;
  end;
end;

function TTLZNullableBooleanHelper.ToInteger: Integer;
begin
  Result := -1;
  if HasValue then
  begin
    Result := TLZBoolean.BoolToInteger(Value);
  end;
end;

function TTLZNullableBooleanHelper.ToString: string;
begin
  Result := '';
  if HasValue then
  begin
    Result := BoolToStr(Value, true);
  end;
end;

end.
