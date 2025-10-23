unit Lazy.Model.Test;

interface

uses
  DUnitX.TestFramework, Lazy.Test.Base;

type

  [TestFixture]
  TLZUtilsBaseTests = class(TLZTestBase)
  public

    [TestCase('A', '1')]
    [TestCase('A', '2')]
    procedure LazyModel_Clone(ATest: string);

  end;

implementation

uses
  SysUtils, DateUtils, Math,
  Lazy.Utils.Base, Lazy.Types, Lazy.Model, System.JSON;

type
  TLZModelTestProperty = class(TLZObject)
  private
    FTestString: string;
    procedure SetTestString(const Value: string);
  public
    property TestString: string read FTestString write SetTestString;
  end;

  TLZModelTest = class(TLZModel)
  private
    FTestInteger: integer;
    FTestString: string;
    FTestProperty: TLZModelTestProperty;
    procedure SetTestInteger(const Value: integer);
    procedure SetTestString(const Value: string);
    procedure SetTestProperty(const Value: TLZModelTestProperty);
  protected
    procedure SetDefaults; override;
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    property TestString: string read FTestString write SetTestString;
    property TestInteger: integer read FTestInteger write SetTestInteger;
    property TestProperty: TLZModelTestProperty read FTestProperty
      write SetTestProperty;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
  end;

  TLZModelTests = class(TLZModelList<TLZModelTest>)
  protected
    function GetDefaultArrayName: string; override;
  end;

{$REGION 'Local procedures and functions'}

procedure TLZUtilsBaseTests.LazyModel_Clone(ATest: string);
var
  LModel1, LModel2: TLZModelTest;
  LModelList: TLZModelTests;
begin
  LModelList := TLZModelTests.Create;
  LModel1 := TLZModelTest.Create;
  LModel2 := nil;
  try
    if SameText(ATest, '1') then
    begin
      LModel1.TestString := ATest;
      LModel1.TestInteger := StrToInt(ATest);
      LModel1.TestProperty.TestString := ATest;
      LModel2 := TLZModelTest.CreateClone(LModel1);
      Assert.IsTrue(SameText(ATest, LModel2.TestString),
        Format('Model2 property TestString "%s" did not match "%s"',
        [LModel2.TestString, ATest]));
      Assert.IsTrue(SameValue(StrToInt(ATest), LModel2.TestInteger),
        Format('Model2 property TestString "%s" did not match "%s"',
        [LModel2.TestString, ATest]));
      Assert.IsTrue(SameText(ATest, LModel2.TestString),
        Format('Model2 property TestProperty.TestString "%s" did not match "%s"',
        [LModel2.TestProperty.TestString, ATest]));
    end;

    if SameText(ATest, '2') then
    begin
      LModelList.FromJSON
        ('{"items":[{"TestString":"Test 1"},{"TestString":"Test 2"}]}');
      Assert.IsTrue(LModelList.Count = 2, 'Model list count should be 2');

      Assert.IsTrue(SameText('Test 1', LModelList[0].TestString),
        Format('Property LModelList[0].TestString "%s" did not match "%s"',
        [LModelList[0].TestString, 'Test 1']));
    end;

  finally
    LModel1.Free;
    LModel2.Free;
    LModelList.Free;
  end;
end;

{ TLZModelTest }

procedure TLZModelTest.AfterModelCreated;
begin
  FTestProperty := TLZModelTestProperty.Create;
end;

procedure TLZModelTest.BeforeModelDestroyed;
begin
  if Assigned(FTestProperty) then
  begin
    FreeAndNil(FTestProperty);
  end;
end;

procedure TLZModelTest.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  TestString := AJSONValue.GetValue<string>('TestString', '');
  TestInteger := AJSONValue.GetValue<integer>('TestInteger', 0);
end;

procedure TLZModelTest.SetDefaults;
begin
  inherited;
  TestString := 'Hello World';
  TestInteger := 12345;
  FTestProperty.TestString := 'World Hello';
end;

procedure TLZModelTest.SetTestInteger(const Value: integer);
begin
  FTestInteger := Value;
end;

procedure TLZModelTest.SetTestProperty(const Value: TLZModelTestProperty);
begin
  FTestProperty.TestString := Value.TestString;
end;

procedure TLZModelTest.SetTestString(const Value: string);
begin
  FTestString := Value;
end;

{ TLZModelTestProperty }

procedure TLZModelTestProperty.SetTestString(const Value: string);
begin
  FTestString := Value;
end;

{ TLZModelTests }

function TLZModelTests.GetDefaultArrayName: string;
begin
  Result := 'items';
end;

end.
