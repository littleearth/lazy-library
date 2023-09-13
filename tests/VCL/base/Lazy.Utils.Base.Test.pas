unit Lazy.Utils.Base.Test;

interface

uses
  DUnitX.TestFramework,
  TestBaseU;

type

  [TestFixture]
  TLazyUtilsBaseTests = class(TTestBase)
  public
    // 'A', 'P', 'D', 'M', 'Y', 'H', 'N', 'S', 'Z'
    [TestCase('Only hours A', '0,   HH, 00:00:00.000')]
    [TestCase('Only hours B', '00,  HH, 00:00:00.000')]
    [TestCase('Only hours C', '07,  HH, 07:00:00.000')]
    [TestCase('Only hours D', '10,  HH, 10:00:00.000')]
    [TestCase('Only hours E', '21,  HH, 21:00:00.000')]
    [TestCase('Only hours F', '24,  HH, 00:00:00.000')]
    [TestCase('Only minutes A', '0,   NN, 00:00:00.000')]
    [TestCase('Only minutes B', '00,  NN, 00:00:00.000')]
    [TestCase('Only minutes C', '05,  NN, 00:05:00.000')]
    [TestCase('Only minutes D', '10,  NN, 00:10:00.000')]
    [TestCase('Only minutes E', '19,  NN, 00:19:00.000')]
    [TestCase('Only minutes F', '60,  NN, 00:00:00.000')]
    [TestCase('Only seconds A', '0,   SS, 00:00:00.000')]
    [TestCase('Only seconds B', '00,  SS, 00:00:00.000')]
    [TestCase('Only seconds C', '02,  SS, 00:00:02.000')]
    [TestCase('Only seconds D', '10,  SS, 00:00:10.000')]
    [TestCase('Only seconds E', '32,  SS, 00:00:32.000')]
    [TestCase('Only seconds F', '60,  SS, 00:00:00.000')]
    [TestCase('Only milliseconds A', '0,   ZZ, 00:00:00.000')]
    [TestCase('Only milliseconds B', '00,  ZZ, 00:00:00.000')]
    [TestCase('Only milliseconds C', '01,  ZZ, 00:00:00.001')]
    [TestCase('Only milliseconds D', '10,  ZZ, 00:00:00.010')]
    [TestCase('Only milliseconds E', '27,  ZZ, 00:00:00.027')]
    [TestCase('Only milliseconds F', '60,  ZZ, 00:00:00.060')]
    [TestCase('Only milliseconds G', '000, ZZZ, 00:00:00.000')]
    [TestCase('Only milliseconds H', '003, ZZZ, 00:00:00.003')]
    [TestCase('Only milliseconds I', '014, ZZZ, 00:00:00.014')]
    [TestCase('Only milliseconds J', '200, ZZZ, 00:00:00.200')]
    [TestCase('Only milliseconds K', '020, ZZZ, 00:00:00.020')]
    [TestCase('Only milliseconds L', '999, ZZZ, 00:00:00.999')]
    procedure DateTimeStrEval_ValidInput(AInputValue, AInputFormat,
      AExpectedAsStr: string);

    [TestCase('Format 1, A', '9,            9:00:00 AM')]
    [TestCase('Format 1, B', '15,           3:00:00 PM')]
    [TestCase('Format 1, C', '24,          12:00:00 AM')]
    [TestCase('Format 1, D', '0,           12:00:00 AM')]
    [TestCase('Format 1, E', '10,          10:00:00 AM')]
    [TestCase('Format 1, F', '100,          1:00:00 AM')]
    [TestCase('Format 1, G', '312,          3:12:00 AM')]
    [TestCase('Format 1, H', '1200,        12:00:00 PM')]
    [TestCase('Format 1, I', '1300,        01:00:00 PM')]
    [TestCase('Format 1, J', '1500,         3:00:00 PM')]
    [TestCase('Format 1, K', '1756,         5:56:00 PM')]
    [TestCase('Format 2, A', '00:00,       12:00:00 AM')]
    [TestCase('Format 2, B', '01:23,       01:23:00 AM')]
    [TestCase('Format 2, C', '12:00,       12:00:00 PM')]
    [TestCase('Format 2, D', '13:45,       01:45:00 PM')]
    [TestCase('Format 2, E', '24:00,       12:00:00 AM')]
    [TestCase('Format 3, A', '00:00:00,    12:00:00 AM')]
    [TestCase('Format 3, B', '01:23:00,    01:23:00 AM')]
    [TestCase('Format 3, C', '12:00:00,    12:00:00 PM')]
    [TestCase('Format 3, D', '13:45:00,    01:45:00 PM')]
    [TestCase('Format 3, E', '24:00:00,    12:00:00 AM')]
    [TestCase('Format 3, F', '05:37:12,    05:37:12 AM')]
    [TestCase('Format 3, H', '13:00:37,    01:00:37 PM')]
    [TestCase('Format 3, I', '13:00:07,    01:00:07 PM')]
    [TestCase('Format 3, J', '13:00:30,    01:00:30 PM')]
    [TestCase('Format 4, A', '1AM,         01:00:00 AM')]
    [TestCase('Format 4, B', '1PM,         01:00:00 PM')]
    [TestCase('Format 4, C', '132PM,       01:32:00 PM')]
    [TestCase('Format 4, A', '1PM,         01:00:00 PM')]
    [TestCase('Format 5, A', '00:00:00 AM, 12:00:00 AM')]
    [TestCase('Format 5, B', '00:00:00 PM, 12:00:00 AM')]
    [TestCase('Format 5, C', '1:00:00 PM,  01:00:00 PM')]
    [TestCase('Format 5, D', '01:00:00 PM, 01:00:00 PM')]
    [TestCase('Format 5, E', '10:00:00 AM, 10:00:00 AM')]
    [TestCase('Format 5, F', '10:00:00 PM, 10:00:00 PM')]
    [TestCase('Format 5, G', '12:00:00 AM, 12:00:00 AM')]
    [TestCase('Format 5, H', '12:00:00 PM, 12:00:00 PM')]
    [TestCase('Format 6, A', '1997-07-16T13:15:30+08:00,  01:15:30 PM')]
    [TestCase('Format 6, B', '2018-07-06 00:22:42,  12:22:42 AM')]
    [TestCase('Format 6, C', '15:53:00.322438,  03:53:00 PM')]
    [TestCase('Format 6, D', '1997-07-16T15:53:00.322438+08:00,  03:53:00 PM')]
    [TestCase('Format 6, E', '1997-07-16T15:53:00.322438Z,  11:53:00 PM')]
    procedure StringToTime_ValidInputDate(AInput, AExpectedAsStr: string);

    [TestCase('Format 1, A',
      '1997-07-16T15:53:00.322438+08:00,  16/07/1997 03:53:00 PM')]
    [TestCase('Format 1, B',
      '1997-07-16T15:53:00.322438Z,  16/07/1997 11:53:00 PM')]
    [TestCase('Format 1, C', '2023-08-21T00:00:00+08:00,  21/08/2023 00:00:00')]
    [TestCase('Format 2, A', '21/08/2023,  21/08/2023 00:00:00')]
    [TestCase('Format 2, B', '21/08/2023 13:53,  21/08/2023 01:53:00 PM')]
    [TestCase('Format 2, C', '2023-08-21 13:53,  21/08/2023 01:53:00 PM')]
    procedure StringToDateTime_ValidInputDate(AInput, AExpectedAsStr: string);

    [TestCase('Now, A', 'T')]
    [TestCase('Now, B', 't')]
    [TestCase('Seconds A', 'S')]
    [TestCase('Seconds B', 's')]
    [TestCase('Seconds C', 'S+0')]
    [TestCase('Seconds D', 'S-0')]
    [TestCase('Minutes A', 'M')]
    [TestCase('Minutes B', 'm')]
    [TestCase('Minutes C', 'M+0')]
    [TestCase('Minutes D', 'M-0')]
    [TestCase('Hours A', 'H')]
    [TestCase('Hours B', 'h')]
    [TestCase('Hours C', 'H+0')]
    [TestCase('Hours D', 'H-0')]
    procedure StringToTime_ValidShortcutInput_Now(AInput: string);

    [TestCase('A', 'S+1,   1')]
    [TestCase('B', 'S+5,   5')]
    [TestCase('C', 'S+17, 17')]
    [TestCase('D', 'S+60, 60')]
    procedure StringToTime_ValidShortcutInput_NowPlusSeconds(AInput,
      AToAdd: string);
    [TestCase('A', 'M+1,   1')]
    [TestCase('B', 'M+5,   5')]
    [TestCase('C', 'M+17, 17')]
    [TestCase('D', 'M+60, 60')]
    procedure StringToTime_ValidShortcutInput_NowPlusMinutes(AInput,
      AToAdd: string);
    [TestCase('A', 'H+1,   1')]
    [TestCase('B', 'H+5,   5')]
    [TestCase('C', 'H+17, 17')]
    [TestCase('D', 'H+24, 24')]
    procedure StringToTime_ValidShortcutInput_NowPlusHours(AInput,
      AToAdd: string);
    [TestCase('A', 'S-1,   1')]
    [TestCase('B', 'S-5,   5')]
    [TestCase('C', 'S-17, 17')]
    [TestCase('D', 'S-60, 60')]
    procedure StringToTime_ValidShortcutInput_NowMinusSeconds(AInput,
      AToSubtract: string);
    [TestCase('A', 'M-1,   1')]
    [TestCase('B', 'M-5,   5')]
    [TestCase('C', 'M-17, 17')]
    [TestCase('D', 'M-60, 60')]
    procedure StringToTime_ValidShortcutInput_NowMinusMinutes(AInput,
      AToSubtract: string);
    [TestCase('A', 'H-1,   1')]
    [TestCase('B', 'H-5,   5')]
    [TestCase('C', 'H-17, 17')]
    [TestCase('D', 'H-24, 24')]
    procedure StringToTime_ValidShortcutInput_NowMinusHours(AInput,
      AToSubtract: string);

    [TestCase('A', '.reg')]
    [TestCase('B', '.txt')]
    [TestCase('C', '.rdp')]
    procedure GetAssociatedApplication_Valid(AInput: string);

    [Test]
    procedure Exploratory;
  end;

implementation

uses
  SysUtils, DateUtils,
  Lazy.Utils.Base;

{$REGION 'Local procedures and functions'}

function NowMock: TDateTime;
begin
  Result := EncodeDateTime(2018, 1, 11, 14, 25, 30, 227);
end;

procedure TestStringToTime(AInput, AExpectedAsStr: string);
var
  LExpected: TTime;
  LActual: TTime;
begin
  // Arrange
  AExpectedAsStr := TrimLeft(AExpectedAsStr);
  LExpected := StrToTime(TrimLeft(AExpectedAsStr));

  // Act
  try
    LActual := Vcl.LazyWindowsU.StringToTime(AInput, False, False, 10,
      trNearest, NowMock);
  except
    on E: Exception do
      Assert.Fail('StringToTime raised exception: ' + E.Message);
  end;

  // Assert
  Assert.IsTrue(SameTime(LExpected, LActual), '"' + AInput + '" produced ' +
    TimeToStr(LActual) + ' which is not equal to expected ' + AExpectedAsStr);
end;

procedure TestStringToDateTime(AInput, AExpectedAsStr: string);
var
  LExpected: TDateTime;
  LActual: TDateTime;
  LSame: boolean;
  LFormatSettings: TFormatSettings;
begin
  // Arrange
  LFormatSettings := TFormatSettings.Create('en-AU');
  AExpectedAsStr := TrimLeft(AExpectedAsStr);
  LExpected := StrToDateTime(AExpectedAsStr, LFormatSettings);
  LExpected := RecodeMilliSecond(LExpected, 0);

  // Act
  try
    LActual := Vcl.LazyWindowsU.StringToDateTime(AInput);
  except
    on E: Exception do
      Assert.Fail('StringToDateTime raised exception: ' + E.Message);
  end;

  // Assert
  LActual := RecodeMilliSecond(LActual, 0);
  LSame := SameDateTime(LExpected, LActual);

  Assert.IsTrue(LSame, '"' + AInput + '" produced ' +
    FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', LActual) +
    ' which is not equal to expected ' +
    FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', LExpected));
end;
{$ENDREGION}

procedure TLazyUtilsBaseTests.DateTimeStrEval_ValidInput(AInputValue,
  AInputFormat, AExpectedAsStr: string);
var
  LExpected, LActual: TDateTime;
begin
  // Arrange
  AInputFormat := TrimLeft(AInputFormat);
  AExpectedAsStr := TrimLeft(AExpectedAsStr);
  LExpected := StrToDateTime(AExpectedAsStr);
  LExpected := RecodeDate(LExpected, 1, 1, 1);

  // Act
  LActual := Vcl.LazyWindowsU.DateTimeStrEval(AInputFormat, AInputValue);

  // Assert
  Assert.IsTrue(SameDateTime(LExpected, LActual),
    '"' + AInputValue + '" produced ' + DateTimeToStr(LActual) +
    ' which is not equal to expected ' + DateTimeToStr(LExpected));
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidInputDate(AInput: string;
  AExpectedAsStr: string);
begin
  TestStringToTime(AInput, AExpectedAsStr);
end;

procedure TLazyUtilsBaseTests.StringToDateTime_ValidInputDate(AInput: string;
  AExpectedAsStr: string);
begin
  TestStringToDateTime(AInput, AExpectedAsStr);
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidShortcutInput_Now(AInput: string);
begin
  TestStringToTime(AInput, TimeToStr(NowMock));
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidShortcutInput_NowPlusSeconds
  (AInput, AToAdd: string);
begin
  AToAdd := TrimLeft(AToAdd);

  TestStringToTime(AInput, TimeToStr(IncSecond(NowMock, StrToInt(AToAdd))));
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidShortcutInput_NowPlusMinutes
  (AInput, AToAdd: string);
begin
  AToAdd := TrimLeft(AToAdd);

  TestStringToTime(AInput, TimeToStr(IncMinute(NowMock, StrToInt(AToAdd))));
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidShortcutInput_NowPlusHours(AInput,
  AToAdd: string);
begin
  AToAdd := TrimLeft(AToAdd);

  TestStringToTime(AInput, TimeToStr(IncHour(NowMock, StrToInt(AToAdd))));
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidShortcutInput_NowMinusSeconds
  (AInput, AToSubtract: string);
begin
  AToSubtract := TrimLeft(AToSubtract);

  TestStringToTime(AInput, TimeToStr(IncSecond(NowMock,
    StrToInt(AToSubtract) * -1)));
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidShortcutInput_NowMinusMinutes
  (AInput, AToSubtract: string);
begin
  AToSubtract := TrimLeft(AToSubtract);

  TestStringToTime(AInput, TimeToStr(IncMinute(NowMock,
    StrToInt(AToSubtract) * -1)));
end;

procedure TLazyUtilsBaseTests.StringToTime_ValidShortcutInput_NowMinusHours
  (AInput, AToSubtract: string);
begin
  AToSubtract := TrimLeft(AToSubtract);
  TestStringToTime(AInput, TimeToStr(IncHour(NowMock,
    StrToInt(AToSubtract) * -1)));
end;

procedure TLazyUtilsBaseTests.Exploratory;
var
  LTimePre, LNowPre, LTimePost, LNowPost: TTime;
  LTimePreDbl, LNowPreDbl, LTimePostDbl, LNowPostDbl: double;
  s: string;
  LAreEqual: boolean;
begin
  LTimePre := Time;
  LNowPre := Now;
  LTimePost := IncSecond(LTimePre, 24);
  LNowPost := IncSecond(LNowPre, 24);

  LTimePreDbl := LTimePre;
  LNowPreDbl := LNowPre;
  LTimePostDbl := LNowPreDbl;
  LNowPostDbl := LNowPost;

  LAreEqual := SameTime(LTimePost, LNowPost);

  s := 'a';
end;

procedure TLazyUtilsBaseTests.GetAssociatedApplication_Valid(AInput: string);
var
  LValue: string;
begin
  try
    LValue := GetAssociatedApplication(AInput);
  except
    on E: Exception do
      Assert.Fail('GetAssociatedApplication raised exception: ' + E.Message);
  end;

  // Assert
  Assert.IsFalse(IsEmptyString(LValue),
    Format('No file association found for "%s"', [AInput]));
  Assert.IsTrue(FileExists(LValue), '"' + AInput + '" produced ' + LValue +
    ' however file does not exist.');

end;

end.
