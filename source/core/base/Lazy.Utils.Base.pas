unit Lazy.Utils.Base;

interface

uses
  Lazy.Types,
  SysUtils, Classes, Variants;

// File and Folder Routines

type
  TLazyFileBase = class(TLazyObject)
  public
    class function GetTempFolder: string;
    class function GetGUIDFileName(AFolder, APrefix,
      AExtension: string): string;
    class function GetTempFile(APrefix: string; AExtension: string = '.tmp';
      AFolder: string = ''): string;
    class function GetApplicationDir: string;
    class function CheckDirectoryExists(ADirectory: string;
      ACreate: Boolean): Boolean;
    class function IsValidFileName(AFileName: TFileName): Boolean;
    class function ValidateFileName(AFileName: TFileName): TFileName;
    class function ExtractUrlFileName(const AURL: string): string;
    class procedure QuickFileSearch(const PathName, FileName: string;
      const Recurse: Boolean; FileList: TStrings);
    class function RandomFileName(ALength: integer): string;

  end;

  TLazySystemBase = class(TLazyObject)
  public
    class function GetApplicationParameters(AParameter: string;
      var AValue: string): Boolean;
    class function GetApplicationDir: string;
  end;

  // String Routines

  TLazyStringBase = class(TLazyObject)
  public
    class function RemoveHTMLTags(const S: string): string;
    class function IsEmptyString(AValue: string): Boolean;
    class procedure ParseDelimited(const sl: TStrings; const Value: string;
      const delimiter: string);
    class function ExtractQuotedString(const S: string; Quote: char): string;
    class function StripNonAlphaNumeric(const AValue: string): string;
    class function StripNonNumeric(const AValue: string;
      AAllowDecimal: Boolean = False; AAllowNegative: Boolean = False): string;
    class function StripCharsInSet(const AValue: string;
      ACharset: TSysCharSet): string;
    class function StipNonStandard(const AValue: string): string;
    class function EncodeHTML(AValue: String): string;
    class function DecodeHTML(AValue: String): string;
    class function StripHTML(S: string): string;
    class function StripExtraSpaces(AValue: string; ARemoveTab: Boolean = False;
      ARemoveCRLF: Boolean = False): string;
    class function StringCleaner(const AValue: string;
      ARemoveTab: Boolean = False; ARemoveCRLF: Boolean = False;
      ACharset: TSysCharSet = [#0 .. #8, #11, #12, #14 .. #31,
      #127 .. #255]): string;
    class function ContainsNonAlphaNumberic(const AValue: string): Boolean;
    class function TitleCase(const AText: string;
      const ALowerCaseFirst: Boolean = True): string;
    class function GeneratePassword(ALength: integer): string;
    class function LeadingZeroes(const ANumber, ALength: integer): string;
    class function SplitStringToWords(const AString: string;
      AWords: TStrings): integer;
    class function GetNthNumber(AValue: integer): string;
    class function LeftPad(S: string; Ch: char; Len: integer): string;
    class function RightPad(S: string; Ch: char; Len: integer): string;
    class function StrMaxLen(const S: string; MaxLen: integer): string;
    class function FormatByteSize(const bytes: extended): string;
  end;

  // Date and Time Routines
  TLazyDateTimeBase = class(TLazyObject)
  protected
    class var NowFunc: TDateTimeFunc;
  public

    // Override the Now function with a mock for testing
    class procedure SetNowMock(AFunction: TDateTimeFunc);

    class function RoundTime(ADateTime: TDateTime; AMins: double;
      ARounding: TLazyTimeRounding = trNearest): TDateTime;
    class function EncodeTimeTextFromMinutes(AMinutes: integer): string;
    class function DecodeTimeTextToMinutes(ATime: string): integer;
    class function MinutesToDaysHoursMinutes(AMinutes: integer;
      AHoursPerDay: integer = 24; ALongFormat: Boolean = False): string;
    class procedure GenerateMonthList(AMonths: TStrings);
    class procedure SetTimeFormat(ATimeFormat: TLazyTimeFormat);
    class function LocalDateTimeFromUTCDateTime(const UTCDateTime: TDateTime)
      : TDateTime;
    class function DateTimeStrEval(const ADateTimeFormat: string;
      const ADateTimeStr: string): TDateTime;
    class function StringToDate(AValue: string): TDate;
    class function StringToDateDef(AValue: string; ADefault: TDate): TDate;
    class function StringToTime(AValue: string; ARounding: Boolean = False;
      ARoundUserEntries: Boolean = False; ARoundValue: integer = 10;
      ARoundingType: TLazyTimeRounding = trNearest): TTime;
    class function StringToTimeDef(AValue: string; ADefault: TTime;
      ARounding: Boolean = False; ARoundUserEntries: Boolean = False;
      ARoundValue: integer = 10;
      ARoundingType: TLazyTimeRounding = trNearest): TTime;
    class function StringToDateTime(AValue: string): TDateTime;
    class function StringToDateTimeDef(AValue: string; ADefault: TDateTime)
      : TDateTime;
    class function ConvertDoubleDigitYear(AValue: integer): integer;
    class function MinutesToMetricMinutes(AMinutes: integer): integer;
    class function MetricMinutesToMinutes(AMetricMinutes: integer): integer;
    class function GetEasterDate(AYear: integer;
      var AEaster: TDateTime): Boolean;
    class function DateTimeToString(ADateTime: TDateTime;
      ANullText: string = ''): string; overload;
    class function DateTimeToString(ADateTime: TDateTime;
      AFormatSettings: TFormatSettings; ANullText: string = '')
      : string; overload;
    class function DateToString(ADate: TDateTime;
      AFormatSettings: TFormatSettings; ANullText: string = '')
      : string; overload;
    class function DateToString(ADate: TDate; ANullText: string = '')
      : string; overload;
    class function DateToString(ADate: TDateTime; ANullText: string = '')
      : string; overload;
    class function TimeToString(ATime: TDateTime;
      AFormatSettings: TFormatSettings; ANullText: string = '')
      : string; overload;
    class function TimeToString(ATime: TTime; ANullText: string = '')
      : string; overload;
    class function TimeToString(ATime: TDateTime; ANullText: string = '')
      : string; overload;
    class function DateTimeDifference(AStartDate: TDateTime;
      AEndDate: TDateTime): string; overload;
    class function DateTimeDifference(AStartDate: TDateTime;
      AEndDate: TDateTime; var ADays: word; var AHours: word;
      var AMinutes: word; var ASeconds: word): string; overload;
    class function DurationFromMinutes(AMinutes: integer;
      AHoursMinutesOnly: Boolean = False;
      ADateTimeFormat: string = 'hh:nn'): string;
    class function DurationFromMilliseconds(AMilliseconds: Int64;
      AHoursMinutesOnly: Boolean = False;
      ADateTimeFormat: string = 'hh:nn:ss.z'): string;
    class function DurationString(AElapsed: TDateTime;
      AHoursMinutesOnly: Boolean = False;
      ADateTimeFormat: string = 'hh:nn:ss.z'): string;
    class function StringToDuration(AValue: string;
      var AHours, AMinutes: integer): Boolean;
  end;

  // Boolean Routines
  TLazyBooleanBase = class(TLazyObject)
  public
    class function BoolToInteger(ABoolean: Boolean): integer;
    class function IntegerToBool(AValue: integer): Boolean;
  end;

  // Math Routines
  TLazyMathBase = class(TLazyObject)
  public
    class function CalculatePercentage(AValue: integer;
      ATotal: integer): integer;
    class function HexToInt(S: String): LongInt;
  end;

implementation

uses
  System.DateUtils, System.StrUtils, System.IOUtils, System.VarUtils,
  Lazy.ISO8601;

class function TLazyDateTimeBase.MinutesToMetricMinutes
  (AMinutes: integer): integer;
begin
  Result := Round((AMinutes * 10) / 6);
end;

class function TLazyDateTimeBase.MetricMinutesToMinutes(AMetricMinutes
  : integer): integer;
begin
  Result := Round((AMetricMinutes / 10) * 6);
end;

class function TLazyDateTimeBase.GetEasterDate(AYear: integer;
  var AEaster: TDateTime): Boolean;
{ From Vol 3 of The Art of Computer Programming, Donlad E. Knuth }
var
  golden, century: integer;
  Correction1, Correction2: integer;
  Sunday, Epact: integer;
  N: integer;
begin
  golden := AYear mod 19 + 1;
  century := AYear div 100 + 1;
  Correction1 := trunc(3.0 * century / 4.0) - 12;
  Correction2 := trunc((8.0 * century + 5.0) / 25.0) - 5;
  Sunday := trunc(5.0 * AYear / 4.0) - Correction1 - 10;
  Epact := (11 * golden + 20 + Correction2 - Correction1) mod 30;
  IF ((Epact = 25) and (golden > 11)) or (Epact = 24) then
    Epact := Epact + 1;
  N := 44 - Epact;
  if N < 21 then
    N := N + 30;
  N := N + 7 - ((Sunday + N) mod 7);
  try
    { Pass a Year, Month, and Day and get a date back }
    AEaster := encodedate(AYear, 3, 1) + N - 1;
    Result := True;
  except
    Result := False;
    AEaster := 0;
  end;
end;

class function TLazyDateTimeBase.DateTimeToString(ADateTime: TDateTime;
  AFormatSettings: TFormatSettings; ANullText: string = ''): string;
begin
  if ADateTime = 0 then
  begin
    Result := ANullText;
  end
  else
  begin
    Result := DateTimeToStr(ADateTime, AFormatSettings);
  end;
end;

class function TLazyDateTimeBase.DateTimeToString(ADateTime: TDateTime;
  ANullText: string = ''): string;
begin
  Result := DateTimeToString(ADateTime, FormatSettings, ANullText);
end;

class function TLazyDateTimeBase.DateToString(ADate: TDateTime;
  AFormatSettings: TFormatSettings; ANullText: string = ''): string;
begin
  if ADate = 0 then
  begin
    Result := ANullText;
  end
  else
  begin
    Result := DateToStr(ADate, AFormatSettings);
  end;
end;

class function TLazyDateTimeBase.DateToString(ADate: TDate;
  ANullText: string = ''): string;
begin
  Result := DateToString(ADate, FormatSettings, ANullText);
end;

class function TLazyDateTimeBase.DateToString(ADate: TDateTime;
  ANullText: string = ''): string;
begin
  Result := DateToString(ADate, FormatSettings, ANullText);
end;

class function TLazyDateTimeBase.TimeToString(ATime: TDateTime;
  AFormatSettings: TFormatSettings; ANullText: string = ''): string;
begin
  if ATime = 0 then
  begin
    Result := ANullText;
  end
  else
  begin
    Result := DateToStr(ATime, AFormatSettings);
  end;
end;

class function TLazyDateTimeBase.TimeToString(ATime: TTime;
  ANullText: string = ''): string;
begin
  Result := TimeToString(ATime, FormatSettings, ANullText);
end;

class function TLazyDateTimeBase.TimeToString(ATime: TDateTime;
  ANullText: string = ''): string;
begin
  Result := TimeToString(ATime, FormatSettings, ANullText);
end;

class function TLazyDateTimeBase.DateTimeDifference(AStartDate: TDateTime;
  AEndDate: TDateTime): string;
var
  Days, Hours, Mins, Secs: word;
begin
  Result := DateTimeDifference(AStartDate, AEndDate, Days, Hours, Mins, Secs);
end;

class function TLazyDateTimeBase.DateTimeDifference(AStartDate: TDateTime;
  AEndDate: TDateTime; var ADays: word; var AHours: word; var AMinutes: word;
  var ASeconds: word): string;
begin
  ADays := DaysBetween(AStartDate, AEndDate);
  AHours := HoursBetween(AEndDate, AStartDate) mod 24; // Remove total days
  AMinutes := MinutesBetween(AStartDate, AEndDate) mod 60;
  ASeconds := SecondsBetween(AStartDate, AEndDate) mod 60;
  Result := Format('%d days, %d hours, %d min, %d secs',
    [ADays, AHours, AMinutes, ASeconds]);
end;

class function TLazyDateTimeBase.DurationString(AElapsed: TDateTime;
  AHoursMinutesOnly: Boolean; ADateTimeFormat: string): string;
var
  LDays, LHours, LMinutes: integer;
begin
  if AHoursMinutesOnly then
  begin
    LDays := trunc(AElapsed);
    LHours := HourOf(AElapsed) + (LDays * 24);
    LMinutes := MinuteOf(AElapsed);
    Result := Format('%.2d:%.2d', [LHours, LMinutes]);
  end
  else
  begin
    Result := Format('%d days, %s',
      [trunc(AElapsed), FormatDateTime(ADateTimeFormat, Frac(AElapsed))]);
  end;
end;

class function TLazyDateTimeBase.DurationFromMinutes(AMinutes: integer;
  AHoursMinutesOnly: Boolean; ADateTimeFormat: string): string;
var
  LElapsed: TDateTime;
begin
  LElapsed := AMinutes / SecsPerDay;
  Result := DurationString(LElapsed, AHoursMinutesOnly, ADateTimeFormat);
end;

class function TLazyDateTimeBase.DurationFromMilliseconds(AMilliseconds: Int64;
  AHoursMinutesOnly: Boolean; ADateTimeFormat: string): string;
var
  LElapsed: TDateTime;
begin
  LElapsed := AMilliseconds / MSecsPerSec / SecsPerDay;
  Result := DurationString(LElapsed, AHoursMinutesOnly, ADateTimeFormat);
end;

class function TLazyDateTimeBase.StringToDuration(AValue: string;
  var AHours, AMinutes: integer): Boolean;
var
  LDurationStr: string;
begin
  Result := True;
  AMinutes := 0;
  AHours := 0;
  LDurationStr := Trim(AValue);
  try
    if not TLazyStringBase.IsEmptyString(LDurationStr) then
    begin
      LDurationStr := StringReplace(LDurationStr, '.', ':', [rfReplaceAll]);
      if Pos(':', LDurationStr) = 0 then
      begin
        AHours := StrToInt(LDurationStr);
        AMinutes := 0;
      end
      else
      begin
        if Pos(':', LDurationStr) = 1 then
        begin
          AHours := 0;
          AMinutes := StrToInt(Copy(LDurationStr, Pos(':', LDurationStr) + 1,
            length(LDurationStr)));
        end
        else
        begin
          AHours := StrToInt(Copy(LDurationStr, 1, Pos(':', LDurationStr) - 1));
          AMinutes := StrToInt(Copy(LDurationStr, Pos(':', LDurationStr) + 1,
            length(LDurationStr)));
        end;
      end;
    end;
  except
    Result := False;
  end;

end;

class function TLazyDateTimeBase.RoundTime(ADateTime: TDateTime; AMins: double;
  ARounding: TLazyTimeRounding = trNearest): TDateTime;
var
  MinsAsFraction: double;
begin
  Result := ADateTime;
  MinsAsFraction := 1440 / AMins;
  case ARounding of
    trNearest:
      Result := Round(ADateTime * MinsAsFraction) / MinsAsFraction;
    trUp:
      Result := (trunc(ADateTime * MinsAsFraction) / MinsAsFraction) +
        (AMins / 1440);
    trDown:
      Result := trunc(ADateTime * MinsAsFraction) / MinsAsFraction;
  end;
end;

class function TLazyFileBase.ExtractUrlFileName(const AURL: string): string;
var
  i: integer;
begin
  i := LastDelimiter('/', AURL);
  Result := Copy(AURL, i + 1, length(AURL) - (i));
end;

class function TLazyFileBase.GetTempFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;

class function TLazyFileBase.RandomFileName(ALength: integer): string;
var
  str: string;
begin
  Randomize;
  str := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  Result := '';
  repeat
    Result := Result + str[Random(length(str)) + 1];
  until (length(Result) = ALength);
end;

class function TLazyFileBase.GetTempFile(APrefix: string;
  AExtension: string = '.tmp'; AFolder: string = ''): string;
var
  Attempts: Cardinal;
  Folder: string;
begin
  Attempts := 0;
  Folder := AFolder;
  if TLazyStringBase.IsEmptyString(Folder) then
    Folder := GetTempFolder;
  repeat
    Result := IncludeTrailingPathDelimiter(Folder) + Copy(APrefix, 1, 3) +
      RandomFileName(5) + AExtension;
    Inc(Attempts);
  until (not FileExists(Result)) or (Attempts >= High(Cardinal));
  if (Attempts >= High(Cardinal)) then
  begin
    raise Exception.Create('Failed to generate temporary file.');
  end;
end;

class function TLazyFileBase.GetGUIDFileName(AFolder, APrefix,
  AExtension: string): string;
var
  FileName: string;
  Guid: TGUID;
  Attempts: integer;
begin
  Result := '';
  Attempts := 0;
  repeat
    FileName := '';
    CreateGUID(Guid);
    FileName := IncludeTrailingPathDelimiter(AFolder) + APrefix +
      GUIDtoString(Guid);
    Result := ChangeFileExt(FileName, AExtension);
    Inc(Attempts);
  until (not FileExists(Result)) or (Attempts >= High(Attempts));
  if (Attempts >= High(Attempts)) then
  begin
    raise Exception.Create('Failed to generate GUID file.');
  end;
end;

class function TLazySystemBase.GetApplicationDir: string;
begin
  Result := TLazyFileBase.GetApplicationDir;
end;

class function TLazySystemBase.GetApplicationParameters(AParameter: string;
  var AValue: string): Boolean;
var
  LParamIdx: integer;
  LParameter: string;
begin
  Result := False;
  LParamIdx := 1;
  While (LParamIdx <= ParamCount) and (not Result) do
  begin
    try
      LParameter := ParamStr(LParamIdx);
      if Pos(UpperCase(AParameter), UpperCase(LParameter)) = 1 then
      begin
        AValue := LParameter;
        AValue := StringReplace(AValue, AParameter + ':', '',
          [rfReplaceAll, rfIgnoreCase]);
        AValue := StringReplace(AValue, AParameter, '',
          [rfReplaceAll, rfIgnoreCase]);
        AValue := AnsiDequotedStr(AValue, '"');
        Result := True;
      end;
    finally
      Inc(LParamIdx);
    end;
  end;
end;

class function TLazyFileBase.GetApplicationDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

class function TLazyFileBase.CheckDirectoryExists(ADirectory: string;
  ACreate: Boolean): Boolean;
begin
  try
    if ACreate then
    begin
      if not DirectoryExists(ADirectory) then
      begin
        ForceDirectories(ADirectory);
      end;
    end;
  finally
    Result := DirectoryExists(ADirectory);
  end;
end;

class function TLazyFileBase.IsValidFileName(AFileName: TFileName): Boolean;
var
  TestFile: TextFile;
begin
  Result := False;
  if Trim(AFileName) <> '' then
  begin
    if not FileExists(AFileName) then
    begin
{$I-}
      AssignFile(TestFile, AFileName);
      Rewrite(TestFile);
      CloseFile(TestFile);
{$I+}
      Result := IOResult = 0;
      if FileExists(AFileName) then
        DeleteFile(AFileName);
    end
    else
    begin
      Result := True;
    end;
  end;
end;

class function TLazyFileBase.ValidateFileName(AFileName: TFileName): TFileName;
begin
  Result := AFileName;
  Result := TLazyStringBase.StripExtraSpaces(Result, True, True);
  Result := TLazyStringBase.StripCharsInSet(Result,
    ['\', '/', ':', '*', '?', '"', '<', '>', '|']);
end;

class procedure TLazyFileBase.QuickFileSearch(const PathName, FileName: string;
  const Recurse: Boolean; FileList: TStrings);
var
  Rec: TSearchRec;
  path: string;
  Cancel: Boolean;
begin
  path := IncludeTrailingPathDelimiter(PathName);
  Cancel := False;
  if FindFirst(path + FileName, faAnyFile, Rec) = 0 then
    try
      repeat
        if (Rec.Name <> '.') and (Rec.Name <> '..') then
        begin
          FileList.Add(path + Rec.Name);
        end;
      until (FindNext(Rec) <> 0) or (Cancel = True);
    finally
      FindClose(Rec);
    end;

  if (Recurse) and (Cancel = False) then
  begin
    if FindFirst(path + '*', faDirectory, Rec) = 0 then
      try
        repeat
          if ((Rec.Attr and faDirectory) = faDirectory) and (Rec.Name <> '.')
            and (Rec.Name <> '..') then
          begin
            TLazyFileBase.QuickFileSearch(path + Rec.Name, FileName, True,
              FileList);
          end;
        until (FindNext(Rec) <> 0) or (Cancel = True);
      finally
        FindClose(Rec);
      end;
  end;
end;

class function TLazyStringBase.ContainsNonAlphaNumberic
  (const AValue: string): Boolean;
var
  i: integer;
begin
  Result := False;
  i := 1;
  while (i <= length(AValue)) and (not Result) do
  begin
    if not CharInSet(AValue[i], ['A' .. 'Z', 'a' .. 'z', '0' .. '9']) then
    begin
      Result := True;
    end;
    Inc(i);
  end;
end;

class function TLazyStringBase.StripNonAlphaNumeric(const AValue
  : string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(AValue) do
  begin
    if CharInSet(AValue[i], ['A' .. 'Z', 'a' .. 'z', '0' .. '9']) then
      Result := Result + AValue[i];
  end;
  Result := Trim(Result);
end;

class function TLazyStringBase.StripNonNumeric(const AValue: string;
  AAllowDecimal: Boolean = False; AAllowNegative: Boolean = False): string;
var
  i: integer;
begin
  Result := '';
  if Trim(AValue) <> '' then
  begin

    for i := 1 to length(AValue) do
    begin
      if (CharInSet(AValue[i], ['0' .. '9'])) or
        ((AAllowDecimal) and (AValue[i] = '.')) or
        ((AAllowNegative) and (AValue[i] = '-')) then
      begin
        Result := Result + AValue[i];
      end;
    end;
  end;
  Result := Trim(Result);
end;

class function TLazyStringBase.StripCharsInSet(const AValue: string;
  ACharset: TSysCharSet): string;
var
  i: integer;
begin
  for i := 1 to length(AValue) do
  begin
    if not CharInSet(AValue[i], ACharset) then
      Result := Result + AValue[i];
  end;
end;

class function TLazyStringBase.StipNonStandard(const AValue: string): string;
begin
  Result := StripCharsInSet(AValue, [#0 .. #9, #11, #12, #14 .. #31,
    #127 .. #255]);
end;

class function TLazyStringBase.RemoveHTMLTags(const S: string): string;
var
  i: integer;
  InTag: Boolean;
begin
  Result := '';
  InTag := False;
  for i := 1 to length(S) do
  begin
    if S[i] = '<' then
      InTag := True
    else if S[i] = '>' then
      InTag := False
    else if not InTag then
      Result := Result + S[i];
  end;
end;

class function TLazyStringBase.EncodeHTML(AValue: String): string;
begin
  Result := AValue;
  Result := ReplaceStr(Result, '&', '&amp;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
  Result := ReplaceStr(Result, chr(39), '&apos;');
  Result := ReplaceStr(Result, '"', '&quot;');
  Result := ReplaceStr(Result, #13, '#0013'); // CR
  Result := ReplaceStr(Result, #10, '#0010'); // LF

end;

class function TLazyStringBase.DecodeHTML(AValue: String): string;
begin
  Result := AValue;
  Result := ReplaceStr(Result, '&lt;', '<');
  Result := ReplaceStr(Result, '&gt;', '>');
  Result := ReplaceStr(Result, '&apos;', chr(39));
  Result := ReplaceStr(Result, '&quot;', '"');
  Result := ReplaceStr(Result, '&amp;', '&');
  Result := ReplaceStr(Result, '&#10;', #10);
  Result := ReplaceStr(Result, '&#13;', #13);
  Result := ReplaceStr(Result, '&#xA;', #10);
  Result := ReplaceStr(Result, '&#xD;', #13);
  Result := ReplaceStr(Result, '#0010', #10);
  Result := ReplaceStr(Result, '#0013', #13);
  Result := ReplaceStr(Result, '&nbsp;', ' ');
end;

class function TLazyStringBase.StripHTML(S: string): string;
begin
  Result := RemoveHTMLTags(S);
  Result := DecodeHTML(Result);
  Result := Trim(Result);
end;

class function TLazyStringBase.StripExtraSpaces(AValue: string;
  ARemoveTab: Boolean = False; ARemoveCRLF: Boolean = False): string;
var
  i: integer;
  Source: string;
begin
  Source := Trim(AValue);

  Source := StringReplace(Source, #160, ' ', [rfReplaceAll]);

  if ARemoveTab then
    Source := StringReplace(Source, #9, ' ', [rfReplaceAll]);
  if ARemoveCRLF then
  begin
    Source := StringReplace(Source, #10, ' ', [rfReplaceAll]);
    Source := StringReplace(Source, #13, ' ', [rfReplaceAll]);
  end;

  if length(Source) > 1 then
  begin
    Result := Source[1];
    for i := 2 to length(Source) do
    begin
      if Source[i] = ' ' then
      begin
        if not(Source[i - 1] = ' ') then
          Result := Result + ' ';
      end
      else
      begin
        Result := Result + Source[i];
      end;
    end;
  end
  else
  begin
    Result := Source;
  end;
  Result := Trim(Result);
end;

class function TLazyStringBase.StringCleaner(const AValue: string;
  ARemoveTab: Boolean = False; ARemoveCRLF: Boolean = False;
  ACharset: TSysCharSet = [#0 .. #8, #11, #12, #14 .. #31,
  #127 .. #255]): string;
begin
  Result := AValue;
  Result := StripCharsInSet(Result, ACharset);
  Result := StripExtraSpaces(Result, ARemoveTab, ARemoveCRLF);
end;

class function TLazyStringBase.TitleCase(const AText: string;
  const ALowerCaseFirst: Boolean = True): string;
const
  cDelimiters = [#9, #10, #13, ' ', ',', '.', ':', ';', '"', '\', '/', '(', ')',
    '[', ']', '{', '}'];
var
  iLoop: integer;
begin
  Result := AText;
  if (Result <> '') then
  begin
    if (ALowerCaseFirst) then
      Result := LowerCase(Result);

    Result[1] := UpCase(Result[1]);
    for iLoop := 2 to length(Result) do
      if (CharInSet(Result[iLoop - 1], cDelimiters)) then
        Result[iLoop] := UpCase(Result[iLoop]);
  end;
end;

class function TLazyStringBase.IsEmptyString(AValue: string): Boolean;
begin
  Result := Trim(AValue) = '';
end;

class function TLazyStringBase.ExtractQuotedString(const S: string;
  Quote: char): string;
var
  i: integer;
begin
  Result := S;
  i := length(Result);
  if length(S) >= 2 then
  begin
    if (i > 0) and (Result[1] = Quote) and (Result[i] = Quote) then
    begin
      Delete(Result, i, 1);
      Delete(Result, 1, 1);
      for i := length(Result) downto 2 do
      begin
        if (Result[i] = Quote) and (Result[i - 1] = Quote) then
          Delete(Result, i, 1);
      end;
    end;
  end;
end;

class function TLazyDateTimeBase.DecodeTimeTextToMinutes(ATime: string)
  : integer;
var
  TempStr: string;
  idx, Hours, Minutes: integer;
begin
  TempStr := '';
  idx := 1;
  Hours := 0;
  Minutes := 0;
  while (idx <= length(ATime)) do
  begin
    if ATime[idx] <> ':' then
    begin
      TempStr := TempStr + ATime[idx];
      Minutes := StrToIntDef(TempStr, 0);
    end
    else
    begin
      Hours := StrToIntDef(TempStr, 0);
      TempStr := '';
    end;
    Inc(idx);
  end;
  Result := (Hours * 60) + Minutes;
end;

class function TLazyDateTimeBase.EncodeTimeTextFromMinutes
  (AMinutes: integer): string;
var
  Hours, Minutes: integer;
begin
  Hours := trunc(AMinutes / 60);
  Minutes := AMinutes - (Hours * 60);
  Result := Format('%d:%.2d', [Hours, Minutes]);
end;

class function TLazyDateTimeBase.MinutesToDaysHoursMinutes(AMinutes: integer;
  AHoursPerDay: integer = 24; ALongFormat: Boolean = False): string;
var
  TotalMinutes: integer;
  Days: integer;
  Hours: integer;
  Minutes: integer;
  Prefix: string;
  MinuteText: string;
  HourText: string;
  DayText: string;
begin
  TotalMinutes := AMinutes;
  Prefix := '';
  Days := 0;
  Hours := 0;
  Minutes := 0;

  if (TotalMinutes < 0) then
  begin
    Prefix := '-';
    TotalMinutes := TotalMinutes * -1;
  end;

  if TotalMinutes <> 0 then
  begin
    Hours := TotalMinutes div 60;
    Minutes := TotalMinutes mod 60;
    Days := Hours div AHoursPerDay;
    Hours := Hours mod AHoursPerDay;
  end;

  if Minutes <> 1 then
    MinuteText := 'minutes'
  else
    MinuteText := 'minute';

  if Hours <> 1 then
    HourText := 'hours'
  else
    HourText := 'hour';

  if Days <> 1 then
    DayText := 'days'
  else
    DayText := 'day';

  // Result := Format('%s%.2d:%.2d:%.2d', [Prefix, Days, Hours, Minutes]);
  if ALongFormat then
  begin
    if Days > 0 then
    begin
      Result := Format('%s%d %s %d %s %d %s', [Prefix, Days, DayText, Hours,
        HourText, Minutes, MinuteText]);
    end
    else
    begin
      Result := Format('%s%d %s %d %s', [Prefix, Hours, HourText, Minutes,
        MinuteText]);
    end;
  end
  else
  begin
    Result := Format('%s%.2d:%.2d:%.2d', [Prefix, Days, Hours, Minutes]);
  end;
end;

class procedure TLazyDateTimeBase.GenerateMonthList(AMonths: TStrings);
var
  MonthIdx: integer;
  LocaleFormatSettings: TFormatSettings;
begin
  LocaleFormatSettings := TFormatSettings.Create;
  AMonths.Clear;
  for MonthIdx := 1 to 12 do
  begin
    AMonths.Add(LocaleFormatSettings.LongMonthNames[MonthIdx]);
  end;
end;

class procedure TLazyDateTimeBase.SetNowMock(AFunction: TDateTimeFunc);
begin
  NowFunc := AFunction;
end;

class procedure TLazyDateTimeBase.SetTimeFormat(ATimeFormat: TLazyTimeFormat);
var
  LocaleFormatSettings: TFormatSettings;
begin
  LocaleFormatSettings := TFormatSettings.Create;
  case ATimeFormat of
    tfDefault:
      begin
        FormatSettings.ShortTimeFormat := LocaleFormatSettings.ShortTimeFormat;
        FormatSettings.LongTimeFormat := LocaleFormatSettings.LongTimeFormat;
      end;
    tf24Hour:
      begin
        FormatSettings.ShortTimeFormat := 'HH:mm';
        FormatSettings.LongTimeFormat := 'HH:mm:ss';
      end;
    tf12Hour:
      begin
        FormatSettings.ShortTimeFormat := 'hh:mm AMPM';
        FormatSettings.LongTimeFormat := 'hh:mm:ss AMPM';
      end;
  end;
end;

class function TLazyDateTimeBase.LocalDateTimeFromUTCDateTime(const UTCDateTime
  : TDateTime): TDateTime;
begin
  Result := TTimeZone.Local.ToLocalTime(UTCDateTime);
end;

class function TLazyDateTimeBase.DateTimeStrEval(const ADateTimeFormat: string;
  const ADateTimeStr: string): TDateTime;
var
  i, ii, iii: integer;
  LTemp, LFormat, LData, LMask, LSpec: string;
  LYear, LMonth, LDay, LHour, LMinute, LSecond, LMillisecond: word;
  LAmPm: integer;
  LLocaleFormatSettings: TFormatSettings;
begin
  LYear := 1;
  LMonth := 1;
  LDay := 1;
  LHour := 0;
  LMinute := 0;
  LSecond := 0;
  LMillisecond := 0;
  LFormat := UpperCase(ADateTimeFormat);
  LData := UpperCase(ADateTimeStr);
  i := 1;
  LMask := '';
  LAmPm := 0;
{$WARN SYMBOL_PLATFORM OFF}
  LLocaleFormatSettings := TFormatSettings.Create;
{$WARN SYMBOL_PLATFORM ON}
  while i < length(LFormat) do
  begin
    if CharInSet(LFormat[i], ['A', 'P', 'D', 'M', 'Y', 'H', 'N', 'S', 'Z']) then
    begin
      // Start of a date specifier
      LMask := LFormat[i];
      ii := i + 1;

      // Keep going till not valid specifier
      while True do
      begin
        if ii > length(LFormat) then
          Break;
        // End of specifier string
        LSpec := LMask + LFormat[ii];

        if (LSpec = 'DD') or (LSpec = 'DDD') or (LSpec = 'DDDD') or
          (LSpec = 'MM') or (LSpec = 'MMM') or (LSpec = 'MMMM') or
          (LSpec = 'YY') or (LSpec = 'YYY') or (LSpec = 'YYYY') or
          (LSpec = 'HH') or (LSpec = 'NN') or (LSpec = 'SS') or (LSpec = 'ZZ')
          or (LSpec = 'ZZZ') or (LSpec = 'AP') or (LSpec = 'AM') or
          (LSpec = 'AMP') or (LSpec = 'AMPM') or (LSpec = 'AM/PM') then
        begin
          LMask := LSpec;
          Inc(ii);
        end
        else
        begin
          // End of or Invalid specifier
          Break;
        end;
      end;

      // Got a valid specifier ? - evaluate it from data string
      if (LMask <> '') and (length(LData) > 0) then
      begin
        // Day 1..31
        if (LMask = 'DD') then
        begin
          LDay := StrToIntDef(Trim(Copy(LData, 1, 2)), 0);
          Delete(LData, 1, 2);
        end;

        // Day Sun..Sat (Just remove from data string)
        if LMask = 'DDD' then
          Delete(LData, 1, 3);

        // Day Sunday..Saturday (Just remove from data string LEN)
        if LMask = 'DDDD' then
        begin
          LTemp := Copy(LData, 1, 3);
          for iii := 1 to 7 do
          begin
            if LTemp = UpperCase(Copy(LLocaleFormatSettings.LongDayNames[iii],
              1, 3)) then
            begin
              Delete(LData, 1, length(LLocaleFormatSettings.LongDayNames[iii]));
              Break;
            end;
          end;
        end;

        // Month 1..12
        if (LMask = 'MM') then
        begin
          LMonth := StrToIntDef(Trim(Copy(LData, 1, 2)), 0);
          Delete(LData, 1, 2);
        end;

        // Month Jan..Dec
        if LMask = 'MMM' then
        begin
          LTemp := Copy(LData, 1, 3);
          for iii := 1 to 12 do
          begin
            if LTemp = UpperCase(Copy(LLocaleFormatSettings.LongMonthNames[iii],
              1, 3)) then
            begin
              LMonth := iii;
              Delete(LData, 1, 3);
              Break;
            end;
          end;
        end;

        // Month January..December
        if LMask = 'MMMM' then
        begin
          LTemp := Copy(LData, 1, 3);
          for iii := 1 to 12 do
          begin
            if LTemp = UpperCase(Copy(LLocaleFormatSettings.LongMonthNames[iii],
              1, 3)) then
            begin
              LMonth := iii;
              Delete(LData, 1,
                length(LLocaleFormatSettings.LongMonthNames[iii]));
              Break;
            end;
          end;
        end;

        // Year 2 Digit
        if LMask = 'YY' then
        begin
          LYear := StrToIntDef(Copy(LData, 1, 2), 0);
          Delete(LData, 1, 2);
          if LYear < LLocaleFormatSettings.TwoDigitYearCenturyWindow then
            LYear := (YearOf(Date) div 100) * 100 + LYear
          else
            LYear := (YearOf(Date) div 100 - 1) * 100 + LYear;
        end;

        // Year 4 Digit
        if LMask = 'YYYY' then
        begin
          LYear := StrToIntDef(Copy(LData, 1, 4), 0);
          Delete(LData, 1, 4);
        end;

        // Hours
        if LMask = 'HH' then
        begin
          LHour := StrToIntDef(Trim(Copy(LData, 1, 2)), 0);
          Delete(LData, 1, 2);
        end;

        // Minutes
        if LMask = 'NN' then
        begin
          LMinute := StrToIntDef(Trim(Copy(LData, 1, 2)), 0);
          Delete(LData, 1, 2);
        end;

        // Seconds
        if LMask = 'SS' then
        begin
          LSecond := StrToIntDef(Trim(Copy(LData, 1, 2)), 0);
          Delete(LData, 1, 2);
        end;

        // Milliseconds
        if (LMask = 'ZZ') or (LMask = 'ZZZ') then
        begin
          LMillisecond := StrToIntDef(Trim(Copy(LData, 1, 3)), 0);
          Delete(LData, 1, 3);
        end;

        // AmPm A or P flag
        if (LMask = 'AP') then
        begin
          if LData[1] = 'A' then
            LAmPm := -1
          else
            LAmPm := 1;
          Delete(LData, 1, 1);
        end;

        // AmPm AM or PM flag
        if (LMask = 'AM') or (LMask = 'AMP') or (LMask = 'AMPM') or
          (LMask = 'AM/PM') then
        begin
          if Copy(LData, 1, 2) = 'AM' then
            LAmPm := -1
          else
            LAmPm := 1;
          Delete(LData, 1, 2);
        end;

        LMask := '';
        // i    := ii;
      end;
      i := ii;
    end
    else
    begin
      // Remove delimiter from data string
      if length(LData) > 1 then
        Delete(LData, 1, 1);
      Inc(i);
    end;
  end;

  if LHour = 24 then
    LHour := 0;
  if LMinute = 60 then
    LMinute := 0;
  if LSecond = 60 then
    LSecond := 0;

  if LHour > 24 then
    raise EInvalidOp.Create('Hours cannot be larger than 24');
  if LMinute > 60 then
    raise EInvalidOp.Create('Minutes cannot be larger than 60');
  if LSecond > 60 then
    raise EInvalidOp.Create('Seconds cannot be larger than 60');

  if LAmPm = 1 then
    LHour := LHour + 12;

  if not TryEncodeDateTime(LYear, LMonth, LDay, LHour, LMinute, LSecond,
    LMillisecond, Result) then
    Result := 0;
end;

class function TLazyDateTimeBase.StringToDate(AValue: string): TDate;
var
  DayIdx: integer;
  Day, Month, Year: integer;
  Date: TDate;
  DateStr, IncDayValue: string;
  LAllowOperation: Boolean;
begin
  Result := 0;

  if length(AValue) > 0 then
  begin

    if not SameText(DateToStr(0), AValue) then
    begin

      LAllowOperation := False;

      Date := TIso8601.DateFromIso8601(AValue);

      if Date = 0 then
      begin

        DateStr := StringReplace(AValue, '.', '/', [rfReplaceAll]);
        // Allow 2.4.80 dates
        DateStr := StringReplace(AValue, '%', '', [rfReplaceAll]);
        // Different option for variables (eg %TODAY%)

        // Check for dates entered in format 01 or 0101 or 01012012
        Day := StrToIntDef(DateStr, 0);
        if Day <> 0 then
        begin
          Month := MonthOf(Today);
          Year := YearOf(Today);

          // Day
          if length(AValue) >= 2 then
          begin
            Day := StrToIntDef(Copy(AValue, 1, 2), 0);
          end;

          // Month
          if (length(DateStr) >= 4) then
          begin
            Month := StrToIntDef(Copy(AValue, 3, 2), 0);
          end;

          // Year 2 digit
          if (length(AValue) = 6) then
          begin
            Year := ConvertDoubleDigitYear(StrToIntDef(Copy(AValue, 5, 2), 0));
          end;

          // Year 4 digit
          if (length(AValue) = 8) then
          begin
            Year := StrToIntDef(Copy(AValue, 5, 4), 0);
          end;

          if (Day > 0) and (Month > 0) and (Year > 0) then
          begin
            DateStr := DateToStr(encodedate(Year, Month, Day));
          end;
        end;

        if Pos('Y', UpperCase(DateStr)) = 1 then
        begin
          Date := (IncDay(NowFunc, -1));
          LAllowOperation := True;
        end;
        if Pos('T', UpperCase(DateStr)) = 1 then
        begin
          Date := (IncDay(NowFunc, 1));
          LAllowOperation := True;
        end;
        if (Pos('N', UpperCase(DateStr)) = 1) or
          (Pos('TODAY', UpperCase(DateStr)) = 1) then
        begin
          Date := NowFunc;
          LAllowOperation := True;
        end;

        for DayIdx := 0 to 6 do
        begin
          case DayOfTheWeek(IncDay(NowFunc, DayIdx)) of
            1:
              begin
                if Pos('MON', UpperCase(DateStr)) = 1 then
                begin
                  Date := IncDay(NowFunc, DayIdx);
                  LAllowOperation := True;
                end;
              end;
            2:
              begin
                if Pos('TUE', UpperCase(DateStr)) = 1 then
                begin
                  Date := IncDay(NowFunc, DayIdx);
                  LAllowOperation := True;
                end;
              end;
            3:
              begin
                if Pos('WED', UpperCase(DateStr)) = 1 then
                begin
                  Date := IncDay(NowFunc, DayIdx);
                  LAllowOperation := True;
                end;
              end;
            4:
              begin
                if Pos('THU', UpperCase(DateStr)) = 1 then
                begin
                  Date := IncDay(NowFunc, DayIdx);
                  LAllowOperation := True;
                end;
              end;
            5:
              begin
                if Pos('FRI', UpperCase(DateStr)) = 1 then
                begin
                  Date := IncDay(NowFunc, DayIdx);
                  LAllowOperation := True;
                end;
              end;
            6:
              begin
                if Pos('SAT', UpperCase(DateStr)) = 1 then
                begin
                  Date := IncDay(NowFunc, DayIdx);
                  LAllowOperation := True;
                end;
              end;
            7:
              begin
                if Pos('SUN', UpperCase(DateStr)) = 1 then
                begin
                  LAllowOperation := True;
                  Date := IncDay(NowFunc, DayIdx);
                end;
              end;
          end;
        end;

        if Pos('BOM', UpperCase(DateStr)) = 1 then
        begin
          Date := (StartOfTheMonth(NowFunc));
          LAllowOperation := True;
        end;

        if Pos('EOM', UpperCase(DateStr)) = 1 then
        begin
          Date := (EndOfTheMonth(NowFunc));
          LAllowOperation := True;
        end;

        if Pos('BOW', UpperCase(DateStr)) = 1 then
        begin
          Date := (StartOfTheWeek(NowFunc));
          LAllowOperation := True;
        end;
        if Pos('EOW', UpperCase(DateStr)) = 1 then
        begin
          Date := (EndOfTheWeek(NowFunc));
          LAllowOperation := True;
        end;

        if Pos('BOY', UpperCase(DateStr)) = 1 then
        begin
          Date := (StartOfTheYear(NowFunc));
          LAllowOperation := True;
        end;
        if Pos('EOY', UpperCase(DateStr)) = 1 then
        begin
          Date := (EndOfTheYear(NowFunc));
          LAllowOperation := True;
        end;

        if LAllowOperation then
        begin
          if ((Pos('+', DateStr) > 0) or (Pos('-', DateStr) > 0)) then
          begin
            if (Pos('+', DateStr) > 0) then
              IncDayValue := Copy(DateStr, Pos('+', DateStr) + 1,
                length(DateStr));
            if (Pos('-', DateStr) > 0) then
              IncDayValue := Copy(DateStr, Pos('-', DateStr) + 1,
                length(DateStr));
            if Pos('+', DateStr) > 0 then
            begin
              Date := IncDay(Date, StrToIntDef(IncDayValue, 0));
            end
            else
            begin
              Date := IncDay(Date, StrToIntDef(IncDayValue, 0) * -1);
            end;
          end;
        end;
      end;

      if Date <> 0 then
        DateStr := DateToStr(Date);

      Result := VarToDateTime(DateStr);
    end;
  end;
end;

class function TLazyDateTimeBase.StringToDateDef(AValue: string;
  ADefault: TDate): TDate;
begin
  try
    Result := StringToDate(AValue);
  except
    Result := ADefault;
  end;
end;

class function TLazyDateTimeBase.StringToTime(AValue: string;
  ARounding: Boolean = False; ARoundUserEntries: Boolean = False;
  ARoundValue: integer = 10;
  ARoundingType: TLazyTimeRounding = trNearest): TTime;
var
  LTime: TTime;
  LDateTime: TDateTime;
  LTimeStr: string;
  LTimeValue: integer;
  LIncTimeValue: string;
  LUserInput: Boolean;
begin
  LTime := 0;

  // Check known edge cases first
  if AValue = '24:00' then
    LTimeStr := '0'
  else if AValue = '24:00:00' then
    LTimeStr := '0'
  else if length(AValue) > 0 then
  begin
    LUserInput := True;

    if not SameText(DateToStr(0), AValue) then
    begin

      if LTime = 0 then
      begin
        LTimeStr := TLazyStringBase.StringCleaner(AValue, True, True);
        LTimeStr := StringReplace(LTimeStr, '%', '',
          [rfReplaceAll, rfIgnoreCase]);

        if (Pos('T', UpperCase(LTimeStr)) = 1) then
          LTime := NowFunc;

        if (UpperCase(LTimeStr)[1] = 'S') then
        begin
          LTime := NowFunc;

          if (Pos('+', LTimeStr) > 0) then
          begin
            LIncTimeValue := Copy(LTimeStr, Pos('+', LTimeStr) + 1,
              length(LTimeStr));
            LTime := IncSecond(NowFunc, StrToIntDef(LIncTimeValue, 0));
          end
          else if (Pos('-', LTimeStr) > 0) then
          begin
            LIncTimeValue := Copy(LTimeStr, Pos('-', LTimeStr) + 1,
              length(LTimeStr));
            LTime := IncSecond(NowFunc, StrToIntDef(LIncTimeValue, 0) * -1);
          end;
        end;

        if (UpperCase(LTimeStr)[1] = 'M') or (UpperCase(LTimeStr)[1] = 'N') then
        begin
          LTime := NowFunc;

          if Pos('+', LTimeStr) > 0 then
          begin
            LIncTimeValue := Copy(LTimeStr, Pos('+', LTimeStr) + 1,
              length(LTimeStr));
            LTime := IncMinute(NowFunc, StrToIntDef(LIncTimeValue, 0));
          end
          else if Pos('-', LTimeStr) > 0 then
          begin
            LIncTimeValue := Copy(LTimeStr, Pos('-', LTimeStr) + 1,
              length(LTimeStr));
            LTime := IncMinute(NowFunc, StrToIntDef(LIncTimeValue, 0) * -1);
          end;
        end;

        if (UpperCase(LTimeStr)[1] = 'H') then
        begin
          LTime := NowFunc;

          if (Pos('+', LTimeStr) > 0) then
          begin
            LIncTimeValue := Copy(LTimeStr, Pos('+', LTimeStr) + 1,
              length(LTimeStr));
            LTime := IncHour(NowFunc, StrToIntDef(LIncTimeValue, 0));
          end
          else if Pos('-', LTimeStr) > 0 then
          begin
            LIncTimeValue := Copy(LTimeStr, Pos('-', LTimeStr) + 1,
              length(LTimeStr));
            LTime := IncHour(NowFunc, StrToIntDef(LIncTimeValue, 0) * -1);
          end;
        end;

        if LTime = 0 then
        begin
          LTimeValue := StrToIntDef(TLazyStringBase.StripNonNumeric(LTimeStr,
            False, False), -1);
          if (LTimeValue <> -1) and (length(IntToStr(LTimeValue)) <= 6) then
          begin
            case length(IntToStr(LTimeValue)) of
              1:
                LTime := DateTimeStrEval('hh', '0' + IntToStr(LTimeValue));
              2:
                LTime := DateTimeStrEval('hh', IntToStr(LTimeValue));
              3:
                LTime := DateTimeStrEval('hhnn', '0' + IntToStr(LTimeValue));
              4:
                LTime := DateTimeStrEval('hhnn', IntToStr(LTimeValue));
              5:
                LTime := DateTimeStrEval('hhnnss', '0' + IntToStr(LTimeValue));
              6:
                LTime := DateTimeStrEval('hhnnss', IntToStr(LTimeValue));
            end;
            if Pos('pm', LowerCase(LTimeStr)) > 0 then
            begin
              if (HourOf(LTime) < 12) and (HourOf(LTime) > 0) then
              begin
                LTime := IncHour(LTime, 12);
              end;
            end;
            if Pos('am', LowerCase(LTimeStr)) > 0 then
            begin
              if (HourOf(LTime) >= 12) and (HourOf(LTime) <= 24) then
              begin
                LTime := IncHour(LTime, -12);
              end;
            end;
            LUserInput := False;
            LTimeStr := TimeToStr(LTime);
          end;
        end;

        if LTime <> 0 then
        begin
          LUserInput := False;
          // Time has been calculated from a variable.
          LTimeStr := TimeToStr(LTime);
        end
        else
        begin

          if (LTime = 0) and ((Pos('T', UpperCase(AValue)) > 0) or
            (Pos('Z', UpperCase(AValue)) > 0)) then
          begin
            try
              LTime := ISO8601ToDate(AValue, False);
            except
              LTime := 0;
            end;
          end;

          if LTime = 0 then
          begin
            try
              if VarDateFromStr(PWideChar(LTimeStr), VAR_LOCALE_USER_DEFAULT, 0,
                LDateTime) = VAR_OK then
              begin
                LTime := TimeOf(LDateTime);
              end;
              // LTime := VarToDateTime(LTimeStr);
            except
              LTime := 0;
            end;
          end;

          if LTime = 0 then
          begin
            try
              LTime := TIso8601.TimeFromIso8601(AValue);
              if LTime = 0 then
                LTime := TimeOf(TIso8601.DateTimeFromIso8601(AValue));
            except
              LTime := 0;
            end;
          end;

          LTimeStr := TimeToStr(LTime);
        end;

        LTime := VarToDateTime(LTimeStr);
      end;

      if ARounding and ((LUserInput = False) or (ARoundUserEntries)) then
      begin
        LTime := RoundTime(LTime, ARoundValue, ARoundingType);
        LTimeStr := TimeToStr(LTime);
      end
      else
        LTimeStr := TimeToStr(LTime);
    end
    else
    begin
      LTimeStr := '0';
    end;
  end;

  Result := TimeOf(VarToDateTime(LTimeStr));
end;

class function TLazyDateTimeBase.StringToTimeDef(AValue: string;
  ADefault: TTime; ARounding: Boolean = False;
  ARoundUserEntries: Boolean = False; ARoundValue: integer = 10;
  ARoundingType: TLazyTimeRounding = trNearest): TTime;
begin
  try
    Result := StringToTime(AValue, ARounding, ARoundUserEntries, ARoundValue,
      ARoundingType);
  except
    Result := ADefault;
  end;
end;

class function TLazyDateTimeBase.StringToDateTime(AValue: string): TDateTime;
var
  LDate: TDate;
  LTime: TTime;
  LDateTime: TDateTime;
begin
  LDateTime := 0;
  try
    if not TLazyStringBase.IsEmptyString(AValue) then
    begin
      try
        LDateTime := TIso8601.DateTimeFromIso8601(AValue);
      except
        LDateTime := 0;
      end;

      try
        LDateTime := VarToDateTime(AValue);
      except
        LDateTime := 0;
      end;

      if LDateTime = 0 then
      begin
        LTime := StringToTime(AValue);
        LDate := StringToDate(AValue);
        LDateTime := LDate + LTime;
      end;
    end;

  finally
    Result := LDateTime;
  end;
end;

class function TLazyDateTimeBase.StringToDateTimeDef(AValue: string;
  ADefault: TDateTime): TDateTime;
var
  Date: TDate;
  Time: TTime;
begin
  Date := StringToDateDef(AValue, ADefault);
  Time := StringToTimeDef(AValue, ADefault);
  Result := Date + Time;
end;

class function TLazyDateTimeBase.ConvertDoubleDigitYear
  (AValue: integer): integer;
var
  YearWindow: integer;
  century: integer;
begin
  century := (YearOf(NowFunc) div 1000) * 1000;
  YearWindow := YearOf(NowFunc) - (century) + 50;
  Result := AValue;
  if Result >= YearWindow then
  begin
    Result := Result + century - 100;
  end
  else
  begin
    Result := Result + century;
  end;
end;

class function TLazyBooleanBase.BoolToInteger(ABoolean: Boolean): integer;
begin
  Result := 0;
  if ABoolean then
  begin
    Result := 1;
  end;
end;

class function TLazyBooleanBase.IntegerToBool(AValue: integer): Boolean;
begin
  Result := AValue = 1;
end;

class function TLazyMathBase.CalculatePercentage(AValue: integer;
  ATotal: integer): integer;
begin
  Result := 0;
  if ATotal > 0 then
  begin
    Result := Round((AValue / ATotal) * 100);
  end;
end;

class function TLazyMathBase.HexToInt(S: String): LongInt;
const
  DecDigits = ['0' .. '9'];
  HexVals: Array [0 .. $F] Of integer = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, $A, $B,
    $C, $D, $E, $F);
  UpCaseHexLetters = ['A' .. 'F'];
  LowCaseHexLetters = ['a' .. 'f'];
var
  v: LongInt;
  i: integer;
  LookUpIndex: integer;
begin
  if length(S) <= 8 then
  begin
    v := 0;
    for i := 1 to length(S) do
    begin
{$R-}
      v := v Shl 4;
{$R+}
      if CharInSet(S[i], DecDigits) then
      begin
        LookUpIndex := Ord(S[i]) - Ord('0');
      end
      else
      begin
        if CharInSet(S[i], UpCaseHexLetters) then
        begin
          LookUpIndex := Ord(S[i]) - Ord('A') + $A;
        end
        else
        begin
          if CharInSet(S[i], LowCaseHexLetters) then
          begin
            LookUpIndex := Ord(S[i]) - Ord('a') + $A;
          end
          else
          begin
            LookUpIndex := 0;
          end;
        end;
      end;
      v := v Or HexVals[LookUpIndex];
    end;
    Result := v;
  end
  else
  begin
    Result := 0;
  end;
end;

class procedure TLazyStringBase.ParseDelimited(const sl: TStrings;
  const Value: string; const delimiter: string);
var
  dx: integer;
  ns: string;
  txt: string;
  delta: integer;
begin
  delta := length(delimiter);
  txt := Value + delimiter;
  sl.BeginUpdate;
  sl.Clear;
  try
    while length(txt) > 0 do
    begin
      dx := Pos(delimiter, txt);
      ns := Copy(txt, 0, dx - 1);
      sl.Add(ns);
      txt := Copy(txt, dx + delta, MaxINt);
    end;
  finally
    sl.EndUpdate;
  end;
end;

class function TLazyStringBase.GeneratePassword(ALength: integer): string;
var
  i: integer;
  AllowedCharacters: string;
begin
  AllowedCharacters :=
    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#@!';
  Randomize;
  Result := '';
  for i := 1 to ALength do
  begin
    Result := Result + AllowedCharacters[Random(length(AllowedCharacters)) + 1];
  end;
end;

class function TLazyStringBase.SplitStringToWords(const AString: string;
  AWords: TStrings): integer;
var
  idx: integer;
  word: string;
  ValidChar: Boolean;
begin
  Result := -1;
  if Assigned(AWords) then
  begin
    AWords.Clear;
    word := '';
    for idx := 1 to length(AString) + 1 do
    begin
      ValidChar := False;
      if idx <= length(AString) then
      begin
        ValidChar := Ord(AString[idx]) > 32;
      end;

      if ValidChar then
      begin
        word := word + AString[idx];
      end
      else
      begin
        if Trim(word) <> '' then
        begin
          AWords.Add(word);
        end;
        word := '';
      end;
    end;
    Result := AWords.Count;
  end;
end;

class function TLazyStringBase.GetNthNumber(AValue: integer): string;
var
  BaseValue: integer;
begin

  Result := IntToStr(AValue);

  if length(Result) >= 2 then
  begin
    BaseValue := StrToIntDef(Result[length(Result) - 1] +
      Result[length(Result)], 0);
  end
  else
  begin
    BaseValue := AValue;
  end;

  if (BaseValue >= 11) and (BaseValue <= 19) then // teens
  begin
    Result := Result + 'th';
  end;

  if Result[length(Result)] = '1' then
  begin
    Result := Result + 'st';
  end;
  if Result[length(Result)] = '2' then
  begin
    Result := Result + 'nd';
  end;
  if Result[length(Result)] = '3' then
  begin
    Result := Result + 'rd';
  end;

  if StrToIntDef(Result, -1) <> -1 then
  // still just a number add default "th"
  begin
    Result := Result + 'th';
  end;
end;

class function TLazyStringBase.LeftPad(S: string; Ch: char;
  Len: integer): string;
var
  RestLen: integer;
begin
  Result := S;
  RestLen := Len - length(S);
  if RestLen < 1 then
    Exit;
  Result := S + StringOfChar(Ch, RestLen);
end;

class function TLazyStringBase.RightPad(S: string; Ch: char;
  Len: integer): string;
var
  RestLen: integer;
begin
  Result := S;
  RestLen := Len - length(S);
  if RestLen < 1 then
    Exit;
  Result := StringOfChar(Ch, RestLen) + S;
end;

class function TLazyStringBase.StrMaxLen(const S: string;
  MaxLen: integer): string;
begin
  Result := S;
  if length(Result) <= MaxLen then
    Exit;
  SetLength(Result, MaxLen);
  Result[MaxLen] := '…';
end;

class function TLazyStringBase.FormatByteSize(const bytes: extended): string;
const
  B = 1; // byte
  KB = 1024 * B; // kilobyte
  MB = 1024 * KB; // megabyte
  GB = 1024 * MB; // gigabyte
begin
  if bytes > GB then
    Result := FormatFloat('0.00 GB', bytes / GB)
  else if bytes > MB then
    Result := FormatFloat('0.00 MB', bytes / MB)
  else if bytes > KB then
    Result := FormatFloat('0.00 KB', bytes / KB)
  else
    Result := FormatFloat('0.00 bytes', bytes);
end;

class function TLazyStringBase.LeadingZeroes(const ANumber,
  ALength: integer): string;
begin
  Result := SysUtils.Format('%.*d', [ALength, ANumber]);
end;

initialization

TLazyDateTimeBase.NowFunc := Now;

end.
