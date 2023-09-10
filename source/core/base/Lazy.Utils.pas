unit Lazy.Utils;

interface

uses
  Lazy.Types,
  SysUtils, Classes, Variants;

// File and Folder Routines
function GetTempFolder: string;
function GetGUIDFileName(AFolder, APrefix, AExtension: string): string;
function GetTempFile(APrefix: string; AExtension: string = '.tmp';
  AFolder: string = ''): string;
function GetApplicationParameters(AParameter: string;
  var AValue: string): Boolean;
function GetApplicationDir: string;
function CheckDirectoryExists(ADirectory: string; ACreate: Boolean): Boolean;
function IsValidFileName(AFileName: TFileName): Boolean;
function ValidateFileName(AFileName: TFileName): TFileName;
function ExtractUrlFileName(const AURL: string): string;

// String Routines
function IsEmptyString(AValue: string): Boolean;
procedure ParseDelimited(const sl: TStrings; const Value: string;
  const delimiter: string);
function ExtractQuotedString(const S: string; Quote: char): string;
function StripNonAlphaNumeric(const AValue: string): string;
function StripNonNumeric(const AValue: string; AAllowDecimal: Boolean = False;
  AAllowNegative: Boolean = False): string;
function StripCharsInSet(const AValue: string; ACharset: TSysCharSet): string;
function StipNonStandard(const AValue: string): string;
function EncodeHTML(AValue: String): string;
function DecodeHTML(AValue: String): string;
function StripHTML(S: string): string;
function StripExtraSpaces(AValue: string; ARemoveTab: Boolean = False;
  ARemoveCRLF: Boolean = False): string;
function StringCleaner(const AValue: string; ARemoveTab: Boolean = False;
  ARemoveCRLF: Boolean = False; ACharset: TSysCharSet = [#0 .. #8, #11, #12,
  #14 .. #31, #127 .. #255]): string;
function ContainsNonAlphaNumberic(const AValue: string): Boolean;
function TitleCase(const AText: string;
  const ALowerCaseFirst: Boolean = True): string;
function GeneratePassword(ALength: integer): string;
function LeadingZeroes(const ANumber, ALength: integer): string;
function SplitStringToWords(const AString: string; AWords: TStrings): integer;
function GetNthNumber(AValue: integer): string;
function LeftPad(S: string; Ch: char; Len: integer): string;
function RightPad(S: string; Ch: char; Len: integer): string;
function StrMaxLen(const S: string; MaxLen: integer): string;
function FormatByteSize(const bytes: extended): string;

// Date and Time Routines
function RoundTime(ADateTime: TDateTime; AMins: double;
  ARounding: TLazyTimeRounding = trNearest): TDateTime;
function EncodeTimeTextFromMinutes(AMinutes: integer): string;
function DecodeTimeTextToMinutes(ATime: string): integer;
function MinutesToDaysHoursMinutes(AMinutes: integer;
  AHoursPerDay: integer = 24; ALongFormat: Boolean = False): string;
procedure GenerateMonthList(AMonths: TStrings);
procedure SetTimeFormat(ATimeFormat: TLazyTimeFormat);
function LocalDateTimeFromUTCDateTime(const UTCDateTime: TDateTime): TDateTime;
function DateTimeStrEval(const DateTimeFormat: string;
  const DateTimeStr: string): TDateTime;
function StringToDate(AValue: string): TDate;
function StringToDateDef(AValue: string; ADefault: TDate): TDate;
function StringToTime(AValue: string; ARounding: Boolean = False;
  ARoundUserEntries: Boolean = False; ARoundValue: integer = 10;
  ARoundingType: TLazyTimeRounding = trNearest): TTime;
function StringToTimeDef(AValue: string; ADefault: TTime;
  ARounding: Boolean = False; ARoundUserEntries: Boolean = False;
  ARoundValue: integer = 10;
  ARoundingType: TLazyTimeRounding = trNearest): TTime;
function StringToDateTime(AValue: string): TDateTime;
function StringToDateTimeDef(AValue: string; ADefault: TDateTime): TDateTime;
function ConvertDoubleDigitYear(AValue: integer): integer;
function MinutesToMetricMinutes(AMinutes: integer): integer;
function MetricMinutesToMinutes(AMetricMinutes: integer): integer;
Function GetEasterDate(AYear: integer; var AEaster: TDateTime): Boolean;
function DateTimeToString(ADateTime: TDateTime; ANullText: string = '')
  : string; overload;
function DateTimeToString(ADateTime: TDateTime;
  AFormatSettings: TFormatSettings; ANullText: string = ''): string; overload;
function DateToString(ADate: TDateTime; AFormatSettings: TFormatSettings;
  ANullText: string = ''): string; overload;
function DateToString(ADate: TDate; ANullText: string = ''): string; overload;
function DateToString(ADate: TDateTime; ANullText: string = '')
  : string; overload;
function TimeToString(ATime: TDateTime; AFormatSettings: TFormatSettings;
  ANullText: string = ''): string; overload;
function TimeToString(ATime: TTime; ANullText: string = ''): string; overload;
function TimeToString(ATime: TDateTime; ANullText: string = '')
  : string; overload;
function DateTimeDifference(AStartDate: TDateTime; AEndDate: TDateTime)
  : string; overload;
function DateTimeDifference(AStartDate: TDateTime; AEndDate: TDateTime;
  var ADays: word; var AHours: word; var AMinutes: word; var ASeconds: word)
  : string; overload;

// Boolean Routines
function BoolToInteger(ABoolean: Boolean): integer;
function IntegerToBool(AValue: integer): Boolean;

// Math Routines
function CalculatePercentage(AValue: integer; ATotal: integer): integer;
Function HexToInt(S: String): LongInt;

implementation

uses
  System.DateUtils, System.StrUtils, System.IOUtils;

function MinutesToMetricMinutes(AMinutes: integer): integer;
begin
  Result := Round((AMinutes * 10) / 6);
end;

function MetricMinutesToMinutes(AMetricMinutes: integer): integer;
begin
  Result := Round((AMetricMinutes / 10) * 6);
end;

Function GetEasterDate(AYear: integer; var AEaster: TDateTime): Boolean;
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

function DateTimeToString(ADateTime: TDateTime;
  AFormatSettings: TFormatSettings; ANullText: string = ''): string; overload;
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

function DateTimeToString(ADateTime: TDateTime; ANullText: string = ''): string;
begin
  Result := DateTimeToString(ADateTime, FormatSettings, ANullText);
end;

function DateToString(ADate: TDateTime; AFormatSettings: TFormatSettings;
  ANullText: string = ''): string;
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

function DateToString(ADate: TDate; ANullText: string = ''): string;
begin
  Result := DateToString(ADate, FormatSettings, ANullText);
end;

function DateToString(ADate: TDateTime; ANullText: string = ''): string;
begin
  Result := DateToString(ADate, FormatSettings, ANullText);
end;

function TimeToString(ATime: TDateTime; AFormatSettings: TFormatSettings;
  ANullText: string = ''): string;
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

function TimeToString(ATime: TTime; ANullText: string = ''): string;
begin
  Result := TimeToString(ATime, FormatSettings, ANullText);
end;

function TimeToString(ATime: TDateTime; ANullText: string = ''): string;
begin
  Result := TimeToString(ATime, FormatSettings, ANullText);
end;

function DateTimeDifference(AStartDate: TDateTime; AEndDate: TDateTime): string;
var
  Days, Hours, Mins, Secs: word;
begin
  Result := DateTimeDifference(AStartDate, AEndDate, Days, Hours, Mins, Secs);
end;

function DateTimeDifference(AStartDate: TDateTime; AEndDate: TDateTime;
  var ADays: word; var AHours: word; var AMinutes: word; var ASeconds: word)
  : string; overload;
begin
  ADays := DaysBetween(AStartDate, AEndDate);
  AHours := HoursBetween(AEndDate, AStartDate) mod 24; // Remove total days
  AMinutes := MinutesBetween(AStartDate, AEndDate) mod 60;
  ASeconds := SecondsBetween(AStartDate, AEndDate) mod 60;
  Result := Format('%d days, %d hours, %d min, %d secs',
    [ADays, AHours, AMinutes, ASeconds]);
end;

function RoundTime(ADateTime: TDateTime; AMins: double;
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

function ExtractUrlFileName(const AURL: string): string;
var
  i: integer;
begin
  i := LastDelimiter('/', AURL);
  Result := Copy(AURL, i + 1, Length(AURL) - (i));
end;

function GetTempFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;

function GetTempFile(APrefix: string; AExtension: string = '.tmp';
  AFolder: string = ''): string;
var
  Attempts: Cardinal;
  Folder: string;

  function RandomFileName(ALength: integer): string;
  var
    str: string;
  begin
    Randomize;
    str := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
    Result := '';
    repeat
      Result := Result + str[Random(Length(str)) + 1];
    until (Length(Result) = ALength);
  end;

begin
  Attempts := 0;
  Folder := AFolder;
  if IsEmptyString(Folder) then
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

function GetGUIDFileName(AFolder, APrefix, AExtension: string): string;
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
  until (not FileExists(Result)) or (Attempts >= High(Cardinal));
  if (Attempts >= High(Cardinal)) then
  begin
    raise Exception.Create('Failed to generate GUID file.');
  end;
end;

function GetApplicationParameters(AParameter: string;
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

function GetApplicationDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function CheckDirectoryExists(ADirectory: string; ACreate: Boolean): Boolean;
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

function IsValidFileName(AFileName: TFileName): Boolean;
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

function ValidateFileName(AFileName: TFileName): TFileName;
begin
  Result := AFileName;
  Result := StripExtraSpaces(Result, True, True);
  Result := StripCharsInSet(Result, ['\', '/', ':', '*', '?', '"', '<',
    '>', '|']);
end;

procedure QuickFileSearch(const PathName, FileName: string;
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
            QuickFileSearch(path + Rec.Name, FileName, True, FileList);
          end;
        until (FindNext(Rec) <> 0) or (Cancel = True);
      finally
        FindClose(Rec);
      end;
  end;
end;

function ContainsNonAlphaNumberic(const AValue: string): Boolean;
var
  i: integer;
begin
  Result := False;
  i := 1;
  while (i <= Length(AValue)) and (not Result) do
  begin
    if not(AValue[i] in ['A' .. 'Z', 'a' .. 'z', '0' .. '9']) then
    begin
      Result := True;
    end;
    Inc(i);
  end;
end;

function StripNonAlphaNumeric(const AValue: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(AValue) do
  begin
    if AValue[i] in ['A' .. 'Z', 'a' .. 'z', '0' .. '9'] then
      Result := Result + AValue[i];
  end;
  Result := Trim(Result);
end;

function StripNonNumeric(const AValue: string; AAllowDecimal: Boolean = False;
  AAllowNegative: Boolean = False): string;
var
  i: integer;
begin
  Result := '';
  if Trim(AValue) <> '' then
  begin

    for i := 1 to Length(AValue) do
    begin
      if (AValue[i] in ['0' .. '9']) or ((AAllowDecimal) and (AValue[i] = '.'))
        or ((AAllowNegative) and (AValue[i] = '-')) then
      begin
        Result := Result + AValue[i];
      end;
    end;
  end;
  Result := Trim(Result);
end;

function StripCharsInSet(const AValue: string; ACharset: TSysCharSet): string;
var
  i: integer;
begin
  for i := 1 to Length(AValue) do
  begin
    if not CharInSet(AValue[i], ACharset) then
      Result := Result + AValue[i];
  end;
end;

function StipNonStandard(const AValue: string): string;
begin
  Result := StripCharsInSet(AValue, [#0 .. #9, #11, #12, #14 .. #31,
    #127 .. #255]);
end;

function RemoveTags(const S: string): string;
var
  i: integer;
  InTag: Boolean;
begin
  Result := '';
  InTag := False;
  for i := 1 to Length(S) do
  begin
    if S[i] = '<' then
      InTag := True
    else if S[i] = '>' then
      InTag := False
    else if not InTag then
      Result := Result + S[i];
  end;
end;

function EncodeHTML(AValue: String): string;
begin
  Result := AValue;
  // Result := TNetEncoding.HTML.Encode(Result);
  Result := ReplaceStr(Result, '&', '&amp;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
  Result := ReplaceStr(Result, chr(39), '&apos;');
  Result := ReplaceStr(Result, '"', '&quot;');
  // Result := ReplaceStr(Result, #13, '&#xD;'); // CR
  // Result := ReplaceStr(Result, #10, '&#xA;'); // LF
  // Result := ReplaceStr(Result, #13, '&#13;'); // CR
  // Result := ReplaceStr(Result, #10, '&#10;'); // LF
  Result := ReplaceStr(Result, #13, '#0013'); // CR
  Result := ReplaceStr(Result, #10, '#0010'); // LF

end;

function DecodeHTML(AValue: String): string;
begin
  Result := AValue;
  // Result := TNetEncoding.HTML.Decode(Result);
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

function StripHTML(S: string): string;
begin
  Result := RemoveTags(S);
  Result := DecodeHTML(Result);
  Result := Trim(Result);
end;

// function StripHTML(S: string): string;
// var
// TagBegin, TagEnd, TagLength: integer;
// begin
// TagBegin := Pos('<', S);
// while (TagBegin > 0) do
// begin // while there is a < in S
// TagEnd := Pos('>', S); // find the matching >
// TagLength := TagEnd - TagBegin + 1;
// Delete(S, TagBegin, TagLength); // delete the tag
// TagBegin := Pos('<', S); // search for next <
// end;
//
// S := StringReplace(S, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
//
// Result := Trim(S); // give the result
// end;

function StripExtraSpaces(AValue: string; ARemoveTab: Boolean = False;
  ARemoveCRLF: Boolean = False): string;
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

  if Length(Source) > 1 then
  begin
    Result := Source[1];
    for i := 2 to Length(Source) do
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

function StringCleaner(const AValue: string; ARemoveTab: Boolean = False;
  ARemoveCRLF: Boolean = False; ACharset: TSysCharSet = [#0 .. #8, #11, #12,
  #14 .. #31, #127 .. #255]): string;
begin
  Result := AValue;
  Result := StripCharsInSet(Result, ACharset);
  Result := StripExtraSpaces(Result, ARemoveTab, ARemoveCRLF);
end;

function TitleCase(const AText: string;
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
    for iLoop := 2 to Length(Result) do
      if (Result[iLoop - 1] in cDelimiters) then
        Result[iLoop] := UpCase(Result[iLoop]);
  end;
end;

function IsEmptyString(AValue: string): Boolean;
begin
  Result := Trim(AValue) = '';
end;

function ExtractQuotedString(const S: string; Quote: char): string;
var
  { P: PChar;
    begin
    P := PChar(S);
    if P^ = Quote then
    Result := AnsiExtractQuotedStr(P, Quote)
    else
    Result := S; }

  i: integer;
begin
  Result := S;
  i := Length(Result);
  if Length(S) >= 2 then
  begin
    if (i > 0) and (Result[1] = Quote) and (Result[i] = Quote) then
    begin
      Delete(Result, i, 1);
      Delete(Result, 1, 1);
      for i := Length(Result) downto 2 do
      begin
        if (Result[i] = Quote) and (Result[i - 1] = Quote) then
          Delete(Result, i, 1);
      end;
    end;
  end;
end;

function DecodeTimeTextToMinutes(ATime: string): integer;
var
  TempStr: string;
  idx, Hours, Minutes: integer;
begin
  TempStr := '';
  idx := 1;
  Hours := 0;
  Minutes := 0;
  while (idx <= Length(ATime)) do
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

function EncodeTimeTextFromMinutes(AMinutes: integer): string;
var
  Hours, Minutes: integer;
begin
  Hours := trunc(AMinutes / 60);
  Minutes := AMinutes - (Hours * 60);
  Result := Format('%d:%.2d', [Hours, Minutes]);
end;

function MinutesToDaysHoursMinutes(AMinutes: integer;
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

procedure GenerateMonthList(AMonths: TStrings);
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

procedure SetTimeFormat(ATimeFormat: TLazyTimeFormat);
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


function LocalDateTimeFromUTCDateTime(const UTCDateTime: TDateTime): TDateTime;
begin
  Result := TTimeZone.Local.ToLocalTime(UTCDateTime);
end;

function DateTimeStrEval(const DateTimeFormat: string;
  const DateTimeStr: string): TDateTime;
var
  i, ii, iii: integer;
  Retvar: TDateTime;
  Tmp, Fmt, Data, Mask, Spec: string;
  Year, Month, Day, Hour, Minute, Second, MSec: word;
  AmPm: integer;
  LocaleFormatSettings: TFormatSettings;
begin
  Year := 1;
  Month := 1;
  Day := 1;
  Hour := 0;
  Minute := 0;
  Second := 0;
  MSec := 0;
  Fmt := UpperCase(DateTimeFormat);
  Data := UpperCase(DateTimeStr);
  i := 1;
  Mask := '';
  AmPm := 0;

  LocaleFormatSettings := TFormatSettings.Create;

  while i < Length(Fmt) do
  begin
    if Fmt[i] in ['A', 'P', 'D', 'M', 'Y', 'H', 'N', 'S', 'Z'] then
    begin
      // Start of a date specifier
      Mask := Fmt[i];
      ii := i + 1;

      // Keep going till not valid specifier
      while True do
      begin
        if ii > Length(Fmt) then
          Break; // End of specifier string
        Spec := Mask + Fmt[ii];

        if (Spec = 'DD') or (Spec = 'DDD') or (Spec = 'DDDD') or (Spec = 'MM')
          or (Spec = 'MMM') or (Spec = 'MMMM') or (Spec = 'YY') or
          (Spec = 'YYY') or (Spec = 'YYYY') or (Spec = 'HH') or (Spec = 'NN') or
          (Spec = 'SS') or (Spec = 'ZZ') or (Spec = 'ZZZ') or (Spec = 'AP') or
          (Spec = 'AM') or (Spec = 'AMP') or (Spec = 'AMPM') or (Spec = 'AM/PM')
        then
        begin
          Mask := Spec;
          Inc(ii);
        end
        else
        begin
          // End of or Invalid specifier
          Break;
        end;
      end;

      // Got a valid specifier ? - evaluate it from data string
      if (Mask <> '') and (Length(Data) > 0) then
      begin
        // Day 1..31
        if (Mask = 'DD') then
        begin
          Day := StrToIntDef(Trim(Copy(Data, 1, 2)), 0);
          Delete(Data, 1, 2);
        end;

        // Day Sun..Sat (Just remove from data string)
        if Mask = 'DDD' then
          Delete(Data, 1, 3);

        // Day Sunday..Saturday (Just remove from data string LEN)
        if Mask = 'DDDD' then
        begin
          Tmp := Copy(Data, 1, 3);
          for iii := 1 to 7 do
          begin
            if Tmp = UpperCase(Copy(LocaleFormatSettings.LongDayNames[iii],
              1, 3)) then
            begin
              Delete(Data, 1, Length(LocaleFormatSettings.LongDayNames[iii]));
              Break;
            end;
          end;
        end;

        // Month 1..12
        if (Mask = 'MM') then
        begin
          Month := StrToIntDef(Trim(Copy(Data, 1, 2)), 0);
          Delete(Data, 1, 2);
        end;

        // Month Jan..Dec
        if Mask = 'MMM' then
        begin
          Tmp := Copy(Data, 1, 3);
          for iii := 1 to 12 do
          begin
            if Tmp = UpperCase(Copy(LocaleFormatSettings.LongMonthNames[iii],
              1, 3)) then
            begin
              Month := iii;
              Delete(Data, 1, 3);
              Break;
            end;
          end;
        end;

        // Month January..December
        if Mask = 'MMMM' then
        begin
          Tmp := Copy(Data, 1, 3);
          for iii := 1 to 12 do
          begin
            if Tmp = UpperCase(Copy(LocaleFormatSettings.LongMonthNames[iii],
              1, 3)) then
            begin
              Month := iii;
              Delete(Data, 1, Length(LocaleFormatSettings.LongMonthNames[iii]));
              Break;
            end;
          end;
        end;

        // Year 2 Digit
        if Mask = 'YY' then
        begin
          Year := StrToIntDef(Copy(Data, 1, 2), 0);
          Delete(Data, 1, 2);
          if Year < LocaleFormatSettings.TwoDigitYearCenturyWindow then
            Year := (YearOf(Date) div 100) * 100 + Year
          else
            Year := (YearOf(Date) div 100 - 1) * 100 + Year;
        end;

        // Year 4 Digit
        if Mask = 'YYYY' then
        begin
          Year := StrToIntDef(Copy(Data, 1, 4), 0);
          Delete(Data, 1, 4);
        end;

        // Hours
        if Mask = 'HH' then
        begin
          Hour := StrToIntDef(Trim(Copy(Data, 1, 2)), 0);
          Delete(Data, 1, 2);
        end;

        // Minutes
        if Mask = 'NN' then
        begin
          Minute := StrToIntDef(Trim(Copy(Data, 1, 2)), 0);
          Delete(Data, 1, 2);
        end;

        // Seconds
        if Mask = 'SS' then
        begin
          Second := StrToIntDef(Trim(Copy(Data, 1, 2)), 0);
          Delete(Data, 1, 2);
        end;

        // Milliseconds
        if (Mask = 'ZZ') or (Mask = 'ZZZ') then
        begin
          MSec := StrToIntDef(Trim(Copy(Data, 1, 3)), 0);
          Delete(Data, 1, 3);
        end;

        // AmPm A or P flag
        if (Mask = 'AP') then
        begin
          if Data[1] = 'A' then
            AmPm := -1
          else
            AmPm := 1;
          Delete(Data, 1, 1);
        end;

        // AmPm AM or PM flag
        if (Mask = 'AM') or (Mask = 'AMP') or (Mask = 'AMPM') or (Mask = 'AM/PM')
        then
        begin
          if Copy(Data, 1, 2) = 'AM' then
            AmPm := -1
          else
            AmPm := 1;
          Delete(Data, 1, 2);
        end;

        Mask := '';
        // i    := ii;
      end;
      i := ii;
    end
    else
    begin
      // Remove delimiter from data string
      if Length(Data) > 1 then
        Delete(Data, 1, 1);
      Inc(i);
    end;
  end;

  if AmPm = 1 then
    Hour := Hour + 12;
  if not TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, MSec, Retvar)
  then
  begin
    Retvar := 0.0;
  end;
  Result := Retvar;
end;

function StringToDate(AValue: string): TDate;
var
  DayIdx: integer;
  Day, Month, Year: integer;
  Date: TDate;
  DateStr, IncDayValue: string;
begin
  Result := 0;
  if Length(AValue) > 0 then
  begin
    Date := 0;

    DateStr := StringReplace(AValue, '.', '/', [rfReplaceAll]);
    // Allow 2.4.80 dates

    // Check for dates entered in format 01 or 0101 or 01012012
    Day := StrToIntDef(DateStr, 0);
    if Day <> 0 then
    begin
      Month := MonthOf(Today);
      Year := YearOf(Today);

      // Day
      if Length(AValue) >= 2 then
      begin
        Day := StrToIntDef(Copy(AValue, 1, 2), 0);
      end;

      // Month
      if (Length(DateStr) >= 4) then
      begin
        Month := StrToIntDef(Copy(AValue, 3, 2), 0);
      end;

      // Year 2 digit
      if (Length(AValue) = 6) then
      begin
        Year := ConvertDoubleDigitYear(StrToIntDef(Copy(AValue, 5, 2), 0));
      end;

      // Year 4 digit
      if (Length(AValue) = 8) then
      begin
        Year := StrToIntDef(Copy(AValue, 5, 4), 0);
      end;

      if (Day > 0) and (Month > 0) and (Year > 0) then
      begin
        DateStr := DateToStr(encodedate(Year, Month, Day));
      end;
    end;

    if (UpperCase(DateStr) = 'Y') or (UpperCase(DateStr) = 'YESTERDAY') then
      Date := (IncDay(Now, -1));
    if (UpperCase(DateStr) = 'T') or (UpperCase(DateStr) = 'TOMORROW') then
      Date := (IncDay(Now, 1));

    for DayIdx := 0 to 6 do
    begin
      case DayOfTheWeek(IncDay(Now, DayIdx)) of
        1:
          begin
            if (UpperCase(DateStr) = 'MON') or (UpperCase(DateStr) = 'MONDAY')
            then
              Date := IncDay(Now, DayIdx);
          end;
        2:
          begin
            if (UpperCase(DateStr) = 'TUE') or (UpperCase(DateStr) = 'TUESDAY')
            then
              Date := IncDay(Now, DayIdx);
          end;
        3:
          begin
            if (UpperCase(DateStr) = 'WED') or (UpperCase(DateStr) = 'WEDNESDAY')
            then
              Date := IncDay(Now, DayIdx);
          end;
        4:
          begin
            if (UpperCase(DateStr) = 'THU') or (UpperCase(DateStr) = 'THURSDAY')
            then
              Date := IncDay(Now, DayIdx);
          end;
        5:
          begin
            if (UpperCase(DateStr) = 'FRI') or (UpperCase(DateStr) = 'FRIDAY')
            then
              Date := IncDay(Now, DayIdx);
          end;
        6:
          begin
            if (UpperCase(DateStr) = 'SAT') or (UpperCase(DateStr) = 'SATURDAY')
            then
              Date := IncDay(Now, DayIdx);
          end;
        7:
          begin
            if (UpperCase(DateStr) = 'SUN') or (UpperCase(DateStr) = 'SUNDAY')
            then
              Date := IncDay(Now, DayIdx);
          end;
      end;
    end;

    if UpperCase(DateStr) = 'BOM' then
      Date := (StartOfTheMonth(Now));
    if UpperCase(DateStr) = 'EOM' then
      Date := (EndOfTheMonth(Now));

    if UpperCase(DateStr) = 'BOW' then
      Date := (StartOfTheWeek(Now));
    if UpperCase(DateStr) = 'EOW' then
      Date := (EndOfTheWeek(Now));

    if (UpperCase(DateStr)[1] = 'N') then
    begin
      Date := Now;
      if ((Pos('+', DateStr) > 0) or (Pos('-', DateStr) > 0)) then
      begin
        if (Pos('+', DateStr) > 0) then
          IncDayValue := Copy(DateStr, Pos('+', DateStr) + 1, Length(DateStr));
        if (Pos('-', DateStr) > 0) then
          IncDayValue := Copy(DateStr, Pos('-', DateStr) + 1, Length(DateStr));
        if Pos('+', DateStr) > 0 then
        begin
          Date := IncDay(Now, StrToIntDef(IncDayValue, 0));
        end
        else
        begin
          Date := IncDay(Now, StrToIntDef(IncDayValue, 0) * -1);
        end;
      end;
    end;

    if Date <> 0 then
      DateStr := DateToStr(Date);

    Result := VarToDateTime(DateStr);
  end;
end;

function StringToDateDef(AValue: string; ADefault: TDate): TDate;
begin
  try
    Result := StringToDate(AValue);
  except
    Result := ADefault;
  end;
end;

function StringToTime(AValue: string; ARounding: Boolean = False;
  ARoundUserEntries: Boolean = False; ARoundValue: integer = 10;
  ARoundingType: TLazyTimeRounding = trNearest): TTime;
var
  Time: TTime;
  TimeStr: string;
  TimeValue: integer;
  IncTimeValue: string;
  UserInput: Boolean;
begin

  if Length(AValue) > 0 then
  begin

    Time := 0;
    UserInput := True;
    // TimeStr := AValue;
    TimeStr := StringCleaner(AValue, True, True);

    if (UpperCase(TimeStr)[1] = 'S') then
    begin
      Time := Now;
      if ((Pos('+', TimeStr) > 0) or (Pos('-', TimeStr) > 0)) then
      begin
        if (Pos('+', TimeStr) > 0) then
          IncTimeValue := Copy(TimeStr, Pos('+', TimeStr) + 1, Length(TimeStr));
        if (Pos('-', TimeStr) > 0) then
          IncTimeValue := Copy(TimeStr, Pos('-', TimeStr) + 1, Length(TimeStr));
        if Pos('+', TimeStr) > 0 then
        begin
          Time := IncSecond(Now, StrToIntDef(IncTimeValue, 0));
        end
        else
        begin
          Time := IncSecond(Now, StrToIntDef(IncTimeValue, 0) * -1);
        end;
      end;
    end;

    if (UpperCase(TimeStr)[1] = 'M') or (UpperCase(TimeStr)[1] = 'N') then
    begin
      Time := Now;
      if ((Pos('+', TimeStr) > 0) or (Pos('-', TimeStr) > 0)) then
      begin
        if (Pos('+', TimeStr) > 0) then
          IncTimeValue := Copy(TimeStr, Pos('+', TimeStr) + 1, Length(TimeStr));
        if (Pos('-', TimeStr) > 0) then
          IncTimeValue := Copy(TimeStr, Pos('-', TimeStr) + 1, Length(TimeStr));
        if Pos('+', TimeStr) > 0 then
        begin
          Time := IncMinute(Now, StrToIntDef(IncTimeValue, 0));
        end
        else
        begin
          Time := IncMinute(Now, StrToIntDef(IncTimeValue, 0) * -1);
        end;
      end;
    end;

    if (UpperCase(TimeStr)[1] = 'H') then
    begin
      Time := Now;
      if ((Pos('+', TimeStr) > 0) or (Pos('-', TimeStr) > 0)) then
      begin
        if (Pos('+', TimeStr) > 0) then
          IncTimeValue := Copy(TimeStr, Pos('+', TimeStr) + 1, Length(TimeStr));
        if (Pos('-', TimeStr) > 0) then
          IncTimeValue := Copy(TimeStr, Pos('-', TimeStr) + 1, Length(TimeStr));
        if Pos('+', TimeStr) > 0 then
        begin
          Time := IncHour(Now, StrToIntDef(IncTimeValue, 0));
        end
        else
        begin
          Time := IncHour(Now, StrToIntDef(IncTimeValue, 0) * -1);
        end;
      end;
    end;

    TimeValue := StrToIntDef(TimeStr, -1);
    if TimeValue <> -1 then
    begin
      Time := DateTimeStrEval('hhnn', TimeStr);
    end;

    if Time <> 0 then
    begin
      UserInput := False; // Time has been calculated from a variable.
      TimeStr := TimeToStr(Time);
    end;

    Time := VarToDateTime(TimeStr);

    if ARounding and ((UserInput = False) or (ARoundUserEntries)) then
    begin
      Time := RoundTime(Time, ARoundValue, ARoundingType);
      TimeStr := TimeToStr(Time);
    end
    else
    begin
      TimeStr := TimeToStr(Time);
    end;
  end;

  Result := TimeOf(VarToDateTime(TimeStr));

end;

function StringToTimeDef(AValue: string; ADefault: TTime;
  ARounding: Boolean = False; ARoundUserEntries: Boolean = False;
  ARoundValue: integer = 10;
  ARoundingType: TLazyTimeRounding = trNearest): TTime;
begin
  try
    Result := StringToTime(AValue, ARounding, ARoundUserEntries, ARoundValue,
      ARoundingType);
  except
    Result := ADefault;
  end;
end;

function StringToDateTime(AValue: string): TDateTime;
var
  Date: TDate;
  Time: TTime;
begin
  Date := StringToDate(AValue);
  Time := StringToTime(AValue);
  Result := Date + Time;
end;

function StringToDateTimeDef(AValue: string; ADefault: TDateTime): TDateTime;
var
  Date: TDate;
  Time: TTime;
begin
  Date := StringToDateDef(AValue, ADefault);
  Time := StringToTimeDef(AValue, ADefault);
  Result := Date + Time;
end;

function ConvertDoubleDigitYear(AValue: integer): integer;
var
  YearWindow: integer;
  century: integer;
begin
  century := (YearOf(Now) div 1000) * 1000;
  YearWindow := YearOf(Now) - (century) + 50;
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

function BoolToInteger(ABoolean: Boolean): integer;
begin
  Result := 0;
  if ABoolean then
  begin
    Result := 1;
  end;
end;

function IntegerToBool(AValue: integer): Boolean;
begin
  Result := AValue = 1;
end;

function CalculatePercentage(AValue: integer; ATotal: integer): integer;
begin
  Result := 0;
  if ATotal > 0 then
  begin
    Result := Round((AValue / ATotal) * 100);
  end;
end;

Function HexToInt(S: String): LongInt;
const
  DecDigits: Set Of '0' .. '9' = ['0' .. '9'];
  HexVals: Array [0 .. $F] Of integer = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, $A, $B,
    $C, $D, $E, $F);
  UpCaseHexLetters: Set Of 'A' .. 'F' = ['A' .. 'F'];
  LowCaseHexLetters: Set Of 'a' .. 'f' = ['a' .. 'f'];
var
  v: LongInt;
  i: integer;
  LookUpIndex: integer;
begin
  if Length(S) <= 8 then
  begin
    v := 0;
    for i := 1 to Length(S) do
    begin
{$R-}
      v := v Shl 4;
{$R+}
      if S[i] in DecDigits then
      begin
        LookUpIndex := Ord(S[i]) - Ord('0');
      end
      else
      begin
        if S[i] in UpCaseHexLetters then
        begin
          LookUpIndex := Ord(S[i]) - Ord('A') + $A;
        end
        else
        begin
          if S[i] in LowCaseHexLetters then
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

procedure ParseDelimited(const sl: TStrings; const Value: string;
  const delimiter: string);
var
  dx: integer;
  ns: string;
  txt: string;
  delta: integer;
begin
  delta := Length(delimiter);
  txt := Value + delimiter;
  sl.BeginUpdate;
  sl.Clear;
  try
    while Length(txt) > 0 do
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

function GeneratePassword(ALength: integer): string;
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
    Result := Result + AllowedCharacters[Random(Length(AllowedCharacters)) + 1];
  end;
end;

function SplitStringToWords(const AString: string; AWords: TStrings): integer;
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
    for idx := 1 to Length(AString) + 1 do
    begin
      ValidChar := False;
      if idx <= Length(AString) then
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

function GetNthNumber(AValue: integer): string;
var
  BaseValue: integer;
begin

  Result := IntToStr(AValue);

  if Length(Result) >= 2 then
  begin
    BaseValue := StrToIntDef(Result[Length(Result) - 1] +
      Result[Length(Result)], 0);
  end
  else
  begin
    BaseValue := AValue;
  end;

  if (BaseValue >= 11) and (BaseValue <= 19) then // teens
  begin
    Result := Result + 'th';
  end;

  if Result[Length(Result)] = '1' then
  begin
    Result := Result + 'st';
  end;
  if Result[Length(Result)] = '2' then
  begin
    Result := Result + 'nd';
  end;
  if Result[Length(Result)] = '3' then
  begin
    Result := Result + 'rd';
  end;

  if StrToIntDef(Result, -1) <> -1 then
  // still just a number add default "th"
  begin
    Result := Result + 'th';
  end;
end;

function LeftPad(S: string; Ch: char; Len: integer): string;
var
  RestLen: integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := S + StringOfChar(Ch, RestLen);
end;

function RightPad(S: string; Ch: char; Len: integer): string;
var
  RestLen: integer;
begin
  Result := S;
  RestLen := Len - Length(S);
  if RestLen < 1 then
    Exit;
  Result := StringOfChar(Ch, RestLen) + S;
end;

function StrMaxLen(const S: string; MaxLen: integer): string;
begin
  Result := S;
  if Length(Result) <= MaxLen then
    Exit;
  SetLength(Result, MaxLen);
  Result[MaxLen] := '…';
end;

function FormatByteSize(const bytes: extended): string;
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

function LeadingZeroes(const ANumber, ALength: integer): string;
begin
  Result := SysUtils.Format('%.*d', [ALength, ANumber]);
end;

end.
