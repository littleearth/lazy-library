    # Lazy Library - AI Coding Assistant Guide

This guide helps AI coding assistants understand the Lazy Library architecture, patterns, and best practices for productive development.

## Quick Start Architecture Overview

The Lazy Library is a multi-platform Delphi component library with a three-tier architecture:

- **CORE**: Platform-independent foundation (utilities, REST clients, logging, models)
- **VCL**: Windows-specific components (WinAPI wrappers, system integration)  
- **FMX**: Cross-platform FireMonkey components

**Key Design Patterns:**
- **Inheritance Hierarchy**: All classes inherit from `TLZObject`, `TLZPersistent`, or `TLZComponent` for built-in logging
- **Multi-Handler Logging**: Dispatcher pattern where log messages go to all registered handlers simultaneously
- **Model-Based REST**: JSON models inherit from `TLZModel` with automatic serialization
- **OAuth2 REST Clients**: Inherit from `TLZRESTClientOAuth2Base` with automatic token management

## Essential Development Workflows

### Building the Library
```bash
# Build all packages (from Delphi IDE or command line)
# Packages are organized by platform:
packages/LazyLibrary.dpk          # Core runtime
packages/LazyLibraryVCL.dpk       # VCL components  
packages/LazyLibraryCoreDesign.dpk # Design-time

# Output goes to:
build/vcl/bin/    # VCL binaries
build/fmx/bin/    # FMX binaries
```

### Testing Strategy
```pascal
// All tests inherit from TLZTestBase in tests/Lazy.Test.Base.pas
[TestFixture]
TMyTests = class(TLZTestBase)
  [TestCase('Description', 'input,expected')]
  procedure MyTest(AInput, AExpected: string);
end;
```

### Logging Integration (Critical Pattern)
```pascal
// ALWAYS inherit from TLZ* base classes for automatic logging:
type
  TMyClass = class(TLZObject)  // Gets built-in logging methods
    procedure DoWork;
  end;

procedure TMyClass.DoWork;
begin
  Log('Starting work');                    // Info logging
  Debug('DoWork', 'Processing %d items', [Count]); // Debug with procedure name
  try
    // Work here
    Warning('Cache at %d%%', [CachePercent]); // Warnings
  except
    on E: Exception do
      Error(E);  // Auto-extracts full exception details + stack trace
  end;
end;

// Setup logging handlers (usually in app initialization):
LazyLog.AddHandler('FileLog', TLZLogFileStream);    // File logging
LazyLog.AddHandler('Console', TLZLogConsole);       // Console with colors
LazyLog.AddHandler('SQLite', TLZLogSQLite);         // Database logging
```

### REST Client Development Pattern
```pascal
// Always inherit from appropriate base:
type
  TMyAPIClient = class(TLZRESTClientOAuth2Base)
  protected
    function GetBaseURL: string; override;
  public
    procedure GetData(AResult: TMyModelList);
  end;

// OAuth2 connection is handled automatically
// Focus on API-specific methods, not auth plumbing
```

### Model Development Pattern
```pascal
// Always inherit from TLZModel for JSON support:
type
  TMyModel = class(TLZModel)
  private
    FName: string;
    FValue: Integer;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
  end;

// Generic list automatically gets JSON array support:
  TMyModelList = class(TLZModelList<TMyModel>);
```

## Critical File Locations

- **Base Types**: `source/core/base/Lazy.Types.pas` - Contains `TLZObject` hierarchy and logging integration
- **Global Logger**: `source/core/log/Lazy.Log.pas` - Multi-handler dispatcher system
- **REST Base**: `source/core/rest/Lazy.RESTClient.pas` - OAuth2 and Basic Auth base classes  
- **Model Base**: `source/core/base/Lazy.Model.pas` - JSON serialization foundation
- **Examples**: `example/` - Working examples for each major component

## Project-Specific Conventions

### Exception Handling
```pascal
// DON'T manually format exception info - it's automatic:
try
  DoSomething;
except
  on E: Exception do
    Error(E);  // Automatically includes stack trace, app context, active forms
end;
```

### Progress Tracking for Long Operations
```pascal
// Use built-in action tracking:
LazyLog.LogActionBegin(Self, 'DataSync', 'Starting sync process');
try
  for I := 0 to Count - 1 do
  begin
    // Process item
    if I mod 100 = 0 then
      LazyLog.LogActionProgress(Self, 'DataSync', (I * 100) div Count, 
        Format('Processed %d of %d', [I, Count]));
  end;
finally
  LazyLog.LogActionEnd(Self, 'DataSync');
end;

// Query active operations from UI:
var LActions := LazyLog.GetAllActions;  // Real-time progress monitoring
```

### Component Registration
```pascal
// Design-time components register in *Design.pas files:
RegisterComponents('Lazy Library - Category', [TMyComponent]);
```

### Namespace Conventions
- `Lazy.*` - Core utilities and base classes
- `Azure.*`, `MSGraph.*`, `DUO.*` - Service-specific REST clients
- `VCL.Lazy.*` - VCL-specific implementations
- `FMX.Lazy.*` - FMX-specific implementations
- `WinApi.*` - Windows API wrappers

## Integration Points

### OAuth2 Flow Integration
The library handles OAuth2 automatically but requires browser forms for auth:
- VCL: `VCL.Lazy.AuthorizeBrowserForm` 
- FMX: `FMX.Lazy.AuthorizeBrowserForm`

### Exception Detail Enhancement
Register third-party exception handlers for enhanced stack traces:
```pascal
// JclDebug, MadExcept, EurekaLog integration:
TLZExceptionDetails.RegisterHandler(TMyStackTraceHandler.Create);
```

## Component Catalog

Components are organized by category: CORE, VCL, and FMX.

---

## Table of Contents

- [Quick Start Architecture Overview](#quick-start-architecture-overview)
- [Essential Development Workflows](#essential-development-workflows)
- [Critical File Locations](#critical-file-locations)
- [Project-Specific Conventions](#project-specific-conventions)
- [Integration Points](#integration-points)
- [Component Catalog](#component-catalog)
  - [CORE Components](#core-components)
  - [VCL Components](#vcl-components)
  - [FMX Components](#fmx-components)

---

## CORE Components

Platform-independent components that work across VCL and FMX.

### Base Utilities

#### Lazy.Types

**TLZLogLevel** - Enum: `logError`, `logWarning`, `logInformation`, `logDebug`

**TLZTimeRounding** - Enum: `trNearest`, `trUp`, `trDown`

**TLZTimeFormat** - Enum: `tfDefault`, `tf12Hour`, `tf24Hour`

**TLZFieldNameCase** - Enum: `fncSnakeCase`, `fncKebabCase`, `fncCamelCase`, `fncPascalCase`

**TLZObject** - Base class for all Lazy objects with logging methods
- `procedure Log(AMessage: string)`
- `procedure Log(AMessage: string; const Args: array of const)` - Format overload
- `procedure Debug(AProcedure: string; AMessage: string)`
- `procedure Debug(AProcedure: string; AMessage: string; const Args: array of const)` - Format overload
- `procedure Warning(AMessage: string)`
- `procedure Warning(AMessage: string; const Args: array of const)` - Format overload
- `procedure Error(AMessage: string; AErrorCode: integer = 0)` 
- `procedure Error(AMessage: string; const Args: array of const; AErrorCode: integer = 0)` - Format overload
- `procedure Error(AException: Exception; AMessage: string = '')`
- `procedure Error(AException: Exception; AMessage: string; const Args: array of const)` - Format overload

**TLZPersistent** - Base persistent class with logging methods
- Same methods as TLZObject

**TLZComponent** - Base component class with logging methods
- Same methods as TLZObject

**Usage in Descendants**:
```delphi
type
  TMyClass = class(TLZObject)
    procedure ProcessData;
  end;

procedure TMyClass.ProcessData;
begin
  Log('Starting data processing');
  Debug('ProcessData', 'Found %d records', [RecordCount]);
  Warning('Cache usage at %d%%', [CachePercent]);
end;
```

**TApplicationVersionDetails** - Version management
- `procedure Reset`
- `function AsString: string`
- `function IsCurrent(ACompareVersion: string): boolean`
- `property Version: string`
- `property MajorVersion: integer`
- `property MinorVersion: integer`
- `property ReleaseVersion: integer`
- `property BuildVersion: integer`

---

#### Lazy.Model

**TLZModel** - Base model class with JSON support
- `constructor Create`
- `constructor CreateClone(ASource: TLZModel)`
- `class function CreateModel<T: TLZModel>: T`
- `procedure Clone(ASource: TLZModel)`
- `procedure Assign(ASource: TLZModel)`
- `procedure FromJSONValue(AJSONValue: TJSONValue)` - abstract
- `procedure FromJSON(AJSON: string)`
- `function ToJSONValue: TJSONValue` - abstract
- `function ToJSON(AFormat: boolean = false): string`

**TLZModelList<T>** - Generic model list with JSON support
- `constructor Create`
- `constructor CreateClone(ASource: TLZModelList<T>)`
- `procedure Assign(ASource: TLZModelList<T>)`
- `procedure Add(ASource: T)`
- `function Add: T`
- `procedure Clear`
- `procedure FromJSONValue(AJSONValue: TJSONValue; AClear: boolean = true; AArrayName: string = ''; AFreeJSONValue: boolean = false)`
- `procedure FromJSON(AJSON: string)`
- `procedure ToJSONValue(var AJSONValue: TJSONValue; AArrayName: string = '')`
- `function ToJSON(AFormat: boolean = false): string`
- `function GetEnumerator: TEnumerator<T>`
- `property Count: integer`
- `property Model[AIndex: integer]: T` - default
- `property Models: TObjectList<T>`

---

#### Lazy.Utils.Base

**TLZFileBase** - File and folder operations
- `class function GetTempFolder: string`
- `class function GetGUIDFileName(AFolder, APrefix, AExtension: string): string`
- `class function GetTempFile(APrefix: string; AExtension: string = '.tmp'; AFolder: string = ''): string`
- `class function GetApplicationDir: string`
- `class function CheckDirectoryExists(ADirectory: string; ACreate: Boolean): Boolean`
- `class function IsValidFileName(AFileName: TFileName): Boolean`
- `class function ValidateFileName(AFileName: TFileName): TFileName`
- `class function ExtractUrlFileName(const AURL: string): string`
- `class procedure QuickFileSearch(const PathName, FileName: string; const Recurse: Boolean; FileList: TStrings)`
- `class function RandomFileName(ALength: integer): string`
- `class function Copy(ASource: string; ADestination: string; AOverwrite: Boolean = true): Boolean`
- `class function Move(ASource: string; ADestination: string): Boolean`
- `class function Delete(AFileName: string): Boolean`

**TLZSystemBase** - System operations
- `class function GetApplicationParameters(AParameter: string; var AValue: string): Boolean`
- `class function GetApplicationDir: string`

**TLZStringBase** - String utilities
- `class function RemoveHTMLTags(const S: string): string`
- `class function IsEmptyString(AValue: string): Boolean`
- `class procedure ParseDelimited(const sl: TStrings; const Value: string; const delimiter: string)`
- `class function ExtractQuotedString(const S: string; Quote: char): string`
- `class function StripNonAlphaNumeric(const AValue: string): string`
- `class function StripNonNumeric(const AValue: string; AAllowDecimal: Boolean = False; AAllowNegative: Boolean = False): string`
- `class function StripCharsInSet(const AValue: string; ACharset: TSysCharSet): string`
- `class function StipNonStandard(const AValue: string): string`
- `class function EncodeHTML(AValue: String): string`
- `class function DecodeHTML(AValue: String): string`
- `class function StripHTML(S: string): string`
- `class function StripExtraSpaces(AValue: string; ARemoveTab: Boolean = False; ARemoveCRLF: Boolean = False): string`
- `class function StringCleaner(const AValue: string; ARemoveTab: Boolean = False; ARemoveCRLF: Boolean = False; ACharset: TSysCharSet): string`
- `class function ContainsNonAlphaNumberic(const AValue: string): Boolean`
- `class function TitleCase(const AText: string; const ALowerCaseFirst: Boolean = true): string`
- `class function GeneratePassword(ALength: integer): string`
- `class function LeadingZeroes(const ANumber, ALength: integer): string`
- `class function SplitStringToWords(const AString: string; AWords: TStrings): integer`
- `class function GetNthNumber(AValue: integer): string`
- `class function LeftPad(S: string; Ch: char; Len: integer): string`
- `class function RightPad(S: string; Ch: char; Len: integer): string`
- `class function StrMaxLen(const S: string; MaxLen: integer): string`
- `class function FormatByteSize(const bytes: extended): string`

**TLZDateTimeBase** - Date and time utilities
- `class procedure SetNowMock(AFunction: TDateTimeFunc)` - for testing
- `class function RoundTime(ADateTime: TDateTime; AMins: double; ARounding: TLZTimeRounding = trNearest): TDateTime`
- `class function EncodeTimeTextFromMinutes(AMinutes: integer): string`
- `class function DecodeTimeTextToMinutes(ATime: string): integer`
- `class function MinutesToDaysHoursMinutes(AMinutes: integer; AHoursPerDay: integer = 24; ALongFormat: Boolean = False): string`
- `class procedure GenerateMonthList(AMonths: TStrings)`
- `class procedure SetTimeFormat(ATimeFormat: TLZTimeFormat)`
- `class function LocalDateTimeFromUTCDateTime(const UTCDateTime: TDateTime): TDateTime`
- `class function DateTimeStrEval(const ADateTimeFormat: string; const ADateTimeStr: string): TDateTime`
- `class function StringToDate(AValue: string): TDate`
- `class function StringToDateDef(AValue: string; ADefault: TDate): TDate`
- `class function StringToTime(AValue: string; ARounding: Boolean = False; ARoundUserEntries: Boolean = False; ARoundValue: integer = 10; ARoundingType: TLZTimeRounding = trNearest): TTime`
- `class function StringToTimeDef(AValue: string; ADefault: TTime; ARounding: Boolean = False; ARoundUserEntries: Boolean = False; ARoundValue: integer = 10; ARoundingType: TLZTimeRounding = trNearest): TTime`
- `class function StringToDateTime(AValue: string): TDateTime`
- `class function StringToDateTimeDef(AValue: string; ADefault: TDateTime): TDateTime`
- `class function ConvertDoubleDigitYear(AValue: integer): integer`
- `class function MinutesToMetricMinutes(AMinutes: integer): integer`
- `class function MetricMinutesToMinutes(AMetricMinutes: integer): integer`
- `class function GetEasterDate(AYear: integer; var AEaster: TDateTime): Boolean`
- `class function DateTimeToString(ADateTime: TDateTime; ANullText: string = ''): string`
- `class function DateTimeToString(ADateTime: TDateTime; AFormatSettings: TFormatSettings; ANullText: string = ''): string`
- `class function DateToString(ADate: TDate; ANullText: string = ''): string`
- `class function DateToString(ADate: TDateTime; ANullText: string = ''): string`
- `class function DateToString(ADate: TDateTime; AFormatSettings: TFormatSettings; ANullText: string = ''): string`
- `class function TimeToString(ATime: TTime; ANullText: string = ''): string`
- `class function TimeToString(ATime: TDateTime; ANullText: string = ''): string`
- `class function TimeToString(ATime: TDateTime; AFormatSettings: TFormatSettings; ANullText: string = ''): string`
- `class function DateTimeDifference(AStartDate: TDateTime; AEndDate: TDateTime): string`
- `class function DateTimeDifference(AStartDate: TDateTime; AEndDate: TDateTime; var ADays: word; var AHours: word; var AMinutes: word; var ASeconds: word): string`
- `class function DurationFromMinutes(AMinutes: integer; AHoursMinutesOnly: Boolean = False; ADateTimeFormat: string = 'hh:nn'): string`
- `class function DurationFromMilliseconds(AMilliseconds: Int64; AHoursMinutesOnly: Boolean = False; ADateTimeFormat: string = 'hh:nn:ss.z'): string`
- `class function DurationString(AElapsed: TDateTime; AHoursMinutesOnly: Boolean = False; ADateTimeFormat: string = 'hh:nn:ss.z'): string`
- `class function StringToDuration(AValue: string; var AHours, AMinutes: integer): Boolean`

**TLZBooleanBase** - Boolean utilities
- `class function BoolToInteger(ABoolean: Boolean): integer`
- `class function IntegerToBool(AValue: integer): Boolean`
- `class function StringToBool(AValue: string): Boolean`

**TLZMathBase** - Math utilities
- `class function CalculatePercentage(AValue: integer; ATotal: integer): integer`
- `class function HexToInt(S: String): LongInt`

**TLZURLBase** - URL utilities
- `class function IsValidURL(AURL: string): Boolean`

---

#### Lazy.Base64

**TLZBase64** - Base64 encoding/decoding
- `class function ToFile(ABase64: string; AFileName: string): boolean`
- `class function FromFile(AFileName: string; var ABase64: string): boolean`
- `class function ToField(ABase64: string; AField: TField): boolean`
- `class function FromField(AField: TField; var ABase64: string): boolean`

---

#### Lazy.Compare

**TLZCompareMode** - Enum: `vcString`, `vcInteger`, `vcFloat`, `vcDate`, `vcTime`, `vcDateTime`, `vcVersionString`, `vcVersionFile`, `vcMD5String`, `vcMD5File`, `vcGetMD5`, `vcGetVersionFile`, `vcGetVersionProduct`

**TLZCompareBase** - Comparison utilities
- `class function CompareVersion(AVersion1: string; AVersion2: string): integer`
- `class function CompareInteger(AValue1, AValue2: string): integer`
- `class function CompareFloat(AValue1, AValue2: string): integer`
- `class function CompareDate(AValue1, AValue2: string): integer`
- `class function CompareTime(AValue1, AValue2: string): integer`
- `class function CompareDateTime(AValue1, AValue2: string): integer`
- `class function GenerateMD5(AFilename: TFileName): string`

---

#### Lazy.CryptINI

**TLZCryptINI** - Encrypted INI file (inherits from TMemIniFile)
- `constructor Create(const AFileName: TFileName; AKey: string = CRYPTINI_DEFAULT_KEY; ASaveOnDestroy: Boolean = False)`
- `procedure UpdateFile`
- `procedure LoadFile`
- `procedure WriteStrings(ASection: string; AStrings: TStrings)`
- `procedure ReadStrings(ASection: string; AStrings: TStrings)`
- `property Key: string`
- `property SaveOnDestroy: Boolean`

---

#### Lazy.CSVJSONUtils

**TLZCSVQuoteMode** - Enum: `qmMinimal`, `qmAll`, `qmNonNumeric`, `qmNone`

**TLZDetectedCaseType** - Enum: `dctUnknown`, `dctPascalCase`, `dctCamelCase`, `dctSnakeCase`, `dctKebabCase`, `dctSpaceCase`, `dctMixed`

**TLZFieldNameProcessor** - Field name case conversion
- `constructor Create`
- `function ConvertFieldNameCase(const AFieldName: string; ACaseType: TLZFieldNameCase): string`
- `function ProcessFieldName(const AOriginalName: string): string`
- `procedure ClearFieldNames`
- `property FieldNameCase: TLZFieldNameCase`
- `property OnFieldNameConflict: TFieldNameConflictEvent`
- `property FieldNames: TStringList`

**TLZCSVParser** - CSV parsing
- `class function ParseCSVLine(const ALine: string; ADelimiter: Char = ','): TStringList`

**TLZCSVFormatter** - CSV formatting
- `class function FormatCSVLine(AFields: TStringList; ADelimiter: Char = ','; AQuoteChar: Char = '"'; AQuoteMode: TLZCSVQuoteMode = qmMinimal): string`

---

#### Lazy.CSVtoJSON

**TLZCSVToJSON** - CSV to JSON conversion
- `constructor Create`
- `class function Convert(ACSVData: string; AFieldNameCase: TLZFieldNameCase = fncPascalCase; const AArrayName: string = ''): TJSONValue`
- `function ConvertCSVToJSON(const ACSVData: string): TJSONValue`
- `property FieldNameCase: TLZFieldNameCase`
- `property OnFieldNameConflict: TFieldNameConflictEvent`
- `property ArrayName: string`

---

#### Lazy.JSONtoCSV

**TLZJSONToCSV** - JSON to CSV conversion
- `constructor Create`
- `class function Convert(AJSONData: TJSONValue; AFieldNameCase: TLZFieldNameCase = fncPascalCase; ADelimiter: Char = ','; AIncludeHeaders: Boolean = True; AQuoteMode: TLZCSVQuoteMode = qmMinimal): string`
- `class function Convert(const AJSONString: string; AFieldNameCase: TLZFieldNameCase = fncPascalCase; ADelimiter: Char = ','; AIncludeHeaders: Boolean = True; AQuoteMode: TLZCSVQuoteMode = qmMinimal): string`
- `function ConvertJSONToCSV(AJSONData: TJSONValue): string`
- `function ConvertJSONToCSV(const AJSONString: string): string`
- `property FieldNameCase: TLZFieldNameCase`
- `property OnFieldNameConflict: TFieldNameConflictEvent`
- `property Delimiter: Char`
- `property QuoteChar: Char`
- `property QuoteMode: TLZCSVQuoteMode`
- `property IncludeHeaders: Boolean`

---

#### Lazy.JSONFormatter

**TJSONFormatMode** - Enum: `jfmCompact`, `jfmFormatted`

**TLZJSONFormatter** - JSON formatting and field name conversion
- `constructor Create`
- `class function Format(const AJSONString: string; AFieldNameCase: TLZFieldNameCase = fncPascalCase; const AArrayName: string = ''; ARemoveArrayWrapper: Boolean = False; AFormatMode: TJSONFormatMode = jfmFormatted): string`
- `class function Format(AJSONValue: TJSONValue; AFieldNameCase: TLZFieldNameCase = fncPascalCase; const AArrayName: string = ''; ARemoveArrayWrapper: Boolean = False; AFormatMode: TJSONFormatMode = jfmFormatted): string`
- `class function Format(AJSONValue: TJSONValue; AFormatMode: TJSONFormatMode): string`
- `function FormatJSON(const AJSONString: string): string`
- `function FormatJSON(AJSONValue: TJSONValue): string`
- `property FieldNameCase: TLZFieldNameCase`
- `property OnFieldNameConflict: TFieldNameConflictEvent`
- `property ArrayName: string`
- `property RemoveArrayWrapper: Boolean`
- `property FormatMode: TJSONFormatMode`
- `property IndentSize: Integer`

---

#### Lazy.GUID

**TLZGUID** - GUID utilities
- `class function NewGuid: TGuid`
- `class function EmptyGuid: TGuid`
- `class function IsEmptyGuid(GUID: TGuid): boolean`
- `class function ToString(GUID: TGuid): string`
- `class function ToQuotedString(GUID: TGuid): string`
- `class function FromString(Value: string): TGuid`
- `class function EqualGuids(Guid1, Guid2: TGuid): boolean`

---

#### Lazy.ISO8601

**TLZUtc** - UTC conversion
- `class function FromUtc(const Value: TDateTime): TDateTime`
- `class function ToUtc(const Value: TDateTime): TDateTime`
- `class function UtcNow: TDateTime`

**TLZToIso8601** - Convert to ISO8601
- `class function DateTimeToIso8601(const Value: TDateTime; AReturnUTC: boolean = false): string`
- `class function DateToIso8601(const Value: TDate): string`
- `class function TimeToIso8601(const Value: TTime): string`
- `class function UtcTimeToIso8601(const Value: TTime): string`

**TLZIso8601** - ISO8601 conversion
- `class function DateFromIso8601(const Value: string): TDate`
- `class function DateTimeFromIso8601(const Value: string; AReturnUTC: boolean = false): TDateTime`
- `class function TimeFromIso8601(const Value: string): TTime`
- `class function UtcDateTimeToIso8601(const Value: TDateTime): string`

---

#### Lazy.MD5

**TLZMD5Mode** - Enum: `md5Generate`, `md5Compare`

**TLZMD5** - MD5 hashing
- `function LoadMD5(AFileName: TFileName): string`
- `function SaveMD5(AFileName: TFileName; AMD5: string): Boolean`
- `function GenerateMD5(AFileName: TFileName): string`
- `function CompareMD5(AFileName: TFileName): Boolean`

---

#### Lazy.NATO

**TLZNATO** - NATO phonetic alphabet
- `class function AsNATO(AValue: string): string`

---

#### Lazy.Nullable

**TLZNullable<T>** - Generic nullable type
- `constructor Create(AValue: T)`
- `function GetValueOrDefault: T`
- `function GetValueOrDefault(Default: T): T`
- `procedure SetValue(AValue: T)`
- `function ToString: string`
- `function IsNull: Boolean`
- `procedure Clear`
- `property HasValue: Boolean`
- `property Value: T`

**Type Aliases**:
- `TLZNullableInteger = TLZNullable<Integer>`
- `TLZNullableString = TLZNullable<String>`
- `TLZNullableDate = TLZNullable<Tdate>`
- `TLZNullableTime = TLZNullable<TTime>`
- `TLZNullableDateTime = TLZNullable<TDateTime>`
- `TLZNullableDouble = TLZNullable<Double>`
- `TLZNullableSingle = TLZNullable<Single>`
- `TLZNullableCurrency = TLZNullable<Currency>`
- `TLZNullableBoolean = TLZNullable<Boolean>`

**Helpers** (for nullable types and base types):
- `TTLZNullableTimeHelper`
- `TTLZNullableDateHelper`
- `TTLZNullableDateTimeHelper`
- `TTLZNullableBooleanHelper`
- `TDateTimeHelper`
- `TDateHelper`
- `TTimeHelper`
- `TBooleanHelper`

Methods include: `FromString`, `ToString`, `FromJSON`, `FromInteger`, `ToInteger`

---

#### Lazy.Passwords

**TLZPasswords** - Password generation and hashing
- `class function Generate(ALength: integer; AUpperCase: boolean = true; ALowerCase: boolean = true; ADigits: boolean = true; AAllowedSpecialCharacters: string = ''): string`
- `class function GeneratePhrase: string`
- `class function HashPassword(const APassword: string): string`
- `class function ValidatePasswordHash(const APassword, AHash: string): boolean`

---

#### Lazy.StopWatch

**TLZTimeElapsedEntry** - Single elapsed time entry
- `property IsRunning: boolean`
- `property StartTime: TDateTime`
- `property EndTime: TDateTime`
- `property ElapsedMilliseconds: Int64`

**TLZStopWatch** - Stopwatch with pause/resume support
- `constructor Create(const AStartOnCreate: boolean = false)`
- `procedure Start`
- `procedure Stop`
- `procedure Pause`
- `procedure Resume`
- `procedure Reset`
- `property Count: integer`
- `property ElapsedEntry[AIndex: integer]: TLZTimeElapsedEntry`
- `property ElapsedMilliseconds: Int64`
- `property RuntimeMilliseconds: Int64`
- `property Elapsed: string`
- `property RuntimeElapsed: string`
- `property ElapsedFormat: string`
- `property Status: string`
- `property IsRunning: boolean`
- `property IsPaused: boolean`
- `property StartTime: TDateTime`
- `property EndTime: TDateTime`

---

#### Lazy.StringDifference

**TLZStringDifferenceType** - Enum: `dtNone`, `dtAdded`, `dtRemoved`, `dtModified`

**TLZStringDifference** - String comparison and diff
- `constructor Create`
- `function Compare(const Str1, Str2: string): Boolean`
- `function GetDifferenceReport: string`
- `procedure Clear`
- `property Differences: TLZStringDifferenceList`
- `property IgnoreCase: Boolean`
- `property IgnoreCRLF: Boolean`
- `property IgnoreWhitespace: Boolean`

---

#### Lazy.ThreadedFileStream

**TLZThreadedFileStream** - Thread-safe file stream (inherits from TThread)
- `constructor Create(const AFileName: string)`
- `function Write(const Buffer; Count: Longint): Longint`
- `procedure AppendStr(const S: RawByteString)`
- `procedure AppendStr(const S: UTF8String)`
- `procedure AppendStr(const S: UnicodeString)`
- `procedure Flush`
- `procedure Clear`
- `property FileTime: TDateTime`
- `property FlushInterval: Cardinal`

---

#### Lazy.ThreadedStringList

**TLZThreadStringList** - Thread-safe string list
- `constructor Create`
- `function LockList: boolean`
- `procedure UnlockList`
- `function Add(const S: string): integer`
- `procedure AddStrings(Strings: TStrings)`
- `procedure Delete(Index: integer)`
- `procedure Clear`
- `procedure Exchange(Index1, Index2: integer)`
- `function Find(const S: string; var Index: integer): boolean`
- `procedure Insert(Index: integer; const S: string)`
- `function IndexOf(const S: string): integer`
- `function IndexOfName(const Name: string): integer`
- `procedure Sort`
- `function GetText: PChar`
- `procedure LoadFromFile(const FileName: string)`
- `procedure LoadFromStream(Stream: TStream)`
- `procedure SaveToFile(const FileName: string)`
- `procedure SaveToStream(Stream: TStream)`
- `property Duplicates: TDuplicates`
- `property Capacity: integer`
- `property CommaText: string`
- `property Count: integer`
- `property Delimiter: Char`
- `property DelimitedText: string`
- `property Names[Index: integer]: string`
- `property Values[const Name: string]: string`
- `property Strings[Index: integer]: string` - default
- `property Text: string`
- `property Sorted: boolean`

---

#### Lazy.Token

**TLZToken** - String tokenizer
- `constructor Create(ASource: string; ASeperator: Char)`
- `constructor Create(ASource: string; ASeperators: TLZTokenSeperators = [])`
- `constructor Create`
- `function TokenExists(AValue: string; ACaseSensitive: Boolean = false): Boolean`
- `class function GetTokens(ASource: string; ASeperators: TLZTokenSeperators): TStrings`
- `class function GetTokens(ASource: string): TStrings`
- `property Tokens[AIndex: integer]: string`
- `property Strings: TStringList`
- `property Source: string`
- `property Seperator: Char`
- `property Text: string`
- `property OutOfBoundsException: Boolean`
- `property OutOfBoundsValue: string`
- `property Count: integer`

---

### REST API Clients

#### Lazy.REST.Types

**TLZOAuth2ConnectionHeader** - HTTP header for OAuth2
- `property Name: string`
- `property Value: string`

**TLZOAuth2URLVariable** - URL variable for OAuth2
- `property Name: string`
- `property Value: string`
- `property Required: boolean`

**TLZOAuth2Connection** - OAuth2 connection configuration
- `constructor Create`
- `property ClientID: string`
- `property ClientSecret: string`
- `property RedirectURI: string`
- `property TenantID: string`
- `property AuthURL: string`
- `property TokenURL: string`
- `property Scope: string`
- `property BaseURL: string`
- `property Headers: TLZOAuth2ConnectionHeaders`
- `property URLVariables: TLZOAuth2URLVariables`

**TLZOAuth2Token** - OAuth2 token
- `constructor Create`
- `procedure Clear`
- `function IsValid: boolean`
- `property AccessToken: string`
- `property RefreshToken: string`
- `property TokenType: string`
- `property ExpiresIn: integer`
- `property ExpiresAt: TDateTime`
- `property Scope: string`

**TLZOData** - OData query helper
- `constructor Create`
- `function GetQueryString: string`
- `property Filter: string`
- `property Select: string`
- `property OrderBy: string`
- `property Top: integer`
- `property Skip: integer`
- `property Expand: string`
- `property Count: boolean`

---

#### Lazy.RESTClient

**TLZRESTClient** - Base REST client
- `constructor Create`
- `procedure Execute(ARequest: TRESTRequest; AResponse: TRESTResponse)`
- `function GET(AURL: string; AResponse: TRESTResponse): boolean`
- `function POST(AURL: string; ABody: string; AResponse: TRESTResponse): boolean`
- `function PUT(AURL: string; ABody: string; AResponse: TRESTResponse): boolean`
- `function PATCH(AURL: string; ABody: string; AResponse: TRESTResponse): boolean`
- `function DELETE(AURL: string; AResponse: TRESTResponse): boolean`
- `property BaseURL: string`
- `property OnLog: TOnLog`
- `property OnWarning: TOnWarning`
- `property OnDebug: TOnDebug`
- `property OnError: TOnError`
- `property OnProgress: TOnProgress`

**TLZRESTClientBasicAuth** - Basic authentication REST client
- All TLZRESTClient methods
- `property Username: string`
- `property Password: string`

**TLZRESTClientOAuth2Base** - OAuth2 REST client
- All TLZRESTClient methods
- `function Authorize: boolean`
- `function GetToken: boolean`
- `function RefreshToken: boolean`
- `property Connection: TLZOAuth2Connection`
- `property Token: TLZOAuth2Token`

**TLZRESTClientOAuth2** - OAuth2 with browser authorization
- All TLZRESTClientOAuth2Base methods
- Implements browser-based OAuth2 flow

---

#### Azure.Core & Azure.Management

**TLZAzureOAuth2Connection** - Azure OAuth2 connection
- Inherits from TLZOAuth2Connection
- Pre-configured for Azure

**TLZAzureOAuth2Token** - Azure OAuth2 token
- Inherits from TLZOAuth2Token

**TLZAzureManagement** - Azure Management API client
- All TLZRESTClientOAuth2Base methods
- Azure-specific endpoints and operations

---

#### MSGraph.Core, MSGraph.API & MSGraph.Models

**TLZMSGraphAPI** - Microsoft Graph API client
- All TLZRESTClientOAuth2Base methods
- `procedure GetUsers(AUsers: TLZMSGraphUsers; AFilter: string = '')`
- `procedure GetUser(AUserID: string; AUser: TLZMSGraphUser)`
- `procedure GetGroups(AGroups: TLZMSGraphGroups; AFilter: string = '')`
- `procedure GetGroup(AGroupID: string; AGroup: TLZMSGraphGroup)`
- `procedure GetGroupMembers(AGroupID: string; AMembers: TLZMSGraphGroupMembers)`
- `procedure GetDomains(ADomains: TLZMSGraphDomains)`
- `procedure GetLicenseDetails(ALicenses: TLZMSGraphLicenseDetails)`
- `procedure GetSubscriptions(ASubscriptions: TLZMSGraphCompanySubscriptions)`
- `procedure GetSites(ASites: TLZMSGraphSites)`
- `procedure GetDrives(ADrives: TLZMSGraphDrives; ASiteID: string = '')`
- `procedure GetDriveItems(ADriveID: string; AItems: TLZMSGraphDriveItems; AFolderPath: string = '')`
- `procedure GetMailFolders(AUserID: string; AFolders: TLZMSGraphMailFolders)`
- `procedure GetMailRules(AUserID: string; ARules: TLZMSGraphMailRules)`
- `procedure GetAuditSignIns(ASignIns: TLZMSGraphAuditSignIns; AFilter: string = '')`
- Additional user, group, mail, drive, and audit operations

**Models** (all inherit from TLZMSGraphModel):
- `TLZMSGraphUser`
- `TLZMSGraphGroup`
- `TLZMSGraphDomain`
- `TLZMSGraphLicenseDetail`
- `TLZMSGraphCompanySubscription`
- `TLZMSGraphSite`
- `TLZMSGraphDrive`
- `TLZMSGraphDriveItem`
- `TLZMSGraphMailFolder`
- `TLZMSGraphMailRule`
- `TLZMSGraphAuditSignIn`
- `TLZMSGraphApplication`
- Authentication method models

Each model includes:
- `FromJSONValue` and `ToJSONValue` methods
- Properties specific to the Microsoft Graph entity

---

#### DUO.API, DUO.Auth, DUO.Admin, DUO.Accounts & DUO.Models

**TLZDUOAuthAPI** - DUO Authentication API
- `function Ping: boolean`
- `function Check: boolean`
- `function Preauth(AUsername: string; var AResult: string; var ADevices: TLZDUODevices): boolean`
- `function Auth(AUsername: string; AFactor: string; ADevice: string = ''): boolean`

**TLZDUOAdminApi** - DUO Admin API
- `procedure GetUsers(AUsers: TLZDUOUsers)`
- `procedure GetUser(AUserID: string; AUser: TLZDUOUser)`
- `procedure GetPhones(APhones: TLZDUOPhones)`
- Additional user and device management

**TLZDUOAccountsApi** - DUO Accounts API
- Account management operations

**Models** (all inherit from TLZDuoModel):
- `TLZDUOUser`
- `TLZDUODevice`
- `TLZDUOPhone`
- `TLZDUOAccount`

---

#### UniFi.Api & UniFi.Models

**TLZUniFiClient** - UniFi Controller API client
- `function Login: boolean`
- `function Logout: boolean`
- `procedure GetSites(ASites: TLZUniFiSites)`
- `procedure GetDevices(ASite: string; ADevices: TLZUniFiDevices)`
- `procedure GetWLANConfs(ASite: string; AWLANs: TLZUniFiWLANConfs)`
- Additional UniFi operations

**Models**:
- `TLZUniFiSite`
- `TLZUniFiDevice`
- `TLZUniFiWLANConf`

---

#### PWA.Core

**TLZPWACore** - Pulseway API base
- Inherits from TLZRESTClientBasicAuth

**TLZPWANotifications** - Pulseway notifications
- `function SendNotification(ATitle: string; AMessage: string; APriority: string = 'normal'): boolean`

---

#### SASBOSS.Api & SASBOSS.Models

**TLZSASBOSSProvisioningClient** - SASBOSS provisioning API
- Enterprise and group management

**TLZSASBOSSBillingClient** - SASBOSS billing API
- Invoice and billing operations

**Models**:
- `TLZSASBOSSEnterprise`
- `TLZSASBOSSGroup`
- `TLZSASBOSSInvoice`
- `TLZSASBOSSInvoiceItem`

---

#### O365.WebHook

**TLZO365WebHook** - Office 365 webhook client
- `function SendMessage(AWebHookURL: string; AMessage: TLZO365WebHookMessage): boolean`

**TLZO365WebHookMessage** - Base webhook message

**TLZO365WebHookSimpleText** - Simple text message
- `property Text: string`

---

### Logging

The logging system uses a multi-handler dispatcher pattern where log messages are sent to all registered handlers simultaneously. Each handler can process messages differently (file, memory, database, remote, etc.).

#### Lazy.Log

**TLazyLogMessage** - Unified log message object passed to all handlers
- `property ApplicationName: string` - Set via `TLZLog.ApplicationName` class variable
- `property DateTime: TDateTime` - Automatically captured
- `property ThreadID: Cardinal` - Automatically captured
- `property LogClass: string` - Sender's class name
- `property LogType: TLazyLogType` - ltError, ltWarning, ltInformation, ltDebug, ltProgress
- `property LogMessage: string` - The log message text (automatically enhanced with exception details when logging exceptions)
- `property LogProcedure: string` - Procedure name (for Debug)
- `property LogLevel: TLZLogLevel` - logError, logWarning, logInformation, logDebug
- `property ErrorCode: integer` - Error code (if applicable)
- `property Exception: Exception` - Exception object (if applicable)
- `property StackTrace: string` - Stack trace (automatically populated from TLZExceptionDetails when logging exceptions)
- `property ActionName: string` - Action name (for progress tracking)
- `property ActionStatus: TLazyLogActionStatus` - asBegin, asProgress, asEnd
- `property ActionProgress: integer` - Progress percentage (0-100)

**TLazyLogAction** - Active action tracking for long-running operations
- `property ThreadID: Cardinal` - Thread running the action
- `property ActionName: string` - Name of the action
- `property ActionProgress: integer` - Current progress (0-100)
- `property ActionStatus: TLazyLogActionStatus` - asBegin, asProgress, asEnd
- `property ActionMessage: string` - Latest status message
- `property StartDateTime: TDateTime` - When action started
- `property LastUpdateDateTime: TDateTime` - Last progress update
- `function GetActionKey: string` - Returns unique key "ThreadID_ActionName"
- `function GetDuration: string` - Returns formatted duration "hh:nn:ss"

**TLZLogHandler** - Abstract base class for all log handlers
- `constructor Create`
- `procedure ProcessLogMessage(ALogMessage: TLazyLogMessage)` - **Override this in descendants**
- `property LogLevel: TLZLogLevel`
- `property Name: string`

**TLZLog** - Main log dispatcher class
- `class var ApplicationName: string` - Set application name for all log messages
- `constructor Create`
- `destructor Destroy`
- `class function GetLogLevelText(ALogLevel: TLZLogLevel): string`

*Handler Management:*
- `function AddHandler(AName: string; AHandlerClass: TLZLogHandlerClass): TLZLogHandler`
- `procedure RemoveHandler(AName: string)`
- `function FindHandler(AName: string): TLZLogHandler`
- `procedure ClearHandlers`
- `property Handlers: TLZLogHandlerList`

*Action Tracking:*
- `function GetAction(AThreadID: Cardinal; AActionName: string): TLazyLogAction` - Get specific action
- `function GetAllActions: TArray<TLazyLogAction>` - Get all active actions
- `function GetCurrentThreadActions: TArray<TLazyLogAction>` - Get current thread's actions

*Standard Logging Methods (all have Format overloads):*
- `procedure Log(ASender: TObject; AMessage: string)`
- `procedure Log(ASender: TObject; AMessage: string; const Args: array of const)`
- `procedure Debug(ASender: TObject; AProcedure: string; AMessage: string)`
- `procedure Debug(ASender: TObject; AProcedure: string; AMessage: string; const Args: array of const)`
- `procedure Warning(ASender: TObject; AMessage: string)`
- `procedure Warning(ASender: TObject; AMessage: string; const Args: array of const)`
- `procedure Error(ASender: TObject; AMessage: string; AErrorCode: integer = 0)`
- `procedure Error(ASender: TObject; AMessage: string; const Args: array of const; AErrorCode: integer = 0)`
- `procedure Error(ASender: TObject; AException: Exception; AMessage: string = '')` - **Auto-extracts exception details**
- `procedure Error(ASender: TObject; AException: Exception; AMessage: string; const Args: array of const)` - **Auto-extracts exception details**
- `procedure Error(ASender: string; AException: Exception; AMessage: string = '')` - **Auto-extracts exception details**

*Progress Tracking Methods:*
- `procedure LogActionBegin(ASender: TObject; AActionName: string; AStatus: string = '')`
- `procedure LogActionEnd(ASender: TObject; AActionName: string)`
- `procedure LogActionProgress(ASender: TObject; AActionName: string; AProgress: integer; AStatus: string = '')`

- `property LogLevel: TLZLogLevel`

**Global Functions** (all have Format overloads):
- `procedure SetLazyLogClass(ALazyLogClass: TLZLogClass)` - (Legacy, use AddHandler instead)
- `function LazyLog: TLZLog` - Access global logger instance
- `function LazyLogCache: string` - Get memory log cache text
- `procedure Log(ASender: TObject; AMessage: string [; const Args: array of const])`
- `procedure Debug(ASender: TObject; AProcedure: string; AMessage: string [; const Args: array of const])`
- `procedure Warning(ASender: TObject; AMessage: string [; const Args: array of const])`
- `procedure Error(ASender: TObject; AMessage: string [; const Args: array of const]; AErrorCode: integer = 0)`
- `procedure Error(ASender: TObject; AException: Exception; AMessage: string [; const Args: array of const])`

**Example Usage**:
```delphi
// Set application name
TLZLog.ApplicationName := 'MyApplication';

// Initialize with multiple handlers
LazyLog.AddHandler('FileLog', TLZLogFileStream);
LazyLog.AddHandler('MemoryLog', TLZLogMemory);
LazyLog.LogLevel := logDebug;

// Standard logging with formatting
LazyLog.Log(Self, 'Processing %d records', [Count]);
Log(Self, 'Found %d items in %s', [ItemCount, Category]); // Global procedure

// Progress tracking
LazyLog.LogActionBegin(Self, 'Import', 'Starting data import');
LazyLog.LogActionProgress(Self, 'Import', 50, 'Processing record 50 of 100');
LazyLog.LogActionEnd(Self, 'Import');

// Query active actions (for UI progress indicators)
var LActions := LazyLog.GetAllActions;
for var LAction in LActions do
  ShowProgress(LAction.ActionName, LAction.ActionProgress, LAction.GetDuration);
```

**Monitoring Long-Running Tasks with TLazyLogAction**:

The action tracking system maintains a live registry of all active operations. This is useful for:
- Displaying progress bars in UI applications
- Monitoring background thread operations
- Detecting stalled processes
- Building progress dashboards

```delphi
// Worker thread performs a long operation
procedure TWorkerThread.Execute;
begin
  LazyLog.LogActionBegin(Self, 'DataSync', 'Connecting to server');
  try
    for LIdx := 0 to RecordCount - 1 do
    begin
      // Process record
      if LIdx mod 100 = 0 then
        LazyLog.LogActionProgress(Self, 'DataSync', 
          (LIdx * 100) div RecordCount, 
          Format('Synced %d of %d', [LIdx, RecordCount]));
    end;
  finally
    LazyLog.LogActionEnd(Self, 'DataSync');
  end;
end;

// UI timer queries progress
procedure TMainForm.TimerUpdate(Sender: TObject);
var
  LActions: TArray<TLazyLogAction>;
  LAction: TLazyLogAction;
begin
  LActions := LazyLog.GetAllActions;
  
  // Display all active operations
  Memo1.Lines.Clear;
  for LAction in LActions do
  begin
    Memo1.Lines.Add(Format('%s: %d%% [%s] - %s',
      [LAction.ActionName, 
       LAction.ActionProgress, 
       LAction.GetDuration,
       LAction.ActionMessage]));
  end;
  
  // Or get specific action
  LAction := LazyLog.GetAction(WorkerThreadID, 'DataSync');
  if Assigned(LAction) then
    ProgressBar1.Position := LAction.ActionProgress;
end;
```

**Note**: Actions are automatically removed from tracking when `LogActionEnd` is called. Use `GetAllActions` or `GetCurrentThreadActions` to retrieve only active (in-progress) operations.

**Enhanced Exception Logging**:

When logging exceptions using any `Error` method with an `Exception` parameter, the system automatically:
1. Creates a `TLZExceptionDetails` instance
2. Extracts comprehensive exception information (see Lazy.Exception.Details below)
3. Populates the `StackTrace` property in `TLazyLogMessage`
4. Includes full exception details in the log message

```delphi
try
  DoSomething;
except
  on E: Exception do
    LazyLog.Error(Self, E);  // Automatically includes detailed exception info
end;
```

The logged output includes: exception class, message, address, application info (executable, version), active form/control details, and stack trace (when handlers are registered).

---

#### Lazy.Exception.Details

**ILZExceptionHandler** - Interface for custom exception detail handlers
- `procedure EnhanceExceptionDetails(const ADetails: TLZExceptionDetails)`
- `function GetHandlerName: string`

Implement this interface to integrate third-party stack trace tools (JclDebug, MadExcept, EurekaLog).

**TLZExceptionDetails** - Comprehensive exception information extractor
- `constructor Create(const AException: Exception; const ASender: TObject = nil; const AExceptAddr: Pointer = nil)`
- `class procedure RegisterHandler(const AHandler: ILZExceptionHandler)` - Register custom enhancement handler
- `class procedure UnregisterHandler(const AHandler: ILZExceptionHandler)`
- `class procedure ClearHandlers`
- `function AsString: string` - Formatted text output
- `function AsXML: string` - XML format
- `function AsJSON: string` - JSON format

*Properties (all writable for handler enhancement):*
- `property SourceException: Exception` - The original exception object
- `property Executable: string` - Full path from ParamStr(0)
- `property ApplicationTitle: string` - Application.Title
- `property ProductName: string` - From version resources
- `property FileVersion: string` - e.g., "1.2.3.4"
- `property ProductVersion: string` - Product version
- `property UnitName: string` - Source unit (via handlers)
- `property ProcedureName: string` - Procedure/function name (via handlers)
- `property SourceName: string` - Source file path (via handlers)
- `property LineNumber: Integer` - Line number in source (via handlers)
- `property StackTrace: string` - Full stack trace (via handlers)
- `property SenderClass: string` - Sender object class name
- `property ExceptionClass: string` - Exception class name
- `property Address: string` - Exception address as hex string
- `property ActiveFormClass: string` - Screen.ActiveForm.ClassName
- `property ActiveFormCaption: string` - Screen.ActiveForm.Caption
- `property ActiveControlClass: string` - Screen.ActiveControl.ClassName
- `property ActiveControlName: string` - Screen.ActiveControl.Name
- `property ExceptionMessage: string` - Exception.Message
- `property Timestamp: TDateTime` - When exception was captured

**Integration with Lazy.Log**: Exception details are automatically extracted when logging exceptions. All `Error` methods that accept an `Exception` parameter use `TLZExceptionDetails` internally.

**Custom Handler Example** (JclDebug):
```delphi
type
  TJclDebugHandler = class(TInterfacedObject, ILZExceptionHandler)
  public
    procedure EnhanceExceptionDetails(const ADetails: TLZExceptionDetails);
    function GetHandlerName: string;
  end;

procedure TJclDebugHandler.EnhanceExceptionDetails(const ADetails: TLZExceptionDetails);
var
  LList: TJclStackInfoList;
  i: Integer;
begin
  LList := JclLastExceptStackList;
  if Assigned(LList) and (LList.Count > 0) then
  begin
    // Populate source location from first frame
    ADetails.UnitName := LList.Items[0].ModuleName;
    ADetails.ProcedureName := LList.Items[0].ProcedureName;
    ADetails.SourceName := LList.Items[0].SourceName;
    ADetails.LineNumber := LList.Items[0].LineNumber;
    
    // Build full stack trace
    for i := 0 to LList.Count - 1 do
      ADetails.StackTrace := ADetails.StackTrace + 
        Format('[%p] %s.%s (Line %d)', [...]);
  end;
end;

// Register during app initialization
initialization
  TLZExceptionDetails.RegisterHandler(TJclDebugHandler.Create);
```

**Direct Usage** (without logging):
```delphi
var
  LDetails: TLZExceptionDetails;
begin
  try
    DoSomething;
  except
    on E: Exception do
    begin
      LDetails := TLZExceptionDetails.Create(E, Self, ExceptAddr);
      try
        // Use details
        TFile.WriteAllText('crash.log', LDetails.AsString);
        TFile.WriteAllText('crash.xml', LDetails.AsXML);
        TFile.WriteAllText('crash.json', LDetails.AsJSON);
      finally
        LDetails.Free;
      end;
    end;
  end;
end;
```

**Conditional Compilation**: Supports VCL, FMX, and console applications. Form/control properties are only populated when available.

---

#### Lazy.Log.Memory

**TLZLogMemory** - In-memory logging handler
- Inherits from TLZLogHandler
- `class var DefaultCacheSize: integer` - Default: 5000 lines (changeable at runtime)
- `function LogText: string` - Get all log text
- `class function Text: string` - Get log text from global 'MemoryLog' handler

Automatically trims old entries when cache size is exceeded (oldest entries removed first).

---

#### Lazy.Log.FileStream

**TLZLogFileStream** - File-based logging handler with automatic duration tracking
- Inherits from TLZLogHandler
- `procedure ClearLogFile` - Clear the log file
- `procedure FlushLogFile` - Force flush to disk
- `property FileName: TFileName` - Log file path (per instance)

Default log file: `<application>.log`

**Progress Tracking**: Automatically tracks action start times per thread and calculates duration on `LogActionEnd`

**Format**: CSV format with timestamp, log level, and message

---

#### Lazy.Log.Debugger

**TLZLogDebugger** - IDE debugger output handler
- Inherits from TLZLogHandler
- Outputs log messages to IDE debugger window via `OutputDebugString`
- Windows only (`{$IFDEF MSWINDOWS}`)
- Skips progress updates (`asProgress`) to avoid spam
- Logs action begin/end events

---

#### Lazy.Log.NTEventLog

**TLZLogNTEventLog** - Windows Event Log handler (Windows only)
- Inherits from TLZLogHandler
- `class var ApplicationName: string` - Application name for Event Log registration
- `procedure RegisterApplication` - Register application in Event Log
- `procedure UnregisterApplication` - Remove registration
- `function IsRegistered: boolean` - Check if registered

**Behavior**:
- Only logs errors and warnings (skips debug, info, progress)
- Automatically registers application on first use
- Requires administrator privileges for registration

---

#### Lazy.Log.SQLite

**TLZLogSQLite** - SQLite database logging handler using FireDAC
- Inherits from TLZLogHandler
- `class var DatabaseFileName: string` - Database file path (default: `<app>.log.db`)
- `class var MaxLogEntries: Integer` - Maximum entries before auto-purge (default: 100000)
- `class var AutoPurgeOldLogs: Boolean` - Enable auto-purge (default: True)
- `procedure PurgeOldLogs(AKeepDays: Integer = 30)` - Delete old log entries
- `procedure ClearAllLogs` - Delete all logs and vacuum database

**Database Schema**:
- **LogMessages** table: Full history of all log messages with exception details
- **LogActions** table: Live tracking of active operations only (not a history)
  - `asBegin` → **INSERT** new action
  - `asProgress` → **UPDATE** existing action
  - `asEnd` → **DELETE** action (completed)
- Automatic table creation on first use
- Primary key on (ApplicationName, ThreadID, ActionName) ensures uniqueness
- DateTime columns indexed for performance
- WAL (Write-Ahead Logging) mode enabled

**Usage**:
```delphi
// Configure
TLZLogSQLite.DatabaseFileName := 'C:\Logs\MyApp.log.db';
TLZLogSQLite.AutoPurgeOldLogs := True;

// Add handler
LazyLog.AddHandler('SQLite', TLZLogSQLite);

// Query logs directly
SELECT * FROM LogMessages WHERE LogType = 'ERROR' ORDER BY DateTime DESC;

-- Query active operations only
SELECT ActionName, ActionProgress, StartDateTime FROM LogActions;
```

See `source/core/log/Lazy.Log.SQLite.README.md` for detailed documentation and query examples.

---

### Network

#### Lazy.NetworkTools

**TLZNetworkTools** - Network utilities
- `class procedure GetLocalNetworkList(AList: TStrings; AIncludeSubnet: boolean = false)`
- `class function GetNetworkAddress(AIPAddress: string): string`
- `class procedure GetLocalNetwork(var ANetwork: string; var ASubnet: string)`
- `class procedure GetIPList(AIPList: TStrings; ANetwork: string = ''; ASubnet: string = '255.255.255.0')`
- `class procedure GetIPList(AIPList: TStrings; ANetworkList: TStrings)`
- `class procedure GetLocalNetworkIPList(AIPList: TStrings)`
- `class function IsIPAddress(AIP: string): boolean`
- `class function Ping(const AHost: string; ATimeout: integer = 200; APacketSize: integer = 24): boolean`
- `class function GetHostname: string`
- `class function GetIPAddress: string`
- `class function GetIPAddresses: string`

---

#### Lazy.SNMP.Client

SNMP client implementation (details in source file)

---

## VCL Components

Windows-specific VCL components.

### VCL Base

#### VCL.Lazy.Utils.Windows

Platform-specific implementations that extend the base classes:

**TLZFile** - VCL file operations
- All TLZFileBase methods
- Windows-specific implementations

**TLZSystem** - VCL system operations
- All TLZSystemBase methods
- Windows-specific implementations

**TLZString** - VCL string operations
- All TLZStringBase methods
- Windows-specific implementations

**TLZURL** - VCL URL operations
- All TLZURLBase methods

**TLZDateTime** - VCL date/time operations
- All TLZDateTimeBase methods

**TLZBoolean** - VCL boolean operations
- All TLZBooleanBase methods

**TLZMath** - VCL math operations
- All TLZMathBase methods

---

#### VCL.Lazy.Compare

**TLZCompare** - VCL comparison utilities
- All TLZCompareBase methods
- Windows file version comparison

**TLZCompareConsole** - Console comparison with output
- All TLZCompare methods
- Console output functionality

---

#### VCL.Lazy.ThreadTimer

**TLZThreadTimer** - Thread-based timer
- Inherits from TThread
- `property Interval: Cardinal`
- `property OnTimer: TNotifyEvent`

---

#### VCL.Lazy.WTSSessionChange

**TLZWTSSessionChange** - Windows Terminal Services session monitoring
- `constructor Create(AOwner: TComponent; AWindowHandle: HWND)`
- `property OnSessionChange: TOnWTSSessionChange`
- Monitors session lock/unlock, logon/logoff events

---

### VCL WinAPI

#### WinApi.SystemInformation

**TLZSystemInformation** - System and hardware information
- `function GetBIOSSerial: string`
- `function GetBIOSVendor: string`
- `function GetBIOSVersion: string`
- `function GetBIOSDate: string`
- `function GetMotherboardManufacturer: string`
- `function GetMotherboardProduct: string`
- `function GetMotherboardVersion: string`
- `function GetMotherboardSerial: string`
- `function GetProcessorName: string`
- `function GetProcessorID: string`
- `function GetOSName: string`
- `function GetOSVersion: string`
- `function GetOSArchitecture: string`
- `function GetPhysicalMemory: Int64`
- `function GetMACAddress: string`
- Additional system information methods

---

#### WinApi.FileVersionInformation

**TLZFileVersionInformation** - File version information
- `constructor Create`
- `function LoadFromFile(AFileName: TFileName): boolean`
- `function GetVersionString: string`
- `function GetProductString: string`
- `property CompanyName: string`
- `property FileDescription: string`
- `property FileVersion: string`
- `property InternalName: string`
- `property LegalCopyright: string`
- `property OriginalFilename: string`
- `property ProductName: string`
- `property ProductVersion: string`
- Additional version properties

---

#### WinApi.VSS.Copy

**TLZVSSCopy** - Volume Shadow Copy operations
- `function CreateSnapshot(ADrive: string): boolean`
- `function CopyFile(ASource: string; ADestination: string): boolean`
- `function DeleteSnapshot: boolean`
- `property SnapshotID: TGUID`
- `property SnapshotPath: string`

---

#### WinApi.DirectoryWatch

**TLZDirectoryWatch** - File system monitoring
- `constructor Create`
- `procedure Start`
- `procedure Stop`
- `property Directory: string`
- `property WatchSubTree: boolean`
- `property OnChange: TDirectoryWatchEvent`
- `property OnFileAdded: TDirectoryWatchFileEvent`
- `property OnFileRemoved: TDirectoryWatchFileEvent`
- `property OnFileModified: TDirectoryWatchFileEvent`
- `property OnFileRenamed: TDirectoryWatchRenameEvent`

---

#### WinApi.DeviceNotifier

**TLZDeviceNotifier** - Device arrival/removal notification
- `constructor Create(AOwner: TComponent; AWindowHandle: HWND)`
- `property OnDeviceArrival: TDeviceNotifyEvent`
- `property OnDeviceRemoval: TDeviceNotifyEvent`
- `property OnDeviceChange: TDeviceNotifyEvent`

---

#### WinApi.ICMP

**TLZICMP** - ICMP/Ping operations
- `constructor Create`
- `function Ping(AHost: string; ATimeout: integer = 1000): boolean`
- `property ReplyIP: string`
- `property ReplyTime: integer`
- `property ReplyTTL: integer`
- `property ReplyStatus: integer`

---

#### WinApi.Services

**TLZNTServiceControl** - Windows service management
- `function Start(AServiceName: string): boolean`
- `function Stop(AServiceName: string): boolean`
- `function Restart(AServiceName: string): boolean`
- `function GetStatus(AServiceName: string): DWORD`
- `function IsRunning(AServiceName: string): boolean`
- `function Install(AServiceName: string; ADisplayName: string; AFileName: string): boolean`
- `function Uninstall(AServiceName: string): boolean`

---

#### WinApi.NamedPipes.Server

**TLZNamedPipeServer** - Named pipe server
- `constructor Create(APipeName: string)`
- `procedure Start`
- `procedure Stop`
- `property OnClientConnect: TNotifyEvent`
- `property OnClientDisconnect: TNotifyEvent`
- `property OnDataReceived: TPipeDataEvent`

---

#### WinApi.NamedPipes.Client

**TLZNamedPipeClient** - Named pipe client
- `constructor Create`
- `function Connect(APipeName: string): boolean`
- `procedure Disconnect`
- `function SendData(AData: string): boolean`
- `function ReceiveData: string`

---

#### WinApi.RunAs

**TNukeRunAs** - Execute with elevation
- `function Execute(ACommandLine: string; AWaitForCompletion: boolean = true): boolean`
- `function ExecuteAsAdmin(ACommandLine: string; AWaitForCompletion: boolean = true): boolean`
- `property ExitCode: DWORD`

---

#### WinApi.WindowsMessages

**TLZWindowsMessages** - Windows message handling
- `constructor Create(AWindowHandle: HWND)`
- `procedure RegisterMessage(AMessage: string; AHandler: TMessageEvent)`
- `procedure UnregisterMessage(AMessage: string)`
- `function SendMessage(AMessage: string; wParam: WPARAM; lParam: LPARAM): LRESULT`

---

#### WinApi.Networklist

**TNetworklist** - Network adapter enumeration
- Network connection list and status

---

### VCL Forms

#### VCL.Lazy.AuthorizeBrowserForm

**TLZAuthorizeBrowserForm** - OAuth browser authorization
- Embedded browser for OAuth2 flows
- `property AuthURL: string`
- `property RedirectURL: string`
- `property OnAuthorizationCode: TAuthCodeEvent`

---

#### VCL.DUO.PreAuthForm

**TFormDuoPreAuth** - DUO pre-authentication UI
- DUO Security pre-authentication dialog
- Device selection and approval

---

## FMX Components

FireMonkey cross-platform components.

### FMX Forms

#### FMX.Lazy.AuthorizeBrowserForm

**TLZAuthorizeBrowserForm** - FMX OAuth browser
- Cross-platform OAuth2 authorization browser
- `property AuthURL: string`
- `property RedirectURL: string`
- `property OnAuthorizationCode: TAuthCodeEvent`



## Notes

- All classes prefixed with `TLZ` are part of the Lazy Library
- Most utility classes use `class` methods and don't require instantiation
- Model classes (TLZModel descendants) support automatic JSON serialization
- REST API clients handle OAuth2 authentication automatically
- VCL components require Windows platform
- FMX components are cross-platform (Windows, macOS, Linux, iOS, Android)
- Use `LazyLog` for application-wide logging with multiple handlers
- Use `TLazyLogAction` to monitor progress of long-running operations in real-time
- All logging methods support format arguments for convenient string formatting
- Thread-safe components are explicitly marked (ThreadedFileStream, ThreadedStringList)
- Inherit from `TLZObject`, `TLZPersistent`, or `TLZComponent` for built-in logging methods

---

## Author & License

**Author**: Tristan Marlow  
**Copyright**: (c) Tristan David Marlow, Little Earth Solutions

For detailed usage examples, see the `example` directory in the repository.

