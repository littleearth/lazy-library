{ -----------------------------------------------------------------------------
  Unit Name: WinApi.PerformanceData
  Author: Tristan Marlow
  Purpose: Little Earth Solutions Windows Performance Data

  ----------------------------------------------------------------------------
  Copyright (c) 2016 Tristan David Marlow
  Copyright (c) 2016 ABit Consulting
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History: 26/10/2009 - First Release.

  ----------------------------------------------------------------------------- }
unit WinApi.PerformanceData;

interface

uses
  JwaWinPerf,
  Windows, Messages, SysUtils, Classes;

type
  PPerfLibHeader = ^TPerfLibHeader;

  TPerfLibHeader = packed record
    Signature: array [0 .. 7] of Char;
    DataSize: Cardinal;
    ObjectCount: Cardinal;
  end;

type
  TPerfDataBlock = packed record
    Signature: array [0 .. 3] of wchar;
    littleEndian: Cardinal;
    version: Cardinal;
    revision: Cardinal;
    totalByteLength: Cardinal;
    headerLength: Cardinal;
    numObjectTypes: integer;
    defaultObject: Cardinal;
    systemTime: TSystemTime;
    perfTime: comp;
    perfFreq: comp;
    perfTime100nSec: comp;
    systemNameLength: Cardinal;
    systemnameOffset: Cardinal;
  end;

  TPerfObjectType = packed record
    totalByteLength: Cardinal;
    definitionLength: Cardinal;
    headerLength: Cardinal;
    objectNameTitleIndex: Cardinal;
    objectNameTitle: PWideChar;
    objectHelpTitleIndex: Cardinal;
    objectHelpTitle: PWideChar;
    detailLevel: Cardinal;
    numCounters: integer;
    defaultCounter: integer;
    numInstances: integer;
    codePage: Cardinal;
    perfTime: comp;
    perfFreq: comp;
  end;

  TPerfCounterDefinition = packed record
    byteLength: Cardinal;
    counterNameTitleIndex: Cardinal;
    counterNameTitle: PWideChar;
    counterHelpTitleIndex: Cardinal;
    counterHelpTitle: PWideChar;
    defaultScale: integer;
    defaultLevel: Cardinal;
    counterType: Cardinal;
    counterSize: Cardinal;
    counterOffset: Cardinal;
  end;

  TPerfInstanceDefinition = packed record
    byteLength: Cardinal;
    parentObjectTitleIndex: Cardinal;
    parentObjectInstance: Cardinal;
    uniqueID: integer;
    nameOffset: Cardinal;
    nameLength: Cardinal;
  end;

function GetProcessorUsage: integer;
function GetMemoryLoad: Cardinal;
function GetCounterBlock(Obj: PPerfObjectType): PPerfCounterBlock; overload;
function GetCounterBlock(Instance: PPerfInstanceDefinition)
  : PPerfCounterBlock; overload;
function GetCounterDataAddress(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition; Instance: PPerfInstanceDefinition = nil)
  : Pointer; overload;
function GetCounterDataAddress(Obj: PPerfObjectType; Counter, Instance: integer)
  : Pointer; overload;
function GetCounter(Obj: PPerfObjectType; Index: integer)
  : PPerfCounterDefinition;
function GetCounterByNameIndex(Obj: PPerfObjectType; NameIndex: Cardinal)
  : PPerfCounterDefinition;
function GetCounterValue32(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition; Instance: PPerfInstanceDefinition = nil)
  : Cardinal;
function GetCounterValue64(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
  Instance: PPerfInstanceDefinition = nil): UInt64;
function GetCounterValueText(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
  Instance: PPerfInstanceDefinition = nil): PChar;
function GetCounterValueWideText(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition; Instance: PPerfInstanceDefinition = nil)
  : PWideChar;
function GetFirstCounter(Obj: PPerfObjectType): PPerfCounterDefinition;
function GetFirstInstance(Obj: PPerfObjectType): PPerfInstanceDefinition;
function GetFirstObject(Data: PPerfDataBlock): PPerfObjectType; overload;
function GetFirstObject(Header: PPerfLibHeader): PPerfObjectType; overload;
function GetInstance(Obj: PPerfObjectType; Index: integer)
  : PPerfInstanceDefinition;
function GetInstanceName(Instance: PPerfInstanceDefinition): PWideChar;
function GetNextCounter(Counter: PPerfCounterDefinition)
  : PPerfCounterDefinition;
function GetNextInstance(Instance: PPerfInstanceDefinition)
  : PPerfInstanceDefinition;
function GetNextObject(Obj: PPerfObjectType): PPerfObjectType;
function GetObjectSize(Obj: PPerfObjectType): Cardinal;
function GetObject(Data: PPerfDataBlock; Index: integer)
  : PPerfObjectType; overload;
function GetObject(Header: PPerfLibHeader; Index: integer)
  : PPerfObjectType; overload;
function GetObjectByNameIndex(Data: PPerfDataBlock; NameIndex: Cardinal)
  : PPerfObjectType; overload;
function GetObjectByNameIndex(Header: PPerfLibHeader; NameIndex: Cardinal)
  : PPerfObjectType; overload;
function GetPerformanceData(const RegValue: string): PPerfDataBlock;
function GetProcessInstance(Obj: PPerfObjectType; ProcessID: Cardinal)
  : PPerfInstanceDefinition;
function GetSimpleCounterValue32(ObjIndex, CtrIndex: integer): Cardinal;
function GetSimpleCounterValue64(ObjIndex, CtrIndex: integer): UInt64;

function GetProcessName(ProcessID: Cardinal): WideString;
function GetProcessPercentProcessorTime(ProcessID: Cardinal;
  Data1, Data2: PPerfDataBlock; ProcessorCount: integer = -1): Double;
function GetProcessPrivateBytes(ProcessID: Cardinal): UInt64;
function GetProcessThreadCount(ProcessID: Cardinal): Cardinal;
function GetProcessVirtualBytes(ProcessID: Cardinal): UInt64;
function GetProcessorCount: integer;
function GetSystemProcessCount: Cardinal;
function GetSystemUpTime: TDateTime;

var
  PerfFrequency: TLargeInteger;

const
  // perfdisk.dll
  ObjPhysicalDisk = 234;
  ObjLogicalDisk = 236;
  // perfnet.dll
  ObjBrowser = 52;
  ObjRedirector = 262;
  ObjServer = 330;
  ObjServerWorkQueues = 1300;
  // perfos.dll
  ObjSystem = 2;
  CtrProcesses = 248;
  CtrSystemUpTime = 674;
  ObjMemory = 4;
  ObjCache = 86;
  ObjProcessor = 238;
  ObjObjects = 260;
  ObjPagingFile = 700;
  // perfproc.dll
  ObjProcess = 230;
  CtrPercentProcessorTime = 6;
  CtrVirtualBytes = 174;
  CtrPrivateBytes = 186;
  CtrThreadCount = 680;
  CtrIDProcess = 784;
  ObjThread = 232;
  ObjProcessAddressSpace = 786;
  ObjImage = 740;
  ObjThreadDetails = 816;
  ObjFullImage = 1408;
  ObjJobObject = 1500;
  ObjJobObjectDetails = 1548;
  ObjHeap = 1760;
  // winspool.drv
  ObjPrintQueue = 1450;
  // tapiperf.dll
  ObjTelephony = 1150;
  // perfctrs.dll
  ObjNBTConnection = 502;
  ObjNetworkInterface = 510;
  ObjIP = 546;
  ObjICMP = 582;
  ObjTCP = 638;
  ObjUDP = 658;

implementation

function GetProcessorTime: Int64;
var
  c1, c2, c3: Cardinal;
  i1, i2: integer;
  perfDataBlock: ^TPerfDataBlock;
  perfObjectType: ^TPerfObjectType;
  perfCounterDef: ^TPerfCounterDefinition;
  perfInstanceDef: ^TPerfInstanceDefinition;
begin
  Result := 0;
  perfDataBlock := nil;
  try
    c1 := $10000;
    while True do
    begin
      ReallocMem(perfDataBlock, c1);
      c2 := c1;
      case RegQueryValueEx(HKEY_PERFORMANCE_DATA, '238', nil, @c3,
        Pointer(perfDataBlock), @c2) of
        ERROR_MORE_DATA:
          c1 := c1 * 2;
        ERROR_SUCCESS:
          break;
      else
        exit;
      end;
    end;
    perfObjectType := Pointer(Cardinal(perfDataBlock) +
      perfDataBlock^.headerLength);
    for i1 := 0 to perfDataBlock^.numObjectTypes - 1 do
    begin
      if perfObjectType^.objectNameTitleIndex = 238 then
      begin // 238 -> "Processor"
        perfCounterDef := Pointer(Cardinal(perfObjectType) +
          perfObjectType^.headerLength);
        for i2 := 0 to perfObjectType^.numCounters - 1 do
        begin
          if perfCounterDef^.counterNameTitleIndex = 6 then
          begin // 6 -> "% Processor Time"
            perfInstanceDef :=
              Pointer(Cardinal(perfObjectType) +
              perfObjectType^.definitionLength);
            Result := PInt64(Cardinal(perfInstanceDef) +
              perfInstanceDef^.byteLength + perfCounterDef^.counterOffset)^;
            break;
          end;
          Inc(perfCounterDef);
        end;
        break;
      end;
      perfObjectType := Pointer(Cardinal(perfObjectType) +
        perfObjectType^.totalByteLength);
    end;
  finally
    FreeMem(perfDataBlock)
  end;
end;

function GetProcessorUsage: integer;
var
  tickCount: Cardinal;
  processorTime: Int64;
begin
  try
    tickCount := GetTickCount;
    processorTime := GetProcessorTime;
    Sleep(500);
    Result := 100 - Round(((GetProcessorTime - processorTime) div 100) /
      (GetTickCount - tickCount));
  except
    Result := 0;
  end;
end;

function GetMemoryLoad: Cardinal;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  Result := Status.dwMemoryLoad;
end;

function GetCounterBlock(Obj: PPerfObjectType): PPerfCounterBlock;
begin
  if Assigned(Obj) and (Obj^.numInstances = PERF_NO_INSTANCES) then
    Cardinal(Result) := Cardinal(Obj) + SizeOf(TPerfObjectType) +
      (Obj^.numCounters * SizeOf(TPerfCounterDefinition))
  else
    Result := nil;
end;

function GetCounterBlock(Instance: PPerfInstanceDefinition): PPerfCounterBlock;
begin
  if Assigned(Instance) then
    Cardinal(Result) := Cardinal(Instance) + Instance^.byteLength
  else
    Result := nil;
end;

function GetCounterDataAddress(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
  Instance: PPerfInstanceDefinition = nil): Pointer;
var
  Block: PPerfCounterBlock;
begin
  Result := nil;
  if not Assigned(Obj) or not Assigned(Counter) then
    exit;

  if Obj^.numInstances = PERF_NO_INSTANCES then
    Block := GetCounterBlock(Obj)
  else
  begin
    if not Assigned(Instance) then
      exit;

    Block := GetCounterBlock(Instance);
  end;

  if not Assigned(Block) then
    exit;

  Cardinal(Result) := Cardinal(Block) + Counter^.counterOffset;
end;

function GetCounterDataAddress(Obj: PPerfObjectType;
  Counter, Instance: integer): Pointer;
begin
  Result := nil;
  if not Assigned(Obj) or (Counter < 0) or
    (Cardinal(Counter) > Obj^.numCounters - 1) then
    exit;

  if Obj^.numInstances = PERF_NO_INSTANCES then
  begin
    if Instance <> -1 then
      exit;
  end
  else
  begin
    if (Instance < 0) or (Instance > Obj^.numInstances - 1) then
      exit;
  end;

  Result := GetCounterDataAddress(Obj, GetCounter(Obj, Counter),
    GetInstance(Obj, Instance));
end;

function GetCounter(Obj: PPerfObjectType; Index: integer)
  : PPerfCounterDefinition;
var
  I: integer;
begin
  if Assigned(Obj) and (Index >= 0) and (Cardinal(Index) <= Obj^.numCounters - 1)
  then
  begin
    Result := GetFirstCounter(Obj);
    if not Assigned(Result) then
      exit;

    for I := 0 to Index - 1 do
    begin
      Result := GetNextCounter(Result);
      if not Assigned(Result) then
        exit;
    end;
  end
  else
    Result := nil;
end;

function GetCounterByNameIndex(Obj: PPerfObjectType; NameIndex: Cardinal)
  : PPerfCounterDefinition;
var
  Counter: PPerfCounterDefinition;
  I: integer;
begin
  Result := nil;

  Counter := GetFirstCounter(Obj);
  for I := 0 to Obj^.numCounters - 1 do
  begin
    if not Assigned(Counter) then
      exit;

    if Counter^.counterNameTitleIndex = NameIndex then
    begin
      Result := Counter;
      break;
    end;

    Counter := GetNextCounter(Counter);
  end;
end;

function GetCounterValue32(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition; Instance: PPerfInstanceDefinition = nil)
  : Cardinal;
var
  DataAddr: Pointer;
begin
  Result := 0;

  DataAddr := GetCounterDataAddress(Obj, Counter, Instance);
  if not Assigned(DataAddr) then
    exit;

  if Counter^.counterType and $00000300 = PERF_SIZE_DWORD then // 32-bit value
    case Counter^.counterType and $00000C00 of // counter type
      PERF_TYPE_NUMBER, PERF_TYPE_COUNTER:
        Result := PCardinal(DataAddr)^;
    end;
end;

function GetCounterValue64(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
  Instance: PPerfInstanceDefinition = nil): UInt64;
var
  DataAddr: Pointer;
begin
  Result := 0;

  DataAddr := GetCounterDataAddress(Obj, Counter, Instance);
  if not Assigned(DataAddr) then
    exit;

  if Counter^.counterType and $00000300 = PERF_SIZE_LARGE then // 64-bit value
    case Counter^.counterType and $00000C00 of // counter type
      PERF_TYPE_NUMBER, PERF_TYPE_COUNTER:
        Result := UInt64(PInt64(DataAddr)^);
    end;
end;

function GetCounterValueText(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
  Instance: PPerfInstanceDefinition = nil): PChar;
var
  DataAddr: Pointer;
begin
  Result := nil;

  DataAddr := GetCounterDataAddress(Obj, Counter, Instance);
  if not Assigned(DataAddr) then
    exit;

  if Counter^.counterType and $00000300 = PERF_SIZE_VARIABLE_LEN then
    // variable-length value
    if (Counter^.counterType and $00000C00 = PERF_TYPE_TEXT) and
      (Counter^.counterType and $00010000 = PERF_TEXT_ASCII) then
      Result := PChar(DataAddr);
end;

function GetCounterValueWideText(Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition; Instance: PPerfInstanceDefinition = nil)
  : PWideChar;
var
  DataAddr: Pointer;
begin
  Result := nil;

  DataAddr := GetCounterDataAddress(Obj, Counter, Instance);
  if not Assigned(DataAddr) then
    exit;

  if Counter^.counterType and $00000300 = PERF_SIZE_VARIABLE_LEN then
    // variable-length value
    if (Counter^.counterType and $00000C00 = PERF_TYPE_TEXT) and
      (Counter^.counterType and $00010000 = PERF_TEXT_UNICODE) then
      Result := PWideChar(DataAddr);
end;

function GetFirstCounter(Obj: PPerfObjectType): PPerfCounterDefinition;
begin
  if Assigned(Obj) then
    Cardinal(Result) := Cardinal(Obj) + Obj^.headerLength
  else
    Result := nil;
end;

function GetFirstInstance(Obj: PPerfObjectType): PPerfInstanceDefinition;
begin
  if not Assigned(Obj) or (Obj^.numInstances = PERF_NO_INSTANCES) then
    Result := nil
  else
    Cardinal(Result) := Cardinal(Obj) + SizeOf(TPerfObjectType) +
      (Obj^.numCounters * SizeOf(TPerfCounterDefinition));
end;

function GetFirstObject(Data: PPerfDataBlock): PPerfObjectType; overload;
begin
  if Assigned(Data) then
    Cardinal(Result) := Cardinal(Data) + Data^.headerLength
  else
    Result := nil;
end;

function GetFirstObject(Header: PPerfLibHeader): PPerfObjectType; overload;
begin
  if Assigned(Header) then
    Cardinal(Result) := Cardinal(Header) + SizeOf(TPerfLibHeader)
  else
    Result := nil;
end;

function GetInstance(Obj: PPerfObjectType; Index: integer)
  : PPerfInstanceDefinition;
var
  I: integer;
begin
  if Assigned(Obj) and (Index >= 0) and (Index <= Obj^.numInstances - 1) then
  begin
    Result := GetFirstInstance(Obj);
    if not Assigned(Result) then
      exit;

    for I := 0 to Index - 1 do
    begin
      Result := GetNextInstance(Result);
      if not Assigned(Result) then
        exit;
    end;
  end
  else
    Result := nil;
end;

function GetInstanceName(Instance: PPerfInstanceDefinition): PWideChar;
begin
  if Assigned(Instance) then
    Cardinal(Result) := Cardinal(Instance) + Instance^.nameOffset
  else
    Result := nil;
end;

function GetNextCounter(Counter: PPerfCounterDefinition)
  : PPerfCounterDefinition;
begin
  if Assigned(Counter) then
    Cardinal(Result) := Cardinal(Counter) + Counter^.byteLength
  else
    Result := nil;
end;

function GetNextInstance(Instance: PPerfInstanceDefinition)
  : PPerfInstanceDefinition;
var
  Block: PPerfCounterBlock;
begin
  Block := GetCounterBlock(Instance);
  if Assigned(Block) then
    Cardinal(Result) := Cardinal(Block) + Block^.byteLength
  else
    Result := nil;
end;

function GetNextObject(Obj: PPerfObjectType): PPerfObjectType;
begin
  if Assigned(Obj) then
    Cardinal(Result) := Cardinal(Obj) + Obj^.totalByteLength
  else
    Result := nil;
end;

function GetObjectSize(Obj: PPerfObjectType): Cardinal;
var
  I: integer;
  Instance: PPerfInstanceDefinition;
begin
  Result := 0;

  if Assigned(Obj) then
  begin
    if Obj^.numInstances = PERF_NO_INSTANCES then
      Result := Obj^.totalByteLength
    else
    begin
      Instance := GetFirstInstance(Obj);
      if not Assigned(Instance) then
        exit;

      for I := 0 to Obj^.numInstances - 1 do
      begin
        Instance := GetNextInstance(Instance);
        if not Assigned(Instance) then
          exit;
      end;

      Result := Cardinal(Instance) - Cardinal(Obj);
    end;
  end;
end;

function GetObject(Data: PPerfDataBlock; Index: integer): PPerfObjectType;
var
  I: integer;
begin
  if Assigned(Data) and (Index >= 0) and
    (Cardinal(Index) <= Data^.numObjectTypes - 1) then
  begin
    Result := GetFirstObject(Data);
    if not Assigned(Result) then
      exit;

    for I := 0 to Index - 1 do
    begin
      Result := GetNextObject(Result);
      if not Assigned(Result) then
        exit;
    end;
  end
  else
    Result := nil;
end;

function GetObject(Header: PPerfLibHeader; Index: integer): PPerfObjectType;
var
  I: integer;
begin
  if Assigned(Header) and (Index >= 0) then
  begin
    Result := GetFirstObject(Header);
    if not Assigned(Result) then
      exit;

    for I := 0 to Index - 1 do
    begin
      Result := GetNextObject(Result);
      if not Assigned(Result) then
        exit;
    end;
  end
  else
    Result := nil;
end;

function GetObjectByNameIndex(Data: PPerfDataBlock; NameIndex: Cardinal)
  : PPerfObjectType;
var
  Obj: PPerfObjectType;
  I: integer;
begin
  Result := nil;

  Obj := GetFirstObject(Data);
  for I := 0 to Data^.numObjectTypes - 1 do
  begin
    if not Assigned(Obj) then
      exit;

    if Obj^.objectNameTitleIndex = NameIndex then
    begin
      Result := Obj;
      break;
    end;

    Obj := GetNextObject(Obj);
  end;
end;

function GetObjectByNameIndex(Header: PPerfLibHeader; NameIndex: Cardinal)
  : PPerfObjectType; overload;
var
  Obj: PPerfObjectType;
  I: integer;
begin
  Result := nil;

  Obj := GetFirstObject(Header);
  for I := 0 to Header^.ObjectCount - 1 do
  begin
    if not Assigned(Obj) then
      exit;

    if Obj^.objectNameTitleIndex = NameIndex then
    begin
      Result := Obj;
      break;
    end;

    Obj := GetNextObject(Obj);
  end;
end;

function GetPerformanceData(const RegValue: string): PPerfDataBlock;
const
  BufSizeInc = 4096;
var
  BufSize, RetVal: Cardinal;
begin
  BufSize := BufSizeInc;
  Result := AllocMem(BufSize);
  try
    RetVal := RegQueryValueEx(HKEY_PERFORMANCE_DATA, PChar(RegValue), nil, nil,
      PByte(Result), @BufSize);
    try
      repeat
        case RetVal of
          ERROR_SUCCESS:
            break;
          ERROR_MORE_DATA:
            begin
              Inc(BufSize, BufSizeInc);
              ReallocMem(Result, BufSize);
              RetVal := RegQueryValueEx(HKEY_PERFORMANCE_DATA, PChar(RegValue),
                nil, nil, PByte(Result), @BufSize);
            end;
        else
          RaiseLastOSError;
        end;
      until False;
    finally
      RegCloseKey(HKEY_PERFORMANCE_DATA);
    end;
  except
    FreeMem(Result);
    raise;
  end;
end;

function GetProcessInstance(Obj: PPerfObjectType; ProcessID: Cardinal)
  : PPerfInstanceDefinition;
var
  Counter: PPerfCounterDefinition;
  Instance: PPerfInstanceDefinition;
  Block: PPerfCounterBlock;
  I: integer;
begin
  Result := nil;

  Counter := GetCounterByNameIndex(Obj, CtrIDProcess);
  if not Assigned(Counter) then
    exit;

  Instance := GetFirstInstance(Obj);
  for I := 0 to Obj^.numInstances - 1 do
  begin
    Block := GetCounterBlock(Instance);
    if not Assigned(Block) then
      exit;

    if PCardinal(Cardinal(Block) + Counter^.counterOffset)^ = ProcessID then
    begin
      Result := Instance;
      break;
    end;

    Instance := GetNextInstance(Instance);
  end;
end;

function GetSimpleCounterValue32(ObjIndex, CtrIndex: integer): Cardinal;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
begin
  Result := 0;

  Data := GetPerformanceData(IntToStr(ObjIndex));
  try
    Obj := GetObjectByNameIndex(Data, ObjIndex);
    if not Assigned(Obj) then
      exit;

    Counter := GetCounterByNameIndex(Obj, CtrIndex);
    if not Assigned(Counter) then
      exit;

    Result := GetCounterValue32(Obj, Counter);
  finally
    FreeMem(Data);
  end;
end;

function GetSimpleCounterValue64(ObjIndex, CtrIndex: integer): UInt64;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
begin
  Result := 0;

  Data := GetPerformanceData(IntToStr(ObjIndex));
  try
    Obj := GetObjectByNameIndex(Data, ObjIndex);
    if not Assigned(Obj) then
      exit;

    Counter := GetCounterByNameIndex(Obj, CtrIndex);
    if not Assigned(Counter) then
      exit;

    Result := GetCounterValue64(Obj, Counter);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessName(ProcessID: Cardinal): WideString;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
begin
  Result := '';

  Data := GetPerformanceData(IntToStr(ObjProcess));
  try
    Obj := GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(Obj) then
      exit;

    Instance := GetProcessInstance(Obj, ProcessID);
    if not Assigned(Instance) then
      exit;

    Result := GetInstanceName(Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessPercentProcessorTime(ProcessID: Cardinal;
  Data1, Data2: PPerfDataBlock; ProcessorCount: integer): Double;
var
  Value1, Value2: UInt64;

  function GetValue(Data: PPerfDataBlock): UInt64;
  var
    Obj: PPerfObjectType;
    Instance: PPerfInstanceDefinition;
    Counter: PPerfCounterDefinition;
  begin
    Result := 0;

    Obj := GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(Obj) then
      exit;
    Counter := GetCounterByNameIndex(Obj, CtrPercentProcessorTime);
    if not Assigned(Counter) then
      exit;
    Instance := GetProcessInstance(Obj, ProcessID);
    if not Assigned(Instance) then
      exit;

    Result := GetCounterValue64(Obj, Counter, Instance);
  end;

begin
  if ProcessorCount = -1 then
    ProcessorCount := GetProcessorCount;

  Value1 := GetValue(Data1);
  Value2 := GetValue(Data2);

  Result := 100 * (Value2 - Value1) /
    (Data2^.perfTime100nSec.QuadPart - Data1^.perfTime100nSec.QuadPart) /
    ProcessorCount;
end;

function GetProcessPrivateBytes(ProcessID: Cardinal): UInt64;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
  Counter: PPerfCounterDefinition;
begin
  Result := 0;

  Data := GetPerformanceData(IntToStr(ObjProcess));
  try
    Obj := GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(Obj) then
      exit;

    Counter := GetCounterByNameIndex(Obj, CtrPrivateBytes);
    if not Assigned(Counter) then
      exit;

    Instance := GetProcessInstance(Obj, ProcessID);
    if not Assigned(Instance) then
      exit;

    Result := GetCounterValue64(Obj, Counter, Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessThreadCount(ProcessID: Cardinal): Cardinal;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
  Counter: PPerfCounterDefinition;
begin
  Result := 0;

  Data := GetPerformanceData(IntToStr(ObjProcess));
  try
    Obj := GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(Obj) then
      exit;

    Counter := GetCounterByNameIndex(Obj, CtrThreadCount);
    if not Assigned(Counter) then
      exit;

    Instance := GetProcessInstance(Obj, ProcessID);
    if not Assigned(Instance) then
      exit;

    Result := GetCounterValue32(Obj, Counter, Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessVirtualBytes(ProcessID: Cardinal): UInt64;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
  Instance: PPerfInstanceDefinition;
  Counter: PPerfCounterDefinition;
begin
  Result := 0;

  Data := GetPerformanceData(IntToStr(ObjProcess));
  try
    Obj := GetObjectByNameIndex(Data, ObjProcess);
    if not Assigned(Obj) then
      exit;

    Counter := GetCounterByNameIndex(Obj, CtrVirtualBytes);
    if not Assigned(Counter) then
      exit;

    Instance := GetProcessInstance(Obj, ProcessID);
    if not Assigned(Instance) then
      exit;

    Result := GetCounterValue64(Obj, Counter, Instance);
  finally
    FreeMem(Data);
  end;
end;

function GetProcessorCount: integer;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
begin
  Result := -1;

  Data := GetPerformanceData(IntToStr(ObjProcessor));
  try
    Obj := GetFirstObject(Data);
    if not Assigned(Obj) then
      exit;

    Result := Obj^.numInstances;
    if Result > 1 then // disregard the additional '_Total' instance
      Dec(Result);
  finally
    FreeMem(Data);
  end;
end;

function GetSystemProcessCount: Cardinal;
begin
  Result := GetSimpleCounterValue32(ObjSystem, CtrProcesses);
end;

function GetSystemUpTime: TDateTime;
const
  SecsPerDay = 60 * 60 * 24;
var
  Data: PPerfDataBlock;
  Obj: PPerfObjectType;
  Counter: PPerfCounterDefinition;
  SecsStartup: UInt64;
begin
  Result := 0;

  Data := GetPerformanceData(IntToStr(ObjSystem));
  try
    Obj := GetObjectByNameIndex(Data, ObjSystem);
    if not Assigned(Obj) then
      exit;

    Counter := GetCounterByNameIndex(Obj, CtrSystemUpTime);
    if not Assigned(Counter) then
      exit;

    SecsStartup := GetCounterValue64(Obj, Counter);
    // subtract from snapshot time and divide by base frequency and number of seconds per day
    // to get a TDateTime representation
    Result := (Obj^.perfTime.QuadPart - SecsStartup) / Obj^.perfFreq.QuadPart /
      SecsPerDay;
  finally
    FreeMem(Data);
  end;
end;

initialization

QueryPerformanceFrequency(PerfFrequency);

finalization

end.
