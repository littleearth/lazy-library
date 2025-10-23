{ -----------------------------------------------------------------------------
  Unit Name: WinApi.SystemInformation
  Author: Tristan Marlow
  Purpose: Little Earth Solutions System Information Component
  - Gather system details

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------- }
unit WinApi.SystemInformation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  VCL.Lazy.Utils.Windows, Lazy.Types, VCL.Lazy.Types.Windows,
  Windows, Messages, SysUtils, Classes,
  ExtCtrls, System.Win.Registry, Printers,
  WinSock, SHFolder, System.Generics.Collections;

type
  ESystemInformationException = class(Exception);

type
  TLZSystemInformation = class(TLZComponent)
  private
    FNetworkIPList: TStringList;
    FPrinterList: TStringList;
    FShellFolderList: TStringList;
    FInstalledSoftwareList: TStringList;
    FActiveProcessList: TStringList;
    FDriveList: TStringList;
  protected
    function GetOSVersion: TLZOSVersion;
    function GetOSStartupState: TLZOSStartupState;
    function GetOSStartupStateString: string;
    function GetOSVersionString: string;
    function GetOSUptime: string;
    function GetCPUType: string;
    function GetCPUVendor: string;
    function GetCPUSpeed: double;
    function GetCPUSpeedString: string;
    function GetCPUUsage: double;
    function GetVideoCardDetails: string;
    function GetMemoryLoad: integer;
    function GetMemoryTotalPhysical: string;
    function GetMemoryFreePhysical: string;
    function GetMemoryTotalPageFile: string;
    function GetMemoryFreePageFile: string;
    function GetMemoryTotalVirtual: string;
    function GetMemoryFreeVirtual: string;
    function GetNetworkUserName: string;
    function GetNetworkComputerName: string;
    function GetNetworkIPAddress: string;
    function GetNetworkIPList: TStrings;
    function GetNetworkMACAddress: string;
    function GetDriveUsage(ADrive: char; var AFreeSpace: double;
      var ATotalSpace: double; var AUsage: integer): Boolean;
    function GetDriveList: TStrings;
    function GetPrinterList: TStrings;
    function GetShellFolderList: TStrings;
    function GetInstalledSoftwareList: TStrings;
    function GetActiveProcessList: TStrings;
    function IsWOW64: Boolean;
    function GetBIOSVendor: string;
    function GetSystemManufacturer: string;
    function GetSystemProductName: string;
    function GetRegistryString(const RootKey: Windows.HKEY;
      const SubKey, Name: string): string;
    function GetSessionType: string;
    function GetOSArchitecture: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SimpleReport: string;
    function DriveUsage(ADrive: char): integer;
  published
    property BIOSVendor: string read GetBIOSVendor;
    property SystemManufacturer: string read GetSystemManufacturer;
    property SystemProductName: string read GetSystemProductName;
    property OSVersion: string Read GetOSVersionString;
    property OSStartupState: string Read GetOSStartupStateString;
    property OSUptime: string Read GetOSUptime;
    property OS64: Boolean Read IsWOW64;
    property OSArchitecture: string read GetOSArchitecture;
    property CPUType: string Read GetCPUType;
    property CPUVendor: string Read GetCPUVendor;
    property CPUSpeed: string Read GetCPUSpeedString;
    property CPUUsage: double read GetCPUUsage;
    property VideoCardDetails: string Read GetVideoCardDetails;
    property MemoryLoad: integer Read GetMemoryLoad;
    property MemoryTotalPhysical: string Read GetMemoryTotalPhysical;
    property MemoryFreePhysical: string Read GetMemoryFreePhysical;
    property MemoryTotalPageFile: string Read GetMemoryTotalPageFile;
    property MemoryFreePageFile: string Read GetMemoryFreePageFile;
    property MemoryTotalVirtual: string Read GetMemoryTotalVirtual;
    property MemoryFreeVirtual: string Read GetMemoryFreeVirtual;
    property NetworkUserName: string Read GetNetworkUserName;
    property NetworkComputerName: string Read GetNetworkComputerName;
    property NetworkIPAddress: string read GetNetworkIPAddress;
    property NetworkIPList: TStrings Read GetNetworkIPList;
    property NetworkMACAddress: string read GetNetworkMACAddress;
    property DriveList: TStrings Read GetDriveList;
    property PrinterList: TStrings Read GetPrinterList;
    property ShellFolderList: TStrings Read GetShellFolderList;
    property InstalledSoftwareList: TStrings Read GetInstalledSoftwareList;
    property ActiveProcessList: TStrings Read GetActiveProcessList;
    property SessionType: string read GetSessionType;
  end;

implementation

uses
  IdStack, WinApi.Nb30, IpHlpApi, IpTypes, WinAPI.PerformanceDataHelper;

constructor TLZSystemInformation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDriveList := TStringList.Create;
  FNetworkIPList := TStringList.Create;
  FPrinterList := TStringList.Create;
  FShellFolderList := TStringList.Create;
  FInstalledSoftwareList := TStringList.Create;
  FActiveProcessList := TStringList.Create;
end;

destructor TLZSystemInformation.Destroy;
begin
  FreeAndNil(FDriveList);
  FreeAndNil(FNetworkIPList);
  FreeAndNil(FPrinterList);
  FreeAndNil(FShellFolderList);
  FreeAndNil(FInstalledSoftwareList);
  FreeAndNil(FActiveProcessList);
  inherited Destroy;
end;

function TLZSystemInformation.IsWOW64: Boolean;
type
  TIsWow64Process = function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
var
  IsWow64Result: BOOL;
  IsWow64Process: TIsWow64Process;
begin
  IsWow64Process := Windows.GetProcAddress(GetModuleHandle('kernel32'),
    'IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
      raise Exception.Create('Bad process handle');
    result := IsWow64Result;
  end
  else
    result := False;
end;

function TLZSystemInformation.GetBIOSVendor: string;
begin
  result := GetRegistryString(HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\', 'BIOSVendor');
end;

function TLZSystemInformation.GetSystemManufacturer: string;
begin
  result := GetRegistryString(HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\', 'SystemManufacturer');
end;

function TLZSystemInformation.GetSystemProductName: string;
begin
  result := GetRegistryString(HKEY_LOCAL_MACHINE,
    'HARDWARE\DESCRIPTION\System\Bios\', 'SystemProductName');
end;

function TLZSystemInformation.GetRegistryString(const RootKey: Windows.HKEY;
  const SubKey, Name: string): string;
var
  Reg: TRegistry; // registry access object
  ValueInfo: TRegDataInfo; // info about registry value
begin
  result := '';
  // Open registry at required root key
  Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
  try
    Reg.RootKey := RootKey;
    // Open registry key and check value exists
    if Reg.OpenKeyReadOnly(SubKey) and Reg.ValueExists(Name) then
    begin
      // Check if registry value is string or integer
      Reg.GetDataInfo(Name, ValueInfo);
      case ValueInfo.RegData of
        rdString, rdExpandString:
          // string value: just return it
          result := Reg.ReadString(Name);
        rdInteger:
          // integer value: convert to string
          result := IntToStr(Reg.ReadInteger(Name));
      else
        // unsupported value: raise exception
        raise ESystemInformationException.Create('Unsupported registry type');
      end;
    end;
  finally
    // Close registry
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function TLZSystemInformation.GetOSVersion: TLZOSVersion;
var
  osVerInfo: Windows.TOSVersionInfo;
  majorVersion, minorVersion, buildVersion: integer;
begin
  result := osUnknown;
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if Windows.GetVersionEx(osVerInfo) then
  begin
    minorVersion := osVerInfo.dwMinorVersion;
    majorVersion := osVerInfo.dwMajorVersion;
    buildVersion := TOSVersion.Build;
    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT:
        begin
          if majorVersion <= 4 then
            result := osWinNT
          else if (majorVersion = 5) and (minorVersion = 0) then
            result := osWin2000
          else if (majorVersion = 5) and (minorVersion = 1) then
            result := osWinXP
          else if (majorVersion = 6) and (minorVersion = 0) then
            result := osWinVista
          else if (majorVersion = 6) and (minorVersion = 1) then
            result := osWinSeven
          else if (majorVersion = 6) and (minorVersion = 2) then
            result := osWin8
          else if (majorVersion = 6) and (minorVersion = 3) then
            result := osWin81
          else if (majorVersion = 10) and (buildVersion < 22000) then
            result := osWin10
          else if (majorVersion = 10) and (buildVersion >= 22000) then
            result := osWin11;
        end;
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          if (majorVersion = 4) and (minorVersion = 0) then
            result := osWin95
          else if (majorVersion = 4) and (minorVersion = 10) then
          begin
            if osVerInfo.szCSDVersion[1] = 'A' then
              result := osWin98SE
            else
              result := osWin98;
          end
          else if (majorVersion = 4) and (minorVersion = 90) then
            result := osWinME
          else
            result := osUnknown;
        end;
    end;
  end;
end;

function TLZSystemInformation.GetOSVersionString: string;
begin
  result := TOSVersion.ToString;
end;

function TLZSystemInformation.GetOSArchitecture: string;
begin
  case TOSVersion.Architecture of
    arIntelX86:
      result := 'x86';
    arIntelX64:
      result := 'x64';
  else
    result := 'Unknown';
  end;
end;

function TLZSystemInformation.GetOSStartupState: TLZOSStartupState;
begin
  result := ssUnknown;
  case GetSystemMetrics(SM_CLEANBOOT) of
    0:
      result := ssNormal;
    1:
      result := ssSafeMode;
    2:
      result := ssSafeModeNetwork;
  end;
end;

function TLZSystemInformation.GetOSStartupStateString: string;
begin
  case GetOSStartupState of
    ssNormal:
      result := 'Normal Boot';
    ssSafeMode:
      result := 'Safe Mode: Fail-Safe Boot';
    ssSafeModeNetwork:
      result := 'Safe Mode: Fail-safe with network boot';
  else
    begin
      result := 'Unknown';
    end;
  end;
end;

function TLZSystemInformation.GetOSUptime: string;
const
  ticksperday: cardinal = 1000 * 60 * 60 * 24;
  ticksperhour: cardinal = 1000 * 60 * 60;
  ticksperminute: cardinal = 1000 * 60;
  tickspersecond: cardinal = 1000;
var
  t: longword;
  d, h, m, s: cardinal;
begin
  t := GetTickCount;

  d := t div ticksperday;
  Dec(t, d * ticksperday);

  h := t div ticksperhour;
  Dec(t, h * ticksperhour);

  m := t div ticksperminute;
  Dec(t, m * ticksperminute);

  s := t div tickspersecond;
  result := IntToStr(d) + ' Days ' + IntToStr(h) + ' Hours ' + IntToStr(m) +
    ' Minutes ' + IntToStr(s) + ' Seconds';
end;

function TLZSystemInformation.GetActiveProcessList: TStrings;
var
  WinHandleList: HWND;

  procedure GetAllWindowsProc(AHandle: HWND);
  var
    P: array [0 .. 256] of char;
  begin
    P[0] := #0;
    GetWindowText(AHandle, P, 255);
    if (P[0] <> #0) then
    begin
      if IsWindowVisible(AHandle) then
      begin
        FActiveProcessList.Add(P);
      end;
    end;
  end;

begin
  FActiveProcessList.Clear;
  WinHandleList := FindWindow(nil, nil);
  GetAllWindowsProc(WinHandleList);
  while (WinHandleList <> 0) do // While there are still windows
  begin
    WinHandleList := GetWindow(WinHandleList, GW_HWNDNEXT);
    GetAllWindowsProc(WinHandleList);
  end; // While
  result := FActiveProcessList;
end;

function TLZSystemInformation.GetDriveUsage(ADrive: char;
  var AFreeSpace: double; var ATotalSpace: double; var AUsage: integer)
  : Boolean;
begin
  result := True;
  try
    AFreeSpace := DiskFree(Ord(ADrive) - 64);
    ATotalSpace := DiskSize(Ord(ADrive) - 64);
    if ATotalSpace > 0 then
    begin
      AUsage := 100 - Round((AFreeSpace / ATotalSpace) * 100);
    end
    else
    begin
      AUsage := 0;
    end;
  except
    result := False;
  end;
end;

function TLZSystemInformation.GetDriveList: TStrings;
var
  Drive: char;
  DriveLetter: string;
  DriveType: string;
  FreeSpace, TotalSpace: double;
  Usage: integer;

begin
  FDriveList.Clear;
  for Drive := 'A' to 'Z' do
  begin
    DriveLetter := Drive + ':\';
    DriveType := '';
    TotalSpace := 0;
    FreeSpace := 0;
    Usage := 0;
    try
      case GetDriveType(PChar(Drive + ':\')) of
        DRIVE_REMOVABLE:
          begin
            DriveType := 'Floppy Drive';
          end;
        DRIVE_FIXED:
          begin
            DriveType := 'Fixed Drive';
            GetDriveUsage(Drive, FreeSpace, TotalSpace, Usage);
          end;
        DRIVE_REMOTE:
          begin
            DriveType := 'Network Drive';
            GetDriveUsage(Drive, FreeSpace, TotalSpace, Usage);
          end;
        DRIVE_CDROM:
          begin
            DriveType := 'CD-ROM Drive';
          end;
        DRIVE_RAMDISK:
          begin
            DriveType := 'RAM Disk';
            GetDriveUsage(Drive, FreeSpace, TotalSpace, Usage);
          end;
      end;
      if DriveType <> '' then
      begin
        FDriveList.Add(Format('%s - %s (Usage: %d%%)',
          [DriveLetter, DriveType, Usage]));
      end;
    except
    end;
  end;
  result := FDriveList;
end;

function TLZSystemInformation.GetPrinterList: TStrings;
begin
  FPrinterList.Assign(Printer.Printers);
  result := FPrinterList;
end;

function TLZSystemInformation.GetVideoCardDetails: string;
var
  lpDisplayDevice: TDisplayDevice;
  dwFlags: DWORD;
begin
  lpDisplayDevice.cb := SizeOf(lpDisplayDevice);
  dwFlags := 0;
  EnumDisplayDevices(nil, 0, lpDisplayDevice, dwFlags);
  result := lpDisplayDevice.DeviceString;
end;

function TLZSystemInformation.GetMemoryLoad: integer;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  result := Status.dwMemoryLoad;
end;

function TLZSystemInformation.GetMemoryTotalPhysical: string;
var
  MemStatus: TMemoryStatusEx;
begin
  FillChar(MemStatus, SizeOf(MemStatus), 0);
  MemStatus.dwLength := SizeOf(MemStatus);
  Win32Check(GlobalMemoryStatusEx(MemStatus));
  result := TLZString.FormatByteSize(MemStatus.ullTotalPhys);
end;

function TLZSystemInformation.GetMemoryFreePhysical: string;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  result := IntToStr((Status.dwAvailPhys div 1024) div 1024);
end;

function TLZSystemInformation.GetMemoryTotalPageFile: string;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  result := IntToStr((Status.dwTotalPageFile div 1024) div 1024);
end;

function TLZSystemInformation.GetMemoryFreePageFile: string;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  result := IntToStr((Status.dwAvailPageFile div 1024) div 1024);
end;

function TLZSystemInformation.GetMemoryTotalVirtual: string;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  result := IntToStr((Status.dwTotalVirtual div 1024) div 1024);
end;

function TLZSystemInformation.GetMemoryFreeVirtual: string;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  result := IntToStr((Status.dwAvailVirtual div 1024) div 1024);
end;

function TLZSystemInformation.GetCPUType: string;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Registry.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\SYSTEM\CentralProcessor\0');
    result := Registry.ReadString('Identifier');
  finally
    Registry.Free;
  end;
end;

function TLZSystemInformation.GetCPUUsage: double;
var
  LQuery: HQUERY;
  LCPUTotal: HCOUNTER;
  LCounterVal: TPdhFmtCounterValue;
begin
  PdhOpenQuery(nil, 0, LQuery);
  try
    PdhAddCounter(LQuery, '\Processor(_Total)\% Processor Time', 0, LCPUTotal);
    PdhCollectQueryData(LQuery);
    Sleep(500);
    PdhCollectQueryData(LQuery);
    PdhGetFormattedCounterValue(LCPUTotal, PDH_FMT_DOUBLE, nil, LCounterVal);
    result := LCounterVal.doubleValue;
  finally
    PdhCloseQuery(LQuery);
  end;
end;

function TLZSystemInformation.GetCPUVendor: string;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Registry.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\SYSTEM\CentralProcessor\0');
    result := Registry.ReadString('VendorIdentifier');
  finally
    Registry.Free;
  end;
end;

{$IFDEF CPUX64}

function GetCpuClockCycleCount: Int64; assembler; register;
asm
  dw 310Fh // rdtsc
  shl rdx, 32
  or rax, rdx
end;
{$ENDIF}

function TLZSystemInformation.GetCPUSpeed: double;
{$IFDEF CPUX86}
const
  DelayTime = 500; // measure time in ms
var
  TimerHi, TimerLo: DWORD;
  PriorityClass, Priority: integer;
begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetthreadPriority(GetCurrentthread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetthreadPriority(GetCurrentthread, thread_PRIORITY_TIME_CRITICAL);
  Sleep(10);
  asm
    dw 310Fh // rdtsc
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  Sleep(DelayTime);
  asm
    dw 310Fh // rdtsc
    sub eax, TimerLo
    sbb edx, TimerHi
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  SetthreadPriority(GetCurrentthread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  result := TimerLo / (1000.0 * DelayTime);
end;
{$ELSE}

const
  DelayTime = 500;
var
  x, y: UInt64;
  PriorityClass, Priority: integer;
begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority := GetthreadPriority(GetCurrentthread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetthreadPriority(GetCurrentthread, thread_PRIORITY_TIME_CRITICAL);
  Sleep(10);
  x := GetCpuClockCycleCount;
  Sleep(DelayTime);
  y := GetCpuClockCycleCount;
  SetthreadPriority(GetCurrentthread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  result := ((y - x) and $FFFFFFFF) / (1000 * DelayTime);
end;
{$ENDIF}

function TLZSystemInformation.GetCPUSpeedString: string;
var
  CPUSpeed: real;
  CPUSpeedMeasure: string;
begin
  CPUSpeed := GetCPUSpeed;
  if CPUSpeed >= 1000 then
  begin
    CPUSpeed := (CPUSpeed / 1000);
    CPUSpeedMeasure := 'GHz';
  end
  else
  begin
    CPUSpeed := Round(CPUSpeed);
    CPUSpeedMeasure := 'MHz';
  end;
  result := FormatFloat('#.##', CPUSpeed) + ' ' + CPUSpeedMeasure;
end;

function TLZSystemInformation.GetNetworkUserName: string;
var
  UserName: string;
  UserNameLen: DWORD;
begin
  UserNameLen := 255;
  SetLength(UserName, UserNameLen);
  if GetUserName(PChar(UserName), UserNameLen) then
    result := Copy(UserName, 1, UserNameLen - 1)
  else
    result := 'Unknown';
end;

function TLZSystemInformation.GetNetworkComputerName: string;
var
  lpBuffer: PWideChar; // lpstr;
  success: Boolean;
  nSize: DWORD;
begin
  nSize := 255;
  result := '';
  GetMem(lpBuffer, 255);
  try
    success := GetComputerName(lpBuffer, nSize);
    if success then
      result := StrPas(lpBuffer)
    else
      result := '';
  finally
    FreeMem(lpBuffer);
  end;
end;

function TLZSystemInformation.GetNetworkIPAddress: string;
begin
  result := '';
  TIdStack.IncUsage;
  try
    if Assigned(GStack) then
    begin
      result := TLZString.StripExtraSpaces(GStack.LocalAddress, True, False);
    end;
  finally
    TIdStack.DecUsage;
  end;
  if TLZString.IsEmptyString(result) then
  begin
    result := '127.0.0.1';
  end;
end;

function TLZSystemInformation.GetNetworkIPList: TStrings;
begin
  FNetworkIPList.Clear;
  TIdStack.IncUsage;
  try
    if Assigned(GStack) then
    begin
      FNetworkIPList.Add(TLZString.StripExtraSpaces(GStack.LocalAddresses.Text,
        True, False));
    end;
  finally
    TIdStack.DecUsage;
    FNetworkIPList.Add('127.0.0.1');
  end;
  result := FNetworkIPList;
end;

function TLZSystemInformation.GetInstalledSoftwareList: TStrings;

var
  Registry: TRegistry;
  KeyList: TStringList;
  ProductList: TStringList;
  i, n: integer;

begin
  Registry := TRegistry.Create;
  KeyList := TStringList.Create;
  ProductList := TStringList.Create;
  FInstalledSoftwareList.Clear;
  try
    with Registry do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall')
      then
      begin
        GetKeyNames(KeyList);
        CloseKey;
        for i := 0 to Pred(KeyList.Count) do
        begin
          if OpenKeyReadOnly
            ('\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\' +
            KeyList.Strings[i]) then
          begin
            GetValueNames(ProductList);
            n := ProductList.IndexOf('DisplayName');
            if (n <> -1) and (ProductList.IndexOf('UninstallString') <> -1) then
            begin
              FInstalledSoftwareList.Add
                (Registry.ReadString(ProductList.Strings[n]));
            end;
          end;
        end;
      end
      else
      begin
        FInstalledSoftwareList.Add('Unable to obtain software list.');
      end;
      CloseKey;
    end;
  finally
    FreeAndNil(Registry);
    FreeAndNil(KeyList);
    FreeAndNil(ProductList);
  end;
  result := FInstalledSoftwareList;
end;

function TLZSystemInformation.GetSessionType: string;
begin
  result := 'Local Session';
  if TLZSystem.IsRemoteSession then
  begin
    result := 'RDP Session';
  end;
  if TLZSystem.IsCitrixSession then
  begin
    result := 'Citrix Session';
  end;
end;

function TLZSystemInformation.GetShellFolderList: TStrings;
begin
  FShellFolderList.Clear;
  FShellFolderList.Add('PERSONAL=' + TLZFile.GetUserProfileFolder);
  FShellFolderList.Add('APPDATA=' + TLZFile.getUserAppDataFolder);
  FShellFolderList.Add('LOCAL_APPDATA=' + TLZFile.GetUserLocalAppDataFolder);

  FShellFolderList.Add('COMMON_APPDATA=' + TLZFile.GetCommonAppDataFolder);
  FShellFolderList.Add('WINDOWS=' + TLZFile.GEtWindowsFolder);
  FShellFolderList.Add('SYSTEM=' + TLZFile.GetWindowsSystemFolder);
  FShellFolderList.Add('PROGRAM_FILES=' + TLZFile.GetProgramFilesFolder
    (oaUnkown));
  FShellFolderList.Add('MYPICTURES=' + TLZFile.GetPicturesFolder);
  FShellFolderList.Add('COMMON_DOCUMENTS=' + TLZFile.GetPublicDocumentFolder);
  result := FShellFolderList;
end;

function TLZSystemInformation.DriveUsage(ADrive: char): integer;
var
  FreeSpace, TotalSpace: double;
begin
  if not GetDriveUsage(ADrive, FreeSpace, TotalSpace, result) then
  begin
    result := -1;
  end;
end;

function TLZSystemInformation.GetNetworkMACAddress: string;
type
  // Based on code at MSDN knowledge base Q118623 article at
  // http://support.microsoft.com/kb/q118623/}
  // According to MSDN this method should fail on Windows 6.0 (Vista) and later.
  // It has been known to fail on Vista, but works on Vista Home Premium SP1!
  // It would seem that the call will succeed if there's an active network with
  // Netbios over TCP enabled.
  // Again according to Microsoft, the function is unreliable on Windows 95, 98
  // and Me.

  // This type is defined in MSDN sample code, but tests have found this is
  // not needed (on XP Pro) and Adapter can be of type TAdapterStatus. This
  // method uses the type in case other OSs require it
  TAStat = packed record
    Adapt: TAdapterStatus;
    NameBuff: array [0 .. 29] of TNameBuffer;
  end;
var
  Adapter: TAStat;
  // info about a network adapter
  AdapterList: TLanaEnum; // numbers for current LAN adapters
  Ncb: TNCB; // network control block descriptor
  i: integer; // loops thru all adapters in list

  // Examines given NetBios API call return value to check if call succeeded.
  function NetBiosSucceeded(const RetCode: AnsiChar): Boolean;
  begin
    result := UCHAR(RetCode) = NRC_GOODRET;
  end;

begin
  // Assume not adapter
  result := '';
  // Get list of adapters
  FillChar(Ncb, SizeOf(Ncb), 0);
  Ncb.ncb_command := AnsiChar(NCBENUM);
  Ncb.ncb_buffer := PAnsiChar(@AdapterList);
  Ncb.ncb_length := SizeOf(AdapterList);
  if not NetBiosSucceeded(Netbios(@Ncb)) then
    Exit;
  // Get status of each adapter, exiting when first valid one reached
  // MSDN cautions us not to assume lana[0] is valid
  for i := 0 to Pred(integer(AdapterList.length)) do
  begin
    // reset the adapter
    FillChar(Ncb, SizeOf(Ncb), 0);
    Ncb.ncb_command := AnsiChar(NCBRESET);
    Ncb.ncb_lana_num := AdapterList.lana[i];
    if not NetBiosSucceeded(Netbios(@Ncb)) then
      Exit;
    // get status of adapter
    FillChar(Ncb, SizeOf(Ncb), 0);
    Ncb.ncb_command := AnsiChar(NCBASTAT);
    Ncb.ncb_lana_num := AdapterList.lana[i];
    Ncb.ncb_callname := '*               ';
    Ncb.ncb_buffer := PAnsiChar(@Adapter);
    Ncb.ncb_length := SizeOf(Adapter);
    if NetBiosSucceeded(Netbios(@Ncb)) then
    begin
      // we have a MAC address: return it
      with Adapter.Adapt do
      begin
        if not TLZString.IsEmptyString(result) then
          result := result + #13#10;

        result := result + Format('%.2x-%.2x-%.2x-%.2x-%.2x-%.2x',
          [Ord(adapter_address[0]), Ord(adapter_address[1]),
          Ord(adapter_address[2]), Ord(adapter_address[3]),
          Ord(adapter_address[4]), Ord(adapter_address[5])]);
      end;
      // Exit;
    end;
  end;
end;

function TLZSystemInformation.SimpleReport: string;
var
  Report: TStringList;
begin
  Report := TStringList.Create;
  try
    try
      Report.Add('--- OS ---');
      Report.Add('OS Version: ' + GetOSVersionString);
      Report.Add('OS startup state: ' + OSStartupState);
      Report.Add('OS uptime: ' + OSUptime);
      Report.Add('OS 64bit: ' + BoolToStr(OS64, True));
      Report.Add('');
      Report.Add('--- Hardware ---');
      Report.Add('CPU type: ' + CPUType);
      Report.Add('CPU vendor: ' + CPUVendor);
      Report.Add('CPU speed: ' + CPUSpeed);
      Report.Add('CPU load: ' + FormatFloat('##0.00', CPUUsage) + '%');
      Report.Add('MEM total (Physical): ' + MemoryTotalPhysical);
      Report.Add('MEM free (Physical): ' + MemoryFreePhysical);
      Report.Add('MEM load: ' + IntToStr(MemoryLoad) + '%');
      Report.Add('Video card: ' + GetVideoCardDetails);
      Report.Add('Storage details');
      Report.Add(DriveList.Text);
      Report.Add('');
      Report.Add('--- Network ---');
      Report.Add('Computer name: ' + NetworkComputerName);
      Report.Add('IP addresses: ' + TLZString.StringCleaner(NetworkIPList.Text,
        True, True));
      Report.Add('');
      Report.Add('--- Session ---');
      Report.Add('Username: ' + NetworkUserName);
      Report.Add('Session type: ' + SessionType);

    except
      on E: Exception do
      begin
        Report.Add('---');
        Report.Add('Report Error: ' + E.Message);
        Report.Add('---');
      end;
    end;
  finally
    result := Report.Text;
    FreeAndNil(Report);
  end;
end;

end.
