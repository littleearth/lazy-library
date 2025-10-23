unit VCL.Lazy.Utils.Windows;

interface

uses Windows, Messages, SysUtils, Classes, Lazy.Utils.Base,
  VCL.Lazy.Types.Windows,
  Lazy.Types;

type
  TLZFile = class(TLZFileBase)
  public
    class function GetKnownFolderPath(AGUID: TGUID): string;
    class function GetDesktopFolder: string;
    class function GetDocumentFolder: string;
    class function GetPublicDocumentFolder: string;
    class function GetVideoFolder: string;
    class function GetPicturesFolder: string;
    class function GetUserProfileFolder: string;
    class function GetUserAppDataFolder: string;
    class function GetUserLocalAppDataFolder: string;
    class function GetCommonAppDataFolder: string;
    class function GetWindowsSystemFolder: string;
    class function GetWindowsSystemX86Folder: string;
    class function GetWindowsFolder: string;
    class function GetFileAgeDays(AFileName: TFileName): integer;
    class function GetProgramFilesFolder(APlatform: TLZOSArchitecture): string;
    class function GetApplicationTempFolder(AIncludePID: Boolean = true;
      AUseCommonAppData: Boolean = false): string;
    class function InUse(AFileName: TFileName): Boolean;
    class function Delete(AFileName: string; ARecycleBin: Boolean = false)
      : Boolean; reintroduce;
    class function DeleteOnReboot(AFileName: string): Boolean;
    class function CopyFileToClipboard(AFileName: TFileName): Boolean;
    class function CopyFilesToClipboard(AFiles: TStrings): Boolean;
    class function GetMIMEType(AFileName: TFileName): string;
    class function GetAssociatedApplication(AExtension: string): string;
    class function OpenWith(AFileName: string): Boolean;
    class function ExecuteFile(const Operation, FileName, Params,
      DefaultDir: string; ShowCmd: word): integer;
    class function OpenDefaultAssociatedApplication(AFileName: string;
      APrint: Boolean = false; AAllowOpenWith: Boolean = true;
      AEdit: Boolean = false): Boolean;
    class function ExecuteProcess(ACommand: string; AParams: string;
      ADefaultDir: string; AShowCmd: word; var APID: Cardinal;
      var AExitCode: word; AVerb: string = 'open'; AWait: Boolean = false)
      : Boolean; overload;
    class function ExecuteProcess(ACommand: string; AParams: string;
      ADefaultDir: string; AShowCmd: word; var APID: Cardinal;
      AVerb: string = 'open'; AWait: Boolean = false): Boolean; overload;
    class function ExecuteAndWait(const ACommand: string;
      const AParams: string = ''; AShowCmd: word = SW_HIDE): integer;
    class function OpenFolderAndSelectFile(const AFileName: string): Boolean;
    class function OpenFolder(const AFolder: string): Boolean;
  end;

  TLZSystem = class(TLZSystemBase)
  public
    class function GetElevationLevel(var AIsElevated: Boolean)
      : TLZOSElevationLevel;
    class function IsDelphiRunning: Boolean;
    class function IsAdministrator: Boolean;
    class function IsRemoteSession: Boolean;
    class function IsCitrixSession: Boolean;
    class function IsUACEnabled: Boolean;
    class function IsCapsLockOn: Boolean;
    class function IsNumLockOn: Boolean;
    class function IsScrollLockOn: Boolean;
    class function IsApplicationElevated: Boolean;
    class function IsSystemX64: Boolean;
    class function IsMachineInDomain: Boolean;
    class function IsDarkModeIsEnabled: Boolean;
    class procedure RestartApplication;
    class function ExpandEnvironmentVariable(const AString: String): String;
    class procedure GetEnvironmentVariables(AVariables: TStrings);
    class function GetEnvironmentVariableValue(AVariable: string): string;
    class function GetApplicationParameters(AParameter: string;
      var AValue: string): Boolean;
    class procedure Delay(ATime: integer);
  end;

  TLZString = class(TLZStringBase)
  public
    class function AlphaFriendlyStringCompare(S1: string; S2: string): integer;
    class function WinErrorString(WinError: integer): string;
  end;

  TLZURL = class(TLZURLBase)
  public
    class procedure OpenDefaultBrowser(AURL: string);
    class procedure OpenDefaultMailClient(ARecipient: string;
      ASubject: string = '');
  end;

  TLZDateTime = class(TLZDateTimeBase);
  TLZBoolean = class(TLZBooleanBase);
  TLZMath = class(TLZMathBase);

implementation

uses
  WinAPi.ShLwApi, WinAPi.psAPI, WinAPi.KnownFolders, WinAPi.ShlObj, VCL.Forms,
  System.Win.Registry, VCL.Clipbrd, WinAPi.ShellApi, IdGlobalProtocols,
  System.IOUtils, System.DateUtils;

{ TLZString }

class function TLZString.AlphaFriendlyStringCompare(S1, S2: string): integer;
begin
  Result := StrCmpLogicalW(PChar(S1), PChar(S2));
end;

{ TLZApplication }

class function TLZSystem.IsAdministrator: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: Cardinal;
  psidAdministrators: PSID;
  x: integer;
begin
  Result := false;
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;
  if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY, true, hAccessToken) then
  begin
    if GetLastError <> ERROR_NO_TOKEN then
      Exit;
    if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken) then
      Exit;
  end;
  try
    GetTokenInformation(hAccessToken, TokenGroups, nil, 0, dwInfoBufferSize);
    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      Exit;
    GetMem(ptgGroups, dwInfoBufferSize);
    try
      if not GetTokenInformation(hAccessToken, TokenGroups, ptgGroups,
        dwInfoBufferSize, dwInfoBufferSize) then
        Exit;
      if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0,
        psidAdministrators) then
        Exit;
      try
        for x := 0 to ptgGroups^.GroupCount - 1 do
        begin
          if EqualSid(psidAdministrators, ptgGroups^.Groups[x].Sid) then
          begin
            Result := true;
            Break;
          end;
        end;
      finally
        FreeSid(psidAdministrators);
      end;
    finally
      FreeMem(ptgGroups);
    end;
  finally
    CloseHandle(hAccessToken);
  end;
end;

class function TLZString.WinErrorString(WinError: integer): string;
var
  A: array [0 .. MAX_PATH] of char;
begin
  FillChar(A, SizeOf(A), #0);
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, WinError, LANG_SYSTEM_DEFAULT,
    @A, MAX_PATH, nil);
  Result := string(A);
end;

{ TLZFile }

class function TLZFile.CopyFileToClipboard(AFileName: TFileName): Boolean;
var
  DropFiles: PDropFiles;
  hGlobal: THandle;
  iLen: integer;
begin
  Result := false;
  iLen := Length(AFileName);
  hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
    SizeOf(TDropFiles) + ((iLen + 2) * SizeOf(char)));
  if (hGlobal <> 0) then
  begin
    try
      DropFiles := GlobalLock(hGlobal);
      if (DropFiles <> nil) then
      begin
        try
          DropFiles^.pFiles := SizeOf(TDropFiles);

          DropFiles^.fWide := true;

          if AFileName <> '' then
            System.Move(AFileName[1], (PByte(DropFiles) + SizeOf(TDropFiles))^,
              iLen * SizeOf(char));
        finally
          GlobalUnlock(hGlobal);
        end;
        Clipboard.SetAsHandle(CF_HDROP, hGlobal);
        Result := true;
      end;
    except
      GlobalFree(hGlobal);
    end;
  end;
end;

class function TLZFile.Delete(AFileName: string; ARecycleBin: Boolean): Boolean;
var
  LSHFileOpStruct: TSHFileOpStruct;
begin
  Result := false;
  if FileExists(AFileName) then
  begin
    if ARecycleBin then
    begin
      ZeroMemory(@LSHFileOpStruct, SizeOf(TSHFileOpStruct));
      with LSHFileOpStruct do
      begin
        Wnd := Application.Handle;
        wFunc := FO_DELETE;
        pFrom := PChar(AFileName + #0);
        fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT or
          FOF_NOERRORUI;
      end;
      ShFileOperation(LSHFileOpStruct);
    end
    else
    begin
      TFile.Delete(AFileName);
    end;
    Result := not FileExists(AFileName);
  end;
end;

class function TLZFile.DeleteOnReboot(AFileName: string): Boolean;
begin
  Result := MoveFileEx(PChar(AFileName), nil, MOVEFILE_DELAY_UNTIL_REBOOT);
end;

class function TLZFile.ExecuteAndWait(const ACommand, AParams: string;
  AShowCmd: word): integer;
var
  LPID: Cardinal;
  LExitCode: word;
begin
  Result := 0;
  if ExecuteProcess(ACommand, AParams, GetCurrentDir, AShowCmd, LPID, LExitCode,
    'open', true) then
  begin
    Result := LExitCode;
  end;
end;

class function TLZFile.ExecuteFile(const Operation, FileName, Params,
  DefaultDir: string; ShowCmd: word): integer;
begin
  Result := ShellExecute(Application.Handle, PWideChar(Operation),
    PWideChar(FileName), PWideChar(Params), PChar(DefaultDir), ShowCmd);
end;

class function TLZFile.ExecuteProcess(ACommand, AParams, ADefaultDir: string;
  AShowCmd: word; var APID: Cardinal; var AExitCode: word; AVerb: string;
  AWait: Boolean): Boolean;
var
  LSEInfo: TShellExecuteInfo;
  LProcess: DWord;
  LExitCode: DWord;
begin
  APID := 0;
  Result := false;
  AExitCode := 0;
  FillChar(LSEInfo, SizeOf(LSEInfo), 0);
  LSEInfo.cbSize := SizeOf(TShellExecuteInfo);
  with LSEInfo do
  begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(ACommand);
    lpVerb := PChar(AVerb);
    lpParameters := PChar(AParams);
    lpDirectory := PChar(ADefaultDir);
    nShow := AShowCmd;
  end;
  if ShellExecuteEx(@LSEInfo) then
  begin
    APID := GetProcessId(LSEInfo.hProcess);
    LProcess := LSEInfo.hProcess;
    if AWait then
    begin
      while WaitForSingleObject(LProcess, 50) <> WAIT_OBJECT_0 do
        Application.ProcessMessages;

      if GetExitCodeProcess(LProcess, LExitCode) then
      begin
        AExitCode := LExitCode;
      end;

      CloseHandle(LProcess);
    end;
    Result := true;
  end;
end;

class function TLZFile.ExecuteProcess(ACommand, AParams, ADefaultDir: string;
  AShowCmd: word; var APID: Cardinal; AVerb: string; AWait: Boolean): Boolean;
var
  LExitCode: word;
begin
  Result := ExecuteProcess(ACommand, AParams, ADefaultDir, AShowCmd, APID,
    LExitCode, AVerb, AWait);
end;

class function TLZFile.CopyFilesToClipboard(AFiles: TStrings): Boolean;
var
  FileList: string;
begin
  FileList := AFiles.Text;
  FileList := StringReplace(FileList, #10, '', [rfReplaceAll]);
  FileList := StringReplace(FileList, #13, #0, [rfReplaceAll]);
  FileList := FileList + #0#0;
  Result := CopyFileToClipboard(FileList);
end;

class function TLZFile.GetCommonAppDataFolder: string;
begin
  Result := IncludeTrailingPathDelimiter
    (GetKnownFolderPath(FOLDERID_LocalAppDataLow) + Application.Title);
  CheckDirectoryExists(Result, true);
end;

class function TLZFile.GetDesktopFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Desktop);
end;

class function TLZFile.GetDocumentFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Documents);
end;

class function TLZFile.GetFileAgeDays(AFileName: TFileName): integer;
var
  LDate: TDateTime;
begin
  try
    LDate := TFile.GetLastAccessTime(AFileName);
    Result := DaysBetween(Now, LDate);
  except
    Result := -1;
  end;
end;

class function TLZFile.GetKnownFolderPath(AGUID: TGUID): string;
var
  LFolderPath: PWideChar;
begin
  Result := '';
  if SHGetKnownFolderPath(AGUID, 0, 0, LFolderPath) = S_OK then
  begin
    Result := IncludeTrailingPathDelimiter(LFolderPath);
  end;
end;

class function TLZFile.GetMIMEType(AFileName: TFileName): string;
begin
  if SameText(ExtractFileExt(AFileName), '.woff') then
  begin
    Result := 'application/x-font-woff';
  end;
  if SameText(ExtractFileExt(AFileName), '.woff2') then
  begin
    Result := 'application/x-font-woff2';
  end;
  if SameText(ExtractFileExt(AFileName), '.js') then
  begin
    Result := 'application/javascript';
  end;
  if TLZString.IsEmptyString(Result) then
  begin
    Result := GetMIMETypeFromFile(AFileName);
  end;
  if TLZString.IsEmptyString(Result) then
  begin
    // RFC 7231 https://tools.ietf.org/html/rfc7231#page-11
    Result := '';
  end;
end;

class function TLZFile.GetPicturesFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Pictures);
end;

class function TLZFile.GetProgramFilesFolder
  (APlatform: TLZOSArchitecture): string;
begin
  case APlatform of
    oaX86:
      begin
        Result := GetKnownFolderPath(FOLDERID_ProgramFilesX86);
      end;
    oaX64:
      begin
        Result := GetKnownFolderPath(FOLDERID_ProgramFilesX64);
      end;
  else
    begin
      Result := GetKnownFolderPath(FOLDERID_ProgramFiles);
    end;
  end;
end;

class function TLZFile.GetApplicationTempFolder(AIncludePID, AUseCommonAppData
  : Boolean): string;
var
  LTitle: string;
begin
  LTitle := Application.Title;
  if TLZString.IsEmptyString(LTitle) then
    LTitle := ExtractFileName(ParamStr(0));
  if AUseCommonAppData then
  begin
    Result := GetCommonAppDataFolder;
    Result := IncludeTrailingPathDelimiter(Result + LTitle);
    Result := IncludeTrailingPathDelimiter(Result + 'Temp');
  end
  else
  begin
    Result := IncludeTrailingPathDelimiter(GetTempFolder);
    Result := IncludeTrailingPathDelimiter(Result + LTitle);
  end;
  if AIncludePID then
  begin
    Result := IncludeTrailingPathDelimiter
      (Result + IntToStr(GetCurrentProcessId));
  end;
  CheckDirectoryExists(Result, true);
end;

class function TLZFile.GetAssociatedApplication(AExtension: string): string;
var
  Buffer: array [0 .. 1024] of char;
  bufsize: DWord;
  Res: HResult;
begin
  Result := '';
  try
    bufsize := SizeOf(Buffer);
    Buffer[0] := #0;
    Res := AssocQueryString(ASSOCF_REMAPRUNDLL or ASSOCF_NOTRUNCATE,
      ASSOCSTR_EXECUTABLE, PChar(AExtension), nil, Buffer, @bufsize);
    If Res = S_OK then
    begin
      Result := Buffer;
    end;
  except

  end;
end;

class function TLZFile.GetPublicDocumentFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_PublicDesktop);
end;

class function TLZFile.GetUserAppDataFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_RoamingAppData);
  Result := IncludeTrailingPathDelimiter(Result + Application.Title);
  CheckDirectoryExists(Result, true);
end;

class function TLZFile.GetUserLocalAppDataFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_LocalAppData);
  Result := IncludeTrailingPathDelimiter(Result + Application.Title);
  CheckDirectoryExists(Result, true);
end;

class function TLZFile.GetUserProfileFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Profile);
end;

class function TLZFile.GetVideoFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Videos);
end;

class function TLZFile.GetWindowsFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Windows);
end;

class function TLZFile.GetWindowsSystemFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_System);
end;

class function TLZFile.GetWindowsSystemX86Folder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_SystemX86);
end;

class function TLZFile.InUse(AFileName: TFileName): Boolean;
var
  HFileRes: HFILE;
begin
  Result := false;
  if not FileExists(AFileName) then
    Exit;
  HFileRes := CreateFile(PChar(AFileName), GENERIC_READ or GENERIC_WRITE, 0,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;

class function TLZFile.OpenDefaultAssociatedApplication(AFileName: string;
  APrint, AAllowOpenWith, AEdit: Boolean): Boolean;
var
  Command: string;
begin
  Result := false;
  Command := 'open';
  if APrint then
    Command := 'print';
  if AEdit then
    Command := 'edit';
  if FileExists(AFileName) then
  begin
    if GetAssociatedApplication(ExtractFileExt(AFileName)) <> '' then
    begin
      Result := ExecuteFile(Command, AFileName, '', ExtractFilePath(AFileName),
        SW_NORMAL) > 32;
    end
    else
    begin
      if AAllowOpenWith then
      begin
        Result := OpenWith(AFileName);
      end;
    end;
  end;
end;

class function TLZFile.OpenFolder(const AFolder: string): Boolean;
begin
  Result := ExecuteFile('open', PChar('explorer.exe'), PChar(AFolder), '',
    SW_SHOWNORMAL) > 32;
end;

class function TLZFile.OpenFolderAndSelectFile(const AFileName: string)
  : Boolean;
var
  LIDL: PItemIDList;
begin
  Result := false;
  LIDL := ILCreateFromPath(PChar(AFileName));
  if LIDL <> nil then
    try
      Result := SHOpenFolderAndSelectItems(LIDL, 0, nil, 0) = S_OK;
    finally
      ILFree(LIDL);
    end;
end;

class function TLZFile.OpenWith(AFileName: string): Boolean;
begin
  Result := ExecuteFile('open', PChar('rundll32.exe'),
    PChar('shell32.dll,OpenAs_RunDLL ' + AFileName), '', SW_SHOWNORMAL) > 32;
end;

class function TLZSystem.IsCapsLockOn: Boolean;
begin
  Result := 0 <> (GetKeyState(VK_CAPITAL) and $01);
end;

class function TLZSystem.IsCitrixSession: Boolean;
begin
  try
    Result := (Copy(GetEnvironmentVariableValue('SESSIONNAME'), 1, 4) = 'ICA-');
  except
    Result := false;
  end;
end;

class function TLZSystem.IsDelphiRunning: Boolean;
begin
{$WARN SYMBOL_PLATFORM OFF}
  Result := DebugHook <> 0;
{$WARN SYMBOL_PLATFORM ON}
end;

class function TLZSystem.IsMachineInDomain: Boolean;
type
  TNetRenameMachineInDomain = function(lpServer, MachineName, lpAccount,
    Password: PWideChar; Options: LongInt): LongInt stdcall;
var
  ResultCode: integer;
  NetRenameMachineInDomain: TNetRenameMachineInDomain;
  NetAPIHandle: THandle;
const // ref : lmerr.h
  NERR_BASE = 2100;
  // This machine is already joined to a domain.
  NERR_SetupAlreadyJoined = (NERR_BASE + 591);
  // This machine is not currently joined to a domain.
  NERR_SetupNotJoined = (NERR_BASE + 592);
  // This machine is a domain controller and cannot be unjoined from a domain.
  NERR_SetupDomainController = (NERR_BASE + 593);
  // The destination domain controller does not support
  // creating machine accounts in OUs.
  NERR_DefaultJoinRequired = (NERR_BASE + 594);
  // The specified workgroup name is invalid.
  NERR_InvalidWorkgroupName = (NERR_BASE + 595);
  // The specified computer name is incompatible with the
  // default language used on the domain controller.
  NERR_NameUsesIncompatibleCodePage = (NERR_BASE + 596);
  // The specified computer account could not be found.
  // Contact an administrator to verify the account is in the domain.
  // If the account has been deleted unjoin, reboot, and rejoin the domain.
  NERR_ComputerAccountNotFound = (NERR_BASE + 597);
  // This version of Windows cannot be joined to a domain.
  NERR_PersonalSku = (NERR_BASE + 598);
  // An attempt to resolve the DNS name of a DC in the domain being joined has failed.
  // Please verify this client is configured to reach a DNS server that can
  // resolve DNS names in the target domain.
  NERR_SetupCheckDNSConfig = (NERR_BASE + 599);
begin
  try
    NetAPIHandle := LoadLibrary(PChar('netapi32.dll'));
    @NetRenameMachineInDomain := GetProcAddress(NetAPIHandle,
      PChar('NetRenameMachineInDomain'));
    ResultCode := NetRenameMachineInDomain(nil, nil, nil, nil, 0);
    FreeLibrary(NetAPIHandle);
  finally
  end;

  Result := ResultCode <> NERR_SetupNotJoined;
end;

class function TLZSystem.IsDarkModeIsEnabled: Boolean;
const
  TheKey = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  TheValue = 'AppsUseLightTheme';
var
  LRegistry: TRegistry;
begin
  Result := false;
  LRegistry := TRegistry.Create(KEY_READ);
  try
    LRegistry.RootKey := HKEY_CURRENT_USER;
    if LRegistry.KeyExists(TheKey) then
      if LRegistry.OpenKey(TheKey, false) then
        try
          if LRegistry.ValueExists(TheValue) then
            Result := LRegistry.ReadInteger(TheValue) = 0;
        finally
          LRegistry.CloseKey;
        end;
  finally
    LRegistry.Free;
  end;
end;

class function TLZSystem.IsNumLockOn: Boolean;
begin
  Result := 0 <> (GetKeyState(VK_NUMLOCK));
end;

class function TLZSystem.IsRemoteSession: Boolean;
const
  sm_RemoteSession = $1000; { from WinUser.h }
begin
  try
    Result := (GetSystemMetrics(sm_RemoteSession) <> 0);
  except
    on E: Exception do
    begin
      Result := false;
    end;
  end;
end;

class function TLZSystem.IsScrollLockOn: Boolean;
begin
  Result := 0 <> (GetKeyState(VK_SCROLL));
end;

class function TLZSystem.IsUACEnabled: Boolean;
var
  LRegistry: TRegistry;
begin
  Result := false;
  try
    LRegistry := TRegistry.Create;
    try
      LRegistry.RootKey := HKEY_LOCAL_MACHINE;
      if LRegistry.OpenKeyReadOnly
        ('SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
      begin
        if LRegistry.KeyExists('EnableLUA') then
        begin
          Result := LRegistry.ReadInteger('EnableLUA') = 1;
        end;
      end;
    finally
      FreeAndNil(LRegistry);
    end;
  except
    Result := false;
  end;
end;

class function TLZSystem.IsSystemX64: Boolean;
{$IFNDEF CPUX64}
type
  TIsWow64Process = function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
var
  IsWow64Result: BOOL;
  IsWow64Process: TIsWow64Process;
{$ENDIF}
begin
{$IFDEF CPUX64}
  Result := true;
{$ELSE}
  IsWow64Process := Windows.GetProcAddress(GetModuleHandle('kernel32'),
    'IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    if not IsWow64Process(GetCurrentProcess, IsWow64Result) then
      raise Exception.Create('Bad process handle');
    Result := IsWow64Result;
  end
  else
    Result := false;
{$ENDIF}
end;

class function TLZSystem.GetApplicationParameters(AParameter: string;
  var AValue: string): Boolean;
var
  LParamIdx: integer;
  LParameter: string;
begin
  Result := false;
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
        Result := true;
      end;
    finally
      Inc(LParamIdx);
    end;
  end;
end;

class function TLZSystem.GetElevationLevel(var AIsElevated: Boolean)
  : TLZOSElevationLevel;
const
  TokenElevationType = 18;
  TokenElevation = 20;
  TokenElevationTypeDefault = 1;
  TokenElevationTypeFull = 2;
  TokenElevationTypeLimited = 3;

var
  token: THandle;
  ElevationType: integer;
  Elevation: DWord;
  dwSize: Cardinal;
begin
  Result := elUnknown;
  AIsElevated := false;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token) then
  begin
    try
      if GetTokenInformation(token, TTokenInformationClass(TokenElevationType),
        @ElevationType, SizeOf(ElevationType), dwSize) then
      begin
        case ElevationType of
          TokenElevationTypeDefault:
            Result := elDefault;
          TokenElevationTypeFull:
            Result := elFull;
          TokenElevationTypeLimited:
            Result := elLimited;
        else
          Result := elUnknown;
        end;
      end;
      if GetTokenInformation(token, TTokenInformationClass(TokenElevation),
        @Elevation, SizeOf(Elevation), dwSize) then
      begin
        if Elevation = 0 then
          AIsElevated := false
        else
          AIsElevated := true;
      end;
    finally
      CloseHandle(token);
    end;
  end;

end;

class function TLZSystem.IsApplicationElevated: Boolean;
begin
  if Win32MajorVersion <= 5 then
  begin
    Result := IsAdministrator;
  end
  else
  begin
    GetElevationLevel(Result);
  end;
end;

class procedure TLZSystem.RestartApplication;
begin
  ShellExecute(Application.Handle, nil, PChar(Application.ExeName), nil, nil,
    SW_SHOWNORMAL);
  Application.Terminate;
end;

class function TLZSystem.GetEnvironmentVariableValue(AVariable: string): string;
var
  Buffer: integer;
begin
  Buffer := GetEnvironmentVariable(PChar(AVariable), nil, 0);
  if Buffer > 0 then
  begin
    SetLength(Result, Buffer - 1);
    GetEnvironmentVariable(PChar(AVariable), PChar(Result), Buffer);
  end
  else
    Result := '';
end;

class procedure TLZSystem.Delay(ATime: integer);
var
  i: integer;
begin
  for i := 0 to ATime do
  begin
    Sleep(1);
    Application.ProcessMessages;
  end;
end;

class function TLZSystem.ExpandEnvironmentVariable(const AString
  : String): String;
var
  bufsize: integer;
begin
  bufsize := ExpandEnvironmentStrings(PChar(AString), nil, 0);
  SetLength(Result, bufsize);
  ExpandEnvironmentStrings(PChar(AString), PChar(Result), bufsize);
  Result := TrimRight(Result);
end;

class procedure TLZSystem.GetEnvironmentVariables(AVariables: TStrings);
var
  LVariable: Boolean;
  LStr: PChar;
  LRes: string;
begin
  LStr := GetEnvironmentStrings;
  LRes := '';
  LVariable := false;
  while true do
  begin
    if LStr^ = #0 then
    begin
      if LVariable then
        AVariables.Add(LRes);
      LVariable := true;
      Inc(LStr);
      LRes := '';
      if LStr^ = #0 then
        Break
      else
        LRes := LRes + LStr^;
    end
    else if LVariable then
      LRes := LRes + LStr^;
    Inc(LStr);
  end;
end;

{ TLZURL }

class procedure TLZURL.OpenDefaultBrowser(AURL: string);
var
  URL: string;
  Allow: Boolean;
begin
  Allow := true;
  URL := AURL;
  if Allow then
    Allow := Trim(AURL) <> '';
  if Allow then
  begin
    if (Pos('http', URL) = 0) and (Pos('mailto', URL) = 0) and
      (Pos('ftp', URL) = 0) then
    begin
      URL := 'http://' + URL;
    end;
  end;
  if Allow then
  begin
    TLZFile.ExecuteFile('open', URL, '', TLZFile.GetApplicationDir,
      SW_SHOWNORMAL);
  end;
end;

class procedure TLZURL.OpenDefaultMailClient(ARecipient, ASubject: string);
begin
  OpenDefaultBrowser('mailto:' + ARecipient + '?subject=' + ASubject);
end;

end.
