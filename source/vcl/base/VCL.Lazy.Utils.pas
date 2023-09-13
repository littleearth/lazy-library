unit VCL.Lazy.Utils;

interface

uses Windows, Messages, SysUtils, Classes, Lazy.Utils.Base, Lazy.Types,
  VCL.Lazy.Types;

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
    class function GetProgramFilesFolder(APlatform
      : TLZOSArchitecture): string;
    class function CopyFileToClipboard(AFileName: TFileName): Boolean;
    class function CopyFilesToClipboard(AFiles: TStrings): Boolean;
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
    class procedure RestartApplication;
    class function ExpandEnvironmentVariable(const AString: String): String;
    class procedure GetEnvironmentVariables(AVariables: TStrings);
    class function GetEnvironmentVariableValue(AVariable: string): string;
  end;

  TLZString = class(TLZStringBase)
  public
    class function AlphaFriendlyStringCompare(S1: string; S2: string): integer;
  end;

  TLZDateTime = class(TLZDateTimeBase);
  TLZBoolean = class(TLZBooleanBase);
  TLZMath = class(TLZMathBase);

implementation

uses
  WinAPi.ShLwApi, WinAPi.psAPI, WinAPi.KnownFolders, WinAPi.ShlObj, VCL.Forms,
  System.Win.Registry, VCL.Clipbrd, WinAPi.ShellApi;

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
  Result := False;
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;
  if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken) then
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
            Result := True;
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

{ TLZFile }

class function TLZFile.CopyFileToClipboard(AFileName: TFileName): Boolean;
var
  DropFiles: PDropFiles;
  hGlobal: THandle;
  iLen: integer;
begin
  Result := False;
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

          DropFiles^.fWide := True;

          if AFileName <> '' then
            Move(AFileName[1], (PByte(DropFiles) + SizeOf(TDropFiles))^,
              iLen * SizeOf(char));
        finally
          GlobalUnlock(hGlobal);
        end;
        Clipboard.SetAsHandle(CF_HDROP, hGlobal);
        Result := True;
      end;
    except
      GlobalFree(hGlobal);
    end;
  end;
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
  CheckDirectoryExists(Result, True);
end;

class function TLZFile.GetDesktopFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Desktop);
end;

class function TLZFile.GetDocumentFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_Documents);
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

class function TLZFile.GetPublicDocumentFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_PublicDesktop);
end;

class function TLZFile.GetUserAppDataFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_RoamingAppData);
  Result := IncludeTrailingPathDelimiter(Result + Application.Title);
  CheckDirectoryExists(Result, True);
end;

class function TLZFile.GetUserLocalAppDataFolder: string;
begin
  Result := GetKnownFolderPath(FOLDERID_LocalAppData);
  Result := IncludeTrailingPathDelimiter(Result + Application.Title);
  CheckDirectoryExists(Result, True);
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

class function TLZSystem.IsCapsLockOn: Boolean;
begin
  Result := 0 <> (GetKeyState(VK_CAPITAL) and $01);
end;

class function TLZSystem.IsCitrixSession: Boolean;
begin
  try
    Result := (Copy(GetEnvironmentVariableValue('SESSIONNAME'), 1, 4) = 'ICA-');
  except
    Result := False;
  end;
end;

class function TLZSystem.IsDelphiRunning: Boolean;
begin
{$WARN SYMBOL_PLATFORM OFF}
  Result := DebugHook <> 0;
{$WARN SYMBOL_PLATFORM ON}
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
      Result := False;
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
  Result := False;
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
    Result := False;
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
  Result := True;
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
    Result := False;
{$ENDIF}
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
  AIsElevated := False;
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
          AIsElevated := False
        else
          AIsElevated := True;
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

class function TLZSystem.GetEnvironmentVariableValue
  (AVariable: string): string;
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
  LVariable := False;
  while True do
  begin
    if LStr^ = #0 then
    begin
      if LVariable then
        AVariables.Add(LRes);
      LVariable := True;
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

end.
