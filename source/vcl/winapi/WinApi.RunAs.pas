{ -----------------------------------------------------------------------------
  Unit Name: WinApi.RunAsU
  Author: Tristan Marlow
  Purpose: Little Earth Solutions Run application as current or another user.

  ----------------------------------------------------------------------------
  Copyright (c) 2016 Tristan David Marlow
  Copyright (c) 2016 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History: 26/10/2009 - First Release.
  29/09/2010 - If a launch with login details fails TNukeRunAs will
  attempt to find alternatives.
  14/06/2012 - If UAC is enabled, domain credentials will be ignored
  due to elevation issues.

  ----------------------------------------------------------------------------- }
unit WinApi.RunAs;

interface

uses
  Lazy.Types,
  Lazy.Utils.Windows,
  SysUtils,
  Classes,
  Types,
  Windows,
  Forms,
  ShellApi;

type
  ENukeRunAsException = Exception;

  // Create an error event if the command fail
  TOnErrorEvent = procedure(Sender: TObject; ErrorMessage: string) of object;

  // To add a BeforeExecute and AfterExecute events
  TExecuteEvent = procedure(Sender: TObject) of object;

  TNukeRunAs = class(TLZComponent)
  private
    FUserName: string;
    FPassword: string;
    FDomain: string;
    FCommand: string;
    FParameters: string;
    FOnError: TOnErrorEvent;
    FBeforeExecute: TExecuteEvent;
    FAfterExecute: TExecuteEvent;
  protected
    procedure SetUserName(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetDomain(AValue: string);
    procedure SetCommand(AValue: string);
    procedure SetParameters(AValue: string);
    function LaunchProcess(ACommand: string; AStartupInfo: TStartupInfo;
      var AProcessInfo: TProcessInformation; var AErrorMessage: string)
      : boolean;
    function LaunchProcessWithLogon(AUsername: PWideChar; ADomain: PWideChar;
      APassword: PWideChar; ACommand: PWideChar; AStartupInfo: TStartupInfo;
      var AProcessInfo: TProcessInformation; var AErrorMessage: string)
      : boolean;
    { function LaunchProcessWithLogonAndToken(AUsername: PWideChar;
      ADomain: PWideChar; APassword: PWideChar; ACommand: PWideChar;
      AStartupInfo: TStartupInfo; var AProcessInfo: TProcessInformation;
      var AErrorMessage: string): boolean; }
    function RunAsAdmin(AHandle: HWND; ACommand: string; AParameters: string;
      AWait: boolean; var AErrorMessage: string): boolean;
  public
    function Execute: boolean;
  published
    property OnError: TOnErrorEvent Read FOnError Write FOnError;
    property BeforeExecute: TExecuteEvent Read FBeforeExecute
      Write FBeforeExecute;
    property AfterExecute: TExecuteEvent Read FAfterExecute Write FAfterExecute;
    property UserName: string Read FUserName Write SetUserName;
    property Password: string Read FPassword Write SetPassword;
    property Domain: string Read FDomain Write SetDomain;
    property Command: string Read FCommand Write SetCommand;
    property Parameters: string Read FParameters Write SetParameters;
  end;

  // API function reference
function CreateProcessWithLogonW(lpUsername, lpDomain, lpPassword: PWideChar;
  dwLogonFlags: dword; lpApplicationName: PWideChar; lpCommandLine: PWideChar;
  dwCreationFlags: dword; lpEnvironment: Pointer; lpCurrentDirectory: PWideChar;
  const lpStartupInfo: TStartupInfo;
  var lpProcessInformation: TProcessInformation): BOOL; stdcall;
  external 'advapi32.dll';

implementation

uses
  Lazy.Log;

{ TNukeRunAs }

procedure TNukeRunAs.SetUserName(AValue: string);
begin
  FUserName := Trim(AValue);
end;

procedure TNukeRunAs.SetPassword(AValue: string);
begin
  FPassword := Trim(AValue);
end;

procedure TNukeRunAs.SetDomain(AValue: string);
begin
  FDomain := Trim(AValue);
end;

procedure TNukeRunAs.SetCommand(AValue: string);
begin
  FCommand := Trim(AValue);
end;

procedure TNukeRunAs.SetParameters(AValue: string);
begin
  FParameters := AValue;
end;

function TNukeRunAs.LaunchProcess(ACommand: string; AStartupInfo: TStartupInfo;
  var AProcessInfo: TProcessInformation; var AErrorMessage: string): boolean;
begin
  Result := False;
  AErrorMessage := '';
  if CreateProcess(nil, PChar(ACommand), nil, nil, False,
    CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS, nil, nil, AStartupInfo,
    AProcessInfo) then
  begin
    Result := True;
  end
  else
  begin
    AErrorMessage := TLZString.WinErrorString(GetLastError);
  end;
  Debug('LaunchProcess', 'Result: ' + BoolToStr(Result, True) + ', Error: ' +
    AErrorMessage);
end;

{ function TNukeRunAs.LaunchProcessWithLogonAndToken(AUsername: PWideChar;
  ADomain: PWideChar; APassword: PWideChar; ACommand: PWideChar;
  AStartupInfo: TStartupInfo; var AProcessInfo: TProcessInformation;
  var AErrorMessage: string): boolean;
  var
  CurrentToken, NewToken: cardinal;
  //SID: PSid;
  begin
  //New(SID);
  Result := False;
  AErrorMessage := '';
  try
  if LogonUser(PChar(AUserName), PChar(ADomain), PChar(APassword),
  LOGON32_LOGON_NETWORK, LOGON32_PROVIDER_DEFAULT, CurrentToken) then
  begin
  if Windows.DuplicateTokenEx(CurrentToken, MAXIMUM_ALLOWED, nil,
  Windows.SecurityImpersonation, TokenPrimary, NewToken) then
  begin
  if CreateProcessWithTokenW(NewToken, 0, nil, ACommand, 0, nil,
  nil, @AstartupInfo, @AProcessInfo) then
  begin
  Result := True;
  end
  else
  begin
  AErrorMessage := 'CreateProcessWithTokenW: ' + TLZString.WinErrorString(GetLastError);
  end;
  end
  else
  begin
  AErrorMessage := 'DuplicateTokenEx: ' + TLZString.WinErrorString(GetLastError);
  end;
  end
  else
  begin
  AErrorMessage := 'LogonUser: ' + TLZString.WinErrorString(GetLastError);
  end;
  finally
  //Dispose(SID);
  if CurrentToken <> 0 then
  CloseHandle(CurrentToken);
  if NewToken <> 0 then
  CloseHandle(NewToken);
  end;
  Debug('LaunchProcessWithLogonAndToken', 'Result: ' + BoolToStr(Result, True) +
  ', Error: ' + AErrorMessage);
  end; }

function TNukeRunAs.RunAsAdmin(AHandle: HWND; ACommand: string;
  AParameters: string; AWait: boolean; var AErrorMessage: string): boolean;
{
  See Step 3: Redesign for UAC Compatibility (UAC)
  http://msdn.microsoft.com/en-us/library/bb756922.aspx
}
var
  sei: TShellExecuteInfo;
  ExitCode: dword;
begin
  Result := False;
  AErrorMessage := '';
  ZeroMemory(@sei, SizeOf(sei));
  sei.cbSize := SizeOf(TShellExecuteInfo);
  sei.Wnd := AHandle;
  sei.fMask := SEE_MASK_NOCLOSEPROCESS;
  // SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := PChar('runas');
  sei.lpFile := PChar(ACommand); // PAnsiChar;
  if Parameters <> '' then
    sei.lpParameters := PChar(AParameters); // PAnsiChar;
  sei.nShow := SW_SHOWNORMAL; // Integer;

  if ShellExecuteEx(@sei) then
  begin
    Result := True;
    if AWait then
    begin
      repeat
        GetExitCodeProcess(sei.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE);
    end;
  end
  else
  begin
    AErrorMessage := TLZString.WinErrorString(GetLastError);
  end;

  Debug('RunAsAdmin', 'Result: ' + BoolToStr(Result, True) + ', Error: ' +
    AErrorMessage);
end;

function TNukeRunAs.LaunchProcessWithLogon(AUsername: PWideChar;
  ADomain: PWideChar; APassword: PWideChar; ACommand: PWideChar;
  AStartupInfo: TStartupInfo; var AProcessInfo: TProcessInformation;
  var AErrorMessage: string): boolean;
begin
  Result := False;
  AErrorMessage := '';
  { if CreateProcessWithLogonW(AUsername, ADomain, APassword, 0, nil,
    ACommand, 0, nil, nil, AStartupInfo, AProcessInfo) then }
  if CreateProcessWithLogonW(AUsername, ADomain, APassword, 0, nil, ACommand, 0,
    nil, nil, AStartupInfo, AProcessInfo) then
  begin
    Result := True;
  end
  else
  begin
    AErrorMessage := TLZString.WinErrorString(GetLastError);
  end;
  Debug('LaunchProcessWithLogon', 'Result: ' + BoolToStr(Result, True) +
    ', Error: ' + AErrorMessage);
end;

function TNukeRunAs.Execute: boolean;
var
  StartupInfo: TStartupInfo;
  CommandAndParameters: string;
  ProcessInfo: TProcessInformation;
  wDomain: PWideChar;
  wUsername: PWideChar;
  wPassword: PWideChar;
  wCommand: PWideChar;
  ErrorMessage: string;
  WaitForExit: boolean;

begin
  ErrorMessage := '';
  Result := False;
  WaitForExit := True;
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self);

  CommandAndParameters := Trim(FCommand + ' ' + FParameters);

  if Trim(FCommand) <> '' then
  begin
    // Setup some flags to execute dos commands in hide mode
    // FillChar(StartupInfo, SizeOf(StartupInfo), #0);
    ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    if Trim(FUserName) <> '' then
    begin
      // Setup some flags to execute commands in hide mode
      StartupInfo.wShowWindow := SW_HIDE;
    end
    else
    begin
      // Show process when no user name is specified.
      StartupInfo.wShowWindow := SW_NORMAL;
    end;

    // Allocate necessary memory for the fourth main properties
    GetMem(wDomain, Length(FDomain) * SizeOf(widechar) + SizeOf(widechar));
    GetMem(wUsername, Length(FUserName) * SizeOf(widechar) + SizeOf(widechar));
    GetMem(wPassword, Length(FPassword) * SizeOf(widechar) + SizeOf(widechar));
    GetMem(wCommand, Length(CommandAndParameters) * SizeOf(widechar) +
      SizeOf(widechar));

    // Convert the fourth main properties to WideString data type
    StringToWideChar(FDomain, wDomain, Length(FDomain) * SizeOf(widechar) +
      SizeOf(widechar));
    StringToWideChar(FUserName, wUsername, Length(FUserName) * SizeOf(widechar)
      + SizeOf(widechar));
    StringToWideChar(FPassword, wPassword, Length(FPassword) * SizeOf(widechar)
      + SizeOf(widechar));
    StringToWideChar(CommandAndParameters, wCommand,
      Length(CommandAndParameters) * SizeOf(widechar) + SizeOf(widechar));

    if TLZSystem.IsUACEnabled then
    begin
      Debug('Execute',
        'UAC is enabled, skipping domain credentials, to avoid "bad stub" error.');
      if (TLZSystem.IsAdministrator) then
      begin
        // Show process when no user name is specified.
        StartupInfo.wShowWindow := SW_NORMAL;
        Debug('Execute',
          'User appears to have administrator rights, attempting to launch process.');
        Result := LaunchProcess(CommandAndParameters, StartupInfo, ProcessInfo,
          ErrorMessage);
      end
      else
      begin
        Debug('Execute',
          'User does not have administrator rights, attempting to launch application via RunAs');

        if RunAsAdmin(0, Command, Parameters, True, ErrorMessage) then
        begin
          WaitForExit := False;
          Result := True;
        end
        else
        begin
          Debug('Execute', 'Failed to launch via RunAs, Error: ' +
            ErrorMessage);
          Debug('Execute',
            'Attempting to launch application, without credentials, last attempt');
          StartupInfo.wShowWindow := SW_NORMAL;
          Result := LaunchProcess(CommandAndParameters, StartupInfo,
            ProcessInfo, ErrorMessage);
        end;

      end;
    end
    else
    begin
      if FUserName <> '' then
      begin
        // Call the command as a different user/password/domain
        Debug('Execute',
          Format('Attempting to launch with credentials, Username: %s, Domain: %s, Password: %s',
          [wUsername, wDomain, wPassword]));
        Result := LaunchProcessWithLogon(wUsername, wDomain, wPassword,
          wCommand, StartupInfo, ProcessInfo, ErrorMessage);

        if not Result then
        begin
          Debug('Execute', 'Failed to launch process with credentials, Error: '
            + ErrorMessage);

          if not TLZSystem.IsMachineInDomain then
          begin
            Debug('Execute', 'Machine does not appear to be on a domain.');
          end
          else
          begin
            Debug('Execute', 'Machine appears to be on a domain.');
          end;

          StringToWideChar('', wDomain, Length(FDomain) * SizeOf(widechar) +
            SizeOf(widechar));

          Debug('Execute', 'Attempting to launch without domain details.');
          Result := LaunchProcessWithLogon(wUsername, wDomain, wPassword,
            wCommand, StartupInfo, ProcessInfo, ErrorMessage);

          if not Result then
          begin
            Debug('Execute', 'Failed to launch without domain details, Error: '
              + ErrorMessage);

            Debug('Execute',
              'Checking if current user has administrator rights');

            if (TLZSystem.IsAdministrator) and (not Result) then
            begin
              // Show process when no user name is specified.
              StartupInfo.wShowWindow := SW_NORMAL;
              Debug('Execute',
                'User appears to have administrator rights, attempting to launch process.');
              Result := LaunchProcess(CommandAndParameters, StartupInfo,
                ProcessInfo, ErrorMessage);
            end
            else
            begin
              Debug('Execute',
                'User does not have administrator rights, attempting to launch application via RunAs');

              if RunAsAdmin(0, Command, Parameters, True, ErrorMessage) then
              begin
                WaitForExit := False;
                Result := True;
              end
              else
              begin
                Debug('Execute', 'Failed to launch via RunAs, Error: ' +
                  ErrorMessage);
                Debug('Execute',
                  'Attempting to launch application, without credentials, last attempt');
                StartupInfo.wShowWindow := SW_NORMAL;
                Result := LaunchProcess(CommandAndParameters, StartupInfo,
                  ProcessInfo, ErrorMessage);
              end;

            end;
          end;
        end;
      end
      else
      begin
        Debug('Execute',
          'Attempting to launch application, no user credential supplied');
        StartupInfo.wShowWindow := SW_NORMAL;
        Result := LaunchProcess(CommandAndParameters, StartupInfo, ProcessInfo,
          ErrorMessage);
      end;
    end;
    if (Result) and (WaitForExit) and (ProcessInfo.hProcess <> 0) then
    begin
      // Wait for the command to end
      Debug('Execute', 'Waiting for process to end, Start Time:' +
        DateTimeToStr(Now));
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      Debug('Execute', 'Process complete, End Time:' + DateTimeToStr(Now));
    end;

    if not Result then
    begin
      if TLZString.IsEmptyString(ErrorMessage) then
        ErrorMessage := 'An unkown error occoured'; // at least show something.
      if Assigned(FOnError) then
        FOnError(Self, ErrorMessage)
      else
        raise ENukeRunAsException.Create(ErrorMessage);
    end;

    // UnAllocate necessary memory
    FreeMem(wDomain);
    FreeMem(wUsername);
    FreeMem(wPassword);
    FreeMem(wCommand);
  end
  else
  begin
    if Assigned(FOnError) then
      FOnError(Self, 'No command specified.')
    else
      raise ENukeRunAsException.Create('No command specified.');
  end;

  if Assigned(FAfterExecute) then
    FAfterExecute(Self);
end;

end.
