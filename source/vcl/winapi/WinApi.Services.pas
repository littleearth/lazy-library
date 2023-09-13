unit WinApi.Services;

interface

uses
  Lazy.Types, Lazy.Utils,
  SysUtils, Windows, Messages, WinSvc, SvcMgr, Classes;

type
  TNTServiceStatus = (svStopped, svRunning, svPaused, svStartPending,
    svStopPending, svContinuePending, svPausePending, svUnknown);

type
  TNTServiceFailureAction = (sfaNone = integer(SC_ACTION_NONE),
    sfaRestart = integer(SC_ACTION_RESTART),
    sfaReboot = integer(SC_ACTION_REBOOT),
    sfaRunCommand = integer(SC_ACTION_RUN_COMMAND));

type
  TLazyNTServiceControl = class(TLazyComponent)
  private
    FMachineName: string;
    FServiceName: string;
    FNTServiceStatus: TNTServiceStatus;
    FStopDependencies: boolean;
    FStartDependencies: boolean;
  protected
    procedure UpdateServiceStatus;
    function GetServiceStatusString: string;
    function GetServiceStatus: TNTServiceStatus;
    procedure SetServiceName(AValue: string);
    procedure SetMachineName(AValue: string);
    function GetServiceDependencies(AServer: String; AServiceName: string;
      ADependencies: TStrings): boolean;
    function StopService(AServer: string; AServiceName: string): boolean;
    function StartService(AServer: string; AServiceName: string): boolean;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    function Start: boolean;
    function Stop: boolean;
    function Resume: boolean;
    function Pause: boolean;
    function IsInstalled: boolean;
    function IsRunning: boolean;
    function Install(ADisplayName: string; AFileName: TFileName;
      AServiceStartName: string = ''; AServicePassword: string = '';
      AInteractiveService: boolean = False): boolean;
    function Uninstall: boolean;
    function GetServiceList(AServiceList: TStrings): boolean;
    function GetDependencies(AList: TStrings): boolean;
    function SetServiceFailureActions
      (Action1: TNTServiceFailureAction = sfaNone; Action1Delay: Cardinal = 0;
      Action2: TNTServiceFailureAction = sfaNone; Action2Delay: Cardinal = 0;
      Action3: TNTServiceFailureAction = sfaNone; Action3Delay: Cardinal = 0;
      AResetPeriod: Cardinal = 0; ACommand: string = ''): boolean;
  published
    property ServiceName: string Read FServiceName Write SetServiceName;
    property MachineName: string Read FMachineName Write SetMachineName;
    property NTServiceStatus: TNTServiceStatus Read GetServiceStatus;
    property Status: string Read GetServiceStatusString;
    property StopDependencies: boolean read FStopDependencies
      write FStopDependencies;
    property StartDependencies: boolean read FStartDependencies
      write FStartDependencies;
  end;

implementation

uses
  Lazy.Log;

constructor TLazyNTServiceControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNTServiceStatus := svUnknown;
  FStopDependencies := False;
  FStartDependencies := False;
end;

procedure TLazyNTServiceControl.UpdateServiceStatus;
var
  schm: SC_Handle; // service control manager handle
  schs: SC_Handle; // service handle
  ss: TServiceStatus; // service status
  dwStat: DWord; // current service status
begin
  dwStat := 0;
  // connect to the service control manager
  schm := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_CONNECT);
  // if successful...
  if (schm > 0) then
  begin
    // open a handle to the specified service
    // we want to query service status
    schs := OpenService(schm, PChar(FServiceName), SERVICE_QUERY_STATUS);
    // if successful...
    if (schs > 0) then
    begin
      // retrieve the current status
      // of the specified service
      if (QueryServiceStatus(schs, ss)) then
      begin
        dwStat := ss.dwCurrentState;
      end;
      // close service handle
      CloseServiceHandle(schs);
    end;
    // close service control manager handle
    CloseServiceHandle(schm);
  end;
  case dwStat of
    SERVICE_STOPPED:
      FNTServiceStatus := svStopped;
    SERVICE_RUNNING:
      FNTServiceStatus := svRunning;
    SERVICE_PAUSED:
      FNTServiceStatus := svPaused;
    SERVICE_START_PENDING:
      FNTServiceStatus := svStartPending;
    SERVICE_STOP_PENDING:
      FNTServiceStatus := svStopPending;
    SERVICE_CONTINUE_PENDING:
      FNTServiceStatus := svContinuePending;
    SERVICE_PAUSE_PENDING:
      FNTServiceStatus := svPausePending;
  else
    FNTServiceStatus := svUnknown;
  end;
end;

procedure TLazyNTServiceControl.SetServiceName(AValue: string);
begin
  FServiceName := AValue;
  UpdateServiceStatus;
end;

procedure TLazyNTServiceControl.SetMachineName(AValue: string);
begin
  FMachineName := AValue;
end;

function TLazyNTServiceControl.GetServiceStatus: TNTServiceStatus;
begin
  UpdateServiceStatus;
  Result := FNTServiceStatus;
end;

function TLazyNTServiceControl.GetServiceStatusString: string;
var
  s: string;
begin
  case GetServiceStatus of
    svStopped:
      s := 'STOPPED';
    svRunning:
      s := 'RUNNING';
    svPaused:
      s := 'PAUSED';
    svStartPending:
      s := 'START/PENDING';
    svStopPending:
      s := 'STOP/PENDING';
    svContinuePending:
      s := 'CONTINUE/PENDING';
    svPausePending:
      s := 'PAUSE/PENDING';
  else
    s := 'UNKNOWN';
  end;
  Result := s;
end;

function TLazyNTServiceControl.Pause: boolean;
var
  schm: SC_Handle; // service control manager handle
  schs: SC_Handle; // service handle
  ss: TServiceStatus; // service status
  dwChkP: DWord; // check point
begin
  // connect to the service control manager
  schm := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  // if successful...
  if (schm > 0) then
  begin
    // open a handle to the specified service
    // we want to query service status
    schs := OpenService(schm, PChar(FServiceName), SERVICE_ALL_ACCESS);
    // if successful...
    if (schs > 0) then
    begin
      if ControlService(schs, SERVICE_CONTROL_PAUSE, ss) then
      begin
        // check status
        if (QueryServiceStatus(schs, ss)) then
        begin
          while (SERVICE_PAUSED <> ss.dwCurrentState) do
          begin
            // dwCheckPoint contains a value that the
            // service increments periodically to
            // report its progress during a
            // lengthy operation. Save current value
            dwChkP := ss.dwCheckPoint;
            // wait a bit before checking status again
            // dwWaitHint is the estimated amount of
            // time the calling program should wait
            // before calling QueryServiceStatus()
            // again. Idle events should be
            // handled here...
            Sleep(ss.dwWaitHint);
            if not QueryServiceStatus(schs, ss) then
            begin
              // couldn't check status break from the
              // loop
              break;
            end;

            if ss.dwCheckPoint < dwChkP then
            begin
              // QueryServiceStatus didn't increment
              // dwCheckPoint as it should have.
              // Avoid an infinite loop by breaking
              break;
            end;
          end;
        end;
      end
      else
        RaiseLastOsError;
      // close service handle
      CloseServiceHandle(schs);
    end;
    // close service control manager handle
    CloseServiceHandle(schm);
  end;
  Result := SERVICE_PAUSED = ss.dwCurrentState;
end;

function TLazyNTServiceControl.Resume: boolean;
var
  schm: SC_Handle; // service control manager handle
  schs: SC_Handle; // service handle
  ss: TServiceStatus; // service status
  dwChkP: DWord; // check point
begin
  // connect to the service control manager
  schm := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  // if successful...
  if (schm > 0) then
  begin
    // open a handle to the specified service
    // we want to query service status
    schs := OpenService(schm, PChar(FServiceName), SERVICE_ALL_ACCESS);
    // if successful...
    if (schs > 0) then
    begin
      if ControlService(schs, SERVICE_CONTROL_CONTINUE, ss) then
      begin
        // check status
        if (QueryServiceStatus(schs, ss)) then
        begin
          while (SERVICE_RUNNING <> ss.dwCurrentState) do
          begin
            // dwCheckPoint contains a value that the
            // service increments periodically to
            // report its progress during a
            // lengthy operation. Save current value
            dwChkP := ss.dwCheckPoint;
            // wait a bit before checking status again
            // dwWaitHint is the estimated amount of
            // time the calling program should wait
            // before calling QueryServiceStatus()
            // again. Idle events should be
            // handled here...
            Sleep(ss.dwWaitHint);
            if not QueryServiceStatus(schs, ss) then
            begin
              // couldn't check status break from the
              // loop
              break;
            end;

            if ss.dwCheckPoint < dwChkP then
            begin
              // QueryServiceStatus didn't increment
              // dwCheckPoint as it should have.
              // Avoid an infinite loop by breaking
              break;
            end;
          end;
        end;
      end
      else
        RaiseLastOsError;
      // close service handle
      CloseServiceHandle(schs);
    end;
    // close service control manager handle
    CloseServiceHandle(schm);
  end;
  Result := SERVICE_RUNNING = ss.dwCurrentState;
end;

function TLazyNTServiceControl.Start: boolean;
var
  Dependencies: TStringList;
  Idx: integer;
begin
  Dependencies := TStringList.Create;
  try
    Result := StartService(FMachineName, FServiceName);
    if Result and FStartDependencies then
    begin
      Log(Format('Starting %s', [FServiceName]));
      GetServiceDependencies(FMachineName, FServiceName, Dependencies);
      for Idx := 0 to Pred(Dependencies.Count) do
      begin
        Log(Format('Starting Dependency %s (%s)', [Dependencies.Names[Idx],
          Dependencies.ValueFromIndex[Idx]]));
        try
          StartService(FMachineName, Dependencies.Names[Idx]);
        except
          on E: Exception do
          begin
            Error(E);
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Dependencies);
  end;
end;

function TLazyNTServiceControl.GetServiceDependencies(AServer: String;
  AServiceName: string; ADependencies: TStrings): boolean;
var
  schm, schs: SC_Handle;
  Services, s: PEnumServiceStatus;
  BytesNeeded, ServicesReturned: DWord;
  i: integer;
begin
  Result := False;
  Services := nil;
  ADependencies.Clear;
  schm := OpenSCManager(PChar(AServer), nil, SC_MANAGER_CONNECT);
  // if successful...
  if schm > 0 then
  begin
    // open a handle to the specified service
    // we want to stop the service and
    // query service status
    schs := OpenService(schm, PChar(AServiceName),
      SERVICE_ENUMERATE_DEPENDENTS);
    if schs > 0 then
    begin
      EnumDependentServices(schs, SERVICE_ACTIVE + SERVICE_INACTIVE, Services,
        0, BytesNeeded, ServicesReturned);

      GetMem(Services, BytesNeeded);
      try
        if EnumDependentServices(schs, SERVICE_ACTIVE + SERVICE_INACTIVE,
          Services, BytesNeeded, BytesNeeded, ServicesReturned) then
        begin
          // Now process it...
          s := Services;
          for i := 0 to Pred(ServicesReturned) do
          begin
            Debug('GetServiceDependencies', s^.lpServiceName + '( ' +
              s^.lpDisplayName + ')');
            ADependencies.Add(s^.lpServiceName + '=' + s^.lpDisplayName);
            Inc(s);
          end;
          Result := True;
        end;
      finally
        FreeMem(Services);
      end;

    end;
    CloseServiceHandle(schs);
  end;
  CloseServiceHandle(schm);
end;

function TLazyNTServiceControl.StopService(AServer: string;
  AServiceName: string): boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  dwChkP: DWord;
begin
  // connect to the service control manager
  schm := OpenSCManager(PChar(AServer), nil, SC_MANAGER_CONNECT);
  // if successful...
  if schm > 0 then
  begin
    // open a handle to the specified service
    // we want to stop the service and
    // query service status
    schs := OpenService(schm, PChar(AServiceName), SERVICE_STOP or
      SERVICE_QUERY_STATUS);
    // if successful...
    if schs > 0 then
    begin
      if ControlService(schs, SERVICE_CONTROL_STOP, ss) then
      begin
        // check status
        if (QueryServiceStatus(schs, ss)) then
        begin
          while (SERVICE_STOPPED <> ss.dwCurrentState) do
          begin
            // dwCheckPoint contains a value that the
            // service increments periodically to
            // report its progress during a lengthy
            // operation. Save current value
            dwChkP := ss.dwCheckPoint;
            // Wait a bit before checking status again.
            // dwWaitHint is the estimated amount of
            // time the calling program should wait
            // before calling QueryServiceStatus()
            // again. Idle events should be
            // handled here...
            Sleep(ss.dwWaitHint);

            if (not QueryServiceStatus(schs, ss)) then
            begin
              // couldn't check status
              // break from the loop
              break;
            end;

            if (ss.dwCheckPoint < dwChkP) then
            begin
              // QueryServiceStatus didn't increment
              // dwCheckPoint as it should have.
              // Avoid an infinite loop by breaking
              break;
            end;
          end;
        end;
      end;

      // close service handle
      CloseServiceHandle(schs);
    end;

    // close service control manager handle
    CloseServiceHandle(schm);
  end;

  // return TRUE if the service status is stopped
  Result := SERVICE_STOPPED = ss.dwCurrentState;
end;

function TLazyNTServiceControl.StartService(AServer: string;
  AServiceName: string): boolean;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
  psTemp: PChar;
  dwChkP: DWord; // check point
begin
  // connect to the service control manager
  schm := OpenSCManager(PChar(AServer), nil, SC_MANAGER_CONNECT);
  // if successful...
  if (schm > 0) then
  begin
    // open a handle to the specified service
    // we want to start the service and query service
    // status
    schs := OpenService(schm, PChar(AServiceName), SERVICE_START or
      SERVICE_QUERY_STATUS);
    // if successful...
    if (schs > 0) then
    begin
      psTemp := nil;
      if (WinSvc.StartService(schs, 0, psTemp)) then
      begin
        // check status
        if (QueryServiceStatus(schs, ss)) then
        begin
          while (SERVICE_RUNNING <> ss.dwCurrentState) do
          begin
            // dwCheckPoint contains a value that the
            // service increments periodically to
            // report its progress during a
            // lengthy operation. Save current value
            dwChkP := ss.dwCheckPoint;
            // wait a bit before checking status again
            // dwWaitHint is the estimated amount of
            // time the calling program should wait
            // before calling QueryServiceStatus()
            // again. Idle events should be
            // handled here...
            Sleep(ss.dwWaitHint);
            if not QueryServiceStatus(schs, ss) then
            begin
              // couldn't check status break from the
              // loop
              break;
            end;

            if ss.dwCheckPoint < dwChkP then
            begin
              // QueryServiceStatus didn't increment
              // dwCheckPoint as it should have.
              // Avoid an infinite loop by breaking
              break;
            end;
          end;
        end;
      end;
      // close service handle
      CloseServiceHandle(schs);
    end;
    // close service control manager handle
    CloseServiceHandle(schm);
  end;
  // Return TRUE if the service status is running
  Result := GetServiceStatus = svRunning;
end;

function TLazyNTServiceControl.SetServiceFailureActions
  (Action1: TNTServiceFailureAction = sfaNone; Action1Delay: Cardinal = 0;
  Action2: TNTServiceFailureAction = sfaNone; Action2Delay: Cardinal = 0;
  Action3: TNTServiceFailureAction = sfaNone; Action3Delay: Cardinal = 0;
  AResetPeriod: Cardinal = 0; ACommand: string = ''): boolean;
var
  schm, schs: SC_Handle;
  sfa: SERVICE_FAILURE_ACTIONS;
  actions: array [0 .. 2] of SC_ACTION;

  function GetActionTypes(ANTServiceFailureAction: TNTServiceFailureAction)
    : SC_ACTION_TYPE;
  begin
    case ANTServiceFailureAction of
      sfaRestart:
        Result := SC_ACTION_RESTART;
      sfaReboot:
        Result := SC_ACTION_REBOOT;
      sfaRunCommand:
        Result := SC_ACTION_RUN_COMMAND
    else
      Result := SC_ACTION_NONE;
    end;
  end;

begin
  Result := False;
  schm := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  if (schm > 0) then
  begin
    schs := OpenService(schm, PChar(FServiceName), SERVICE_ALL_ACCESS);
    if (schs > 0) then
    begin
      try
        try
          sfa.dwResetPeriod := AResetPeriod;
          sfa.lpCommand := PChar(ACommand);
          sfa.lpRebootMsg := Nil;
          sfa.cActions := 3;
          actions[0].&Type := GetActionTypes(Action1);
          actions[0].Delay := Action1Delay;
          actions[1].&Type := GetActionTypes(Action2);
          actions[1].Delay := Action2Delay;
          actions[2].&Type := GetActionTypes(Action3);
          actions[2].Delay := Action3Delay;
          sfa.lpsaActions := @actions;

          Result := ChangeServiceConfig2(schs,
            SERVICE_CONFIG_FAILURE_ACTIONS, @sfa);
        except
          on E: Exception do
          begin
            Error(E);
          end;
        end;
      finally
        CloseServiceHandle(schs);
      end;

    end;
    CloseServiceHandle(schm);
  end;
end;

// Return TRUE if successful
function TLazyNTServiceControl.Stop: boolean;
var
  Dependencies: TStringList;
  Idx: integer;
begin
  Dependencies := TStringList.Create;
  try
    if FStopDependencies then
    begin
      GetServiceDependencies(FMachineName, FServiceName, Dependencies);
      for Idx := 0 to Pred(Dependencies.Count) do
      begin
        Log(Format('Stopping Dependency %s (%s)', [Dependencies.Names[Idx],
          Dependencies.ValueFromIndex[Idx]]));
        try
          StopService(FMachineName, Dependencies.Names[Idx]);
        except
          on E: Exception do
          begin
            Error(E);
          end;
        end;
      end;
    end;
    Log(Format('Stopping %s', [FServiceName]));
    Result := StopService(FMachineName, FServiceName);
  finally
    FreeAndNil(Dependencies);
  end;
end;

function TLazyNTServiceControl.GetServiceList(AServiceList: TStrings): boolean;
var
  hSCM: THandle;
  pEnumStatus: LPENUM_SERVICE_STATUSW;
  dwByteNeeded: DWord;
  dwNumOfService: DWord;
  dwResumeHandle: DWord;
  nI: integer;
begin
  pEnumStatus := nil;
  dwByteNeeded := 0;
  dwResumeHandle := 0;
  try
    hSCM := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
    if not EnumServicesStatus(hSCM, SERVICE_WIN32 or SERVICE_DRIVER,
      SERVICE_ACTIVE or SERVICE_INACTIVE, pEnumStatus, 0, dwByteNeeded,
      dwNumOfService, dwResumeHandle) then
    begin
      if GetLastError = ERROR_MORE_DATA then
      begin
        GetMem(pEnumStatus, dwByteNeeded);
        if not EnumServicesStatus(hSCM, SERVICE_WIN32 or SERVICE_DRIVER,
          SERVICE_ACTIVE or SERVICE_INACTIVE, pEnumStatus, dwByteNeeded,
          dwByteNeeded, dwNumOfService, dwResumeHandle) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
    end;
    AServiceList.Clear;
    for nI := 0 to dwNumOfService - 1 do
    begin
      AServiceList.Add(TEnumServiceStatus(PEnumServiceStatus(PChar(pEnumStatus)
        + nI * SizeOf(TEnumServiceStatus))^).lpServiceName);
    end;
    CloseServiceHandle(hSCM);
    Result := True;
  finally
    if Assigned(pEnumStatus) then
      FreeMem(pEnumStatus);
  end;
end;

function TLazyNTServiceControl.GetDependencies(AList: TStrings): boolean;
begin
  Result := GetServiceDependencies(FMachineName, FServiceName, AList);
end;

function TLazyNTServiceControl.IsInstalled: boolean;
var
  hSCM: THandle;
  schs: THandle;
begin
  hSCM := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_CONNECT);
  Result := False;
  if hSCM <> 0 then
  begin
    schs := OpenService(hSCM, PChar(FServiceName), SERVICE_QUERY_CONFIG);
    if schs <> 0 then
    begin
      Result := True;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(hSCM);
  end;
end;

function TLazyNTServiceControl.IsRunning: boolean;
begin
  case GetServiceStatus of
    svStopped:
      Result := False;
    svRunning:
      Result := True;
    svPaused:
      Result := True;
    svStartPending:
      Result := True;
    svStopPending:
      Result := True;
    svContinuePending:
      Result := True;
    svPausePending:
      Result := True;
  else
    Result := False;
  end;
end;

function TLazyNTServiceControl.Install(ADisplayName: string;
  AFileName: TFileName; AServiceStartName: string = '';
  AServicePassword: string = ''; AInteractiveService: boolean = False): boolean;
var
  schm: SC_Handle;
  schs: SC_Handle;
  ServiceType: Cardinal;
  ServiceStartName: PChar;
  ServicePassword: PChar;
begin
  Result := False;
  schm := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  if (schm > 0) then
  begin
    try
      ServiceStartName := nil;
      ServicePassword := nil;
      if AInteractiveService then
      begin
        ServiceType := SERVICE_WIN32_OWN_PROCESS + SERVICE_INTERACTIVE_PROCESS;
      end
      else
      begin
        ServiceType := SERVICE_WIN32_OWN_PROCESS;
        if Trim(AServiceStartName) <> '' then
        begin
          ServiceStartName := PChar(AServiceStartName);
          ServicePassword := PChar(AServicePassword);
        end
        else
        begin

        end;
      end;
      schs := CreateService(schm, PChar(FServiceName), PChar(ADisplayName),
        SERVICE_ALL_ACCESS, ServiceType, SERVICE_AUTO_START,
        SERVICE_ERROR_NORMAL, PChar(AFileName), '', nil, '', ServiceStartName,
        ServicePassword);
      if (schs = 0) then
      begin
        RaiseLastOsError;
      end
      else
      begin
        Result := True;
      end;
      if schs <> 0 then
        CloseServiceHandle(schs);
    finally
      CloseServiceHandle(schm);
    end;
  end;
end;

function TLazyNTServiceControl.Uninstall: boolean;
var
  SCMHandle: longword;
  ServiceHandle: longword;
begin
  Result := False;
  SCMHandle := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  if SCMHandle <> 0 then
  begin
    try
      ServiceHandle := OpenService(SCMHandle, PChar(FServiceName),
        SERVICE_ALL_ACCESS);
      if ServiceHandle <> 0 then
      begin
        try
          if DeleteService(ServiceHandle) then
          begin
            Result := True;
          end
          else
          begin
            RaiseLastOsError;
          end;
        finally
          if ServiceHandle <> 0 then
            CloseServiceHandle(ServiceHandle);
        end;
      end;
    finally
      if SCMHandle <> 0 then
        CloseServiceHandle(SCMHandle);
    end;
  end;

end;

end.
