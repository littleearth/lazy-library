{ -----------------------------------------------------------------------------
  Name: VCL.LazyWTSSessionChange

  Allows the monitoring of WM_WTSSESSION_CHANGE Windows Messages and stops
  excessive calling of WM_SETTINGCHANGE when a rdp session is
  locked/disconnected.

  ----------------------------------------------------------------------------- }

unit VCL.Lazy.WTSSessionChange;

interface

uses Winapi.Windows, System.SysUtils, Lazy.Types, Lazy.Log, VCL.Lazy.ThreadTimer, Winapi.Messages;

type
  TOnWTSSessionChange = procedure(ASender: TObject; AParam: Cardinal) of object;

  TLazyWTSSessionChange = class(TLazyObject)
  private
    FMethodWnd: HWND;
    FThreadTimer: TLazyThreadTimer;
    FDelayUpdateMetricSettings: boolean;
    FDelayUpdateMetricSettingsDuration: integer;
    FOnWTSSessionChange: TOnWTSSessionChange;
    function IsOSSupported: boolean;
    function LoadLibrary: boolean;
    function UnloadLibrary: boolean;
    function IsLibraryLoaded: boolean;
    procedure SetOnWTSSessionChange(const Value: TOnWTSSessionChange);
    procedure WTSSessionWndProc(var Message: TMessage);
    procedure EnableMetricSettingsTimer;
    procedure DisableMetricSettingsTimer;
    procedure MetricSettingsOnTimer(Sender: TObject);
  public
    constructor Create(ADelayUpdateMetricSettings: boolean = true;
      ADelayUpdateMetricSettingsDuration: integer = 30000);
    destructor Destroy; override;
    procedure StopUpdateMetricSettings;
    procedure StartUpdateMetricSettings(ADelay: integer = -1);
    property OnWTSSessionChange: TOnWTSSessionChange read FOnWTSSessionChange
      write SetOnWTSSessionChange;
  end;

implementation

uses
  VCL.Forms, System.Classes;

{ TLazyWTSSessionChange }

constructor TLazyWTSSessionChange.Create(ADelayUpdateMetricSettings: boolean;
  ADelayUpdateMetricSettingsDuration: integer);
begin
  FMethodWnd := 0;
  FThreadTimer := nil;
  FDelayUpdateMetricSettings := ADelayUpdateMetricSettings;
  FDelayUpdateMetricSettingsDuration := ADelayUpdateMetricSettingsDuration;
  LoadLibrary;
end;

destructor TLazyWTSSessionChange.Destroy;
begin
  try
    DisableMetricSettingsTimer;
    UnloadLibrary;
  finally
    inherited;
  end;
end;

procedure TLazyWTSSessionChange.DisableMetricSettingsTimer;
begin
  if Assigned(FThreadTimer) then
  begin
    FThreadTimer.Terminate;
    FThreadTimer := nil;
  end;
end;

procedure TLazyWTSSessionChange.MetricSettingsOnTimer(Sender: TObject);
begin
  if IsLibraryLoaded then
  begin
    Debug('WTSSessionWndProc', 'Enabling Application.UpdateMetricSettings');
    DisableMetricSettingsTimer;
    Application.UpdateMetricSettings := true;
  end;
end;

procedure TLazyWTSSessionChange.EnableMetricSettingsTimer;
begin
  FThreadTimer := TLazyThreadTimer.Create(MetricSettingsOnTimer,
    FDelayUpdateMetricSettingsDuration);
end;

procedure TLazyWTSSessionChange.WTSSessionWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_WTSSESSION_CHANGE then
  begin
    if Assigned(FOnWTSSessionChange) then
      FOnWTSSessionChange(Self, Message.WParam);
    if FDelayUpdateMetricSettings then
    begin
      case Message.WParam of
        WTS_SESSION_LOCK:
          begin
            Debug('WTSSessionWndProc', 'WTS_SESSION_LOCK');
            StopUpdateMetricSettings;
          end;
        WTS_REMOTE_DISCONNECT:
          begin
            Debug('WTSSessionWndProc', 'WTS_REMOTE_DISCONNECT');
            StopUpdateMetricSettings;
          end;
        WTS_REMOTE_CONNECT:
          begin
            Debug('WTSSessionWndProc', 'WTS_REMOTE_CONNECT');
            StartUpdateMetricSettings;
          end;
        WTS_SESSION_UNLOCK:
          begin
            Debug('WTSSessionWndProc', 'WTS_SESSION_UNLOCK');
            StartUpdateMetricSettings;
          end;
      end;
    end;
  end;
  Message.Result := DefWindowProc(FMethodWnd, Message.Msg, Message.WParam,
    Message.LParam);
end;

function TLazyWTSSessionChange.IsLibraryLoaded: boolean;
begin
  Result := FMethodWnd <> 0;
end;

function TLazyWTSSessionChange.IsOSSupported: boolean;
begin
  Result := ((Win32MajorVersion = 6) and (Win32MinorVersion >= 2)) or
    (Win32MajorVersion > 6);
end;

function TLazyWTSSessionChange.LoadLibrary: boolean;
begin
  if IsOSSupported and (not IsLibraryLoaded) then
  begin
    FMethodWnd := AllocateHWnd(WTSSessionWndProc);
    WTSRegisterSessionNotification(FMethodWnd, NOTIFY_FOR_THIS_SESSION);
  end;
  Result := IsLibraryLoaded;
end;

procedure TLazyWTSSessionChange.SetOnWTSSessionChange
  (const Value: TOnWTSSessionChange);
begin
  FOnWTSSessionChange := Value;
end;

procedure TLazyWTSSessionChange.StartUpdateMetricSettings(ADelay: integer);
begin
  DisableMetricSettingsTimer;
  if ADelay <> -1 then
    FDelayUpdateMetricSettingsDuration := ADelay;
  Debug('WTSSessionWndProc', Format('Starting metric settings timer, delay %d',
    [FDelayUpdateMetricSettingsDuration]));
  EnableMetricSettingsTimer;
end;

procedure TLazyWTSSessionChange.StopUpdateMetricSettings;
begin
  Debug('WTSSessionWndProc', 'Disabling Application.UpdateMetricSettings');
  DisableMetricSettingsTimer;
  Application.UpdateMetricSettings := False;
end;

function TLazyWTSSessionChange.UnloadLibrary: boolean;
begin
  if FMethodWnd <> 0 then
  begin
    WTSUnRegisterSessionNotification(FMethodWnd);
    DeallocateHWnd(FMethodWnd);
  end;
  Result := IsLibraryLoaded;
end;

end.
