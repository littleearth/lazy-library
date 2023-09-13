unit WinApi.WindowsMessages;

interface

uses
  Lazy.Types, Windows, Messages, SysUtils, Classes, Forms;

type
  TOnWindowsMessageEvent = procedure(ASender: TObject; var AMsg: TMessage)
    of object;

type
  TRegisterSuspendResumeNotification = function(hRecipient: HWND;
    ulFlags: Cardinal): HWND; stdcall;
  TUnregisterSuspendResumeNotification = function(hPowernotify: HWND)
    : BOOL; stdcall;

type
  TLZWindowsMessages = class(TLZObject)
  private
    FMsgHandlerHWND: HWND;
    FRegisterSuspendResumeNotificationHandle: HWND;
    FRegisterSuspendResumeNotification: TRegisterSuspendResumeNotification;
    FUnregisterSuspendResumeNotification: TUnregisterSuspendResumeNotification;
    FOnPowerBroadcast: TOnWindowsMessageEvent;
    FOnTimeChange: TOnWindowsMessageEvent;
    FOnEndSession: TOnWindowsMessageEvent;
    FOnSettingsChange: TOnWindowsMessageEvent;
    procedure WMTimeChange(var Msg: TMessage);
    procedure WMEndSession(var Msg: TMessage);
    procedure WMPowerBroadcast(var Msg: TMessage);
    procedure WMSettingsChange(var Msg: TMessage);
    procedure SetOnEndSession(const Value: TOnWindowsMessageEvent);
    procedure SetOnPowerBroadcast(const Value: TOnWindowsMessageEvent);
    procedure SetOnSettingsChange(const Value: TOnWindowsMessageEvent);
    procedure SetOnTimeChange(const Value: TOnWindowsMessageEvent);
    function EnableSuspendResumeNotifications(Value: boolean): boolean;
  protected
    procedure WndMethod(var Msg: TMessage); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property OnPowerBroadcast: TOnWindowsMessageEvent read FOnPowerBroadcast
      write SetOnPowerBroadcast;
    property OnTimeChange: TOnWindowsMessageEvent read FOnTimeChange
      write SetOnTimeChange;
    property OnEndSession: TOnWindowsMessageEvent read FOnEndSession
      write SetOnEndSession;
    property OnSettingsChange: TOnWindowsMessageEvent read FOnSettingsChange
      write SetOnSettingsChange;
  end;

implementation

uses
  Lazy.Log;

{ TPowerMonitor }

constructor TLZWindowsMessages.Create;
var
  LModule: HMODULE;
begin
  FMsgHandlerHWND := AllocateHWnd(WndMethod);
  if TOSVersion.Check(6) then
  begin
    LModule := GetModuleHandle(User32);
    if LModule <> 0 then
    begin
      @FRegisterSuspendResumeNotification :=
        GetProcAddress(LModule, 'RegisterSuspendResumeNotification');
      @FUnregisterSuspendResumeNotification :=
        GetProcAddress(LModule, 'UnregisterSuspendResumeNotification');
    end;
    EnableSuspendResumeNotifications(True);
  end;
end;

destructor TLZWindowsMessages.Destroy;
begin
  try
    DeallocateHWnd(FMsgHandlerHWND);
    if TOSVersion.Check(6, 2) then
    begin
      EnableSuspendResumeNotifications(False);
      if assigned(FUnregisterSuspendResumeNotification) and
        (FRegisterSuspendResumeNotificationHandle <> 0) then
        FUnregisterSuspendResumeNotification
          (FRegisterSuspendResumeNotificationHandle);
    end;
  finally
    inherited;
  end;
end;

function TLZWindowsMessages.EnableSuspendResumeNotifications
  (Value: boolean): boolean;
begin
  if Value and (FRegisterSuspendResumeNotificationHandle = 0) then
  begin
    if assigned(FRegisterSuspendResumeNotification) then
      FRegisterSuspendResumeNotificationHandle :=
        FRegisterSuspendResumeNotification(Application.Handle,
        DEVICE_NOTIFY_WINDOW_HANDLE);

    Result := FRegisterSuspendResumeNotificationHandle <> 0;
  end
  else
  begin
    if assigned(FUnregisterSuspendResumeNotification) and
      (FRegisterSuspendResumeNotificationHandle <> 0) then
      FUnregisterSuspendResumeNotification
        (FRegisterSuspendResumeNotificationHandle);
    FRegisterSuspendResumeNotificationHandle := 0;
    Result := True;
  end;
end;

procedure TLZWindowsMessages.SetOnEndSession(const Value
  : TOnWindowsMessageEvent);
begin
  FOnEndSession := Value;
end;

procedure TLZWindowsMessages.SetOnPowerBroadcast(const Value
  : TOnWindowsMessageEvent);
begin
  FOnPowerBroadcast := Value;
end;

procedure TLZWindowsMessages.SetOnSettingsChange(const Value
  : TOnWindowsMessageEvent);
begin
  FOnSettingsChange := Value;
end;

procedure TLZWindowsMessages.SetOnTimeChange(const Value
  : TOnWindowsMessageEvent);
begin
  FOnTimeChange := Value;
end;

procedure TLZWindowsMessages.WMPowerBroadcast(var Msg: TMessage);
begin
  case Msg.WParam of
    PBT_APMBATTERYLOW:
      Log('Battery power is low.');

    PBT_APMOEMEVENT:
      Log('OEM-defined event occurred.');

    PBT_APMPOWERSTATUSCHANGE:
      Log('Power status has changed.');

    PBT_APMQUERYSUSPEND, PBT_APMQUERYSTANDBY:
      Log('Request for permission to suspend / standby.');

    PBT_APMQUERYSUSPENDFAILED:
      Log('Suspension request denied.');

    PBT_APMQUERYSTANDBYFAILED:
      Log('Standby request denied.');

    PBT_APMSUSPEND:
      Log('Operation suspend.');
    PBT_APMSTANDBY:
      Log('Operation standby.');

    PBT_APMRESUMEAUTOMATIC, PBT_APMRESUMECRITICAL, PBT_APMRESUMESUSPEND,
      PBT_APMRESUMESTANDBY:
      Log('System resumed.');
  end;
  Log(Format('WM_POWERBROADCAST wParam: %d', [Msg.WParam]));
  if assigned(FOnPowerBroadcast) then
    FOnPowerBroadcast(Self, Msg);
end;

procedure TLZWindowsMessages.WMTimeChange(var Msg: TMessage);
begin
  Log('System date/time change event');
  Log(Format('WM_TIMECHANGE wParam: %d', [Msg.WParam]));
  if assigned(FOnTimeChange) then
    FOnTimeChange(Self, Msg);
end;

procedure TLZWindowsMessages.WMEndSession(var Msg: TMessage);
begin
  Log('End Session Request');
  Log(Format('WM_ENDSESSION wParam: %d', [Msg.WParam]));
  if assigned(FOnEndSession) then
    FOnEndSession(Self, Msg);
end;

procedure TLZWindowsMessages.WMSettingsChange(var Msg: TMessage);
begin
  Log('System-wide setting or policy settings have changed.');
  Log(Format('WM_SETTINGCHANGE wParam: %d', [Msg.WParam]));
  if assigned(FOnSettingsChange) then
    FOnSettingsChange(Self, Msg);
end;

procedure TLZWindowsMessages.WndMethod(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_POWERBROADCAST:
      WMPowerBroadcast(Msg);
    WM_TIMECHANGE:
      WMTimeChange(Msg);
    WM_ENDSESSION:
      WMEndSession(Msg);
    WM_SETTINGCHANGE:
      WMSettingsChange(Msg);
  end;
end;

end.
