unit WinApi.DeviceNotifier;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Variants,
  System.Classes, Lazy.Types;

type
  PDevBroadcastHdr = ^DEV_BROADCAST_HDR;

  DEV_BROADCAST_HDR = packed record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
    dbch_handle: THandle;
    dbch_hdevnotify: HDEVNOTIFY;
    dbch_eventguid: TGUID;
    dbch_nameoffset: LONG;
  end;

  TDevBroadcastHdr = DEV_BROADCAST_HDR;

  PDevBroadcastVolume = ^DEV_BROADCAST_VOLUME;

  DEV_BROADCAST_VOLUME = packed record
    dbcv_size: DWORD;
    dbcv_devicetype: DWORD;
    dbcv_reserved: DWORD;
    dbcv_unitmask: DWORD;
    dbcv_flags: WORD;
  end;

  TDevBroadcastVolume = DEV_BROADCAST_VOLUME;

type
  PDevBroadcastDeviceInterface = ^DEV_BROADCAST_DEVICEINTERFACE;

  DEV_BROADCAST_DEVICEINTERFACE = record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: TGUID;
    dbcc_name: Char;
  end;

  TDevBroadcastDeviceInterface = DEV_BROADCAST_DEVICEINTERFACE;

const
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVTYP_DEVICEINTERFACE = $00000005;
  DBT_DEVTYP_VOLUME = $00000002;
  DBT_DEVTYP_PORT = $00000003;
  DBT_DEVTYP_HANDLE = $00000006;
  DBTF_Media = $0001;

  GUID_DEVINTERFACE_USB_DEVICE
    : TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  GUID_DEVINTERFACE_COMPORT: TGUID = '{86E0D1E0-8089-11D0-9CE4-08003E301F73}';
  GUID_DEVINTERFACE_PARALLEL: TGUID = '{97F76EF0-F883-11D0-AF1F-0000F800845C}';
  GUID_DEVINTERFACE_VOLUME: TGUID = '{53F5630D-B6BF-11D0-94F2-00A0C91EFB8B}';
  GUID_IO_VOLUME_MOUNT: TGUID = '{B5804878-1A96-11D2-8FFD-00A0C9A06D32}';

type
  TDeviceNotifyProc = procedure(ASender: TObject; const ADeviceName: String;
    ADeviceType: Cardinal) of Object;

  TLZDeviceNotifier = class(TLZObject)
  private
    hRecipient: HWND;
    FNotificationHandle: Pointer;
    FDeviceArrival: TDeviceNotifyProc;
    FDeviceRemoval: TDeviceNotifyProc;
    procedure WndProc(var Msg: TMessage);
    function GetDrive(pDBVol: PDevBroadcastVolume): string;
  public
    constructor Create(GUID_DEVINTERFACE: TGUID);
    property OnDeviceArrival: TDeviceNotifyProc read FDeviceArrival
      write FDeviceArrival;
    property OnDeviceRemoval: TDeviceNotifyProc read FDeviceRemoval
      write FDeviceRemoval;
    destructor Destroy; override;
  end;

implementation

constructor TLZDeviceNotifier.Create(GUID_DEVINTERFACE: TGUID);
var
  NotificationFilter: TDevBroadcastDeviceInterface;
begin
  inherited Create;
  hRecipient := AllocateHWnd(WndProc);
  ZeroMemory(@NotificationFilter, SizeOf(NotificationFilter));
  NotificationFilter.dbcc_size := SizeOf(NotificationFilter);
  NotificationFilter.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  NotificationFilter.dbcc_classguid := GUID_DEVINTERFACE;
  // register the device class to monitor
  FNotificationHandle := RegisterDeviceNotification(hRecipient,
    @NotificationFilter, DEVICE_NOTIFY_WINDOW_HANDLE);
end;

function TLZDeviceNotifier.GetDrive(pDBVol: PDevBroadcastVolume): string;
var
  i: Byte;
  Maske: DWORD;
begin
  // if (pDBVol^.dbcv_flags and DBTF_Media) = DBTF_Media then
  // begin
  Maske := pDBVol^.dbcv_unitmask;
  for i := 0 to 25 do
  begin
    if (Maske and 1) = 1 then
      Result := Char(i + Ord('A')) + ':';
    Maske := Maske shr 1;
  end;
  // end;
end;

procedure TLZDeviceNotifier.WndProc(var Msg: TMessage);
var
  Dbi: PDevBroadcastDeviceInterface;
  Dbv: PDevBroadcastVolume;
  DeviceType: Cardinal;
  DeviceName: string;
begin
  with Msg do
    if (Msg = WM_DEVICECHANGE) and
      ((WParam = DBT_DEVICEARRIVAL) or (WParam = DBT_DEVICEREMOVECOMPLETE)) then
      try
        Dbi := PDevBroadcastDeviceInterface(LParam);
        DeviceName := PChar(@Dbi.dbcc_name);
        DeviceType := Dbi.dbcc_devicetype;
        case DeviceType of
          DBT_DEVTYP_DEVICEINTERFACE:
            begin
            end;
          DBT_DEVTYP_VOLUME:
            begin
              Dbv := PDevBroadcastVolume(LParam);
              DeviceName := GetDrive(Dbv);
            end;
        end;

        if WParam = DBT_DEVICEARRIVAL then
        begin
          if Assigned(FDeviceArrival) then
            FDeviceArrival(Self, DeviceName, DeviceType);
        end
        else if WParam = DBT_DEVICEREMOVECOMPLETE then
        begin
          if Assigned(FDeviceRemoval) then
            FDeviceRemoval(Self, DeviceName, DeviceType);
        end;
      except
        Result := DefWindowProc(hRecipient, Msg, WParam, LParam);
      end
    else
      Result := DefWindowProc(hRecipient, Msg, WParam, LParam);
end;

destructor TLZDeviceNotifier.Destroy;
begin
  try
    UnregisterDeviceNotification(FNotificationHandle);
    DeallocateHWnd(hRecipient);
  finally
    inherited;
  end;
end;

end.
