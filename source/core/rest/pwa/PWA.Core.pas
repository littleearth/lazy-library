unit PWA.Core;

interface

uses
  Lazy.Types, System.SysUtils, System.Types, System.Classes,
  System.Variants, Lazy.RESTClient;

const
  PWA_DEFAULT_END_POINT = 'https://api.pulseway.com/v2/';

type
  TLZPWACore = class(TLZRESTClientBasicAuth)
  private
    FEndPoint: string;
    procedure SetEndPoint(const Value: string);
  protected
    procedure SetDefaults; override;
    function GetBaseURL: string; override;
  public
    property EndPoint: string read FEndPoint write SetEndPoint;
  end;

  TLZPWAPriority = (pwaLow, pwaNormal, pwaElevated, pwaCritical);

  TLZPWAPriorityHelper = class(TLZObject)
  public
    class function AsString(APWAPriority: TLZPWAPriority): string;
    class function FromString(AValue: string): TLZPWAPriority;
  end;

  TLZPWANotifications = class(TLZPWACore)
  public
    procedure RegisterSytem(AInstanceID: string; AName: string;
      AGroup: string = ''; ADescription: string = '';
      ANextRefreshInterval: integer = 5; ANotifyWhenOffline: boolean = true);
    procedure Notify(AInstanceID: string; ATitle: string; AMessage: string;
      APriority: TLZPWAPriority);

  end;

  { TPWACore }

implementation

uses
  System.JSON;

function TLZPWACore.GetBaseURL: string;
begin
  REsult := EndPoint;
end;

procedure TLZPWACore.SetDefaults;
begin
  inherited;
  FEndPoint := PWA_DEFAULT_END_POINT;
end;

procedure TLZPWACore.SetEndPoint(const Value: string);
begin
  FEndPoint := Value;
end;

{ TPWANotifications }

procedure TLZPWANotifications.Notify(AInstanceID, ATitle, AMessage: string;
  APriority: TLZPWAPriority);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('instance_id', AInstanceID);
    LJSON.AddPair('title', ATitle);
    LJSON.AddPair('message', AMessage);
    LJSON.AddPair('priority', TLZPWAPriorityHelper.AsString(APriority));

    Post('notifications', LJSON.ToJSON,
      procedure(Asender: TObject; ASuccess: boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      begin
        if ASuccess then
        begin
          Log(ARESTResponse.JSONValue.Format);
        end
        else
        begin
          Error(AMessage);
        end;
      end)
  finally
    FreeAndNil(LJSON);
  end;
end;

procedure TLZPWANotifications.RegisterSytem(AInstanceID, AName, AGroup,
  ADescription: string; ANextRefreshInterval: integer;
ANotifyWhenOffline: boolean);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('instance_id', AInstanceID);
    LJSON.AddPair('name', AName);
    LJSON.AddPair('group', AGroup);
    LJSON.AddPair('next_refresh_interval_minutes', ANextRefreshInterval);
    LJSON.AddPair('notify_when_offline', ANotifyWhenOffline);
    Post('systems', LJSON.ToJSON,
      procedure(Asender: TObject; ASuccess: boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      begin
        if ASuccess then
        begin
          Log(ARESTResponse.JSONValue.Format);
        end
        else
        begin
          Error(AMessage);
        end;
      end);
  finally
    FreeAndNil(LJSON);
  end;
end;

{ TPWAPriorityHelper }

class function TLZPWAPriorityHelper.AsString(APWAPriority
  : TLZPWAPriority): string;
begin
  REsult := 'normal';
  case APWAPriority of
    pwaLow:
      REsult := 'low';
    pwaElevated:
      REsult := 'elevated';
    pwaCritical:
      REsult := 'critical';
  end;
end;

class function TLZPWAPriorityHelper.FromString(AValue: string): TLZPWAPriority;
begin
  REsult := pwaNormal;
  if SameText(AValue, 'low') then
    REsult := pwaLow;
  if SameText(AValue, 'elevated') then
    REsult := pwaElevated;
  if SameText(AValue, 'critical') then
    REsult := pwaCritical;
end;

end.
