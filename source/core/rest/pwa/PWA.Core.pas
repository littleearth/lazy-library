unit PWA.Core;

interface

uses
  Lazy.Types, System.SysUtils, System.Types, System.Classes,
  System.Variants, Lazy.RESTClient;

const
  PWA_DEFAULT_END_POINT = 'https://api.pulseway.com/v2/';

type
  TPWACore = class(TLazyRESTClientBasicAuth)
  private
    FEndPoint: string;
    procedure SetEndPoint(const Value: string);
  protected
    procedure SetDefaults; override;
    function GetBaseURL: string; override;
  public
    property EndPoint: string read FEndPoint write SetEndPoint;
  end;

  TPWAPriority = (pwaLow, pwaNormal, pwaElevated, pwaCritical);

  TPWAPriorityHelper = class(TLazyObject)
  public
    class function AsString(APWAPriority: TPWAPriority): string;
    class function FromString(AValue: string): TPWAPriority;
  end;

  TPWANotifications = class(TPWACore)
  public
    procedure RegisterSytem(AInstanceID: string; AName: string;
      AGroup: string = ''; ADescription: string = '';
      ANextRefreshInterval: integer = 5; ANotifyWhenOffline: boolean = true);
    procedure Notify(AInstanceID: string; ATitle: string; AMessage: string;
      APriority: TPWAPriority);

  end;

  { TPWACore }

implementation

uses
  System.JSON;

function TPWACore.GetBaseURL: string;
begin
  REsult := EndPoint;
end;

procedure TPWACore.SetDefaults;
begin
  inherited;
  FEndPoint := PWA_DEFAULT_END_POINT;
end;

procedure TPWACore.SetEndPoint(const Value: string);
begin
  FEndPoint := Value;
end;

{ TPWANotifications }

procedure TPWANotifications.Notify(AInstanceID, ATitle, AMessage: string;
  APriority: TPWAPriority);
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair('instance_id', AInstanceID);
    LJSON.AddPair('title', ATitle);
    LJSON.AddPair('message', AMessage);
    LJSON.AddPair('priority', TPWAPriorityHelper.AsString(APriority));

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

procedure TPWANotifications.RegisterSytem(AInstanceID, AName, AGroup,
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

class function TPWAPriorityHelper.AsString(APWAPriority: TPWAPriority): string;
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

class function TPWAPriorityHelper.FromString(AValue: string): TPWAPriority;
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
