unit WinApi.Networklist;

interface

uses
  SysUtils,
  ActiveX,
  ComObj,
  Windows,
  NETWORKLIST_TLB;

type
  ENetworkListException = class(Exception);

Type
  TNetworklist = class
  private
    FNetworkListManager: INetworkListManager;
    function GetNetworkConnectivity(Connectivity: NLM_CONNECTIVITY): string;
    function GetConnected: Boolean;
    function GetConnectivity: string;
    function GetInternet: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Connected: Boolean read GetConnected;
    property Internet: Boolean read GetInternet;
    property Connectivity: string read GetConnectivity;
  end;

implementation

constructor TNetworklist.Create;
begin
  CoInitialize(nil);
  if TOSVersion.Check(6) then
  begin
    FNetworkListManager := CoNetworkListManager.Create;
  end;
end;

destructor TNetworklist.Destroy;
begin
  try
    CoUninitialize;
  finally
    inherited;
  end;
end;

function TNetworklist.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(FNetworkListManager) then
    Result := FNetworkListManager.IsConnected;
end;

function TNetworklist.GetConnectivity: string;
begin
  Result := 'Unknown';
  if Assigned(FNetworkListManager) then
    Result := GetNetworkConnectivity(FNetworkListManager.GetConnectivity);
end;

function TNetworklist.GetInternet: Boolean;
begin
  Result := False;
  if Assigned(FNetworkListManager) then
    Result := FNetworkListManager.IsConnectedToInternet;
end;

function TNetworklist.GetNetworkConnectivity(Connectivity
  : NLM_CONNECTIVITY): string;
begin
  Result := '';
  if NLM_CONNECTIVITY_DISCONNECTED and Connectivity <> 0 then
    Result := Result + 'Disconnected, ';
  if NLM_CONNECTIVITY_IPV4_NOTRAFFIC and Connectivity <> 0 then
    Result := Result + 'Connected but not ipv4 traffic, ';
  if NLM_CONNECTIVITY_IPV6_NOTRAFFIC and Connectivity <> 0 then
    Result := Result + 'Connected but not ipv6 traffic, ';
  if NLM_CONNECTIVITY_IPV4_SUBNET and Connectivity <> 0 then
    Result := Result + 'Subnet ipv4, ';
  if NLM_CONNECTIVITY_IPV4_LOCALNETWORK and Connectivity <> 0 then
    Result := Result + 'LocalNetwork ipv4, ';
  if NLM_CONNECTIVITY_IPV4_INTERNET and Connectivity <> 0 then
    Result := Result + 'Internet ipv4, ';
  if NLM_CONNECTIVITY_IPV6_SUBNET and Connectivity <> 0 then
    Result := Result + 'Subnet ipv6, ';
  if NLM_CONNECTIVITY_IPV6_LOCALNETWORK and Connectivity <> 0 then
    Result := Result + 'LocalNetwork ipv6, ';
  if NLM_CONNECTIVITY_IPV6_INTERNET and Connectivity <> 0 then
    Result := Result + 'Internet ipv6, ';

  Result := StringReplace('[' + Result + ']', ', ]', ']', [rfReplaceAll]);
end;

end.
