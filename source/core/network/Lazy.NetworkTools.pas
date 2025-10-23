unit Lazy.NetworkTools;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Lazy.Types, Lazy.Utils;

type
  TLZNetworkTools = class(TLZObject)
  public
    class procedure GetLocalNetworkList(
      AList: TStrings;
      AIncludeSubnet: boolean = false);
    class function GetNetworkAddress(AIPAddress: string): string; static;
    class procedure GetLocalNetwork(
      var ANetwork: string;
      var ASubnet: string);
    class procedure GetIPList(
      AIPList: TStrings;
      ANetwork: string = '';
      ASubnet: string = '255.255.255.0'); overload;
    class procedure GetIPList(
      AIPList: TStrings;
      ANetworkList: TStrings); overload;
    class procedure GetLocalNetworkIPList(AIPList: TStrings);
    class function IsIPAddress(AIP: string): boolean;
    class function Ping(
      const AHost: string;
      ATimeout: integer = 200;
      APacketSize: integer = 24): boolean;
    class function GetHostname: string;
    class function GetIPAddress: string;
    class function GetIPAddresses: string;
  end;

implementation

uses
  IdNetworkCalculator,
  IdGlobal, IdStack, IdBaseComponent,
  IdIcmpClient;

{ TNetTools }

class procedure TLZNetworkTools.GetIPList(
  AIPList: TStrings;
  ANetwork, ASubnet: string);
var
  LIdNetworkCalculator: TIdNetworkCalculator;
  LIdx: integer;
  LIP: string;
begin
  if Assigned(AIPList) then
  begin
    LIdNetworkCalculator := TIdNetworkCalculator.Create(nil);
    try
      LIdNetworkCalculator.NetworkAddress.AsString := ANetwork;
      if not TLZString.IsEmptyString(ASubnet) then
      begin
        LIdNetworkCalculator.NetworkMask.AsString := ASubnet;
      end;
      for LIdx := 0 to (LIdNetworkCalculator.ListIP.Count - 2) do
      begin
        LIP := LIdNetworkCalculator.ListIP[LIdx];
        if AIPList.IndexOf(LIP) = -1 then
        begin
          AIPList.Add(LIP);
        end;
      end;
    finally
      FreeAndNil(LIdNetworkCalculator);
    end;
  end;
end;

{ TDuoNetworkTools }

class function TLZNetworkTools.GetHostname: string;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.Hostname;
  finally
    TIdStack.DecUsage;
  end;
end;

class function TLZNetworkTools.GetIPAddress: string;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.LocalAddress;
  finally
    TIdStack.DecUsage;
  end;
end;

class function TLZNetworkTools.GetIPAddresses: string;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.LocalAddresses.DelimitedText;
  finally
    TIdStack.DecUsage;
  end;
end;

class procedure TLZNetworkTools.GetIPList(AIPList, ANetworkList: TStrings);
var
  LIdx: integer;
begin
  for LIdx := 0 to Pred(ANetworkList.Count) do
  begin
    GetIPList(AIPList, ANetworkList.Names[LIdx],
      ANetworkList.ValueFromIndex[LIdx]);
  end;
end;

class procedure TLZNetworkTools.GetLocalNetwork(var ANetwork, ASubnet: string);
var
  LList: TStringList;
begin
  ANetwork := '';
  ASubnet := '';
  LList := TStringList.Create;
  try
    GetLocalNetworkList(LList, True);
    if LList.Count > 0 then
    begin
      ANetwork := LList.Names[0];
      ASubnet := LList.ValueFromIndex[0];
    end;
  finally
    FreeAndNil(LList);
  end;
end;

class procedure TLZNetworkTools.GetLocalNetworkIPList(AIPList: TStrings);
var
  LNetworks: TStringList;
  LIdx: integer;
begin
  LNetworks := TStringList.Create;
  try
    GetLocalNetworkList(LNetworks, True);
    for LIdx := 0 to Pred(LNetworks.Count) do
    begin
      GetIPList(AIPList, LNetworks.Names[LIdx], LNetworks.ValueFromIndex[LIdx]);
    end;
  finally
    FreeAndNil(LNetworks);
  end;
end;

class function TLZNetworkTools.GetNetworkAddress(AIPAddress: string): string;
var
  LIdNetworkCalculator: TIdNetworkCalculator;
begin
  LIdNetworkCalculator := TIdNetworkCalculator.Create(nil);
  try
    try
      LIdNetworkCalculator.NetworkAddress.AsString := AIPAddress;
      LIdNetworkCalculator.NetworkAddress.Byte4 := 0;
      Result := LIdNetworkCalculator.NetworkAddress.AsString;
    except
      Result := '';
    end;
  finally
    FreeAndNil(LIdNetworkCalculator);
  end;
end;

class procedure TLZNetworkTools.GetLocalNetworkList(
  AList: TStrings;
  AIncludeSubnet: boolean);
var
  LList: TIdStackLocalAddressList;
  I: integer;
  LIPAddress: string;
begin
  TIdStack.IncUsage;
  try
    LList := TIdStackLocalAddressList.Create;
    try
      // for backwards compatibility, return only IPv4 addresses
      GStack.GetLocalAddressList(LList);
      if LList.Count > 0 then
      begin
        AList.BeginUpdate;
        try
          for I := 0 to LList.Count - 1 do
          begin
            if LList[I].IPVersion = Id_IPv4 then
            begin
              LIPAddress := GetNetworkAddress(LList[I].IPAddress);
              if (not TLZString.IsEmptyString(LIPAddress)) then
              begin
                if AIncludeSubnet then
                begin
                  AList.AddPair(LIPAddress, TIdStackLocalAddressIPv4(LList[I])
                    .SubNetMask);
                end
                else
                begin
                  AList.Add(LIPAddress);
                end;
              end;
            end;
          end;
        finally
          AList.EndUpdate;
        end;
      end;
    finally
      LList.Free;
    end;
  finally
    TIdStack.DecUsage;
  end;
end;

class function TLZNetworkTools.IsIPAddress(AIP: string): boolean;
var
  LIdNetworkCalculator: TIdNetworkCalculator;
begin
  Result := True;
  LIdNetworkCalculator := TIdNetworkCalculator.Create(nil);
  try
    try
      LIdNetworkCalculator.NetworkAddress.AsString := AIP;
    except
      Result := false;
    end;
  finally
    FreeAndNil(LIdNetworkCalculator);
  end;
end;

class function TLZNetworkTools.Ping(
  const AHost: string;
  ATimeout, APacketSize: integer): boolean;
var
  IdIcmpClient: TIdIcmpClient;
begin

  IdIcmpClient := TIdIcmpClient.Create(nil);
  try
    IdIcmpClient.ReceiveTimeout := ATimeout;
    IdIcmpClient.Host := AHost;
    IdIcmpClient.PacketSize := APacketSize;
    IdIcmpClient.Protocol := 1;
    IdIcmpClient.IPVersion := Id_IPv4;

    try
      IdIcmpClient.Ping;
      Result := True;
    except
      Result := false;
    end;
    if IdIcmpClient.ReplyStatus.ReplyStatusType <> rsEcho Then
      Result := false;
  finally
    FreeAndNil(IdIcmpClient);
  end;

end;

end.
