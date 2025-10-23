unit Unifi.Models;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  System.JSON, System.Generics.Collections, Lazy.Types, Lazy.Log, Lazy.Model;

type
  TLZUniFiModel = class(TLZModel);

  TLZUniFiModelList<T: TLZUniFiModel> = class(TLZModelList<T>)
  protected
    function GetDefaultArrayName: string; override;
  end;

  TLZUniFiSite = class(TLZUniFiModel)
  private
    FName: string;
    FID: string;
    FExternalID: string;
    FRole: string;
    FDescription: string;
    FAnonymousID: string;
    FDeviceCount: integer;
    procedure SetID(const Value: string);
    procedure SetName(const Value: string);
    procedure SetAnonymousID(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetDeviceCount(const Value: integer);
    procedure SetExternalID(const Value: string);
    procedure SetRole(const Value: string);
  protected
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property ID: string read FID write SetID;
    property Name: string read FName write SetName;
    property Description: string read FDescription write SetDescription;
    property ExternalID: string read FExternalID write SetExternalID;
    property AnonymousID: string read FAnonymousID write SetAnonymousID;
    property DeviceCount: integer read FDeviceCount write SetDeviceCount;
    property Role: string read FRole write SetRole;
  end;

  TLZUniFiSites = class(TLZUniFiModelList<TLZUniFiSite>)
  protected
  public
    function Find(AID: string): TLZUniFiSite;
    function FindByName(AName: string): TLZUniFiSite;
  end;

  TLZUniFiWLANConf = class(TLZUniFiModel)
  private
    FName: string;
    FEnabled: boolean;
    FWPAMode: string;
    FID: string;
    FPassphrase: string;
    FSecurity: string;
    FSiteID: string;
    procedure SetEnabled(const Value: boolean);
    procedure SetID(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPassphrase(const Value: string);
    procedure SetSecurity(const Value: string);
    procedure SetSiteID(const Value: string);
    procedure SetWPAMode(const Value: string);
  protected
    procedure SetDefaults; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property ID: string read FID write SetID;
    property SiteID: string read FSiteID write SetSiteID;
    property Name: string read FName write SetName;
    property WPAMode: string read FWPAMode write SetWPAMode;
    property Security: string read FSecurity write SetSecurity;
    property Passphrase: string read FPassphrase write SetPassphrase;
    property Enabled: boolean read FEnabled write SetEnabled;
  end;

  TLZUniFiWLANConfs = class(TLZUniFiModelList<TLZUniFiWLANConf>)
  protected
  public
    function Find(AID: string): TLZUniFiWLANConf;
    function FindByName(AName: string): TLZUniFiWLANConf;
  end;

  TLZUniFiDevice = class(TLZUniFiModel)
  private
    FVersion: string;
    FName: string;
    FDeviceType: string;
    FState: integer;
    FModel: string;
    FInformURL: string;
    FAdopted: boolean;
    FSerial: string;
    FID: string;
    FSupportWifi6e: boolean;
    FIPAddress: string;
    FSiteID: string;
    FMAC: string;
    FDisconnectionReason: string;
    procedure SetAdopted(const Value: boolean);
    procedure SetDeviceType(const Value: string);
    procedure SetID(const Value: string);
    procedure SetInformURL(const Value: string);
    procedure SetIPAddress(const Value: string);
    procedure SetMAC(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetName(const Value: string);
    procedure SetSerial(const Value: string);
    procedure SetSiteID(const Value: string);
    procedure SetState(const Value: integer);
    procedure SetSupportWifi6e(const Value: boolean);
    procedure SetVersion(const Value: string);
    procedure SetDisconnectionReason(const Value: string);
  protected
    procedure SetDefaults; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property ID: string read FID write SetID;
    property SiteID: string read FSiteID write SetSiteID;
    property Name: string read FName write SetName;
    property DeviceType: string read FDeviceType write SetDeviceType;
    property Model: string read FModel write SetModel;
    property Version: string read FVersion write SetVersion;
    property Adopted: boolean read FAdopted write SetAdopted;
    property IPAddress: string read FIPAddress write SetIPAddress;
    property MAC: string read FMAC write SetMAC;
    property Serial: string read FSerial write SetSerial;
    property SupportWifi6e: boolean read FSupportWifi6e write SetSupportWifi6e;
    property InformURL: string read FInformURL write SetInformURL;
    property State: integer read FState write SetState;
    property DisconnectionReason: string read FDisconnectionReason
      write SetDisconnectionReason;
  end;

  TLZUniFiDevices = class(TLZUniFiModelList<TLZUniFiDevice>)
  protected
  public
    function Find(AID: string): TLZUniFiDevice;
    function FindByMAC(AMAC: string): TLZUniFiDevice;
  end;

implementation

uses
  Lazy.Utils;

{ TLZUniFiSite }

procedure TLZUniFiSite.AfterModelCreated;
begin
  inherited;

end;

procedure TLZUniFiSite.BeforeModelDestroyed;
begin
  inherited;

end;

procedure TLZUniFiSite.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  FAnonymousID := '';

  ID := AJSONValue.GetValue<string>('_id');
  Name := AJSONValue.GetValue<string>('name');
  Description := AJSONValue.GetValue<string>('desc');
  Role := AJSONValue.GetValue<string>('role');
  AJSONValue.TryGetValue<string>('anonymous_id', FAnonymousID);
  ExternalID := AJSONValue.GetValue<string>('external_id');
  DeviceCount := AJSONValue.GetValue<integer>('device_count');
end;

function TLZUniFiSite.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FID) then
      LJSONObject.AddPair('_id', FID);
    
    if not TLZString.IsEmptyString(FName) then
      LJSONObject.AddPair('name', FName);
    
    if not TLZString.IsEmptyString(FDescription) then
      LJSONObject.AddPair('desc', FDescription);
    
    if not TLZString.IsEmptyString(FRole) then
      LJSONObject.AddPair('role', FRole);
    
    if not TLZString.IsEmptyString(FAnonymousID) then
      LJSONObject.AddPair('anonymous_id', FAnonymousID);
    
    if not TLZString.IsEmptyString(FExternalID) then
      LJSONObject.AddPair('external_id', FExternalID);
    
    LJSONObject.AddPair('device_count', TJSONNumber.Create(FDeviceCount));
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZUniFiSite.SetAnonymousID(const Value: string);
begin
  FAnonymousID := Value;
end;

procedure TLZUniFiSite.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TLZUniFiSite.SetDeviceCount(const Value: integer);
begin
  FDeviceCount := Value;
end;

procedure TLZUniFiSite.SetExternalID(const Value: string);
begin
  FExternalID := Value;
end;

procedure TLZUniFiSite.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZUniFiSite.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TLZUniFiSite.SetRole(const Value: string);
begin
  FRole := Value;
end;

{ TLZUniFiSites }

function TLZUniFiSites.Find(AID: string): TLZUniFiSite;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].ID, AID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZUniFiSites.FindByName(AName: string): TLZUniFiSite;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].Name, AName) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TLZUniFiModelList<T> }

function TLZUniFiModelList<T>.GetDefaultArrayName: string;
begin
  Result := '';
end;

{ TLZUniFiWLANConf }

procedure TLZUniFiWLANConf.FromJSONValue(AJSONValue: TJSONValue);
begin
  SetDefaults;
  ID := AJSONValue.GetValue<string>('_id');
  Name := AJSONValue.GetValue<string>('name');
  Enabled := AJSONValue.GetValue<boolean>('enabled');
  SiteID := AJSONValue.GetValue<string>('site_id');
  AJSONValue.TryGetValue<string>('wpa_mode', FWPAMode);
  AJSONValue.TryGetValue<string>('security', FSecurity);
  AJSONValue.TryGetValue<string>('x_passphrase', FPassphrase);

end;

function TLZUniFiWLANConf.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FID) then
      LJSONObject.AddPair('_id', FID);
    
    if not TLZString.IsEmptyString(FName) then
      LJSONObject.AddPair('name', FName);
    
    LJSONObject.AddPair('enabled', TJSONBool.Create(FEnabled));
    
    if not TLZString.IsEmptyString(FSiteID) then
      LJSONObject.AddPair('site_id', FSiteID);
    
    if not TLZString.IsEmptyString(FWPAMode) then
      LJSONObject.AddPair('wpa_mode', FWPAMode);
    
    if not TLZString.IsEmptyString(FSecurity) then
      LJSONObject.AddPair('security', FSecurity);
    
    if not TLZString.IsEmptyString(FPassphrase) then
      LJSONObject.AddPair('x_passphrase', FPassphrase);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZUniFiWLANConf.SetDefaults;
begin
  FID := '';
  FName := '';
  FWPAMode := '';
  FPassphrase := '';
  FSecurity := '';
  FSiteID := '';
end;

procedure TLZUniFiWLANConf.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TLZUniFiWLANConf.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZUniFiWLANConf.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TLZUniFiWLANConf.SetPassphrase(const Value: string);
begin
  FPassphrase := Value;
end;

procedure TLZUniFiWLANConf.SetSecurity(const Value: string);
begin
  FSecurity := Value;
end;

procedure TLZUniFiWLANConf.SetSiteID(const Value: string);
begin
  FSiteID := Value;
end;

procedure TLZUniFiWLANConf.SetWPAMode(const Value: string);
begin
  FWPAMode := Value;
end;

{ TLZUniFiWLANConfs }

function TLZUniFiWLANConfs.Find(AID: string): TLZUniFiWLANConf;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].ID, AID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZUniFiWLANConfs.FindByName(AName: string): TLZUniFiWLANConf;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].Name, AName) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TLZUniFiDevice }

procedure TLZUniFiDevice.FromJSONValue(AJSONValue: TJSONValue);
begin
  SetDefaults;

  AJSONValue.TryGetValue<string>('_id', FID);
  AJSONValue.TryGetValue<boolean>('adopted', FAdopted);
  AJSONValue.TryGetValue<boolean>('support_wifi6e', FSupportWifi6e);
  AJSONValue.TryGetValue<string>('name', FName);
  AJSONValue.TryGetValue<string>('site_id', FSiteID);
  AJSONValue.TryGetValue<string>('ip', FIPAddress);
  AJSONValue.TryGetValue<string>('inform_url', FInformURL);
  AJSONValue.TryGetValue<string>('version', FVersion);
  AJSONValue.TryGetValue<string>('serial', FSerial);
  AJSONValue.TryGetValue<string>('mac', FMAC);
  AJSONValue.TryGetValue<integer>('state', FState);
  AJSONValue.TryGetValue<string>('disconnection_reason', FDisconnectionReason);

end;

function TLZUniFiDevice.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FID) then
      LJSONObject.AddPair('_id', FID);
    
    LJSONObject.AddPair('adopted', TJSONBool.Create(FAdopted));
    LJSONObject.AddPair('support_wifi6e', TJSONBool.Create(FSupportWifi6e));
    
    if not TLZString.IsEmptyString(FName) then
      LJSONObject.AddPair('name', FName);
    
    if not TLZString.IsEmptyString(FSiteID) then
      LJSONObject.AddPair('site_id', FSiteID);
    
    if not TLZString.IsEmptyString(FIPAddress) then
      LJSONObject.AddPair('ip', FIPAddress);
    
    if not TLZString.IsEmptyString(FInformURL) then
      LJSONObject.AddPair('inform_url', FInformURL);
    
    if not TLZString.IsEmptyString(FVersion) then
      LJSONObject.AddPair('version', FVersion);
    
    if not TLZString.IsEmptyString(FSerial) then
      LJSONObject.AddPair('serial', FSerial);
    
    if not TLZString.IsEmptyString(FMAC) then
      LJSONObject.AddPair('mac', FMAC);
    
    LJSONObject.AddPair('state', TJSONNumber.Create(FState));
    
    if not TLZString.IsEmptyString(FDisconnectionReason) then
      LJSONObject.AddPair('disconnection_reason', FDisconnectionReason);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZUniFiDevice.SetAdopted(const Value: boolean);
begin
  FAdopted := Value;
end;

procedure TLZUniFiDevice.SetDefaults;
begin
  FVersion := '';
  FName := '';
  FDeviceType := '';
  FState := 0;
  FModel := '';
  FInformURL := '';
  FAdopted := False;
  FSerial := '';
  FID := '';
  FSupportWifi6e := False;
  FIPAddress := '';
  FSiteID := '';
  FMAC := '';
  FDisconnectionReason := '';

end;

procedure TLZUniFiDevice.SetDeviceType(const Value: string);
begin
  FDeviceType := Value;
end;

procedure TLZUniFiDevice.SetDisconnectionReason(const Value: string);
begin
  FDisconnectionReason := Value;
end;

procedure TLZUniFiDevice.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZUniFiDevice.SetInformURL(const Value: string);
begin
  FInformURL := Value;
end;

procedure TLZUniFiDevice.SetIPAddress(const Value: string);
begin
  FIPAddress := Value;
end;

procedure TLZUniFiDevice.SetMAC(const Value: string);
begin
  FMAC := Value;
end;

procedure TLZUniFiDevice.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TLZUniFiDevice.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TLZUniFiDevice.SetSerial(const Value: string);
begin
  FSerial := Value;
end;

procedure TLZUniFiDevice.SetSiteID(const Value: string);
begin
  FSiteID := Value;
end;

procedure TLZUniFiDevice.SetState(const Value: integer);
begin
  FState := Value;
end;

procedure TLZUniFiDevice.SetSupportWifi6e(const Value: boolean);
begin
  FSupportWifi6e := Value;
end;

procedure TLZUniFiDevice.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

{ TLZUniFiDevices }

function TLZUniFiDevices.Find(AID: string): TLZUniFiDevice;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].ID, AID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZUniFiDevices.FindByMAC(AMAC: string): TLZUniFiDevice;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].MAC, AMAC) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

end.
