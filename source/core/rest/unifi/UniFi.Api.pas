unit UniFi.Api;

interface

uses
  Lazy.Types,
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  IdHTTP, IdSSLOpenSSL, IdCookie, IdURI, IdGlobal;

const
  UNIFI_BASEURL = 'https://127.0.0.1:8443';
  UNIFI_DEFAULT_SITE = 'default';
  UNIFI_COOKIE_NAME = 'unificookie';
  UNIFI_VERSION = '8.0.28';

type
  THTTPMethod = (hmGET, hmPOST, hmPUT, hmDELETE, hmPATCH);

  TLZUniFiClient = class(TLZObject)
  private
    FBaseUrl: string;
    FUser: string;
    FPassword: string;
    FSite: string;
    FVersion: string;
    FIsLoggedIn: Boolean;
    FIsUniFiOS: Boolean;
    FExecRetries: Integer;
    FCookies: string;
    FLastResultsRaw: string;
    FLastErrorMessage: string;
    FSSLVerifyPeer: Boolean;
    FSSLVerifyHost: Integer;
    FRequestTimeout: Integer;
    FConnectTimeout: Integer;
    FUnifiCookieName: string;
    FIdHTTP: TIdHTTP;
    FIdSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    procedure InitializeHTTPClient;
    procedure CreateXCSRFTokenHeader;
    function UpdateUnifiCookie: Boolean;
    function ExecuteRequest(const APath: string; AMethod: THTTPMethod = hmGET;
      APayload: TJSONObject = nil): string;
    function FetchResults(const APath: string; AMethod: THTTPMethod = hmGET;
      APayload: TJSONObject = nil; AReturnBoolean: Boolean = False;
      ALoginRequired: Boolean = True): TJSONValue;
    function FetchResultsBoolean(const APath: string;
      AMethod: THTTPMethod = hmGET; APayload: TJSONObject = nil;
      ALoginRequired: Boolean = True): Boolean;
    procedure SetPassword(const AValue: string);
    procedure SetUser(const AValue: string);
    procedure SetBaseUrl(const AValue: string);
    function ValidateBaseURL: Boolean;
    procedure SetSite(const AValue: string);
  protected
    procedure SetDefaults; virtual;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    // Core functionality
    function Login: Boolean;
    function Logout: Boolean;

    // Device management
    function ListDevices(const AMAC: string = ''): TJSONArray;
    function AdoptDevice(const AMAC: string): Boolean;
    function RestartDevice(const AMAC: string;
      const ARebootType: string = 'soft'): Boolean;
    function BlockSTA(const AMAC: string): Boolean;
    function UnblockSTA(const AMAC: string): Boolean;

    // Client management
    function ListClients(const AMAC: string = ''): TJSONArray;
    function AuthorizeGuest(const AMAC: string; AMinutes: Integer;
      AUp: Integer = 0; ADown: Integer = 0; AMegaBytes: Integer = 0;
      const AAPMAC: string = ''): Boolean;
    function UnauthorizeGuest(const AMAC: string): Boolean;

    // User/Admin management
    function ListAdmins: TJSONArray;
    function GetSelf: TJSONObject;
    function RevokeSuperAdmin(const AAdminId: string): Boolean;
    function RevokeAdminFromSite(const AAdminId: string;
      const ASiteName: string = ''): Boolean;
    function DeleteSuperAdmin(const AAdminId: string): Boolean;
    function DeleteUserFromAllSites(const AAdminId: string): Boolean;
    function FindAdminByEmail(const AEmail: string): TJSONObject;
    function FindAdminByName(const AName: string): TJSONObject;

    // Statistics & reporting
    function GetSiteStats(AStartTime: TDateTime = 0; AEndTime: TDateTime = 0)
      : TJSONArray;
    function GetClientStats(const AMAC: string; AStartTime: TDateTime = 0;
      AEndTime: TDateTime = 0): TJSONArray;

    function ListSites: TJSONArray;

    function SetWLANSettings(const AWLanId: string; const AXPassphrase: string;
      const AName: string = ''): Boolean;

    function ListWLANGroups: TJSONArray;
    function ListWLANConf(const AWLanId: string = ''): TJSONArray;

    function ListSettings: TJSONArray;
    function ListEvents(AHistoryHours: Integer = 720; AStart: Integer = 0;
      ALimit: Integer = 3000): TJSONArray;

    // Properties
    property BaseUrl: string read FBaseUrl write SetBaseUrl;
    property User: string read FUser write SetUser;
    property Password: string read FPassword write SetPassword;
    property Site: string read FSite write SetSite;
    property IsLoggedIn: Boolean read FIsLoggedIn;
    property IsUniFiOS: Boolean read FIsUniFiOS;
    property LastResultsRaw: string read FLastResultsRaw;
    property LastErrorMessage: string read FLastErrorMessage;
    property Cookies: string read FCookies write FCookies;
  end;

implementation

uses
  System.DateUtils, System.NetEncoding, System.StrUtils,
  Lazy.Utils;

{ TLZUniFiClient }

procedure TLZUniFiClient.SetDefaults;
begin
  FBaseUrl := UNIFI_BASEURL;
  FUser := '';
  FPassword := '';
  FSite := UNIFI_DEFAULT_SITE;
  FVersion := UNIFI_VERSION;
  FUnifiCookieName := UNIFI_COOKIE_NAME;

  FIsLoggedIn := False;
  FIsUniFiOS := False;
  FExecRetries := 0;

  FSSLVerifyPeer := False;
  FSSLVerifyHost := 0;
  FRequestTimeout := 30;
  FConnectTimeout := 10;
end;

constructor TLZUniFiClient.Create;
begin
  inherited Create;

  SetDefaults;

  InitializeHTTPClient;
end;

procedure TLZUniFiClient.CreateXCSRFTokenHeader;
var
  LCookieParts: TArray<string>;
  LJWTParts: TArray<string>;
  LJWTPayload: string;
  LJsonData: TJSONObject;
begin
  if (FCookies <> '') and (Pos('TOKEN', FCookies) > 0) then
  begin
    LCookieParts := FCookies.Split(['=']);
    if Length(LCookieParts) < 2 then
      Exit;

    LJWTParts := LCookieParts[1].Split(['.']);
    if Length(LJWTParts) < 2 then
      Exit;

    try
      LJWTPayload := TNetEncoding.Base64URL.Decode(LJWTParts[1]);
      LJsonData := TJSONObject.ParseJSONValue(LJWTPayload) as TJSONObject;
      try
        if LJsonData.GetValue('csrfToken') <> nil then
        begin
          FIdHTTP.Request.CustomHeaders.Values['x-csrf-token'] :=
            LJsonData.GetValue('csrfToken').Value;
        end;
      finally
        LJsonData.Free;
      end;
    except
      on E: Exception do
        Error('CSRF Token Error: ' + E.Message);
    end;
  end;
end;

destructor TLZUniFiClient.Destroy;
begin
  if FIsLoggedIn then
    Logout;

  FIdSSLIOHandler.Free;
  FIdHTTP.Free;
  inherited;
end;

procedure TLZUniFiClient.InitializeHTTPClient;
begin
  FIdHTTP := TIdHTTP.Create(nil);
  FIdSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  FIdHTTP.IOHandler := FIdSSLIOHandler;
  FIdHTTP.HandleRedirects := False;
  FIdHTTP.ConnectTimeout := FConnectTimeout * 1000;
  FIdHTTP.ReadTimeout := FRequestTimeout * 1000;

  FIdSSLIOHandler.SSLOptions.Mode := sslmClient;
  FIdSSLIOHandler.SSLOptions.VerifyMode := [];
  FIdSSLIOHandler.SSLOptions.VerifyDepth := 0;
  FIdSSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
  FIdSSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];

  if FSSLVerifyPeer then
  begin
    FIdSSLIOHandler.SSLOptions.VerifyMode := [sslvrfPeer];
    FIdSSLIOHandler.SSLOptions.VerifyDepth := FSSLVerifyHost;
  end;

  // Set default headers
  FIdHTTP.Request.CustomHeaders.Clear;
  FIdHTTP.Request.ContentType := 'application/json';
  FIdHTTP.Request.Accept := 'application/json';
end;

function TLZUniFiClient.Login: Boolean;
var
  LResponse: string;
  LLoginURL: string;
  LPayload: TJSONObject;
begin
  Result := False;

  // Skip if already logged in
  if UpdateUnifiCookie then
  begin
    FIsLoggedIn := True;
    Exit(True);
  end;

  try
    // Check if UniFi OS
    try
      FIdHTTP.Head(FBaseUrl + '/');
      FIsUniFiOS := (FIdHTTP.ResponseCode = 200);
    except
      on E: Exception do
        FIsUniFiOS := False;
    end;

    // Prepare login request
    if FIsUniFiOS then
      LLoginURL := '/api/auth/login'
    else
      LLoginURL := '/api/login';

    // Create login payload
    LPayload := TJSONObject.Create;
    try
      LPayload.AddPair('username', FUser);
      LPayload.AddPair('password', FPassword);

      LResponse := ExecuteRequest(LLoginURL, hmPOST, LPayload);

      if LResponse <> '' then
      begin
        FIsLoggedIn := True;
        Result := True;
      end;
    finally
      LPayload.Free;
    end;

  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Error('Login error: ' + E.Message);
    end;
  end;
end;

function TLZUniFiClient.Logout: Boolean;
var
  LLogoutURL: string;
begin
  Result := False;
  if not FIsLoggedIn then
    Exit;

  try
    if FIsUniFiOS then
    begin
      LLogoutURL := '/api/auth/logout';
      CreateXCSRFTokenHeader;
    end
    else
      LLogoutURL := '/logout';

    ExecuteRequest(LLogoutURL, hmPOST);

    FIsLoggedIn := False;
    FCookies := '';
    Result := True;

  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Error('Logout error: ' + E.Message);
    end;
  end;
end;

function TLZUniFiClient.ExecuteRequest(const APath: string;
  AMethod: THTTPMethod; APayload: TJSONObject): string;
var
  LURL: string;
  LRequestStream: TStringStream;
  LResponseStream: TStringStream;
  LMethod: THTTPMethod;
begin
  Result := '';

  LMethod := AMethod;

  // Build full URL
  LURL := FBaseUrl;

  if FIsUniFiOS then
  begin
    LURL := LURL + '/proxy/network' + APath;
  end
  else
  begin
    LURL := LURL + APath;
  end;

  try
    Debug('ExecuteRequest', LURL);
    LRequestStream := nil;
    LResponseStream := TStringStream.Create('', TEncoding.UTF8);
    try
      // Prepare request payload if provided
      if Assigned(APayload) then
      begin
        LRequestStream := TStringStream.Create(APayload.ToString,
          TEncoding.UTF8);

        if LMethod = hmGET then
          LMethod := hmPOST;
      end
      else
      begin
        LRequestStream := TStringStream.Create('');
      end;

      // Execute request based on method
      case LMethod of
        hmGET:
          FIdHTTP.Get(LURL, LResponseStream);

        hmPOST:
          FIdHTTP.Post(LURL, LRequestStream, LResponseStream);

        hmPUT:
          FIdHTTP.Put(LURL, LRequestStream, LResponseStream);

        hmDELETE:
          FIdHTTP.Delete(LURL);
      end;

      Result := LResponseStream.DataString;
      Debug('ExecuteRequest', Result);
      FLastResultsRaw := Result;

      // Handle 401 unauthorized
      if FIdHTTP.ResponseCode = 401 then
      begin
        if FExecRetries = 0 then
        begin
          FIsLoggedIn := False;
          FCookies := '';
          Inc(FExecRetries);

          if Login then
            Result := ExecuteRequest(APath, LMethod, APayload);
        end;
      end;

    finally
      LRequestStream.Free;
      LResponseStream.Free;
    end;

  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Error('Request error: ' + E.Message);
    end;
  end;
end;

function TLZUniFiClient.UpdateUnifiCookie: Boolean;
begin
  Result := False;
  if (FCookies <> '') then
  begin
    // Check if cookie contains JWT token for UniFi OS
    FIsUniFiOS := Pos('TOKEN', FCookies) > 0;
    Result := True;
  end;
end;

function TLZUniFiClient.ValidateBaseURL: Boolean;
begin
  if not TLZString.IsEmptyString(FBaseUrl) then
  begin
    if FBaseUrl[Length(FBaseUrl)] = '/' then
    begin
      Delete(FBaseUrl, Length(FBaseUrl), 1);
    end;
    if (Pos('http://', FBaseUrl) <> 1) and (Pos('https://', FBaseUrl) <> 1) then
    begin
      FBaseUrl := 'https://' + FBaseUrl;
    end;
  end;
  Result := not TLZString.IsEmptyString(FBaseUrl);
end;

function TLZUniFiClient.FetchResults(const APath: string; AMethod: THTTPMethod;
  APayload: TJSONObject; AReturnBoolean: Boolean; ALoginRequired: Boolean)
  : TJSONValue;
var
  LResponse: string;
  LJsonResponse: TJSONObject;
  LJsonValue: TJSONValue;
begin
  Result := nil;

  // Check login state if required
  if ALoginRequired and (not FIsLoggedIn) then
    Exit;

  try
    LResponse := ExecuteRequest(APath, AMethod, APayload);
    if LResponse = '' then
      Exit;

    LJsonResponse := TJSONObject.ParseJSONValue(LResponse) as TJSONObject;
    try
      if LJsonResponse.GetValue<string>('meta.rc') = 'ok' then
      begin
        FLastErrorMessage := '';
        if AReturnBoolean then
          Result := TJSONBool.Create(True)
        else
        begin
          LJsonValue := LJsonResponse.GetValue('data');
          if LJsonValue is TJSONArray then
            Result := LJsonValue.Clone as TJSONArray;
        end;
      end
      else if LJsonResponse.GetValue<string>('meta.rc') = 'error' then
      begin
        FLastErrorMessage := LJsonResponse.GetValue('meta.msg').Value;
        Error('Error: ' + FLastErrorMessage);
      end;

      // Handle v2 API responses
      if StartsStr('/v2/api/', APath) then
      begin
        if LJsonResponse.GetValue('errorCode') <> nil then
        begin
          FLastErrorMessage := LJsonResponse.GetValue('message').Value;
          Error('Error: ' + FLastErrorMessage);
          Exit;
        end;
        Result := LJsonResponse.Clone as TJSONValue;
      end;

    finally
      LJsonResponse.Free;
    end;

  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Error('FetchResults error: ' + E.Message);
    end;
  end;
end;

function TLZUniFiClient.FetchResultsBoolean(const APath: string;
  AMethod: THTTPMethod; APayload: TJSONObject; ALoginRequired: Boolean)
  : Boolean;
var
  LJSON: TJSONValue;
begin
  Result := False;
  LJSON := nil;
  try
    LJSON := FetchResults(APath, AMethod, APayload, True, ALoginRequired);
    if (Assigned(LJSON)) and (LJSON is TJSONBool) then
    begin
      Result := (LJSON as TJSONBool).AsBoolean;
    end;
  finally
    LJSON.Free;
  end;
end;

// User/Admin Management Methods

function TLZUniFiClient.ListAdmins: TJSONArray;
begin
  Result := FetchResults('/api/stat/admin') as TJSONArray;
end;

function TLZUniFiClient.GetSelf: TJSONObject;
var
  LJsonArray: TJSONArray;
begin
  Result := nil;
  LJsonArray := FetchResults('/api/self') as TJSONArray;
  try
    if Assigned(LJsonArray) and (LJsonArray.Count > 0) then
    begin
      Result := LJsonArray.Items[0].Clone as TJSONObject;
    end;
  finally
    LJsonArray.Free;
  end;
end;

function TLZUniFiClient.RevokeSuperAdmin(const AAdminId: string): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'revoke-super-admin');
    LPayload.AddPair('admin', AAdminId);
    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/sitemgr', [FSite]),
      hmPOST, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.RevokeAdminFromSite(const AAdminId: string;
  const ASiteName: string = ''): Boolean;
var
  LPayload: TJSONObject;
  LSiteName: string;
begin
  if ASiteName = '' then
    LSiteName := FSite
  else
    LSiteName := ASiteName;

  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'revoke-admin');
    LPayload.AddPair('admin', AAdminId);
    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/sitemgr', [LSiteName]),
      hmPOST, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.DeleteSuperAdmin(const AAdminId: string): Boolean;
var
  LSites: TJSONArray;
  LSite: TJSONObject;
  LSiteName: string;
  LFirstSite: Boolean;
  i: Integer;
begin
  Result := True;
  LFirstSite := True;

  try
    // Get all sites
    LSites := ListSites;
    try
      if not Assigned(LSites) then
      begin
        Result := False;
        Exit;
      end;

      // Iterate through all sites
      for i := 0 to LSites.Count - 1 do
      begin
        LSite := LSites.Items[i] as TJSONObject;
        LSiteName := LSite.GetValue<string>('name');

        // First revoke super admin privilege (only needed once)
        if LFirstSite then
        begin
          if not RevokeSuperAdmin(AAdminId) then
          begin
            Log('Warning: Failed to revoke super admin privilege');
          end;
          LFirstSite := False;
        end;

        // Revoke admin from this site
        if not RevokeAdminFromSite(AAdminId, LSiteName) then
        begin
          Log(Format('Warning: Failed to revoke admin from site: %s',
            [LSiteName]));
          Result := False;
        end;
      end;

    finally
      LSites.Free;
    end;

  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Error('DeleteSuperAdmin error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TLZUniFiClient.DeleteUserFromAllSites(const AAdminId: string): Boolean;
var
  LSites: TJSONArray;
  LSite: TJSONObject;
  LSiteName: string;
  i: Integer;
begin
  Result := True;

  try
    // Get all sites
    LSites := ListSites;
    try
      if not Assigned(LSites) then
      begin
        Result := False;
        Exit;
      end;

      // Iterate through all sites and revoke admin access
      for i := 0 to LSites.Count - 1 do
      begin
        LSite := LSites.Items[i] as TJSONObject;
        LSiteName := LSite.GetValue<string>('name');

        // Revoke admin from this site
        if RevokeAdminFromSite(AAdminId, LSiteName) then
        begin
          Log(Format('Revoke admin %s from site: %s', [AAdminId, LSiteName]));
        end
        else
        begin
          Log(Format('Warning: Failed to revoke admin %s from site: %s',
            [AAdminId, LSiteName]));
        end;
      end;

    finally
      LSites.Free;
    end;

  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Error('DeleteUserFromAllSites error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TLZUniFiClient.FindAdminByEmail(const AEmail: string): TJSONObject;
var
  LAdmins: TJSONArray;
  LAdmin: TJSONObject;
  LAdminEmail: string;
  i: Integer;
begin
  Result := nil;

  LAdmins := ListAdmins;
  try
    if not Assigned(LAdmins) then
      Exit;

    for i := 0 to LAdmins.Count - 1 do
    begin
      LAdmin := LAdmins.Items[i] as TJSONObject;
      if LAdmin.TryGetValue<string>('email', LAdminEmail) then
      begin
        if SameText(LAdminEmail, AEmail) then
        begin
          Result := LAdmin.Clone as TJSONObject;
          Break;
        end;
      end;
    end;

  finally
    LAdmins.Free;
  end;
end;

function TLZUniFiClient.FindAdminByName(const AName: string): TJSONObject;
var
  LAdmins: TJSONArray;
  LAdmin: TJSONObject;
  LAdminName: string;
  i: Integer;
begin
  Result := nil;

  LAdmins := ListAdmins;
  try
    if not Assigned(LAdmins) then
      Exit;

    for i := 0 to LAdmins.Count - 1 do
    begin
      LAdmin := LAdmins.Items[i] as TJSONObject;
      if LAdmin.TryGetValue<string>('name', LAdminName) then
      begin
        if SameText(LAdminName, AName) then
        begin
          Result := LAdmin.Clone as TJSONObject;
          Break;
        end;
      end;
    end;

  finally
    LAdmins.Free;
  end;
end;

// Original methods continue...

function TLZUniFiClient.ListDevices(const AMAC: string = ''): TJSONArray;
var
  LPath: string;
  LPayload: TJSONObject;
begin
  LPath := Format('/api/s/%s/stat/device', [FSite]);

  if AMAC <> '' then
  begin
    LPayload := TJSONObject.Create;
    try
      LPayload.AddPair('mac', LowerCase(AMAC));
      Result := FetchResults(LPath, hmGET, LPayload) as TJSONArray;
    finally
      LPayload.Free;
    end;
  end
  else
    Result := FetchResults(LPath) as TJSONArray;
end;

function TLZUniFiClient.AdoptDevice(const AMAC: string): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'adopt');
    LPayload.AddPair('mac', LowerCase(AMAC));
    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/devmgr', [FSite]),
      hmGET, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.RestartDevice(const AMAC: string;
  const ARebootType: string = 'soft'): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'restart');
    LPayload.AddPair('mac', LowerCase(AMAC));
    if ARebootType = 'hard' then
      LPayload.AddPair('reboot_type', 'hard');

    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/devmgr', [FSite]),
      hmGET, LPayload);
  finally
    LPayload.Free;
  end;
end;

procedure TLZUniFiClient.SetBaseUrl(const AValue: string);
begin
  FBaseUrl := AValue;
  ValidateBaseURL;
end;

procedure TLZUniFiClient.SetPassword(const AValue: string);
begin
  FPassword := AValue;
end;

procedure TLZUniFiClient.SetSite(const AValue: string);
begin
  FSite := Trim(AValue);
end;

procedure TLZUniFiClient.SetUser(const AValue: string);
begin
  FUser := AValue;
end;

function TLZUniFiClient.BlockSTA(const AMAC: string): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'block-sta');
    LPayload.AddPair('mac', LowerCase(AMAC));
    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/stamgr', [FSite]),
      hmGET, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.UnblockSTA(const AMAC: string): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'unblock-sta');
    LPayload.AddPair('mac', LowerCase(AMAC));
    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/stamgr', [FSite]),
      hmGET, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.ListClients(const AMAC: string = ''): TJSONArray;
var
  LPath: string;
begin
  if AMAC <> '' then
    LPath := Format('/api/s/%s/stat/sta/%s', [FSite, LowerCase(AMAC)])
  else
    LPath := Format('/api/s/%s/stat/sta', [FSite]);

  Result := FetchResults(LPath) as TJSONArray;
end;

function TLZUniFiClient.AuthorizeGuest(const AMAC: string; AMinutes: Integer;
  AUp: Integer = 0; ADown: Integer = 0; AMegaBytes: Integer = 0;
  const AAPMAC: string = ''): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'authorize-guest');
    LPayload.AddPair('mac', LowerCase(AMAC));
    LPayload.AddPair('minutes', TJSONNumber.Create(AMinutes));

    if AUp > 0 then
      LPayload.AddPair('up', TJSONNumber.Create(AUp));
    if ADown > 0 then
      LPayload.AddPair('down', TJSONNumber.Create(ADown));
    if AMegaBytes > 0 then
      LPayload.AddPair('bytes', TJSONNumber.Create(AMegaBytes));
    if AAPMAC <> '' then
      LPayload.AddPair('ap_mac', LowerCase(AAPMAC));

    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/stamgr', [FSite]),
      hmGET, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.UnauthorizeGuest(const AMAC: string): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('cmd', 'unauthorize-guest');
    LPayload.AddPair('mac', LowerCase(AMAC));
    Result := FetchResultsBoolean(Format('/api/s/%s/cmd/stamgr', [FSite]),
      hmGET, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.GetSiteStats(AStartTime: TDateTime = 0;
  AEndTime: TDateTime = 0): TJSONArray;
var
  LPayload: TJSONObject;
  LStartUnix: Int64;
  LEndUnix: Int64;
  LArray: TJSONArray;
begin

  // Convert TDateTime to Unix timestamp in milliseconds
  if AEndTime = 0 then
    LEndUnix := DateTimeToUnix(Now) * 1000
  else
    LEndUnix := DateTimeToUnix(AEndTime) * 1000;

  if AStartTime = 0 then
    LStartUnix := LEndUnix - (12 * 3600 * 1000)
    // Last 12 hours by default
  else
    LStartUnix := DateTimeToUnix(AStartTime) * 1000;

  LPayload := TJSONObject.Create;
  LArray := TJSONArray.Create;
  try
    LPayload.AddPair('start', TJSONNumber.Create(LStartUnix));
    LPayload.AddPair('end', TJSONNumber.Create(LEndUnix));

    // Add default attributes
    try
      LArray.Add('bytes');
      LArray.Add('wan-tx_bytes');
      LArray.Add('wan-rx_bytes');
      LArray.Add('wlan_bytes');
      LArray.Add('num_sta');
      LArray.Add('lan-num_sta');
      LArray.Add('wlan-num_sta');
      LArray.Add('time');
      LPayload.AddPair('attrs', LArray);
    except
      raise;
    end;

    Result := FetchResults(Format('/api/s/%s/stat/report/5minutes.site', [FSite]
      ), hmGET, LPayload) as TJSONArray;
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.GetClientStats(const AMAC: string;
  AStartTime: TDateTime = 0; AEndTime: TDateTime = 0): TJSONArray;
var
  LPayload: TJSONObject;
  LStartUnix: Int64;
  LEndUnix: Int64;
  LArray: TJSONArray;
begin
  Result := nil;
  if AMAC = '' then
    Exit;

  // Convert TDateTime to Unix timestamp in milliseconds
  if AEndTime = 0 then
    LEndUnix := DateTimeToUnix(Now) * 1000
  else
    LEndUnix := DateTimeToUnix(AEndTime) * 1000;

  if AStartTime = 0 then
    LStartUnix := LEndUnix - (12 * 3600 * 1000)
    // Last 12 hours by default
  else
    LStartUnix := DateTimeToUnix(AStartTime) * 1000;

  LPayload := TJSONObject.Create;
  LArray := TJSONArray.Create;
  try
    LPayload.AddPair('mac', LowerCase(AMAC));
    LPayload.AddPair('start', TJSONNumber.Create(LStartUnix));
    LPayload.AddPair('end', TJSONNumber.Create(LEndUnix));

    // Add default attributes
    try
      LArray.Add('time');
      LArray.Add('rx_bytes');
      LArray.Add('tx_bytes');
      LPayload.AddPair('attrs', LArray);
    except
      raise;
    end;

    Result := FetchResults(Format('/api/s/%s/stat/report/5minutes.user', [FSite]
      ), hmGET, LPayload) as TJSONArray;
  finally
    LPayload.Free;
    LArray.Free;
  end;
end;

function TLZUniFiClient.ListSites: TJSONArray;
begin
  Result := FetchResults('/api/self/sites') as TJSONArray;
end;

function TLZUniFiClient.SetWLANSettings(const AWLanId, AXPassphrase: string;
  const AName: string): Boolean;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('x_passphrase', Trim(AXPassphrase));
    if AName <> '' then
      LPayload.AddPair('name', Trim(AName));

    Result := FetchResultsBoolean(Format('/api/s/%s/rest/wlanconf/%s',
      [FSite, Trim(AWLanId)]), hmPUT, LPayload);
  finally
    LPayload.Free;
  end;
end;

function TLZUniFiClient.ListWLANGroups: TJSONArray;
begin
  Result := FetchResults(Format('/api/s/%s/list/wlangroup', [FSite]))
    as TJSONArray;
end;

function TLZUniFiClient.ListWLANConf(const AWLanId: string): TJSONArray;
begin
  Result := FetchResults(Format('/api/s/%s/rest/wlanconf/%s',
    [FSite, Trim(AWLanId)])) as TJSONArray;
end;

function TLZUniFiClient.ListSettings: TJSONArray;
begin
  Result := FetchResults(Format('/api/s/%s/get/setting', [FSite]))
    as TJSONArray;
end;

function TLZUniFiClient.ListEvents(AHistoryHours: Integer = 720;
  AStart: Integer = 0; ALimit: Integer = 3000): TJSONArray;
var
  LPayload: TJSONObject;
begin
  LPayload := TJSONObject.Create;
  try
    LPayload.AddPair('_sort', '-time');
    LPayload.AddPair('within', TJSONNumber.Create(AHistoryHours));
    LPayload.AddPair('type', TJSONNull.Create);
    LPayload.AddPair('_start', TJSONNumber.Create(AStart));
    LPayload.AddPair('_limit', TJSONNumber.Create(ALimit));

    Result := FetchResults(Format('/api/s/%s/stat/event', [FSite]), hmGET,
      LPayload) as TJSONArray;
  finally
    LPayload.Free;
  end;
end;

end.
