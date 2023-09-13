unit Lazy.RESTClient;

interface

uses
  Lazy.Types, Lazy.REST.Types, System.SysUtils, System.Types, System.Classes,
  System.Variants, REST.Client, REST.Types, REST.Json, System.Json;

type
  TLZRESTResponse = reference to procedure(ASender: TObject; ASuccess: boolean;
    AMessage: string; ARESTResponse: TRESTResponse; ACustomData: string);

  TLZProcessRESTResponse = reference to procedure(ARESTRequest: TRESTRequest;
    ARESTResponse: TRESTResponse; ASuccess: boolean; AMessage: string;
    AJSON: string; ACustomData: string);
  TLZBeforeRESTRequest = reference to procedure(ARESTRequest: TRESTRequest;
    ACustomData: string);
  TLZAfterRESTRequest = reference to procedure(ARESTRequest: TRESTRequest;
    ACustomData: string);

  TLZAsyncState = (lasDefault, lasTrue, lasFalse);

  TLZRESTClientBase = class(TLZObject)
  private
    FDefaultAsync: boolean;
    procedure SetDefaultAsync(const Value: boolean);
  protected
    procedure CreateObjects; virtual;
    procedure DestroyObjects; virtual;
    procedure SetDefaults; virtual;
    function GetBaseURL: string; virtual;
    function ProcessURLVariables(AURL: string): string; virtual;
    procedure AfterCreateRestClient(ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse); virtual;
    procedure BeforeDestroyRestClient(ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse); virtual;
    procedure Execute(ABaseURL: string; AResource: string; ABody: string;
      ALazyProcessRESTResponse: TLZProcessRESTResponse;
      ALazyBeforeRESTRequest: TLZBeforeRESTRequest = nil;
      ALazyAfterRESTRequest: TLZAfterRESTRequest = nil;
      AMethod: TRESTRequestMethod = TRESTRequestMethod.rmGET;
      ACustomData: string = ''; AASync: TLZAsyncState = lasDefault); overload;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property DefaultAsync: boolean read FDefaultAsync write SetDefaultAsync;
  end;

  TLZRESTClient = class(TLZRESTClientBase)
  protected
    procedure Execute(AResource, ABody: string;
      ALazyRESTResponse: TLZRESTResponse; AMethod: TRESTRequestMethod;
      ACustomData: string; AASync: TLZAsyncState); overload;
    procedure BeforeRESTRequest(ARESTRequest: TRESTRequest;
      ACustomData: string); virtual;
    procedure AfterRESTRequest(ARESTRequest: TRESTRequest;
      ACustomData: string); virtual;
  public
    procedure Get(AResource: string; ALazyRESTResponse: TLZRESTResponse;
      ACustomData: string = ''; AASync: TLZAsyncState = lasDefault);
    procedure Post(AResource: string; ABody: string;
      ALazyRESTResponse: TLZRESTResponse; ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault);
    procedure Put(AResource: string; ABody: string;
      ALazyRESTResponse: TLZRESTResponse; ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault);
    procedure Delete(AResource: string; ALazyRESTResponse: TLZRESTResponse;
      ACustomData: string = ''; AASync: TLZAsyncState = lasDefault);
  end;

  TLZRESTClientBasicAuth = class(TLZRESTClient)
  private
    FPassword: string;
    FUsername: string;
    procedure SetPassword(const Value: string);
    procedure SetUsername(const Value: string);
  protected
    procedure AfterCreateRestClient(ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse); override;
    procedure BeforeDestroyRestClient(ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse); override;
  public
    property Username: string read FUsername write SetUsername;
    property Password: string read FPassword write SetPassword;
  end;

  TOnLazyBrowserLoginRequest = procedure(ASender: TObject; AURL: string;
    AConnection: TLZOAuth2Connection; AToken: TLZOAuth2Token) of object;

  TOnLazyOAuth2TokenRequestComplete = procedure(ASender: TObject;
    ASuccess: boolean; AMessage: string; AToken: TLZOAuth2Token) of object;

  TLZRESTClientOAuth2 = class(TLZRESTClient)
  private
    FOnLazyBrowserLoginRequest: TOnLazyBrowserLoginRequest;
    FOnLazyOAuth2TokenRequestComplete: TOnLazyOAuth2TokenRequestComplete;
    FConnection: TLZOAuth2Connection;
    FToken: TLZOAuth2Token;
    function GetAuthenticated: boolean;
    procedure SetOnLazyBrowserLoginRequest(const Value
      : TOnLazyBrowserLoginRequest);
    procedure SetOnLazyOAuth2TokenRequestComplete
      (const Value: TOnLazyOAuth2TokenRequestComplete);
  protected
    procedure CreateObjects; override;
    procedure DestroyObjects; override;
    function ProcessURLVariables(AURL: string): string; override;
    function GetBaseURL: string; override;
    function GetOAuth2ConnectionClass: TLZOAuth2ConnectionClass; virtual;
    procedure BeforeRESTRequest(ARESTRequest: TRESTRequest;
      ACustomData: string); override;
    function GetConnection: TLZOAuth2Connection; virtual;
  public
    procedure Authenticate(AASync: TLZAsyncState = lasDefault);
    procedure ClearAuthentication;
    procedure RequestToken(AASync: TLZAsyncState = lasDefault);
    property IsAuthenticated: boolean read GetAuthenticated;
    property Token: TLZOAuth2Token read FToken;
    property OnBrowserLoginRequest: TOnLazyBrowserLoginRequest
      read FOnLazyBrowserLoginRequest write SetOnLazyBrowserLoginRequest;
    property OnOAuth2TokenRequestComplete: TOnLazyOAuth2TokenRequestComplete
      read FOnLazyOAuth2TokenRequestComplete
      write SetOnLazyOAuth2TokenRequestComplete;
  end;

implementation

uses Lazy.Utils, System.NetEncoding, System.Net.URLClient, System.DateUtils,
  REST.Authenticator.Basic;

{ TLZRESTClient }

procedure TLZRESTClientBase.AfterCreateRestClient(ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse);
begin

end;

procedure TLZRESTClientBase.BeforeDestroyRestClient(ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse);
begin

end;

constructor TLZRESTClientBase.Create;
begin
  inherited;
  FDefaultAsync := True;
  SetDefaults;
  CreateObjects;
end;

procedure TLZRESTClientBase.CreateObjects;
begin

end;

destructor TLZRESTClientBase.Destroy;
begin
  try
    DestroyObjects;
  finally
    inherited;
  end;
end;

procedure TLZRESTClientBase.DestroyObjects;
begin

end;

procedure TLZRESTClientBase.Execute(ABaseURL, AResource, ABody: string;
  ALazyProcessRESTResponse: TLZProcessRESTResponse;
  ALazyBeforeRESTRequest: TLZBeforeRESTRequest;
  ALazyAfterRESTRequest: TLZAfterRESTRequest; AMethod: TRESTRequestMethod;
  ACustomData: string; AASync: TLZAsyncState);
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LExecuteProc: TProc;
  LAsync: boolean;
begin
  LRESTClient := TRESTClient.Create(ProcessURLVariables(ABaseURL));
  LRESTRequest := TRESTRequest.Create(nil);
  LRESTResponse := TRESTResponse.Create(nil);

  case AASync of
    lasTrue:
      LAsync := True;
    lasFalse:
      LAsync := False;
  else
    LAsync := FDefaultAsync;
  end;

  LRESTClient.RaiseExceptionOn500 := False;
  LRESTClient.SynchronizedEvents := not LAsync;
  LRESTRequest.SynchronizedEvents := not LAsync;
  LRESTRequest.Client := LRESTClient;
  LRESTRequest.Response := LRESTResponse;
  LRESTRequest.ClearBody;

  AfterCreateRestClient(LRESTClient, LRESTRequest, LRESTResponse);

  LRESTRequest.Method := AMethod;
  LRESTRequest.Resource := ProcessURLVariables(AResource);

  if Assigned(ALazyBeforeRESTRequest) then
    ALazyBeforeRESTRequest(LRESTRequest, ACustomData);

  if not TLZString.IsEmptyString(ABody) then
  begin
    Debug('Execute', 'Body: ' + ABody);
    LRESTRequest.AddBody(ABody, CONTENTTYPE_APPLICATION_JSON);
  end;

  Debug('Execute', LRESTClient.BaseURL + '/' + LRESTRequest.Resource);

  LExecuteProc := procedure()
    var
      LSuccess: boolean;
      LJSON, LMessage: string;
    begin
      LMessage := '';
      LJSON := '';
      try
        try
          Debug('Execute',
            Format('Response: StatusCode: %d, StatusText: %s, Content: %s',
            [LRESTRequest.Response.StatusCode, LRESTRequest.Response.StatusText,
            LRESTRequest.Response.Content]));

          LSuccess := (LRESTRequest.Response.StatusCode >= 200) and
            (LRESTRequest.Response.StatusCode < 300);
          if LSuccess then
          begin
            LJSON := LRESTRequest.Response.JSONValue.Format;
          end
          else
          begin
            LMessage := Format('{ "errorCode":%d, "errorMessage":"%s %s" }',
              [LRESTRequest.Response.StatusCode,
              LRESTRequest.Response.StatusText, LRESTRequest.Response.Content]);
          end;
        except
          on E: Exception do
          begin
            Error(E);
            LMessage := Format('{ "exception":"%s" }', [E.Message]);
            LSuccess := False;
          end;
        end;

        try
          if Assigned(ALazyProcessRESTResponse) then
            ALazyProcessRESTResponse(LRESTRequest, LRESTResponse, LSuccess,
              LMessage, LJSON, ACustomData);
        except
          on E: Exception do
          begin
            Error(E);
          end;
        end;

        try
          if Assigned(ALazyAfterRESTRequest) then
            ALazyAfterRESTRequest(LRESTRequest, ACustomData);
        except
          on E: Exception do
          begin
            Error(E);
          end;
        end;
      finally
        BeforeDestroyRestClient(LRESTClient, LRESTRequest, LRESTResponse);

        if Assigned(LRESTResponse) then
        begin
          FreeAndNil(LRESTResponse);
        end;

        if Assigned(LRESTRequest) then
        begin
          FreeAndNil(LRESTRequest);
        end;

        if Assigned(LRESTClient) then
        begin
          FreeAndNil(LRESTClient);
        end;
      end;
    end;

  if LAsync then
  begin
    LRESTRequest.ExecuteASync(LExecuteProc);
  end
  else
  begin
    LRESTRequest.Execute;
    LExecuteProc;
  end;

end;

function TLZRESTClientBase.GetBaseURL: string;
begin

end;

function TLZRESTClientBase.ProcessURLVariables(AURL: string): string;
begin
  Result := AURL;
end;

procedure TLZRESTClientBase.SetDefaultAsync(const Value: boolean);
begin
  FDefaultAsync := Value;
end;

procedure TLZRESTClientBase.SetDefaults;
begin

end;

procedure TLZRESTClient.Execute(AResource, ABody: string;
  ALazyRESTResponse: TLZRESTResponse; AMethod: TRESTRequestMethod;
  ACustomData: string; AASync: TLZAsyncState);
begin
  Execute(GetBaseURL, AResource, ABody,
    procedure(ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse;
      ASuccess: boolean; AMessage: string; AJSON: string; ACustomData: string)
    begin
      if Assigned(ALazyRESTResponse) then
        ALazyRESTResponse(Self, ASuccess, AMessage, ARESTResponse, ACustomData);
    end,
    procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
      BeforeRESTRequest(ARESTRequest, ACustomData);
    end,
    procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
      AfterRESTRequest(ARESTRequest, ACustomData);
    end, AMethod, ACustomData, AASync);
end;

procedure TLZRESTClient.Get(AResource: string;
ALazyRESTResponse: TLZRESTResponse; ACustomData: string; AASync: TLZAsyncState);
begin
  Execute(AResource, '', ALazyRESTResponse, TRESTRequestMethod.rmGET,
    ACustomData, AASync);
end;

procedure TLZRESTClient.Post(AResource, ABody: string;
ALazyRESTResponse: TLZRESTResponse; ACustomData: string; AASync: TLZAsyncState);
begin
  Execute(AResource, ABody, ALazyRESTResponse, TRESTRequestMethod.rmPOST,
    ACustomData, AASync);
end;

procedure TLZRESTClient.Put(AResource, ABody: string;
ALazyRESTResponse: TLZRESTResponse; ACustomData: string; AASync: TLZAsyncState);
begin
  Execute(AResource, ABody, ALazyRESTResponse, TRESTRequestMethod.rmPUT,
    ACustomData, AASync);
end;

procedure TLZRESTClient.AfterRESTRequest(ARESTRequest: TRESTRequest;
ACustomData: string);
begin

end;

procedure TLZRESTClient.BeforeRESTRequest(ARESTRequest: TRESTRequest;
ACustomData: string);
begin

end;

procedure TLZRESTClient.Delete(AResource: string;
ALazyRESTResponse: TLZRESTResponse; ACustomData: string; AASync: TLZAsyncState);
begin
  Execute(AResource, '', ALazyRESTResponse, TRESTRequestMethod.rmDELETE,
    ACustomData, AASync);
end;

{ TLZRESTClientBasicAuth }

procedure TLZRESTClientBasicAuth.AfterCreateRestClient(ARESTClient: TRESTClient;
ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse);
var
  LBasicAuth: THTTPBasicAuthenticator;
begin
  LBasicAuth := THTTPBasicAuthenticator.Create(FUsername, FPassword);
  ARESTClient.Authenticator := LBasicAuth;
end;

procedure TLZRESTClientBasicAuth.BeforeDestroyRestClient
  (ARESTClient: TRESTClient; ARESTRequest: TRESTRequest;
ARESTResponse: TRESTResponse);
begin
  inherited;
  if Assigned(ARESTClient.Authenticator) then
  begin
    ARESTClient.Authenticator.Free;
  end;
end;

procedure TLZRESTClientBasicAuth.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TLZRESTClientBasicAuth.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

{ TLZRESTClientOAuth2 }

procedure TLZRESTClientOAuth2.Authenticate(AASync: TLZAsyncState);
var
  LURL: string;
begin
  if TLZString.IsEmptyString(Token.AuthToken) or
    TLZString.IsEmptyString(Token.RefreshToken) then
  begin
    LURL := GetConnection.AuthorizeEndPoint + '?client_id=' + GetConnection.ClientId +
      '&response_type=code' + '&redirect_uri=' + TNetEncoding.URL.Encode
      (GetConnection.RedirectURL) + '&response_mode=query' + '&state=1' +
      '&scope=openid';
    LURL := ProcessURLVariables(LURL);
    if Assigned(FOnLazyBrowserLoginRequest) then
    begin
      FOnLazyBrowserLoginRequest(Self, LURL, GetConnection, Token);
    end
    else
    begin
      raise ERESTException.CreateFmt
        ('Interactive login required for URL "%s" or provide valid Auth and Refresh tokens.',
        [LURL]);
    end;
  end
  else
  begin
    RequestToken(AASync);
  end;

end;

procedure TLZRESTClientOAuth2.BeforeRESTRequest(ARESTRequest: TRESTRequest;
ACustomData: string);
begin
  inherited;
  if not TLZString.IsEmptyString(Token.AuthToken) then
  begin
    ARESTRequest.Params.AddItem('Authorization', 'Bearer ' + Token.AuthToken,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  end;
  ARESTRequest.Params.AddItem('Content-Type', 'application/json',
    TRESTRequestParameterKind.pkHTTPHEADER);
end;

procedure TLZRESTClientOAuth2.ClearAuthentication;
begin
  Token.Reset;
end;

procedure TLZRESTClientOAuth2.CreateObjects;
begin
  inherited;
  FConnection := GetOAuth2ConnectionClass.Create;
  FToken := TLZOAuth2Token.Create;
end;

procedure TLZRESTClientOAuth2.DestroyObjects;
begin
  try
    FreeAndNil(FConnection);
    FreeAndNil(FToken);
  finally
    inherited;
  end;
end;

function TLZRESTClientOAuth2.GetAuthenticated: boolean;
begin
  Result := (not TLZString.IsEmptyString(Token.AuthToken)) and
    (Token.ExpiresIn >= Now);
end;

function TLZRESTClientOAuth2.GetBaseURL: string;
begin
  Result := FConnection.RESTEndPoint;
end;

function TLZRESTClientOAuth2.GetConnection: TLZOAuth2Connection;
begin
  Result := FConnection;
end;

function TLZRESTClientOAuth2.GetOAuth2ConnectionClass: TLZOAuth2ConnectionClass;
begin
  Result := TLZOAuth2Connection;
end;

function TLZRESTClientOAuth2.ProcessURLVariables(AURL: string): string;
begin
  Result := inherited;
  Result := FConnection.ProcessURLVariables(Result);
  Debug('ProcessURLVariables', AURL + ' => ' + Result);
end;

procedure TLZRESTClientOAuth2.RequestToken(AASync: TLZAsyncState);
var
  LProcess: TLZProcessRESTResponse;
  LBefore: TLZBeforeRESTRequest;
  LAfter: TLZAfterRESTRequest;
begin
  LProcess :=
      procedure(ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse;
    ASuccess: boolean; AMessage: string; AJSON: string; ACustomData: string)
    var
      LExpiresSeconds: integer;
    begin
      if ASuccess then
      begin
        Token.AuthToken := ARESTRequest.Response.JSONValue.GetValue<String>
          ('access_token');

        Token.RefreshToken := ARESTRequest.Response.JSONValue.GetValue<String>
          ('refresh_token');

        LExpiresSeconds := ARESTRequest.Response.JSONValue.GetValue<integer>
          ('expires_in');
        if (LExpiresSeconds > -1) then
          Token.ExpiresIn := IncSecond(Now, LExpiresSeconds)
        else
          Token.ExpiresIn := 0;
      end;
      if Assigned(FOnLazyOAuth2TokenRequestComplete) then
        FOnLazyOAuth2TokenRequestComplete(Self, ASuccess, AMessage, Token);
    end;

  LBefore := procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
      if TLZString.IsEmptyString(Token.AuthToken) or
        TLZString.IsEmptyString(Token.RefreshToken) then
      begin

        ARESTRequest.Params.AddItem('client_id', GetConnection.ClientId,
          TRESTRequestParameterKind.pkQUERY);

        ARESTRequest.Params.AddItem('grant_type', 'authorization_code',
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_id', GetConnection.ClientId,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('code', Token.AuthCode,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_secret', GetConnection.ClientSecret,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('scope', GetConnection.Scope,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('redirect_uri', GetConnection.RedirectURL,
          TRESTRequestParameterKind.pkREQUESTBODY);
      end
      else
      begin

        // Use refresh token
        ARESTRequest.Params.AddItem('client_id', GetConnection.ClientId,
          TRESTRequestParameterKind.pkQUERY);
        ARESTRequest.Params.AddItem('code', Token.AuthCode,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('grant_type', 'refresh_token',
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_id', GetConnection.ClientId,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_secret', GetConnection.ClientSecret,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('refresh_token', Token.RefreshToken,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('scope', GetConnection.Scope,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('redirect_uri', GetConnection.RedirectURL,
          TRESTRequestParameterKind.pkREQUESTBODY);
      end;
    end;

  LAfter := procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
    end;

  inherited Execute(GetConnection.TokenEndPoint, '', '', LProcess, LBefore, LAfter,
    TRESTRequestMethod.rmPOST, '', AASync);

end;

procedure TLZRESTClientOAuth2.SetOnLazyBrowserLoginRequest
  (const Value: TOnLazyBrowserLoginRequest);
begin
  FOnLazyBrowserLoginRequest := Value;
end;

procedure TLZRESTClientOAuth2.SetOnLazyOAuth2TokenRequestComplete
  (const Value: TOnLazyOAuth2TokenRequestComplete);
begin
  FOnLazyOAuth2TokenRequestComplete := Value;
end;

end.
