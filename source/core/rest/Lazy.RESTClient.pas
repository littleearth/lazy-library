unit Lazy.RESTClient;

interface

uses
  Lazy.Types, Lazy.REST.Types, System.SysUtils, System.Types, System.Classes,
  System.Variants, REST.Client, REST.Types, System.Json;

type
  TLZRESTRequest = reference to procedure(ASender: TObject;
    ARESTRequest: TRESTRequest; ACustomData: string);
  TLZRESTResponse = reference to procedure(ASender: TObject; ASuccess: boolean;
    AMessage: string; ARESTResponse: TRESTResponse; ACustomData: string);

  TLZProcessRESTResponse = reference to procedure(ARESTRequest: TRESTRequest;
    ARESTResponse: TRESTResponse; ASuccess: boolean; AMessage: string;
    AJSON: string; ACustomData: string);
  TLZProcessRESTRequest = reference to procedure(ARESTRequest: TRESTRequest;
    ACompletionProc: TProc; AASync: boolean; ACustomData: string);
  TLZBeforeRESTRequest = reference to procedure(ARESTRequest: TRESTRequest;
    ACustomData: string);
  TLZAfterRESTRequest = reference to procedure(ARESTRequest: TRESTRequest;
    ACustomData: string);

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
    procedure AfterCreateRestClient(
      ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest;
      ARESTResponse: TRESTResponse); virtual;
    procedure BeforeDestroyRestClient(
      ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest;
      ARESTResponse: TRESTResponse); virtual;
    procedure Execute(
      ABaseURL: string;
      AResource: string;
      ABody: string;
      ALazyProcessRESTResponse: TLZProcessRESTResponse;
      ALazyBeforeRESTRequest: TLZBeforeRESTRequest = nil;
      ALazyAfterRESTRequest: TLZAfterRESTRequest = nil;
      AMethod: TRESTRequestMethod = TRESTRequestMethod.rmGET;
      ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault;
      ALazyProcessRESTRequest: TLZProcessRESTRequest = nil;
      AFailOnInvalidJSON: boolean = true); overload;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property DefaultAsync: boolean read FDefaultAsync write SetDefaultAsync;
  end;

  TLZRESTClient = class(TLZRESTClientBase)
  protected
    procedure Execute(
      AResource, ABody: string;
      ALazyRESTResponse: TLZRESTResponse;
      AMethod: TRESTRequestMethod;
      ACustomData: string;
      AASync: TLZAsyncState;
      AFailOnInvalidJSON: boolean = true); overload;
    procedure Execute(
      AResource, ABody: string;
      ALazyRESTRequest: TLZRESTRequest;
      ALazyRESTResponse: TLZRESTResponse;
      AMethod: TRESTRequestMethod;
      ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault;
      AFailOnInvalidJSON: boolean = true); overload;
    procedure BeforeRESTRequest(
      ARESTRequest: TRESTRequest;
      ACustomData: string); virtual;
    procedure AfterRESTRequest(
      ARESTRequest: TRESTRequest;
      ACustomData: string); virtual;
  public
    procedure Get(
      AResource: string;
      ALazyRESTResponse: TLZRESTResponse;
      ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault;
      AFailOnInvalidJSON: boolean = true);
    procedure Post(
      AResource: string;
      ABody: string;
      ALazyRESTResponse: TLZRESTResponse;
      ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault);
    procedure Put(
      AResource: string;
      ABody: string;
      ALazyRESTResponse: TLZRESTResponse;
      ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault);
    procedure Delete(
      AResource: string;
      ALazyRESTResponse: TLZRESTResponse;
      ACustomData: string = '';
      AASync: TLZAsyncState = lasDefault);
  end;

  TLZRESTClientBasicAuth = class(TLZRESTClient)
  private
    FPassword: string;
    FUsername: string;
    procedure SetPassword(const Value: string);
    procedure SetUsername(const Value: string);
  protected
    procedure AfterCreateRestClient(
      ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest;
      ARESTResponse: TRESTResponse); override;
    procedure BeforeDestroyRestClient(
      ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest;
      ARESTResponse: TRESTResponse); override;
  public
    property Username: string read FUsername write SetUsername;
    property Password: string read FPassword write SetPassword;
  end;

  TLZRESTClientOAuth1Base = class(TLZRESTClient)
  private
    FConsumerSecret: string;
    FIPImplementationID: string;
    FSigningClassName: string;
    FRequestTokenSecret: string;
    FAccessTokenEndpoint: string;
    FAccessToken: string;
    FAuthenticationEndpoint: string;
    FRequestTokenEndpoint: string;
    FConsumerKey: string;
    FVerifierPIN: string;
    FCallbackEndpoint: string;
    FRequestToken: string;
    FAccessTokenSecret: string;
    procedure SetAccessToken(const Value: string);
    procedure SetAccessTokenEndpoint(const Value: string);
    procedure SetAccessTokenSecret(const Value: string);
    procedure SetAuthenticationEndpoint(const Value: string);
    procedure SetCallbackEndpoint(const Value: string);
    procedure SetConsumerKey(const Value: string);
    procedure SetConsumerSecret(const Value: string);
    procedure SetIPImplementationID(const Value: string);
    procedure SetRequestToken(const Value: string);
    procedure SetRequestTokenEndpoint(const Value: string);
    procedure SetRequestTokenSecret(const Value: string);
    procedure SetSigningClassName(const Value: string);
    procedure SetVerifierPIN(const Value: string);
  protected
    procedure AfterCreateRestClient(
      ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest;
      ARESTResponse: TRESTResponse); override;
    procedure BeforeDestroyRestClient(
      ARESTClient: TRESTClient;
      ARESTRequest: TRESTRequest;
      ARESTResponse: TRESTResponse); override;
  public
    property AccessToken: string read FAccessToken write SetAccessToken;
    property AccessTokenSecret: string read FAccessTokenSecret
      write SetAccessTokenSecret;
    property RequestToken: string read FRequestToken write SetRequestToken;
    property RequestTokenSecret: string read FRequestTokenSecret
      write SetRequestTokenSecret;
    property AccessTokenEndpoint: string read FAccessTokenEndpoint
      write SetAccessTokenEndpoint;
    property RequestTokenEndpoint: string read FRequestTokenEndpoint
      write SetRequestTokenEndpoint;
    property AuthenticationEndpoint: string read FAuthenticationEndpoint
      write SetAuthenticationEndpoint;
    property CallbackEndpoint: string read FCallbackEndpoint
      write SetCallbackEndpoint;
    property ConsumerKey: string read FConsumerKey write SetConsumerKey;
    property ConsumerSecret: string read FConsumerSecret
      write SetConsumerSecret;
    property SigningClassName: string read FSigningClassName
      write SetSigningClassName;
    property VerifierPIN: string read FVerifierPIN write SetVerifierPIN;
    property IPImplementationID: string read FIPImplementationID
      write SetIPImplementationID;
  end;

  TOnLazyBrowserLoginRequest = procedure(
    ASender: TObject;
    AURL: string;
    AConnection: TLZOAuth2Connection;
    AToken: TLZOAuth2Token;
    AUserDataFolder: string) of object;

  TOnLazyOAuth2TokenRequestComplete = procedure(
    ASender: TObject;
    ASuccess: boolean;
    AMessage: string;
    AToken: TLZOAuth2Token) of object;

  TLZRESTClientOAuth2Base = class(TLZRESTClient)
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
    function GetOAuth2TokenClass: TLZOAuth2TokenClass; virtual;
    procedure BeforeRESTRequest(
      ARESTRequest: TRESTRequest;
      ACustomData: string); override;
    procedure BeforeTokenRequest(
      ARESTRequest: TRESTRequest;
      ACustomData: string); virtual;
    function GetConnection: TLZOAuth2Connection; virtual;
  public
    procedure Authenticate(
      AASync: TLZAsyncState = lasDefault;
      ABrowserUserDataFolder: string = '');
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

  TLZRESTClientOAuth2 = class(TLZRESTClientOAuth2Base)
  public
    property Connection: TLZOAuth2Connection read GetConnection;
  end;

implementation

uses Lazy.Utils, System.NetEncoding, System.DateUtils,
  REST.Authenticator.Basic, REST.Authenticator.OAuth;

{ TLZRESTClient }

procedure TLZRESTClientBase.AfterCreateRestClient(
  ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest;
  ARESTResponse: TRESTResponse);
begin

end;

procedure TLZRESTClientBase.BeforeDestroyRestClient(
  ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest;
  ARESTResponse: TRESTResponse);
begin

end;

constructor TLZRESTClientBase.Create;
begin
  inherited;
  FDefaultAsync := true;
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

procedure TLZRESTClientBase.Execute(
  ABaseURL, AResource, ABody: string;
  ALazyProcessRESTResponse: TLZProcessRESTResponse;
  ALazyBeforeRESTRequest: TLZBeforeRESTRequest;
  ALazyAfterRESTRequest: TLZAfterRESTRequest;
  AMethod: TRESTRequestMethod;
  ACustomData: string;
  AASync: TLZAsyncState;
  ALazyProcessRESTRequest: TLZProcessRESTRequest;
  AFailOnInvalidJSON: boolean);
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LRESTContentType: TRESTContentType;
  LExecuteProc: TProc;
  LAsync: boolean;
begin
  LRESTClient := TRESTClient.Create(ProcessURLVariables(ABaseURL));
  LRESTRequest := TRESTRequest.Create(nil);
  LRESTResponse := TRESTResponse.Create(nil);

  case AASync of
    lasTrue:
      LAsync := true;
    lasFalse:
      LAsync := false;
  else
    LAsync := FDefaultAsync;
  end;
  // LRESTClient.ContentType := ctAPPLICATION_JSON;
  // LRESTClient.Accept := '*/*';
  LRESTClient.RaiseExceptionOn500 := false;
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

  Debug('Execute', LRESTClient.BaseURL + '/' + LRESTRequest.Resource + ' Body: '
    + TLZString.StringCleaner(LRESTRequest.GetFullRequestBody, true, true) +
    ', Content Type: ' + LRESTContentType);

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
            if Assigned(LRESTRequest.Response.JSONValue) then
            begin
              LJSON := LRESTRequest.Response.JSONValue.ToString;
            end
            else
            begin
              if AFailOnInvalidJSON then
              begin
                LSuccess := false;
                LMessage := 'Response is not a JSON value';
              end;
            end;
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
            LSuccess := false;
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
  if Assigned(ALazyProcessRESTRequest) then
  begin
    ALazyProcessRESTRequest(LRESTRequest, LExecuteProc, LAsync, ACustomData);
  end
  else
  begin
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

procedure TLZRESTClient.Execute(
  AResource, ABody: string;
  ALazyRESTResponse: TLZRESTResponse;
  AMethod: TRESTRequestMethod;
  ACustomData: string;
  AASync: TLZAsyncState;
  AFailOnInvalidJSON: boolean);
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
    end, AMethod, ACustomData, AASync, nil, AFailOnInvalidJSON);
end;

procedure TLZRESTClient.Get(
  AResource: string;
  ALazyRESTResponse: TLZRESTResponse;
  ACustomData: string;
  AASync: TLZAsyncState;
  AFailOnInvalidJSON: boolean);
begin
  Execute(AResource, '', ALazyRESTResponse, TRESTRequestMethod.rmGET,
    ACustomData, AASync, AFailOnInvalidJSON);
end;

procedure TLZRESTClient.Post(
  AResource, ABody: string;
  ALazyRESTResponse: TLZRESTResponse;
  ACustomData: string;
  AASync: TLZAsyncState);
begin
  Execute(AResource, ABody, ALazyRESTResponse, TRESTRequestMethod.rmPOST,
    ACustomData, AASync);
end;

procedure TLZRESTClient.Put(
  AResource, ABody: string;
  ALazyRESTResponse: TLZRESTResponse;
  ACustomData: string;
  AASync: TLZAsyncState);
begin
  Execute(AResource, ABody, ALazyRESTResponse, TRESTRequestMethod.rmPUT,
    ACustomData, AASync);
end;

procedure TLZRESTClient.AfterRESTRequest(
  ARESTRequest: TRESTRequest;
  ACustomData: string);
begin

end;

procedure TLZRESTClient.BeforeRESTRequest(
  ARESTRequest: TRESTRequest;
  ACustomData: string);
begin

end;

procedure TLZRESTClient.Delete(
  AResource: string;
  ALazyRESTResponse: TLZRESTResponse;
  ACustomData: string;
  AASync: TLZAsyncState);
begin
  Execute(AResource, '', ALazyRESTResponse, TRESTRequestMethod.rmDELETE,
    ACustomData, AASync);
end;

procedure TLZRESTClient.Execute(
  AResource, ABody: string;
  ALazyRESTRequest: TLZRESTRequest;
  ALazyRESTResponse: TLZRESTResponse;
  AMethod: TRESTRequestMethod;
  ACustomData: string;
  AASync: TLZAsyncState;
  AFailOnInvalidJSON: boolean);
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
      if Assigned(ALazyRESTRequest) then
        ALazyRESTRequest(Self, ARESTRequest, ACustomData);
    end,
    procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
      AfterRESTRequest(ARESTRequest, ACustomData);
    end, AMethod, ACustomData, AASync, nil, AFailOnInvalidJSON);
end;

{ TLZRESTClientBasicAuth }

procedure TLZRESTClientBasicAuth.AfterCreateRestClient(
  ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest;
  ARESTResponse: TRESTResponse);
var
  LBasicAuth: THTTPBasicAuthenticator;
begin
  LBasicAuth := THTTPBasicAuthenticator.Create(FUsername, FPassword);
  ARESTClient.Authenticator := LBasicAuth;
end;

procedure TLZRESTClientBasicAuth.BeforeDestroyRestClient(
  ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest;
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

procedure TLZRESTClientOAuth2Base.Authenticate(
  AASync: TLZAsyncState;
  ABrowserUserDataFolder: string);
var
  LURL: string;
  LAuthRequired: boolean;
begin
  LAuthRequired := false;
  case Token.GrantType of
    gtAuthorizationCode:
      LAuthRequired := TLZString.IsEmptyString(Token.AuthToken) and
        (not TLZString.IsEmptyString(GetConnection.AuthorizeEndPoint));
    gtRefreshToken:
      LAuthRequired := TLZString.IsEmptyString(Token.AuthToken) and
        (not TLZString.IsEmptyString(GetConnection.AuthorizeEndPoint));
  end;
  if LAuthRequired then
  begin
    LURL := GetConnection.AuthorizeEndPoint;
    if Pos('?', LURL) = 0 then
    begin
      LURL := LURL +
        '?client_id=%clientid%&response_type=code&redirect_uri=%redirect_uri%&scope=%scope%';
      // LURL := GetConnection.AuthorizeEndPoint + '?client_id=' +
      // GetConnection.ClientId + '&response_type=code' + '&redirect_uri=' +
      // TNetEncoding.URL.Encode(GetConnection.RedirectURL) +
      // '&response_mode=query' + '&state=1' + '&scope=' + GetConnection.Scope;
    end;
    LURL := ProcessURLVariables(LURL);
    if Assigned(FOnLazyBrowserLoginRequest) then
    begin
      FOnLazyBrowserLoginRequest(Self, LURL, GetConnection, Token,
        ABrowserUserDataFolder);
    end
    else
    begin
      raise ERESTException.CreateFmt
        ('Interactive login required for url "%s" or provide valid Auth and Refresh tokens.',
        [LURL]);
    end;
  end
  else
  begin
    RequestToken(AASync);
  end;

end;

procedure TLZRESTClientOAuth2Base.BeforeRESTRequest(
  ARESTRequest: TRESTRequest;
  ACustomData: string);
var
  LHeader: TLZOAuth2ConnectionHeader;
begin
  inherited;
  if not TLZString.IsEmptyString(Token.AuthToken) then
  begin
    ARESTRequest.Params.AddItem('Authorization', Token.TokenAuthorizationName +
      ' ' + Token.AuthToken, TRESTRequestParameterKind.pkHTTPHEADER,
      [poDoNotEncode]);
  end;
  ARESTRequest.Params.AddItem('Content-Type', ctAPPLICATION_JSON,
    TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

  for LHeader in FConnection.Headers do
  begin
    ARESTRequest.Params.AddItem(LHeader.Key, LHeader.Value,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  end;

end;

procedure TLZRESTClientOAuth2Base.BeforeTokenRequest(
  ARESTRequest: TRESTRequest;
  ACustomData: string);
begin

  if Token.GrantType = gtRefreshToken then
  begin
    if TLZString.IsEmptyString(Token.RefreshToken) then
    begin
      Token.GrantType := gtAuthorizationCode;
    end;
  end;

  case Token.GrantType of
    gtAuthorizationCode:
      begin
        ARESTRequest.Params.AddItem('grant_type', 'authorization_code',
          TRESTRequestParameterKind.pkGETorPOST);
        ARESTRequest.Params.AddItem('code', Token.AuthToken,
          TRESTRequestParameterKind.pkGETorPOST);
      end;
    gtClientCredentials:
      begin
        ARESTRequest.Params.AddItem('grant_type', 'client_credentials',
          TRESTRequestParameterKind.pkGETorPOST);
      end;
    gtRefreshToken:
      begin
        ARESTRequest.Params.AddItem('grant_type', 'refresh_token',
          TRESTRequestParameterKind.pkGETorPOST);
        ARESTRequest.Params.AddItem('refresh_token', Token.RefreshToken,
          TRESTRequestParameterKind.pkGETorPOST);
      end;
    gtCustom:
      begin
        ARESTRequest.Params.AddItem('grant_type', Token.GrantTypeCustom,
          TRESTRequestParameterKind.pkGETorPOST);
      end;
  end;

  if not TLZString.IsEmptyString(GetConnection.ClientId) then
  begin
    ARESTRequest.Params.AddItem('client_id', GetConnection.ClientId,
      TRESTRequestParameterKind.pkGETorPOST);
  end;

  if not TLZString.IsEmptyString(GetConnection.ClientSecret) then
  begin
    ARESTRequest.Params.AddItem('client_secret', GetConnection.ClientSecret,
      TRESTRequestParameterKind.pkGETorPOST);
  end;

  if not TLZString.IsEmptyString(GetConnection.Resource) then
  begin
    ARESTRequest.Params.AddItem('resource', GetConnection.Resource,
      TRESTRequestParameterKind.pkGETorPOST);
  end;

  if not TLZString.IsEmptyString(GetConnection.Scope) then
  begin
    ARESTRequest.Params.AddItem('scope', GetConnection.Scope,
      TRESTRequestParameterKind.pkGETorPOST);
  end;

  if not TLZString.IsEmptyString(GetConnection.RedirectURL) then
  begin
    ARESTRequest.Params.AddItem('redirect_uri', GetConnection.RedirectURL,
      TRESTRequestParameterKind.pkGETorPOST);
  end;

end;

procedure TLZRESTClientOAuth2Base.ClearAuthentication;
begin
  Token.Reset;
end;

procedure TLZRESTClientOAuth2Base.CreateObjects;
begin
  inherited;
  FConnection := GetOAuth2ConnectionClass.Create;
  FToken := GetOAuth2TokenClass.Create;
end;

procedure TLZRESTClientOAuth2Base.DestroyObjects;
begin
  try
    FreeAndNil(FConnection);
    FreeAndNil(FToken);
  finally
    inherited;
  end;
end;

function TLZRESTClientOAuth2Base.GetAuthenticated: boolean;
begin
  Result := (not TLZString.IsEmptyString(Token.AuthToken)) and
    (Token.ExpiresIn >= Now);
end;

function TLZRESTClientOAuth2Base.GetBaseURL: string;
begin
  Result := FConnection.RESTEndPoint;
end;

function TLZRESTClientOAuth2Base.GetConnection: TLZOAuth2Connection;
begin
  Result := FConnection;
end;

function TLZRESTClientOAuth2Base.GetOAuth2ConnectionClass
  : TLZOAuth2ConnectionClass;
begin
  Result := TLZOAuth2Connection;
end;

function TLZRESTClientOAuth2Base.GetOAuth2TokenClass: TLZOAuth2TokenClass;
begin
  Result := TLZOAuth2Token;
end;

function TLZRESTClientOAuth2Base.ProcessURLVariables(AURL: string): string;
begin
  Result := inherited;
  Result := FConnection.ProcessURLVariables(Result);
  Debug('ProcessURLVariables', AURL + ' => ' + Result);
end;

procedure TLZRESTClientOAuth2Base.RequestToken(AASync: TLZAsyncState);
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
      LRefreshToken: string;
    begin
      if ASuccess then
      begin
        Token.AuthToken := ARESTRequest.Response.JSONValue.GetValue<String>
          ('access_token');

        Token.RefreshToken := '';
        if ARESTRequest.Response.JSONValue.TryGetValue<String>('refresh_token',
          LRefreshToken) then
        begin
          Token.RefreshToken := LRefreshToken;
          if not TLZString.IsEmptyString(LRefreshToken) then
          begin
            Token.GrantType := gtRefreshToken;
          end;
        end;

        if not ARESTRequest.Response.JSONValue.TryGetValue<integer>
          ('expires_in', LExpiresSeconds) then
        begin
          LExpiresSeconds := -1;
        end;

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
      BeforeTokenRequest(ARESTRequest, ACustomData);
    end;

  LAfter := procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
    end;

  inherited Execute(GetConnection.TokenEndPoint, '', '', LProcess, LBefore,
    LAfter, TRESTRequestMethod.rmPOST, '', AASync);

end;

procedure TLZRESTClientOAuth2Base.SetOnLazyBrowserLoginRequest
  (const Value: TOnLazyBrowserLoginRequest);
begin
  FOnLazyBrowserLoginRequest := Value;
end;

procedure TLZRESTClientOAuth2Base.SetOnLazyOAuth2TokenRequestComplete
  (const Value: TOnLazyOAuth2TokenRequestComplete);
begin
  FOnLazyOAuth2TokenRequestComplete := Value;
end;

{ TLZRESTClientOAuth1Base }

procedure TLZRESTClientOAuth1Base.AfterCreateRestClient(
  ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest;
  ARESTResponse: TRESTResponse);
var
  LAuth: TOAuth1Authenticator;
begin
  LAuth := TOAuth1Authenticator.Create(nil);
  LAuth.AccessToken := AccessToken;
  LAuth.AccessTokenSecret := AccessTokenSecret;
  LAuth.AccessTokenEndpoint := AccessTokenEndpoint;
  LAuth.RequestToken := RequestToken;
  LAuth.RequestTokenSecret := RequestTokenSecret;
  LAuth.RequestTokenEndpoint := RequestTokenEndpoint;
  LAuth.CallbackEndpoint := CallbackEndpoint;
  LAuth.ConsumerKey := ConsumerKey;
  LAuth.ConsumerSecret := ConsumerSecret;
  LAuth.SigningClassName := SigningClassName;
  LAuth.VerifierPIN := VerifierPIN;
  LAuth.IPImplementationID := IPImplementationID;
  ARESTClient.Authenticator := LAuth;
end;

procedure TLZRESTClientOAuth1Base.BeforeDestroyRestClient(
  ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest;
  ARESTResponse: TRESTResponse);
begin
  inherited;
  if Assigned(ARESTClient.Authenticator) then
  begin
    ARESTClient.Authenticator.Free;
  end;
end;

procedure TLZRESTClientOAuth1Base.SetAccessToken(const Value: string);
begin
  FAccessToken := Value;
end;

procedure TLZRESTClientOAuth1Base.SetAccessTokenEndpoint(const Value: string);
begin
  FAccessTokenEndpoint := Value;
end;

procedure TLZRESTClientOAuth1Base.SetAccessTokenSecret(const Value: string);
begin
  FAccessTokenSecret := Value;
end;

procedure TLZRESTClientOAuth1Base.SetAuthenticationEndpoint
  (const Value: string);
begin
  FAuthenticationEndpoint := Value;
end;

procedure TLZRESTClientOAuth1Base.SetCallbackEndpoint(const Value: string);
begin
  FCallbackEndpoint := Value;
end;

procedure TLZRESTClientOAuth1Base.SetConsumerKey(const Value: string);
begin
  FConsumerKey := Value;
end;

procedure TLZRESTClientOAuth1Base.SetConsumerSecret(const Value: string);
begin
  FConsumerSecret := Value;
end;

procedure TLZRESTClientOAuth1Base.SetIPImplementationID(const Value: string);
begin
  FIPImplementationID := Value;
end;

procedure TLZRESTClientOAuth1Base.SetRequestToken(const Value: string);
begin
  FRequestToken := Value;
end;

procedure TLZRESTClientOAuth1Base.SetRequestTokenEndpoint(const Value: string);
begin
  FRequestTokenEndpoint := Value;
end;

procedure TLZRESTClientOAuth1Base.SetRequestTokenSecret(const Value: string);
begin
  FRequestTokenSecret := Value;
end;

procedure TLZRESTClientOAuth1Base.SetSigningClassName(const Value: string);
begin
  FSigningClassName := Value;
end;

procedure TLZRESTClientOAuth1Base.SetVerifierPIN(const Value: string);
begin
  FVerifierPIN := Value;
end;

end.
