unit Lazy.RESTClient;

interface

uses
  Lazy.Types, System.SysUtils, System.Types, System.Classes,
  System.Variants, REST.Client, REST.Types, REST.Json, System.Json;

type
  TLazyRESTResponse = reference to procedure(ASender: TObject;
    ASuccess: boolean; AMessage: string; ARESTResponse: TRESTResponse;
    ACustomData: string);

  TLazyProcessRESTResponse = reference to procedure(ARESTRequest: TRESTRequest;
    ARESTResponse: TRESTResponse; ASuccess: boolean; AMessage: string;
    AJSON: string; ACustomData: string);
  TLazyBeforeRESTRequest = reference to procedure(ARESTRequest: TRESTRequest;
    ACustomData: string);
  TLazyAfterRESTRequest = reference to procedure(ARESTRequest: TRESTRequest;
    ACustomData: string);

  TLazyAsyncState = (lasDefault, lasTrue, lasFalse);

  TLazyRESTClientBase = class(TLazyObject)
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
      ALazyProcessRESTResponse: TLazyProcessRESTResponse;
      ALazyBeforeRESTRequest: TLazyBeforeRESTRequest = nil;
      ALazyAfterRESTRequest: TLazyAfterRESTRequest = nil;
      AMethod: TRESTRequestMethod = TRESTRequestMethod.rmGET;
      ACustomData: string = ''; AASync: TLazyAsyncState = lasDefault); overload;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property DefaultAsync: boolean read FDefaultAsync write SetDefaultAsync;
  end;

  TLazyRESTClient = class(TLazyRESTClientBase)
  protected
    procedure Execute(AResource, ABody: string;
      ALazyRESTResponse: TLazyRESTResponse; AMethod: TRESTRequestMethod;
      ACustomData: string; AASync: TLazyAsyncState); overload;
    procedure BeforeRESTRequest(ARESTRequest: TRESTRequest;
      ACustomData: string); virtual;
    procedure AfterRESTRequest(ARESTRequest: TRESTRequest;
      ACustomData: string); virtual;
  public
    procedure Get(AResource: string; ALazyRESTResponse: TLazyRESTResponse;
      ACustomData: string = ''; AASync: TLazyAsyncState = lasDefault);
    procedure Post(AResource: string; ABody: string;
      ALazyRESTResponse: TLazyRESTResponse; ACustomData: string = '';
      AASync: TLazyAsyncState = lasDefault);
    procedure Put(AResource: string; ABody: string;
      ALazyRESTResponse: TLazyRESTResponse; ACustomData: string = '';
      AASync: TLazyAsyncState = lasDefault);
    procedure Delete(AResource: string; ALazyRESTResponse: TLazyRESTResponse;
      ACustomData: string = ''; AASync: TLazyAsyncState = lasDefault);
  end;

  TLazyRESTClientBasicAuth = class(TLazyRESTClient)
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
    AConnection: TLazyOAuth2Connection; AToken: TLazyOAuth2Token) of object;

  TOnLazyOAuth2TokenRequestComplete = procedure(ASender: TObject;
    ASuccess: boolean; AMessage: string; AToken: TLazyOAuth2Token) of object;

  TLazyRESTClientOAuth2 = class(TLazyRESTClient)
  private
    FOnLazyBrowserLoginRequest: TOnLazyBrowserLoginRequest;
    FOnLazyOAuth2TokenRequestComplete: TOnLazyOAuth2TokenRequestComplete;
    FConnection: TLazyOAuth2Connection;
    FToken: TLazyOAuth2Token;
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
    function GetOAuth2ConnectionClass: TLazyOAuth2ConnectionClass; virtual;
    procedure BeforeRESTRequest(ARESTRequest: TRESTRequest;
      ACustomData: string); override;
  public
    procedure Authenticate(AASync: TLazyAsyncState = lasDefault);
    procedure ClearAuthentication;
    procedure RequestToken(AASync: TLazyAsyncState = lasDefault);
    property IsAuthenticated: boolean read GetAuthenticated;
    property Token: TLazyOAuth2Token read FToken;
    property Connection: TLazyOAuth2Connection read FConnection;
    property OnBrowserLoginRequest: TOnLazyBrowserLoginRequest
      read FOnLazyBrowserLoginRequest write SetOnLazyBrowserLoginRequest;
    property OnOAuth2TokenRequestComplete: TOnLazyOAuth2TokenRequestComplete
      read FOnLazyOAuth2TokenRequestComplete
      write SetOnLazyOAuth2TokenRequestComplete;
  end;

implementation

uses Lazy.Utils, System.NetEncoding, System.Net.URLClient, System.DateUtils,
  REST.Authenticator.Basic;

{ TLazyRESTClient }

procedure TLazyRESTClientBase.AfterCreateRestClient(ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse);
begin

end;

procedure TLazyRESTClientBase.BeforeDestroyRestClient(ARESTClient: TRESTClient;
  ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse);
begin

end;

constructor TLazyRESTClientBase.Create;
begin
  inherited;
  FDefaultAsync := True;
  SetDefaults;
  CreateObjects;
end;

procedure TLazyRESTClientBase.CreateObjects;
begin

end;

destructor TLazyRESTClientBase.Destroy;
begin
  try
    DestroyObjects;
  finally
    inherited;
  end;
end;

procedure TLazyRESTClientBase.DestroyObjects;
begin

end;

procedure TLazyRESTClientBase.Execute(ABaseURL, AResource, ABody: string;
  ALazyProcessRESTResponse: TLazyProcessRESTResponse;
  ALazyBeforeRESTRequest: TLazyBeforeRESTRequest;
  ALazyAfterRESTRequest: TLazyAfterRESTRequest; AMethod: TRESTRequestMethod;
  ACustomData: string; AASync: TLazyAsyncState);
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

  if not TlazyString.IsEmptyString(ABody) then
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

function TLazyRESTClientBase.GetBaseURL: string;
begin

end;

function TLazyRESTClientBase.ProcessURLVariables(AURL: string): string;
begin
  Result := AURL;
end;

procedure TLazyRESTClientBase.SetDefaultAsync(const Value: boolean);
begin
  FDefaultAsync := Value;
end;

procedure TLazyRESTClientBase.SetDefaults;
begin

end;

procedure TLazyRESTClient.Execute(AResource, ABody: string;
  ALazyRESTResponse: TLazyRESTResponse; AMethod: TRESTRequestMethod;
  ACustomData: string; AASync: TLazyAsyncState);
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

procedure TLazyRESTClient.Get(AResource: string;
ALazyRESTResponse: TLazyRESTResponse; ACustomData: string;
AASync: TLazyAsyncState);
begin
  Execute(AResource, '', ALazyRESTResponse, TRESTRequestMethod.rmGET,
    ACustomData, AASync);
end;

procedure TLazyRESTClient.Post(AResource, ABody: string;
ALazyRESTResponse: TLazyRESTResponse; ACustomData: string;
AASync: TLazyAsyncState);
begin
  Execute(AResource, ABody, ALazyRESTResponse, TRESTRequestMethod.rmPOST,
    ACustomData, AASync);
end;

procedure TLazyRESTClient.Put(AResource, ABody: string;
ALazyRESTResponse: TLazyRESTResponse; ACustomData: string;
AASync: TLazyAsyncState);
begin
  Execute(AResource, ABody, ALazyRESTResponse, TRESTRequestMethod.rmPUT,
    ACustomData, AASync);
end;

procedure TLazyRESTClient.AfterRESTRequest(ARESTRequest: TRESTRequest;
ACustomData: string);
begin

end;

procedure TLazyRESTClient.BeforeRESTRequest(ARESTRequest: TRESTRequest;
ACustomData: string);
begin

end;

procedure TLazyRESTClient.Delete(AResource: string;
ALazyRESTResponse: TLazyRESTResponse; ACustomData: string;
AASync: TLazyAsyncState);
begin
  Execute(AResource, '', ALazyRESTResponse, TRESTRequestMethod.rmDELETE,
    ACustomData, AASync);
end;

{ TLazyRESTClientBasicAuth }

procedure TLazyRESTClientBasicAuth.AfterCreateRestClient
  (ARESTClient: TRESTClient; ARESTRequest: TRESTRequest;
ARESTResponse: TRESTResponse);
var
  LBasicAuth: THTTPBasicAuthenticator;
begin
  LBasicAuth := THTTPBasicAuthenticator.Create(FUsername, FPassword);
  ARESTClient.Authenticator := LBasicAuth;
end;

procedure TLazyRESTClientBasicAuth.BeforeDestroyRestClient
  (ARESTClient: TRESTClient; ARESTRequest: TRESTRequest;
ARESTResponse: TRESTResponse);
begin
  inherited;
  if Assigned(ARESTClient.Authenticator) then
  begin
    ARESTClient.Authenticator.Free;
  end;
end;

procedure TLazyRESTClientBasicAuth.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TLazyRESTClientBasicAuth.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

{ TLazyRESTClientOAuth2 }

procedure TLazyRESTClientOAuth2.Authenticate(AASync: TLazyAsyncState);
var
  LURL: string;
begin
  if TlazyString.IsEmptyString(Token.AuthToken) or
    TlazyString.IsEmptyString(Token.RefreshToken) then
  begin
    LURL := Connection.AuthorizeEndPoint + '?client_id=' + Connection.ClientId +
      '&response_type=code' + '&redirect_uri=' + TNetEncoding.URL.Encode
      (Connection.RedirectURL) + '&response_mode=query' + '&state=1' +
      '&scope=openid';
    LURL := ProcessURLVariables(LURL);
    if Assigned(FOnLazyBrowserLoginRequest) then
    begin
      FOnLazyBrowserLoginRequest(Self, LURL, Connection, Token);
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

procedure TLazyRESTClientOAuth2.BeforeRESTRequest(ARESTRequest: TRESTRequest;
ACustomData: string);
begin
  inherited;
  if not TlazyString.IsEmptyString(Token.AuthToken) then
  begin
    ARESTRequest.Params.AddItem('Authorization', 'Bearer ' + Token.AuthToken,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  end;
  ARESTRequest.Params.AddItem('Content-Type', 'application/json',
    TRESTRequestParameterKind.pkHTTPHEADER);
end;

procedure TLazyRESTClientOAuth2.ClearAuthentication;
begin
  Token.Reset;
end;

procedure TLazyRESTClientOAuth2.CreateObjects;
begin
  inherited;
  FConnection := GetOAuth2ConnectionClass.Create;
  FToken := TLazyOAuth2Token.Create;
end;

procedure TLazyRESTClientOAuth2.DestroyObjects;
begin
  try
    FreeAndNil(FConnection);
    FreeAndNil(FToken);
  finally
    inherited;
  end;
end;

function TLazyRESTClientOAuth2.GetAuthenticated: boolean;
begin
  Result := (not TlazyString.IsEmptyString(Token.AuthToken)) and
    (Token.ExpiresIn >= Now);
end;

function TLazyRESTClientOAuth2.GetBaseURL: string;
begin
  Result := FConnection.RESTEndPoint;
end;

function TLazyRESTClientOAuth2.GetOAuth2ConnectionClass
  : TLazyOAuth2ConnectionClass;
begin
  Result := TLazyOAuth2Connection;
end;

function TLazyRESTClientOAuth2.ProcessURLVariables(AURL: string): string;
begin
  Result := inherited;
  Result := FConnection.ProcessURLVariables(Result);
  Debug('ProcessURLVariables', AURL + ' => ' + Result);
end;

procedure TLazyRESTClientOAuth2.RequestToken(AASync: TLazyAsyncState);
var
  LProcess: TLazyProcessRESTResponse;
  LBefore: TLazyBeforeRESTRequest;
  LAfter: TLazyAfterRESTRequest;
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
      if TlazyString.IsEmptyString(Token.AuthToken) or
        TlazyString.IsEmptyString(Token.RefreshToken) then
      begin

        ARESTRequest.Params.AddItem('client_id', Connection.ClientId,
          TRESTRequestParameterKind.pkQUERY);

        ARESTRequest.Params.AddItem('grant_type', 'authorization_code',
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_id', Connection.ClientId,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('code', Token.AuthCode,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_secret', Connection.ClientSecret,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('scope', Connection.Scope,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('redirect_uri', Connection.RedirectURL,
          TRESTRequestParameterKind.pkREQUESTBODY);
      end
      else
      begin

        // Use refresh token
        ARESTRequest.Params.AddItem('client_id', Connection.ClientId,
          TRESTRequestParameterKind.pkQUERY);
        ARESTRequest.Params.AddItem('code', Token.AuthCode,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('grant_type', 'refresh_token',
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_id', Connection.ClientId,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('client_secret', Connection.ClientSecret,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('refresh_token', Token.RefreshToken,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('scope', Connection.Scope,
          TRESTRequestParameterKind.pkREQUESTBODY);
        ARESTRequest.Params.AddItem('redirect_uri', Connection.RedirectURL,
          TRESTRequestParameterKind.pkREQUESTBODY);
      end;
    end;

  LAfter := procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
    end;

  inherited Execute(Connection.TokenEndPoint, '', '', LProcess, LBefore, LAfter,
    TRESTRequestMethod.rmPOST, '', AASync);

end;

procedure TLazyRESTClientOAuth2.SetOnLazyBrowserLoginRequest
  (const Value: TOnLazyBrowserLoginRequest);
begin
  FOnLazyBrowserLoginRequest := Value;
end;

procedure TLazyRESTClientOAuth2.SetOnLazyOAuth2TokenRequestComplete
  (const Value: TOnLazyOAuth2TokenRequestComplete);
begin
  FOnLazyOAuth2TokenRequestComplete := Value;
end;

end.
