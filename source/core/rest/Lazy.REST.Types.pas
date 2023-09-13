{ -----------------------------------------------------------------------------
  Unit Name: Lazy.REST.Types
  Author: Tristan Marlow
  Purpose: Common types

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

  ----------------------------------------------------------------------------; }
unit Lazy.REST.Types;

interface

uses
  Lazy.Types, System.SyncObjs, System.SysUtils, System.Variants, System.Classes,
  REST.Types, REST.Client;

type
  ELZRESTException = class(Exception);

  TRESTRequest = REST.Client.TRESTRequest;
  TRESTResponse = REST.Client.TRESTResponse;
  TRESTRequestMethod = REST.Types.TRESTRequestMethod;

  TLZOAuth2Connection = class(TLZObject)
  private
    FScope: string;
    FClientId: string;
    FClientSecret: string;
    FTokenEndPoint: string;
    FRESTEndPoint: string;
    FRedirectURL: string;
    FAuthorizeEndPoint: string;
    procedure SetAuthorizeEndPoint(const Value: string);
    procedure SetClientId(const Value: string);
    procedure SetClientSecret(const Value: string);
    procedure SetRedirectURL(const Value: string);
    procedure SetRESTEndPoint(const Value: string);
    procedure SetScope(const Value: string);
    procedure SetTokenEndPoint(const Value: string);
  protected
    procedure SetDefaults; virtual;
  public
    constructor Create; reintroduce;
    function ProcessURLVariables(AURL: string): string; virtual;
    property ClientId: string read FClientId write SetClientId;
    property ClientSecret: string read FClientSecret write SetClientSecret;
    property RedirectURL: string read FRedirectURL write SetRedirectURL;
    property AuthorizeEndPoint: string read FAuthorizeEndPoint
      write SetAuthorizeEndPoint;
    property TokenEndPoint: string read FTokenEndPoint write SetTokenEndPoint;
    property RESTEndPoint: string read FRESTEndPoint write SetRESTEndPoint;
    property Scope: string read FScope write SetScope;
  end;

  TLZOAuth2ConnectionClass = class of TLZOAuth2Connection;

  TLZOAuth2Token = class(TLZObject)
  private
    FRefreshToken: string;
    FAuthCode: string;
    FAuthToken: string;
    FExpiresIn: TDateTime;
    procedure SetAuthCode(const Value: string);
    procedure SetAuthToken(const Value: string);
    procedure SetExpiresIn(const Value: TDateTime);
    procedure SetRefreshToken(const Value: string);
  public
    procedure Reset;
    property AuthCode: string read FAuthCode write SetAuthCode;
    property AuthToken: string read FAuthToken write SetAuthToken;
    property RefreshToken: string read FRefreshToken write SetRefreshToken;
    property ExpiresIn: TDateTime read FExpiresIn write SetExpiresIn;
  end;

implementation

uses
  Lazy.Log;

{ TLZOAuth2Connection }

constructor TLZOAuth2Connection.Create;
begin
  inherited;
  SetDefaults;
end;

function TLZOAuth2Connection.ProcessURLVariables(AURL: string): string;
begin
  Result := AURL;
  Result := StringReplace(Result, '%clientid%', ClientId,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TLZOAuth2Connection.SetAuthorizeEndPoint(const Value: string);
begin
  FAuthorizeEndPoint := Value;
end;

procedure TLZOAuth2Connection.SetClientId(const Value: string);
begin
  FClientId := Value;
end;

procedure TLZOAuth2Connection.SetClientSecret(const Value: string);
begin
  FClientSecret := Value;
end;

procedure TLZOAuth2Connection.SetDefaults;
begin

end;

procedure TLZOAuth2Connection.SetRedirectURL(const Value: string);
begin
  FRedirectURL := Value;
end;

procedure TLZOAuth2Connection.SetRESTEndPoint(const Value: string);
begin
  FRESTEndPoint := Value;
end;

procedure TLZOAuth2Connection.SetScope(const Value: string);
begin
  FScope := Value;
end;

procedure TLZOAuth2Connection.SetTokenEndPoint(const Value: string);
begin
  FTokenEndPoint := Value;
end;

{ TLZOAuth2Token }

procedure TLZOAuth2Token.Reset;
begin
  FAuthCode := '';
  FRefreshToken := '';
  FAuthToken := '';
  FExpiresIn := 0;
end;

procedure TLZOAuth2Token.SetAuthCode(const Value: string);
begin
  FAuthCode := Value;
end;

procedure TLZOAuth2Token.SetAuthToken(const Value: string);
begin
  FAuthToken := Value;
end;

procedure TLZOAuth2Token.SetExpiresIn(const Value: TDateTime);
begin
  FExpiresIn := Value;
end;

procedure TLZOAuth2Token.SetRefreshToken(const Value: string);
begin
  FRefreshToken := Value;
end;

end.
