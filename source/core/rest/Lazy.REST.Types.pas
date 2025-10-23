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
  REST.Types, REST.Client, System.JSON, System.Generics.Collections;

type
  ELZRESTException = class(Exception);

  TLZAsyncState = (lasDefault, lasTrue, lasFalse);

  TRESTRequest = REST.Client.TRESTRequest;
  TRESTResponse = REST.Client.TRESTResponse;
  TRESTRequestMethod = REST.Types.TRESTRequestMethod;

  TLZOAuth2ConnectionHeader = class(TLZObject)
  private
    FKey: string;
    FValue: string;
    procedure SetKey(const Value: string);
    procedure SetValue(const Value: string);
  public
    property Key: string read FKey write SetKey;
    property Value: string read FValue write SetValue;
  end;

  TLZOAuth2ConnectionHeaders = class(TObjectList<TLZOAuth2ConnectionHeader>)
  public
    function Add(
      AKey: string;
      AValue: string): integer; overload;
    procedure FromString(AValue: string);
    procedure FromStrings(AValue: TStrings);
  end;

  TLZOAuth2URLVariable = class(TLZObject)
  private
    FKey: string;
    FValue: string;
    procedure SetKey(const Value: string);
    procedure SetValue(const Value: string);
  public
    property Key: string read FKey write SetKey;
    property Value: string read FValue write SetValue;

  end;

  TLZOAuth2URLVariables = class(TObjectList<TLZOAuth2URLVariable>)
  private
    FPrefix: string;
    FSuffix: string;
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
  public
    procedure AfterConstruction; override;
    function Add(
      AKey: string;
      AValue: string): integer; overload;
    procedure FromString(
      AValue: string;
      AClear: boolean = false);
    procedure FromStrings(
      AValue: TStrings;
      AClear: boolean = false);
    function ProcessURLVariables(AURL: string): string;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
  end;

  TLZOAuth2Connection = class(TLZObject)
  private
    FScope: string;
    FClientId: string;
    FClientSecret: string;
    FTokenEndPoint: string;
    FRESTEndPoint: string;
    FRedirectURL: string;
    FAuthorizeEndPoint: string;
    FResource: string;
    FHeaders: TLZOAuth2ConnectionHeaders;
    FURLVariables: TLZOAuth2URLVariables;
    procedure SetAuthorizeEndPoint(const Value: string);
    procedure SetClientId(const Value: string);
    procedure SetClientSecret(const Value: string);
    procedure SetRedirectURL(const Value: string);
    procedure SetRESTEndPoint(const Value: string);
    procedure SetScope(const Value: string);
    procedure SetTokenEndPoint(const Value: string);
    procedure SetResource(const Value: string);
  protected
    procedure SetDefaults; virtual;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function ProcessURLVariables(AURL: string): string; virtual;
    property ClientId: string read FClientId write SetClientId;
    property ClientSecret: string read FClientSecret write SetClientSecret;
    property RedirectURL: string read FRedirectURL write SetRedirectURL;
    property AuthorizeEndPoint: string read FAuthorizeEndPoint
      write SetAuthorizeEndPoint;
    property TokenEndPoint: string read FTokenEndPoint write SetTokenEndPoint;
    property RESTEndPoint: string read FRESTEndPoint write SetRESTEndPoint;
    property Scope: string read FScope write SetScope;
    property Resource: string read FResource write SetResource;
    property Headers: TLZOAuth2ConnectionHeaders read FHeaders;
    property URLVariables: TLZOAuth2URLVariables read FURLVariables;
  end;

  TLZOAuth2ConnectionClass = class of TLZOAuth2Connection;

  TLZOAuth2GrantType = (gtAuthorizationCode, gtClientCredentials,
    gtRefreshToken, gtCustom);

  TLZOAuth2Token = class(TLZObject)
  private
    FRefreshToken: string;
    FAuthToken: string;
    FExpiresIn: TDateTime;
    FGrantType: TLZOAuth2GrantType;
    FTokenAuthorizationName: string;
    FGrantTypeCustom: string;
    procedure SetAuthToken(const Value: string);
    procedure SetExpiresIn(const Value: TDateTime);
    procedure SetRefreshToken(const Value: string);
    procedure SetGrantType(const Value: TLZOAuth2GrantType);
    procedure SetTokenAuthorizationName(const Value: string);
    procedure SetGrantTypeCustom(const Value: string);
  protected
    procedure SetDefaults; virtual;
    procedure RefreshTokenChanged; virtual;
  public
    constructor Create; reintroduce;
    procedure Reset;
    property TokenAuthorizationName: string read FTokenAuthorizationName
      write SetTokenAuthorizationName;
    property AuthToken: string read FAuthToken write SetAuthToken;
    property RefreshToken: string read FRefreshToken write SetRefreshToken;
    property ExpiresIn: TDateTime read FExpiresIn write SetExpiresIn;
    property GrantType: TLZOAuth2GrantType read FGrantType write SetGrantType;
    property GrantTypeCustom: string read FGrantTypeCustom
      write SetGrantTypeCustom;
  end;

  TLZOAuth2TokenClass = class of TLZOAuth2Token;

  TLZOData = class(TLZObject)
  public
    class function GetValue(
      AJSONValue: TJSONValue;
      AKey: string;
      AIncludeQuotes: boolean = true): string;
  end;

implementation

uses
  Lazy.Log, Lazy.Token, System.NetEncoding;

{ TLZOAuth2Connection }

constructor TLZOAuth2Connection.Create;
begin
  inherited;
  FHeaders := TLZOAuth2ConnectionHeaders.Create;
  FURLVariables := TLZOAuth2URLVariables.Create;
  SetDefaults;
end;

destructor TLZOAuth2Connection.Destroy;
begin
  try
    FreeAndNil(FHeaders);
    FreeAndNil(FURLVariables);
  finally
    inherited;
  end;
end;

function TLZOAuth2Connection.ProcessURLVariables(AURL: string): string;
begin
  Result := AURL;
  Result := StringReplace(Result, '%clientid%', ClientId,
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%redirect_uri%',
    TNetEncoding.URL.Encode(RedirectURL), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%scope%', Scope,
    [rfReplaceAll, rfIgnoreCase]);
  Result := FURLVariables.ProcessURLVariables(Result);
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

procedure TLZOAuth2Connection.SetResource(const Value: string);
begin
  FResource := Value;
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

constructor TLZOAuth2Token.Create;
begin
  inherited;
  FTokenAuthorizationName := 'Bearer';
  SetDefaults;
end;

procedure TLZOAuth2Token.RefreshTokenChanged;
begin

end;

procedure TLZOAuth2Token.Reset;
begin
  FRefreshToken := '';
  FAuthToken := '';
  FExpiresIn := 0;
  FGrantType := gtAuthorizationCode;
  SetDefaults;
end;

procedure TLZOAuth2Token.SetAuthToken(const Value: string);
begin
  FAuthToken := Value;
end;

procedure TLZOAuth2Token.SetDefaults;
begin
  FTokenAuthorizationName := 'Bearer';
end;

procedure TLZOAuth2Token.SetExpiresIn(const Value: TDateTime);
begin
  FExpiresIn := Value;
end;

procedure TLZOAuth2Token.SetGrantType(const Value: TLZOAuth2GrantType);
begin
  FGrantType := Value;
end;

procedure TLZOAuth2Token.SetGrantTypeCustom(const Value: string);
begin
  FGrantTypeCustom := Value;
end;

procedure TLZOAuth2Token.SetRefreshToken(const Value: string);
begin
  if Value <> FRefreshToken then
  begin
    FRefreshToken := Value;
    RefreshTokenChanged;
  end;

end;

procedure TLZOAuth2Token.SetTokenAuthorizationName(const Value: string);
begin
  FTokenAuthorizationName := Value;
end;

{ TLZOAuth2ConnectionHeader }

procedure TLZOAuth2ConnectionHeader.SetKey(const Value: string);
begin
  FKey := Value;
end;

procedure TLZOAuth2ConnectionHeader.SetValue(const Value: string);
begin
  FValue := Value;
end;

{ TLZOAuth2ConnectionHeaders }

function TLZOAuth2ConnectionHeaders.Add(AKey, AValue: string): integer;
var
  LHEader: TLZOAuth2ConnectionHeader;
begin
  LHEader := TLZOAuth2ConnectionHeader.Create;
  LHEader.Key := AKey;
  LHEader.Value := AValue;
  Result := Add(LHEader);
end;

procedure TLZOAuth2ConnectionHeaders.FromString(AValue: string);
var
  LStrings: TStrings;
begin
  LStrings := TLZToken.GetTokens(AValue);
  try
    FromStrings(LStrings);
  finally
    LStrings.Free;
  end;
end;

procedure TLZOAuth2ConnectionHeaders.FromStrings(AValue: TStrings);
var
  LIdx: integer;
begin
  for LIdx := 0 to Pred(AValue.Count) do
  begin
    Add(AValue.Names[LIdx], AValue.ValueFromIndex[LIdx]);
  end;
end;

{ TLZOAuth2URLVariable }

procedure TLZOAuth2URLVariable.SetKey(const Value: string);
begin
  FKey := Value;
end;

procedure TLZOAuth2URLVariable.SetValue(const Value: string);
begin
  FValue := Value;
end;

{ TLZOAuth2URLVariables }

function TLZOAuth2URLVariables.Add(AKey, AValue: string): integer;
var
  LHEader: TLZOAuth2URLVariable;
begin
  LHEader := TLZOAuth2URLVariable.Create;
  LHEader.Key := AKey;
  LHEader.Value := AValue;
  Result := Add(LHEader);
end;

procedure TLZOAuth2URLVariables.AfterConstruction;
begin
  inherited;
  FPrefix := '%';
  FSuffix := '%';
end;

procedure TLZOAuth2URLVariables.FromString(
  AValue: string;
  AClear: boolean);
var
  LStrings: TStrings;
begin
  LStrings := TLZToken.GetTokens(AValue);
  try
    FromStrings(LStrings, AClear);
  finally
    LStrings.Free;
  end;
end;

procedure TLZOAuth2URLVariables.FromStrings(
  AValue: TStrings;
  AClear: boolean);
var
  LIdx: integer;
begin
  if AClear then
    Clear;
  for LIdx := 0 to Pred(AValue.Count) do
  begin
    Add(AValue.Names[LIdx], AValue.ValueFromIndex[LIdx]);
  end;
end;

function TLZOAuth2URLVariables.ProcessURLVariables(AURL: string): string;
var
  LVariable: TLZOAuth2URLVariable;
  LSource: string;
begin
  Result := AURL;
  for LVariable in Self do
  begin
    LSource := Trim(Prefix + LVariable.Key + Suffix);
    Result := StringReplace(Result, LSource, LVariable.Value,
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TLZOAuth2URLVariables.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TLZOAuth2URLVariables.SetSuffix(const Value: string);
begin
  FSuffix := Value;
end;

{ TLZOData }

class function TLZOData.GetValue(
  AJSONValue: TJSONValue;
  AKey: string;
  AIncludeQuotes: boolean): string;
var
  LObject: TJSONObject;
  LPair: TJSONPair;
  LKey: string;
begin
  Result := '';
  LKey := AKey;
  if AIncludeQuotes then
    LKey := AnsiQuotedStr(LKey, '"');
  LObject := AJSONValue as TJSONObject;
  for LPair in LObject do
  begin
    if SameText(LPair.JsonString.ToString, LKey) then
    begin
      Result := LPair.JSONValue.GetValue<string>;
      Exit;
    end;
  end;
end;

end.
