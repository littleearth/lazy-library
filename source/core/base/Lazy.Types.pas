{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Types
  Author: Tristan Marlow
  Purpose: Common types

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

  ----------------------------------------------------------------------------; }
unit Lazy.Types;

interface

uses
  Windows, Messages,
  System.SyncObjs, System.SysUtils, System.Variants, System.Classes,
  REST.Types, REST.Client;

type
  ELazyException = class(Exception);
  TLazyLogLevel = (logError, logWarning, logInformation, logDebug);

  TLazyTimeRounding = (trNearest, trUp, trDown);
  TLazyTimeFormat = (tfDefault, tf12Hour, tf24Hour);

  TDateTimeFunc = reference to function: TDateTime;

  TRESTRequest = REST.Client.TRESTRequest;
  TRESTResponse = REST.Client.TRESTResponse;
  TRESTRequestMethod = REST.Types.TRESTRequestMethod;

  TLazyObject = class(TObject)
  protected
    procedure Log(AMessage: string);
    procedure Debug(AProcedure: string; AMessage: string);
    procedure Warning(AMessage: string);
    procedure Error(AMessage: string; AErrorCode: integer = 0); overload;
    procedure Error(AException: Exception; AMessage: string = ''); overload;
  end;

  TLazyComponent = class(TComponent)
  protected
    procedure Log(AMessage: string);
    procedure Debug(AProcedure: string; AMessage: string);
    procedure Warning(AMessage: string);
    procedure Error(AMessage: string; AErrorCode: integer = 0); overload;
    procedure Error(AException: Exception; AMessage: string = ''); overload;
  end;

  TLazyOAuth2Connection = class(TLazyObject)
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

  TLazyOAuth2ConnectionClass = class of TLazyOAuth2Connection;

  TLazyOAuth2Token = class(TLazyObject)
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

type
  TApplicationVersionDetails = class(TLazyComponent)
  private
    FMajorVersion: integer;
    FMinorVersion: integer;
    FReleaseVersion: integer;
    FBuildVersion: integer;
  protected
    procedure SetVersion(AVersionString: string);
    function GetVersion: string;
  public
    procedure Reset;
    function AsString: string;
    function IsCurrent(ACompareVersion: string): boolean;
  published
    property Version: string read GetVersion write SetVersion;
    property MajorVersion: integer read FMajorVersion write FMajorVersion;
    property MinorVersion: integer read FMinorVersion write FMinorVersion;
    property ReleaseVersion: integer read FReleaseVersion write FReleaseVersion;
    property BuildVersion: integer read FBuildVersion write FBuildVersion;
  end;

implementation

uses
  Lazy.Log;

{ TLazyObject }

procedure TLazyObject.Debug(AProcedure, AMessage: string);
begin
  LazyLog.Debug(Self, AProcedure, AMessage);
end;

procedure TLazyObject.Error(AException: Exception; AMessage: string);
begin
  LazyLog.Error(Self, AException, AMessage);
end;

procedure TLazyObject.Error(AMessage: string; AErrorCode: integer);
begin
  LazyLog.Error(Self, AMessage, AErrorCode);
end;

procedure TLazyObject.Log(AMessage: string);
begin
  LazyLog.Log(Self, AMessage);
end;

procedure TLazyObject.Warning(AMessage: string);
begin
  LazyLog.Warning(Self, AMessage);
end;

{ TLazyComponent }

procedure TLazyComponent.Debug(AProcedure, AMessage: string);
begin
  LazyLog.Debug(Self, AProcedure, AMessage);
end;

procedure TLazyComponent.Error(AException: Exception; AMessage: string);
begin
  LazyLog.Error(Self, AException, AMessage);
end;

procedure TLazyComponent.Error(AMessage: string; AErrorCode: integer);
begin
  LazyLog.Error(Self, AMessage, AErrorCode);
end;

procedure TLazyComponent.Log(AMessage: string);
begin
  LazyLog.Log(Self, AMessage);
end;

procedure TLazyComponent.Warning(AMessage: string);
begin
  LazyLog.Warning(Self, AMessage);
end;

{ TLazyOAuth2Connection }

constructor TLazyOAuth2Connection.Create;
begin
  inherited;
  SetDefaults;
end;

function TLazyOAuth2Connection.ProcessURLVariables(AURL: string): string;
begin
  Result := AURL;
  Result := StringReplace(Result, '%clientid%', ClientId,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TLazyOAuth2Connection.SetAuthorizeEndPoint(const Value: string);
begin
  FAuthorizeEndPoint := Value;
end;

procedure TLazyOAuth2Connection.SetClientId(const Value: string);
begin
  FClientId := Value;
end;

procedure TLazyOAuth2Connection.SetClientSecret(const Value: string);
begin
  FClientSecret := Value;
end;

procedure TLazyOAuth2Connection.SetDefaults;
begin

end;

procedure TLazyOAuth2Connection.SetRedirectURL(const Value: string);
begin
  FRedirectURL := Value;
end;

procedure TLazyOAuth2Connection.SetRESTEndPoint(const Value: string);
begin
  FRESTEndPoint := Value;
end;

procedure TLazyOAuth2Connection.SetScope(const Value: string);
begin
  FScope := Value;
end;

procedure TLazyOAuth2Connection.SetTokenEndPoint(const Value: string);
begin
  FTokenEndPoint := Value;
end;

{ TLazyOAuth2Token }

procedure TLazyOAuth2Token.Reset;
begin
  FAuthCode := '';
  FRefreshToken := '';
  FAuthToken := '';
  FExpiresIn := 0;
end;

procedure TLazyOAuth2Token.SetAuthCode(const Value: string);
begin
  FAuthCode := Value;
end;

procedure TLazyOAuth2Token.SetAuthToken(const Value: string);
begin
  FAuthToken := Value;
end;

procedure TLazyOAuth2Token.SetExpiresIn(const Value: TDateTime);
begin
  FExpiresIn := Value;
end;

procedure TLazyOAuth2Token.SetRefreshToken(const Value: string);
begin
  FRefreshToken := Value;
end;

{ TApplicationVersionDetails }

procedure TApplicationVersionDetails.Reset;
begin
  FMajorVersion := 0;
  FMinorVersion := 0;
  FReleaseVersion := 0;
  FBuildVersion := 0;
end;

function TApplicationVersionDetails.AsString: string;
begin
  Result := IntToStr(FMajorVersion) + '.' + IntToStr(FMinorVersion) + '.' +
    IntToStr(FReleaseVersion) + '.' + IntToStr(FBuildVersion);
end;

function TApplicationVersionDetails.GetVersion: string;
begin
  Result := AsString;
end;

procedure TApplicationVersionDetails.SetVersion(AVersionString: string);
var
  TempStr: string;
  Idx, VersionIdx: integer;
begin
  Reset;
  TempStr := '';
  Idx := 1;
  VersionIdx := 0;
  AVersionString := AVersionString + '.';
  while (Idx <= Length(AVersionString)) do
  begin
    if (AVersionString[Idx] = '.') or (AVersionString[Idx] = ' ') then
    begin
      case VersionIdx of
        0:
          FMajorVersion := StrToIntDef(TempStr, 0);
        1:
          FMinorVersion := StrToIntDef(TempStr, 0);
        2:
          FReleaseVersion := StrToIntDef(TempStr, 0);
        3:
          FBuildVersion := StrToIntDef(TempStr, 0);
      end;
      Inc(VersionIdx);
      TempStr := '';
    end
    else
    begin
      TempStr := TempStr + AVersionString[Idx];
    end;
    Inc(Idx);
  end;
end;

function TApplicationVersionDetails.IsCurrent(ACompareVersion: string): boolean;
var
  CompareVersionDetails: TApplicationVersionDetails;
  compareVersionMajorMinor: integer;
  compareVersionReleaseBuild: integer;
  currentVersionMajorMinor: integer;
  currentVersionReleaseBuild: integer;
begin
  Result := false;

  CompareVersionDetails := TApplicationVersionDetails.Create(nil);
  try
    CompareVersionDetails.Version := ACompareVersion;
    Debug('IsCurrent', Self.AsString + ' <> ' + CompareVersionDetails.AsString);

    compareVersionMajorMinor := CompareVersionDetails.MajorVersion * 1000 +
      CompareVersionDetails.MinorVersion;
    currentVersionMajorMinor := Self.MajorVersion * 1000 + Self.MinorVersion;

    if compareVersionMajorMinor > currentVersionMajorMinor then
    begin
      Result := True;
    end
    else
    begin
      if compareVersionMajorMinor = currentVersionMajorMinor then
      begin
        compareVersionReleaseBuild := CompareVersionDetails.ReleaseVersion *
          1000 + CompareVersionDetails.BuildVersion;
        currentVersionReleaseBuild := Self.ReleaseVersion * 1000 +
          Self.BuildVersion;

        if compareVersionReleaseBuild >= currentVersionReleaseBuild then
        begin
          Result := True;
        end;
      end;
    end;
  finally
    FreeAndNil(CompareVersionDetails)
  end;
end;

end.
