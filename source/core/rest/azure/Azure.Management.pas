unit Azure.Management;

interface

uses
  Lazy.Types, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, REST.Client, REST.Types, System.Json,
  Azure.Core, Lazy.RESTClient, Lazy.REST.Types;

type
  TLZAzureManagementOAuth2Connection = class(TLZAzureOAuth2Connection)
  protected
    procedure SetDefaults; override;
  end;

  TLZAzureManagementOAuth2Token = class(TLZAzureOAuth2Token)
  protected
    procedure RefreshTokenChanged; override;
  end;

  TLZAzureManagement = class(TLZRESTClientOAuth2Base)
  protected
    function GetOAuth2ConnectionClass: TLZOAuth2ConnectionClass; override;
    function GetOAuth2TokenClass: TLZOAuth2TokenClass; override;
    function GetConnection: TLZAzureManagementOAuth2Connection; reintroduce;
  public
    property Connection: TLZAzureManagementOAuth2Connection read GetConnection;
  end;

implementation

uses
  Lazy.Utils;

{ TAzureManagementConnection }

procedure TLZAzureManagementOAuth2Connection.SetDefaults;
begin
  inherited;
  RedirectURL := 'http://localhost';
  AuthorizeEndPoint :=
    'https://login.microsoftonline.com/%tenantid%/oauth2/v2.0/authorize';
  TokenEndPoint :=
    'https://login.microsoftonline.com/%tenantid%/oauth2/v2.0/token';
  RESTEndPoint := 'https://management.azure.com/';
  Scope := 'offline_access https://management.azure.com/.default';
end;

{ TAzureManagement }

function TLZAzureManagement.GetConnection: TLZAzureManagementOAuth2Connection;
begin
  Result := inherited GetConnection as TLZAzureManagementOAuth2Connection;
end;

function TLZAzureManagement.GetOAuth2ConnectionClass: TLZOAuth2ConnectionClass;
begin
  Result := TLZAzureManagementOAuth2Connection;
end;

function TLZAzureManagement.GetOAuth2TokenClass: TLZOAuth2TokenClass;
begin
  Result := TLZAzureManagementOAuth2Token;
end;

{ TLZAzureManagementOAuth2Token }

procedure TLZAzureManagementOAuth2Token.RefreshTokenChanged;
begin
  if not TLZString.IsEmptyString(RefreshToken) then
  begin
    if (GrantType <> gtRefreshToken) then
      GrantType := gtRefreshToken;
  end;
end;

end.
