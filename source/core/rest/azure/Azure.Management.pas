unit Azure.Management;

interface

uses
  Lazy.Types, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, REST.Client, REST.Types, REST.Json, System.Json,
  Azure.Core, Lazy.RESTClient;

type
  TAzureManagementOAuth2Connection = class(TAzureOAuth2Connection)
  protected
    procedure SetDefaults; override;
  end;

  TAzureManagement = class(TLazyRESTClientOAuth2)
  protected
    function GetOAuth2ConnectionClass: TLazyOAuth2ConnectionClass; override;
  end;

implementation

{ TAzureManagementConnection }

procedure TAzureManagementOAuth2Connection.SetDefaults;
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

function TAzureManagement.GetOAuth2ConnectionClass: TLazyOAuth2ConnectionClass;
begin
  Result := TAzureManagementOAuth2Connection;
end;

end.
