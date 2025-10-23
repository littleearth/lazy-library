unit MSGraph.Core;

interface

uses
  Lazy.REST.Types;

type
  TLZMSGraphOAuth2Connection = class(TLZOAuth2Connection)
  private
    FTenantID: string;
    procedure SetTenantID(const Value: string);
  public
    function ProcessURLVariables(AURL: string): string; override;
    property TenantID: string read FTenantID write SetTenantID;
  end;

  TLZMSGraphOAuth2Token = class(TLZOAuth2Token)
  protected
    procedure SetDefaults; override;
  end;

implementation

uses
  System.SysUtils;

{ TMSGraphConnection }

function TLZMSGraphOAuth2Connection.ProcessURLVariables(AURL: string): string;
begin
  Result := inherited;
  Result := StringReplace(Result, '%tenantid%', TenantID,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TLZMSGraphOAuth2Connection.SetTenantID(const Value: string);
begin
  FTenantID := Value;
end;

{ TLZMSGraphOAuth2Token }

procedure TLZMSGraphOAuth2Token.SetDefaults;
begin
  inherited;
  GrantType := gtClientCredentials;
end;

end.
