unit Azure.Core;

interface

uses
  Lazy.REST.Types;

type
  TLZAzureOAuth2Connection = class(TLZOAuth2Connection)
  private
    FTenantID: string;
    procedure SetTenantID(const Value: string);
  public
    function ProcessURLVariables(AURL: string): string; override;
    property TenantID: string read FTenantID write SetTenantID;
  end;

implementation

uses
  System.SysUtils;

{ TAzureConnection }

function TLZAzureOAuth2Connection.ProcessURLVariables(AURL: string): string;
begin
  Result := inherited;
  Result := StringReplace(Result, '%tenantid%', TenantID,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TLZAzureOAuth2Connection.SetTenantID(const Value: string);
begin
  FTenantID := Value;
end;

end.
