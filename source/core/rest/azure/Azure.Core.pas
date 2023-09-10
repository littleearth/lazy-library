unit Azure.Core;

interface

uses
  Lazy.Types;

type
  TAzureOAuth2Connection = class(TLazyOAuth2Connection)
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

function TAzureOAuth2Connection.ProcessURLVariables(AURL: string): string;
begin
  Result := inherited;
  Result := StringReplace(Result, '%tenantid%', TenantID,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TAzureOAuth2Connection.SetTenantID(const Value: string);
begin
  FTenantID := Value;
end;

end.
