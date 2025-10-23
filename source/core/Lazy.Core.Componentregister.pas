unit Lazy.Core.Componentregister;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  duo.accounts, duo.admin, duo.auth, duo.models;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Lazy Library - DUO', [TLZDUOAccountsApi, TLZDUOAdminApi,
    TLZDUOAuthAPI]);
end;

end.
