unit duo.accounts;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  REST.Authenticator.Basic, System.JSON, duo.api, System.Generics.Collections,
  duo.models, duo.admin;

type

  TOnDuoAccountsEvent = procedure(ASender: TObject; AAccounts: TDUOAccounts;
    AMessage: string; ASuccess: boolean; var AOwnsObjects: boolean) of object;
  TOnDuoAccountsProc = reference to procedure(AAccounts: TDUOAccounts;
    AMessage: string; ASuccess: boolean; var AOwnsObjects: boolean);

  TOnDuoBillingEvent = procedure(ASender: TObject; AAcountID: string;
    AEdition: string; AMessage: string; ASuccess: boolean) of object;
  TOnDuoBillingProc = reference to procedure(AAcountID: string;
    AEdition: string; AMessage: string; ASuccess: boolean);

  TOnDuoTelephonyCreditsEvent = procedure(ASender: TObject; AAcountID: string;
    ACredits: integer; AMessage: string; ASuccess: boolean) of object;
  TOnDuoTelephonyCreditsProc = reference to procedure(AAcountID: string;
    ACredits: integer; AMessage: string; ASuccess: boolean);

  TDUOAccountsApi = class(TDuoAPIBase)
  private
    FOnGetAccounts: TOnDuoAccountsEvent;
    FOnGetBilling: TOnDuoBillingEvent;
    FOnGetTelphonyCredits: TOnDuoTelephonyCreditsEvent;
    procedure SetOnGetAccounts(const Value: TOnDuoAccountsEvent);
    procedure SetOnGetBilling(const Value: TOnDuoBillingEvent);
    procedure SetOnGetTelphonyCredits(const Value: TOnDuoTelephonyCreditsEvent);
  protected
    function GetBaseResource: string; override;
    function GetAdminBaseResource: string;
  public
    procedure GetAccounts(AIncludeBillingEdition: boolean = false;
      AIncludeTeleponyCredits: boolean = false;
      AOnDuoAccountsProc: TOnDuoAccountsProc = nil);
    procedure GetBilling(AAccountID: string; AAPIHostname: string;
      AOnDuoBillingProc: TOnDuoBillingProc = nil);
    procedure GetTelephonyCredits(AAccountID: string; AAPIHostname: string;
      AOnDuoTelephonyCreditsProc: TOnDuoTelephonyCreditsProc = nil);
  published
    property OnGetAccounts: TOnDuoAccountsEvent read FOnGetAccounts
      write SetOnGetAccounts;
    property OnGetBilling: TOnDuoBillingEvent read FOnGetBilling
      write SetOnGetBilling;
    property OnGetTelephonyCredits: TOnDuoTelephonyCreditsEvent
      read FOnGetTelphonyCredits write SetOnGetTelphonyCredits;
  end;

implementation

uses
  System.StrUtils;

{ TDUOAccounts }

procedure TDUOAccountsApi.GetAccounts(AIncludeBillingEdition,
  AIncludeTeleponyCredits: boolean; AOnDuoAccountsProc: TOnDuoAccountsProc);
var
  LAccounts: TDUOAccounts;
  LResult, LOwnsObjects: boolean;
  LMessage: string;
begin
  LOwnsObjects := true;
  LAccounts := TDUOAccounts.Create;
  try

    LResult := ExecuteRequest(LMessage, BaseResource + 'account/list', rmPOST,
      procedure(ARESTRequest: TRESTRequest)
      begin
      end,
      procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
        var AMessage: string)
      var
        LResult, LStatusMsg: string;
        LAccount: TDUOAccount;
      begin
        if ASuccess then
        begin
          try
            LResult := ARESTResponse.JSONValue.GetValue<string>('stat', '');
            if (SameText(LResult, 'Ok')) then
            begin
              LAccounts.FromJSONValue(ARESTResponse.JSONValue);
              if AIncludeBillingEdition then
              begin
                for LAccount in LAccounts.Items do
                begin
                  GetBilling(LAccount.AccountID, LAccount.APIHostname,
                    procedure(AAcountID: string; AEdition: string;
                      AMessage: string; ASuccess: boolean)
                    begin
                      if ASuccess then
                        LAccount.BillingEdition := AEdition;
                    end);
                end;
              end;
              if AIncludeTeleponyCredits then
              begin
                for LAccount in LAccounts.Items do
                begin
                  GetTelephonyCredits(LAccount.AccountID, LAccount.APIHostname,
                    procedure(AAcountID: string; ACredits: integer;
                      AMessage: string; ASuccess: boolean)
                    begin
                      if ASuccess then
                        LAccount.TeleponyCredits := ACredits;
                    end);
                end;
              end;
              ASuccess := true;
            end
            else
            begin
              LStatusMsg := ARESTResponse.JSONValue.GetValue<string>
                ('message', '');
              ASuccess := false;
              AMessage := LStatusMsg;
            end;
          except
            on E: Exception do
            begin
              Error(E);
              ASuccess := false;
              AMessage := E.Message;
            end;
          end;
        end;
      end);

    if Assigned(AOnDuoAccountsProc) then
    begin
      AOnDuoAccountsProc(LAccounts, LMessage, LResult, LOwnsObjects);
    end;
    if Assigned(FOnGetAccounts) then
    begin
      FOnGetAccounts(Self, LAccounts, LMessage, LResult, LOwnsObjects);
    end;

  finally
    if LOwnsObjects then
      FreeAndNil(LAccounts);
  end;

end;

function TDUOAccountsApi.GetAdminBaseResource: string;
begin
  Result := '/admin/v1/';
end;

function TDUOAccountsApi.GetBaseResource: string;
begin
  Result := '/accounts/v1/';
end;

procedure TDUOAccountsApi.GetBilling(AAccountID, AAPIHostname: string;
AOnDuoBillingProc: TOnDuoBillingProc);
var
  LEdition: string;
  LResult: boolean;
  LMessage: string;
  LAPIHostName: string;
begin
  LEdition := '';
  LAPIHostName := Self.Hostname;
  try
    Hostname := AAPIHostname;
    LResult := ExecuteRequest(LMessage, GetAdminBaseResource +
      'billing/edition', rmGet,
      procedure(ARESTRequest: TRESTRequest)
      begin
        ARESTRequest.AddParameter('account_id', AAccountID,
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end,
      procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
        var AMessage: string)
      var
        LResult, LStatusMsg: string;
      begin
        if ASuccess then
        begin
          try
            LResult := ARESTResponse.JSONValue.GetValue<string>('stat', '');
            if (SameText(LResult, 'Ok')) then
            begin
              LEdition := ARESTResponse.JSONValue.GetValue<string>
                ('response.edition', '');
              ASuccess := true;
            end
            else
            begin
              LStatusMsg := ARESTResponse.JSONValue.GetValue<string>
                ('message', '');
              ASuccess := false;
              AMessage := LStatusMsg;
            end;
          except
            on E: Exception do
            begin
              Error(E);
              ASuccess := false;
              AMessage := E.Message;
            end;
          end;
        end;
      end);
    if Assigned(AOnDuoBillingProc) then
    begin
      AOnDuoBillingProc(AAccountID, LEdition, LMessage, LResult);
    end;
    if Assigned(FOnGetBilling) then
    begin
      FOnGetBilling(Self, AAccountID, LEdition, LMessage, LResult);
    end;
  finally
    Hostname := LAPIHostName;
  end;

end;

procedure TDUOAccountsApi.GetTelephonyCredits(AAccountID, AAPIHostname: string;
AOnDuoTelephonyCreditsProc: TOnDuoTelephonyCreditsProc);
var
  LCredits: integer;
  LResult: boolean;
  LMessage: string;
  LAPIHostName: string;
begin
  LCredits := 0;
  LAPIHostName := Self.Hostname;
  try
    Hostname := AAPIHostname;
    LResult := ExecuteRequest(LMessage, GetAdminBaseResource +
      'billing/telephony_credits', rmGet,
      procedure(ARESTRequest: TRESTRequest)
      begin
        ARESTRequest.AddParameter('account_id', AAccountID,
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end,
      procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
        var AMessage: string)
      var
        LResult, LStatusMsg: string;
      begin
        if ASuccess then
        begin
          try
            LResult := ARESTResponse.JSONValue.GetValue<string>('stat', '');
            if (SameText(LResult, 'Ok')) then
            begin
              LCredits := ARESTResponse.JSONValue.GetValue<integer>
                ('response.credits', 0);
              ASuccess := true;
            end
            else
            begin
              LStatusMsg := ARESTResponse.JSONValue.GetValue<string>
                ('message', '');
              ASuccess := false;
              AMessage := LStatusMsg;
            end;
          except
            on E: Exception do
            begin
              Error(E);
              ASuccess := false;
              AMessage := E.Message;
            end;
          end;
        end;
      end);

    if Assigned(AOnDuoTelephonyCreditsProc) then
    begin
      AOnDuoTelephonyCreditsProc(AAccountID, LCredits, LMessage, LResult);
    end;
    if Assigned(FOnGetTelphonyCredits) then
    begin
      FOnGetTelphonyCredits(Self, AAccountID, LCredits, LMessage, LResult);
    end;
  finally
    Hostname := LAPIHostName;
  end;
end;

procedure TDUOAccountsApi.SetOnGetAccounts(const Value: TOnDuoAccountsEvent);
begin
  FOnGetAccounts := Value;
end;

procedure TDUOAccountsApi.SetOnGetBilling(const Value: TOnDuoBillingEvent);
begin
  FOnGetBilling := Value;
end;

procedure TDUOAccountsApi.SetOnGetTelphonyCredits
  (const Value: TOnDuoTelephonyCreditsEvent);
begin
  FOnGetTelphonyCredits := Value;
end;

end.
