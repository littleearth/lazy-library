unit SASBOSS.Api;

interface

uses
  System.Classes, System.SysUtils,
  Lazy.Types, Lazy.RESTClient, Lazy.REST.Types, SASBOSS.Models,
  REST.Types;

const
  SASBOSS_PROVISIONING_URL_DEFAULT = 'https://api.sasboss.com.au:10000/';
  SASBOSS_BILLING_URL_DEFAULT = 'https://api.sasboss.com.au:10001/';
  SASBOSS_TOKEN_EXPIRE_HOURS = 2;
  SASBOSS_RECORD_LIMIT_DEFAULT = 50;

type
  TLZSASBOSSURLMode = (sbProvisioning, sbBilling);

  TOnSASBOSSAuthenticateProc = reference to procedure(AMessage: string;
    ASuccess: boolean; AToken: string; AAPIUser: string; ARoleType: string);

  TOnSASBOSSEnterprisesProc = reference to procedure
    (AData: TLZSASBOSSEnterprises; AMessage: string; ASuccess: boolean;
    var AOwnsObjects: boolean);

  TOnSASBOSSInvoicesProc = reference to procedure(AData: TLZSASBOSSInvoices;
    AMessage: string; ASuccess: boolean; var AOwnsObjects: boolean);
  TOnSASBOSSInvoiceChargesProc = reference to procedure
    (AData: TLZSASBOSSInvoiceItems; AMessage: string; ASuccess: boolean;
    var AOwnsObjects: boolean);
  TOnSASBOSSGroupsProc = reference to procedure(AData: TLZSASBOSSGroups;
    AMessage: string; ASuccess: boolean; var AOwnsObjects: boolean);

  TLZSASBOSSClientBase = class(TLZRESTClient)
  private
    FURLMode: TLZSASBOSSURLMode;
    FToken: string;
    FAPIUser: string;
    FRoleType: string;
    FTokenDateTime: TDateTime;
    FSASBOSSProvisioningURL: string;
    FSASBOSSBillingURL: string;
    FTokenExpireHours: integer;
    procedure SetSASBOSSBILLINGURL(const Value: string);
    procedure SetSASBOSSProvisioningURL(const Value: string);
    function GetAuthenticate: boolean;
    procedure SetTokenExpireHours(const Value: integer);
  protected
    function GetBaseURL: string; override;
    procedure SetDefaults; override;
    property SASBOSSProvisioningURL: string read FSASBOSSProvisioningURL
      write SetSASBOSSProvisioningURL;
    property SASBOSSBillingURL: string read FSASBOSSBillingURL
      write SetSASBOSSBILLINGURL;
    property TokenExpireHours: integer read FTokenExpireHours
      write SetTokenExpireHours;
    procedure BeforeRESTRequest(
      ARESTRequest: TRESTRequest;
      ACustomData: string); override;

    procedure GetAllList<T: TLZSASBOSSModel>(
      AURL: string;
      AList: TLZSASBOSSModelList<T>;
      var ASuccess: boolean;
      var AMessage: string;
      AURLMode: TLZSASBOSSURLMode = sbProvisioning;
      ALimit: integer = SASBOSS_RECORD_LIMIT_DEFAULT);

    procedure GetList<T: TLZSASBOSSModel>(
      AURL: string;
      AList: TLZSASBOSSModelList<T>;
      var ASuccess: boolean;
      var AMessage: string;
      AURLMode: TLZSASBOSSURLMode = sbProvisioning;
      AOffset: integer = 0;
      ALimit: integer = -1);
  public
    procedure Authenticate(
      AUsername: string;
      APassword: string;
      AAuthenticateProc: TOnSASBOSSAuthenticateProc = nil;
      AASync: TLZAsyncState = lasDefault);
    procedure ClearAuthentication;
    procedure GetEnterprises(AEnterprisesProc: TOnSASBOSSEnterprisesProc = nil);
    procedure GetAllGroups(
      AOnData: TOnSASBOSSGroupsProc;
      ALimit: integer = SASBOSS_RECORD_LIMIT_DEFAULT);
    procedure GetGroups(
      AOnData: TOnSASBOSSGroupsProc;
      AOffset: integer;
      ALimit: integer = SASBOSS_RECORD_LIMIT_DEFAULT);
    property IsAuthenticated: boolean read GetAuthenticate;
  end;

  TLZSASBOSSProvisioningClient = class(TLZSASBOSSClientBase)
  public
    property SASBOSSProvisioningURL;
    property SASBOSSBillingURL;
  end;

  TLZSASBOSSBillingClient = class(TLZSASBOSSClientBase)
  public
    property SASBOSSProvisioningURL;
    property SASBOSSBillingURL;
    procedure GetAllEnterpriseInvoices(
      AEnterpriseID: string;
      AOnData: TOnSASBOSSInvoicesProc;
      ALimit: integer = SASBOSS_RECORD_LIMIT_DEFAULT);
    procedure GetEnterpriseInvoices(
      AEnterpriseID: string;
      AOnData: TOnSASBOSSInvoicesProc;
      AOffset: integer;
      ALimit: integer = SASBOSS_RECORD_LIMIT_DEFAULT);
    procedure GetEnterpriseInvoiceCharges(
      AEnterpriseID: string;
      AInvoiceID: string;
      AOnData: TOnSASBOSSInvoiceChargesProc);
  end;

implementation

uses Lazy.Utils, System.DateUtils, System.StrUtils, Lazy.CSVtoJSON;

{ TLZSASBOSSClientBase }

procedure TLZSASBOSSClientBase.Authenticate(
  AUsername, APassword: string;
  AAuthenticateProc: TOnSASBOSSAuthenticateProc;
  AASync: TLZAsyncState);
var
  LURL: string;
begin
  ClearAuthentication;
  FURLMode := sbProvisioning;
  LURL := Format('token/?apiUser=%s&apiPass=%s', [AUsername, APassword]);
  Get(LURL,
    procedure(ASender: TObject; ASuccess: boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        ARESTResponse.JSONValue.TryGetValue<string>('token', FToken);
        ARESTResponse.JSONValue.TryGetValue<string>('apiUser', FAPIUser);
        ARESTResponse.JSONValue.TryGetValue<string>('roleType', FRoleType);
        if not TLZString.IsEmptyString(FToken) then
        begin
          FTokenDateTime := Now;
        end;
      end;
      if Assigned(AAuthenticateProc) then
      begin
        AAuthenticateProc(AMessage, ASuccess, FToken, FAPIUser, FRoleType);
      end;
    end, '', AASync);
end;

procedure TLZSASBOSSClientBase.BeforeRESTRequest(
  ARESTRequest: TRESTRequest;
  ACustomData: string);
begin
  inherited;
  if IsAuthenticated then
  begin
    ARESTRequest.Params.AddItem('X-Token', FToken,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    ARESTRequest.Params.AddItem('X-API-User', FAPIUser,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  end;
end;

procedure TLZSASBOSSClientBase.ClearAuthentication;
begin
  FToken := '';
  FAPIUser := '';
  FRoleType := '';
  FTokenDateTime := 0;
end;

function TLZSASBOSSClientBase.GetAuthenticate: boolean;
var
  LNow: TDateTime;
begin
  Result := not TLZString.IsEmptyString(FToken);
  if Result then
  begin
    LNow := Now;
    Log(Format('Now: %s, Token date time: %s, Token expire hours: %d',
      [DateTimeToStr(LNow), DateTimeToStr(FTokenDateTime), FTokenExpireHours]));
    if HoursBetween(LNow, FTokenDateTime) > FTokenExpireHours then
    begin
      Log('Token has (probably) expired');
      Result := False;
    end;
  end;
end;

function TLZSASBOSSClientBase.GetBaseURL: string;
begin
  case FURLMode of
    sbProvisioning:
      Result := FSASBOSSProvisioningURL;
    sbBilling:
      Result := FSASBOSSBillingURL;
  end;
end;

procedure TLZSASBOSSClientBase.GetEnterprises(AEnterprisesProc
  : TOnSASBOSSEnterprisesProc);
var
  LData: TLZSASBOSSEnterprises;
  LSuccess, LOwnsObjects: boolean;
  LMessage: string;
begin
  LData := TLZSASBOSSEnterprises.Create;
  LSuccess := False;
  LMessage := '';
  LOwnsObjects := true;
  try
    GetList<TLZSASBOSSEnterprise>('provisioning/enterprise',
      LData as TLZSASBOSSModelList<TLZSASBOSSEnterprise>, LSuccess, LMessage,
      sbProvisioning);
    if Assigned(AEnterprisesProc) then
      AEnterprisesProc(LData, LMessage, LSuccess, LOwnsObjects);
  finally
    if LOwnsObjects then
      FreeAndNil(LData);
  end;
end;

procedure TLZSASBOSSClientBase.GetGroups(
  AOnData: TOnSASBOSSGroupsProc;
  AOffset, ALimit: integer);
var
  LURL: string;
  LGroups: TLZSASBOSSGroups;
  LOwnsObjects, LSuccess: boolean;
  LMessage: string;
begin
  LOwnsObjects := true;
  LGroups := TLZSASBOSSGroups.Create;
  LURL := 'provisioning/group';
  try
    GetList<TLZSASBOSSGroup>(LURL,
      LGroups as TLZSASBOSSModelList<TLZSASBOSSGroup>, LSuccess, LMessage,
      sbProvisioning, AOffset, ALimit);
    if Assigned(AOnData) then
      AOnData(LGroups, LMessage, LSuccess, LOwnsObjects);
  finally
    if LOwnsObjects then
    begin
      FreeAndNil(LGroups);
    end;
  end;
end;

procedure TLZSASBOSSClientBase.GetList<T>(
  AURL: string;
  AList: TLZSASBOSSModelList<T>;
  var ASuccess: boolean;
  var AMessage: string;
  AURLMode: TLZSASBOSSURLMode;
  AOffset, ALimit: integer);
var
  LURL: string;
  LSuccess: boolean;
  LMessage: string;
  LLimit, LOffset: integer;
  LList: TLZSASBOSSModelList<T>;
begin
  FURLMode := AURLMode;
  LList := AList;
  LLimit := ALimit;
  LSuccess := true;
  LMessage := '';
  LOffset := AOffset;
  if LLimit <> -1 then
  begin
    LURL := Format('%s%slimit=%d&offset=%d',
      [AURL, IfThen(Pos('?', AURL) > 0, '&', '?'), LLimit, LOffset]);
  end
  else
  begin
    LURL := AURL;
  end;
  Get(LURL,
    procedure(ASender: TObject; ASuccess: boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      LSuccess := ASuccess;
      LMessage := AMessage;
      LList.FromJSONValue(ARESTResponse.JSONValue, False);
    end, '', lasFalse);
  ASuccess := LSuccess;
  AMessage := LMessage;
end;

procedure TLZSASBOSSClientBase.GetAllGroups(
  AOnData: TOnSASBOSSGroupsProc;
  ALimit: integer);
var
  LURL: string;
  LData: TLZSASBOSSGroups;
  LOwnsObjects, LSuccess: boolean;
  LMessage: string;
begin
  LOwnsObjects := true;
  LData := TLZSASBOSSGroups.Create;
  LURL := 'provisioning/group';
  try
    GetAllList<TLZSASBOSSGroup>(LURL,
      LData as TLZSASBOSSModelList<TLZSASBOSSGroup>, LSuccess, LMessage,
      sbProvisioning, ALimit);
    if Assigned(AOnData) then
      AOnData(LData, LMessage, LSuccess, LOwnsObjects);
  finally
    if LOwnsObjects then
    begin
      FreeAndNil(LData);
    end;
  end;
end;

procedure TLZSASBOSSClientBase.GetAllList<T>(
  AURL: string;
  AList: TLZSASBOSSModelList<T>;
  var ASuccess: boolean;
  var AMessage: string;
  AURLMode: TLZSASBOSSURLMode;
  ALimit: integer);
var
  LLimit, LOffset: integer;
  LItemCount: integer;
begin
  AList.Clear;
  LLimit := ALimit;
  LOffset := 0;
  Repeat
    LItemCount := AList.Count;
    GetList<T>(AURL, AList, ASuccess, AMessage, AURLMode, LOffset, LLimit);
    if ASuccess then
    begin
      if (LItemCount = AList.Count) then
      begin
        // Nothing was added
        LOffset := -1;
      end
      else
      begin
        LOffset := AList.Count;
      end;
    end
    else
    begin
      ASuccess := False;
      LOffset := -1;
    end;
  Until (LOffset = -1);
end;

procedure TLZSASBOSSClientBase.SetDefaults;
begin
  inherited;
  FSASBOSSProvisioningURL := SASBOSS_PROVISIONING_URL_DEFAULT;
  FSASBOSSBillingURL := SASBOSS_BILLING_URL_DEFAULT;
  FTokenExpireHours := SASBOSS_TOKEN_EXPIRE_HOURS;
end;

procedure TLZSASBOSSClientBase.SetSASBOSSBILLINGURL(const Value: string);
begin
  FSASBOSSBillingURL := Value;
end;

procedure TLZSASBOSSClientBase.SetSASBOSSProvisioningURL(const Value: string);
begin
  FSASBOSSProvisioningURL := Value;
end;

procedure TLZSASBOSSClientBase.SetTokenExpireHours(const Value: integer);
begin
  FTokenExpireHours := Value;
end;

{ TLZSASBOSSBillingClient }

procedure TLZSASBOSSBillingClient.GetAllEnterpriseInvoices(
  AEnterpriseID: string;
  AOnData: TOnSASBOSSInvoicesProc;
  ALimit: integer);
var
  LURL: string;
  LData: TLZSASBOSSInvoices;
  LOwnsObjects, LSuccess: boolean;
  LMessage: string;
begin
  LOwnsObjects := true;
  LData := TLZSASBOSSInvoices.Create;
  LURL := Format('billing/enterprise/%s/invoice', [AEnterpriseID]);
  try
    GetAllList<TLZSASBOSSInvoice>(LURL,
      LData as TLZSASBOSSModelList<TLZSASBOSSInvoice>, LSuccess, LMessage,
      sbBilling, ALimit);
    if Assigned(AOnData) then
      AOnData(LData, LMessage, LSuccess, LOwnsObjects);
  finally
    if LOwnsObjects then
    begin
      FreeAndNil(LData);
    end;
  end;
end;

procedure TLZSASBOSSBillingClient.GetEnterpriseInvoiceCharges(
  AEnterpriseID, AInvoiceID: string;
  AOnData: TOnSASBOSSInvoiceChargesProc);
var
  LURL: string;
begin
  FURLMode := sbBilling;
  LURL := Format('billing/enterprise/%s/invoice/%s?type=csv',
    [AEnterpriseID, AInvoiceID]);
  Get(LURL,
    procedure(ASender: TObject; ASuccess: boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    var
      LData: TLZSASBOSSInvoiceItems;
      LOwnsObjects: boolean;
    begin
      LData := TLZSASBOSSInvoiceItems.Create;
      LOwnsObjects := true;
      try
        if ASuccess then
        begin
          LData.FromCSV(ARESTResponse.Content);
        end;
        if Assigned(AOnData) then
        begin
          AOnData(LData, AMessage, ASuccess, LOwnsObjects);
        end;
      finally
        if LOwnsObjects then
        begin
          FreeAndNil(LData);
        end;
      end;
    end, '', lasFalse, False);
end;

procedure TLZSASBOSSBillingClient.GetEnterpriseInvoices(
  AEnterpriseID: string;
  AOnData: TOnSASBOSSInvoicesProc;
  AOffset, ALimit: integer);
var
  LURL: string;
  LData: TLZSASBOSSInvoices;
  LOwnsObjects, LSuccess: boolean;
  LMessage: string;
begin
  LOwnsObjects := true;
  LData := TLZSASBOSSInvoices.Create;
  LURL := Format('billing/enterprise/%s/invoice', [AEnterpriseID]);
  try
    GetList<TLZSASBOSSInvoice>(LURL,
      LData as TLZSASBOSSModelList<TLZSASBOSSInvoice>, LSuccess, LMessage,
      sbProvisioning, AOffset, ALimit);
    if Assigned(AOnData) then
      AOnData(LData, LMessage, LSuccess, LOwnsObjects);
  finally
    if LOwnsObjects then
    begin
      FreeAndNil(LData);
    end;
  end;
end;

end.
