unit SASBOSS.Models;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  System.JSON, System.Generics.Collections, Lazy.Types, Lazy.Log, Lazy.Model;

type
  TLZSASBOSSModel = class(TLZModel)
  protected
    function GetJSONDateValue(
      AJSONValue: TJSONValue;
      AName: string;
      ADefault: TDateTime = 0): TDateTime;
    function GetJSONBooleanValue(
      AJSONValue: TJSONValue;
      AName: string;
      ADefault: Boolean): Boolean;
  end;

  TLZSASBOSSModelList<T: TLZSASBOSSModel> = class(TLZModelList<T>)
  protected
    function GetDefaultArrayName: string; override;
  end;

  TLZSASBOSSEnterprise = class(TLZSASBOSSModel)
  private
    FEnterpriseName: string;
    FEnterpriseID: string;
    FEnterpriseStatus: string;
    procedure SetEnterpriseID(const Value: string);
    procedure SetEnterpriseName(const Value: string);
    procedure SetEnterpriseStatus(const Value: string);

  protected
    procedure SetDefaults; override;
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property EnterpriseName: string read FEnterpriseName
      write SetEnterpriseName;
    property EnterpriseID: string read FEnterpriseID write SetEnterpriseID;
    property EnterpriseStatus: string read FEnterpriseStatus
      write SetEnterpriseStatus;
  end;

  TLZSASBOSSEnterprises = class(TLZSASBOSSModelList<TLZSASBOSSEnterprise>)
  protected
    function GetDefaultArrayName: string; override;
  public
    function Find(AID: string): TLZSASBOSSEnterprise;
    function FindByName(AName: string): TLZSASBOSSEnterprise;
  end;

  TLZSASBOSSGroup = class(TLZSASBOSSModel)
  private
    FEnterpriseID: string;
    FExternalServiceRefId: string;
    FSandboxCleared: Boolean;
    FGroupID: string;
    FGroupStatus: string;
    FGroupShortname: string;
    FSandboxExpiryDate: TDateTime;
    FCallingLineIdPhoneNumber: string;
    FExternalBillingRefId: string;
    FIsSandboxDateApproved: Boolean;
    FServiceCount: integer;
    FSandboxTestPhaseEnabled: Boolean;
    FGroupName: string;
    procedure SetCallingLineIdPhoneNumber(const Value: string);
    procedure SetEnterpriseID(const Value: string);
    procedure SetExternalBillingRefId(const Value: string);
    procedure SetExternalServiceRefId(const Value: string);
    procedure SetGroupID(const Value: string);
    procedure SetGroupName(const Value: string);
    procedure SetGroupShortname(const Value: string);
    procedure SetGroupStatus(const Value: string);
    procedure SetIsSandboxDateApproved(const Value: Boolean);
    procedure SetSandboxCleared(const Value: Boolean);
    procedure SetSandboxExpiryDate(const Value: TDateTime);
    procedure SetSandboxTestPhaseEnabled(const Value: Boolean);
    procedure SetServiceCount(const Value: integer);
  protected
    procedure SetDefaults; override;
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property GroupID: string read FGroupID write SetGroupID;
    property GroupName: string read FGroupName write SetGroupName;
    property EnterpriseID: string read FEnterpriseID write SetEnterpriseID;
    property GroupShortname: string read FGroupShortname
      write SetGroupShortname;
    property GroupStatus: string read FGroupStatus write SetGroupStatus;
    property ExternalServiceRefId: string read FExternalServiceRefId
      write SetExternalServiceRefId;
    property ExternalBillingRefId: string read FExternalBillingRefId
      write SetExternalBillingRefId;
    property SandboxExpiryDate: TDateTime read FSandboxExpiryDate
      write SetSandboxExpiryDate;
    property IsSandboxDateApproved: Boolean read FIsSandboxDateApproved
      write SetIsSandboxDateApproved;
    property SandboxTestPhaseEnabled: Boolean read FSandboxTestPhaseEnabled
      write SetSandboxTestPhaseEnabled;
    property SandboxCleared: Boolean read FSandboxCleared
      write SetSandboxCleared;
    property ServiceCount: integer read FServiceCount write SetServiceCount;
    property CallingLineIdPhoneNumber: string read FCallingLineIdPhoneNumber
      write SetCallingLineIdPhoneNumber;

  end;

  TLZSASBOSSGroups = class(TLZSASBOSSModelList<TLZSASBOSSGroup>)
  protected
    function GetDefaultArrayName: string; override;
  public
    function Find(AID: string): TLZSASBOSSGroup;
  end;

  TLZSASBOSSInvoice = class(TLZSASBOSSModel)
  private
    FContactEmail: string;
    FContactLastName: string;
    FLoginResellerId: string;
    FPaidAmount: Double;
    FDeleteReason: string;
    FInvoiceAmountDue: Double;
    FDeletedDate: TDateTime;
    FLoginId: string;
    FContactId: string;
    FDeleteNote: string;
    FEnterpriseName: string;
    FDateIssued: TDateTime;
    FID: string;
    FPaidDate: TDateTime;
    FContactFirstName: string;
    FInvoiceGSTDue: Double;
    FInvoiceDueDate: TDateTime;
    FTotalAmountDue: Double;
    FEnterpriseID: string;
    FAdjustmentNoteTotal: Double;
    FReference: string;
    FAdjustmentNoteTotalGst: Double;
    FDeletedByContactEmail: string;
    FGroupID: string;
    procedure SetAdjustmentNoteTotal(const Value: Double);
    procedure SetAdjustmentNoteTotalGst(const Value: Double);
    procedure SetContactEmail(const Value: string);
    procedure SetContactFirstName(const Value: string);
    procedure SetContactId(const Value: string);
    procedure SetContactLastName(const Value: string);
    procedure SetDateIssued(const Value: TDateTime);
    procedure SetDeletedByContactEmail(const Value: string);
    procedure SetDeletedDate(const Value: TDateTime);
    procedure SetDeleteNote(const Value: string);
    procedure SetDeleteReason(const Value: string);
    procedure SetEnterpriseID(const Value: string);
    procedure SetEnterpriseName(const Value: string);
    procedure SetID(const Value: string);
    procedure SetInvoiceAmountDue(const Value: Double);
    procedure SetInvoiceDueDate(const Value: TDateTime);
    procedure SetInvoiceGSTDue(const Value: Double);
    procedure SetLoginId(const Value: string);
    procedure SetLoginResellerId(const Value: string);
    procedure SetPaidAmount(const Value: Double);
    procedure SetPaidDate(const Value: TDateTime);
    procedure SetReference(const Value: string);
    procedure SetTotalAmountDue(const Value: Double);
    procedure SetGroupID(const Value: string);
  protected
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
    procedure SetDefaults; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property ID: string read FID write SetID;
    property Reference: string read FReference write SetReference;
    property EnterpriseID: string read FEnterpriseID write SetEnterpriseID;
    property GroupID: string read FGroupID write SetGroupID;
    property TotalAmountDue: Double read FTotalAmountDue
      write SetTotalAmountDue;
    property InvoiceAmountDue: Double read FInvoiceAmountDue
      write SetInvoiceAmountDue;
    property InvoiceGSTDue: Double read FInvoiceGSTDue write SetInvoiceGSTDue;
    property PaidAmount: Double read FPaidAmount write SetPaidAmount;
    property AdjustmentNoteTotal: Double read FAdjustmentNoteTotal
      write SetAdjustmentNoteTotal;
    property AdjustmentNoteTotalGst: Double read FAdjustmentNoteTotalGst
      write SetAdjustmentNoteTotalGst;
    property DateIssued: TDateTime read FDateIssued write SetDateIssued;
    property PaidDate: TDateTime read FPaidDate write SetPaidDate;
    property InvoiceDueDate: TDateTime read FInvoiceDueDate
      write SetInvoiceDueDate;
    property EnterpriseName: string read FEnterpriseName
      write SetEnterpriseName;
    property LoginId: string read FLoginId write SetLoginId;
    property ContactFirstName: string read FContactFirstName
      write SetContactFirstName;
    property ContactLastName: string read FContactLastName
      write SetContactLastName;
    property ContactEmail: string read FContactEmail write SetContactEmail;
    property ContactId: string read FContactId write SetContactId;
    property LoginResellerId: string read FLoginResellerId
      write SetLoginResellerId;
    property DeleteReason: string read FDeleteReason write SetDeleteReason;
    property DeleteNote: string read FDeleteNote write SetDeleteNote;
    property DeletedByContactEmail: string read FDeletedByContactEmail
      write SetDeletedByContactEmail;
    property DeletedDate: TDateTime read FDeletedDate write SetDeletedDate;
  end;

  TLZSASBOSSInvoices = class(TLZSASBOSSModelList<TLZSASBOSSInvoice>)
  protected
    function GetDefaultArrayName: string; override;
  public
    function Find(AID: string): TLZSASBOSSInvoice;
  end;

  TLZSASBOSSInvoiceItem = class(TLZSASBOSSModel)
  private
    FProRate: Double;
    FServiceRef: string;
    FPeriodStart: TDateTime;
    FUnitPrice: Double;
    FQty: Double;
    FInvoiceId: string;
    FEnterpriseName: string;
    FGLAccount: string;
    FTotalExGst: Double;
    FGstRate: Double;
    FGroupRef: string;
    FDIDNumber: string;
    FPeriodEnd: TDateTime;
    FInvoiceCategory: string;
    FServicePhoneNumber: string;
    FDescription: string;
    FItemCode: string;
    FTotalIncGst: Double;
    FEnterpriseRef: string;
    FRecurringChargeId: string;
    FInvoiceItemId: string;
    FGroupName: string;
    procedure SetDescription(const Value: string);
    procedure SetDIDNumber(const Value: string);
    procedure SetEnterpriseName(const Value: string);
    procedure SetEnterpriseRef(const Value: string);
    procedure SetGLAccount(const Value: string);
    procedure SetGroupName(const Value: string);
    procedure SetGroupRef(const Value: string);
    procedure SetGstRate(const Value: Double);
    procedure SetInvoiceCategory(const Value: string);
    procedure SetInvoiceId(const Value: string);
    procedure SetInvoiceItemId(const Value: string);
    procedure SetItemCode(const Value: string);
    procedure SetPeriodEnd(const Value: TDateTime);
    procedure SetPeriodStart(const Value: TDateTime);
    procedure SetProRate(const Value: Double);
    procedure SetQty(const Value: Double);
    procedure SetRecurringChargeId(const Value: string);
    procedure SetServicePhoneNumber(const Value: string);
    procedure SetServiceRef(const Value: string);
    procedure SetTotalExGst(const Value: Double);
    procedure SetTotalIncGst(const Value: Double);
    procedure SetUnitPrice(const Value: Double);
  protected
    procedure SetDefaults; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property EnterpriseName: string read FEnterpriseName
      write SetEnterpriseName;
    property EnterpriseRef: string read FEnterpriseRef write SetEnterpriseRef;
    property GroupName: string read FGroupName write SetGroupName;
    property GroupRef: string read FGroupRef write SetGroupRef;
    property ServicePhoneNumber: string read FServicePhoneNumber
      write SetServicePhoneNumber;
    property ServiceRef: string read FServiceRef write SetServiceRef;
    property InvoiceId: string read FInvoiceId write SetInvoiceId;
    property InvoiceItemId: string read FInvoiceItemId write SetInvoiceItemId;
    property Description: string read FDescription write SetDescription;
    property ProRate: Double read FProRate write SetProRate;
    property Qty: Double read FQty write SetQty;
    property UnitPrice: Double read FUnitPrice write SetUnitPrice;
    property GstRate: Double read FGstRate write SetGstRate;
    property TotalExGst: Double read FTotalExGst write SetTotalExGst;
    property TotalIncGst: Double read FTotalIncGst write SetTotalIncGst;
    property PeriodStart: TDateTime read FPeriodStart write SetPeriodStart;
    property PeriodEnd: TDateTime read FPeriodEnd write SetPeriodEnd;
    property GLAccount: string read FGLAccount write SetGLAccount;
    property ItemCode: string read FItemCode write SetItemCode;
    property InvoiceCategory: string read FInvoiceCategory
      write SetInvoiceCategory;
    property DIDNumber: string read FDIDNumber write SetDIDNumber;
    property RecurringChargeId: string read FRecurringChargeId
      write SetRecurringChargeId;
  end;

  TLZSASBOSSInvoiceItems = class(TLZSASBOSSModelList<TLZSASBOSSInvoiceItem>)
  protected
    function GetDefaultArrayName: string; override;
  public
    function Find(AID: string): TLZSASBOSSInvoiceItem;
    procedure FromCSV(ACSV: string);
  end;

implementation

uses System.StrUtils, Lazy.Utils, Lazy.CSVtoJSON, Lazy.ISO8601;

{ TLZSASBOSSModelList<T> }

function TLZSASBOSSModelList<T>.GetDefaultArrayName: string;
begin
  Result := '';
end;

{ TLZSASBOSSEnterprise }

procedure TLZSASBOSSEnterprise.AfterModelCreated;
begin
  inherited;

end;

procedure TLZSASBOSSEnterprise.BeforeModelDestroyed;
begin
  inherited;

end;

procedure TLZSASBOSSEnterprise.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  SetDefaults;
  AJSONValue.TryGetValue<string>('enterpriseId', FEnterpriseID);
  AJSONValue.TryGetValue<string>('enterpriseName', FEnterpriseName);
  AJSONValue.TryGetValue<string>('enterpriseStatus', FEnterpriseStatus);
end;

function TLZSASBOSSEnterprise.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FEnterpriseID) then
      LJSONObject.AddPair('enterpriseId', FEnterpriseID);
    
    if not TLZString.IsEmptyString(FEnterpriseName) then
      LJSONObject.AddPair('enterpriseName', FEnterpriseName);
    
    if not TLZString.IsEmptyString(FEnterpriseStatus) then
      LJSONObject.AddPair('enterpriseStatus', FEnterpriseStatus);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZSASBOSSEnterprise.SetDefaults;
begin
  inherited;
  FEnterpriseID := '';
  FEnterpriseName := '';
  FEnterpriseStatus := '';
end;

procedure TLZSASBOSSEnterprise.SetEnterpriseID(const Value: string);
begin
  FEnterpriseID := Value;
end;

procedure TLZSASBOSSEnterprise.SetEnterpriseName(const Value: string);
begin
  FEnterpriseName := Value;
end;

procedure TLZSASBOSSEnterprise.SetEnterpriseStatus(const Value: string);
begin
  FEnterpriseStatus := Value;
end;

{ TLZSASBOSSEnterprises }

function TLZSASBOSSEnterprises.Find(AID: string): TLZSASBOSSEnterprise;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].EnterpriseID, AID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZSASBOSSEnterprises.FindByName(AName: string): TLZSASBOSSEnterprise;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].EnterpriseName, AName) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZSASBOSSEnterprises.GetDefaultArrayName: string;
begin
  Result := '';
end;

{ TLZSASBOSSInvoice }

procedure TLZSASBOSSInvoice.AfterModelCreated;
begin
  inherited;

end;

procedure TLZSASBOSSInvoice.BeforeModelDestroyed;
begin
  inherited;

end;

procedure TLZSASBOSSInvoice.FromJSONValue(AJSONValue: TJSONValue);
begin
  SetDefaults;
  AJSONValue.TryGetValue<string>('id', FID);
  AJSONValue.TryGetValue<string>('enterpriseId', FEnterpriseID);
  AJSONValue.TryGetValue<string>('groupId', FGroupID);
  AJSONValue.TryGetValue<string>('enterpriseName', FEnterpriseName);
  AJSONValue.TryGetValue<string>('loginId', FLoginId);
  AJSONValue.TryGetValue<string>('contactFirstName', FContactFirstName);
  AJSONValue.TryGetValue<string>('contactLastName', FContactLastName);
  AJSONValue.TryGetValue<string>('contactEmail', FContactEmail);
  AJSONValue.TryGetValue<string>('contactId', FContactId);
  AJSONValue.TryGetValue<string>('loginResellerId', FLoginResellerId);
  AJSONValue.TryGetValue<string>('deleteReason', FDeleteReason);
  AJSONValue.TryGetValue<string>('deleteNote', FDeleteNote);
  AJSONValue.TryGetValue<string>('deletedByContactEmail',
    FDeletedByContactEmail);

  AJSONValue.TryGetValue<Double>('totalAmountDue', FTotalAmountDue);
  AJSONValue.TryGetValue<Double>('invoiceAmountDue', FInvoiceAmountDue);
  AJSONValue.TryGetValue<Double>('invoiceGstDue', FInvoiceGSTDue);
  AJSONValue.TryGetValue<Double>('paidAmount', FPaidAmount);
  AJSONValue.TryGetValue<Double>('adjustmentNoteTotal', FAdjustmentNoteTotal);
  AJSONValue.TryGetValue<Double>('adjustmentNoteTotalGst',
    FAdjustmentNoteTotalGst);

  FDateIssued := GetJSONDateValue(AJSONValue, 'dateIssued', FDateIssued);
  FInvoiceDueDate := GetJSONDateValue(AJSONValue, 'invoiceDueDate',
    FInvoiceDueDate);
  FPaidDate := GetJSONDateValue(AJSONValue, 'paidDate', FPaidDate);
  FDeletedDate := GetJSONDateValue(AJSONValue, 'deletedDate', FDeletedDate);

end;

function TLZSASBOSSInvoice.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FID) then
      LJSONObject.AddPair('id', FID);
    
    if not TLZString.IsEmptyString(FEnterpriseID) then
      LJSONObject.AddPair('enterpriseId', FEnterpriseID);
    
    if not TLZString.IsEmptyString(FGroupID) then
      LJSONObject.AddPair('groupId', FGroupID);
    
    if not TLZString.IsEmptyString(FEnterpriseName) then
      LJSONObject.AddPair('enterpriseName', FEnterpriseName);
    
    if not TLZString.IsEmptyString(FLoginId) then
      LJSONObject.AddPair('loginId', FLoginId);
    
    if not TLZString.IsEmptyString(FContactFirstName) then
      LJSONObject.AddPair('contactFirstName', FContactFirstName);
    
    if not TLZString.IsEmptyString(FContactLastName) then
      LJSONObject.AddPair('contactLastName', FContactLastName);
    
    if not TLZString.IsEmptyString(FContactEmail) then
      LJSONObject.AddPair('contactEmail', FContactEmail);
    
    if not TLZString.IsEmptyString(FContactId) then
      LJSONObject.AddPair('contactId', FContactId);
    
    if not TLZString.IsEmptyString(FLoginResellerId) then
      LJSONObject.AddPair('loginResellerId', FLoginResellerId);
    
    if not TLZString.IsEmptyString(FDeleteReason) then
      LJSONObject.AddPair('deleteReason', FDeleteReason);
    
    if not TLZString.IsEmptyString(FDeleteNote) then
      LJSONObject.AddPair('deleteNote', FDeleteNote);
    
    if not TLZString.IsEmptyString(FDeletedByContactEmail) then
      LJSONObject.AddPair('deletedByContactEmail', FDeletedByContactEmail);
    
    LJSONObject.AddPair('totalAmountDue', TJSONNumber.Create(FTotalAmountDue));
    LJSONObject.AddPair('invoiceAmountDue', TJSONNumber.Create(FInvoiceAmountDue));
    LJSONObject.AddPair('invoiceGstDue', TJSONNumber.Create(FInvoiceGSTDue));
    LJSONObject.AddPair('paidAmount', TJSONNumber.Create(FPaidAmount));
    LJSONObject.AddPair('adjustmentNoteTotal', TJSONNumber.Create(FAdjustmentNoteTotal));
    LJSONObject.AddPair('adjustmentNoteTotalGst', TJSONNumber.Create(FAdjustmentNoteTotalGst));
    
    if FDateIssued <> 0 then
      LJSONObject.AddPair('dateIssued', TLZIso8601.DateTimeToIso8601(FDateIssued, true));
    
    if FInvoiceDueDate <> 0 then
      LJSONObject.AddPair('invoiceDueDate', TLZIso8601.DateTimeToIso8601(FInvoiceDueDate, true));
    
    if FPaidDate <> 0 then
      LJSONObject.AddPair('paidDate', TLZIso8601.DateTimeToIso8601(FPaidDate, true));
    
    if FDeletedDate <> 0 then
      LJSONObject.AddPair('deletedDate', TLZIso8601.DateTimeToIso8601(FDeletedDate, true));
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZSASBOSSInvoice.SetAdjustmentNoteTotal(const Value: Double);
begin
  FAdjustmentNoteTotal := Value;
end;

procedure TLZSASBOSSInvoice.SetAdjustmentNoteTotalGst(const Value: Double);
begin
  FAdjustmentNoteTotalGst := Value;
end;

procedure TLZSASBOSSInvoice.SetContactEmail(const Value: string);
begin
  FContactEmail := Value;
end;

procedure TLZSASBOSSInvoice.SetContactFirstName(const Value: string);
begin
  FContactFirstName := Value;
end;

procedure TLZSASBOSSInvoice.SetContactId(const Value: string);
begin
  FContactId := Value;
end;

procedure TLZSASBOSSInvoice.SetContactLastName(const Value: string);
begin
  FContactLastName := Value;
end;

procedure TLZSASBOSSInvoice.SetDateIssued(const Value: TDateTime);
begin
  FDateIssued := Value;
end;

procedure TLZSASBOSSInvoice.SetDefaults;
begin
  inherited;
  FContactEmail := '';
  FContactLastName := '';
  FLoginResellerId := '';
  FPaidAmount := 0;
  FDeleteReason := '';
  FInvoiceAmountDue := 0;
  FDeletedDate := 0;
  FLoginId := '';
  FContactId := '';
  FDeleteNote := '';
  FEnterpriseName := '';
  FDateIssued := 0;
  FID := '';
  FPaidDate := 0;
  FContactFirstName := '';
  FInvoiceGSTDue := 0;
  FInvoiceDueDate := 0;
  FTotalAmountDue := 0;
  FEnterpriseID := '';
  FAdjustmentNoteTotal := 0;
  FReference := '';
  FAdjustmentNoteTotalGst := 0;
  FDeletedByContactEmail := '';
end;

procedure TLZSASBOSSInvoice.SetDeletedByContactEmail(const Value: string);
begin
  FDeletedByContactEmail := Value;
end;

procedure TLZSASBOSSInvoice.SetDeletedDate(const Value: TDateTime);
begin
  FDeletedDate := Value;
end;

procedure TLZSASBOSSInvoice.SetDeleteNote(const Value: string);
begin
  FDeleteNote := Value;
end;

procedure TLZSASBOSSInvoice.SetDeleteReason(const Value: string);
begin
  FDeleteReason := Value;
end;

procedure TLZSASBOSSInvoice.SetEnterpriseID(const Value: string);
begin
  FEnterpriseID := Value;
end;

procedure TLZSASBOSSInvoice.SetEnterpriseName(const Value: string);
begin
  FEnterpriseName := Value;
end;

procedure TLZSASBOSSInvoice.SetGroupID(const Value: string);
begin
  FGroupID := Value;
end;

procedure TLZSASBOSSInvoice.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZSASBOSSInvoice.SetInvoiceAmountDue(const Value: Double);
begin
  FInvoiceAmountDue := Value;
end;

procedure TLZSASBOSSInvoice.SetInvoiceDueDate(const Value: TDateTime);
begin
  FInvoiceDueDate := Value;
end;

procedure TLZSASBOSSInvoice.SetInvoiceGSTDue(const Value: Double);
begin
  FInvoiceGSTDue := Value;
end;

procedure TLZSASBOSSInvoice.SetLoginId(const Value: string);
begin
  FLoginId := Value;
end;

procedure TLZSASBOSSInvoice.SetLoginResellerId(const Value: string);
begin
  FLoginResellerId := Value;
end;

procedure TLZSASBOSSInvoice.SetPaidAmount(const Value: Double);
begin
  FPaidAmount := Value;
end;

procedure TLZSASBOSSInvoice.SetPaidDate(const Value: TDateTime);
begin
  FPaidDate := Value;
end;

procedure TLZSASBOSSInvoice.SetReference(const Value: string);
begin
  FReference := Value;
end;

procedure TLZSASBOSSInvoice.SetTotalAmountDue(const Value: Double);
begin
  FTotalAmountDue := Value;
end;

{ TLZSASBOSSInvoices }

function TLZSASBOSSInvoices.Find(AID: string): TLZSASBOSSInvoice;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].ID, AID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZSASBOSSInvoices.GetDefaultArrayName: string;
begin
  Result := 'invoices';
end;

{ TLZSASBOSSGroup }

procedure TLZSASBOSSGroup.AfterModelCreated;
begin
  inherited;

end;

procedure TLZSASBOSSGroup.BeforeModelDestroyed;
begin
  inherited;

end;

procedure TLZSASBOSSGroup.FromJSONValue(AJSONValue: TJSONValue);
begin
  SetDefaults;
  AJSONValue.TryGetValue<string>('groupId', FGroupID);
  AJSONValue.TryGetValue<string>('enterpriseId', FEnterpriseID);
  AJSONValue.TryGetValue<string>('groupName', FGroupName);
  AJSONValue.TryGetValue<string>('groupShortName', FGroupShortname);
  AJSONValue.TryGetValue<string>('externalServiceRefId', FExternalServiceRefId);
  AJSONValue.TryGetValue<string>('externalBillingRefId', FExternalBillingRefId);
  AJSONValue.TryGetValue<string>('groupStatus', FGroupStatus);
  AJSONValue.TryGetValue<string>('callingLineIdPhoneNumber',
    FCallingLineIdPhoneNumber);

  AJSONValue.TryGetValue<integer>('serviceCount', FServiceCount);

  FSandboxExpiryDate := GetJSONDateValue(AJSONValue, 'sandboxExpiryDate',
    FSandboxExpiryDate);

  FIsSandboxDateApproved := GetJSONBooleanValue(AJSONValue,
    'isSandboxDateApproved', FIsSandboxDateApproved);
  FSandboxTestPhaseEnabled := GetJSONBooleanValue(AJSONValue,
    'sandboxTestPhaseEnabled', FSandboxTestPhaseEnabled);
  FSandboxCleared := GetJSONBooleanValue(AJSONValue, 'sandboxCleared',
    FSandboxCleared);

end;

function TLZSASBOSSGroup.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FGroupID) then
      LJSONObject.AddPair('groupId', FGroupID);
    
    if not TLZString.IsEmptyString(FEnterpriseID) then
      LJSONObject.AddPair('enterpriseId', FEnterpriseID);
    
    if not TLZString.IsEmptyString(FGroupName) then
      LJSONObject.AddPair('groupName', FGroupName);
    
    if not TLZString.IsEmptyString(FGroupShortname) then
      LJSONObject.AddPair('groupShortName', FGroupShortname);
    
    if not TLZString.IsEmptyString(FExternalServiceRefId) then
      LJSONObject.AddPair('externalServiceRefId', FExternalServiceRefId);
    
    if not TLZString.IsEmptyString(FExternalBillingRefId) then
      LJSONObject.AddPair('externalBillingRefId', FExternalBillingRefId);
    
    if not TLZString.IsEmptyString(FGroupStatus) then
      LJSONObject.AddPair('groupStatus', FGroupStatus);
    
    if not TLZString.IsEmptyString(FCallingLineIdPhoneNumber) then
      LJSONObject.AddPair('callingLineIdPhoneNumber', FCallingLineIdPhoneNumber);
    
    LJSONObject.AddPair('serviceCount', TJSONNumber.Create(FServiceCount));
    
    if FSandboxExpiryDate <> 0 then
      LJSONObject.AddPair('sandboxExpiryDate', TLZIso8601.DateTimeToIso8601(FSandboxExpiryDate, true));
    
    LJSONObject.AddPair('isSandboxDateApproved', TJSONBool.Create(FIsSandboxDateApproved));
    LJSONObject.AddPair('sandboxTestPhaseEnabled', TJSONBool.Create(FSandboxTestPhaseEnabled));
    LJSONObject.AddPair('sandboxCleared', TJSONBool.Create(FSandboxCleared));
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZSASBOSSGroup.SetCallingLineIdPhoneNumber(const Value: string);
begin
  FCallingLineIdPhoneNumber := Value;
end;

procedure TLZSASBOSSGroup.SetDefaults;
begin
  FEnterpriseID := '';
  FExternalServiceRefId := '';
  FSandboxCleared := false;
  FGroupID := '';
  FGroupStatus := '';
  FGroupShortname := '';
  FSandboxExpiryDate := 0;
  FCallingLineIdPhoneNumber := '';
  FExternalBillingRefId := '';
  FIsSandboxDateApproved := false;
  FServiceCount := 0;
  FSandboxTestPhaseEnabled := false;
  FGroupName := '';

end;

procedure TLZSASBOSSGroup.SetEnterpriseID(const Value: string);
begin
  FEnterpriseID := Value;
end;

procedure TLZSASBOSSGroup.SetExternalBillingRefId(const Value: string);
begin
  FExternalBillingRefId := Value;
end;

procedure TLZSASBOSSGroup.SetExternalServiceRefId(const Value: string);
begin
  FExternalServiceRefId := Value;
end;

procedure TLZSASBOSSGroup.SetGroupID(const Value: string);
begin
  FGroupID := Value;
end;

procedure TLZSASBOSSGroup.SetGroupName(const Value: string);
begin
  FGroupName := Value;
end;

procedure TLZSASBOSSGroup.SetGroupShortname(const Value: string);
begin
  FGroupShortname := Value;
end;

procedure TLZSASBOSSGroup.SetGroupStatus(const Value: string);
begin
  FGroupStatus := Value;
end;

procedure TLZSASBOSSGroup.SetIsSandboxDateApproved(const Value: Boolean);
begin
  FIsSandboxDateApproved := Value;
end;

procedure TLZSASBOSSGroup.SetSandboxCleared(const Value: Boolean);
begin
  FSandboxCleared := Value;
end;

procedure TLZSASBOSSGroup.SetSandboxExpiryDate(const Value: TDateTime);
begin
  FSandboxExpiryDate := Value;
end;

procedure TLZSASBOSSGroup.SetSandboxTestPhaseEnabled(const Value: Boolean);
begin
  FSandboxTestPhaseEnabled := Value;
end;

procedure TLZSASBOSSGroup.SetServiceCount(const Value: integer);
begin
  FServiceCount := Value;
end;

{ TLZSASBOSSGroups }

function TLZSASBOSSGroups.Find(AID: string): TLZSASBOSSGroup;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].GroupID, AID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZSASBOSSGroups.GetDefaultArrayName: string;
begin
  Result := 'groups';
end;

{ TLZSASBOSSModel }

function TLZSASBOSSModel.GetJSONBooleanValue(
  AJSONValue: TJSONValue;
  AName: string;
  ADefault: Boolean): Boolean;
var
  LBoolStr: string;
begin
  Result := ADefault;
  if AJSONValue.TryGetValue<string>(AName, LBoolStr) then
  begin
    Result := SameText(LBoolStr, 'yes');
  end;
end;

function TLZSASBOSSModel.GetJSONDateValue(
  AJSONValue: TJSONValue;
  AName: string;
  ADefault: TDateTime): TDateTime;
var
  LDateStr: string;
begin
  Result := ADefault;
  if AJSONValue.TryGetValue<string>(AName, LDateStr) then
  begin
    Result := TLZDateTime.StringToDateTimeDef(LDateStr, ADefault);
  end;
end;

{ TLZSASBOSSInvoiceItem }

procedure TLZSASBOSSInvoiceItem.FromJSONValue(AJSONValue: TJSONValue);
begin
  SetDefaults;
  // String properties
  AJSONValue.TryGetValue<string>('EnterpriseName', FEnterpriseName);
  AJSONValue.TryGetValue<string>('EnterpriseRef', FEnterpriseRef);
  AJSONValue.TryGetValue<string>('GroupName', FGroupName);
  AJSONValue.TryGetValue<string>('GroupRef', FGroupRef);
  AJSONValue.TryGetValue<string>('ServicePhoneNumber', FServicePhoneNumber);
  AJSONValue.TryGetValue<string>('ServiceRef', FServiceRef);
  AJSONValue.TryGetValue<string>('InvoiceId', FInvoiceId);
  AJSONValue.TryGetValue<string>('InvoiceItemId', FInvoiceItemId);
  AJSONValue.TryGetValue<string>('Description', FDescription);
  AJSONValue.TryGetValue<string>('GLAccount', FGLAccount);
  AJSONValue.TryGetValue<string>('ItemCode', FItemCode);
  AJSONValue.TryGetValue<string>('InvoiceCategory', FInvoiceCategory);
  AJSONValue.TryGetValue<string>('DIDNumber', FDIDNumber);
  AJSONValue.TryGetValue<string>('RecurringChargeId', FRecurringChargeId);

  // Double properties
  AJSONValue.TryGetValue<Double>('ProRate', FProRate);
  AJSONValue.TryGetValue<Double>('Qty', FQty);
  AJSONValue.TryGetValue<Double>('UnitPrice', FUnitPrice);
  AJSONValue.TryGetValue<Double>('GstRate', FGstRate);
  AJSONValue.TryGetValue<Double>('TotalExGst', FTotalExGst);
  AJSONValue.TryGetValue<Double>('TotalIncGst', FTotalIncGst);

  // Date properties
  FPeriodStart := GetJSONDateValue(AJSONValue, 'PeriodStart', FPeriodStart);
  FPeriodEnd := GetJSONDateValue(AJSONValue, 'PeriodEnd', FPeriodEnd);

end;

function TLZSASBOSSInvoiceItem.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FEnterpriseName) then
      LJSONObject.AddPair('EnterpriseName', FEnterpriseName);
    
    if not TLZString.IsEmptyString(FEnterpriseRef) then
      LJSONObject.AddPair('EnterpriseRef', FEnterpriseRef);
    
    if not TLZString.IsEmptyString(FGroupName) then
      LJSONObject.AddPair('GroupName', FGroupName);
    
    if not TLZString.IsEmptyString(FGroupRef) then
      LJSONObject.AddPair('GroupRef', FGroupRef);
    
    if not TLZString.IsEmptyString(FServicePhoneNumber) then
      LJSONObject.AddPair('ServicePhoneNumber', FServicePhoneNumber);
    
    if not TLZString.IsEmptyString(FServiceRef) then
      LJSONObject.AddPair('ServiceRef', FServiceRef);
    
    if not TLZString.IsEmptyString(FInvoiceId) then
      LJSONObject.AddPair('InvoiceId', FInvoiceId);
    
    if not TLZString.IsEmptyString(FInvoiceItemId) then
      LJSONObject.AddPair('InvoiceItemId', FInvoiceItemId);
    
    if not TLZString.IsEmptyString(FDescription) then
      LJSONObject.AddPair('Description', FDescription);
    
    if not TLZString.IsEmptyString(FGLAccount) then
      LJSONObject.AddPair('GLAccount', FGLAccount);
    
    if not TLZString.IsEmptyString(FItemCode) then
      LJSONObject.AddPair('ItemCode', FItemCode);
    
    if not TLZString.IsEmptyString(FInvoiceCategory) then
      LJSONObject.AddPair('InvoiceCategory', FInvoiceCategory);
    
    if not TLZString.IsEmptyString(FDIDNumber) then
      LJSONObject.AddPair('DIDNumber', FDIDNumber);
    
    if not TLZString.IsEmptyString(FRecurringChargeId) then
      LJSONObject.AddPair('RecurringChargeId', FRecurringChargeId);
    
    LJSONObject.AddPair('ProRate', TJSONNumber.Create(FProRate));
    LJSONObject.AddPair('Qty', TJSONNumber.Create(FQty));
    LJSONObject.AddPair('UnitPrice', TJSONNumber.Create(FUnitPrice));
    LJSONObject.AddPair('GstRate', TJSONNumber.Create(FGstRate));
    LJSONObject.AddPair('TotalExGst', TJSONNumber.Create(FTotalExGst));
    LJSONObject.AddPair('TotalIncGst', TJSONNumber.Create(FTotalIncGst));
    
    if FPeriodStart <> 0 then
      LJSONObject.AddPair('PeriodStart', TLZIso8601.DateTimeToIso8601(FPeriodStart, true));
    
    if FPeriodEnd <> 0 then
      LJSONObject.AddPair('PeriodEnd', TLZIso8601.DateTimeToIso8601(FPeriodEnd, true));
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZSASBOSSInvoiceItem.SetDefaults;
begin
  inherited;
  // Initialize with default values
  FEnterpriseName := '';
  FEnterpriseRef := '';
  FGroupName := '';
  FGroupRef := '';
  FServicePhoneNumber := '';
  FServiceRef := '';
  FInvoiceId := '';
  FInvoiceItemId := '';
  FDescription := '';
  FProRate := 0.0;
  FQty := 0.0;
  FUnitPrice := 0.0;
  FGstRate := 0.0;
  FTotalExGst := 0.0;
  FTotalIncGst := 0.0;
  FPeriodStart := 0;
  FPeriodEnd := 0;
  FGLAccount := '';
  FItemCode := '';
  FInvoiceCategory := '';
  FDIDNumber := '';
  FRecurringChargeId := '';
end;

procedure TLZSASBOSSInvoiceItem.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetDIDNumber(const Value: string);
begin
  FDIDNumber := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetEnterpriseName(const Value: string);
begin
  FEnterpriseName := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetEnterpriseRef(const Value: string);
begin
  FEnterpriseRef := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetGLAccount(const Value: string);
begin
  FGLAccount := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetGroupName(const Value: string);
begin
  FGroupName := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetGroupRef(const Value: string);
begin
  FGroupRef := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetGstRate(const Value: Double);
begin
  FGstRate := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetInvoiceCategory(const Value: string);
begin
  FInvoiceCategory := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetInvoiceId(const Value: string);
begin
  FInvoiceId := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetInvoiceItemId(const Value: string);
begin
  FInvoiceItemId := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetItemCode(const Value: string);
begin
  FItemCode := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetPeriodEnd(const Value: TDateTime);
begin
  FPeriodEnd := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetPeriodStart(const Value: TDateTime);
begin
  FPeriodStart := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetProRate(const Value: Double);
begin
  FProRate := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetQty(const Value: Double);
begin
  FQty := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetRecurringChargeId(const Value: string);
begin
  FRecurringChargeId := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetServicePhoneNumber(const Value: string);
begin
  FServicePhoneNumber := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetServiceRef(const Value: string);
begin
  FServiceRef := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetTotalExGst(const Value: Double);
begin
  FTotalExGst := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetTotalIncGst(const Value: Double);
begin
  FTotalIncGst := Value;
end;

procedure TLZSASBOSSInvoiceItem.SetUnitPrice(const Value: Double);
begin
  FUnitPrice := Value;
end;

{ TLZSASBOSSInvoiceItems }

function TLZSASBOSSInvoiceItems.Find(AID: string): TLZSASBOSSInvoiceItem;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].InvoiceItemId, AID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

procedure TLZSASBOSSInvoiceItems.FromCSV(ACSV: string);
var
  LJSONValue: TJSONValue;
begin
  LJSONValue := TLZCSVToJSON.Convert(ACSV, fncPascalCase, GetDefaultArrayName);
  try
    FromJSONValue(LJSONValue);
  finally
    LJSONValue.Free;
  end;
end;

function TLZSASBOSSInvoiceItems.GetDefaultArrayName: string;
begin
  Result := 'invoiceitems';
end;

end.
