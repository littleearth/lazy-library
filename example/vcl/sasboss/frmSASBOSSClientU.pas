unit frmSASBOSSClientU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Actions, Vcl.ActnList, Vcl.Buttons, SASBOSS.Api, SASBOSS.Models,
  Vcl.ComCtrls, System.UITypes;

type
  TfrmSASBOSSClient = class(TForm)
    GridPanel1: TGridPanel;
    Panel1: TPanel;
    Label1: TLabel;
    editUsername: TEdit;
    Panel2: TPanel;
    Label2: TLabel;
    editPassword: TEdit;
    GridPanel2: TGridPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GridPanel3: TGridPanel;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ActionList: TActionList;
    ActionLogin: TAction;
    ActionLogout: TAction;
    ActionEnterprises: TAction;
    ActionInvoices: TAction;
    ListViewData: TListView;
    BitBtn5: TBitBtn;
    ActionGroups: TAction;
    ActionInvoiceCharges: TAction;
    BitBtn6: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionLogoutUpdate(Sender: TObject);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionEnterprisesExecute(Sender: TObject);
    procedure ActionInvoicesExecute(Sender: TObject);
    procedure ActionGroupsExecute(Sender: TObject);
    procedure ActionInvoiceChargesExecute(Sender: TObject);
  private
    FSASBOSSBillingClient: TLZSASBOSSBillingClient;
    function GetIsAuthenticated: boolean;
    procedure AddObjectToListView(
      AObject: TObject;
      AListView: TListView);
    procedure ResetListView(AListView: TListView);
  public
    property IsAuthenticated: boolean read GetIsAuthenticated;
  end;

var
  frmSASBOSSClient: TfrmSASBOSSClient;

implementation

uses
  Lazy.Utils.Windows, System.TypInfo, System.Rtti;

{$R *.dfm}

function IsDateTimeType(ATypeInfo: PTypeInfo): boolean;
var
  LTypeName: string;
begin
  Result := False;
  if not Assigned(ATypeInfo) then
    Exit;

  LTypeName := string(ATypeInfo^.Name);

  // Check for common DateTime type names
  Result := SameText(LTypeName, 'TDateTime') or SameText(LTypeName, 'TDate') or
    SameText(LTypeName, 'TTime') or SameText(LTypeName, 'DateTime') or
    SameText(LTypeName, 'Date') or SameText(LTypeName, 'Time');

  // Alternative approach: check if it's the same TypeInfo as TDateTime
  if not Result then
    Result := ATypeInfo = TypeInfo(TDateTime);
end;

function ValueToString(const AValue: TValue): string;
var
  LObject: TObject;
  LDateTime: TDateTime;
begin
  if AValue.IsEmpty then
    Result := '(empty)'
  else
  begin
    case AValue.Kind of
      tkInteger:
        Result := IntToStr(AValue.AsInteger);
      tkInt64:
        Result := IntToStr(AValue.AsInt64);
      tkFloat:
        begin
          // Check if this is a DateTime type
          if IsDateTimeType(AValue.TypeInfo) then
          begin
            LDateTime := AValue.AsExtended;
            if LDateTime = 0 then
              Result := '(no date)'
            else if Frac(LDateTime) = 0 then
              Result := DateToStr(LDateTime) // Date only
            else if Trunc(LDateTime) = 0 then
              Result := TimeToStr(LDateTime) // Time only
            else
              Result := DateTimeToStr(LDateTime); // Date and time
          end
          else
            Result := FloatToStr(AValue.AsExtended);
        end;
      tkString, tkLString, tkWString, tkUString:
        Result := AValue.AsString;
      tkEnumeration:
        begin
          if AValue.TypeInfo = TypeInfo(boolean) then
            Result := BoolToStr(AValue.AsBoolean, True)
          else
            Result := AValue.ToString;
        end;
      tkClass:
        begin
          LObject := AValue.AsObject;
          if Assigned(LObject) then
            Result := LObject.ClassName
          else
            Result := '(nil)';
        end;
      tkSet:
        Result := AValue.ToString;
      tkVariant:
        Result := VarToStr(AValue.AsVariant);
      tkArray, tkDynArray:
        Result := '(array)';
      tkRecord:
        Result := '(record)';
      tkInterface:
        Result := '(interface)';
      tkPointer:
        Result := '(pointer)';
    else
      Result := AValue.ToString;
    end;
  end;
end;

procedure TfrmSASBOSSClient.AddObjectToListView(
  AObject: TObject;
  AListView: TListView);
var
  LRttiContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiProperty: TRttiProperty;
  LProperties: TArray<TRttiProperty>;
  LListItem: TListItem;
  LPropValue: TValue;
  LI: Integer;
  LPropName: string;
  LPropValueStr: string;
begin
  if not Assigned(AObject) or not Assigned(AListView) then
    Exit;

  LRttiContext := TRttiContext.Create;
  try
    LRttiType := LRttiContext.GetType(AObject.ClassType);
    if not Assigned(LRttiType) then
      Exit;

    // Get all public and published properties
    LProperties := LRttiType.GetProperties;

    if Length(LProperties) = 0 then
      Exit;

    // Create columns if this is the first object being added
    if AListView.Items.Count = 0 then
    begin
      AListView.Columns.Clear;
      for LI := Low(LProperties) to High(LProperties) do
      begin
        LRttiProperty := LProperties[LI];
        LPropName := LRttiProperty.Name;
        with AListView.Columns.Add do
        begin
          Caption := LPropName;
          Width := 100; // Set default width
        end;
      end;
    end;

    // Add new row for this object
    LListItem := AListView.Items.Add;
    LListItem.Data := AObject;

    // Set values for each property column
    for LI := Low(LProperties) to High(LProperties) do
    begin
      LRttiProperty := LProperties[LI];

      // Get property value
      try
        if LRttiProperty.IsReadable then
        begin
          LPropValue := LRttiProperty.GetValue(AObject);
          LPropValueStr := ValueToString(LPropValue);
        end
        else
          LPropValueStr := '(not readable)';
      except
        on E: Exception do
          LPropValueStr := '(error: ' + E.Message + ')';
      end;

      // Set the value in the appropriate column
      if LI = 0 then
        LListItem.Caption := LPropValueStr
      else
        LListItem.SubItems.Add(LPropValueStr);
    end;
  finally
    LRttiContext.Free;
  end;
end;

procedure TfrmSASBOSSClient.ResetListView(AListView: TListView);
begin
  AListView.Clear;
  AListView.Columns.Clear;
  AListView.ViewStyle := vsReport;
  AListView.GridLines := True;
  AListView.RowSelect := True;
end;

procedure TfrmSASBOSSClient.ActionEnterprisesExecute(Sender: TObject);
begin
  FSASBOSSBillingClient.GetEnterprises(
    procedure(AEnterprises: TLZSASBOSSEnterprises; AMessage: string;
      ASuccess: boolean; var AOwnsObjects: boolean)
    var
      LEnterprise: TLZSASBOSSEnterprise;
    begin
      if ASuccess then
      begin
        ResetListView(ListViewData);
        ListViewData.Items.BeginUpdate;
        try
          for LEnterprise in AEnterprises do
          begin
            AddObjectToListView(LEnterprise, ListViewData);
          end;
        finally
          ListViewData.Items.EndUpdate;
        end;
      end
      else
      begin
        MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
    end);
end;

procedure TfrmSASBOSSClient.ActionGroupsExecute(Sender: TObject);
begin
  FSASBOSSBillingClient.GetAllGroups(
    procedure(AGroups: TLZSASBOSSGroups; AMessage: string; ASuccess: boolean;
      var AOwnsObjects: boolean)
    var
      LGroup: TLZSASBOSSGroup;
    begin
      if ASuccess then
      begin
        ResetListView(ListViewData);
        ListViewData.Items.BeginUpdate;
        try
          for LGroup in AGroups do
          begin
            AddObjectToListView(LGroup, ListViewData);
          end;
        finally
          ListViewData.Items.EndUpdate;
        end;
      end
      else
      begin
        MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
    end);
end;

procedure TfrmSASBOSSClient.ActionInvoiceChargesExecute(Sender: TObject);
var
  LEnterpriseID, LInvoiceID: string;
begin
  if InputQuery('Enterprise ID', 'Enter enterprise id', LEnterpriseID) and
    InputQuery('Invoice ID', 'Enter invoice id', LInvoiceID) then
  begin
    FSASBOSSBillingClient.GetEnterpriseInvoiceCharges(LEnterpriseID, LInvoiceID,
      procedure(AInvoiceItems: TLZSASBOSSInvoiceItems; AMessage: string;
        ASuccess: boolean; var AOwnsObjects: boolean)
      var
        LObject: TLZSASBOSSInvoiceItem;
      begin
        if ASuccess then
        begin
          ResetListView(ListViewData);
          ListViewData.Items.BeginUpdate;
          try
            for LObject in AInvoiceItems do
            begin
              AddObjectToListView(LObject, ListViewData);
            end;
          finally
            ListViewData.Items.EndUpdate;
          end;
        end
        else
        begin
          MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
        end;
      end);
  end;
end;

procedure TfrmSASBOSSClient.ActionInvoicesExecute(Sender: TObject);
var
  LEnterpriseID: string;
begin
  if InputQuery('Enterprise ID', 'Enter enterpriseid', LEnterpriseID) then
  begin
    FSASBOSSBillingClient.GetAllEnterpriseInvoices(LEnterpriseID,
      procedure(AInvoices: TLZSASBOSSInvoices; AMessage: string;
        ASuccess: boolean; var AOwnsObjects: boolean)
      var
        LObject: TLZSASBOSSInvoice;
      begin
        if ASuccess then
        begin
          ResetListView(ListViewData);
          ListViewData.Items.BeginUpdate;
          try
            for LObject in AInvoices do
            begin
              AddObjectToListView(LObject, ListViewData);
            end;
          finally
            ListViewData.Items.EndUpdate;
          end;
        end
        else
        begin
          MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
        end;
      end);
  end;
end;

procedure TfrmSASBOSSClient.ActionLoginExecute(Sender: TObject);
begin
  FSASBOSSBillingClient.Authenticate(editUsername.Text, editPassword.Text,
    procedure(AMessage: string; ASuccess: boolean; AToken: string;
      AAPIUser: string; ARoleType: string)
    begin
      if not ASuccess then
      begin
        MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
    end);
end;

procedure TfrmSASBOSSClient.ActionLogoutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsAuthenticated;
end;

procedure TfrmSASBOSSClient.FormCreate(Sender: TObject);
begin
  FSASBOSSBillingClient := TLZSASBOSSBillingClient.Create;
end;

procedure TfrmSASBOSSClient.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSASBOSSBillingClient);
end;

function TfrmSASBOSSClient.GetIsAuthenticated: boolean;
begin
  Result := Assigned(FSASBOSSBillingClient) and
    (FSASBOSSBillingClient.IsAuthenticated);
end;

end.
