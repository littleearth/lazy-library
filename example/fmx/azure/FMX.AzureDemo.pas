unit FMX.AzureDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, IPPeerClient,
  Lazy.Types, Azure.Core, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Client, REST.Types, REST.Json, System.Json,
  FMX.Memo.Types, Azure.Management, System.Actions, FMX.ActnList;

type
  TfrmAzureDemo = class(TForm)
    LabelRaw: TLabel;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    btnLogin: TButton;
    LabelAuthStatus: TLabel;
    btnGetSubscriptions: TButton;
    GridPanelLayout3: TGridPanelLayout;
    editSubscription: TEdit;
    btnGetResourceGroups: TButton;
    GridPanelLayout4: TGridPanelLayout;
    editResourceGroup: TEdit;
    btnGetVMs: TButton;
    GridPanelLayout5: TGridPanelLayout;
    editVM: TEdit;
    btnGetMetrics: TButton;
    GridPanelLayout6: TGridPanelLayout;
    memoLog: TMemo;
    GridPanelLayout7: TGridPanelLayout;
    Label1: TLabel;
    editAccessToken: TEdit;
    Label2: TLabel;
    editRefreshToken: TEdit;
    ActionList: TActionList;
    ActionGetSubscriptions: TAction;
    ActionGetResourceGroups: TAction;
    ActionGetVirtualMachines: TAction;
    ActionGetMetrics: TAction;
    ActionLogin: TAction;
    GridPanelLayout8: TGridPanelLayout;
    Label3: TLabel;
    editTennantId: TEdit;
    Label4: TLabel;
    editClientID: TEdit;
    Label5: TLabel;
    editClientSecret: TEdit;
    TimerLog: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ActionIsAuthenticatedUpdate(Sender: TObject);
    procedure ActionGetSubscriptionsExecute(Sender: TObject);
    procedure ActionGetResourceGroupsExecute(Sender: TObject);
    procedure ActionGetVirtualMachinesExecute(Sender: TObject);
    procedure ActionGetMetricsExecute(Sender: TObject);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionLoginUpdate(Sender: TObject);
    procedure TimerLogTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FAzureManagement: TAzureManagement;
    procedure Log(AMessage: string);
    procedure OnAzureBrowserLoginRequest(ASender: TObject; AURL: string;
      AConnection: TLazyOAuth2Connection; AToken: TLazyOAuth2Token);
    procedure OnLazyOAuth2TokenRequestComplete(ASender: TObject;
      ASuccess: boolean; AMessage: string; AToken: TLazyOAuth2Token);
  public
    { Public declarations }
  end;

var
  frmAzureDemo: TfrmAzureDemo;

implementation

{$R *.fmx}

uses
  FMX.Lazy.AuthorizeBrowserForm, System.NetEncoding, System.Net.URLClient,
  System.DateUtils, Lazy.Log, Lazy.Log.Basic;

procedure TfrmAzureDemo.ActionGetMetricsExecute(Sender: TObject);
begin
  FAzureManagement.Get
    (Format('subscriptions/%s/resourceGroups/%s/providers/Microsoft.Compute/virtualMachines/%s/providers/microsoft.insights/metrics'
    + '?api-version=2021-05-01&metricnames=CPU Credits Remaining,CPU Credits Consumed,Network Out Total',
    [editSubscription.Text, editResourceGroup.Text, editVM.Text]),
    procedure(ASender: TObject; ASuccess: boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        Log(ARESTResponse.JSONValue.Format);
      end
      else
      begin
        Log('Error: ' + AMessage);
      end;
    end);
end;

procedure TfrmAzureDemo.ActionGetResourceGroupsExecute(Sender: TObject);
begin
  FAzureManagement.Get
    (Format('subscriptions/%s/resourceGroups/?api-version=2022-12-01',
    [editSubscription.Text]),
    procedure(ASender: TObject; ASuccess: boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        Log(ARESTResponse.JSONValue.Format);
      end
      else
      begin
        Log('Error: ' + AMessage);
      end;
    end);
end;

procedure TfrmAzureDemo.ActionGetSubscriptionsExecute(Sender: TObject);
begin
  FAzureManagement.Get('subscriptions?api-version=2022-12-01',
    procedure(ASender: TObject; ASuccess: boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        Log(ARESTResponse.JSONValue.Format);
      end
      else
      begin
        Log('Error: ' + AMessage);
      end;
    end);

end;

procedure TfrmAzureDemo.ActionGetVirtualMachinesExecute(Sender: TObject);
begin
  FAzureManagement.Get
    (Format('subscriptions/%s/resourceGroups/%s/providers/Microsoft.Compute/virtualMachines/?api-version=2023-07-01',
    [editSubscription.Text, editResourceGroup.Text]),
    procedure(ASender: TObject; ASuccess: boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        Log(ARESTResponse.JSONValue.Format);
      end
      else
      begin
        Log('Error: ' + AMessage);
      end;
    end);
end;

procedure TfrmAzureDemo.ActionIsAuthenticatedUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FAzureManagement.IsAuthenticated;
end;

procedure TfrmAzureDemo.ActionLoginExecute(Sender: TObject);
begin
  if not FAzureManagement.IsAuthenticated then
  begin
    (FAzureManagement.Connection as TAzureManagementOAuth2Connection).TenantId
      := editTennantId.Text;
    FAzureManagement.Connection.ClientId := editClientID.Text;
    FAzureManagement.Connection.ClientSecret := editClientSecret.Text;
    FAzureManagement.Token.RefreshToken := editRefreshToken.Text;
    FAzureManagement.Token.AuthToken := editAccessToken.Text;
    FAzureManagement.Authenticate;
  end
  else
  begin
    FAzureManagement.ClearAuthentication;
  end;
end;

procedure TfrmAzureDemo.ActionLoginUpdate(Sender: TObject);
begin
  if FAzureManagement.IsAuthenticated then
  begin
    (Sender as TAction).Text := 'Log out';
    LabelAuthStatus.Text := FAzureManagement.Token.AuthCode.Substring(0, 8);
  end
  else
  begin
    (Sender as TAction).Text := 'Log in';
    LabelAuthStatus.Text := 'Not Logged In';
  end;
end;

procedure TfrmAzureDemo.OnAzureBrowserLoginRequest(ASender: TObject;
AURL: string; AConnection: TLazyOAuth2Connection; AToken: TLazyOAuth2Token);
var
  LMessage: string;
  LForm: TLazyAuthorizeBrowserForm;
begin
  LForm := TLazyAuthorizeBrowserForm.Create(Self);
  try
    if LForm.GetAuthToken(LMessage, AURL, AConnection, AToken) then
    begin
      Log(AToken.AuthCode);
      FAzureManagement.RequestToken;
    end
    else
    begin
      ShowMessage(LMessage);
    end;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TfrmAzureDemo.OnLazyOAuth2TokenRequestComplete(ASender: TObject;
ASuccess: boolean; AMessage: string; AToken: TLazyOAuth2Token);
begin
  if ASuccess then
  begin
    Log(AToken.AuthToken);
    Log(AToken.RefreshToken);
    Log(DateTimeToStr(AToken.ExpiresIn));
    editAccessToken.Text := AToken.AuthToken;
    if not SameText(editRefreshToken.Text, AToken.RefreshToken) then
    begin
      editRefreshToken.Text := AToken.RefreshToken;
      Log('Refresh token has changed');
    end;
  end
  else
  begin
    ShowMessage(AMessage);
  end;
end;

procedure TfrmAzureDemo.TimerLogTimer(Sender: TObject);
begin
  memoLog.Lines.Text := (LazyLog as TLazyLogBasic).LogText;
end;

procedure TfrmAzureDemo.FormCreate(Sender: TObject);
begin
  FAzureManagement := TAzureManagement.Create;
  FAzureManagement.OnBrowserLoginRequest := OnAzureBrowserLoginRequest;
  FAzureManagement.OnOAuth2TokenRequestComplete :=
    OnLazyOAuth2TokenRequestComplete;
end;

procedure TfrmAzureDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAzureManagement);
end;

procedure TfrmAzureDemo.Log(AMessage: string);
begin
  LazyLog.Log(Self, AMessage);
end;

end.
