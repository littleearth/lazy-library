unit VCL.AzureDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics, System.UITypes,
  VCL.Controls, VCL.Forms, VCL.Dialogs, Lazy.Types, Azure.Core,
  Azure.Management, Lazy.REST.Types,
  VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, System.Actions, VCL.ActnList,
  VCL.Buttons, System.Generics.Collections, System.Messaging, VCL.AppEvnts;

type
  TVirtualMachineMessage = class(TMessage)
  private
    FVMName: string;
    FID: string;
    FCPUCreditsRemaining: Double;
    FCPUCreditsUsed: Double;
    FNetworkOut: Double;
    procedure SetCPUCreditsRemaining(const Value: Double);
    procedure SetID(const Value: string);
    procedure SetVMName(const Value: string);
    procedure SetCPUCreditsUsed(const Value: Double);
    procedure SetNetworkOut(const Value: Double);
  public
    constructor Create(AID: string; AVMName: string;
      ACPUCreditsRemaining: Double; ACPUCreditsUsed: Double;
      ANetworkOut: Double);
    property ID: string read FID write SetID;
    property VMName: string read FVMName write SetVMName;
    property CPUCreditsRemaining: Double read FCPUCreditsRemaining
      write SetCPUCreditsRemaining;
    property CPUCreditsUsed: Double read FCPUCreditsUsed
      write SetCPUCreditsUsed;
    property NetworkOut: Double read FNetworkOut write SetNetworkOut;

  end;

  TfrmAzureDemo = class(TForm)
    GridPanel1: TGridPanel;
    Label1: TLabel;
    editTennantID: TEdit;
    Label2: TLabel;
    editClientID: TEdit;
    Label3: TLabel;
    editClientSecret: TEdit;
    GridPanel2: TGridPanel;
    ListViewVirtualMachines: TListView;
    BitBtn1: TBitBtn;
    ActionList: TActionList;
    ActionExecute: TAction;
    pnlLog: TPanel;
    Label4: TLabel;
    memoLog: TMemo;
    Splitter1: TSplitter;
    ApplicationEvents: TApplicationEvents;
    procedure ActionExecuteExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FAzureManagement: TLZAzureManagement;
    procedure OnBrowserLoginRequest(ASender: TObject; AURL: string;
      AConnection: TLZOAuth2Connection; AToken: TLZOAuth2Token);
    procedure OnTokenRequestComplete(ASender: TObject; ASuccess: Boolean;
      AMessage: string; AToken: TLZOAuth2Token);
  public
    procedure Authenticate;
    procedure GetCPUCreditsRemaining(AVirtualMachines: TStrings);
    procedure GetSubscriptions;
    procedure GetResourceGroups(ASubscriptions: TStrings);
    procedure GetVirualMachines(AResourceGroups: TStrings);
  end;

var
  frmAzureDemo: TfrmAzureDemo;

implementation

{$R *.dfm}

uses
  VCL.Lazy.AuthorizeBrowserForm, System.JSON, Lazy.Log, Lazy.Log.Basic;

procedure TfrmAzureDemo.ActionExecuteExecute(Sender: TObject);
begin
  ListViewVirtualMachines.Clear;
  Authenticate;
end;

procedure TfrmAzureDemo.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
  memoLog.Lines.Text := (LazyLog as TLZLogBasic).LogText;
end;

procedure TfrmAzureDemo.Authenticate;
begin
  (FAzureManagement.Connection as TLZAzureOAuth2Connection).TenantId :=
    editTennantID.Text;
  FAzureManagement.Connection.ClientId := editClientID.Text;
  FAzureManagement.Connection.ClientSecret := editClientSecret.Text;
  FAzureManagement.Authenticate;
end;

procedure TfrmAzureDemo.FormCreate(Sender: TObject);
var
  LVirtualMachineListener: TMessageListener;
begin
  FAzureManagement := TLZAzureManagement.Create;
  FAzureManagement.OnBrowserLoginRequest := OnBrowserLoginRequest;
  FAzureManagement.OnOAuth2TokenRequestComplete := OnTokenRequestComplete;

  LVirtualMachineListener := procedure(const Sender: TObject; const M: TMessage)
    var
      LVirtualMachineMessage: TVirtualMachineMessage;
      LListItem: TListItem;
    begin
      LVirtualMachineMessage := M as TVirtualMachineMessage;
      ListViewVirtualMachines.Items.BeginUpdate;
      try
        LListItem := ListViewVirtualMachines.Items.Add;
        LListItem.Caption := LVirtualMachineMessage.VMName;
        LListItem.SubItems.Add
          (FloatToStr(LVirtualMachineMessage.CPUCreditsRemaining));
        LListItem.SubItems.Add
          (FloatToStr(LVirtualMachineMessage.CPUCreditsUsed));
        LListItem.SubItems.Add(FloatToStr(LVirtualMachineMessage.NetworkOut / 1024 / 1024));
      finally
        ListViewVirtualMachines.Items.EndUpdate;
      end;
    end;
  TMessageManager.DefaultManager.SubscribeToMessage(TVirtualMachineMessage,
    LVirtualMachineListener);
end;

procedure TfrmAzureDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAzureManagement);
end;

procedure TfrmAzureDemo.GetCPUCreditsRemaining(AVirtualMachines: TStrings);
var
  LIdx: integer;
  LID, LName: string;
begin
  for LIdx := 0 to Pred(AVirtualMachines.Count) do
  begin
    LID := AVirtualMachines.Names[LIdx];
    LName := AVirtualMachines.ValueFromIndex[LIdx];
    LazyLog.Debug(Self, 'GetCPUCreditsRemaining', LName);
    FAzureManagement.Get(Format('%s/providers/microsoft.insights/metrics' +
      '?api-version=2021-05-01&metricnames=CPU Credits Remaining,CPU Credits Consumed,Network Out Total',
      [LID]),
      procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      var
        LValues: TJSONArray;
        LTimes: TJSONArray;
        LTimeSeries: TJSONArray;
        LValue: TJSONValue;
        LTime: TJSONValue;
        LTimeSeriesEntry: TJSONValue;
        LTimeIdx: integer;
        LCPUCredit, LCPUConsumed, LNetworkOut: Double;
        LType: string;
        LVirtualMachine: TVirtualMachineMessage;
      begin

        if ASuccess then
        begin
          try
            LValues := ARESTResponse.JSONValue.FindValue('value') as TJSONArray;
            LCPUCredit := -1;
            LCPUConsumed := -1;
            LNetworkOut := -1;
            for LValue in LValues do
            begin
              LTimeSeries := LValue.FindValue('timeseries') as TJSONArray;
              LType := LValue.FindValue('name').GetValue<string>('value');
              for LTimeSeriesEntry in LTimeSeries do
              begin
                LTimes := LTimeSeriesEntry.FindValue('data') as TJSONArray;
                if Assigned(LTimes) then
                begin
                  LTimeIdx := LTimes.Count - 1;
                  if SameText(LType, 'CPU Credits Remaining') then
                  begin
                    while (LCPUCredit = -1) and (LTimeIdx >= 0) do
                    begin
                      LTime := LTimes.Items[LTimeIdx];
                      LCPUCredit := LTime.GetValue<Double>('average', -1);
                      Dec(LTimeIdx);
                    end;
                  end;
                  if SameText(LType, 'CPU Credits Consumed') then
                  begin
                    while (LCPUConsumed = -1) and (LTimeIdx >= 0) do
                    begin
                      LTime := LTimes.Items[LTimeIdx];
                      LCPUConsumed := LTime.GetValue<Double>('average', -1);
                      Dec(LTimeIdx);
                    end;
                  end;
                  if SameText(LType, 'Network Out Total') then
                  begin
                    while (LNetworkOut = -1) and (LTimeIdx >= 0) do
                    begin
                      LTime := LTimes.Items[LTimeIdx];
                      LNetworkOut := LTime.GetValue<Double>('total', -1);
                      Dec(LTimeIdx);
                    end;
                  end;
                end;
              end;
            end;
            LVirtualMachine := TVirtualMachineMessage.Create(ACustomData,
              ACustomData, LCPUCredit, LCPUConsumed, LNetworkOut);
            TMessageManager.DefaultManager.SendMessage(Self, LVirtualMachine);
          except
            on E: Exception do
            begin
              LazyLog.Error(Self, E);
            end;
          end;
        end
        else
        begin
          LazyLog.Error(Self, AMessage);
        end;
      end, LName);
  end;
end;

procedure TfrmAzureDemo.GetResourceGroups(ASubscriptions: TStrings);
var
  LIdx: integer;
  LSubscription: string;
begin

  for LIdx := 0 to Pred(ASubscriptions.Count) do
  begin
    LSubscription := ASubscriptions.Names[LIdx];
    FAzureManagement.Get
      (Format('subscriptions/%s/resourceGroups/?api-version=2022-12-01',
      [LSubscription]),
      procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      var
        LValues: TJSONArray;
        LValue: TJSONValue;
        LID, LName: string;
        LResourceGroups: TStringList;
      begin
        LResourceGroups := TStringList.Create;
        try
          if ASuccess then
          begin
            LValues := ARESTResponse.JSONValue.FindValue('value') as TJSONArray;
            for LValue in LValues do
            begin
              LID := LValue.GetValue<string>('id');
              LName := LValue.GetValue<string>('name');
              LResourceGroups.AddPair(LID, LName);
            end;
            if LResourceGroups.Count > 0 then
            begin
              LazyLog.Log(Self, LResourceGroups.Text);
              GetVirualMachines(LResourceGroups);
            end
            else
            begin
              LazyLog.Log(Self,
                Format('Error: No resource group found for subscription %s',
                [ARESTResponse.FullRequestURI]));
            end;
          end
          else
          begin
            LazyLog.Error(Self, AMessage);
          end;
        finally
          FreeAndNil(LResourceGroups);
        end;
      end);
  end;
end;

procedure TfrmAzureDemo.GetSubscriptions;
begin
  FAzureManagement.Get('subscriptions?api-version=2022-12-01',
    procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    var
      LValues: TJSONArray;
      LValue: TJSONValue;
      LID, LName: string;
      LEnabled: Boolean;
      LSubscriptions: TStringList;
    begin
      LSubscriptions := TStringList.Create;
      try
        LazyLog.Log(Self, ARESTResponse.JSONValue.Format());
        if ASuccess then
        begin
          LValues := ARESTResponse.JSONValue.FindValue('value') as TJSONArray;
          for LValue in LValues do
          begin
            LID := LValue.GetValue<string>('subscriptionId');
            LName := LValue.GetValue<string>('displayName');
            LEnabled := SameText(LValue.GetValue<string>('state'), 'Enabled');
            if LEnabled then
            begin
              LSubscriptions.AddPair(LID, LName);
            end;
            if LSubscriptions.Count > 0 then
            begin
              LazyLog.Log(Self, LSubscriptions.Text);
              GetResourceGroups(LSubscriptions);
            end
            else
            begin
              LazyLog.Error(Self, 'No subscriptions found');
            end;
          end;
        end
        else
        begin
          LazyLog.Error(Self, AMessage);
        end;
      finally
        FreeAndNil(LSubscriptions);
      end;
    end);

end;

procedure TfrmAzureDemo.GetVirualMachines(AResourceGroups: TStrings);
var
  LIdx: integer;
  LResource: string;
begin
  for LIdx := 0 to Pred(AResourceGroups.Count) do
  begin
    LResource := AResourceGroups.Names[LIdx];
    FAzureManagement.Get
      (Format('%s/providers/Microsoft.Compute/virtualMachines/?api-version=2023-07-01',
      [LResource]),
      procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      var
        LValues: TJSONArray;
        LValue: TJSONValue;
        LID, LName, LVmsize: string;
        LList: TStringList;
      begin
        LList := TStringList.Create;
        try
          if ASuccess then
          begin
            LValues := ARESTResponse.JSONValue.FindValue('value') as TJSONArray;
            for LValue in LValues do
            begin
              LID := LValue.GetValue<string>('id');
              LName := LValue.GetValue<string>('name');
              LVmsize := LValue.FindValue('properties')
                .FindValue('hardwareProfile').GetValue<string>('vmSize');
              if LVmsize.Contains('Standard_B') then
              begin
                LList.AddPair(LID, LName);
              end
              else
              begin
                LazyLog.Log(Self, 'Skipped VM: ' + LName + ', VM size: '
                  + LVmsize);
              end;
            end;
            if LList.Count > 0 then
            begin
              GetCPUCreditsRemaining(LList);
            end
            else
            begin
              LazyLog.Error(Self,
                Format('No resource group found for resource %s', [LResource]));
            end;
          end
          else
          begin
            LazyLog.Error(Self, AMessage);
          end;
        finally
          FreeAndNil(LList);
        end;
      end);
  end;
end;

procedure TfrmAzureDemo.OnBrowserLoginRequest(ASender: TObject; AURL: string;
AConnection: TLZOAuth2Connection; AToken: TLZOAuth2Token);
var
  LMessage: string;
  LForm: TLZAuthorizeBrowserForm;
begin
  LForm := TLZAuthorizeBrowserForm.Create(Self);
  try
    if LForm.GetAuthToken(LMessage, AURL, AConnection, AToken) then
    begin
      LazyLog.Debug(Self, 'OnBrowserLoginRequest', AToken.AuthCode);
      FAzureManagement.RequestToken;
    end
    else
    begin
      MessageDlg(LMessage, TMsgDlgType.mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TfrmAzureDemo.OnTokenRequestComplete(ASender: TObject;
ASuccess: Boolean; AMessage: string; AToken: TLZOAuth2Token);
begin
  if ASuccess then
  begin
    GetSubscriptions;
  end
  else
  begin
    MessageDlg(AMessage, TMsgDlgType.mtError, [mbOk], 0);
  end;
end;

{ TVirtualMachineMessage }

constructor TVirtualMachineMessage.Create(AID, AVMName: string;
ACPUCreditsRemaining, ACPUCreditsUsed, ANetworkOut: Double);
begin
  FVMName := AVMName;
  FID := AID;
  FCPUCreditsRemaining := ACPUCreditsRemaining;
  FCPUCreditsUsed := ACPUCreditsUsed;
  FNetworkOut := ANetworkOut;
end;

procedure TVirtualMachineMessage.SetCPUCreditsRemaining(const Value: Double);
begin
  FCPUCreditsRemaining := Value;
end;

procedure TVirtualMachineMessage.SetCPUCreditsUsed(const Value: Double);
begin
  FCPUCreditsUsed := Value;
end;

procedure TVirtualMachineMessage.SetID(

  const Value: string);
begin
  FID := Value;
end;

procedure TVirtualMachineMessage.SetNetworkOut(const Value: Double);
begin
  FNetworkOut := Value;
end;

procedure TVirtualMachineMessage.SetVMName(

  const Value: string);
begin
  FVMName := Value;
end;

end.
