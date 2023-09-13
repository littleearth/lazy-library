unit vcl.duo.preauthform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, vcl.Graphics, System.UITypes,
  vcl.Controls, vcl.Forms, vcl.Dialogs, duo.api, duo.auth, vcl.ExtCtrls,
  System.Actions, vcl.ActnList, vcl.StdCtrls, vcl.Buttons,
  duo.models;

type
  TFormDuoPreAuth = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ActionList: TActionList;
    ActionOk: TAction;
    ActionCancel: TAction;
    GridPanel1: TGridPanel;
    Panel3: TPanel;
    Label1: TLabel;
    imgLogo: TImage;
    comboDevices: TComboBox;
    Panel4: TPanel;
    Label2: TLabel;
    comboDeviceFactor: TComboBox;
    pnlPasscode: TPanel;
    Label3: TLabel;
    editPasscode: TEdit;
    pnlHeader: TPanel;
    imgHeader: TImage;
    lblHeader: TLabel;
    procedure ActionCancelExecute(Sender: TObject);
    procedure comboDevicesChange(Sender: TObject);
    procedure comboDeviceFactorChange(Sender: TObject);
    procedure ActionOkExecute(Sender: TObject);
    procedure editPasscodeKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  private
    FDUODevices: TDUODevices;
    FDevice: string;
    FFactor: string;
    FPasscode: string;
    procedure UpdateDevices;
    procedure UpdateFactors;
    procedure UpdateFactorOptions;
    procedure Reset;
    function ValidateForm(var AMessage: string): Boolean;
  public
    function Execute(ADUODevices: TDUODevices; var ADevice: string;
      var AFactor: string; var APasscode: string;
      ALogoFileName: TFileName = ''): Boolean;
  end;

implementation

{$R *.dfm}
{ TFormDuoPreAuth }

procedure TFormDuoPreAuth.ActionCancelExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TFormDuoPreAuth.ActionOkExecute(Sender: TObject);
var
  LMessage: string;
begin
  if ValidateForm(LMessage) then
  begin
    Self.ModalResult := mrOk;
  end
  else
  begin
    MessageDlg(LMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;

end;

procedure TFormDuoPreAuth.comboDeviceFactorChange(Sender: TObject);
begin
  UpdateFactorOptions;
end;

procedure TFormDuoPreAuth.comboDevicesChange(Sender: TObject);
begin
  UpdateFactors;
end;

procedure TFormDuoPreAuth.editPasscodeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ActionOk.Execute;
end;

function TFormDuoPreAuth.Execute(ADUODevices: TDUODevices;
  var ADevice, AFactor, APasscode: string; ALogoFileName: TFileName): Boolean;
begin
  Result := False;
  FDUODevices := ADUODevices;
  if Assigned(FDUODevices) and (FDUODevices.Count > 0) then
  begin
    UpdateDevices;
    if FileExists(ALogoFileName) then
      imgLogo.Picture.LoadFromFile(ALogoFileName);
    if Self.ShowModal = mrOk then
    begin
      ADevice := FDevice;
      AFactor := FFactor;
      APasscode := FPasscode;
      Result := True;
    end;
  end
  else
  begin
    MessageDlg('No devices available', TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TFormDuoPreAuth.FormCreate(Sender: TObject);
begin
  lblHeader.Font.Size := lblHeader.Font.Size * 2;
end;

procedure TFormDuoPreAuth.Reset;
begin
  comboDevices.Items.Clear;
  comboDevices.Items.Add('auto');
  comboDeviceFactor.Items.Clear;
  editPasscode.Text := '';
  pnlPasscode.Enabled := False;
  pnlPasscode.Visible := pnlPasscode.Enabled;
end;

procedure TFormDuoPreAuth.UpdateDevices;
var
  LDUODevice: TDUODevice;
begin
  Reset;
  for LDUODevice in FDUODevices.Items do
  begin
    comboDevices.Items.Add(LDUODevice.Number);
  end;
  if comboDevices.Items.Count > 0 then
  begin
    comboDevices.ItemIndex := 0;
    UpdateFactors;
  end;
end;

procedure TFormDuoPreAuth.UpdateFactorOptions;
var
  LFactor: string;
begin
  if comboDeviceFactor.ItemIndex <> -1 then
  begin
    LFactor := comboDeviceFactor.Items[comboDeviceFactor.ItemIndex];
  end;
  pnlPasscode.Enabled := SameText(LFactor, 'mobile_otp');
  pnlPasscode.Visible := pnlPasscode.Enabled;
end;

procedure TFormDuoPreAuth.UpdateFactors;
var
  LDUODevice: TDUODevice;
begin
  LDUODevice := nil;
  comboDeviceFactor.Items.Clear;
  if comboDevices.ItemIndex <> -1 then
  begin
    LDUODevice := FDUODevices.FindByNumber
      (comboDevices.Items[comboDevices.ItemIndex]);
  end;
  if Assigned(LDUODevice) then
  begin
    comboDeviceFactor.Items.Assign(LDUODevice.Capabilities);
  end
  else
  begin
    comboDeviceFactor.Items.Add('auto');
  end;
  if comboDeviceFactor.Items.Count > 0 then
  begin
    comboDeviceFactor.ItemIndex := 0;
    UpdateFactorOptions;
  end;

end;

function TFormDuoPreAuth.ValidateForm(var AMessage: string): Boolean;
var
  LNumber: string;
  LDUODevice: TDUODevice;
begin
  Result := True;
  FDevice := 'auto';
  FFactor := '';
  FPasscode := '';
  if Result then
  begin
    if comboDevices.ItemIndex <> -1 then
    begin
      LNumber := comboDevices.Items[comboDevices.ItemIndex];
      LDUODevice := FDUODevices.FindByNumber(LNumber);
      if Assigned(LDUODevice) then
      begin
        FDevice := LDUODevice.Device;
      end;
    end
    else
    begin
      Result := False;
      AMessage := 'Please select a device';
    end;
  end;

  if Result then
  begin
    if comboDeviceFactor.ItemIndex <> -1 then
    begin
      FFactor := comboDeviceFactor.Items[comboDeviceFactor.ItemIndex];
      if Trim(FFactor) = '' then
      begin
        Result := False;
        AMessage := 'Invalid factor';
      end;
    end
    else
    begin
      Result := False;
      AMessage := 'Please select a factor for authentication';
    end;
  end;

  if Result then
  begin
    if pnlPasscode.Enabled then
    begin
      FFactor := 'passcode';
      FDevice := '';
      FPasscode := editPasscode.Text;
      if Trim(FPasscode) = '' then
      begin
        Result := False;
        AMessage := 'A  must be specified';
      end;
    end;
  end;
end;

end.
