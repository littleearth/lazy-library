unit frmAuthDemoU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, System.UITypes,
  System.Actions, Vcl.ActnList,
  duo.api, duo.auth, duo.models;

type
  TfrmAuthDemo = class(TForm)
    Panel1: TPanel;
    memoLog: TMemo;
    Panel2: TPanel;
    GridPanel1: TGridPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GridPanel2: TGridPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Panel4: TPanel;
    Label2: TLabel;
    editIntegrationKey: TEdit;
    editSecretKey: TEdit;
    BitBtn3: TBitBtn;
    Panel5: TPanel;
    Label3: TLabel;
    editHostname: TEdit;
    Panel6: TPanel;
    Label4: TLabel;
    editUsername: TEdit;
    Panel7: TPanel;
    Label5: TLabel;
    editDisplayName: TEdit;
    Panel8: TPanel;
    Label6: TLabel;
    editPushInfo: TEdit;
    Panel9: TPanel;
    Label7: TLabel;
    editType: TEdit;
    ActionList: TActionList;
    ActionCheck: TAction;
    ActionPreAuth: TAction;
    ActionLogin: TAction;
    BitBtn4: TBitBtn;
    ActionPing: TAction;
    Panel10: TPanel;
    Label8: TLabel;
    editRequestHostname: TEdit;
    Panel11: TPanel;
    Label9: TLabel;
    editRequestIPAddress: TEdit;
    BitBtn5: TBitBtn;
    ActionLogo: TAction;
    imgLogo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ActionCheckExecute(Sender: TObject);
    procedure ActionPreAuthExecute(Sender: TObject);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionPingExecute(Sender: TObject);
    procedure ActionLogoExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLogoFileName: TFileName;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure Login(
      ADevice: string = 'auto';
      AFactor: string = 'auto';
      APasscode: string = '');
    procedure Check;
    procedure PreAuth;
    procedure OnPreAuth(
      ASender: TObject;
      ADevices: TLZDUODevices;
      AMessage: string;
      ASuccess: boolean;
      var AOwnsObjects: boolean);
    procedure OnLogin(
      ASender: TObject;
      AMessage: string;
      AResponse: TDUOLoginResponse);
    procedure OnCheck(
      ASender: TObject;
      AMessage: string;
      ASuccess: boolean);
    procedure Ping;
    procedure OnPing(
      ASender: TObject;
      AMessage: string;
      ASuccess: boolean);
    procedure Logo;
  public
    { Public declarations }
  end;

var
  frmAuthDemo: TfrmAuthDemo;

implementation

{$R *.dfm}

uses
  Vcl.duo.preauthform, System.IOUtils, Lazy.CryptINI, Lazy.Log,
  pngimage;

procedure TfrmAuthDemo.FormCreate(Sender: TObject);
begin
  memoLog.Lines.Clear;
  LoadSettings;
  FLogoFileName := '';
  editPushInfo.Text := 'computer=' + TLZDUONetworkTools.GetHostname + '&ips=' +
    TLZDUONetworkTools.GetIPAddresses;
  editRequestHostname.Text := TLZDUONetworkTools.GetHostname;
  editRequestIPAddress.Text := TLZDUONetworkTools.GetIPAddress;
end;

procedure TfrmAuthDemo.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmAuthDemo.OnPreAuth(
  ASender: TObject;
  ADevices: TLZDUODevices;
  AMessage: string;
  ASuccess: boolean;
  var AOwnsObjects: boolean);
var
  LDevice, LFactor, LPasscode: string;
begin
  if not ASuccess then
  begin
    MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    with TFormDuoPreAuth.Create(Self) do
    begin
      try
        if Execute(ADevices, LDevice, LFactor, LPasscode, FLogoFileName) then
        begin
          Login(LDevice, LFactor, LPasscode);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

procedure TfrmAuthDemo.OnLogin(
  ASender: TObject;
  AMessage: string;
  AResponse: TDUOLoginResponse);
begin
  case AResponse of
    loginFail:
      MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    loginSuccess:
      MessageDlg('Login request successful', TMsgDlgType.mtInformation,
        [TMsgDlgBtn.mbOK], 0);
    loginSMSCodes:
      begin
        MessageDlg(AMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
        PreAuth;
      end;
  end;
end;

procedure TfrmAuthDemo.OnCheck(
  ASender: TObject;
  AMessage: string;
  ASuccess: boolean);
begin
  if not ASuccess then
  begin
    MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    MessageDlg('Check request successful', TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmAuthDemo.OnPing(
  ASender: TObject;
  AMessage: string;
  ASuccess: boolean);
begin
  if not ASuccess then
  begin
    MessageDlg(AMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    MessageDlg('Ping request successful', TMsgDlgType.mtInformation,
      [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmAuthDemo.PreAuth;
var
  LDUOAuth: TLZDUOAuthAPI;
begin
  LDUOAuth := TLZDUOAuthAPI.Create(nil);
  try

    LDUOAuth.IntegrationKey := editIntegrationKey.Text;
    LDUOAuth.SecretKey := editSecretKey.Text;
    LDUOAuth.Hostname := editHostname.Text;
    LDUOAuth.OnPreAuth := OnPreAuth;
    LDUOAuth.PreAuth(editUsername.Text, editRequestHostname.Text,
      editRequestIPAddress.Text);
  finally
    FreeAndNil(LDUOAuth);
  end;
end;

procedure TfrmAuthDemo.LoadSettings;
var
  LFileName: TFileName;
  LSettings: TLZCryptINI;
begin
  LFileName := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) +
    'duo_authdemo.ini';
  LSettings := TLZCryptINI.Create(LFileName);
  try
    editIntegrationKey.Text := LSettings.ReadString('duo',
      'integrationkey', '');
    editSecretKey.Text := LSettings.ReadString('duo', 'secretkey', '');
    editHostname.Text := LSettings.ReadString('duo', 'hostname', '');
    editUsername.Text := LSettings.ReadString('duo', 'username', 'username');
    editDisplayName.Text := LSettings.ReadString('duo', 'displayname',
      'Delphi DUO');
  finally
    FreeAndNil(LSettings);
  end;
end;

procedure TfrmAuthDemo.SaveSettings;
var
  LFileName: TFileName;
  LSettings: TLZCryptINI;
begin
  LFileName := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) +
    'duo_authdemo.ini';
  LSettings := TLZCryptINI.Create(LFileName);
  try
    LSettings.WriteString('duo', 'integrationkey', editIntegrationKey.Text);
    LSettings.WriteString('duo', 'secretkey', editSecretKey.Text);
    LSettings.WriteString('duo', 'hostname', editHostname.Text);
    LSettings.WriteString('duo', 'username', editUsername.Text);
    LSettings.WriteString('duo', 'displayname', editDisplayName.Text);
    LSettings.UpdateFile;
  finally
    FreeAndNil(LSettings);
  end;
end;

procedure TfrmAuthDemo.Login(ADevice, AFactor, APasscode: string);
var
  LDUOAuth: TLZDUOAuthAPI;
begin
  LDUOAuth := TLZDUOAuthAPI.Create(nil);
  try
    LDUOAuth.IntegrationKey := editIntegrationKey.Text;
    LDUOAuth.SecretKey := editSecretKey.Text;
    LDUOAuth.Hostname := editHostname.Text;
    LDUOAuth.OnLogin := OnLogin;
    LDUOAuth.Login(editUsername.Text, editDisplayName.Text, editPushInfo.Text,
      editType.Text, ADevice, AFactor, APasscode, editRequestHostname.Text,
      editRequestIPAddress.Text);
  finally
    FreeAndNil(LDUOAuth);
  end;
end;

procedure TfrmAuthDemo.ActionLoginExecute(Sender: TObject);
begin
  Login;
end;

procedure TfrmAuthDemo.ActionLogoExecute(Sender: TObject);
begin
  Logo;
end;

procedure TfrmAuthDemo.Ping;
var
  LDUOAuth: TLZDUOAuthAPI;
begin
  LDUOAuth := TLZDUOAuthAPI.Create(nil);
  try
    LDUOAuth.IntegrationKey := editIntegrationKey.Text;
    LDUOAuth.SecretKey := editSecretKey.Text;
    LDUOAuth.Hostname := editHostname.Text;
    LDUOAuth.OnPing := OnPing;
    LDUOAuth.Ping;
  finally
    FreeAndNil(LDUOAuth);
  end;
end;

procedure TfrmAuthDemo.Logo;
var
  LDUOAuth: TLZDUOAuthAPI;
  LMessage: string;
  LSuccess: boolean;
begin
  LDUOAuth := TLZDUOAuthAPI.Create(nil);
  try
    LDUOAuth.IntegrationKey := editIntegrationKey.Text;
    LDUOAuth.SecretKey := editSecretKey.Text;
    LDUOAuth.Hostname := editHostname.Text;

    FLogoFileName := TPath.GetTempPath + 'duo_logo.png';

    LDUOAuth.Logo(FLogoFileName, LMessage, LSuccess);
    if LSuccess then
    begin
      imgLogo.Picture.LoadFromFile(FLogoFileName);
    end
    else
    begin
      MessageDlg(LMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
  finally
    FreeAndNil(LDUOAuth);
  end;
end;

procedure TfrmAuthDemo.ActionPingExecute(Sender: TObject);
begin
  Ping;
end;

procedure TfrmAuthDemo.ActionPreAuthExecute(Sender: TObject);
begin
  PreAuth;
end;

procedure TfrmAuthDemo.Check;
var
  LDUOAuth: TLZDUOAuthAPI;
begin
  LDUOAuth := TLZDUOAuthAPI.Create(nil);
  try
    LDUOAuth.IntegrationKey := editIntegrationKey.Text;
    LDUOAuth.SecretKey := editSecretKey.Text;
    LDUOAuth.Hostname := editHostname.Text;
    LDUOAuth.OnCheck := OnCheck;
    LDUOAuth.Check;
  finally
    FreeAndNil(LDUOAuth);
  end;
end;

procedure TfrmAuthDemo.ActionCheckExecute(Sender: TObject);
begin
  Check;
end;

end.
