unit frmAccountsDemoU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, System.UITypes,
  System.Actions, Vcl.ActnList,
  duo.api, duo.accounts, duo.admin, duo.models;

type
  TfrmAccountsDemo = class(TForm)
    Panel1: TPanel;
    memoLog: TMemo;
    Panel2: TPanel;
    GridPanel1: TGridPanel;
    BitBtn1: TBitBtn;
    GridPanel2: TGridPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Panel4: TPanel;
    Label2: TLabel;
    editIntegrationKey: TEdit;
    editSecretKey: TEdit;
    Panel5: TPanel;
    Label3: TLabel;
    editHostname: TEdit;
    ActionList: TActionList;
    ActionGetAccounts: TAction;
    imgLogo: TImage;
    BitBtn2: TBitBtn;
    Panel6: TPanel;
    Label4: TLabel;
    editAccountID: TEdit;
    ActionGetBilling: TAction;
    Panel7: TPanel;
    Label5: TLabel;
    editAccountAPIHostname: TEdit;
    ActionGetLogo: TAction;
    BitBtn3: TBitBtn;
    ActionGetAllUsers: TAction;
    BitBtn4: TBitBtn;
    ActionGetTelephonyCredits: TAction;
    BitBtn5: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionGetAccountsExecute(Sender: TObject);
    procedure ActionGetBillingExecute(Sender: TObject);
    procedure ActionGetLogoExecute(Sender: TObject);
    procedure ActionGetAllUsersExecute(Sender: TObject);
    procedure ActionGetTelephonyCreditsExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FLogoFileName: TFileName;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OnGetBilling(ASender: TObject;
      AAcountID, AEdition, Amessage: string; ASuccess: Boolean);
    procedure OnGetTelephonyCredits(ASender: TObject; AAcountID: string;
      ACredits: integer; Amessage: string; ASuccess: Boolean);
    procedure OnGetUsers(ASender: TObject; AUsers: TLZDUOUsers; Amessage: string;
      ASuccess: Boolean; var AOwnsObjects: Boolean);
  public
    { Public declarations }
  end;

var
  frmAccountsDemo: TfrmAccountsDemo;

implementation

{$R *.dfm}

uses
  System.IOUtils, Lazy.CryptINI, Lazy.Log, Vcl.Imaging.pngimage;

procedure TfrmAccountsDemo.OnGetBilling(ASender: TObject;
  AAcountID, AEdition: string; Amessage: string; ASuccess: Boolean);
begin
  if not ASuccess then
  begin
    MessageDlg(Amessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    MessageDlg(Format('Billing edition for %s is %s', [editAccountID.Text,
      AEdition]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmAccountsDemo.OnGetTelephonyCredits(ASender: TObject;
  AAcountID: string; ACredits: integer; Amessage: string; ASuccess: Boolean);
begin
  if not ASuccess then
  begin
    MessageDlg(Amessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    MessageDlg(Format('Telephony credits for %s is %d', [editAccountID.Text,
      ACredits]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmAccountsDemo.OnGetUsers(ASender: TObject; AUsers: TLZDUOUsers;
  Amessage: string; ASuccess: Boolean; var AOwnsObjects: Boolean);
var
  LUser: TLZDUOUser;
begin
  if not ASuccess then
  begin
    MessageDlg(Amessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    for LUser in AUsers do
    begin
      LazyLog.Log(Self, 'UserID: ' + LUser.UserID + ', Username:' +
        LUser.Username + 'Aliases: ' + LUser.Aliases.DelimitedText + ', Email:'
        + LUser.Email + ', IsEnrolled:' + BoolToStr(LUser.IsEnrolled, true) +
        ', Phones:' + IntToStr(LUser.Phones.Count));
    end;
  end;
end;

procedure TfrmAccountsDemo.ActionGetAllUsersExecute(Sender: TObject);
var
  LDUOAdmin: TLZDUOAdminApi;
begin
  LDUOAdmin := TLZDUOAdminApi.Create(nil);
  try
    LDUOAdmin.IntegrationKey := editIntegrationKey.Text;
    LDUOAdmin.SecretKey := editSecretKey.Text;
    LDUOAdmin.Hostname := editAccountAPIHostname.Text;
    LDUOAdmin.AccountID := editAccountID.Text;
    LDUOAdmin.OnGetUsers := OnGetUsers;
    LDUOAdmin.GetAllUsers;
  finally
    FreeAndNil(LDUOAdmin);
  end;
end;

procedure TfrmAccountsDemo.ActionGetBillingExecute(Sender: TObject);
var
  LAPI: TLZDUOAccountsApi;
begin
  LAPI := TLZDUOAccountsApi.Create(nil);
  try
    LAPI.IntegrationKey := editIntegrationKey.Text;
    LAPI.SecretKey := editSecretKey.Text;
    LAPI.Hostname := editHostname.Text;
    LAPI.OnGetBilling := OnGetBilling;
    LAPI.GetBilling(editAccountID.Text, editAccountAPIHostname.Text);
  finally
    FreeAndNil(LAPI);
  end;
end;

procedure TfrmAccountsDemo.ActionGetLogoExecute(Sender: TObject);
var
  LDUOAdmin: TLZDUOAdminApi;
  LMessage: string;
  LSuccess: Boolean;
begin
  LDUOAdmin := TLZDUOAdminApi.Create(nil);
  try
    LDUOAdmin.IntegrationKey := editIntegrationKey.Text;
    LDUOAdmin.SecretKey := editSecretKey.Text;
    LDUOAdmin.Hostname := editAccountAPIHostname.Text;
    LDUOAdmin.AccountID := editAccountID.Text;

    FLogoFileName := TPath.GetTempPath + 'duo_logo.png';

    LDUOAdmin.GetLogo(FLogoFileName, LMessage, LSuccess);
    if LSuccess then
    begin
      imgLogo.Picture.LoadFromFile(FLogoFileName);
    end
    else
    begin
      MessageDlg(LMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
  finally
    FreeAndNil(LDUOAdmin);
  end;
end;

procedure TfrmAccountsDemo.ActionGetTelephonyCreditsExecute(Sender: TObject);
var
  LAPI: TLZDUOAccountsApi;
begin
  LAPI := TLZDUOAccountsApi.Create(nil);
  try
    LAPI.IntegrationKey := editIntegrationKey.Text;
    LAPI.SecretKey := editSecretKey.Text;
    LAPI.Hostname := editHostname.Text;
    LAPI.OnGetTelephonyCredits := OnGetTelephonyCredits;
    LAPI.GetTelephonyCredits(editAccountID.Text, editAccountAPIHostname.Text);
  finally
    FreeAndNil(LAPI);
  end;
end;

procedure TfrmAccountsDemo.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  memoLog.Lines.Text := LazyLogCache;
end;

procedure TfrmAccountsDemo.FormCreate(Sender: TObject);
begin
  memoLog.Lines.Clear;
  LoadSettings;
  FLogoFileName := '';
end;

procedure TfrmAccountsDemo.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmAccountsDemo.LoadSettings;
var
  LFileName: TFileName;
  LSettings: TLZCryptINI;
begin
  LFileName := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) +
    'duo_accountsdemo.ini';
  LSettings := TLZCryptINI.Create(LFileName);
  try
    editIntegrationKey.Text := LSettings.ReadString('duo',
      'integrationkey', '');
    editSecretKey.Text := LSettings.ReadString('duo', 'secretkey', '');
    editHostname.Text := LSettings.ReadString('duo', 'hostname', '');
  finally
    FreeAndNil(LSettings);
  end;
end;

procedure TfrmAccountsDemo.SaveSettings;
var
  LFileName: TFileName;
  LSettings: TLZCryptINI;
begin
  LFileName := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) +
    'duo_accountsdemo.ini';
  LSettings := TLZCryptINI.Create(LFileName);
  try
    LSettings.WriteString('duo', 'integrationkey', editIntegrationKey.Text);
    LSettings.WriteString('duo', 'secretkey', editSecretKey.Text);
    LSettings.WriteString('duo', 'hostname', editHostname.Text);
    LSettings.UpdateFile;
  finally
    FreeAndNil(LSettings);
  end;
end;

procedure TfrmAccountsDemo.ActionGetAccountsExecute(Sender: TObject);
var
  LAPI: TLZDUOAccountsApi;
begin
  LAPI := TLZDUOAccountsApi.Create(nil);
  try
    LAPI.IntegrationKey := editIntegrationKey.Text;
    LAPI.SecretKey := editSecretKey.Text;
    LAPI.Hostname := editHostname.Text;
    LAPI.GetAccounts(true, true,
      procedure(AAccounts: TLZDUOAccounts; Amessage: string; ASuccess: Boolean;
        var AOwnsObjects: Boolean)
      var
        LItem: TLZDUOAccount;
      begin
        if not ASuccess then
        begin
          MessageDlg(Amessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
        end
        else
        begin
          for LItem in AAccounts do
          begin
            LazyLog.Log(Self, 'AccountID: ' + LItem.AccountID +
              ', Account name:' + LItem.AccountName + 'Hostname: ' +
              LItem.APIHostname + ', Edition: ' + LItem.BillingEdition);
          end;
        end;
      end)
  finally
    FreeAndNil(LAPI);
  end;
end;

end.
