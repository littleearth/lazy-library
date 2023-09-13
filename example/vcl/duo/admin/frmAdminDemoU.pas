unit frmAdminDemoU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, System.UITypes,
  System.Actions, Vcl.ActnList,
  duo.api, duo.admin, duo.models;

type
  TfrmAdminDemo = class(TForm)
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
    Panel5: TPanel;
    Label3: TLabel;
    editHostname: TEdit;
    ActionList: TActionList;
    ActionCheck: TAction;
    ActionPreAdmin: TAction;
    ActionLogin: TAction;
    ActionGetAllUsers: TAction;
    ActionLogo: TAction;
    imgLogo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ActionLogoExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionGetAllUsersExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
  private
    FLogoFileName: TFileName;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure Log(Amessage: string);
    procedure OnGetUsers(ASender: TObject; AUsers: TDUOUsers; Amessage: string;
      ASuccess: boolean; var AOwnsObjects: boolean);
  public
    { Public declarations }
  end;

var
  frmAdminDemo: TfrmAdminDemo;

implementation

{$R *.dfm}

uses
  System.IOUtils, Lazy.CryptINI, Lazy.Log.Basic, Lazy.Log;

procedure TfrmAdminDemo.Log(Amessage: string);
begin
  LazyLog.Log(Self, Amessage);
end;

procedure TfrmAdminDemo.FormCreate(Sender: TObject);
begin
  memoLog.Lines.Clear;
  LoadSettings;
  FLogoFileName := '';
end;

procedure TfrmAdminDemo.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmAdminDemo.LoadSettings;
var
  LFileName: TFileName;
  LSettings: TCryptINI;
begin
  LFileName := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) +
    'duo_Admindemo.ini';
  LSettings := TCryptINI.Create(LFileName);
  try
    editIntegrationKey.Text := LSettings.ReadString('duo',
      'integrationkey', '');
    editSecretKey.Text := LSettings.ReadString('duo', 'secretkey', '');
    editHostname.Text := LSettings.ReadString('duo', 'hostname', '');
  finally
    FreeAndNil(LSettings);
  end;
end;

procedure TfrmAdminDemo.SaveSettings;
var
  LFileName: TFileName;
  LSettings: TCryptINI;
begin
  LFileName := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) +
    'duo_admindemo.ini';
  LSettings := TCryptINI.Create(LFileName);
  try
    LSettings.WriteString('duo', 'integrationkey', editIntegrationKey.Text);
    LSettings.WriteString('duo', 'secretkey', editSecretKey.Text);
    LSettings.WriteString('duo', 'hostname', editHostname.Text);
    LSettings.UpdateFile;
  finally
    FreeAndNil(LSettings);
  end;
end;

procedure TfrmAdminDemo.OnGetUsers(ASender: TObject; AUsers: TDUOUsers;
  Amessage: string; ASuccess: boolean; var AOwnsObjects: boolean);
var
  LUser: TDUOUser;
begin
  if not ASuccess then
  begin
    MessageDlg(Amessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    for LUser in AUsers.Items do
    begin
      Log('UserID: ' + LUser.UserID + ', Username:' + LUser.Username +
        'Aliases: ' + LUser.Aliases.DelimitedText + ', Email:' + LUser.Email +
        ', IsEnrolled:' + BoolToStr(LUser.IsEnrolled, true) + ', Phones:' +
        IntToStr(LUser.Phones.Count));
    end;
  end;
end;

procedure TfrmAdminDemo.ActionGetAllUsersExecute(Sender: TObject);
var
  LDUOAdmin: TDUOAdminApi;
begin
  LDUOAdmin := TDUOAdminApi.Create(nil);
  try
    LDUOAdmin.IntegrationKey := editIntegrationKey.Text;
    LDUOAdmin.SecretKey := editSecretKey.Text;
    LDUOAdmin.Hostname := editHostname.Text;
    LDUOAdmin.OnGetUsers := OnGetUsers;
    LDUOAdmin.GetAllUsers;
  finally
    FreeAndNil(LDUOAdmin);
  end;
end;

procedure TfrmAdminDemo.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  memoLog.Lines.Text := TLazyLogBasic.Text;
end;

procedure TfrmAdminDemo.ActionLogoExecute(Sender: TObject);
var
  LDUOAdmin: TDUOAdminApi;
  LMessage: string;
  LSuccess: boolean;
begin
  LDUOAdmin := TDUOAdminApi.Create(nil);
  try
    LDUOAdmin.IntegrationKey := editIntegrationKey.Text;
    LDUOAdmin.SecretKey := editSecretKey.Text;
    LDUOAdmin.Hostname := editHostname.Text;

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

end.
