unit VCL.OAuthDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics, System.UITypes,
  VCL.Controls, VCL.Forms, VCL.Dialogs, Lazy.Types, Lazy.RESTClient,
  Lazy.REST.Types,
  VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, System.Actions, VCL.ActnList,
  VCL.Buttons, System.Generics.Collections, System.Messaging, VCL.AppEvnts,
  System.JSON;

type
  TfrmOAuthDemo = class(TForm)
    GridPanel2: TGridPanel;
    BitBtn1: TBitBtn;
    ActionList: TActionList;
    ActionAuthenticate: TAction;
    pnlLog: TPanel;
    Label4: TLabel;
    memoLog: TMemo;
    ApplicationEvents: TApplicationEvents;
    ActionGet: TAction;
    GridPanel3: TGridPanel;
    Label6: TLabel;
    editGet: TEdit;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    Label9: TLabel;
    memoResponse: TMemo;
    GridPanel4: TGridPanel;
    GridPanel1: TGridPanel;
    Label1: TLabel;
    editClientID: TEdit;
    Label2: TLabel;
    editClientSecret: TEdit;
    Label3: TLabel;
    editScope: TEdit;
    Label5: TLabel;
    editTokenEndPoint: TEdit;
    Label7: TLabel;
    editRESTEndPoint: TEdit;
    Panel2: TPanel;
    rgGrantType: TRadioGroup;
    GridPanel5: TGridPanel;
    Label11: TLabel;
    editGrantTypeCustom: TEdit;
    Label10: TLabel;
    editTokenAuthorizationName: TEdit;
    Label8: TLabel;
    editHeaders: TEdit;
    Label12: TLabel;
    editAuthorizeEndPoint: TEdit;
    Label13: TLabel;
    editRedirectURL: TEdit;
    GridPanel6: TGridPanel;
    editAuthToken: TEdit;
    editRefreshToken: TEdit;
    Label14: TLabel;
    editURLVariables: TEdit;
    Label15: TLabel;
    editResource: TEdit;
    Panel3: TPanel;
    GridPanel7: TGridPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    comboBoxOptions: TComboBox;
    procedure ActionAuthenticateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEventsIdle(
      Sender: TObject;
      var Done: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ActionGetExecute(Sender: TObject);
    procedure ActionGetUpdate(Sender: TObject);
    procedure comboBoxOptionsChange(Sender: TObject);
  private
    FOAuthAPI: TLZRESTClientOAuth2;
    procedure OnTokenRequestComplete(
      ASender: TObject;
      ASuccess: Boolean;
      AMessage: string;
      AToken: TLZOAuth2Token);
    procedure OnBrowserLoginRequest(
      ASender: TObject;
      AURL: string;
      AConnection: TLZOAuth2Connection;
      AToken: TLZOAuth2Token;
      AUserDataFolder: string);
    procedure ApplySettings;
    function IsAuthenticated: Boolean;
  public
    procedure Authenticate;
  end;

var
  frmOAuthDemo: TfrmOAuthDemo;

implementation

{$R *.dfm}

uses
  VCL.Lazy.AuthorizeBrowserForm, Lazy.Log,
  VCL.Lazy.Utils.Windows,
  System.StrUtils;

procedure TfrmOAuthDemo.ActionAuthenticateExecute(Sender: TObject);
begin
  Authenticate;
end;

procedure TfrmOAuthDemo.ActionGetExecute(Sender: TObject);
begin
  ApplySettings;
  FOAuthAPI.Get(editGet.Text,
    procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        memoResponse.Lines.Text := ARESTResponse.JSONValue.Format;
      end
      else
      begin
        MessageDlg(AMessage, TMsgDlgType.mtError, [mbOk], 0);
      end;
    end, '', TLZAsyncState.lasFalse);
end;

procedure TfrmOAuthDemo.ActionGetUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsAuthenticated;
end;

procedure TfrmOAuthDemo.ApplicationEventsIdle(
  Sender: TObject;
  var Done: Boolean);
begin
  memoLog.Lines.Text := LazyLogCache;
end;

procedure TfrmOAuthDemo.ApplySettings;
begin
  FOAuthAPI.Connection.ClientId := editClientID.Text;
  FOAuthAPI.Connection.ClientSecret := editClientSecret.Text;
  FOAuthAPI.Connection.Scope := editScope.Text;
  FOAuthAPI.Connection.TokenEndPoint := editTokenEndPoint.Text;
  FOAuthAPI.Connection.RESTEndPoint := editRESTEndPoint.Text;
  FOAuthAPI.Connection.AuthorizeEndPoint := editAuthorizeEndPoint.Text;
  FOAuthAPI.Connection.RedirectURL := editRedirectURL.Text;
  FOAuthAPI.Connection.Headers.FromString(editHeaders.Text);
  FOAuthAPI.Connection.URLVariables.FromString(editURLVariables.Text, true);
  FOAuthAPI.Connection.Resource := editResource.Text;
  FOAuthAPI.Token.GrantType := TLZOAuth2GrantType(rgGrantType.ItemIndex);
  FOAuthAPI.Token.GrantTypeCustom := editGrantTypeCustom.Text;
  FOAuthAPI.Token.TokenAuthorizationName := editTokenAuthorizationName.Text;
end;

function TfrmOAuthDemo.IsAuthenticated: Boolean;
begin
  REsult := False;
  if ASsigned(FOAuthAPI) then
  begin
    REsult := FOAuthAPI.IsAuthenticated;
  end;
end;

procedure TfrmOAuthDemo.Authenticate;
begin
  FOAuthAPI.ClearAuthentication;
  ApplySettings;
  FOAuthAPI.Authenticate(lasDefault);
end;

procedure TfrmOAuthDemo.comboBoxOptionsChange(Sender: TObject);
begin
  case comboBoxOptions.ItemIndex of
    0: // MS Graph
      begin
        editScope.Text := 'offline_access https://graph.microsoft.com/.default';
        editTokenEndPoint.Text :=
          'https://login.microsoftonline.com/%tenantid%/oauth2/v2.0/token';
        editRESTEndPoint.Text := 'https://graph.microsoft.com/';
        editAuthorizeEndPoint.Text :=
          'https://login.microsoftonline.com/%tenantid%/oauth2/v2.0/authorize?client_id=%clientid%&response_type=code&redirect_uri=%redirect_uri%&response_mode=query&scope=%scope%&prompt=%prompt%';
        editRedirectURL.Text := 'http://localhost';
        editGrantTypeCustom.Text := '';
        editTokenAuthorizationName.Text := 'Bearer';
        editHeaders.Text := '';
        editURLVariables.Text := 'prompt=select_account;tenantid={changeme}';
        editGet.Text := 'v1.0/users?$top=999';
        rgGrantType.ItemIndex := 1;
      end;
    1: // Sophos
      begin
        editScope.Text := 'token';
        editTokenEndPoint.Text := 'https://id.sophos.com/api/v2/oauth2/token';
        editRESTEndPoint.Text := 'https://api.central.sophos.com/';
        editAuthorizeEndPoint.Text := '';
        editRedirectURL.Text := '';
        editGrantTypeCustom.Text := '';
        editTokenAuthorizationName.Text := 'Bearer';
        editHeaders.Text := 'X-Partner-ID={changeme}';
        editURLVariables.Text := '';
        editGet.Text := 'partner/v1/billing/usage/2023/10?pageSize=1000';
        rgGrantType.ItemIndex := 1;
      end;
    2: // Barracuda
      begin
        editScope.Text :=
          'partners_read+accounts_read+accounts_write+computers_read+computers_write';
        editTokenEndPoint.Text := 'https://auth.echo.intronis.com/oauth2/token';
        editRESTEndPoint.Text := 'https://api.echo.intronis.com/';
        editAuthorizeEndPoint.Text := '';
        editRedirectURL.Text := '';
        editGrantTypeCustom.Text := '';
        editTokenAuthorizationName.Text := 'OAuth';
        editHeaders.Text := 'Accept=application/json';
        editURLVariables.Text := '';
        editGet.Text := 'v1/partners/{partnername}/signups';
        rgGrantType.ItemIndex := 3;
      end;
  end;
end;

procedure TfrmOAuthDemo.FormCreate(Sender: TObject);
begin
  FOAuthAPI := TLZRESTClientOAuth2.Create;
  FOAuthAPI.Token.GrantType := gtClientCredentials;
  FOAuthAPI.OnOAuth2TokenRequestComplete := OnTokenRequestComplete;
  FOAuthAPI.OnBrowserLoginRequest := OnBrowserLoginRequest;
end;

procedure TfrmOAuthDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOAuthAPI);
end;

procedure TfrmOAuthDemo.OnBrowserLoginRequest(
  ASender: TObject;
  AURL: string;
  AConnection: TLZOAuth2Connection;
  AToken: TLZOAuth2Token;
  AUserDataFolder: string);
var
  LMessage: string;
  LForm: TLZAuthorizeBrowserForm;
begin
  LForm := TLZAuthorizeBrowserForm.Create(Self);
  try
    if LForm.GetAuthToken(LMessage, AURL, AConnection, AToken, AUserDataFolder)
    then
    begin
      LazyLog.Debug(Self, 'OnBrowserLoginRequest', AToken.AuthToken);
      FOAuthAPI.RequestToken;
    end
    else
    begin
      MessageDlg(LMessage, TMsgDlgType.mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TfrmOAuthDemo.OnTokenRequestComplete(
  ASender: TObject;
  ASuccess: Boolean;
  AMessage: string;
  AToken: TLZOAuth2Token);
begin
  try
    if ASuccess then
    begin
      editAuthToken.Text := AToken.AuthToken;
      editRefreshToken.Text := AToken.RefreshToken;
      MessageDlg('Token expires: ' + DateTimeToStr(AToken.ExpiresIn),
        TMsgDlgType.mtInformation, [mbOk], 0);
    end
    else
    begin
      MessageDlg(AMessage, TMsgDlgType.mtError, [mbOk], 0);
    end;
  finally

  end;
end;

end.
