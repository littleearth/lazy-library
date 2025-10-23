unit FMX.SharePointDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation, Lazy.Types, MSGraph.Core,
  MSGraph.API, Lazy.REST.Types, MSGraph.Models, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.ListBox, System.Actions, FMX.ActnList;

type
  TfrmSharepointDemo = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    Panel1: TPanel;
    Label1: TLabel;
    editTennantID: TEdit;
    Panel2: TPanel;
    Label2: TLabel;
    editClientID: TEdit;
    Panel3: TPanel;
    Label3: TLabel;
    editClientSecret: TEdit;
    Button1: TButton;
    memoLog: TMemo;
    Panel4: TPanel;
    Label4: TLabel;
    comboSites: TComboBox;
    Button2: TButton;
    Panel5: TPanel;
    Label5: TLabel;
    comboDrives: TComboBox;
    Button3: TButton;
    OpenDialog: TOpenDialog;
    Panel6: TPanel;
    Label6: TLabel;
    editFolderPath: TEdit;
    Button4: TButton;
    ActionList: TActionList;
    ActionRefreshSites: TAction;
    ActionRefreshDrives: TAction;
    ActionAuthenticate: TAction;
    ActionUpload: TAction;
    pnlProgress: TPanel;
    ProgressBar: TProgressBar;
    lblProgress: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionRefreshSitesExecute(Sender: TObject);
    procedure ActionRefreshDrivesExecute(Sender: TObject);
    procedure ActionRefreshDrivesUpdate(Sender: TObject);
    procedure ActionRefreshSitesUpdate(Sender: TObject);
    procedure ActionAuthenticateExecute(Sender: TObject);
    procedure ActionAuthenticateUpdate(Sender: TObject);
    procedure ActionUploadExecute(Sender: TObject);
    procedure ActionUploadUpdate(Sender: TObject);
    procedure comboSitesChange(Sender: TObject);
  private
    FMSGraphAPI: TLZMSGraphAPI;
    procedure OnBrowserLoginRequest(
      ASender: TObject;
      AURL: string;
      AConnection: TLZOAuth2Connection;
      AToken: TLZOAuth2Token;
      AUserDataFolder: string);
    procedure OnTokenRequestComplete(
      ASender: TObject;
      ASuccess: Boolean;
      AMessage: string;
      AToken: TLZOAuth2Token);
    procedure Log(AMessage: string);
    function UploadFile(
      const AUploadURL: string;
      AFileName: TFileName): Boolean;
    function CreateUploadSession(const DriveId, FolderPath,
      FileName: string): string;
  public
    procedure Authenticate;
    procedure GetSites;
    procedure GetDrives(ASiteID: string);
  end;

var
  frmSharepointDemo: TfrmSharepointDemo;

implementation

{$R *.fmx}

uses
  FMX.Lazy.AuthorizeBrowserForm, Lazy.Log, Lazy.Utils, System.JSON;

const
  API_VERSION = 'v1.0/';

  { TfrmSharepointDemo }

procedure TfrmSharepointDemo.OnBrowserLoginRequest(
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
      FMSGraphAPI.RequestToken;
    end
    else
    begin
      Log(LMessage);
      LazyLog.Error(Self, LMessage);
    end;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TfrmSharepointDemo.OnTokenRequestComplete(
  ASender: TObject;
  ASuccess: Boolean;
  AMessage: string;
  AToken: TLZOAuth2Token);
begin
  if ASuccess then
  begin
    Log('Expires: ' + DateTimeToStr(AToken.ExpiresIn));
    GetSites;
  end
  else
  begin
    Log(AMessage);
  end;
end;

procedure TfrmSharepointDemo.FormCreate(Sender: TObject);
begin
  FMSGraphAPI := TLZMSGraphAPI.Create;
  FMSGraphAPI.Token.GrantType := gtAuthorizationCode;
  FMSGraphAPI.Connection.Scope := 'https://graph.microsoft.com/.default';
  FMSGraphAPI.OnBrowserLoginRequest := OnBrowserLoginRequest;
  FMSGraphAPI.OnOAuth2TokenRequestComplete := OnTokenRequestComplete;
end;

procedure TfrmSharepointDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMSGraphAPI);
end;

procedure TfrmSharepointDemo.ActionAuthenticateExecute(Sender: TObject);
begin
  if FMSGraphAPI.IsAuthenticated then
  begin
    FMSGraphAPI.ClearAuthentication;
  end
  else
  begin
    Authenticate;
  end;
end;

procedure TfrmSharepointDemo.ActionAuthenticateUpdate(Sender: TObject);
begin
  if FMSGraphAPI.IsAuthenticated then
  begin
    (Sender as TAction).Caption := 'Sign out';
  end
  else
  begin
    (Sender as TAction).Caption := 'Sign in';
  end;
end;

procedure TfrmSharepointDemo.ActionRefreshDrivesExecute(Sender: TObject);
var
  LSiteID: string;
begin
  LSiteID := comboSites.Items.ValueFromIndex[comboSites.ItemIndex];
  Log(LSiteID);
  GetDrives(comboSites.Items[comboSites.ItemIndex]);
end;

procedure TfrmSharepointDemo.ActionRefreshDrivesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FMSGraphAPI.IsAuthenticated and
    (comboSites.ItemIndex <> -1);
end;

procedure TfrmSharepointDemo.ActionRefreshSitesExecute(Sender: TObject);
begin
  GetSites;
end;

procedure TfrmSharepointDemo.ActionRefreshSitesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FMSGraphAPI.IsAuthenticated;
end;

procedure TfrmSharepointDemo.ActionUploadExecute(Sender: TObject);
var
  LDriveID: string;
  LUploadURL: string;
begin
  LDriveID := comboDrives.Items.ValueFromIndex[comboDrives.ItemIndex];
  if OpenDialog.Execute then
  begin
    LUploadURL := CreateUploadSession(LDriveID, editFolderPath.Text,
      OpenDialog.FileName);
    UploadFile(LUploadURL, OpenDialog.FileName);
  end;
end;

procedure TfrmSharepointDemo.ActionUploadUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FMSGraphAPI.IsAuthenticated and
    (comboDrives.ItemIndex <> -1);
end;

procedure TfrmSharepointDemo.Authenticate;
begin
  (FMSGraphAPI.Connection as TLZMSGraphOAuth2Connection).TenantId :=
    editTennantID.Text;
  FMSGraphAPI.Connection.ClientId := editClientID.Text;
  FMSGraphAPI.Connection.ClientSecret := editClientSecret.Text;
  FMSGraphAPI.ClearAuthentication;
  FMSGraphAPI.Authenticate;
end;

procedure TfrmSharepointDemo.comboSitesChange(Sender: TObject);
begin
  ActionRefreshDrives.Execute;
end;

procedure TfrmSharepointDemo.GetDrives(ASiteID: string);
var
  LNextLink: string;
  LItems: TLZMSGraphDrives;
  LItem: TLZMSGraphDrive;
begin
  LItems := TLZMSGraphDrives.Create;
  comboDrives.Items.Clear;
  comboDrives.Items.BeginUpdate;
  try
    LNextLink := Format(API_VERSION + 'sites/%s/drives', [ASiteID]);
    Repeat
      FMSGraphAPI.Get(LNextLink,
        procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
          ARESTResponse: TRESTResponse; ACustomData: string)
        begin
          LNextLink := '';
          if ASuccess then
          begin
            Log(ARESTResponse.JSONValue.ToJSON);
            LItems.FromJSONValue(ARESTResponse.JSONValue, false);
            LNextLink := FMSGraphAPI.GetODataNextLink(ARESTResponse.JSONValue);
          end
          else
          begin
            Log(AMessage);
            LazyLog.Error(Self, AMessage);
          end;
        end, '', TLZAsyncState.lasFalse);
    Until LNextLink = '';

    for LItem in LItems do
    begin
      comboDrives.Items.AddPair(LItem.DisplayName, LItem.ID);
    end;
  finally
    FreeAndNil(LItems);
    comboDrives.Items.EndUpdate;
  end;
end;

procedure TfrmSharepointDemo.GetSites;
var
  LNextLink: string;
  LItems: TLZMSGraphSites;
  LItem: TLZMSGraphSite;
begin
  LItems := TLZMSGraphSites.Create;
  comboSites.Items.Clear;
  comboSites.Items.BeginUpdate;
  try
    LNextLink := API_VERSION + 'sites';
    Repeat
      FMSGraphAPI.Get(LNextLink,
        procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
          ARESTResponse: TRESTResponse; ACustomData: string)
        begin
          LNextLink := '';
          if ASuccess then
          begin
            Log(ARESTResponse.JSONValue.ToJSON);
            LItems.FromJSONValue(ARESTResponse.JSONValue, false);
            LNextLink := FMSGraphAPI.GetODataNextLink(ARESTResponse.JSONValue);
          end
          else
          begin
            Log(AMessage);
            LazyLog.Error(Self, AMessage);
          end;
        end, '', TLZAsyncState.lasFalse);
    Until LNextLink = '';

    for LItem in LItems do
    begin
      comboSites.Items.AddPair(LItem.DisplayName, LItem.ID);
    end;
  finally
    FreeAndNil(LItems);
    comboSites.Items.EndUpdate;
  end;
end;

procedure TfrmSharepointDemo.Log(AMessage: string);
begin
  memoLog.Lines.Insert(0, AMessage);
end;

function TfrmSharepointDemo.CreateUploadSession(const DriveId, FolderPath,
  FileName: string): string;
var
  LJsonBody: TJSONObject;
  LURL: string;
  LUploadURL: string;
begin
  LUploadURL := '';
  LJsonBody := TJSONObject.Create;
  try
    LJsonBody.AddPair('item',
      TJSONObject.Create.AddPair('@microsoft.graph.conflictBehavior', 'replace')
      .AddPair('name', ExtractFileName(FileName)));

    LURL := Format(API_VERSION + '/drives/%s/root:/%s/%s:/createUploadSession',
      [DriveId, FolderPath, ExtractFileName(FileName)]);
    FMSGraphAPI.Post(LURL, LJsonBody.ToJSON,
      procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      begin
        if ASuccess then
        begin
          Log(ARESTResponse.JSONValue.Format());
          LUploadURL := ARESTResponse.JSONValue.GetValue<string>('uploadUrl');
        end
        else
        begin
          Log(AMessage);
          LazyLog.Error(Self, AMessage);
        end;
      end, '', TLZAsyncState.lasFalse);
  finally
    LJsonBody.Free;
    Result := LUploadURL;
  end;
end;

function TfrmSharepointDemo.UploadFile(
  const AUploadURL: string;
  AFileName: TFileName): Boolean;
var
  LSuccess: Boolean;
  LItem: TLZMSGraphDriveItem;
begin
  LSuccess := false;
  FMSGraphAPI.UploadFile(AUploadURL, AFileName,
    procedure(ASender: TObject; AProgress: Integer; AMessage: string;
      var ACancel: Boolean)
    begin
      lblProgress.Text := AMessage;
      ProgressBar.Value := AProgress;
      Application.ProcessMessages;
    end,
    procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        LItem := TLZMSGraphDriveItem.Create;
        try
          LItem.FromJSONValue(ARESTResponse.JSONValue);
          Log(LItem.WebURL);
        finally
          LItem.Free;
        end;
        LSuccess := TRue;
      end
      else
      begin
        Log(AMessage);
        LazyLog.Error(Self, AMessage);
      end;
    end);
  Result := LSuccess;
end;

end.
