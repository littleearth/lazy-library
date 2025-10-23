unit frmUniFiClientU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.UITypes,
  Vcl.ExtCtrls, Vcl.Buttons, System.JSON, UniFi.APi, UniFi.Models;

type
  TfrmUniFiClient = class(TForm)
    pnlControls: TGridPanel;
    pnlSettings: TGridPanel;
    pnlBaseURL: TPanel;
    Label4: TLabel;
    editBaseURL: TEdit;
    pnlUser: TPanel;
    Label3: TLabel;
    editUser: TEdit;
    pnlPassword: TPanel;
    Label1: TLabel;
    editPassword: TEdit;
    pnlSiteID: TPanel;
    Label2: TLabel;
    TextEditorResponse: TMemo;
    btnSites: TBitBtn;
    btnDevices: TBitBtn;
    btnEvents: TBitBtn;
    btnWLANConfs: TBitBtn;
    btnSiteStats: TBitBtn;
    editSiteID: TComboBox;
    BitBtn1: TBitBtn;
    btnListAdmins: TBitBtn;
    btnDeleteUser: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSitesClick(Sender: TObject);
    procedure btnSiteStatsClick(Sender: TObject);
    procedure btnDevicesClick(Sender: TObject);
    procedure btnWLANConfsClick(Sender: TObject);
    procedure btnEventsClick(Sender: TObject);
    procedure editSiteIDChange(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnListAdminsClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
  private
    FClient: TLZUniFiClient;
    FSites: TLZUniFiSites;
    procedure ApplyClientSettings;
    function IsLoggedIn: boolean;
  public
    { Public declarations }
  end;

var
  frmUniFiClient: TfrmUniFiClient;

implementation

uses
  Lazy.Utils.Windows;

{$R *.dfm}

procedure TfrmUniFiClient.ApplyClientSettings;
begin
  FClient.BaseUrl := editBaseURL.Text;
  FClient.User := editUser.Text;
  FClient.Password := editPassword.Text;
  FClient.Site := editSiteID.Text;
end;

procedure TfrmUniFiClient.btnSitesClick(Sender: TObject);
var
  LSite: TLZUniFiSite;
  LJSON: TJSONArray;
begin
  LJSON := nil;
  try
    if IsLoggedIn then
    begin
      editSiteID.Items.Clear;
      LJSON := FClient.ListSites;
      FSites.FromJSONValue(LJSON);
      for LSite in FSites do
      begin
        editSiteID.Items.Add(LSite.Name);
        TextEditorResponse.Lines.Add(LSite.Name + ' ' + LSite.Description);
      end;
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TfrmUniFiClient.btnSiteStatsClick(Sender: TObject);
begin
  if IsLoggedIn then
  begin
    TextEditorResponse.Lines.Text := FClient.GetSiteStats.Format();
  end;
end;

procedure TfrmUniFiClient.btnWLANConfsClick(Sender: TObject);
var
  LWLANConfs: TLZUniFiWLANConfs;
  LWLANConf: TLZUniFiWLANConf;
  LSite: TLZUniFiSite;
  LJSON: TJSONArray;
begin
  LWLANConfs := TLZUniFiWLANConfs.Create;
  try
    if IsLoggedIn then
    begin

      for LSite in FSites do
      begin
        FClient.Site := LSite.Name;
        TextEditorResponse.Lines.Add('Gathering WLAN configs for site ' +
          LSite.Description);
        LJSON := FClient.ListWLANConf;
        LWLANConfs.FromJSONValue(LJSON, False, '', true);
      end;

      for LWLANConf in LWLANConfs do
      begin
        TextEditorResponse.Lines.Add(LWLANConf.ID + ' ' + LWLANConf.Name + ' ' +
          LWLANConf.Security + ' ' + LWLANConf.Passphrase);
      end;
    end;
  finally
    editSiteIDChange(Sender);
    LWLANConfs.Free;
  end;
end;

procedure TfrmUniFiClient.editSiteIDChange(Sender: TObject);
begin
  FClient.Site := editSiteID.Text;
end;

procedure TfrmUniFiClient.BitBtn1Click(Sender: TObject);
var
  LSiteID: string;
  LWLANID: string;
  LPassPhrase: string;
begin
  try
    if IsLoggedIn then
    begin
      LSiteID := InputBox('SiteID', 'Enter Site ID', FClient.Site);
      LWLANID := InputBox('WLANID', 'Enter WLAN ID', '');
      LPassPhrase := InputBox('Passphrase', 'Enter passphrase', '');
      if (not TLZString.IsEmptyString(LWLANID)) and
        (not TLZString.IsEmptyString(LPassPhrase)) and
        (not TLZString.IsEmptyString(LSiteID)) then
      begin
        FClient.Site := LSiteID;
        if FClient.SetWLANSettings(LWLANID, LPassPhrase) then
        begin
          MessageDlg('Passphrase updated', TMsgDlgType.mtInformation,
            [TMsgDlgBtn.mbOK], 0);
        end
        else
        begin
          MessageDlg('Failed to update passphrase', TMsgDlgType.mtError,
            [TMsgDlgBtn.mbOK], 0);
        end;
      end
      else
      begin
        MessageDlg('SiteID, WLANID and Passphrase must be specified',
          TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
    end;
  finally
    editSiteIDChange(Sender);
  end;
end;

procedure TfrmUniFiClient.btnDeleteUserClick(Sender: TObject);
var
  LAdmin: TJSONObject;
  LAdminId, LName: string;
begin
  LAdmin := nil;
  try
    if IsLoggedIn then
    begin
      if InputQuery('Username', 'Username', LName) then
      begin
        LAdmin := FClient.FindAdminByName(LName);
        if Assigned(LAdmin) then
        begin
          LAdminId := LAdmin.GetValue<string>('_id');
          if MessageDlg(Format('Delete user %s?', [LAdminId]),
            TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
          then
          begin
            TextEditorResponse.Lines.Clear;
            // Option 1: Complete super admin removal
            if FClient.DeleteSuperAdmin(LAdminId) then
              TextEditorResponse.Lines.Add('Super admin removed successfully');

            // Option 2: Just remove from all sites (keeps user account)
            if FClient.DeleteUserFromAllSites(LAdminId) then
              TextEditorResponse.Lines.Add('User removed from all sites');
          end;
        end;
      end;
    end;
  finally
    LAdmin.Free;
  end;
end;

procedure TfrmUniFiClient.btnDevicesClick(Sender: TObject);
var
  LDevices: TLZUniFiDevices;
  LDevice: TLZUniFiDevice;
  LSite: TLZUniFiSite;
  LJSON: TJSONArray;
begin
  LDevices := TLZUniFiDevices.Create;
  try
    if IsLoggedIn then
    begin
      for LSite in FSites do
      begin
        FClient.Site := LSite.Name;
        TextEditorResponse.Lines.Add('Gathering devices for site ' +
          LSite.Description);
        LJSON := FClient.ListDevices;
        LDevices.FromJSONValue(LJSON, False, '', true);
      end;
      for LDevice in LDevices do
      begin
        TextEditorResponse.Lines.Add(LDevice.ID + ' ' + LDevice.Name + ' ' +
          LDevice.Model + ' ' + LDevice.State.ToString + ' ' + LDevice.InformURL
          + ' ' + LDevice.Serial + ' ' + LDevice.MAC);
      end;

    end;
  finally
    FreeAndNil(LDevices);
    editSiteIDChange(Sender);
  end;
end;

procedure TfrmUniFiClient.btnEventsClick(Sender: TObject);
begin
  if IsLoggedIn then
  begin
    TextEditorResponse.Lines.Text := FClient.ListEvents.Format();
  end;
end;

procedure TfrmUniFiClient.btnListAdminsClick(Sender: TObject);
var
  LJSON: TJSONArray;
begin
  LJSON := nil;
  try
    if IsLoggedIn then
    begin
      editSiteID.Items.Clear;
      LJSON := FClient.ListAdmins;
      TextEditorResponse.Lines.Add(LJSON.Format());
      // FSites.FromJSONValue(LJSON);
      // for LSite in FSites do
      // begin
      // editSiteID.Items.Add(LSite.Name);
      // TextEditorResponse.Lines.Add(LSite.Name + ' ' + LSite.Description);
      // end;
    end;
  finally
    LJSON.Free;
  end;
end;

procedure TfrmUniFiClient.FormCreate(Sender: TObject);
begin
  editSiteID.Text := UNIFI_DEFAULT_SITE;
  FClient := TLZUniFiClient.Create;
  FSites := TLZUniFiSites.Create;
  ApplyClientSettings;
end;

procedure TfrmUniFiClient.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClient);
  FreeAndNil(FSites);
end;

function TfrmUniFiClient.IsLoggedIn: boolean;
begin
  if not FClient.IsLoggedIn then
  begin
    ApplyClientSettings;
    FClient.Login;
  end;
  Result := FClient.IsLoggedIn
end;

end.
