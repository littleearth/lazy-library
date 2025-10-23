unit VCL.MSGraphDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics, System.UITypes,
  VCL.Controls, VCL.Forms, VCL.Dialogs, Lazy.Types, MSGraph.Core,
  MSGraph.API, Lazy.REST.Types, MSGraph.Models,
  VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, System.Actions, VCL.ActnList,
  VCL.Buttons, System.Generics.Collections, System.Messaging, VCL.AppEvnts,
  System.JSON, VCL.Menus;

type
  TfrmMSGraphDemo = class(TForm)
    GridPanel1: TGridPanel;
    Label1: TLabel;
    editTennantID: TEdit;
    Label2: TLabel;
    editClientID: TEdit;
    Label3: TLabel;
    editClientSecret: TEdit;
    GridPanel2: TGridPanel;
    BitBtn1: TBitBtn;
    ActionList: TActionList;
    ActionMessageRules: TAction;
    pnlLog: TPanel;
    Label4: TLabel;
    memoLog: TMemo;
    Splitter1: TSplitter;
    ApplicationEvents: TApplicationEvents;
    Panel1: TPanel;
    ProgressBar: TProgressBar;
    lblProgress: TLabel;
    ActionDirectorySubscriptions: TAction;
    PageControlOutput: TPageControl;
    tabMessageRules: TTabSheet;
    tabDirectorySubscriptions: TTabSheet;
    ListViewMessageRules: TListView;
    ListViewDirectorySubscriptions: TListView;
    BitBtn2: TBitBtn;
    tabUserLicenses: TTabSheet;
    ActionUserLicenses: TAction;
    BitBtn3: TBitBtn;
    ListViewUserLicenses: TListView;
    BitBtn4: TBitBtn;
    ActionSignIns: TAction;
    tabSignIns: TTabSheet;
    ListViewSignIns: TListView;
    PopupMenuListView: TPopupMenu;
    ListViewSaveDialog: TSaveDialog;
    ActionExportToCSV: TAction;
    ActionExportToCSV1: TMenuItem;
    tabApplications: TTabSheet;
    ListViewApplications: TListView;
    BitBtn5: TBitBtn;
    ActionApplications: TAction;
    tabAuthenticationMethods: TTabSheet;
    BitBtn6: TBitBtn;
    ListViewAuthenticationMethods: TListView;
    ActionAuthenticationMethods: TAction;
    procedure ActionMessageRulesExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEventsIdle(
      Sender: TObject;
      var Done: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure ActionDirectorySubscriptionsExecute(Sender: TObject);
    procedure ActionUserLicensesExecute(Sender: TObject);
    procedure ActionSignInsExecute(Sender: TObject);
    procedure ActionExportToCSVExecute(Sender: TObject);
    procedure ActionApplicationsExecute(Sender: TObject);
    procedure ActionAuthenticationMethodsExecute(Sender: TObject);
  private
    FMSGraphAPI: TLZMSGraphAPI;
    FSuspicious: TStringList;
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
    procedure ListUsers(AUsers: TLZMSGraphUsers);
    procedure ListRules(
      AUser: TLZMSGraphUser;
      AFolders: TLZMSGraphMailFolders);
    procedure ListFolders(
      AUser: TLZMSGraphUser;
      AFolders: TLZMSGraphMailFolders);
    function GetODataNextLink(AJSONValue: TJSONValue): string;
    procedure Progress(
      AMessage: string;
      AProgress: integer;
      AMax: integer = 100);
    procedure ListChildFolders(AFolders: TLZMSGraphMailFolders);
    procedure ListDirectorySubscriptions;
    procedure ListUserLicenses(AUsers: TLZMSGraphUsers);
    procedure ListSignIns;
    procedure ListApplications;
    procedure ListAuthenticationMethods;
    procedure SaveListViewToCSV(
      AListView: TListView;
      const AFileName: string);
  public
    procedure Authenticate;
  end;

var
  frmMSGraphDemo: TfrmMSGraphDemo;

implementation

{$R *.dfm}

uses
  VCL.Lazy.AuthorizeBrowserForm, Lazy.Log,
  VCL.Lazy.Utils.Windows, System.StrUtils, System.DateUtils;

const
  API_VERSION = 'v1.0/';

procedure TfrmMSGraphDemo.SaveListViewToCSV(
  AListView: TListView;
  const AFileName: string);
var
  CSVFile: TextFile;
  i, j: integer;
  Line: string;
begin
  AssignFile(CSVFile, AFileName);
  Rewrite(CSVFile);
  try
    Line := '';
    for i := 0 to Pred(AListView.Columns.Count) do
    begin
      Line := Line + AnsiQuotedStr
        (TLZString.StringCleaner(AListView.Columns[i].Caption, true,
        true), '"');
      if i < Pred(AListView.Columns.Count) then
        Line := Line + ',';
    end;
    Writeln(CSVFile, Line);

    for i := 0 to Pred(AListView.Items.Count) do
    begin
      Line := AnsiQuotedStr(TLZString.StringCleaner(AListView.Items[i].Caption,
        true, true), '"');
      for j := 0 to Pred(AListView.Items[i].SubItems.Count) do
      begin
        Line := Line + ',' + AnsiQuotedStr
          (TLZString.StringCleaner(AListView.Items[i].SubItems[j], true,
          true), '"');
      end;
      Writeln(CSVFile, Line);
    end;
  finally
    CloseFile(CSVFile);
  end;
end;

procedure TfrmMSGraphDemo.ActionExportToCSVExecute(Sender: TObject);
var
  LListView: TListView;
begin
  LListView := nil;
  if PageControlOutput.ActivePage = tabMessageRules then
  begin
    LListView := ListViewMessageRules;
  end;
  if PageControlOutput.ActivePage = tabDirectorySubscriptions then
  begin
    LListView := ListViewDirectorySubscriptions;
  end;
  if PageControlOutput.ActivePage = tabUserLicenses then
  begin
    LListView := ListViewUserLicenses;
  end;
  if PageControlOutput.ActivePage = tabSignIns then
  begin
    LListView := ListViewSignIns;
  end;
  if PageControlOutput.ActivePage = tabApplications then
  begin
    LListView := ListViewApplications;
  end;
  if PageControlOutput.ActivePage = tabAuthenticationMethods then
  begin
    LListView := ListViewAuthenticationMethods;
  end;

  if Assigned(LListView) and (ListViewSaveDialog.Execute) then
  begin
    SaveListViewToCSV(LListView, ListViewSaveDialog.FileName);
  end;
end;

procedure TfrmMSGraphDemo.ActionMessageRulesExecute(Sender: TObject);
begin
  PageControlOutput.ActivePage := tabMessageRules;
  ListViewMessageRules.Clear;
  Authenticate;
end;

procedure TfrmMSGraphDemo.ActionSignInsExecute(Sender: TObject);
begin
  PageControlOutput.ActivePage := tabSignIns;
  ListViewSignIns.Clear;
  Authenticate;
end;

procedure TfrmMSGraphDemo.ActionUserLicensesExecute(Sender: TObject);
begin
  PageControlOutput.ActivePage := tabUserLicenses;
  ListViewUserLicenses.Clear;
  Authenticate;
end;

procedure TfrmMSGraphDemo.ActionApplicationsExecute(Sender: TObject);
begin
  PageControlOutput.ActivePage := tabApplications;
  ListViewApplications.Clear;
  Authenticate;
end;

procedure TfrmMSGraphDemo.ActionAuthenticationMethodsExecute(Sender: TObject);
begin
  PageControlOutput.ActivePage := tabAuthenticationMethods;
  ListViewAuthenticationMethods.Clear;
  Authenticate;
end;

procedure TfrmMSGraphDemo.ActionDirectorySubscriptionsExecute(Sender: TObject);
begin
  PageControlOutput.ActivePage := tabDirectorySubscriptions;
  ListViewDirectorySubscriptions.Clear;
  Authenticate;
end;

procedure TfrmMSGraphDemo.ApplicationEventsIdle(
  Sender: TObject;
  var Done: Boolean);
begin
  memoLog.Lines.Text := LazyLogCache;
end;

procedure TfrmMSGraphDemo.Authenticate;
begin
  (FMSGraphAPI.Connection as TLZMSGraphOAuth2Connection).TenantId :=
    editTennantID.Text;
  FMSGraphAPI.Connection.ClientId := editClientID.Text;
  FMSGraphAPI.Connection.ClientSecret := editClientSecret.Text;
  FMSGraphAPI.ClearAuthentication;
  FMSGraphAPI.Authenticate;
end;

procedure TfrmMSGraphDemo.FormCreate(Sender: TObject);
begin
  FMSGraphAPI := TLZMSGraphAPI.Create;
  FSuspicious := TStringList.Create;
  FMSGraphAPI.Token.GrantType := gtAuthorizationCode;
  FMSGraphAPI.Connection.Scope := 'https://graph.microsoft.com/.default';
  FMSGraphAPI.OnBrowserLoginRequest := OnBrowserLoginRequest;
  FMSGraphAPI.OnOAuth2TokenRequestComplete := OnTokenRequestComplete;
  PageControlOutput.ActivePageIndex := 0;
end;

procedure TfrmMSGraphDemo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMSGraphAPI);
  FreeAndNil(FSuspicious);
end;

procedure TfrmMSGraphDemo.Progress(
  AMessage: string;
  AProgress: integer;
  AMax: integer);
begin
  ProgressBar.Position := TLZMath.CalculatePercentage(AProgress, AMax);
  lblProgress.Caption := AMessage;
  Application.ProcessMessages;
end;

procedure TfrmMSGraphDemo.ListApplications;
var
  LNextLink: string;
  LItems: TLZMSGraphApplications;
begin
  LItems := TLZMSGraphApplications.Create;
  try
    LNextLink := API_VERSION + 'applications';
    Repeat
      Progress(Format('Gathering Applications %s', [LNextLink]), 0);
      FMSGraphAPI.Get(LNextLink,
        procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
          ARESTResponse: TRESTResponse; ACustomData: string)
        var
          LIdx: integer;
          LPassword: TLZMSGraphPasswordCredential;
          LPasswordList, LPasswordExpiryList: string;
        begin
          LNextLink := '';
          if ASuccess then
          begin
            LNextLink := GetODataNextLink(ARESTResponse.JSONValue);
            LItems.FromJSONValue(ARESTResponse.JSONValue, false);
            ListViewApplications.Items.BeginUpdate;
            try
              for LIdx := 0 to Pred(LItems.Count) do
              begin
                with ListViewApplications.Items.Add do
                begin
                  Caption := LItems.Model[LIdx].ID;
                  SubItems.Add(LItems.Model[LIdx].DisplayName);
                  SubItems.Add(LItems.Model[LIdx].Description);
                  LPasswordList := '';
                  LPasswordExpiryList := '';
                  for LPassword in LItems.Model[LIdx].PasswordCredentials do
                  begin
                    if not TLZString.IsEmptyString(LPasswordList) then
                      LPasswordList := LPasswordList + ',';
                    LPasswordList := LPasswordList + Format('%s %s',
                      [LPassword.DisplayName,
                      DateTimeToStr(LPassword.EndDateTime)]);
                    If (LPassword.EndDateTime < IncMonth(Today, 1)) and
                      (LPassword.EndDateTime >= IncMonth(Today, -1)) then
                    begin
                      if not TLZString.IsEmptyString(LPasswordExpiryList) then
                        LPasswordExpiryList := LPasswordExpiryList + ',';
                      LPasswordExpiryList := LPasswordExpiryList +
                        Format('%s %s', [LPassword.DisplayName,
                        DateTimeToStr(LPassword.EndDateTime)]);
                    end;
                  end;
                  SubItems.Add(LPasswordList);
                  SubItems.Add(LPasswordExpiryList);

                end;
              end;
            finally
              ListViewApplications.Items.EndUpdate;
            end;
          end
          else
          begin
            LazyLog.Error(Self, AMessage);
          end;
        end, '', TLZAsyncState.lasFalse);
    Until LNextLink = '';
  finally
    FreeAndNil(LItems);
  end;
end;

procedure TfrmMSGraphDemo.ListAuthenticationMethods;
var
  LNextLink: string;
  LUserID: string;
  LItems: TLZMSGraphUserAuthenticationMethods;
begin
  LUserID := InputBox('User ID', 'Enter user principal name', '');
  if TLZString.IsEmptyString(LUserID) then
    Exit;
  LItems := TLZMSGraphUserAuthenticationMethods.Create;
  try
    LNextLink := 'beta/users/{userid}/authentication/methods';
    LNextLink := StringReplace(LNextLink, '{userid}', LUserID,
      [rfReplaceAll, rfIgnoreCase]);
    Repeat
      Progress(Format('Gathering Applications %s', [LNextLink]), 0);
      FMSGraphAPI.Get(LNextLink,
        procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
          ARESTResponse: TRESTResponse; ACustomData: string)
        var
          LIdx: integer;
          LPassword: TLZMSGraphPasswordCredential;
          LPasswordList, LPasswordExpiryList: string;
        begin
          LNextLink := '';
          if ASuccess then
          begin
            LNextLink := GetODataNextLink(ARESTResponse.JSONValue);
            LItems.FromJSONValue(ARESTResponse.JSONValue, false);
            ListViewAuthenticationMethods.Items.BeginUpdate;
            try
              for LIdx := 0 to Pred(LItems.Count) do
              begin
                with ListViewAuthenticationMethods.Items.Add do
                begin
                  Caption := LItems.Model[LIdx].ID;
                  SubItems.Add(LItems.Model[LIdx].ResourceType);
                  SubItems.Add(LItems.Model[LIdx].Details);
                end;
              end;
            finally
              ListViewAuthenticationMethods.Items.EndUpdate;
            end;
          end
          else
          begin
            LazyLog.Error(Self, AMessage);
          end;
        end, '', TLZAsyncState.lasFalse);
    Until LNextLink = '';
  finally
    FreeAndNil(LItems);
  end;
end;

procedure TfrmMSGraphDemo.ListChildFolders(AFolders: TLZMSGraphMailFolders);
var
  LIdx: integer;
  LUserID, LFolderId, LFolderName, LNextLink: string;
begin
  LIdx := 0;
  while LIdx < AFolders.Count do
  begin
    LUserID := AFolders[LIdx].ID;
    LFolderId := AFolders[LIdx].ID;
    LFolderName := AFolders[LIdx].DisplayName;
    LNextLink := Format(API_VERSION +
      'users/%s/mailFolders/%s/childFolders/?includeHiddenFolders=true',
      [LUserID, LFolderId]);
    Repeat
      Progress(Format('Child folders for user %s, folder %s/ (%d / %d)',
        [LUserID, LFolderName, LIdx, AFolders.Count]), LIdx, AFolders.Count);
      FMSGraphAPI.Get(LNextLink,
        procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
          ARESTResponse: TRESTResponse; ACustomData: string)
        begin
          LNextLink := '';
          if ASuccess then
          begin
            AFolders.FromJSONValue(ARESTResponse.JSONValue, false);
          end
          else
          begin
            LazyLog.Error(Self, AMessage);
          end;
        end, '', TLZAsyncState.lasFalse);
    Until LNextLink = '';
    Inc(LIdx);
  end;

end;

procedure TfrmMSGraphDemo.ListDirectorySubscriptions;
var
  LNextLink: string;
begin
  LNextLink := 'beta/directory/subscriptions';
  // LNextLink := API_VERSION + 'directory/subscriptions';
  Repeat
    Progress(Format('Gathering Directory Subscription SKU''s %s',
      [LNextLink]), 0);
    FMSGraphAPI.Get(LNextLink,
      procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      var
        LValues: TJSONArray;
        LValue: TJSONValue;
        LDateTime: TDateTime;
      begin
        LNextLink := '';

        if ASuccess then
        begin

          LNextLink := GetODataNextLink(ARESTResponse.JSONValue);

          LValues := ARESTResponse.JSONValue.FindValue('value') as TJSONArray;

          for LValue in LValues do
          begin
            with ListViewDirectorySubscriptions.Items.Add do
            begin
              Caption := LValue.GetValue<string>('id');
              SubItems.Add(LValue.GetValue<string>('status'));
              SubItems.Add(IntToStr(LValue.GetValue<integer>('totalLicenses')));
              SubItems.Add(LValue.GetValue<string>('skuId'));
              SubItems.Add(LValue.GetValue<string>('skuPartNumber'));
              SubItems.Add(BoolToStr(LValue.GetValue<Boolean>
                ('isTrial'), true));
              LDateTime := LValue.GetValue<TDateTime>('createdDateTime', 0);
              if LDateTime <> 0 then
              begin
                SubItems.Add(DateTimeToStr(LDateTime));
              end
              else
              begin
                SubItems.Add('');
              end;
              LDateTime := LValue.GetValue<TDateTime>
                ('nextLifecycleDateTime', 0);

              if LDateTime <> 0 then
              begin
                SubItems.Add(DateTimeToStr(LDateTime));
              end
              else
              begin
                SubItems.Add('');
              end;

            end;
          end;
          Application.ProcessMessages;

        end
        else
        begin
          LazyLog.Error(Self, AMessage);
        end;
      end, '', TLZAsyncState.lasFalse);
  Until LNextLink = '';
end;

procedure TfrmMSGraphDemo.ListSignIns;
var
  LNextLink: string;
  LSignIns: TLZMSGraphAuditSignIns;
begin
  LSignIns := TLZMSGraphAuditSignIns.Create;
  try
    LNextLink := API_VERSION + 'auditLogs/signIns';
    Repeat
      Progress(Format('Gathering Audit Sign In''s %s', [LNextLink]), 0);
      FMSGraphAPI.Get(LNextLink,
        procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
          ARESTResponse: TRESTResponse; ACustomData: string)
        var
          LIdx: integer;
        begin
          LNextLink := '';
          if ASuccess then
          begin
            LNextLink := GetODataNextLink(ARESTResponse.JSONValue);
            LSignIns.FromJSONValue(ARESTResponse.JSONValue, false);
            ListViewSignIns.Items.BeginUpdate;
            try
              for LIdx := 0 to Pred(LSignIns.Count) do
              begin
                with ListViewSignIns.Items.Add do
                begin
                  Caption := LSignIns.Model[LIdx].ID;
                  SubItems.Add(LSignIns.Model[LIdx].UserPrincipalName);
                  SubItems.Add(LSignIns.Model[LIdx].ClientAppUsed);
                  SubItems.Add(LSignIns.Model[LIdx].LocationCountryOrRegion);
                  SubItems.Add(LSignIns.Model[LIdx].LocationState);
                  SubItems.Add(LSignIns.Model[LIdx].LocationCity);
                  SubItems.Add(IntToStr(LSignIns.Model[LIdx].StatusCode));
                  SubItems.Add(LSignIns.Model[LIdx].StatusFailureReason);
                  SubItems.Add(LSignIns.Model[LIdx].StatusAdditionalDetails);
                  SubItems.Add(LSignIns.Model[LIdx].ResourceDisplayName);
                  SubItems.Add(LSignIns.Model[LIdx].IPAddress);
                  SubItems.Add(BoolToStr(LSignIns.Model[LIdx]
                    .IsInteractive, true));
                  SubItems.Add(LSignIns.Model[LIdx].RiskDetail);
                  SubItems.Add(LSignIns.Model[LIdx].RiskState);
                  SubItems.Add(LSignIns.Model[LIdx].UserID);
                  SubItems.Add(LSignIns.Model[LIdx].DeviceID);
                  SubItems.Add(LSignIns.Model[LIdx].DeviceDisplayName);
                  SubItems.Add(LSignIns.Model[LIdx].DeviceOperatingSystem);
                  SubItems.Add(LSignIns.Model[LIdx].DeviceBrowser);
                  SubItems.Add(LSignIns.Model[LIdx].DeviceTrustType);
                  SubItems.Add(DateTimeToStr(LSignIns.Model[LIdx]
                    .CreatedDateTime));
                end;
              end;
            finally
              ListViewSignIns.Items.EndUpdate;
            end;
          end
          else
          begin
            LazyLog.Error(Self, AMessage);
          end;
        end, '', TLZAsyncState.lasFalse);
    Until LNextLink = '';
  finally
    FreeAndNil(LSignIns);
  end;
end;

procedure TfrmMSGraphDemo.ListFolders(
  AUser: TLZMSGraphUser;
  AFolders: TLZMSGraphMailFolders);
var
  LIdx: integer;
  LUserID, LDisplayName, LNextLink: string;
begin
  AFolders.Clear;
  LUserID := AUser.ID;
  LDisplayName := AUser.UserPrincipalName;
  LNextLink := Format(API_VERSION +
    'users/%s/mailFolders/?includeHiddenFolders=true', [LUserID]);
  Repeat
    FMSGraphAPI.Get(LNextLink,
      procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      begin
        LNextLink := '';
        if ASuccess then
        begin
          LNextLink := GetODataNextLink(ARESTResponse.JSONValue);
          AFolders.FromJSONValue(ARESTResponse.JSONValue, false);
        end
        else
        begin
          LazyLog.Error(Self, AMessage);
        end;
      end, '', TLZAsyncState.lasFalse);
  Until LNextLink = '';

end;

procedure TfrmMSGraphDemo.ListUserLicenses(AUsers: TLZMSGraphUsers);
var
  LIdx: integer;
  LUserID, LNextLink: string;
begin
  for LIdx := 0 to Pred(AUsers.Count) do
  begin
    LUserID := AUsers[LIdx].ID;
    LNextLink := Format(API_VERSION + 'users/%s/licenseDetails', [LUserID]);
    Repeat
      Progress(Format('Licenses for user %s', [AUsers[LIdx].UserPrincipalName]),
        LIdx, AUsers.Count);
      FMSGraphAPI.Get(LNextLink,
        procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
          ARESTResponse: TRESTResponse; ACustomData: string)
        var
          LValues: TJSONArray;
          LValue: TJSONValue;
          LID, LName: string;
        begin
          LNextLink := '';
          if ASuccess then
          begin
            LNextLink := GetODataNextLink(ARESTResponse.JSONValue);
            LValues := ARESTResponse.JSONValue.FindValue('value') as TJSONArray;
            for LValue in LValues do
            begin
              LID := LValue.GetValue<string>('id');
              LName := LValue.GetValue<string>('skuPartNumber');
              with ListViewUserLicenses.Items.Add do
              begin
                Caption := LID;
                SubItems.Add(LUserID);
                SubItems.Add(AUsers[LIdx].DisplayName);
                SubItems.Add(AUsers[LIdx].UserPrincipalName);
                SubItems.Add(LName);
                SubItems.Add(DateTimeToStr(AUsers[LIdx].CreatedDateTime));
                SubItems.Add(AUsers[LIdx].ProxyAddresses);
              end;
            end;
          end
          else
          begin
            LazyLog.Error(Self, AMessage);
          end;
        end, '', TLZAsyncState.lasFalse);
    Until LNextLink = '';
  end;
end;

procedure TfrmMSGraphDemo.ListRules(
  AUser: TLZMSGraphUser;
  AFolders: TLZMSGraphMailFolders);
var
  LUserID, LDisplayName: string;
begin
  LUserID := AUser.ID;
  LDisplayName := AUser.UserPrincipalName;

  FMSGraphAPI.Get(Format(API_VERSION +
    'users/%s/mailFolders/inbox/messageRules', [LUserID]),
    procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    var
      LValues: TJSONArray;
      LValue: TJSONValue;
      LID, LName: string;
      LJSON: string;
      LEnabled: Boolean;
      LAction: TJSONValue;
      LForwards: TJSONArray;
      LForward: TJSONValue;
      LForwardAddress: string;
      LMoveFolder: string;
      LFolder: TLZMSGraphMailFolder;
    begin
      if ASuccess then
      begin
        LValues := ARESTResponse.JSONValue.FindValue('value') as TJSONArray;
        for LValue in LValues do
        begin
          LID := LValue.GetValue<string>('id');
          LName := LValue.GetValue<string>('displayName');
          LEnabled := LValue.GetValue<Boolean>('isEnabled');
          if LEnabled then
          begin
            LAction := LValue.FindValue('actions');
            if Assigned(LAction) then
            begin
              LJSON := LAction.Format();

              LMoveFolder := LAction.GetValue<string>('moveToFolder');
              LFolder := AFolders.Find(LMoveFolder);
              if Assigned(LFolder) then
                LMoveFolder := LFolder.DisplayName;

              LForwards := LAction.FindValue('forwardTo') as TJSONArray;
              if Assigned(LForwards) then
              begin
                LForwardAddress := '';
                for LForward in LForwards do
                begin
                  if Trim(LForwardAddress) <> '' then
                    LForwardAddress := LForwardAddress + ',';
                  LForwardAddress := LForwardAddress +
                    LForward.FindValue('emailAddress')
                    .GetValue<string>('address');
                end;
              end;

              with ListViewMessageRules.Items.Add do
              begin
                Caption := LDisplayName;
                SubItems.Add(LID);
                SubItems.Add(LName);
                SubItems.Add(LForwardAddress);
                SubItems.Add(LMoveFolder);
                Application.ProcessMessages;
              end;

              if ContainsText(LMoveFolder, 'RSS') then
              begin
                FSuspicious.Add
                  (Format('Suspicious email rule using folder "%s" for %s, Rule name: %s',
                  [LMoveFolder, LDisplayName, LName]));
              end;

              if (ContainsText(LForwardAddress, '@hotmail')) or
                (ContainsText(LForwardAddress, '@gmail')) or
                (ContainsText(LForwardAddress, '@yahoo')) or
                (ContainsText(LForwardAddress, '@proton')) then
              begin
                FSuspicious.Add
                  (Format('Suspicious email foward address "%s" for %s, Rule name: %s',
                  [LForwardAddress, LDisplayName, LName]));
              end;
            end;
          end;
        end;
      end
      else
      begin
        LazyLog.Error(Self, AMessage);
      end;
    end, '', TLZAsyncState.lasFalse);
end;

function TfrmMSGraphDemo.GetODataNextLink(AJSONValue: TJSONValue): string;
begin
  Result := FMSGraphAPI.GetODataNextLink(AJSONValue);
end;

procedure TfrmMSGraphDemo.ListUsers(AUsers: TLZMSGraphUsers);
var
  LNextLink: string;
  LSelectFields: string;
  LFilterFields: string;
begin
  AUsers.Clear;
  LSelectFields :=
    'id,displayName,givenName,userPrincipalName,createdDateTime,mail,proxyAddresses,userType,assignedLicenses';
  LFilterFields := 'accountEnabled eq true';
  LNextLink := API_VERSION + 'users?$top=999&$select=%s&$filter=%s';
  LNextLink := Format(LNextLink, [LSelectFields, LFilterFields]);
  Repeat
    Progress(Format('Gathering users %s', [LNextLink]), 0);
    FMSGraphAPI.Get(LNextLink,
      procedure(ASender: TObject; ASuccess: Boolean; AMessage: string;
        ARESTResponse: TRESTResponse; ACustomData: string)
      begin
        LNextLink := '';

        if ASuccess then
        begin

          LNextLink := GetODataNextLink(ARESTResponse.JSONValue);
          AUsers.FromJSONValue(ARESTResponse.JSONValue, false);
        end
        else
        begin
          LazyLog.Error(Self, AMessage);
        end;
      end, '', TLZAsyncState.lasFalse);
  Until LNextLink = '';
end;

procedure TfrmMSGraphDemo.OnBrowserLoginRequest(
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
      MessageDlg(LMessage, TMsgDlgType.mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TfrmMSGraphDemo.OnTokenRequestComplete(
  ASender: TObject;
  ASuccess: Boolean;
  AMessage: string;
  AToken: TLZOAuth2Token);
var
  LIdx: integer;
  LUser: TLZMSGraphUser;
  LUsers: TLZMSGraphUsers;
  LFolders: TLZMSGraphMailFolders;
  LFileName: TFileName;
begin
  LUsers := TLZMSGraphUsers.Create;
  LFolders := TLZMSGraphMailFolders.Create;
  try
    if ASuccess then
    begin
      if PageControlOutput.ActivePage = tabMessageRules then
      begin
        FSuspicious.Clear;
        ListUsers(LUsers);
        for LIdx := 0 to Pred(LUsers.Count) do
        begin
          LUser := LUsers[LIdx];
          Progress(Format('User "%s"', [LUser.UserPrincipalName]), LIdx,
            LUsers.Count);
          ListFolders(LUser, LFolders);
          ListRules(LUser, LFolders);
        end;
        if FSuspicious.Count > 0 then
        begin
          LFileName := Format(TLZFile.GetDesktopFolder +
            'o365-suspicious-%s.txt', [editTennantID.Text]);
          FSuspicious.SaveToFile(LFileName);
          LazyLog.Log(Self, LFileName);
          LazyLog.Log(Self, FSuspicious.Text);
        end;
      end;
      if PageControlOutput.ActivePage = tabDirectorySubscriptions then
      begin
        ListDirectorySubscriptions;
      end;
      if PageControlOutput.ActivePage = tabUserLicenses then
      begin
        ListUsers(LUsers);
        ListUserLicenses(LUsers);
      end;
      if PageControlOutput.ActivePage = tabSignIns then
      begin
        ListSignIns;
      end;
      if PageControlOutput.ActivePage = tabApplications then
      begin
        ListApplications;
      end;
      if PageControlOutput.ActivePage = tabAuthenticationMethods then
      begin
        ListAuthenticationMethods;
      end;
      MessageDlg('Complete', TMsgDlgType.mtInformation, [mbOk], 0);
    end
    else
    begin
      MessageDlg(AMessage, TMsgDlgType.mtError, [mbOk], 0);
    end;
  finally
    FreeAndNil(LUsers);
    FreeAndNil(LFolders);
  end;
end;

end.
