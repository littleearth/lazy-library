unit VCL.Lazy.AuthorizeBrowserForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.OleCtrls, SHDocVw, Lazy.Types,
  Lazy.REST.Types, VCL.ComCtrls;

type
  TLZAuthorizeBrowserForm = class(TForm)
    WebBrowser: TWebBrowser;
    StatusBarBrowser: TStatusBar;
    procedure WebBrowserNavigateComplete2(
      ASender: TObject;
      const pDisp: IDispatch;
      const URL: OleVariant);
    procedure FormCreate(Sender: TObject);
    procedure WebBrowserTitleChange(
      ASender: TObject;
      const Text: WideString);
    procedure WebBrowserBeforeNavigate2(
      ASender: TObject;
      const pDisp: IDispatch;
      const URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
  private
    FURL: string;
    FConnection: TLZOAuth2Connection;
    FToken: TLZOAuth2Token;
    FMessage: string;
  protected
    procedure GenerateStatusHTML(
      ATitle: string;
      AMessage: string;
      ATitleColor: string = 'MediumSeaGreen';
      AMessageColor: string = 'DarkSlateGray';
      ABackgroundColor: string = 'White';
      AFontFamily: string = 'monospace'); virtual;
  public
    function GetAuthToken(
      var AMessage: string;
      AURL: string;
      AConnection: TLZOAuth2Connection;
      AToken: TLZOAuth2Token;
      AUserDataFolder: string = '';
      AUseStatusRedirect: boolean = false): boolean;
  end;

implementation

uses
  System.NetEncoding, System.Net.URLClient, VCL.Lazy.Utils.Windows,
  System.IOUtils, FileCtrl, Lazy.Log;

{$R *.dfm}

procedure TLZAuthorizeBrowserForm.GenerateStatusHTML(ATitle, AMessage,
  ATitleColor, AMessageColor, ABackgroundColor, AFontFamily: string);
var
  LHTML: TStringList;
  LFileName: string;
begin
  LHTML := TStringList.Create;
  try
    LHTML.Add('<!DOCTYPE html>');
    LHTML.Add('<html>');
    LHTML.Add('<body style="background-color:%backgroundcolor%">');
    LHTML.Add(
      '<h1 style="color:%titlecolor%;text-align:center;font-family: %fontfamily%";>%title%</h1>');
    LHTML.Add(
      '<p style="color:%messagecolor%;text-align:center;font-family: %fontfamily%">%message%.</p>');
    LHTML.Add('</body>');
    LHTML.Add('</html>');
    LHTML.Text := StringReplace(LHTML.Text, '%backgroundcolor%',
      ABackgroundColor, [rfReplaceAll, rfIgnoreCase]);
    LHTML.Text := StringReplace(LHTML.Text, '%titlecolor%', ATitleColor,
      [rfReplaceAll, rfIgnoreCase]);
    LHTML.Text := StringReplace(LHTML.Text, '%messagecolor%', AMessageColor,
      [rfReplaceAll, rfIgnoreCase]);
    LHTML.Text := StringReplace(LHTML.Text, '%title%', ATitle,
      [rfReplaceAll, rfIgnoreCase]);
    LHTML.Text := StringReplace(LHTML.Text, '%message%', AMessage,
      [rfReplaceAll, rfIgnoreCase]);
    LHTML.Text := StringReplace(LHTML.Text, '%fontfamily%', AFontFamily,
      [rfReplaceAll, rfIgnoreCase]);
    if TLZFile.CheckDirectoryExists(WebBrowser.EdgeUserDataFolder, true) then
    begin
      LFileName := TLZFile.GetTempFile('web', '.html',
        WebBrowser.EdgeUserDataFolder);
      LHTML.SaveToFile(LFileName);
      WebBrowser.Navigate(LFileName);
    end;
  finally
    FreeAndNil(LHTML);
  end;
end;

procedure TLZAuthorizeBrowserForm.FormCreate(Sender: TObject);
begin
  WebBrowser.EdgeUserDataFolder := TLZFile.GetApplicationTempFolder
    (false, false);
  Self.Tag := 0;
end;

function TLZAuthorizeBrowserForm.GetAuthToken(
  var AMessage: string;
  AURL: string;
  AConnection: TLZOAuth2Connection;
  AToken: TLZOAuth2Token;
  AUserDataFolder: string;
  AUseStatusRedirect: boolean): boolean;
begin
  Result := false;
  FMessage := '';
  FConnection := AConnection;
  FToken := AToken;
  FURL := AURL;

  if not TLZString.IsEmptyString(AUserDataFolder) then
  begin
    WebBrowser.EdgeUserDataFolder := AUserDataFolder;
  end;

  StatusBarBrowser.Panels[1].Text := MinimizeName(WebBrowser.EdgeUserDataFolder,
    StatusBarBrowser.Canvas, StatusBarBrowser.Panels[1].Width);

  if AUseStatusRedirect then
  begin
    GenerateStatusHTML('Please wait...', 'Redirecting to <a href="' + AURL +
      '">authentication</a> site');
  end
  else
  begin
    WebBrowser.Navigate(FURL);
  end;

  if ShowModal = mrOk then
  begin
    Result := true;
  end
  else
  begin
    AMessage := FMessage;
    if TLZString.IsEmptyString(AMessage) then
      AMessage := 'Request failed or cancelled';

  end;
end;

procedure TLZAuthorizeBrowserForm.WebBrowserBeforeNavigate2(
  ASender: TObject;
  const pDisp: IDispatch;
  const URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
  var Cancel: WordBool);
begin
  StatusBarBrowser.Panels[2].Text := URL;
end;

procedure TLZAuthorizeBrowserForm.WebBrowserNavigateComplete2(
  ASender: TObject;
  const pDisp: IDispatch;
  const URL: OleVariant);
var
  LURI: TURI;
  LParam: TNameValuePair;
  LError: string;
  LErrorDescription: string;
  LURL: string;
begin
  LURL := URL;
  if LURL.StartsWith(FConnection.RedirectURL) then
  begin
    LURI := TURI.Create(LURL);

    for LParam in LURI.Params do
    begin
      LazyLog.Log(Self, Format('%s = %s', [LParam.Name, LParam.Value]));
      if LParam.Name = 'code' then
      begin
        WebBrowser.Stop;
        FToken.AuthToken := LParam.Value;
        ModalResult := mrOk;
        Hide;
        Exit;
      end
      else if LParam.Name = 'error' then
      begin
        LError := LParam.Value;
        try
          LErrorDescription := LURI.ParameterByName['error_description'];
        except
          LErrorDescription := '';
        end;
        FMessage := Format('Error: %s (%s)', [LErrorDescription, LError]);
        ModalResult := mrCancel;
        Hide;
        Exit;
      end;

    end;
  end;
  if LURL.StartsWith('file:///') then
  begin
    if Self.Tag = 0 then
    begin
      Self.Tag := 1;
      WebBrowser.Navigate(FURL);
    end;
  end;
end;

procedure TLZAuthorizeBrowserForm.WebBrowserTitleChange(
  ASender: TObject;
  const Text: WideString);
begin
  StatusBarBrowser.Panels[0].Text := Text;
  Self.Caption := Text;
end;

end.
