unit VCL.Lazy.AuthorizeBrowserForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.OleCtrls, SHDocVw, Lazy.Types;

type
  TLazyAuthorizeBrowserForm = class(TForm)
    WebBrowser: TWebBrowser;
    procedure WebBrowserNavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
  private
    FConnection: TLazyOAuth2Connection;
    FToken: TLazyOAuth2Token;
    FMessage: string;
  public
    function GetAuthToken(var AMessage: string; AURL: string;
      AConnection: TLazyOAuth2Connection; AToken: TLazyOAuth2Token): Boolean;
  end;

implementation

uses
  System.NetEncoding, System.Net.URLClient, Lazy.Utils;

{$R *.dfm}

function TLazyAuthorizeBrowserForm.GetAuthToken(var AMessage: string;
  AURL: string; AConnection: TLazyOAuth2Connection;
  AToken: TLazyOAuth2Token): Boolean;
begin
  Result := False;
  FMessage := '';
  FConnection := AConnection;
  FToken := AToken;

  WebBrowser.Navigate(AURL);

  if ShowModal = mrOk then
  begin
    Result := True;
  end
  else
  begin
    AMessage := FMessage;
    if IsEmptyString(AMessage) then
      AMessage := 'Request failed or cancelled';

  end;
end;

procedure TLazyAuthorizeBrowserForm.WebBrowserNavigateComplete2
  (ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
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
      if LParam.Name = 'code' then
      begin
        WebBrowser.Stop;

        FToken.AuthCode := LParam.Value;
        ModalResult := mrOk;
        Hide;
        Exit;
      end
      else if LParam.Name = 'error' then
      begin
        LError := LParam.Value;
        LErrorDescription := LURI.ParameterByName['error_description'];
        FMessage := Format('Error: %s (%s)', [LErrorDescription, LError]);

        ModalResult := mrCancel;
        Hide;
        Exit;
      end;

    end;
  end;
end;

end.
