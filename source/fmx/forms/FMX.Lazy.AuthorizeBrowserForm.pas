unit FMX.Lazy.AuthorizeBrowserForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  Winapi.Windows,
  Lazy.Types;

type
  TLazyAuthorizeBrowserForm = class(TForm)
    WebBrowser: TWebBrowser;
    procedure WebBrowserDidFinishLoad(ASender: TObject);
  private
    FConnection: TLazyOAuth2Connection;
    FToken: TLazyOAuth2Token;
    FMessage: string;
  public
    function GetAuthToken(var AMessage: string; AURL: string;
      AConnection: TLazyOAuth2Connection; AToken: TLazyOAuth2Token): Boolean;
  end;

implementation

{$R *.fmx}

uses
  System.NetEncoding, System.Net.URLClient, Lazy.Utils;

{ TAuthorizeForm }

function TLazyAuthorizeBrowserForm.GetAuthToken(var AMessage: string;
  AURL: string; AConnection: TLazyOAuth2Connection;
  AToken: TLazyOAuth2Token): Boolean;
begin
  Result := False;
  FMessage := '';
  FConnection := AConnection;
  FToken := AToken;

  WebBrowser.URL := AURL;

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

procedure TLazyAuthorizeBrowserForm.WebBrowserDidFinishLoad(ASender: TObject);
var
  LURI: TURI;
  LParam: TNameValuePair;
  LError: string;
  LErrorDescription: string;
begin
  if WebBrowser.URL.StartsWith(FConnection.RedirectURL) then
  begin
    LURI := TURI.Create(WebBrowser.URL);

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
