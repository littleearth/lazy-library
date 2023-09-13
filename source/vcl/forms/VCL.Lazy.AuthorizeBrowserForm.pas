unit VCL.Lazy.AuthorizeBrowserForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.OleCtrls, SHDocVw, Lazy.Types,
  Lazy.REST.Types;

type
  TLZAuthorizeBrowserForm = class(TForm)
    WebBrowser: TWebBrowser;
    procedure WebBrowserNavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
  private
    FConnection: TLZOAuth2Connection;
    FToken: TLZOAuth2Token;
    FMessage: string;
  public
    function GetAuthToken(var AMessage: string; AURL: string;
      AConnection: TLZOAuth2Connection; AToken: TLZOAuth2Token): Boolean;
  end;

implementation

uses
  System.NetEncoding, System.Net.URLClient, VCL.Lazy.Utils;

{$R *.dfm}

function TLZAuthorizeBrowserForm.GetAuthToken(var AMessage: string;
  AURL: string; AConnection: TLZOAuth2Connection;
  AToken: TLZOAuth2Token): Boolean;
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
    if TLZString.IsEmptyString(AMessage) then
      AMessage := 'Request failed or cancelled';

  end;
end;

procedure TLZAuthorizeBrowserForm.WebBrowserNavigateComplete2
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
