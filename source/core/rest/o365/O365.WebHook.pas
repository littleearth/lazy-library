unit O365.WebHook;

interface

uses
  System.Classes, System.SysUtils,
  REST.Json, REST.Client, REST.Types;

type
  TLZO365WebHookMessage = class
  end;

  /// <summary> See https://docs.microsoft.com/en-us/microsoftteams/platform/webhooks-and-connectors/how-to/connectors-using
  /// for examples of how to structure the json for creating advanced formats</summary>
  TLZO365WebHookSimpleText = class(TLZO365WebHookMessage)
  private
    FText: String;
  public
    property Text: String read FText write FText;
    constructor Create(const aText: string);
  end;

type
  TLZO365WebHook = class
  private
    FClient: TRESTClient;
    FRequest: TCustomRESTRequest;
    FURL: string;
  protected
    property Client: TRESTClient read FClient;
    property Request: TCustomRESTRequest read FRequest;
  public
    constructor Create(const aURL: string = '');
    destructor Destroy; override;
    function PostMessage(const aMsg: TLZO365WebHookMessage;
      aOwnsMsg: Boolean = False): Boolean; overload;
    function PostMessage(const aMsg: string): Boolean; overload;
    property URL: string read FURL write FURL;
  end;

implementation

{ TWebHook }

constructor TLZO365WebHook.Create(const aURL: string);
begin
  inherited Create;
  FURL := aURL;

  FClient := TRESTClient.Create(nil);
  FRequest := TCustomRESTRequest.Create(nil);
  FRequest.Client := FClient;
end;

destructor TLZO365WebHook.Destroy;
begin
  FRequest.Free;
  FClient.Free;
  inherited;
end;

function TLZO365WebHook.PostMessage(const aMsg: string): Boolean;
var
  LMsg: TLZO365WebHookMessage;
begin
  LMsg := TLZO365WebHookSimpleText.Create(aMsg);
  Result := PostMessage(LMsg, True);
end;

function TLZO365WebHook.PostMessage(const aMsg: TLZO365WebHookMessage;
  aOwnsMsg: Boolean = False): Boolean;
begin
  try
    Request.Client.BaseURL := URL;
    Request.Method := rmPOST;
    Request.AddBody(aMsg);
    Request.Execute;
    Result := Request.Response.Status.Success;
  finally
    if aOwnsMsg then
      aMsg.Free;
  end;
end;

{ TSimpleText }

constructor TLZO365WebHookSimpleText.Create(const aText: string);
begin
  inherited Create;
  FText := aText;
end;

end.
