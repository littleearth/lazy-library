unit DUO.API;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  REST.Authenticator.Basic, System.JSON, System.Generics.Collections,
  Lazy.Types, Lazy.Log, Lazy.Model;

const
  DUO_Line_Seperator = #10;

type

  TOnDuoNotifyEvent = procedure(
    ASender: TObject;
    AMessage: string;
    ASuccess: boolean) of object;
  TOnDuoNotifyProc = reference to procedure(AMessage: string;
    ASuccess: boolean);

  TLZDuoBase = class(TLZObject);

  TLZDuoBaseComponent = class(TLZComponent);

  TLZDuoModel = class(TLZModel);

  TLZDuoModelList<T: TLZDuoModel> = class(TLZModelList<T>)
  protected
    function GetDefaultArrayName: string; override;
  end;

  TBeforeDUORequestProc = reference to procedure(ARESTRequest: TRESTRequest);
  TAfterDUORequestProc = reference to procedure(ARESTResponse: TRESTResponse;
    var ASuccess: boolean; var AMessage: string);

  TLZDuoNetworkTools = class(TLZDuoBase)
  public
    class function GetHostname: string;
    class function GetIPAddress: string;
    class function GetIPAddresses: string;
  end;

  TLZDuoDateTimeHelpers = class(TLZDuoBase)
  public
    class function UnixTimestampToDateTime(
      AValue: string;
      ADefault: TDateTime = 0;
      AReturnUTC: boolean = true): TDateTime;
    class function DateTimeToUnixTimestamp(AValue: TDateTime): string;
  end;

  TLZDuoAPIBase = class(TLZDuoBaseComponent)
  private
    FIntegrationKey: string;
    FSecretKey: string;
    FHostname: string;
    procedure SetHostname(const Value: string);
    procedure SetIntegrationKey(const Value: string);
    procedure SetSecretKey(const Value: string);
  protected
    function ExecuteRequest(
      var AMessage: string;
      AResource: string;
      AMethod: TRESTRequestMethod = rmGET;
      ABeforeDUORequestProc: TBeforeDUORequestProc = nil;
      AAfterDUORequestProc: TAfterDUORequestProc = nil): boolean; virtual;
    function GetBaseResource: string; virtual;
    function IsEmptyString(AValue: string): boolean;
  public
    function EncodeURL(AValue: string): string;
    function GetHMAC(ARequest: string): string;
  published
    property BaseResource: string read GetBaseResource;
    property Hostname: string read FHostname write SetHostname;
    property IntegrationKey: string read FIntegrationKey
      write SetIntegrationKey;
    property SecretKey: string read FSecretKey write SetSecretKey;
  end;

implementation

uses
  IdGlobal, IdHMAC, IdSSLOpenSSL, IdCoderMIME, IdGlobalProtocols, IdStack,
  System.DateUtils, Rtti, TypInfo, Lazy.NetworkTools,
  System.Hash, System.NetEncoding;

{ TDuoAPIBase }

function TLZDuoAPIBase.IsEmptyString(AValue: string): boolean;
begin
  Result := Trim(AValue) = '';
end;

function TLZDuoAPIBase.ExecuteRequest(
  var AMessage: string;
  AResource: string;
  AMethod: TRESTRequestMethod;
  ABeforeDUORequestProc: TBeforeDUORequestProc;
  AAfterDUORequestProc: TAfterDUORequestProc): boolean;
var
  LHTTPBasicAuthenticator: THTTPBasicAuthenticator;
  LRESTResponse: TRESTResponse;
  LRESTRequest: TRESTRequest;
  LRESTClient: TRESTClient;
  LAuthorizationRequest, LAuthorizationRequestParams: string;
  LDate, LHMAC, LMethod: string;
  LParam: TRESTRequestParameter;
begin
  Result := False;
  LHTTPBasicAuthenticator := THTTPBasicAuthenticator.Create(nil);
  LRESTResponse := TRESTResponse.Create(nil);
  LRESTRequest := TRESTRequest.Create(nil);
  LRESTClient := TRESTClient.Create(nil);
  try
    LMethod := 'GET';
    case AMethod of
      rmPOST:
        LMethod := 'POST';
      rmPUT:
        LMethod := 'PUT';
      rmDELETE:
        LMethod := 'DELETE';
      rmPATCH:
        LMethod := 'PATCH';
    end;

    LRESTRequest.client := LRESTClient;

    LRESTRequest.Method := AMethod;
    LRESTRequest.Resource := AResource;
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.SynchronizedEvents := False;
    LRESTClient.ConnectTimeout := CRestDefaultTimeout * 2;
    LRESTClient.ReadTimeout := CRestDefaultTimeout * 2;
    LRESTClient.Authenticator := LHTTPBasicAuthenticator;
    LRESTClient.BaseURL := 'https://' + FHostname;

    LDate := formatdatetime('ddd, d mmm yyyy hh:mm:ss', LocalTimeToUTCTime(Now))
      + ' -0000';

    Debug('ExecuteRequest', 'Date: ' + LDate);

    if Assigned(ABeforeDUORequestProc) then
    begin
      ABeforeDUORequestProc(LRESTRequest);
    end;

    LAuthorizationRequest := LDate + DUO_Line_Seperator + LMethod +
      DUO_Line_Seperator + FHostname + DUO_Line_Seperator + AResource;

    LAuthorizationRequestParams := '';
    For LParam in LRESTRequest.Params do
    begin
      if LParam.Kind = pkGETorPOST then
      begin
        if Trim(LAuthorizationRequestParams) <> '' then
          LAuthorizationRequestParams := LAuthorizationRequestParams + '&';
        LAuthorizationRequestParams := LAuthorizationRequestParams + LParam.Name
          + '=' + LParam.Value;
      end;
    end;
    LAuthorizationRequest := LAuthorizationRequest + DUO_Line_Seperator +
      LAuthorizationRequestParams;

    Debug('ExecuteRequest', 'AuthorizationRequest: ' + LAuthorizationRequest);

    LHMAC := GetHMAC(LAuthorizationRequest);

    Debug('ExecuteRequest', 'HMAC: ' + LHMAC);

    LRESTRequest.AddParameter('Date', LDate, pkHTTPHEADER, [poDoNotEncode]);

    LHTTPBasicAuthenticator.Username := FIntegrationKey;
    LHTTPBasicAuthenticator.Password := LHMAC;

    Debug('ExecuteRequest', 'Request: URL,' + LRESTRequest.GetFullRequestURL +
      ',Body: ' + LRESTRequest.GetFullRequestBody);

    try
      LRESTRequest.Execute;

      Result := LRESTResponse.StatusCode < 400;
      AMessage := LRESTResponse.StatusText;

      Debug('ExecuteRequest',
        Format('Response: (%d) %s, Content: %s, Content Type: %s, Content Encoding: %s ',
        [LRESTResponse.StatusCode, LRESTResponse.StatusText,
        LRESTResponse.Content, LRESTResponse.ContentType,
        LRESTResponse.ContentEncoding]));

      if Assigned(AAfterDUORequestProc) then
      begin
        AAfterDUORequestProc(LRESTResponse, Result, AMessage);
      end;
    except
      on E: Exception do
      begin
        Error(E);
        AMessage := E.Message;
        Result := False;
      end;
    end;

    Debug('ExecuteRequest', 'Result: ' + BoolToStr(Result, true));

  finally
    FreeAndNil(LHTTPBasicAuthenticator);
    FreeAndNil(LRESTResponse);
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTClient);
  end;
end;

function TLZDuoAPIBase.GetBaseResource: string;
begin
  Result := '';
end;

function TLZDuoAPIBase.GetHMAC(ARequest: string): string;
begin
  Result := THashSHA2.GetHMAC(ARequest, FSecretKey, SHA512);
end;

procedure TLZDuoAPIBase.SetHostname(const Value: string);
begin
  FHostname := Value;
end;

procedure TLZDuoAPIBase.SetIntegrationKey(const Value: string);
begin
  FIntegrationKey := Value;
end;

procedure TLZDuoAPIBase.SetSecretKey(const Value: string);
begin
  FSecretKey := Value;
end;

function TLZDuoAPIBase.EncodeURL(AValue: string): string;
const
  FormUnsafeChars: TURLEncoding.TUnsafeChars = [Ord('!'), Ord('"'), Ord('#'),
    Ord('$'), Ord('%'), Ord('&'), Ord(''''), Ord('('), Ord(')'), Ord('*'),
    Ord('+'), Ord(','), Ord('"'), Ord('/'), Ord(':'), Ord(';'), Ord('<'),
    Ord('>'), Ord('='), Ord('?'), Ord('@'), Ord('['), Ord(']'), Ord('\'),
    Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|')];
var
  LEncoding: TURLEncoding;
  LOptions: TURLEncoding.TEncodeOptions;
begin
  LEncoding := TURLEncoding.Create;
  try
    LOptions := [TURLEncoding.TEncodeOption.EncodePercent];
    Result := LEncoding.Encode(AValue, FormUnsafeChars, LOptions);
  finally
    LEncoding.Free;
  end;
  Debug('EncludeURL', Format('%s = %s', [AValue, Result]));
end;

{ TDuoNetworkTools }

class function TLZDuoNetworkTools.GetHostname: string;
begin
  Result := TLZNetworkTools.GetHostname;
end;

class function TLZDuoNetworkTools.GetIPAddress: string;
begin
  Result := TLZNetworkTools.GetIPAddress;
end;

class function TLZDuoNetworkTools.GetIPAddresses: string;
begin
  Result := TLZNetworkTools.GetIPAddresses;
end;

{ TDuoDateTimeHelpers }

class function TLZDuoDateTimeHelpers.UnixTimestampToDateTime(
  AValue: string;
  ADefault: TDateTime;
  AReturnUTC: boolean): TDateTime;
var
  LDateTimeUnix: Int64;
begin
  try
    LDateTimeUnix := StrToInt64(AValue);
    Result := UnixToDateTime(LDateTimeUnix, AReturnUTC);
  except
    Result := ADefault;
  end;
end;

class function TLZDuoDateTimeHelpers.DateTimeToUnixTimestamp(AValue: TDateTime): string;
var
  LUnixTime: Int64;
begin
  LUnixTime := DateTimeToUnix(AValue, true);
  Result := IntToStr(LUnixTime);
end;

{ TLZDuoModelList<T> }

function TLZDuoModelList<T>.GetDefaultArrayName: string;
begin
  Result := 'response';
end;

end.
