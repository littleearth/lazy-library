unit duo.api;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  REST.Authenticator.Basic, System.JSON, System.Generics.Collections;

const
  DUO_Line_Seperator = #10;

type

  TDuoLogLevel = (logDebug, logInfo, logError);
  TDuoLogMessage = procedure(ASender: TObject; AMessage: string;
    ALogLevel: TDuoLogLevel) of object;
  TOnDuoNotifyEvent = procedure(ASender: TObject; AMessage: string;
    ASuccess: boolean) of object;
  TOnDuoNotifyProc = reference to procedure(AMessage: string;
    ASuccess: boolean);

  TDuoBase = class(TObject)
  private
    FOnLog: TDuoLogMessage;
    procedure SetOnLog(const Value: TDuoLogMessage);
  public
    procedure Log(AMessage: string);
    procedure Debug(AProcedure: string; AMessage: string);
    procedure Error(AMessage: string); overload;
    procedure Error(E: Exception; AMessage: string = ''); overload;
    property OnLog: TDuoLogMessage read FOnLog write SetOnLog;
  end;

  TDuoBaseComponent = class(TComponent)
  private
    FOnLog: TDuoLogMessage;
    procedure SetOnLog(const Value: TDuoLogMessage);
  public
    procedure Log(AMessage: string);
    procedure Debug(AProcedure: string; AMessage: string);
    procedure Error(AMessage: string); overload;
    procedure Error(E: Exception; AMessage: string = ''); overload;
    property OnLog: TDuoLogMessage read FOnLog write SetOnLog;
  end;

  TDuoModel = class(TDuoBase)
  public
    constructor CreateClone(ASource: TDuoModel); virtual;
    class function CreateModel<T: TDuoModel>: T;
    procedure Assign(ASource: TDuoModel); virtual; abstract;
    procedure FromJSONValue(AJSONValue: TJSONValue); virtual; abstract;
  end;

  TDuoModelList<T: TDuoModel> = class(TObject)
  private
    FItems: TObjectList<T>;
    function GetCount: integer;
    function GetItems: TObjectList<T>;
  protected
  public
    constructor Create; reintroduce;
    constructor CreateClone(ASource: TDuoModelList<T>); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TDuoModelList<T>); virtual;
    function Add(ASource: T): T; virtual;
    procedure Clear;
    procedure FromJSONValue(AJSONValue: TJSONValue; AClear: boolean = true;
      AArrayName: string = 'response'); virtual;
    property Count: integer read GetCount;
    property Items: TObjectList<T> read GetItems;
  end;

  TBeforeDUORequestProc = reference to procedure(ARESTRequest: TRESTRequest);
  TAfterDUORequestProc = reference to procedure(ARESTResponse: TRESTResponse;
    var ASuccess: boolean; var AMessage: string);

  TDuoNetworkTools = class(TDuoBase)
  public
    class function GetHostname: string;
    class function GetIPAddress: string;
    class function GetIPAddresses: string;
  end;

  TDuoDateTimeHelpers = class(TDuoBase)
  public
    class function UnixTimestampToDateTime(AValue: string;
      ADefault: TDateTime = 0; AReturnUTC: boolean = true): TDateTime;
  end;

  TDuoAPIBase = class(TDuoBaseComponent)
  private
    FIntegrationKey: string;
    FSecretKey: string;
    FHostname: string;
    procedure SetHostname(const Value: string);
    procedure SetIntegrationKey(const Value: string);
    procedure SetSecretKey(const Value: string);
  protected
    function ExecuteRequest(var AMessage: string; AResource: string;
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
  System.DateUtils, Rtti, TypInfo,
  System.Hash, System.Net.URLClient, System.NetEncoding;

{ TDuoAPIBase }

function TDuoAPIBase.IsEmptyString(AValue: string): boolean;
begin
  Result := Trim(AValue) = '';
end;

function TDuoAPIBase.ExecuteRequest(var AMessage: string; AResource: string;
  AMethod: TRESTRequestMethod; ABeforeDUORequestProc: TBeforeDUORequestProc;
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

function TDuoAPIBase.GetBaseResource: string;
begin
  Result := '';
end;

function TDuoAPIBase.GetHMAC(ARequest: string): string;
begin
  Result := THashSHA2.GetHMAC(ARequest, FSecretKey, SHA512);
end;

procedure TDuoAPIBase.SetHostname(const Value: string);
begin
  FHostname := Value;
end;

procedure TDuoAPIBase.SetIntegrationKey(const Value: string);
begin
  FIntegrationKey := Value;
end;

procedure TDuoAPIBase.SetSecretKey(const Value: string);
begin
  FSecretKey := Value;
end;

function TDuoAPIBase.EncodeURL(AValue: string): string;
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

{ TDuoBase }

procedure TDuoBase.Debug(AProcedure, AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('[%s] %s', [AProcedure, AMessage]), logDebug);
end;

procedure TDuoBase.Error(AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('%s', [AMessage]), logError);
end;

procedure TDuoBase.Error(E: Exception; AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('Exception: %s , Message: %s', [E.Message, AMessage]),
      logError);
end;

procedure TDuoBase.Log(AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('%s', [AMessage]), logInfo);
end;

procedure TDuoBase.SetOnLog(const Value: TDuoLogMessage);
begin
  FOnLog := Value;
end;

{ TDuoNetworkTools }

class function TDuoNetworkTools.GetHostname: string;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.Hostname;
  finally
    TIdStack.DecUsage;
  end;
end;

class function TDuoNetworkTools.GetIPAddress: string;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.LocalAddress;
  finally
    TIdStack.DecUsage;
  end;
end;

class function TDuoNetworkTools.GetIPAddresses: string;
begin
  TIdStack.IncUsage;
  try
    Result := GStack.LocalAddresses.DelimitedText;
  finally
    TIdStack.DecUsage;
  end;
end;

{ TDuoDateTimeHelpers }

class function TDuoDateTimeHelpers.UnixTimestampToDateTime(AValue: string;
  ADefault: TDateTime; AReturnUTC: boolean): TDateTime;
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

{ TDuoModelList<T> }

function TDuoModelList<T>.Add(ASource: T): T;
begin
  FItems.Add(ASource);
end;

procedure TDuoModelList<T>.Assign(ASource: TDuoModelList<T>);
var
  LSourceDevice, LDestDevice: T;
begin
  Clear;
  for LSourceDevice in ASource.Items do
  begin
    LDestDevice := T.CreateClone(LSourceDevice);
    FItems.Add(LDestDevice);
  end;
end;

procedure TDuoModelList<T>.Clear;
begin
  FItems.Clear;
end;

constructor TDuoModelList<T>.Create;
begin
  FItems := TObjectList<T>.Create;
end;

constructor TDuoModelList<T>.CreateClone(ASource: TDuoModelList<T>);
begin
  inherited Create;
  Assign(ASource);
end;

destructor TDuoModelList<T>.Destroy;
begin
  try
    FreeAndNil(FItems);
  finally
    inherited;
  end;
end;

procedure TDuoModelList<T>.FromJSONValue(AJSONValue: TJSONValue;
  AClear: boolean; AArrayName: string);
var
  LArray: TJSONArray;
  LIdx: integer;
  LModel: T;
begin
  if AClear then
    Clear;
  LArray := AJSONValue.GetValue<TJSONArray>(AArrayName, nil);
  if Assigned(LArray) then
  begin
    for LIdx := 0 to Pred(LArray.Count) do
    begin
      LModel := TDuoModel.CreateModel<T>;
      LModel.FromJSONValue(LArray.Items[LIdx]);
      Add(LModel);
    end;
  end;
end;

function TDuoModelList<T>.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TDuoModelList<T>.GetItems: TObjectList<T>;
begin
  Result := FItems;
end;

{ TDuoModel }

constructor TDuoModel.CreateClone(ASource: TDuoModel);
begin
  inherited Create;
  Assign(ASource);
end;

class function TDuoModel.CreateModel<T>: T;
var
  LRttiContext: TRTTIContext;
  AValue: TValue;
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  Result := nil;
  rType := LRttiContext.GetType(TypeInfo(T));
  AMethCreate := rType.GetMethod('Create');

  if Assigned(AMethCreate) and rType.IsInstance then
  begin
    instanceType := rType.AsInstance;
    AValue := AMethCreate.Invoke(instanceType.MetaclassType, []);
    Result := AValue.AsType<T>;
  end;
end;

{ TDuoBaseComponent }

procedure TDuoBaseComponent.Debug(AProcedure, AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('[%s] %s', [AProcedure, AMessage]), logDebug);
end;

procedure TDuoBaseComponent.Error(AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('%s', [AMessage]), logError);
end;

procedure TDuoBaseComponent.Error(E: Exception; AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('Exception: %s , Message: %s', [E.Message, AMessage]),
      logError);
end;

procedure TDuoBaseComponent.Log(AMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Format('%s', [AMessage]), logInfo);
end;

procedure TDuoBaseComponent.SetOnLog(const Value: TDuoLogMessage);
begin
  FOnLog := Value;
end;

end.
