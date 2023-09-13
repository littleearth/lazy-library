unit duo.auth;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  REST.Authenticator.Basic, System.JSON, duo.api, System.Generics.Collections,
  duo.models;

type

  TDuoLoginResponse = (loginFail, loginSuccess, loginSMSCodes);

  TOnDuoPreAuthEvent = procedure(ASender: TObject; ADevices: TLZDUODevices;
    AMessage: string; ASuccess: boolean; var AOwnsObjects: boolean) of object;
  TOnDuoPreAuthProc = reference to procedure(ADevices: TLZDUODevices;
    AMessage: string; ASuccess: boolean; var AOwnsObjects: boolean);

  TOnDuoLoginEvent = procedure(ASender: TObject; AMessage: string;
    AResponse: TDuoLoginResponse) of object;
  TOnDuoLoginProc = reference to procedure(AMessage: string;
    AResponse: TDuoLoginResponse);

  TLZDUOAuthAPI = class(TLZDuoAPIBase)
  private
    FOnLogin: TOnDuoLoginEvent;
    FOnCheck: TOnDuoNotifyEvent;
    FOnPreAuth: TOnDuoPreAuthEvent;
    FOnPing: TOnDuoNotifyEvent;
    procedure SetOnCheck(const Value: TOnDuoNotifyEvent);
    procedure SetOnLogin(const Value: TOnDuoLoginEvent);
    procedure SetOnPreAuth(const Value: TOnDuoPreAuthEvent);
    procedure SetOnPing(const Value: TOnDuoNotifyEvent);
  protected
    function GetBaseResource: string; override;
  public
    procedure Logo(AFileName: TFilename; var AMessage: string;
      var ASuccess: boolean);
    procedure Login(AUsername: string; ADisplayName: string = '';
      APushInfo: string = ''; AType: string = 'Login request';
      ADevice: string = 'auto'; AFactor: string = 'auto';
      APasscode: string = ''; AHostname: string = ''; AIPAddr: string = '';
      AUserID: boolean = false; AAsync: boolean = false;
      AOnDuoLoginProc: TOnDuoLoginProc = nil);
    procedure PreAuth(AUsername: string; AHostname: string = '';
      AIPAddr: string = ''; ATrustedDeviceToken: string = '';
      AUserID: boolean = false; AOnDuoPreAuthProc: TOnDuoPreAuthProc = nil);
    procedure Check(AOnDuoNotifyProc: TOnDuoNotifyProc = nil);
    procedure Ping(AOnDuoNotifyProc: TOnDuoNotifyProc = nil);
  published
    property OnPing: TOnDuoNotifyEvent read FOnPing write SetOnPing;
    property OnLogin: TOnDuoLoginEvent read FOnLogin write SetOnLogin;
    property OnCheck: TOnDuoNotifyEvent read FOnCheck write SetOnCheck;
    property OnPreAuth: TOnDuoPreAuthEvent read FOnPreAuth write SetOnPreAuth;
  end;

implementation

{ TDUOAuth }

function TLZDUOAuthAPI.GetBaseResource: string;
begin
  Result := '/auth/v2/';
end;

procedure TLZDUOAuthAPI.Check(AOnDuoNotifyProc: TOnDuoNotifyProc);
var
  LResult: boolean;
  LMessage: string;
begin
  LResult := ExecuteRequest(LMessage, BaseResource + 'check', rmGET, nil,
    procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
      var AMessage: string)
    var
      LStat: string;
    begin
      if ASuccess then
      begin
        try
          LStat := ARESTResponse.JSONValue.GetValue<string>('stat', '');
          if not SameText(LStat, 'OK') then
          begin
            AMessage := LStat;
            ASuccess := false;
          end;
        except
          on E: Exception do
          begin
            Error(E);
            ASuccess := false;
            AMessage := E.Message;
          end;
        end;
      end;
    end);
  if Assigned(AOnDuoNotifyProc) then
    AOnDuoNotifyProc(LMessage, LResult);
  if Assigned(FOnCheck) then
    FOnCheck(Self, LMessage, LResult);
end;

procedure TLZDUOAuthAPI.Login(AUsername, ADisplayName, APushInfo, AType, ADevice,
  AFactor, APasscode, AHostname, AIPAddr: string; AUserID, AAsync: boolean;
AOnDuoLoginProc: TOnDuoLoginProc);
var
  LLoginResponse: TDuoLoginResponse;
  LMessage: string;
begin
  LLoginResponse := loginFail;
  ExecuteRequest(LMessage, BaseResource + 'auth', rmPOST,
    procedure(ARESTRequest: TRESTRequest)
    begin
      // keep parameters alphabetical
      if AAsync then
      begin
        ARESTRequest.AddParameter('async', '1',
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if ADevice <> '' then
      begin
        ARESTRequest.AddParameter('device', ADevice,
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if ADisplayName <> '' then
      begin
        ARESTRequest.AddParameter('display_username', EncodeURL(ADisplayName),
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if AFactor <> '' then
      begin
        ARESTRequest.AddParameter('factor', AFactor,
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if AHostname <> '' then
      begin
        ARESTRequest.AddParameter('hostname', EncodeURL(AHostname),
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if AIPAddr <> '' then
      begin
        ARESTRequest.AddParameter('ipaddr', EncodeURL(AIPAddr),
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if APasscode <> '' then
      begin
        ARESTRequest.AddParameter('passcode', EncodeURL(APasscode),
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if APushInfo <> '' then
      begin
        ARESTRequest.AddParameter('pushinfo', EncodeURL(APushInfo),
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;

      if (SameText(AFactor, 'push') or SameText(AFactor, 'auto')) then
      begin
        if AType <> '' then
        begin
          ARESTRequest.AddParameter('type', EncodeURL(AType),
            TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
        end;
      end;

      if AUserID then
      begin
        ARESTRequest.AddParameter('user_id', EncodeURL(AUsername),
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end
      else
      begin
        ARESTRequest.AddParameter('username', EncodeURL(AUsername),
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;
    end,
    procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
      var AMessage: string)
    var
      LResult, LStatusMsg: string;
    begin
      if ASuccess then
      begin
        try
          LResult := ARESTResponse.JSONValue.GetValue<string>
            ('response.result', '');
          LStatusMsg := ARESTResponse.JSONValue.GetValue<string>
            ('response.status_msg', LResult);

          if SameText(AFactor, 'sms') then
          begin
            LResult := ARESTResponse.JSONValue.GetValue<string>
              ('response.status', '');
            if SameText(LResult, 'sent') then
            begin
              LLoginResponse := loginSMSCodes;
            end
            else
            begin
              ASuccess := false;
              AMessage := LStatusMsg;
            end;
          end;

          if SameText(LResult, 'allow') then
          begin
            LLoginResponse := loginSuccess;
          end
          else
          begin
            ASuccess := false;
            AMessage := LStatusMsg;
          end;
        except
          on E: Exception do
          begin
            Error(E);
            ASuccess := false;
            AMessage := E.Message;
          end;
        end;
      end;
    end);
  if Assigned(AOnDuoLoginProc) then
    AOnDuoLoginProc(LMessage, LLoginResponse);
  if Assigned(FOnLogin) then
    FOnLogin(Self, LMessage, LLoginResponse);
end;

procedure TLZDUOAuthAPI.Logo(AFileName: TFilename; var AMessage: string;
var ASuccess: boolean);
begin
  ASuccess := ExecuteRequest(AMessage, BaseResource + 'logo', rmGET, nil,
    procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
      var AMessage: string)
    var
      LFileStream: TFileStream;
    begin
      if ASuccess then
      begin
        if FileExists(AFileName) then
          DeleteFile(AFileName);
        LFileStream := TFileStream.Create(AFileName, fmCreate);
        try
          try
            LFileStream.WriteBuffer(ARESTResponse.RawBytes,
              Length(ARESTResponse.RawBytes));
            ASuccess := FileExists(AFileName);
          except
            on E: Exception do
            begin
              AMessage := E.Message;
              ASuccess := false;
            end;
          end;
        finally
          FreeAndNil(LFileStream);
        end;
      end;
    end);
end;

procedure TLZDUOAuthAPI.Ping(AOnDuoNotifyProc: TOnDuoNotifyProc);
var
  LResult: boolean;
  LMessage: string;
begin
  LResult := ExecuteRequest(LMessage, BaseResource + 'ping', rmGET, nil,
    procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
      var AMessage: string)
    var
      LStat: string;
    begin
      if ASuccess then
      begin
        try
          LStat := ARESTResponse.JSONValue.GetValue<string>('stat', '');
          if not SameText(LStat, 'OK') then
          begin
            AMessage := LStat;
            ASuccess := false;
          end;
        except
          on E: Exception do
          begin
            Error(E);
            ASuccess := false;
            AMessage := E.Message;
          end;
        end;
      end;
    end);
  if Assigned(AOnDuoNotifyProc) then
    AOnDuoNotifyProc(LMessage, LResult);
  if Assigned(FOnPing) then
    FOnPing(Self, LMessage, LResult);
end;

procedure TLZDUOAuthAPI.PreAuth(AUsername, AHostname, AIPAddr, ATrustedDeviceToken
  : string; AUserID: boolean; AOnDuoPreAuthProc: TOnDuoPreAuthProc);
var
  LResult, LOwnsObjects: boolean;
  LMessage: string;
  LDevices: TLZDUODevices;
begin
  LOwnsObjects := True;
  LDevices := TLZDUODevices.Create;
  try
    LResult := ExecuteRequest(LMessage, BaseResource + 'preauth', rmPOST,
      procedure(ARESTRequest: TRESTRequest)
      begin
        if AHostname <> '' then
        begin
          ARESTRequest.AddParameter('hostname', EncodeURL(AHostname),
            TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
        end;

        if AIPAddr <> '' then
        begin
          ARESTRequest.AddParameter('ipaddr', EncodeURL(AIPAddr),
            TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
        end;

        if ATrustedDeviceToken <> '' then
        begin
          ARESTRequest.AddParameter('trusted_device_token', EncodeURL(AIPAddr),
            TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
        end;

        if AUserID then
        begin
          ARESTRequest.AddParameter('user_id', EncodeURL(AUsername),
            TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
        end
        else
        begin
          ARESTRequest.AddParameter('username', EncodeURL(AUsername),
            TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
        end;
      end,
      procedure(ARESTResponse: TRESTResponse; var ASuccess: boolean;
        var AMessage: string)
      var
        LResult, LStatusMsg: string;
      begin
        if ASuccess then
        begin
          try
            LResult := ARESTResponse.JSONValue.GetValue<string>
              ('response.result', '');
            LStatusMsg := ARESTResponse.JSONValue.GetValue<string>
              ('response.status_msg', LResult);
            if (SameText(LResult, 'allow') or SameText(LResult, 'auth')) then
            begin
              LDevices.FromJSONValue(ARESTResponse.JSONValue, True,
                'response.devices');
            end
            else
            begin
              ASuccess := false;
              AMessage := LStatusMsg;
            end;
          except
            on E: Exception do
            begin
              Error(E);
              ASuccess := false;
              AMessage := E.Message;
            end;
          end;
        end;
      end);
    if Assigned(AOnDuoPreAuthProc) then
      AOnDuoPreAuthProc(LDevices, LMessage, LResult, LOwnsObjects);
    if Assigned(FOnPreAuth) then
      FOnPreAuth(Self, LDevices, LMessage, LResult, LOwnsObjects);
  finally
    if LOwnsObjects then
    begin
      FreeAndNil(LDevices);
    end;
  end;
end;

procedure TLZDUOAuthAPI.SetOnCheck(const Value: TOnDuoNotifyEvent);
begin
  FOnCheck := Value;
end;

procedure TLZDUOAuthAPI.SetOnLogin(const Value: TOnDuoLoginEvent);
begin
  FOnLogin := Value;
end;

procedure TLZDUOAuthAPI.SetOnPing(const Value: TOnDuoNotifyEvent);
begin
  FOnPing := Value;
end;

procedure TLZDUOAuthAPI.SetOnPreAuth(const Value: TOnDuoPreAuthEvent);
begin
  FOnPreAuth := Value;
end;

end.
