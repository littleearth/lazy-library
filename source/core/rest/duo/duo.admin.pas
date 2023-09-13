unit duo.admin;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  REST.Authenticator.Basic, System.JSON, duo.api, System.Generics.Collections,
  duo.models;

type

  TOnDuoUsersEvent = procedure(ASender: TObject; AUsers: TLZDUOUsers;
    AMessage: string; ASuccess: boolean; var AOwnsObjects: boolean) of object;

  TOnDuoUsersProc = reference to procedure(AUsers: TLZDUOUsers; AMessage: string;
    ASuccess: boolean; var AOwnsObjects: boolean);

  TLZDUOAdminApi = class(TLZDuoAPIBase)
  private
    FOnGetUsers: TOnDuoUsersEvent;
    FAccountID: string;
    procedure SetOnGetUsers(const Value: TOnDuoUsersEvent);
    procedure SetAccountID(const Value: string);
  protected
    function GetBaseResource: string; override;
  public
    procedure GetLogo(AFileName: TFilename; var AMessage: string;
      var ASuccess: boolean);
    procedure GetUsers(ALimit: integer = -1; AOffset: integer = -1;
      AUsername: string = ''; AEmail: string = ''; AUserID: boolean = false;
      AGetAllUsers: boolean = false; AOnDuoUsersProc: TOnDuoUsersProc = nil);
    procedure GetAllUsers(AOnDuoUsersProc: TOnDuoUsersProc = nil);
  published
    property AccountID: string read FAccountID write SetAccountID;
    property OnGetUsers: TOnDuoUsersEvent read FOnGetUsers write SetOnGetUsers;
  end;

implementation

uses
  System.StrUtils;

{ TDUOAdmin }

procedure TLZDUOAdminApi.GetAllUsers(AOnDuoUsersProc: TOnDuoUsersProc = nil);
begin
  GetUsers(-1, -1, '', '', false, true, AOnDuoUsersProc);
end;

function TLZDUOAdminApi.GetBaseResource: string;
begin
  Result := '/admin/v1/';
end;

procedure TLZDUOAdminApi.GetLogo(AFileName: TFilename; var AMessage: string;
  var ASuccess: boolean);
begin
  ASuccess := ExecuteRequest(AMessage, BaseResource + 'logo', rmGET,
    procedure(ARESTRequest: TRESTRequest)
    begin
      if not IsEmptyString(FAccountID) then
      begin
        ARESTRequest.AddParameter('account_id', FAccountID,
          TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
      end;
    end,
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

procedure TLZDUOAdminApi.GetUsers(ALimit, AOffset: integer;
AUsername, AEmail: string; AUserID, AGetAllUsers: boolean;
AOnDuoUsersProc: TOnDuoUsersProc);
var
  LUsers: TLZDUOUsers;
  LResult, LOwnsObjects, LComplete: boolean;
  LMessage: string;
  LLastUserCount, LLimit, LOffset: integer;
begin
  LOwnsObjects := true;
  LLimit := ALimit;
  LOffset := AOffset;
  if AGetAllUsers then
  begin
    if LLimit = -1 then
      LLimit := 300;
    LOffset := 0;
  end;
  LLastUserCount := 0;
  LComplete := not AGetAllUsers;
  LUsers := TLZDUOUsers.Create;
  try
    Repeat
      LResult := ExecuteRequest(LMessage, BaseResource + 'users', rmGET,
        procedure(ARESTRequest: TRESTRequest)
        begin
          if not IsEmptyString(FAccountID) then
          begin
            ARESTRequest.AddParameter('account_id', FAccountID,
              TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
          end;
          if LLimit > 0 then
          begin
            ARESTRequest.AddParameter('limit', IntTOstr(LLimit),
              TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
          end;

          if LOffset > 0 then
          begin
            ARESTRequest.AddParameter('offset', IntTOstr(LOffset),
              TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
          end;

          if AEmail <> '' then
          begin
            ARESTRequest.AddParameter('email', EncodeURL(AEmail),
              TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
          end;

          if AUsername <> '' then
          begin
            if AUserID then
            begin
              ARESTRequest.AddParameter('user_ids', EncodeURL(AUsername),
                TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
            end
            else
            begin
              if ContainsText('usernames=', AUsername) then
              begin
                ARESTRequest.AddParameter('usernames', EncodeURL(AUsername),
                  TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
              end
              else
              begin
                ARESTRequest.AddParameter('username', EncodeURL(AUsername),
                  TRESTRequestParameterKind.pkGETorPOST, [poDoNotEncode]);
              end;
            end;
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
              LResult := ARESTResponse.JSONValue.GetValue<string>('stat', '');
              if (SameText(LResult, 'Ok')) then
              begin
                LLastUserCount := LUsers.Count;
                LUsers.FromJSONValue(ARESTResponse.JSONValue, not AGetAllUsers);
                LLastUserCount := LUsers.Count - LLastUserCount;
                ASuccess := true;
              end
              else
              begin
                LStatusMsg := ARESTResponse.JSONValue.GetValue<string>
                  ('message', '');
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

      LOffset := LOffset + LLimit;

      if (LLastUserCount = 0) or (LLastUserCount < LLimit) then
      begin
        LComplete := true;
      end;
    Until (LComplete);

    if Assigned(AOnDuoUsersProc) then
    begin
      AOnDuoUsersProc(LUsers, LMessage, LResult, LOwnsObjects);
    end;
    if Assigned(FOnGetUsers) then
    begin
      FOnGetUsers(Self, LUsers, LMessage, LResult, LOwnsObjects);
    end;
  finally
    if LOwnsObjects then
      FreeAndNil(LUsers);
  end;

end;

procedure TLZDUOAdminApi.SetAccountID(const Value: string);
begin
  FAccountID := Value;
end;

procedure TLZDUOAdminApi.SetOnGetUsers(const Value: TOnDuoUsersEvent);
begin
  FOnGetUsers := Value;
end;

end.
