unit MSGraph.API;

interface

uses
  Lazy.Types, System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, REST.Client, REST.Types, System.Json,
  MSGraph.Core, Lazy.RESTClient, Lazy.REST.Types, Lazy.Model;

const
  LZMSGraphAPI_MaxChunkSize = 320680 * 4;

type

  TLZMSGraphModel = class(TLZModel);

  TLZMSGraphModelList<T: TLZMSGraphModel> = class(TLZModelList<T>)
  protected
    function GetDefaultArrayName: string; override;
  end;

  TLZMSGraphAPIOAuth2Connection = class(TLZMSGraphOAuth2Connection)
  protected
    procedure SetDefaults; override;
  end;

  TLZMSGraphAPIOAuth2Token = class(TLZMSGraphOAuth2Token);

  TLZMSGraphAPI = class(TLZRESTClientOAuth2Base)
  private
  protected
    function GetOAuth2ConnectionClass: TLZOAuth2ConnectionClass; override;
    function GetOAuth2TokenClass: TLZOAuth2TokenClass; override;
    function GetConnection: TLZMSGraphAPIOAuth2Connection; reintroduce;
    procedure FileUploadRequest(
      AFileName: TFileName;
      AProgress: TOnProgressRef;
      AMaxChunkSize: Int64;
      ARESTRequest: TRESTRequest;
      ACompletionProc: TProc;
      AASync: boolean;
      ACustomData: string);
  public
    procedure UploadFile(
      AUploadURL: string;
      AFile: TFileName;
      AProgress: TOnProgressRef = nil;
      ALazyRESTResponse: TLZRESTResponse = nil;
      AMethod: TRESTRequestMethod = rmPUT;
      ALazyRESTRequest: TLZRESTRequest = nil;
      ACustomData: string = '';
      AMaxChunkSize: Int64 = LZMSGraphAPI_MaxChunkSize);
    function GetODataNextLink(AJSONValue: TJSONValue): string;
    function GetODataValue(
      AJSONValue: TJSONValue;
      AKey: string;
      AIncludeQuotes: boolean = true): string;
    property Connection: TLZMSGraphAPIOAuth2Connection read GetConnection;
  end;

implementation

uses
  System.Math, Lazy.Utils;

{ TMSGraphAPIConnection }

procedure TLZMSGraphAPIOAuth2Connection.SetDefaults;
begin
  inherited;
  RedirectURL := 'http://localhost';
  AuthorizeEndPoint :=
    'https://login.microsoftonline.com/%tenantid%/oauth2/v2.0/authorize';
  TokenEndPoint :=
    'https://login.microsoftonline.com/%tenantid%/oauth2/v2.0/token';
  RESTEndPoint := 'https://graph.microsoft.com/';
  Scope := 'offline_access https://graph.microsoft.com/.default';
end;

{ TMSGraphAPI }

function TLZMSGraphAPI.GetConnection: TLZMSGraphAPIOAuth2Connection;
begin
  Result := inherited GetConnection as TLZMSGraphAPIOAuth2Connection;
end;

function TLZMSGraphAPI.GetOAuth2ConnectionClass: TLZOAuth2ConnectionClass;
begin
  Result := TLZMSGraphAPIOAuth2Connection;
end;

function TLZMSGraphAPI.GetOAuth2TokenClass: TLZOAuth2TokenClass;
begin
  Result := TLZMSGraphAPIOAuth2Token;
end;

function TLZMSGraphAPI.GetODataNextLink(AJSONValue: TJSONValue): string;
begin
  Result := GetODataValue(AJSONValue, '@odata.nextLink');
  Result := StringReplace(Result, Connection.RESTEndPoint, '', [rfIgnoreCase]);
end;

function TLZMSGraphAPI.GetODataValue(
  AJSONValue: TJSONValue;
  AKey: string;
  AIncludeQuotes: boolean = true): string;
begin
  Result := TLZOData.GetValue(AJSONValue, AKey, AIncludeQuotes);
end;

procedure TLZMSGraphAPI.FileUploadRequest(
  AFileName: TFileName;
  AProgress: TOnProgressRef;
  AMaxChunkSize: Int64;
  ARESTRequest: TRESTRequest;
  ACompletionProc: TProc;
  AASync: boolean;
  ACustomData: string);
var
  LBuffer: TMemoryStream;
  LFileStream: TFileStream;
  LBytesRead, LFileSize, LStartRange, LChunkSize: Int64;
  LCancel: boolean;
  LContentRange: string;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  LBuffer := TMemoryStream.Create;
  LCancel := false;
  try
    LFileSize := LFileStream.Size;
    LStartRange := 0;

    try
      if Assigned(AProgress) then
        AProgress(Self, 0, Format('Starting upload %s', [AFileName]), LCancel);
      while (LStartRange < LFileSize) and (not LCancel) do
      begin

        LChunkSize := Min(AMaxChunkSize, LFileSize - LStartRange);
        LBuffer.Position := 0;
        LBuffer.Size := LChunkSize;
        LBytesRead := LBuffer.CopyFrom(LFileStream, LChunkSize);

        LContentRange := Format('bytes %d-%d/%d',
          [LStartRange, LStartRange + (LBytesRead - 1), LFileSize]);

        ARESTRequest.AddParameter('Content-Length', IntTOstr(LBytesRead),
          pkHTTPHEADER, [poDoNotEncode]);
        ARESTRequest.AddParameter('Content-Range', LContentRange, pkHTTPHEADER,
          [poDoNotEncode]);

        ARESTRequest.AddBody(LBuffer,
          TRESTContentType.ctAPPLICATION_OCTET_STREAM);
        ARESTRequest.Execute;

        if not(ARESTRequest.Response.StatusCode in [200, 201, 202]) then
        begin
          LCancel := true;
          Error(ARESTRequest.Response.Content,
            ARESTRequest.Response.StatusCode);
        end;

        Inc(LStartRange, LBytesRead);

        if Assigned(AProgress) then
          AProgress(Self, TLZMath.CalculatePercentage(LStartRange, LFileSize),
            Format('%s (%d / %d)', [AFileName, LStartRange, LFileSize]), LCancel);

      end;

    except
      on E: Exception do
      begin
        Error(E);
      end;
    end;
  finally
    LBuffer.Free;
    LFileStream.Free;
  end;
  if Assigned(ACompletionProc) then
    ACompletionProc;
end;

procedure TLZMSGraphAPI.UploadFile(
  AUploadURL: string;
  AFile: TFileName;
  AProgress: TOnProgressRef;
  ALazyRESTResponse: TLZRESTResponse;
  AMethod: TRESTRequestMethod;
  ALazyRESTRequest: TLZRESTRequest;
  ACustomData: string;
  AMaxChunkSize: Int64);
begin
  Execute(AUploadURL, '', '',
    procedure(ARESTRequest: TRESTRequest; ARESTResponse: TRESTResponse;
      ASuccess: boolean; AMessage: string; AJSON: string; ACustomData: string)
    begin
      if Assigned(ALazyRESTResponse) then
        ALazyRESTResponse(Self, ASuccess, AMessage, ARESTResponse, ACustomData);
    end,
    procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
      BeforeRESTRequest(ARESTRequest, ACustomData);
      if Assigned(ALazyRESTRequest) then
        ALazyRESTRequest(Self, ARESTRequest, ACustomData);
    end,
    procedure(ARESTRequest: TRESTRequest; ACustomData: string)
    begin
      AfterRESTRequest(ARESTRequest, ACustomData);
    end, AMethod, ACustomData, lasFalse,
    procedure(ARESTRequest: TRESTRequest; ACompletionProc: TProc;
      AASync: boolean; ACustomData: string)
    begin
      FileUploadRequest(AFile, AProgress, AMaxChunkSize, ARESTRequest,
        ACompletionProc, AASync, ACustomData);
    end);
end;

{ TLZMSGraphModelList<T> }

function TLZMSGraphModelList<T>.GetDefaultArrayName: string;
begin
  Result := 'value';
end;

end.
