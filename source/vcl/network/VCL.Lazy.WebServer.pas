unit VCL.Lazy.WebServer;

interface

uses
  Lazy.Types, Lazy.Utils.Windows, Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, IdHTTPWebBrokerBridge,
  Web.HTTPApp, IdContext, IdSSL, IdSSLOpenSSL, IdCTypes, IdSSLOpenSSLHeaders,
  IdComponent, IdGlobal;

type
{$IFDEF ISAPI}
  TNukeWebServerBaseType = TLZObject;
{$ELSE}
  TLZWebServerBaseType = TLZComponent;
{$ENDIF}

  TLZWebServerBase = class(TLZWebServerBaseType)
  private
    FSSLPassword: string;
    FPort: integer;
    FSSLKeyFile: string;
    FSSLEnabled: Boolean;
    FThreadPoolSize: integer;
    FSSLRootCertFile: string;
    FSSLCertFile: string;
    procedure SetPort(const Value: integer);
    procedure SetSSLCertFile(const Value: string);
    procedure SetSSLEnabled(const Value: Boolean);
    procedure SetSSLKeyFile(const Value: string);
    procedure SetSSLPassword(const Value: string);
    procedure SetSSLRootCertFile(const Value: string);
    procedure SetThreadPoolSize(const Value: integer);
  protected
    property Port: integer read FPort write SetPort;
    property ThreadPoolSize: integer read FThreadPoolSize
      write SetThreadPoolSize;
    property SSLEnabled: Boolean read FSSLEnabled write SetSSLEnabled;
    property SSLCertFile: string read FSSLCertFile write SetSSLCertFile;
    property SSLKeyFile: string read FSSLKeyFile write SetSSLKeyFile;
    property SSLRootCertFile: string read FSSLRootCertFile
      write SetSSLRootCertFile;
    property SSLPassword: string read FSSLPassword write SetSSLPassword;
    procedure BeforeStartServer; virtual; abstract;
    procedure BeforeStopServer; virtual; abstract;
    function GetWebServerRoot: string; virtual;
  public
    function GetMIMEType(AFilename: TFileName): string; virtual;
    function GetRequestedFileName(ADocument, ARootPath: string;
      var AFilename: TFileName): Boolean; virtual;
    procedure WebServerWebAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    property WebServerRoot: string read GetWebServerRoot;
  end;

{$IFDEF ISAPI}

  TLZWebServerISAPI = class(TLZWebServerBase)
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  end;

  TLZWebServer = TLZWebServerISAPI;
{$ELSE}

  TLZWebServerIndy = class(TLZWebServerBase)
  private
    FServer: TIdHTTPWebBrokerBridge;
    FIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
    procedure CreateServer;
    procedure DestroyServer;
    procedure OnParseAuthenticationHandler(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
    procedure OnServerException(AContext: TIdContext; AException: Exception);
    procedure OnGetSSLPassword(var Password: string);
    procedure OnSSLStatusInfo(const AMsg: String);
    procedure OnSSLStatusInfoEx(ASender: TObject; const AsslSocket: PSSL;
      const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
    procedure OnSSLStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure OnQuerySslPort(APort: TIdPort; var VUseSSL: Boolean);
  protected
    procedure BeforeStartServer; override;
    procedure BeforeStopServer; override;
    procedure AuthenticationHandler(AContext: TIdContext;
      const AAuthType, AAuthData: String; var AUsername, APassword: String;
      var AHandled: Boolean); virtual;
    procedure ServerException(AContext: TIdContext; AException: Exception;
      var AHandled: Boolean); virtual;
    property Server: TIdHTTPWebBrokerBridge read FServer;
  public
    constructor Create(AOwner: TComponent); override;
    function StartServer: Boolean;
    procedure StopServer;
    function Active: Boolean;
  end;

  TLZWebServer = TLZWebServerIndy;
{$ENDIF}

implementation

uses
  IdSchedulerOfThreadPool, IdException, System.IOUtils,
  System.StrUtils, IdGlobalProtocols;

{$IFDEF ISAPI}
{ TLZWebServerISAPI }

constructor TLZWebServerISAPI.Create(AOwner: TComponent);
begin
  BeforeStartServer;
end;

destructor TLZWebServerISAPI.Destroy;
begin
  try
    BeforeStopServer
  finally
    inherited;
  end;
end;

{$ELSE}

function TLZWebServerIndy.Active: Boolean;
begin
  Result := Assigned(FServer) and (FServer.Active);
end;

procedure TLZWebServerIndy.OnParseAuthenticationHandler(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  // Allow JWT Bearer authentication's scheme
  if SameText(AAuthType, 'Bearer') then
    VHandled := True;
  AuthenticationHandler(AContext, AAuthType, AAuthData, VUsername, VPassword,
    VHandled);
end;

procedure TLZWebServerIndy.OnServerException(AContext: TIdContext;
  AException: Exception);
var
  LHandled: Boolean;
begin
  LHandled := False;
  if AException is EIdConnClosedGracefully then
  begin
    Log(AException.Message);
    LHandled := True;
  end;
  ServerException(AContext, AException, LHandled);
  if not LHandled then
  begin
    Error(AException);
  end;
end;

procedure TLZWebServerIndy.AuthenticationHandler(AContext: TIdContext;
  const AAuthType, AAuthData: String; var AUsername, APassword: String;
  var AHandled: Boolean);
begin

end;

procedure TLZWebServerIndy.BeforeStartServer;
begin

end;

procedure TLZWebServerIndy.BeforeStopServer;
begin

end;

procedure TLZWebServerIndy.OnSSLStatusInfo(const AMsg: String);
begin
  Debug('OnSSLStatusInfo', AMsg);
end;

procedure TLZWebServerIndy.OnSSLStatusInfoEx(ASender: TObject;
  const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT;
  const AType, AMsg: String);
begin
  Debug('OnSSLStatusInfoEx', AType + AMsg);
end;

procedure TLZWebServerIndy.OnSSLStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  Debug('OnSSLStatus', AStatusText);
end;

constructor TLZWebServerIndy.Create(AOwner: TComponent);
begin
  inherited;
  FSSLEnabled := False;
end;

procedure TLZWebServerIndy.OnQuerySslPort(APort: TIdPort; var VUseSSL: Boolean);
begin
  VUseSSL := FSSLEnabled;
end;

procedure TLZWebServerIndy.CreateServer;
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
  FIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(FServer);

  LScheduler := TIdSchedulerOfThreadPool.Create(FServer);
  LScheduler.PoolSize := FThreadPoolSize;
  FServer.Scheduler := LScheduler;
  FServer.MaxConnections := LScheduler.PoolSize;
  FServer.OnParseAuthentication := OnParseAuthenticationHandler;
  FServer.OnException := OnServerException;
  FServer.Bindings.Clear;
  FServer.OnQuerySslPort := OnQuerySslPort;

  FIOHandleSSL.OnGetPassword := OnGetSSLPassword;
  FIOHandleSSL.OnStatusInfo := OnSSLStatusInfo;
  FIOHandleSSL.OnStatusInfoEx := OnSSLStatusInfoEx;
  FIOHandleSSL.OnStatus := OnSSLStatus;

  IdOpenSSLSetLibPath(TLZFile.GetApplicationDir);

end;

procedure TLZWebServerIndy.OnGetSSLPassword(var Password: string);
begin
  Password := FSSLPassword;
end;

procedure TLZWebServerIndy.ServerException(AContext: TIdContext;
  AException: Exception; var AHandled: Boolean);
begin

end;

function TLZWebServerIndy.StartServer: Boolean;
begin
  Result := False;
  StopServer;
  if not Assigned(FServer) then
  begin
    try
      CreateServer;
      BeforeStartServer;

      FIOHandleSSL.SSLOptions.CertFile := SSLCertFile;
      FIOHandleSSL.SSLOptions.KeyFile := SSLKeyFile;
      FIOHandleSSL.SSLOptions.RootCertFile := SSLRootCertFile;
      FIOHandleSSL.SSLOptions.Mode := sslmServer;
      FIOHandleSSL.SSLOptions.VerifyMode := [];
      FIOHandleSSL.SSLOptions.VerifyDepth := 0;
      FIOHandleSSL.SSLOptions.SSLVersions := [sslvTLSv1_2];
      FIOHandleSSL.SSLOptions.Method := sslvTLSv1_2;

      if FSSLEnabled then
      begin
        Log(Format('SSL Enabled, Cert: %s, Key: %s',
          [SSLCertFile, SSLKeyFile]));
        FServer.IOHandler := FIOHandleSSL;
      end;
      FServer.DefaultPort := Port;
      Log(Format('Starting server on port %d', [FServer.DefaultPort]));
      FServer.Active := True;
      Log(Format('Server started on port %d', [FServer.DefaultPort]));
      Result := FServer.Active;
    except
      on E: Exception do
      begin
        StopServer;
        Error(E);
      end;
    end;
  end;
end;

procedure TLZWebServerIndy.DestroyServer;
begin
  try
    FServer.Active := False;
    FServer.Bindings.Clear;
  finally
    FreeAndNil(FServer);
  end;
end;

procedure TLZWebServerIndy.StopServer;
begin
  try
    try
      BeforeStopServer;
      if Assigned(FServer) then
      begin
        Log(Format('Stopping server on port %d', [FServer.DefaultPort]));
        DestroyServer;
      end;
    except
      on E: Exception do
      begin
        Error(E);
      end;
    end;
  finally
    FServer := nil;
  end;
end;

{$ENDIF}
{ TLZWebServerBase }

procedure TLZWebServerBase.SetPort(const Value: integer);
begin
  FPort := Value;
end;

procedure TLZWebServerBase.SetSSLCertFile(const Value: string);
begin
  FSSLCertFile := Value;
end;

procedure TLZWebServerBase.SetSSLEnabled(const Value: Boolean);
begin
  FSSLEnabled := Value;
end;

procedure TLZWebServerBase.SetSSLKeyFile(const Value: string);
begin
  FSSLKeyFile := Value;
end;

procedure TLZWebServerBase.SetSSLPassword(const Value: string);
begin
  FSSLPassword := Value;
end;

procedure TLZWebServerBase.SetSSLRootCertFile(const Value: string);
begin
  FSSLRootCertFile := Value;
end;

procedure TLZWebServerBase.SetThreadPoolSize(const Value: integer);
begin
  FThreadPoolSize := Value;
end;

function TLZWebServerBase.GetRequestedFileName(ADocument: string;
  ARootPath: string; var AFilename: TFileName): Boolean;
begin

  AFilename := ADocument;
  if Length(Trim(AFilename)) > 0 then
  begin
    if AFilename[1] = '/' then
    begin
      Delete(AFilename, 1, 1);
    end;
  end;

  AFilename := StringReplace(AFilename, '/', '\', [rfReplaceAll]);
  AFilename := StringReplace(AFilename, '\\', '\', [rfReplaceAll]);

  AFilename := TPath.Combine(IncludeTrailingPathDelimiter(ARootPath),
    AFilename);

  if DirectoryExists(AFilename) then
  begin
    AFilename := IncludeTrailingPathDelimiter(AFilename);
    if FileExists(AFilename + 'index.html') then
      AFilename := AFilename + 'index.html'
    else if FileExists(AFilename + 'index.htm') then
      AFilename := AFilename + 'index.htm'
    else if FileExists(AFilename + 'default.htm') then
      AFilename := AFilename + 'default.htm'
    else if FileExists(AFilename + 'default.html') then
      AFilename := AFilename + 'default.html';
  end;

  AFilename := TPath.GetFullPath(AFilename);

  Result := ContainsText(AFilename, ARootPath) and FileExists(AFilename);

end;

function TLZWebServerBase.GetMIMEType(AFilename: TFileName): string;
begin
  Result := TLZFile.GetMIMEType(AFilename);
end;

procedure TLZWebServerBase.WebServerWebAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LPathInfo, LQuery: String;
  LFileName: TFileName;
  LRootFolder: string;
  LContentType: string;
  LFileStream: TFileStream;
  LAllow: Boolean;
begin
  Handled := False;

  LRootFolder := WebServerRoot;

  LQuery := Request.Query;
  LPathInfo := Request.PathInfo;
  LPathInfo := StringReplace(LPathInfo, '/www/', '', []);

  LAllow := True;

  if (LPathInfo = '/') then
  begin
    if not TLZString.isEmptyString(LQuery) then
    begin
      // soap request
      LAllow := False;
    end;
  end;

  if LAllow then
  begin
    if GetRequestedFileName(LPathInfo, LRootFolder, LFileName) then
    begin
      LContentType := GetMIMEType(LFileName);
      LFileStream := TFileStream.Create(LFileName,
        fmOpenRead + fmShareDenyWrite);
      Response.ContentType := LContentType;
      Response.ContentStream := LFileStream;
      Response.StatusCode := 200;
      Response.ContentLength := LFileStream.Size;
      Handled := True;
    end
    else
    begin
      Error(Format('[%s] File not found: %s', [Request.RemoteAddr, LFileName]));
    end;
  end;
end;

function TLZWebServerBase.GetWebServerRoot: string;
begin
  Result := IncludeTrailingPathDelimiter
    (ExtractFilePath(GetModuleName(HInstance)));
  Result := IncludeTrailingPathDelimiter(Result + 'www');
end;

end.
