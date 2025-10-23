{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.SysLog
  Author: Tristan Marlow
  Purpose: Log handler that sends messages to a Syslog server using Indy
  
  Overview:
  This unit provides a log handler that sends log messages to a Syslog server
  using the Indy networking library. It supports both RFC 3164 and RFC 5424
  standards and provides configurable host, port, and facility settings.
  
  Features:
  - Cross-platform Syslog support via Indy
  - RFC 3164 and RFC 5424 compatible
  - Configurable host, port, and facility
  - Automatic severity mapping from log levels
  - Thread-safe message sending
  
  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ---------------------------------------------------------------------------- }
unit Lazy.Log.SysLog;

interface

uses
  SysUtils, Classes, System.SyncObjs, Lazy.Log, Lazy.Types,
  IdSysLog, IdSysLogMessage, IdBaseComponent;

type
  /// <summary>
  /// Log handler that sends messages to a Syslog server
  /// </summary>
  TLZLogSysLog = class(TLZLogHandler)
  private
    FSysLog: TIdSysLog;
    FSysLogLock: TCriticalSection;
    FHost: string;
    FPort: Integer;
    FFacility: TIdSyslogFacility;
    FConnected: Boolean;
    /// <summary>
    /// Sets the Syslog server host name
    /// </summary>
    procedure SetHost(const AValue: string);
    /// <summary>
    /// Sets the Syslog server port number
    /// </summary>
    procedure SetPort(const AValue: Integer);
    /// <summary>
    /// Sets the Syslog facility type
    /// </summary>
    procedure SetFacility(const AValue: TIdSyslogFacility);
    /// <summary>
    /// Maps log level and type to Syslog severity
    /// </summary>
    function GetSeverity(ALogLevel: TLZLogLevel; ALogType: TLazyLogType): TIdSyslogSeverity;
    /// <summary>
    /// Ensures connection to the Syslog server
    /// </summary>
    procedure EnsureConnected;
    /// <summary>
    /// Sends a log message to the Syslog server
    /// </summary>
    procedure SendLogMessage(const AMessage: string; ASeverity: TIdSyslogSeverity);
  protected
  public
    /// <summary>
    /// Default Syslog server host name
    /// </summary>
    class var DefaultHost: string;
    /// <summary>
    /// Default Syslog server port number
    /// </summary>
    class var DefaultPort: Integer;
    /// <summary>
    /// Default Syslog facility type
    /// </summary>
    class var DefaultFacility: TIdSyslogFacility;
    /// <summary>
    /// Default application name for Syslog messages
    /// </summary>
    class var DefaultAppName: string;
    
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Processes a log message and sends it to the Syslog server
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
    
    /// <summary>
    /// Syslog server host name or IP address
    /// </summary>
    property Host: string read FHost write SetHost;
    /// <summary>
    /// Syslog server port number (default: 514)
    /// </summary>
    property Port: Integer read FPort write SetPort;
    /// <summary>
    /// Syslog facility type for categorizing messages
    /// </summary>
    property Facility: TIdSyslogFacility read FFacility write SetFacility;
  end;

implementation

{ TLZLogSysLog }

procedure TLZLogSysLog.AfterConstruction;
begin
  inherited;
  FSysLogLock := TCriticalSection.Create;
  FSysLog := TIdSysLog.Create(nil);
  FConnected := False;
  
  // Set defaults from class variables
  FHost := DefaultHost;
  if FHost = '' then
    FHost := 'localhost';
    
  FPort := DefaultPort;
  if FPort = 0 then
    FPort := 514;
    
  FFacility := DefaultFacility;
end;

procedure TLZLogSysLog.BeforeDestruction;
begin
  FSysLogLock.Acquire;
  try
    if Assigned(FSysLog) then
    begin
      FreeAndNil(FSysLog);
    end;
  finally
    FSysLogLock.Release;
  end;
  
  FreeAndNil(FSysLogLock);
  inherited;
end;

procedure TLZLogSysLog.SetHost(const AValue: string);
begin
  FSysLogLock.Acquire;
  try
    if FHost <> AValue then
    begin
      FHost := AValue;
      FConnected := False;
    end;
  finally
    FSysLogLock.Release;
  end;
end;

procedure TLZLogSysLog.SetPort(const AValue: Integer);
begin
  FSysLogLock.Acquire;
  try
    if FPort <> AValue then
    begin
      FPort := AValue;
      FConnected := False;
    end;
  finally
    FSysLogLock.Release;
  end;
end;

procedure TLZLogSysLog.SetFacility(const AValue: TIdSyslogFacility);
begin
  FSysLogLock.Acquire;
  try
    FFacility := AValue;
  finally
    FSysLogLock.Release;
  end;
end;

function TLZLogSysLog.GetSeverity(ALogLevel: TLZLogLevel; ALogType: TLazyLogType): TIdSyslogSeverity;
begin
  // Map log type first (errors and warnings have priority)
  case ALogType of
    ltError:
      Result := slError;
    ltWarning:
      Result := slWarning;
    ltProgress:
      Result := slNotice;
    ltDebug:
      Result := slDebug;
    ltInformation:
      Result := slInformational;
  else
    // Fall back to log level mapping
    case ALogLevel of
      logDebug:
        Result := slDebug;
      logInformation:
        Result := slInformational;
      logWarning:
        Result := slWarning;
      logError:
        Result := slError;
    else
      Result := slInformational;
    end;
  end;
end;

procedure TLZLogSysLog.EnsureConnected;
begin
  if not FConnected then
  begin
    FSysLog.Host := FHost;
    FSysLog.Port := FPort;
    FConnected := True;
  end;
end;

procedure TLZLogSysLog.SendLogMessage(const AMessage: string; ASeverity: TIdSyslogSeverity);
var
  LSysLogMsg: TIdSysLogMessage;
  LAppName: string;
begin
  if AMessage = '' then
    Exit;
    
  FSysLogLock.Acquire;
  try
    EnsureConnected;
    
    LSysLogMsg := TIdSysLogMessage.Create;
    try
      LSysLogMsg.Facility := FFacility;
      LSysLogMsg.Severity := ASeverity;
      
      // Set application name
      LAppName := DefaultAppName;
      if LAppName = '' then
        LAppName := TLZLog.ApplicationName;
      if LAppName = '' then
        LAppName := ExtractFileName(ParamStr(0));
        
      LSysLogMsg.Msg.Text := Format('[%s] %s', [LAppName, AMessage]);
      
      FSysLog.SendLogMessage(LSysLogMsg);
    finally
      LSysLogMsg.Free;
    end;
  finally
    FSysLogLock.Release;
  end;
end;

procedure TLZLogSysLog.ProcessLogMessage(ALogMessage: TLazyLogMessage);
var
  LFormattedMessage: string;
  LSeverity: TIdSyslogSeverity;
begin
  // Get formatted message from base class
  LFormattedMessage := GetFormattedMessage(ALogMessage);
  
  if LFormattedMessage = '' then
    Exit;
  
  // Determine severity based on log type and level
  LSeverity := GetSeverity(ALogMessage.LogLevel, ALogMessage.LogType);
  
  // Send to syslog server
  try
    SendLogMessage(LFormattedMessage, LSeverity);
  except
    // Suppress exceptions to prevent logging from breaking the application
    // In production, syslog server might be unreachable
  end;
end;

initialization

// Default configuration
TLZLogSysLog.DefaultHost := 'localhost';
TLZLogSysLog.DefaultPort := 514;
TLZLogSysLog.DefaultFacility := sfLocalUseOne;
TLZLogSysLog.DefaultAppName := '';

end.

