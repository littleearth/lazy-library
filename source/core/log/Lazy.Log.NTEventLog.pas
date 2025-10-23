{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.NTEventLog
  Author: Tristan Marlow
  Purpose: Log handler that sends messages to the Windows NT Event Log

  Overview:
  This unit provides a log handler that writes log messages to the Windows
  Event Log system. It automatically registers the application with the
  Event Log service and maps log levels to appropriate event types.

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ----------------------------------------------------------------------------; }
unit Lazy.Log.NTEventLog;

interface

uses
  SysUtils, Classes, Lazy.Log, Lazy.Types
{$IFDEF MSWINDOWS}
  , Winapi.Windows, System.Win.Registry
{$ENDIF}
  ;

type
  /// <summary>
  /// Log handler that writes messages to the Windows NT Event Log
  /// </summary>
  TLZLogNTEventLog = class(TLZLogHandler)
  private
  protected
    {$IFDEF MSWINDOWS}
    /// <summary>
    /// Writes a message to the Windows Event Log
    /// </summary>
    procedure EventLogMessage(AMessage: string; ALogLevel: TLZLogLevel);
    /// <summary>
    /// Registers the application with the Event Log service
    /// </summary>
    procedure RegisterApplication;
    /// <summary>
    /// Unregisters the application from the Event Log service
    /// </summary>
    procedure UnregisterApplication;
    /// <summary>
    /// Checks if the application is registered with the Event Log
    /// </summary>
    function IsRegistered: boolean;
    {$ENDIF}
  public
    /// <summary>
    /// Name of the application for Event Log registration
    /// </summary>
    class var ApplicationName: string;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Processes a log message and writes it to the Event Log
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
  end;

implementation

uses
  Lazy.Utils;

{ TLZLogNTEventLog }

procedure TLZLogNTEventLog.AfterConstruction;
begin
  inherited;
end;

procedure TLZLogNTEventLog.BeforeDestruction;
begin
  inherited;
end;

procedure TLZLogNTEventLog.ProcessLogMessage(ALogMessage: TLazyLogMessage);
var
  LFormattedMessage: string;
begin
  LFormattedMessage :=GetFormattedMessage(ALogMessage, false);

  if not TLZString.IsEmptyString(LFormattedMessage) then
  begin
    {$IFDEF MSWINDOWS}
    EventLogMessage(LFormattedMessage, ALogMessage.LogLevel);
    {$ENDIF}
  end;
end;



{$IFDEF MSWINDOWS}
procedure TLZLogNTEventLog.EventLogMessage(AMessage: string; ALogLevel: TLZLogLevel);
var
  LLogHandle: THandle;
  LEventType: integer;
  LLogEvent: boolean;
begin
  case ALogLevel of
    logError:
      begin
        LEventType := 1;
        LLogEvent := True;
      end;
    logWarning:
      begin
        LEventType := 2;
        LLogEvent := True;
      end;
    logInformation:
      begin
        LEventType := 4;
        LLogEvent := False;
      end;
  else
    begin
      LEventType := 4;
      LLogEvent := False;
    end;
  end;
  
  if LLogEvent then
  begin
    try
      if not IsRegistered then
        RegisterApplication;
    if  IsRegistered then
    begin
      LLogHandle := OpenEventLog(nil, PChar(ApplicationName));
      if LLogHandle <> 0 then
      begin
        ReportEvent(LLogHandle, LEventType, 0, 0, nil, 1, 0, @AMessage, nil);
        CloseEventLog(LLogHandle);
      end;
      end;
    except
      // Suppress exceptions
    end;
  end;
end;


function TLZLogNTEventLog.IsRegistered: boolean;
var
  LReg: TRegistry;
begin
  Result := False;
  if ApplicationName = '' then
    Exit;
    
  LReg := TRegistry.Create;
  try
    LReg.RootKey := HKEY_LOCAL_MACHINE;
    Result := LReg.KeyExists('\SYSTEM\CurrentControlSet\Services\EventLog\Application\' + ApplicationName);
  finally
    LReg.Free;
  end;
end;

procedure TLZLogNTEventLog.RegisterApplication;
var
  LReg: TRegistry;
begin
  if ApplicationName <> '' then
  begin
    LReg := TRegistry.Create;
    try
      LReg.RootKey := HKEY_LOCAL_MACHINE;
      if LReg.OpenKey('\SYSTEM\CurrentControlSet\Services\EventLog\Application\' + ApplicationName, True) then
      begin
        LReg.WriteString('EventMessageFile', ParamStr(0));
        LReg.WriteInteger('TypesSupported', 7);
        LReg.CloseKey;
      end;
    finally
      LReg.Free;
    end;
  end
  else
  begin
    raise Exception.Create('Application name has not been defined.');
  end;
end;

procedure TLZLogNTEventLog.UnregisterApplication;
var
  LReg: TRegistry;
begin
  if ApplicationName <> '' then
  begin
    LReg := TRegistry.Create;
    try
      LReg.RootKey := HKEY_LOCAL_MACHINE;
      LReg.DeleteKey('\SYSTEM\CurrentControlSet\Services\EventLog\Application\' + ApplicationName);
    finally
      LReg.Free;
    end;
  end
  else
  begin
    raise Exception.Create('ApplicationName has not been defined.');
  end;
end;
{$ENDIF}


initialization

{$IFDEF MSWINDOWS}
TLZLogNTEventLog.ApplicationName := '';
{$ENDIF}

end.
