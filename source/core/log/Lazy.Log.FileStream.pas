{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.FileStream
  Author: Tristan Marlow
  Purpose: Log handler that outputs messages to a file

  Overview:
  This unit provides a file-based log handler that writes log messages to
  a text file. It uses a threaded file stream for thread-safe writing and
  automatically creates log files with default naming based on the executable.

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ----------------------------------------------------------------------------; }
unit Lazy.Log.FileStream;

interface

uses
  SysUtils, Classes, System.SyncObjs, System.Generics.Collections,
  System.DateUtils, Lazy.Log, Lazy.Types, Lazy.ThreadedFileStream, Lazy.Utils;

type
  TLZLogFileStream = class(TLZLogHandler)
  private
    FLogFileLocker: TCriticalSection;
    FLog: TLZThreadedFileStream;
    FFileName: TFileName;
    FLogActions: TDictionary<string, TDateTime>;
    /// <summary>
    /// Sets the log file name and initializes logging
    /// </summary>
    procedure SetFileName(const Value: TFileName);
  protected
    /// <summary>
    /// Starts the log file stream
    /// </summary>
    procedure StartLog;
    /// <summary>
    /// Stops the log file stream
    /// </summary>
    procedure StopLog;
    /// <summary>
    /// Writes a log message to the file
    /// </summary>
    procedure WriteLogMessage(ALogMessage: TLazyLogMessage; AMessage: string);
    /// <summary>
    /// Gets the default file name for the log file
    /// </summary>
    function GetDefaultFileName: TFileName; virtual;
    /// <summary>
    /// Gets a unique key for the log action
    /// </summary>
    function GetActionKey(ALogMessage: TLazyLogMessage): string;
    procedure AfterAddHandler; override;
    procedure BeforeRemoveHandler; override;
    /// <summary>
    /// Logs file details when handler is added/removed
    /// </summary>
    procedure LogFileDetails;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Clears all content from the log file
    /// </summary>
    procedure ClearLogFile;
    /// <summary>
    /// Flushes any pending writes to the log file
    /// </summary>
    procedure FlushLogFile;
    /// <summary>
    /// Processes a log message and writes it to the file
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
    /// <summary>
    /// Name of the log file to write to
    /// </summary>
    property FileName: TFileName read FFileName write SetFileName;
  end;

  /// <summary>
  /// Convenience procedure to add a file stream handler to the global log
  /// </summary>
procedure LazyLogAddFileSteamHander(AFileName: string = '';
  AName: string = 'FileStream');

implementation

procedure LazyLogAddFileSteamHander(AFileName: string; AName: string);
begin
  with LazyLog.AddHandler(AName, TLZLogFileStream) as TLZLogFileStream do
  begin
    if AFileName <> '' then
    begin
      FileName := AFileName;
    end;
  end;
end;

procedure TLZLogFileStream.AfterAddHandler;
begin
  inherited;
end;

{ TLZLogBasic }

procedure TLZLogFileStream.AfterConstruction;
begin
  inherited;
  FFileName := '';
  FLog := nil;
  FLogFileLocker := TCriticalSection.Create;
  FLogActions := TDictionary<string, TDateTime>.Create;
end;

procedure TLZLogFileStream.BeforeDestruction;
begin
  try
    StopLog;
    if Assigned(FLogFileLocker) then
    begin
      FLogFileLocker.Free;
      FLogFileLocker := nil;
    end;
    if Assigned(FLogActions) then
    begin
      FLogActions.Free;
      FLogActions := nil;
    end;
  finally
    inherited;
  end;
end;

procedure TLZLogFileStream.BeforeRemoveHandler;
begin
  inherited;
  LogFileDetails;
end;

procedure TLZLogFileStream.ProcessLogMessage(ALogMessage: TLazyLogMessage);
var
  LFormattedMessage: string;
begin
  LFormattedMessage := GetFormattedMessage(ALogMessage, false);

  if not TLZString.IsEmptyString(LFormattedMessage) then
  begin
    WriteLogMessage(ALogMessage, LFormattedMessage);
  end;
end;

procedure TLZLogFileStream.WriteLogMessage(ALogMessage: TLazyLogMessage;
  AMessage: string);
var
  LMessage: string;
begin
  FLogFileLocker.Acquire;
  try
    if not Assigned(FLog) then
    begin
      StartLog;
    end;
    if Assigned(FLog) then
    begin
      LMessage := Format('%s;%s;%s', [FormatDateTime('yyyy-mm-dd hh:nn:ss',
        ALogMessage.DateTime), TLZLog.GetLogLevelText(ALogMessage.LogLevel),
        AMessage]) + #13#10;
      LMessage := TLZString.StringCleaner(LMessage, True, True);
      FLog.AppendStr(LMessage);
    end;
  finally
    FLogFileLocker.Release;
  end;
end;

procedure TLZLogFileStream.SetFileName(const Value: TFileName);
begin
  if not SameText(Value, FFileName) then
  begin
    StopLog;
    FFileName := Value;
    if TLZString.IsEmptyString(FFileName) then
      FFileName := GetDefaultFileName;
    LogFileDetails;
  end;
end;

procedure TLZLogFileStream.StartLog;
begin
  if TLZString.IsEmptyString(FFileName) then
    FileName := GetDefaultFileName;
  if not Assigned(FLog) then
    FLog := TLZThreadedFileStream.Create(FFileName);
  ClearLogFile;
end;

procedure TLZLogFileStream.StopLog;
begin
  if Assigned(FLog) then
  begin
    try
      FLog.Flush;
      FLog.Terminate;
      FLog.Free;
    finally
      FLog := nil;
    end;
  end;
end;

procedure TLZLogFileStream.FlushLogFile;
begin
  if Assigned(FLog) then
    FLog.Flush;
end;

function TLZLogFileStream.GetDefaultFileName: TFileName;
begin
  Result := ChangeFileExt(ParamStr(0), '.log');
end;

procedure TLZLogFileStream.LogFileDetails;
begin
  Log('Filename: %s', [FileName]);
end;

procedure TLZLogFileStream.ClearLogFile;
begin
  if Assigned(FLog) then
    FLog.Clear;
end;

function TLZLogFileStream.GetActionKey(ALogMessage: TLazyLogMessage): string;
var
  LSenderName: string;
begin
  LSenderName := ALogMessage.LogClass;
  if LSenderName = '' then
    LSenderName := 'Unknown';
  Result := Format('%s_%s_%d', [LSenderName, ALogMessage.ActionName,
    ALogMessage.ThreadID]);
end;

initialization

finalization

end.
