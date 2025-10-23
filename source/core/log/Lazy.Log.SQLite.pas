{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.SQLite
  Author: Tristan Marlow
  Purpose: Log handler that stores messages in a SQLite database using FireDAC

  Overview:
  This unit provides a database-based log handler that stores log messages
  in a SQLite database using FireDAC. It automatically creates the necessary
  tables and provides features for purging old logs and managing database
  connections.

  Database Schema:
  - LogMessages table with all standard log fields
  - LogActions table for action tracking
  - Automatic table creation on first use

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ----------------------------------------------------------------------------; }
unit Lazy.Log.SQLite;

interface

uses
  SysUtils, Classes, System.SyncObjs, Lazy.Log, Lazy.Types,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
  FireDAC.DApt, FireDAC.DApt.Intf, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin, FireDAC.Comp.Script,
  FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.Storage, FireDAC.Stan.StorageXML,
  Data.DB;

type
  /// <summary>
  /// Log handler that stores messages in a SQLite database
  /// </summary>
  TLZLogSQLite = class(TLZLogHandler)
  private
    FConnection: TFDConnection;
    FConnectionLock: TCriticalSection;
    FInitialized: Boolean;
    /// <summary>
    /// Initializes the database connection
    /// </summary>
    procedure InitializeDatabase;
    /// <summary>
    /// Creates the necessary database tables
    /// </summary>
    procedure CreateTables;
    /// <summary>
    /// Ensures the database is initialized before use
    /// </summary>
    procedure EnsureInitialized;
  protected
    /// <summary>
    /// Inserts a log message into the database
    /// </summary>
    procedure InsertLogMessage(ALogMessage: TLazyLogMessage);
    /// <summary>
    /// Inserts or updates a log action in the database
    /// </summary>
    procedure InsertLogAction(ALogMessage: TLazyLogMessage);
    procedure AfterAddHandler; override;
    procedure BeforeRemoveHandler; override;
    /// <summary>
    /// Logs database connection details
    /// </summary>
    procedure LogDBDetails;
  public
    /// <summary>
    /// Default database file name for new SQLite log handlers
    /// </summary>
    class var DatabaseFileName: string;
    /// <summary>
    /// Maximum number of log entries to keep before purging
    /// </summary>
    class var MaxLogEntries: Integer;
    /// <summary>
    /// Whether to automatically purge old logs
    /// </summary>
    class var AutoPurgeOldLogs: Boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Processes a log message and stores it in the database
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
    /// <summary>
    /// Purges log entries older than the specified number of days
    /// </summary>
    procedure PurgeOldLogs(AKeepDays: Integer = 30);
    /// <summary>
    /// Clears all log entries from the database
    /// </summary>
    procedure ClearAllLogs;
  end;

implementation

uses
  System.DateUtils;

{ TLZLogSQLite }

procedure TLZLogSQLite.AfterAddHandler;
begin
  inherited;
  LogDBDetails;
end;

procedure TLZLogSQLite.AfterConstruction;
begin
  inherited;
  FConnection := nil;
  FConnectionLock := TCriticalSection.Create;
  FInitialized := False;

  if DatabaseFileName = '' then
    DatabaseFileName := ChangeFileExt(ParamStr(0), '.log.db');
end;

procedure TLZLogSQLite.BeforeDestruction;
begin
  try
    FConnectionLock.Acquire;
    try
      if Assigned(FConnection) then
      begin
        if FConnection.Connected then
          FConnection.Connected := False;
        FreeAndNil(FConnection);
      end;
    finally
      FConnectionLock.Release;
    end;
    FreeAndNil(FConnectionLock);
  finally
    inherited;
  end;
end;

procedure TLZLogSQLite.BeforeRemoveHandler;
begin
  inherited;
  LogDBDetails;
end;

procedure TLZLogSQLite.EnsureInitialized;
begin
  if not FInitialized then
  begin
    FConnectionLock.Acquire;
    try
      if not FInitialized then
      begin
        InitializeDatabase;
        CreateTables;
        FInitialized := True;
      end;
    finally
      FConnectionLock.Release;
    end;
  end;
end;

procedure TLZLogSQLite.InitializeDatabase;
begin
  if not Assigned(FConnection) then
  begin
    FConnection := TFDConnection.Create(nil);
    FConnection.DriverName := 'SQLite';
    FConnection.Params.Values['Database'] := DatabaseFileName;
    FConnection.Params.Values['OpenMode'] := 'CreateUTF8';
    FConnection.Params.Values['LockingMode'] := 'Normal';
    FConnection.Params.Values['Synchronous'] := 'Normal';
    FConnection.Params.Values['JournalMode'] := 'WAL';
    FConnection.Params.Values['PageSize'] := '4096';
    FConnection.Params.Values['CacheSize'] := '10000';
    FConnection.Params.Values['TempStore'] := 'Memory';
    FConnection.LoginPrompt := False;
    FConnection.Connected := True;
  end;
end;

procedure TLZLogSQLite.CreateTables;
var
  LQuery: TFDQuery;
begin
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := FConnection;

    // Create LogMessages table
    LQuery.SQL.Text := 'CREATE TABLE IF NOT EXISTS LogMessages (' +
      '  ID INTEGER PRIMARY KEY AUTOINCREMENT, ' + '  ApplicationName TEXT, ' +
      '  DateTime TEXT, ' + '  ThreadID INTEGER, ' + '  LogClass TEXT, ' +
      '  LogType TEXT, ' + '  LogLevel TEXT, ' + '  LogProcedure TEXT, ' +
      '  LogMessage TEXT, ' + '  ErrorCode INTEGER, ' +
      '  ExceptionMessage TEXT, ' + '  ExceptionStackTrace TEXT' + ')';
    LQuery.ExecSQL;

    // Create LogActions table
    LQuery.SQL.Text := 'CREATE TABLE IF NOT EXISTS LogActions (' +
      '  ApplicationName TEXT NOT NULL, ' + '  ThreadID INTEGER NOT NULL, ' +
      '  ActionName TEXT NOT NULL, ' + '  LogClass TEXT, ' +
      '  StartDateTime TEXT, ' + '  LastUpdateDateTime TEXT, ' +
      '  ActionStatus TEXT, ' + '  ActionProgress INTEGER, ' +
      '  ActionMessage TEXT, ' +
      '  PRIMARY KEY (ApplicationName, ThreadID, ActionName)' + ')';
    LQuery.ExecSQL;

    // Create index on DateTime for faster purging
    LQuery.SQL.Text :=
      'CREATE INDEX IF NOT EXISTS idx_LogMessages_DateTime ON LogMessages(DateTime)';
    LQuery.ExecSQL;

    LQuery.SQL.Text :=
      'CREATE INDEX IF NOT EXISTS idx_LogActions_LastUpdate ON LogActions(LastUpdateDateTime)';
    LQuery.ExecSQL;
  finally
    LQuery.Free;
  end;
end;

procedure TLZLogSQLite.ProcessLogMessage(ALogMessage: TLazyLogMessage);
begin
  if not IsLogLevel(ALogMessage.LogLevel) then
    Exit;

  EnsureInitialized;

  FConnectionLock.Acquire;
  try
    if ALogMessage.LogType = ltProgress then
      InsertLogAction(ALogMessage)
    else
      InsertLogMessage(ALogMessage);

    // Auto-purge if enabled
    if AutoPurgeOldLogs and (Random(1000) = 0) then
      PurgeOldLogs(30);
  finally
    FConnectionLock.Release;
  end;
end;

procedure TLZLogSQLite.InsertLogMessage(ALogMessage: TLazyLogMessage);
var
  LQuery: TFDQuery;
  LLogType: string;
  LLogLevel: string;
  LExceptionMessage: string;
  LExceptionStackTrace: string;
begin
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := FConnection;

    // Convert enums to strings
    case ALogMessage.LogType of
      ltError:
        LLogType := 'ERROR';
      ltWarning:
        LLogType := 'WARNING';
      ltInformation:
        LLogType := 'INFORMATION';
      ltDebug:
        LLogType := 'DEBUG';
      ltProgress:
        LLogType := 'PROGRESS';
    else
      LLogType := 'UNKNOWN';
    end;

    LLogLevel := TLZLog.GetLogLevelText(ALogMessage.LogLevel);

    // Extract exception details
    if Assigned(ALogMessage.Exception) then
    begin
      LExceptionMessage := ALogMessage.Exception.Message;
      LExceptionStackTrace := ALogMessage.Exception.StackTrace;
    end
    else
    begin
      LExceptionMessage := '';
      LExceptionStackTrace := '';
    end;

    LQuery.SQL.Text := 'INSERT INTO LogMessages ' +
      '(ApplicationName, DateTime, ThreadID, LogClass, LogType, LogLevel, ' +
      'LogProcedure, LogMessage, ErrorCode, ExceptionMessage, ExceptionStackTrace) '
      + 'VALUES ' +
      '(:ApplicationName, :DateTime, :ThreadID, :LogClass, :LogType, :LogLevel, '
      + ':LogProcedure, :LogMessage, :ErrorCode, :ExceptionMessage, :ExceptionStackTrace)';

    LQuery.ParamByName('ApplicationName').AsString :=
      ALogMessage.ApplicationName;
    LQuery.ParamByName('DateTime').AsString :=
      FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ALogMessage.DateTime);
    LQuery.ParamByName('ThreadID').AsInteger := ALogMessage.ThreadID;
    LQuery.ParamByName('LogClass').AsString := ALogMessage.LogClass;
    LQuery.ParamByName('LogType').AsString := LLogType;
    LQuery.ParamByName('LogLevel').AsString := LLogLevel;
    LQuery.ParamByName('LogProcedure').AsString := ALogMessage.LogProcedure;
    LQuery.ParamByName('LogMessage').AsString := ALogMessage.LogMessage;
    LQuery.ParamByName('ErrorCode').AsInteger := ALogMessage.ErrorCode;
    LQuery.ParamByName('ExceptionMessage').AsString := LExceptionMessage;
    LQuery.ParamByName('ExceptionStackTrace').AsString := LExceptionStackTrace;

    LQuery.ExecSQL;
  finally
    LQuery.Free;
  end;
end;

procedure TLZLogSQLite.LogDBDetails;
begin
  Log('Database: %s', [DatabaseFileName]);
end;

procedure TLZLogSQLite.InsertLogAction(ALogMessage: TLazyLogMessage);
var
  LQuery: TFDQuery;
  LActionStatus: string;
begin
  LQuery := TFDQuery.Create(nil);
  try
    LQuery.Connection := FConnection;

    // Convert action status to string
    case ALogMessage.ActionStatus of
      asBegin:
        LActionStatus := 'BEGIN';
      asProgress:
        LActionStatus := 'PROGRESS';
      asEnd:
        LActionStatus := 'END';
    else
      LActionStatus := 'UNKNOWN';
    end;

    case ALogMessage.ActionStatus of
      asBegin:
        begin
          // INSERT new action record
          LQuery.SQL.Text := 'INSERT OR REPLACE INTO LogActions ' +
            '(ApplicationName, ThreadID, ActionName, LogClass, ' +
            'StartDateTime, LastUpdateDateTime, ActionStatus, ActionProgress, ActionMessage) '
            + 'VALUES ' +
            '(:ApplicationName, :ThreadID, :ActionName, :LogClass, ' +
            ':StartDateTime, :LastUpdateDateTime, :ActionStatus, :ActionProgress, :ActionMessage)';

          LQuery.ParamByName('ApplicationName').AsString :=
            ALogMessage.ApplicationName;
          LQuery.ParamByName('ThreadID').AsInteger := ALogMessage.ThreadID;
          LQuery.ParamByName('ActionName').AsString := ALogMessage.ActionName;
          LQuery.ParamByName('LogClass').AsString := ALogMessage.LogClass;
          LQuery.ParamByName('StartDateTime').AsString :=
            FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ALogMessage.DateTime);
          LQuery.ParamByName('LastUpdateDateTime').AsString :=
            FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ALogMessage.DateTime);
          LQuery.ParamByName('ActionStatus').AsString := LActionStatus;
          LQuery.ParamByName('ActionProgress').AsInteger :=
            ALogMessage.ActionProgress;
          LQuery.ParamByName('ActionMessage').AsString :=
            ALogMessage.LogMessage;

          LQuery.ExecSQL;
        end;
      asProgress:
        begin
          // UPDATE existing action record
          LQuery.SQL.Text := 'UPDATE LogActions ' +
            'SET LastUpdateDateTime = :LastUpdateDateTime, ' +
            '    ActionStatus = :ActionStatus, ' +
            '    ActionProgress = :ActionProgress, ' +
            '    ActionMessage = :ActionMessage ' +
            'WHERE ApplicationName = :ApplicationName ' +
            '  AND ThreadID = :ThreadID ' + '  AND ActionName = :ActionName';

          LQuery.ParamByName('LastUpdateDateTime').AsString :=
            FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ALogMessage.DateTime);
          LQuery.ParamByName('ActionStatus').AsString := LActionStatus;
          LQuery.ParamByName('ActionProgress').AsInteger :=
            ALogMessage.ActionProgress;
          LQuery.ParamByName('ActionMessage').AsString :=
            ALogMessage.LogMessage;
          LQuery.ParamByName('ApplicationName').AsString :=
            ALogMessage.ApplicationName;
          LQuery.ParamByName('ThreadID').AsInteger := ALogMessage.ThreadID;
          LQuery.ParamByName('ActionName').AsString := ALogMessage.ActionName;

          LQuery.ExecSQL;
        end;
      asEnd:
        begin
          // DELETE completed action record
          LQuery.SQL.Text := 'DELETE FROM LogActions ' +
            'WHERE ApplicationName = :ApplicationName ' +
            '  AND ThreadID = :ThreadID ' + '  AND ActionName = :ActionName';

          LQuery.ParamByName('ApplicationName').AsString :=
            ALogMessage.ApplicationName;
          LQuery.ParamByName('ThreadID').AsInteger := ALogMessage.ThreadID;
          LQuery.ParamByName('ActionName').AsString := ALogMessage.ActionName;

          LQuery.ExecSQL;
        end;
    end;
  finally
    LQuery.Free;
  end;
end;

procedure TLZLogSQLite.PurgeOldLogs(AKeepDays: Integer = 30);
var
  LQuery: TFDQuery;
  LCutoffDate: TDateTime;
begin
  FConnectionLock.Acquire;
  try
    EnsureInitialized;

    LCutoffDate := IncDay(Now, -AKeepDays);
    LQuery := TFDQuery.Create(nil);
    try
      LQuery.Connection := FConnection;

      // Delete old log messages
      LQuery.SQL.Text := 'DELETE FROM LogMessages WHERE DateTime < :CutoffDate';
      LQuery.ParamByName('CutoffDate').AsString :=
        FormatDateTime('yyyy-mm-dd hh:nn:ss', LCutoffDate);
      LQuery.ExecSQL;

      // Delete stale log actions (last update too old - likely orphaned)
      LQuery.SQL.Text :=
        'DELETE FROM LogActions WHERE LastUpdateDateTime < :CutoffDate';
      LQuery.ParamByName('CutoffDate').AsString :=
        FormatDateTime('yyyy-mm-dd hh:nn:ss', LCutoffDate);
      LQuery.ExecSQL;
    finally
      LQuery.Free;
    end;
  finally
    FConnectionLock.Release;
  end;
end;

procedure TLZLogSQLite.ClearAllLogs;
var
  LQuery: TFDQuery;
begin
  FConnectionLock.Acquire;
  try
    EnsureInitialized;

    LQuery := TFDQuery.Create(nil);
    try
      LQuery.Connection := FConnection;

      // Clear all log messages
      LQuery.SQL.Text := 'DELETE FROM LogMessages';
      LQuery.ExecSQL;

      // Clear all log actions
      LQuery.SQL.Text := 'DELETE FROM LogActions';
      LQuery.ExecSQL;

      // Vacuum to reclaim space
      LQuery.SQL.Text := 'VACUUM';
      LQuery.ExecSQL;
    finally
      LQuery.Free;
    end;
  finally
    FConnectionLock.Release;
  end;
end;

initialization

TLZLogSQLite.DatabaseFileName := '';
TLZLogSQLite.MaxLogEntries := 100000;
TLZLogSQLite.AutoPurgeOldLogs := True;

end.
