{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log
  Author: Tristan Marlow
  Purpose: Multi-handler logging system that dispatches log messages to
  registered handlers (FileStream, Console, Database, etc.)

  Overview:
  This unit provides a comprehensive logging framework with support for multiple
  output handlers, progress tracking, and enhanced exception reporting. The
  system is designed to be thread-safe and extensible, allowing applications
  to easily add custom logging handlers while maintaining consistent logging
  behavior across the application.

  Features:
  - Multiple logging handlers can be registered
  - Progress tracking with LogActionBegin/Progress/End
  - Thread-safe handler management
  - Simple dispatcher pattern
  - Enhanced exception logging with detailed context (via Lazy.Exception.Details)
  - Stack trace support through extensible exception handlers

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ----------------------------------------------------------------------------; }

unit Lazy.Log;

interface

uses
  SysUtils, Classes, Lazy.Types, System.SyncObjs, System.Generics.Collections;

type
  // Forward declarations
  ELZLogException = class(Exception);
  TLZLogHandler = class;
  TLZLog = class;

  TLazyLogType = (ltError, ltWarning, ltInformation, ltDebug, ltProgress);
  TLazyLogActionStatus = (asBegin, asProgress, asEnd);

  TLazyLogAction = class
  private
    FThreadID: Cardinal;
    FActionName: string;
    FActionProgress: integer;
    FActionStatus: TLazyLogActionStatus;
    FActionMessage: string;
    FLastUpdateDateTime: TDateTime;
    FStartDateTime: TDateTime;
  public
    /// <summary>
    /// Creates a new log action instance
    /// </summary>
    constructor Create;
    /// <summary>
    /// Returns a unique key for this action based on thread ID and action name
    /// </summary>
    function GetActionKey: string;
    /// <summary>
    /// Returns the duration of the action as a formatted string
    /// </summary>
    function GetDuration: string;
    /// <summary>
    /// Thread ID where this action is running
    /// </summary>
    property ThreadID: Cardinal read FThreadID write FThreadID;
    /// <summary>
    /// Name of the action being tracked
    /// </summary>
    property ActionName: string read FActionName write FActionName;
    /// <summary>
    /// Progress percentage (0-100) of the action
    /// </summary>
    property ActionProgress: integer read FActionProgress write FActionProgress;
    /// <summary>
    /// Current status of the action (Begin, Progress, End)
    /// </summary>
    property ActionStatus: TLazyLogActionStatus read FActionStatus
      write FActionStatus;
    /// <summary>
    /// Optional message describing the current state of the action
    /// </summary>
    property ActionMessage: string read FActionMessage write FActionMessage;
    /// <summary>
    /// Last time this action was updated
    /// </summary>
    property LastUpdateDateTime: TDateTime read FLastUpdateDateTime
      write FLastUpdateDateTime;
    /// <summary>
    /// When this action was started
    /// </summary>
    property StartDateTime: TDateTime read FStartDateTime write FStartDateTime;
  end;

  TLazyLogMessage = class
  private
    FApplicationName: string;
    FDateTime: TDateTime;
    FThreadID: Cardinal;
    FLogClass: string;
    FLogType: TLazyLogType;
    FLogMessage: string;
    FLogProcedure: string;
    FLogLevel: TLZLogLevel;
    FErrorCode: integer;
    FException: Exception;
    FActionName: string;
    FActionStatus: TLazyLogActionStatus;
    FActionProgress: integer;
    FStackTrace: string;
  public
    /// <summary>
    /// Creates a new log message instance
    /// </summary>
    constructor Create;
    /// <summary>
    /// Name of the application generating the log message
    /// </summary>
    property ApplicationName: string read FApplicationName
      write FApplicationName;
    /// <summary>
    /// Timestamp when the log message was created
    /// </summary>
    property DateTime: TDateTime read FDateTime write FDateTime;
    /// <summary>
    /// Thread ID where the log message was generated
    /// </summary>
    property ThreadID: Cardinal read FThreadID write FThreadID;
    /// <summary>
    /// Class name of the object generating the log message
    /// </summary>
    property LogClass: string read FLogClass write FLogClass;
    /// <summary>
    /// Type of log message (Error, Warning, Information, Debug, Progress)
    /// </summary>
    property LogType: TLazyLogType read FLogType write FLogType;
    /// <summary>
    /// The actual log message text
    /// </summary>
    property LogMessage: string read FLogMessage write FLogMessage;
    /// <summary>
    /// Name of the procedure/method generating the log message
    /// </summary>
    property LogProcedure: string read FLogProcedure write FLogProcedure;
    /// <summary>
    /// Log level (Debug, Information, Warning, Error)
    /// </summary>
    property LogLevel: TLZLogLevel read FLogLevel write FLogLevel;
    /// <summary>
    /// Error code associated with the log message
    /// </summary>
    property ErrorCode: integer read FErrorCode write FErrorCode;
    /// <summary>
    /// Exception object associated with error log messages
    /// </summary>
    property Exception: Exception read FException write FException;
    /// <summary>
    /// Stack trace information for error messages
    /// </summary>
    property StackTrace: string read FStackTrace write FStackTrace;
    /// <summary>
    /// Name of the action being tracked (for progress messages)
    /// </summary>
    property ActionName: string read FActionName write FActionName;
    /// <summary>
    /// Status of the action (Begin, Progress, End)
    /// </summary>
    property ActionStatus: TLazyLogActionStatus read FActionStatus
      write FActionStatus;
    /// <summary>
    /// Progress percentage of the action (0-100)
    /// </summary>
    property ActionProgress: integer read FActionProgress write FActionProgress;
  end;

  // Base class for all log handlers
  TLZLogHandler = class(TObject)
  private
    FLogLevel: TLZLogLevel;
    FName: string;
    FAllLogLevels: Boolean;
    procedure SetLogLevel(const AValue: TLZLogLevel);
  protected
    /// <summary>
    /// Checks if the specified log level meets the handler's threshold
    /// </summary>
    function IsLogLevel(ALogLevel: TLZLogLevel): Boolean;
    /// <summary>
    /// Called after the handler is added to the log system
    /// </summary>
    procedure AfterAddHandler; virtual;
    /// <summary>
    /// Called before the handler is removed from the log system
    /// </summary>
    procedure BeforeRemoveHandler; virtual;
    /// <summary>
    /// Logs a message using the global log system
    /// </summary>
    procedure Log(AMessage: string); overload;
    /// <summary>
    /// Logs a formatted message using the global log system
    /// </summary>
    procedure Log(AAMessage: string; const Args: array of const); overload;

  public
    /// <summary>
    /// Creates a new log handler instance
    /// </summary>
    constructor Create; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Primary logging method - handlers should override this to process log messages
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); virtual;
      abstract;
    /// <summary>
    /// Formats a log message for output, optionally prefixing with log level
    /// </summary>
    function GetFormattedMessage(ALogMessage: TLazyLogMessage;
      APrefixLogLevel: Boolean = true): string; virtual;
    /// <summary>
    /// Minimum log level that this handler will process
    /// </summary>
    property LogLevel: TLZLogLevel read FLogLevel write SetLogLevel;
    /// <summary>
    /// Name identifier for this handler
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// When True, all log messages are sent to this handler regardless of log level.
    /// When False, only messages meeting the LogLevel threshold are sent (performance optimization).
    /// Default: False
    /// </summary>
    property AllLogLevels: Boolean read FAllLogLevels write FAllLogLevels;
  end;

  TLZLogHandlerClass = class of TLZLogHandler;
  TLZLogHandlerList = TObjectList<TLZLogHandler>;
  TLazyLogActionsDictionary = TObjectDictionary<string, TLazyLogAction>;

  // Main log dispatcher class
  TLZLog = class(TObject)
  private
    FLogLevel: TLZLogLevel;
    FHandlers: TLZLogHandlerList;
    FHandlersLock: TCriticalSection;
    FActions: TLazyLogActionsDictionary;
    FActionsLock: TCriticalSection;
    procedure SetLogLevel(const Value: TLZLogLevel);
    procedure UpdateAction(ALogMessage: TLazyLogMessage);
    function GetActionKey(AThreadID: Cardinal; AActionName: string): string;
  protected
    /// <summary>
    /// Checks if the specified log level meets the system threshold
    /// </summary>
    function IsLogLevel(ALogLevel: TLZLogLevel): Boolean;
    /// <summary>
    /// Checks if logging is available, optionally checking for handlers
    /// </summary>
    function IsLogAvailable(var AMessage: string; ACheckHandles: Boolean = true)
      : Boolean; overload;
    /// <summary>
    /// Checks if logging is available, optionally checking for handlers
    /// </summary>
    function IsLogAvailable(ACheckHandles: Boolean = true): Boolean; overload;
    /// <summary>
    /// Asserts that logging is available, raising an exception if not
    /// </summary>
    procedure AssertLogAvilable(ACheckHandles: Boolean = true);
  public
    /// <summary>
    /// Name of the application using the logging system
    /// </summary>
    class var ApplicationName: string;
    /// <summary>
    /// Creates a new log dispatcher instance
    /// </summary>
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// Returns a text representation of the specified log level
    /// </summary>
    class function GetLogLevelText(ALogLevel: TLZLogLevel): string;
    // Handler management
    /// <summary>
    /// Adds a new log handler with the specified name and class
    /// </summary>
    function AddHandler(AName: string; AHandlerClass: TLZLogHandlerClass)
      : TLZLogHandler;
    /// <summary>
    /// Removes a log handler by name
    /// </summary>
    procedure RemoveHandler(AName: string);
    /// <summary>
    /// Finds a log handler by name
    /// </summary>
    function FindHandler(AName: string): TLZLogHandler;
    /// <summary>
    /// Removes all log handlers
    /// </summary>
    procedure ClearHandlers;
    /// <summary>
    /// Dispatches a log message to all registered handlers
    /// </summary>
    procedure LogMessageToHandlers(ALogMessage: TLazyLogMessage);
    // Action tracking
    /// <summary>
    /// Gets a specific action by thread ID and action name
    /// </summary>
    function GetAction(AThreadID: Cardinal; AActionName: string)
      : TLazyLogAction;
    /// <summary>
    /// Gets all currently tracked actions
    /// </summary>
    function GetAllActions: TArray<TLazyLogAction>;
    /// <summary>
    /// Gets all actions for the current thread
    /// </summary>
    function GetCurrentThreadActions: TArray<TLazyLogAction>;

    // Standard logging methods - dispatch to all handlers
    /// <summary>
    /// Logs an information message with an object sender
    /// </summary>
    procedure Log(ASender: TObject; AMessage: string); overload; virtual;
    /// <summary>
    /// Logs a formatted information message with an object sender
    /// </summary>
    procedure Log(ASender: TObject; AMessage: string;
      const Args: array of const); overload; virtual;
    /// <summary>
    /// Logs an information message with a string sender
    /// </summary>
    procedure Log(ASender: string; AMessage: string); overload; virtual;
    /// <summary>
    /// Logs a debug message with procedure information
    /// </summary>
    procedure Debug(ASender: TObject; AProcedure: string; AMessage: string);
      overload; virtual;
    /// <summary>
    /// Logs a formatted debug message with procedure information
    /// </summary>
    procedure Debug(ASender: TObject; AProcedure: string; AMessage: string;
      const Args: array of const); overload; virtual;
    /// <summary>
    /// Logs a warning message
    /// </summary>
    procedure Warning(ASender: TObject; AMessage: string); overload; virtual;
    /// <summary>
    /// Logs a formatted warning message
    /// </summary>
    procedure Warning(ASender: TObject; AMessage: string;
      const Args: array of const); overload; virtual;
    /// <summary>
    /// Logs an error message with optional error code
    /// </summary>
    procedure Error(ASender: TObject; AMessage: string;
      AErrorCode: integer = 0); overload; virtual;
    /// <summary>
    /// Logs a formatted error message with optional error code
    /// </summary>
    procedure Error(ASender: TObject; AMessage: string;
      const Args: array of const; AErrorCode: integer = 0); overload; virtual;
    /// <summary>
    /// Logs an exception with optional custom message
    /// </summary>
    procedure Error(ASender: TObject; AException: Exception;
      AMessage: string = ''); overload; virtual;
    /// <summary>
    /// Logs an exception with formatted custom message
    /// </summary>
    procedure Error(ASender: TObject; AException: Exception; AMessage: string;
      const Args: array of const); overload; virtual;
    /// <summary>
    /// Logs an exception with string sender and optional custom message
    /// </summary>
    procedure Error(ASender: string; AException: Exception;
      AMessage: string = ''); overload; virtual;
    /// <summary>
    /// Logs an error message with string sender
    /// </summary>
    procedure Error(ASender: string; AMessage: string = ''); overload; virtual;
    // Progress tracking methods - dispatch to all handlers
    /// <summary>
    /// Begins tracking a new action with optional status message
    /// </summary>
    procedure LogActionBegin(ASender: TObject; AActionName: string;
      AStatus: string = '');
    /// <summary>
    /// Ends tracking of an action
    /// </summary>
    procedure LogActionEnd(ASender: TObject; AActionName: string);
    /// <summary>
    /// Updates the progress of a tracked action
    /// </summary>
    procedure LogActionProgress(ASender: TObject; AActionName: string;
      AProgress: integer; AStatus: string = '');
    /// <summary>
    /// Minimum log level that messages must meet to be processed
    /// </summary>
    property LogLevel: TLZLogLevel read FLogLevel write SetLogLevel;
    /// <summary>
    /// List of all registered log handlers
    /// </summary>
    property Handlers: TLZLogHandlerList read FHandlers;
  end;

  TLZLogClass = class of TLZLog;

var
  _LazyLog: TLZLog;
  _LazyLogClass: TLZLogClass;

/// <summary>
/// Sets the custom log class to use for the global log instance
/// </summary>
procedure SetLazyLogClass(ALazyLogClass: TLZLogClass);
/// <summary>
/// Returns the default log level based on build configuration
/// </summary>
function DefaultLazyLogLevel: TLZLogLevel;
/// <summary>
/// Returns the global log instance, creating it if necessary
/// </summary>
function LazyLog: TLZLog;
/// <summary>
/// Returns the cached log text from the memory handler
/// </summary>
function LazyLogCache: string;

/// <summary>
/// Logs an information message with an object sender
/// </summary>
procedure Log(ASender: TObject; AMessage: string); overload;
/// <summary>
/// Logs a formatted information message with an object sender
/// </summary>
procedure Log(ASender: TObject; AMessage: string;
  const Args: array of const); overload;
/// <summary>
/// Logs an information message with a string sender
/// </summary>
procedure Log(ASender: string; AMessage: string); overload;
/// <summary>
/// Logs a debug message with procedure information
/// </summary>
procedure Debug(ASender: TObject; AProcedure: string;
  AMessage: string); overload;
/// <summary>
/// Logs a formatted debug message with procedure information
/// </summary>
procedure Debug(ASender: TObject; AProcedure: string; AMessage: string;
  const Args: array of const); overload;
/// <summary>
/// Logs a warning message
/// </summary>
procedure Warning(ASender: TObject; AMessage: string); overload;
/// <summary>
/// Logs a formatted warning message
/// </summary>
procedure Warning(ASender: TObject; AMessage: string;
  const Args: array of const); overload;
/// <summary>
/// Logs an error message with optional error code
/// </summary>
procedure Error(ASender: TObject; AMessage: string;
  AErrorCode: integer = 0); overload;
/// <summary>
/// Logs a formatted error message with optional error code
/// </summary>
procedure Error(ASender: TObject; AMessage: string; const Args: array of const;
  AErrorCode: integer = 0); overload;
/// <summary>
/// Logs an exception with optional custom message
/// </summary>
procedure Error(ASender: TObject; AException: Exception;
  AMessage: string = ''); overload;
/// <summary>
/// Logs an exception with formatted custom message
/// </summary>
procedure Error(ASender: TObject; AException: Exception; AMessage: string;
  const Args: array of const); overload;
/// <summary>
/// Logs an exception with string sender and optional custom message
/// </summary>
procedure Error(ASender: string; AException: Exception;
  AMessage: string = ''); overload;
/// <summary>
/// Logs an error message with string sender
/// </summary>
procedure Error(ASender: string; AMessage: string = ''); overload;

implementation

uses
  Lazy.Log.Memory,
  Lazy.Exception.Details;

{ TLazyLogAction }

constructor TLazyLogAction.Create;
begin
  inherited;
  FStartDateTime := Now;
  FLastUpdateDateTime := Now;
  FThreadID := TThread.CurrentThread.ThreadID;
  FActionProgress := 0;
  FActionStatus := asBegin;
end;

function TLazyLogAction.GetActionKey: string;
begin
  Result := Format('%d_%s', [FThreadID, FActionName]);
end;

function TLazyLogAction.GetDuration: string;
var
  LDuration: TDateTime;
begin
  LDuration := Now - FStartDateTime;
  Result := FormatDateTime('hh:nn:ss', LDuration);
end;

{ TLazyLogMessage }

constructor TLazyLogMessage.Create;
begin
  inherited;
  FDateTime := Now;
  FThreadID := TThread.CurrentThread.ThreadID;
  FErrorCode := 0;
  FActionProgress := 0;
end;

{ TLZLogHandler }

constructor TLZLogHandler.Create;
begin
  inherited Create;
   FLogLevel := DefaultLazyLogLevel;
  FAllLogLevels := False;
end;

procedure TLZLogHandler.AfterAddHandler;
begin

end;

procedure TLZLogHandler.AfterConstruction;
begin
  inherited;
end;

procedure TLZLogHandler.BeforeDestruction;
begin
  inherited;
end;

procedure TLZLogHandler.BeforeRemoveHandler;
begin

end;

procedure TLZLogHandler.SetLogLevel(

  const AValue: TLZLogLevel);
begin
  FLogLevel := AValue;
end;

function TLZLogHandler.IsLogLevel(ALogLevel: TLZLogLevel): Boolean;
begin
  Result := (ord(ALogLevel) <= ord(FLogLevel));
end;

procedure TLZLogHandler.Log(AAMessage: string;

  const Args: array of

  const);
begin
  Log(Format(AAMessage, Args));
end;

procedure TLZLogHandler.Log(AMessage: string);
begin
  try
    LazyLog.Log(Self, AMessage);
  except
  end;
end;

function TLZLogHandler.GetFormattedMessage(ALogMessage: TLazyLogMessage;
  APrefixLogLevel: Boolean): string;
begin
  Result := '';
  if not IsLogLevel(ALogMessage.LogLevel) then
    Exit;

  // Format the message based on the log type
  case ALogMessage.LogType of
    ltDebug:
      begin
        if ALogMessage.LogProcedure <> '' then
          Result := Format('[%s] (%s) %s', [ALogMessage.LogClass,
            ALogMessage.LogProcedure, ALogMessage.LogMessage])
        else
          Result := Format('[%s] %s', [ALogMessage.LogClass,
            ALogMessage.LogMessage]);
      end;
    ltError:
      begin
        if Assigned(ALogMessage.Exception) then
        begin
          // Use the enhanced log message which already contains exception details
          if ALogMessage.LogMessage <> '' then
            Result := Format('[%s] %s', [ALogMessage.LogClass,
              ALogMessage.LogMessage])
          else
            Result := ALogMessage.Exception.Message;
        end
        else if ALogMessage.ErrorCode <> 0 then
          Result := Format('[%s] (%d) %s', [ALogMessage.LogClass,
            ALogMessage.ErrorCode, ALogMessage.LogMessage])
        else
          Result := ALogMessage.LogMessage;
      end;
    ltProgress:
      begin
        case ALogMessage.ActionStatus of
          asBegin:
            begin
              if ALogMessage.LogMessage <> '' then
                Result := Format('[%s] [ACTION BEGIN] %s - %s',
                  [ALogMessage.LogClass, ALogMessage.ActionName,
                  ALogMessage.LogMessage])
              else
                Result := Format('[%s] [ACTION BEGIN] %s',
                  [ALogMessage.LogClass, ALogMessage.ActionName]);
            end;
          asEnd:
            begin
              Result := Format('[%s] [ACTION END] %s',
                [ALogMessage.LogClass, ALogMessage.ActionName]);
            end;
          asProgress:
            begin

            end;
        end;
      end;
  else
    Result := Format('[%s] %s', [ALogMessage.LogClass, ALogMessage.LogMessage]);
  end;
  if (APrefixLogLevel) and (Trim(Result) <> '') then
  begin
    Result := TLZLog.GetLogLevelText(ALogMessage.LogLevel) + ' ' + Result;
  end;

end;

{ TLZLog }

constructor TLZLog.Create;
begin
  inherited;
{$IFDEF DEBUG}
  FLogLevel := logDebug;
{$ELSE}
  FLogLevel := logInformation;
{$ENDIF}
  FHandlers := TLZLogHandlerList.Create(true);
  FHandlersLock := TCriticalSection.Create;
  FActions := TLazyLogActionsDictionary.Create([doOwnsValues]);
  FActionsLock := TCriticalSection.Create;
end;

destructor TLZLog.Destroy;
begin
  try
    FHandlersLock.Acquire;
    try
      FreeAndNil(FHandlers);
    finally
      FHandlersLock.Release;
    end;
    FreeAndNil(FHandlersLock);

    FActionsLock.Acquire;
    try
      FreeAndNil(FActions);
    finally
      FActionsLock.Release;
    end;
    FreeAndNil(FActionsLock);
  finally
    inherited;
  end;
end;

procedure TLZLog.SetLogLevel(

  const Value: TLZLogLevel);
var
  LHandler: TLZLogHandler;
begin
  FLogLevel := Value;
  // Update all handlers with the new log level
  FHandlersLock.Acquire;
  try
    for LHandler in FHandlers do
    begin
      LHandler.LogLevel := Value;
    end;
  finally
    FHandlersLock.Release;
  end;
  Debug(Self, 'SetLogLevel', Format('Log level is now %s',
    [GetLogLevelText(FLogLevel)]));
end;

function TLZLog.IsLogAvailable(

  var AMessage: string; ACheckHandles: Boolean = true): Boolean;
begin
  Result := (Trim(ApplicationName) <> '');
  if Result then
  begin
    if ACheckHandles and (FHandlers.Count = 0) then
    begin
      AMessage := 'No log handlers are availabe. Use LazyLog.AddHandler';
      Result := False;
    end;
  end
  else
  begin
    AMessage :=
      'No application name has been specified, set LazyLog.ApplicationName';
  end;

end;

function TLZLog.IsLogAvailable(ACheckHandles: Boolean): Boolean;
var
  LMessage: string;
begin
  Result := IsLogAvailable(LMessage, ACheckHandles);
end;

function TLZLog.IsLogLevel(ALogLevel: TLZLogLevel): Boolean;
begin
  Result := (ord(ALogLevel) <= ord(FLogLevel));
end;

class function TLZLog.GetLogLevelText(ALogLevel: TLZLogLevel): string;
begin
  case ALogLevel of
    logDebug:
      Result := 'DEBUG';
    logInformation:
      Result := 'INFO';
    logWarning:
      Result := 'WARN';
  else
    begin
      Result := 'ERROR';
    end;
  end;
end;

function TLZLog.AddHandler(AName: string; AHandlerClass: TLZLogHandlerClass)
  : TLZLogHandler;
var
  LHandler: TLZLogHandler;
begin
  Result := nil;
  if not Assigned(AHandlerClass) then
    Exit;

  FHandlersLock.Acquire;
  try
    // Check if handler already exists
    LHandler := FindHandler(AName);
    if Assigned(LHandler) then
    begin
      Result := LHandler;
      Exit;
    end;

    // Create new handler
    LHandler := AHandlerClass.Create;
    LHandler.Name := AName;
    LHandler.LogLevel := FLogLevel;
    FHandlers.Add(LHandler);
    try
      LHandler.AfterAddHandler;
    except
    end;
    Result := LHandler;
  finally
    FHandlersLock.Release;
  end;
end;

procedure TLZLog.RemoveHandler(AName: string);
var
  LHandler: TLZLogHandler;
begin
  FHandlersLock.Acquire;
  try
    LHandler := FindHandler(AName);
    if Assigned(LHandler) then
    begin
      try
        LHandler.BeforeRemoveHandler;
      except
      end;
      FHandlers.Remove(LHandler);
    end;
  finally
    FHandlersLock.Release;
  end;
end;

function TLZLog.FindHandler(AName: string): TLZLogHandler;
var
  LHandler: TLZLogHandler;
begin
  Result := nil;
  // Note: This assumes we're already in a lock
  for LHandler in FHandlers do
  begin
    if SameText(LHandler.Name, AName) then
    begin
      Result := LHandler;
      Exit;
    end;
  end;
end;

procedure TLZLog.AssertLogAvilable(ACheckHandles: Boolean);
var
  LMessage: string;
begin
  Assert(IsLogAvailable(LMessage, ACheckHandles), LMessage);
end;

procedure TLZLog.ClearHandlers;
begin
  FHandlersLock.Acquire;
  try
    FHandlers.Clear;
  finally
    FHandlersLock.Release;
  end;
end;

procedure TLZLog.LogMessageToHandlers(ALogMessage: TLazyLogMessage);
var
  LHandler: TLZLogHandler;
  LHandlersCopy: TArray<TLZLogHandler>;
  LIndex: integer;
  LCount: integer;
begin
  // Quick exit if no handlers
  if FHandlers.Count = 0 then
    Exit;

  // Create a snapshot of handlers to minimize lock time
  FHandlersLock.Acquire;
  try
    LCount := FHandlers.Count;
    SetLength(LHandlersCopy, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LHandlersCopy[LIndex] := FHandlers[LIndex];
    end;
  finally
    FHandlersLock.Release;
  end;

  // Process handlers without holding the lock
  for LIndex := 0 to LCount - 1 do
  begin
    LHandler := LHandlersCopy[LIndex];
    try
      // Performance optimization: check log level before processing
      // Skip if handler doesn't want all levels and message doesn't meet threshold
      if LHandler.AllLogLevels or LHandler.IsLogLevel(ALogMessage.LogLevel) then
      begin
        LHandler.ProcessLogMessage(ALogMessage);
      end;
    except
      // Suppress handler exceptions to prevent one handler from affecting others
    end;
  end;
end;

procedure TLZLog.Log(ASender: TObject; AMessage: string);
var
  LClassname: string;
begin
  LClassname := '';
  if Assigned(ASender) then
    LClassname := ASender.ClassName;
  Log(LClassname, AMessage);
end;

procedure TLZLog.Log(ASender: TObject; AMessage: string;

  const Args: array of

  const);
begin
  Log(ASender, Format(AMessage, Args));
end;

procedure TLZLog.Log(ASender, AMessage: string);
var
  LLogMessage: TLazyLogMessage;
begin
  LLogMessage := TLazyLogMessage.Create;
  try
    LLogMessage.ApplicationName := ApplicationName;
    LLogMessage.LogType := ltInformation;
    LLogMessage.LogLevel := logInformation;
    LLogMessage.LogClass := ASender;
    LLogMessage.LogMessage := AMessage;

    LogMessageToHandlers(LLogMessage);
  finally
    LLogMessage.Free;
  end;

end;

procedure TLZLog.Debug(ASender: TObject; AProcedure: string; AMessage: string);
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltDebug;
      LLogMessage.LogLevel := logDebug;
      if Assigned(ASender) then
        LLogMessage.LogClass := ASender.ClassName;
      LLogMessage.LogProcedure := AProcedure;
      LLogMessage.LogMessage := AMessage;

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

procedure TLZLog.Debug(ASender: TObject; AProcedure: string; AMessage: string;

  const Args: array of

  const);
begin
  Debug(ASender, AProcedure, Format(AMessage, Args));
end;

procedure TLZLog.Warning(ASender: TObject; AMessage: string);
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltWarning;
      LLogMessage.LogLevel := logWarning;
      if Assigned(ASender) then
        LLogMessage.LogClass := ASender.ClassName;
      LLogMessage.LogMessage := AMessage;

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

procedure TLZLog.Warning(ASender: TObject; AMessage: string;

  const Args: array of

  const);
begin
  Warning(ASender, Format(AMessage, Args));
end;

procedure TLZLog.Error(ASender: TObject; AMessage: string;
  AErrorCode: integer = 0);
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltError;
      LLogMessage.LogLevel := logError;
      if Assigned(ASender) then
        LLogMessage.LogClass := ASender.ClassName;
      LLogMessage.LogMessage := AMessage;
      LLogMessage.ErrorCode := AErrorCode;

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

procedure TLZLog.Error(ASender: TObject; AMessage: string;
  const Args: array of const; AErrorCode: integer = 0);
begin
  Error(ASender, Format(AMessage, Args), AErrorCode);
end;

procedure TLZLog.Error(ASender: TObject; AException: Exception;
  AMessage: string = '');
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
  LExceptionDetails: TLZExceptionDetails;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltError;
      LLogMessage.LogLevel := logError;
      if Assigned(ASender) then
        LLogMessage.LogClass := ASender.ClassName;
      LLogMessage.Exception := AException;
      LLogMessage.LogMessage := AMessage;

      // Extract enhanced exception details
      if Assigned(AException) then
      begin
        LExceptionDetails := TLZExceptionDetails.Create(AException, ASender,
          ExceptAddr);
        try
          // Populate stack trace from exception details
          LLogMessage.StackTrace := LExceptionDetails.StackTrace;

          // If no custom message was provided, use the detailed exception information
          if Trim(AMessage) = '' then
          begin
            LLogMessage.LogMessage := LExceptionDetails.AsString;
          end
          else
          begin
            // Append exception details to custom message
            LLogMessage.LogMessage := AMessage + #13#10#13#10 +
              LExceptionDetails.AsString;
          end;
        finally
          LExceptionDetails.Free;
        end;
      end;

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

procedure TLZLog.Error(ASender: TObject; AException: Exception;
  AMessage: string; const Args: array of const);
begin
  Error(ASender, AException, Format(AMessage, Args));
end;

procedure TLZLog.Error(ASender: string; AException: Exception;
  AMessage: string);
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
  LExceptionDetails: TLZExceptionDetails;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltError;
      LLogMessage.LogLevel := logError;
      LLogMessage.LogClass := ASender;
      LLogMessage.Exception := AException;
      LLogMessage.LogMessage := AMessage;

      // Extract enhanced exception details
      if Assigned(AException) then
      begin
        LExceptionDetails := TLZExceptionDetails.Create(AException, nil,
          ExceptAddr);
        try
          // Populate stack trace from exception details
          LLogMessage.StackTrace := LExceptionDetails.StackTrace;

          // If no custom message was provided, use the detailed exception information
          if Trim(AMessage) = '' then
          begin
            LLogMessage.LogMessage := LExceptionDetails.AsString;
          end
          else
          begin
            // Append exception details to custom message
            LLogMessage.LogMessage := AMessage + #13#10#13#10 +
              LExceptionDetails.AsString;
          end;
        finally
          LExceptionDetails.Free;
        end;
      end;

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

procedure TLZLog.Error(ASender: string; AMessage: string);
begin
  Error(nil, AMessage);
end;

procedure TLZLog.LogActionBegin(ASender: TObject; AActionName: string;
  AStatus: string = '');
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltProgress;
      LLogMessage.LogLevel := logInformation;
      if Assigned(ASender) then
        LLogMessage.LogClass := ASender.ClassName;
      LLogMessage.ActionName := AActionName;
      LLogMessage.ActionStatus := asBegin;
      LLogMessage.ActionProgress := 0;
      LLogMessage.LogMessage := AStatus;

      // Track action
      UpdateAction(LLogMessage);

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

procedure TLZLog.LogActionEnd(ASender: TObject; AActionName: string);
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltProgress;
      LLogMessage.LogLevel := logInformation;
      if Assigned(ASender) then
        LLogMessage.LogClass := ASender.ClassName;
      LLogMessage.ActionName := AActionName;
      LLogMessage.ActionStatus := asEnd;
      LLogMessage.ActionProgress := 100;

      // Track action (will remove it)
      UpdateAction(LLogMessage);

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

procedure TLZLog.LogActionProgress(ASender: TObject; AActionName: string;
  AProgress: integer; AStatus: string = '');
var
  LHandler: TLZLogHandler;
  LLogMessage: TLazyLogMessage;
begin
  if IsLogAvailable then
  begin
    LLogMessage := TLazyLogMessage.Create;
    try
      LLogMessage.ApplicationName := ApplicationName;
      LLogMessage.LogType := ltProgress;
      LLogMessage.LogLevel := logInformation;
      if Assigned(ASender) then
        LLogMessage.LogClass := ASender.ClassName;
      LLogMessage.ActionName := AActionName;
      LLogMessage.ActionStatus := asProgress;
      LLogMessage.ActionProgress := AProgress;
      LLogMessage.LogMessage := AStatus;

      // Track action
      UpdateAction(LLogMessage);

      FHandlersLock.Acquire;
      try
        for LHandler in FHandlers do
        begin
          try
            LHandler.ProcessLogMessage(LLogMessage);
          except
            // Suppress handler exceptions
          end;
        end;
      finally
        FHandlersLock.Release;
      end;
    finally
      LLogMessage.Free;
    end;
  end;
end;

function TLZLog.GetActionKey(AThreadID: Cardinal; AActionName: string): string;
begin
  Result := Format('%d_%s', [AThreadID, AActionName]);
end;

procedure TLZLog.UpdateAction(ALogMessage: TLazyLogMessage);
var
  LKey: string;
  LAction: TLazyLogAction;
begin
  if not IsLogAvailable then
    Exit;

  if ALogMessage.LogType <> ltProgress then
    Exit;

  LKey := GetActionKey(ALogMessage.ThreadID, ALogMessage.ActionName);

  FActionsLock.Acquire;
  try
    case ALogMessage.ActionStatus of
      asBegin:
        begin
          // Create or update action
          if not FActions.TryGetValue(LKey, LAction) then
          begin
            LAction := TLazyLogAction.Create;
            FActions.Add(LKey, LAction);
          end;
          LAction.ThreadID := ALogMessage.ThreadID;
          LAction.ActionName := ALogMessage.ActionName;
          LAction.ActionStatus := asBegin;
          LAction.ActionProgress := 0;
          LAction.ActionMessage := ALogMessage.LogMessage;
          LAction.StartDateTime := ALogMessage.DateTime;
          LAction.LastUpdateDateTime := ALogMessage.DateTime;
        end;
      asProgress:
        begin
          if FActions.TryGetValue(LKey, LAction) then
          begin
            LAction.ActionStatus := asProgress;
            LAction.ActionProgress := ALogMessage.ActionProgress;
            LAction.ActionMessage := ALogMessage.LogMessage;
            LAction.LastUpdateDateTime := ALogMessage.DateTime;
          end;
        end;
      asEnd:
        begin
          // Remove completed action
          FActions.Remove(LKey);
        end;
    end;
  finally
    FActionsLock.Release;
  end;
end;

function TLZLog.GetAction(AThreadID: Cardinal; AActionName: string)
  : TLazyLogAction;
var
  LKey: string;
begin
  Result := nil;
  LKey := GetActionKey(AThreadID, AActionName);

  FActionsLock.Acquire;
  try
    FActions.TryGetValue(LKey, Result);
  finally
    FActionsLock.Release;
  end;
end;

function TLZLog.GetAllActions: TArray<TLazyLogAction>;
var
  LAction: TLazyLogAction;
  LIdx: integer;
begin
  FActionsLock.Acquire;
  try
    SetLength(Result, FActions.Count);
    LIdx := 0;
    for LAction in FActions.Values do
    begin
      Result[LIdx] := LAction;
      Inc(LIdx);
    end;
  finally
    FActionsLock.Release;
  end;
end;

function TLZLog.GetCurrentThreadActions: TArray<TLazyLogAction>;
var
  LAction: TLazyLogAction;
  LCurrentThreadID: Cardinal;
  LList: TList<TLazyLogAction>;
begin
  LCurrentThreadID := TThread.CurrentThread.ThreadID;
  LList := TList<TLazyLogAction>.Create;
  try
    FActionsLock.Acquire;
    try
      for LAction in FActions.Values do
      begin
        if LAction.ThreadID = LCurrentThreadID then
          LList.Add(LAction);
      end;
    finally
      FActionsLock.Release;
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

procedure SetLazyLogClass(ALazyLogClass: TLZLogClass);
begin
  _LazyLogClass := ALazyLogClass;
  try
    if Assigned(_LazyLog) then
    begin
      FreeAndNil(_LazyLog);
    end;
  except
    _LazyLog := nil;
  end;
end;

function DefaultLazyLogLevel: TLZLogLevel;
begin
{$IFDEF DEBUG}
  Result := logDebug;
{$ELSE}
  Result := logInformation;
{$ENDIF}
end;

procedure AddMemoryLogHandler;
begin
  LazyLog.AddHandler('MemoryLog', TLZLogMemory);
end;

function LazyLog: TLZLog;
begin
  Result := nil;
  if not Assigned(_LazyLog) then
  begin
    if Assigned(_LazyLogClass) then
    begin
      _LazyLog := _LazyLogClass.Create;
      _LazyLog.Log(_LazyLog, 'Application logging has been initialised');
    end
    else
    begin
      _LazyLog := TLZLog.Create;
    end;
  end;
  if Assigned(_LazyLog) then
  begin
    Result := _LazyLog;
  end;
end;

function LazyLogCache: string;
var
  LHandler: TLZLogHandler;
  LMemoryHandler: TLZLogMemory;
begin
  Result := '';
  LHandler := LazyLog.FindHandler('MemoryLog');
  if Assigned(LHandler) and (LHandler is TLZLogMemory) then
  begin
    LMemoryHandler := LHandler as TLZLogMemory;
    Result := LMemoryHandler.LogText;
  end;
end;

procedure Log(ASender: TObject; AMessage: string);
begin
  LazyLog.Log(ASender, AMessage);
end;

procedure Log(ASender: TObject; AMessage: string;

  const Args: array of

  const);
begin
  LazyLog.Log(ASender, AMessage, Args);
end;

procedure Log(ASender: string; AMessage: string);
begin
  LazyLog.Log(ASender, AMessage);
end;

procedure Debug(ASender: TObject; AProcedure: string; AMessage: string);
begin
  LazyLog.Debug(ASender, AProcedure, AMessage);
end;

procedure Debug(ASender: TObject; AProcedure: string; AMessage: string;

  const Args: array of const);
begin
  LazyLog.Debug(ASender, AProcedure, AMessage, Args);
end;

procedure Warning(ASender: TObject; AMessage: string);
begin
  LazyLog.Warning(ASender, AMessage);
end;

procedure Warning(ASender: TObject; AMessage: string;

  const Args: array of const);
begin
  LazyLog.Warning(ASender, AMessage, Args);
end;

procedure Error(ASender: TObject; AMessage: string; AErrorCode: integer = 0);
begin
  LazyLog.Error(ASender, AMessage, AErrorCode);
end;

procedure Error(ASender: TObject; AMessage: string;

  const Args: array of const; AErrorCode: integer = 0);
begin
  LazyLog.Error(ASender, AMessage, Args, AErrorCode);
end;

procedure Error(ASender: TObject; AException: Exception; AMessage: string = '');
begin
  LazyLog.Error(ASender, AException, AMessage);
end;

procedure Error(ASender: TObject; AException: Exception; AMessage: string;

  const Args: array of const);
begin
  LazyLog.Error(ASender, AException, AMessage, Args);
end;

procedure Error(ASender: string; AException: Exception; AMessage: string = '');
begin
  LazyLog.Error(ASender, AException, AMessage);
end;

procedure Error(ASender: string; AMessage: string = '');
begin
  LazyLog.Error(ASender, AMessage);
end;

initialization

SetLazyLogClass(TLZLog);
AddMemoryLogHandler;

finalization

SetLazyLogClass(nil);

end.
