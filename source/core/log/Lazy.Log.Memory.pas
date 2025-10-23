{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.Memory
  Author: Tristan Marlow
  Purpose: Log handler that stores messages in memory

  Overview:
  This unit provides a memory-based log handler that stores log messages
  in a thread-safe string list. It's useful for applications that need
  to access recent log messages programmatically or display them in a UI.

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ----------------------------------------------------------------------------; }
unit Lazy.Log.Memory;

interface

uses
  SysUtils, Classes, Lazy.Log, Lazy.Types, Lazy.ThreadedStringList;

type
  TLZLogMemory = class(TLZLogHandler)
  private
    FLog: TLZThreadStringList;
    FCacheSize : integer;
  protected
    /// <summary>
    /// Adds a log message to the memory cache
    /// </summary>
    procedure AddLogMessage(AMessage: string);
    /// <summary>
    /// Trims log messages to maintain cache size limit
    /// </summary>
    procedure TrimLogMessages;
  public
    /// <summary>
    /// Default cache size for new memory log handlers
    /// </summary>
    class var DefaultCacheSize: integer;
    /// <summary>
    /// Default setting for whether to log all levels regardless of threshold
    /// </summary>
    class var DefaultAllLogLevels: boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Processes a log message and stores it in memory
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
    /// <summary>
    /// Returns all cached log messages as a single string
    /// </summary>
    function LogText: string;
    /// <summary>
    /// Maximum number of log messages to keep in memory
    /// </summary>
    property CacheSize: integer read FCacheSize write FCacheSize;
    /// <summary>
    /// Static method to get log text from the default memory handler
    /// </summary>
    class function Text: string;
  end;

implementation

{ TLZLogMemory }

uses
  Lazy.Utils;

procedure TLZLogMemory.AfterConstruction;
begin
  inherited;
  FLog := TLZThreadStringList.Create;
  FLog.Sorted := False;
  AllLogLevels := DefaultAllLogLevels;
  CacheSize := DefaultCacheSize;
end;

procedure TLZLogMemory.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FLog);
end;

procedure TLZLogMemory.ProcessLogMessage(ALogMessage: TLazyLogMessage);
var
  LFormattedMessage: string;
begin
  // Skip progress messages
  if (ALogMessage.LogType = ltProgress) and
    (ALogMessage.ActionStatus = asProgress) then
    Exit;

  LFormattedMessage := GetFormattedMessage(ALogMessage);

  if not TLZString.IsEmptyString(LFormattedMessage) then
  begin
    AddLogMessage(LFormattedMessage);
  end;
end;

procedure TLZLogMemory.TrimLogMessages;
begin
  while FLog.Count > CacheSize do
  begin
    FLog.Delete(FLog.Count - 1);
  end;
end;

procedure TLZLogMemory.AddLogMessage(AMessage: string);
begin
  FLog.Insert(0, TLZString.StringCleaner(AMessage,true,true));
  TrimLogMessages;
end;

function TLZLogMemory.LogText: string;
begin
  if Assigned(FLog) then
    Result := FLog.Text;
end;

class function TLZLogMemory.Text: string;
var
  LHandler: TLZLogHandler;
begin
  Result := '';
  LHandler := LazyLog.FindHandler('MemoryLog');
  if Assigned(LHandler) and (LHandler is TLZLogMemory) then
    Result := (LHandler as TLZLogMemory).LogText;
end;

initialization

TLZLogMemory.DefaultCacheSize := 5000;
TLZLogMemory.DefaultAllLogLevels := True;

end.
