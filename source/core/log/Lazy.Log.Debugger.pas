{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.Debugger
  Author: Tristan Marlow
  Purpose: Log handler that sends messages to the debugger

  Overview:
  This unit provides a log handler that outputs log messages to the system
  debugger using the Windows OutputDebugString API. This is useful for
  debugging applications as messages appear in the IDE's debug output window
  or external debugger tools.

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ----------------------------------------------------------------------------; }
unit Lazy.Log.Debugger;

interface

uses
  SysUtils, Classes, Lazy.Log, Lazy.Types
{$IFDEF MSWINDOWS}
  , Winapi.Windows
{$ENDIF}
  ;

type
  TLZLogDebugger = class(TLZLogHandler)
  private
  protected
    /// <summary>
    /// Outputs a message to the system debugger
    /// </summary>
    procedure OutputToDebugger(const AMessage: String);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Processes a log message and outputs it to the debugger
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
  end;

implementation

uses
  Lazy.Utils;

{ TLZLogDebugger }

procedure TLZLogDebugger.AfterConstruction;
begin
  inherited;
end;

procedure TLZLogDebugger.BeforeDestruction;
begin
  inherited;
end;

procedure TLZLogDebugger.OutputToDebugger(const AMessage: String);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AMessage))
{$ENDIF}
end;


procedure TLZLogDebugger.ProcessLogMessage(ALogMessage: TLazyLogMessage);
var
  LLogMessage : string;
begin
  LLogMessage := GetFormattedMessage(ALogMessage);
  if TLZString.IsEmptyString(LLogMessage) then
    Exit;
  OutputToDebugger(LLogMessage);
end;

initialization


end.
