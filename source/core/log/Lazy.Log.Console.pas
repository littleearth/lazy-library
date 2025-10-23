{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.Console
  Author: Tristan Marlow
  Purpose: Log handler that outputs to console with support for colors

  Overview:
  This unit provides a console-based log handler that outputs log messages
  to the system console with optional color support. It automatically detects
  and enables VT (Virtual Terminal) mode on Windows for full color support,
  and works natively with color on Unix-like systems.

  Features:
  - Cross-platform console output
  - Color support via ANSI/VT escape codes
  - Windows Terminal support with VT mode
  - Automatic VT mode detection and setup on Windows

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ---------------------------------------------------------------------------- }
unit Lazy.Log.Console;

interface

uses
  SysUtils, Classes, Lazy.Log, Lazy.Types
{$IFDEF MSWINDOWS}
    , Winapi.Windows
{$ENDIF}
    ;

type
  TLZLogConsole = class(TLZLogHandler)
  private
    FUseColors: Boolean;
    FVTEnabled: Boolean;
{$IFDEF MSWINDOWS}
    FStdHandle: THandle;
    /// <summary>
    /// Enables VT mode on Windows console for color support
    /// </summary>
    function EnableVTMode: Boolean;
{$ENDIF}
    /// <summary>
    /// Gets the ANSI color code for the specified log level
    /// </summary>
    function GetColorCode(ALogLevel: TLZLogLevel): string;
    /// <summary>
    /// Gets the ANSI reset code to clear formatting
    /// </summary>
    function GetResetCode: string;
    /// <summary>
    /// Writes text to the console output
    /// </summary>
    procedure WriteToConsole(const AText: string);
  protected
    /// <summary>
    /// Initializes the console for color output
    /// </summary>
    procedure InitializeConsole;
  public
    /// <summary>
    /// Default setting for whether to use colors in console output
    /// </summary>
    class var DefaultUseColors: Boolean;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    /// <summary>
    /// Processes a log message and outputs it to the console
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
    /// <summary>
    /// Whether VT mode is enabled for color support
    /// </summary>
    property VTEnabled: Boolean read FVTEnabled write FVTEnabled;
    /// <summary>
    /// Whether to use colors in console output
    /// </summary>
    property UseColors: Boolean read FUseColors write FUseColors;
  end;

implementation

{ TLZLogConsole }

procedure TLZLogConsole.AfterConstruction;
begin
  inherited;
  FUseColors := DefaultUseColors;
  FVTEnabled := False;
  InitializeConsole;
end;

procedure TLZLogConsole.BeforeDestruction;
begin
  inherited;
end;

procedure TLZLogConsole.InitializeConsole;
begin
{$IFDEF MSWINDOWS}
  FStdHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  if FStdHandle <> INVALID_HANDLE_VALUE then
  begin
    FVTEnabled := EnableVTMode;
  end;
{$ELSE}
  // On Unix-like systems, assume VT codes are supported
  FVTEnabled := True;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

function TLZLogConsole.EnableVTMode: Boolean;
const
  ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004;
var
  LMode: DWORD;
begin
  Result := False;
  if GetConsoleMode(FStdHandle, LMode) then
  begin
    LMode := LMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    Result := SetConsoleMode(FStdHandle, LMode);
  end;
end;
{$ENDIF}

function TLZLogConsole.GetColorCode(ALogLevel: TLZLogLevel): string;
begin
  if not FUseColors or not FVTEnabled then
  begin
    Result := '';
    Exit;
  end;

  // ANSI/VT escape codes for colors
  case ALogLevel of
    logDebug:
      Result := #27'[36m'; // Cyan
    logInformation:
      Result := #27'[32m'; // Green
    logWarning:
      Result := #27'[33m'; // Yellow
    logError:
      Result := #27'[31m'; // Red
  else
    Result := '';
  end;
end;

function TLZLogConsole.GetResetCode: string;
begin
  if not FUseColors or not FVTEnabled then
    Result := ''
  else
    Result := #27'[0m'; // Reset all attributes
end;

procedure TLZLogConsole.WriteToConsole(const AText: string);
begin
{$IFDEF MSWINDOWS}
  // Use Windows API for better Unicode support
  var
    LWritten: DWORD;
  var
    LTextW: string;
  LTextW := AText + sLineBreak;
  WriteConsoleW(FStdHandle, PChar(LTextW), Length(LTextW), LWritten, nil);
{$ELSE}
  // Use standard WriteLn on other platforms
  WriteLn(AText);
{$ENDIF}
end;

procedure TLZLogConsole.ProcessLogMessage(ALogMessage: TLazyLogMessage);
var
  LFormattedMessage: string;
  LColorCode: string;
  LResetCode: string;
  LLogLevelText: string;
  LOutput: string;
begin
  // Get formatted message from base class
  LFormattedMessage := GetFormattedMessage(ALogMessage, False);

  if LFormattedMessage = '' then
    Exit;

  LLogLevelText := TLZLog.GetLogLevelText(ALogMessage.LogLevel);

  // Apply colors if enabled
  LColorCode := GetColorCode(ALogMessage.LogLevel);
  LResetCode := GetResetCode;

  if ALogMessage.LogType = ltInformation then
  begin
    LOutput := Format('%s%s%s', [LResetCode, ALogMessage.LogMessage, LResetCode]);
  end
  else
  begin
    LOutput := Format('%s%s:%s %s', [LColorCode, LLogLevelText, LResetCode,
      LFormattedMessage]);
  end;

  WriteToConsole(LOutput);
end;

initialization

TLZLogConsole.DefaultUseColors := True;

end.
