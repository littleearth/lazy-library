{ -----------------------------------------------------------------------------
  Program Name: DelphiLazyLogConsole
  Author: Tristan Marlow
  Purpose: Console demonstration application for TLazyLog with color output

  Features:
  - Console logging with ANSI/VT colors
  - Multi-threaded logging demonstration
  - Action tracking with progress updates
  - Random log message generation

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions
  ---------------------------------------------------------------------------- }
program DelphiLazyLogConsole;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Lazy.Log in '..\..\..\source\core\log\Lazy.Log.pas',
  Lazy.Log.Console in '..\..\..\source\core\log\Lazy.Log.Console.pas',
  Lazy.Log.FileStream in '..\..\..\source\core\log\Lazy.Log.FileStream.pas',
  Lazy.Types in '..\..\..\source\core\base\Lazy.Types.pas';

type
  TWorkerThread = class(TThread)
  private
    FThreadName: string;
    FDuration: Integer;
    FIterations: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const AThreadName: string; ADuration: Integer);
  end;

var
  GActiveThreads: Integer = 0;
  GThreadLock: TCriticalSection;

  { TWorkerThread }

constructor TWorkerThread.Create(const AThreadName: string; ADuration: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FThreadName := AThreadName;
  FDuration := ADuration;
  FIterations := 0;
end;

procedure TWorkerThread.Execute;
var
  LElapsed: Integer;
  LProgress: Integer;
  LRandomLog: Integer;
begin
  try
    GThreadLock.Acquire;
    try
      Inc(GActiveThreads);
    finally
      GThreadLock.Release;
    end;

    // Start action
    LazyLog.LogActionBegin(Self, FThreadName,
      Format('Starting worker, duration: %d seconds', [FDuration]));

    LElapsed := 0;

    while LElapsed < FDuration do
    begin
      Sleep(5000); // Sleep 5 seconds

      LElapsed := LElapsed + 5;
      Inc(FIterations);
      LProgress := Round((LElapsed / FDuration) * 100);

      // Update progress
      LazyLog.LogActionProgress(Self, FThreadName, LProgress,
        Format('Processing... %d/%d seconds', [LElapsed, FDuration]));

      // Randomly generate log messages
      LRandomLog := Random(10);

      if LRandomLog < 2 then
        LazyLog.Debug(Self, FThreadName,
          Format('Debug checkpoint - iteration %d', [FIterations]))
      else if LRandomLog < 4 then
        LazyLog.Log(Self, Format('[%s] Information message at iteration %d',
          [FThreadName, FIterations]))
      else if LRandomLog < 6 then
        LazyLog.Warning(Self, Format('[%s] Warning detected at iteration %d',
          [FThreadName, FIterations]))
      else if LRandomLog < 7 then
        LazyLog.Error(Self, Format('[%s] Simulated error at iteration %d',
          [FThreadName, FIterations]), 3000 + FIterations);
    end;

    // End action
    LazyLog.LogActionEnd(Self, FThreadName);

  except
    on E: Exception do
      LazyLog.Error(Self, E, Format('Exception in worker thread %s',
        [FThreadName]));
  end;

  GThreadLock.Acquire;
  try
    Dec(GActiveThreads);
  finally
    GThreadLock.Release;
  end;
end;

procedure DisplayBanner;
begin
  WriteLn('================================================================================');
  WriteLn('  LazyLog Console Demo - Color Console Logging');
  WriteLn('  Demonstrates multi-threaded logging with action tracking');
  WriteLn('================================================================================');
  WriteLn;
end;

procedure DisplayMenu;
begin
  WriteLn;
  WriteLn('Available Commands:');
  WriteLn('  1 - Generate random log messages (10-20 messages)');
  WriteLn('  2 - Spawn worker threads (1-5 threads, 15-30 seconds each)');
  WriteLn('  3 - Display active actions');
  WriteLn('  4 - Toggle colors');
  WriteLn('  5 - Change log level');
  WriteLn('  6 - Test all log levels');
  WriteLn('  7 - Test exception logging');
  WriteLn('  Q - Quit');
  WriteLn;
  Write('Enter command: ');
end;

procedure GenerateRandomMessages;
var
  LCount: Integer;
  LIndex: Integer;
  LMessageType: Integer;
begin
  Randomize;
  LCount := Random(11) + 10;

  WriteLn;
  WriteLn(Format('Generating %d random log messages...', [LCount]));

  for LIndex := 1 to LCount do
  begin
    LMessageType := Random(4);

    case LMessageType of
      0:
        LazyLog.Debug(nil, 'GenerateMessages',
          Format('Debug message #%d - Random value: %d',
          [LIndex, Random(1000)]));
      1:
        LazyLog.Log(nil, Format('Information message #%d - Status OK',
          [LIndex]));
      2:
        LazyLog.Warning(nil, Format('Warning message #%d - Check required',
          [LIndex]));
      3:
        LazyLog.Error(nil, Format('Error message #%d - Operation failed',
          [LIndex]), 1000 + Random(999));
    end;
  end;

  WriteLn(Format('Generated %d messages', [LCount]));
end;

procedure SpawnWorkerThreads;
var
  LThreadCount: Integer;
  LIndex: Integer;
  LDuration: Integer;
  LThreadName: string;
begin
  Randomize;
  LThreadCount := Random(5) + 1;

  WriteLn;
  WriteLn(Format('Spawning %d worker threads...', [LThreadCount]));

  for LIndex := 1 to LThreadCount do
  begin
    LDuration := Random(16) + 15; // 15-30 seconds
    LThreadName := Format('Worker-%d', [Random(9000) + 1000 + LIndex]);
    TWorkerThread.Create(LThreadName, LDuration);
  end;

  WriteLn('Threads started');
end;

procedure DisplayActiveActions;
var
  LActions: TArray<TLazyLogAction>;
  LAction: TLazyLogAction;
  LIndex: Integer;
begin
  LActions := LazyLog.GetAllActions;

  WriteLn;
  WriteLn('Active Actions:');
  WriteLn('---------------');

  if Length(LActions) = 0 then
  begin
    WriteLn('  No active actions');
  end
  else
  begin
    for LIndex := Low(LActions) to High(LActions) do
    begin
      LAction := LActions[LIndex];
      WriteLn(Format('  [Thread %d] %s - %d%% - %s - Duration: %s',
        [LAction.ThreadID, LAction.ActionName, LAction.ActionProgress,
        LAction.ActionMessage, LAction.GetDuration]));
    end;
  end;

  WriteLn(Format('Total: %d active actions', [Length(LActions)]));
end;

procedure ToggleColors;
var
  LHandler: TLZLogHandler;
begin
  LHandler := LazyLog.FindHandler('Console');
  if Assigned(LHandler) and (LHandler is TLZLogConsole) then
  begin
    // Toggle the class variable for future handlers
    (LHandler as TLZLogConsole).UseColors := not  (LHandler as TLZLogConsole).UseColors;

    WriteLn;
    WriteLn('Colors ',  (LHandler as TLZLogConsole).UseColors, ' ');

    // Test the new setting
    LazyLog.Log(nil, 'This is a test message with new color setting');
  end;
end;

procedure ChangeLogLevel;
var
  LChoiceStr: string;
  LChoice: Integer;
  LNewLevel: TLZLogLevel;
begin
  WriteLn;
  WriteLn('Select Log Level:');
  WriteLn('  1 - Debug (all messages)');
  WriteLn('  2 - Information (info, warnings, errors)');
  WriteLn('  3 - Warning (warnings and errors)');
  WriteLn('  4 - Error (errors only)');
  Write('Choice: ');
  ReadLn(LChoiceStr);
  LChoice := StrToIntDef(LChoiceStr, -1);

  case LChoice of
    1:
      LNewLevel := logDebug;
    2:
      LNewLevel := logInformation;
    3:
      LNewLevel := logWarning;
    4:
      LNewLevel := logError;
  else
    WriteLn('Invalid choice');
    Exit;
  end;

  LazyLog.LogLevel := LNewLevel;
  WriteLn('Log level changed to: ', TLZLog.GetLogLevelText(LNewLevel));
end;

procedure TestAllLogLevels;
begin
  WriteLn;
  WriteLn('Testing all log levels...');
  WriteLn;

  LazyLog.Debug(nil, 'TestProcedure', 'This is a DEBUG message');
  Sleep(100);

  LazyLog.Log(nil, 'This is an INFORMATION message');
  Sleep(100);

  LazyLog.Warning(nil, 'This is a WARNING message');
  Sleep(100);

  LazyLog.Error(nil, 'This is an ERROR message', 500);
  Sleep(100);

  WriteLn;
  WriteLn('All log levels tested');
end;

procedure TestExceptionLogging;
begin
  WriteLn;
  WriteLn('Testing exception logging...');

  try
    raise Exception.Create('This is a test exception');
  except
    on E: Exception do
      LazyLog.Error(nil, E, 'Caught and logged test exception');
  end;

  WriteLn('Exception logged');
end;

procedure WaitForThreads;
var
  LCount: Integer;
begin
  GThreadLock.Acquire;
  try
    LCount := GActiveThreads;
  finally
    GThreadLock.Release;
  end;

  if LCount > 0 then
  begin
    WriteLn;
    WriteLn(Format('Waiting for %d active threads to complete...', [LCount]));
    WriteLn('(Press Ctrl+C to force exit)');

    while True do
    begin
      Sleep(1000);

      GThreadLock.Acquire;
      try
        LCount := GActiveThreads;
      finally
        GThreadLock.Release;
      end;

      if LCount = 0 then
        Break;

      Write(Format(#13'Active threads: %d  ', [LCount]));
    end;

    WriteLn;
    WriteLn('All threads completed');
  end;
end;

procedure InitializeLogging;
var
  LConsoleHandler: TLZLogHandler;
begin
  // Set application name
  LazyLog.ApplicationName := 'ConsoleDemo';

  // Add console handler with colors
  LazyLog.AddHandler('Console', TLZLogConsole);

  // Optionally add file handler for persistent logging
  LazyLog.AddHandler('FileLog', TLZLogFileStream);

  // Set log level to debug to see everything
  LazyLog.LogLevel := logDebug;

  // Check if VT mode is enabled
  LConsoleHandler := LazyLog.FindHandler('Console');
  if Assigned(LConsoleHandler) and (LConsoleHandler is TLZLogConsole) then
  begin
    if TLZLogConsole(LConsoleHandler).VTEnabled then
      WriteLn('VT mode enabled - colors supported')
    else
      WriteLn('VT mode not available - using plain text');
  end;

  WriteLn;
end;

procedure RunDemo;
var
  LCommand: string;
begin
  DisplayBanner;
  InitializeLogging;

  LazyLog.Log(nil, 'Console demo application started');

  repeat
    DisplayMenu;
    ReadLn(LCommand);
    LCommand := UpperCase(Trim(LCommand));
    
    if LCommand = '1' then
      GenerateRandomMessages
    else if LCommand = '2' then
      SpawnWorkerThreads
    else if LCommand = '3' then
      DisplayActiveActions
    else if LCommand = '4' then
      ToggleColors
    else if LCommand = '5' then
      ChangeLogLevel
    else if LCommand = '6' then
      TestAllLogLevels
    else if LCommand = '7' then
      TestExceptionLogging
    else if LCommand = 'Q' then
    begin
      WriteLn;
      WriteLn('Exiting...');
      Break;
    end
    else
      WriteLn('Invalid command');
    
  until False;

  // Wait for any running threads
  WaitForThreads;

  LazyLog.Log(nil, 'Console demo application shutting down');
end;

begin
  GThreadLock := TCriticalSection.Create;
  try
    RunDemo;
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  GThreadLock.Free;

end.
