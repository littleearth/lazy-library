{ -----------------------------------------------------------------------------
  Unit Name: frmLazyLogDemoU
  Author: Tristan Marlow
  Purpose: Demonstration form for TLazyLog functionality

  Features:
  - Displays log messages from memory handler
  - Random log message generation
  - Multi-threaded action logging

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions
  ---------------------------------------------------------------------------- }
unit frmLazyLogDemoU;

interface

uses
  Lazy.Types,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Lazy.Log, FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait, FireDAC.Stan.Intf, FireDAC.Comp.UI;

type
  TLogWorkerThread = class(TThread)
  private
    FActionName: string;
    FDuration: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AActionName: string; ADuration: Integer);
  end;

  TfrmLazyLogDemo = class(TForm)
    memoLog: TMemo;
    pnlTop: TPanel;
    btnAddRandomMessages: TButton;
    btnSpawnThreads: TButton;
    timerRefresh: TTimer;
    lblStatus: TLabel;
    btnClearLog: TButton;
    btnPurgeLogs: TButton;
    lvActions: TListView;
    timerActions: TTimer;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddRandomMessagesClick(Sender: TObject);
    procedure btnSpawnThreadsClick(Sender: TObject);
    procedure timerRefreshTimer(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnPurgeLogsClick(Sender: TObject);
    procedure timerActionsTimer(Sender: TObject);
  private
    FActiveThreadCount: Integer;
    procedure AddRandomMessages;
    procedure SpawnWorkerThreads;
    procedure UpdateLogDisplay;
    procedure UpdateActionsDisplay;
    procedure ThreadFinished(Sender: TObject);
    procedure InitializeActionsListView;
  public
  end;

var
  frmLazyLogDemo: TfrmLazyLogDemo;

implementation

{$R *.dfm}

uses
  Lazy.Log.SQLite, Lazy.Log.Memory, Lazy.Log.FileStream, System.SyncObjs, CustomLogHandler;
  

var
  GThreadCountLock: TCriticalSection;

  { TLogWorkerThread }

constructor TLogWorkerThread.Create(AActionName: string; ADuration: Integer);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FActionName := AActionName;
  FDuration := ADuration;
end;

procedure TLogWorkerThread.Execute;
var
  LElapsed: Integer;
  LIteration: Integer;
  LProgress: Integer;
  LRandomLogType: Integer;
begin
  try
    // Log action begin
    LazyLog.LogActionBegin(Self, FActionName,
      Format('Starting task, duration: %d seconds', [FDuration]));

    LElapsed := 0;
    LIteration := 0;

    while (not Terminated) and (LElapsed < FDuration) do
    begin
      // Sleep for 10 seconds (or until terminated)
      Sleep(10000);

      if Terminated then
        Break;

      LElapsed := LElapsed + 10;
      Inc(LIteration);
      LProgress := Round((LElapsed / FDuration) * 100);

      // Send progress update
      LazyLog.LogActionProgress(Self, FActionName, LProgress,
        Format('Working... %d/%d seconds elapsed', [LElapsed, FDuration]));

      // Randomly log Debug, Warning, or Error messages
      LRandomLogType := Random(3);
      case LRandomLogType of
        0: // Debug
          LazyLog.Debug(Self, FActionName,
            Format('Debug checkpoint at %d%% - Iteration %d', [LProgress, LIteration]));
        1: // Warning
          LazyLog.Warning(Self,
            Format('[%s] Warning: High iteration count detected (%d)', [FActionName, LIteration]));
        2: // Error
          LazyLog.Error(Self,
            Format('[%s] Non-critical error at iteration %d', [FActionName, LIteration]),
            Random(999) + 2000);
      end;
    end;

    // Log action end
    if not Terminated then
      LazyLog.LogActionEnd(Self, FActionName);

  except
    on E: Exception do
    begin
      LazyLog.Error(Self, E, 'Error in worker thread ' + FActionName);
    end;
  end;
end;

{ TfrmLazyLogDemo }

procedure TfrmLazyLogDemo.FormCreate(Sender: TObject);
begin
  FActiveThreadCount := 0;
  
  LazyLog.ApplicationName := 'Demo';

  // Initialize log handlers
  LazyLog.AddHandler('FileLog', TLZLogFileStream);
  LazyLog.AddHandler('MemoryLog', TLZLogMemory);
  LazyLog.AddHandler('SQLiteLog', TLZLogSQLite);
  LazyLog.AddHandler('CustomLog', TCustomLogHandler);

  // Set log level to Debug
  LazyLog.LogLevel := logDebug;

  // Initialize actions ListView
  InitializeActionsListView;

  // Start the refresh timers
  timerRefresh.Interval := 1000; // Refresh log every second
  timerRefresh.Enabled := True;
  
  timerActions.Interval := 10000; // Refresh actions every 10 seconds
  timerActions.Enabled := True;

  // Log application start
  LazyLog.Log(Self, 'LazyLog Demo Application Started');

  UpdateLogDisplay;
  UpdateActionsDisplay;
end;

procedure TfrmLazyLogDemo.FormDestroy(Sender: TObject);
begin
  timerRefresh.Enabled := False;

  // Log application end
  LazyLog.Log(Self, 'LazyLog Demo Application Closing');

  // Handlers will be cleaned up automatically by the log system
end;

procedure TfrmLazyLogDemo.btnAddRandomMessagesClick(Sender: TObject);
begin
  AddRandomMessages;
  UpdateLogDisplay;
end;

procedure TfrmLazyLogDemo.btnSpawnThreadsClick(Sender: TObject);
begin
  SpawnWorkerThreads;
end;

procedure TfrmLazyLogDemo.btnClearLogClick(Sender: TObject);
var
  LHandler: TLZLogHandler;
begin
  // Clear the SQLite log
  LHandler := LazyLog.FindHandler('SQLiteLog');
  if Assigned(LHandler) and (LHandler is TLZLogSQLite) then
    TLZLogSQLite(LHandler).ClearAllLogs;

  memoLog.Clear;
  LazyLog.Log(Self, 'Logs cleared');
  UpdateLogDisplay;
end;

procedure TfrmLazyLogDemo.btnPurgeLogsClick(Sender: TObject);
var
  LHandler: TLZLogHandler;
begin
  // Purge old logs from SQLite (keep last 7 days)
  LHandler := LazyLog.FindHandler('SQLiteLog');
  if Assigned(LHandler) and (LHandler is TLZLogSQLite) then
  begin
    TLZLogSQLite(LHandler).PurgeOldLogs(7);
    LazyLog.Log(Self, 'Old logs purged (kept last 7 days)');
  end;
  UpdateLogDisplay;
end;

procedure TfrmLazyLogDemo.timerRefreshTimer(Sender: TObject);
begin
  UpdateLogDisplay;
end;

procedure TfrmLazyLogDemo.AddRandomMessages;
var
  LCount: Integer;
  LIndex: Integer;
  LMessageType: Integer;
begin
  // Generate random number of messages (5 to 20)
  Randomize;
  LCount := Random(16) + 5;

  for LIndex := 1 to LCount do
  begin
    LMessageType := Random(4);

    case LMessageType of
      0: // Information
        LazyLog.Log(Self, Format('Information message #%d - Random value: %d',
          [LIndex, Random(1000)]));

      1: // Warning
        LazyLog.Warning(Self, Format('Warning message #%d - Check value: %d',
          [LIndex, Random(100)]));

      2: // Error with error code
        LazyLog.Error(Self, Format('Error message #%d - Operation failed',
          [LIndex]), Random(999) + 1000);

      3: // Debug
        LazyLog.Debug(Self, 'AddRandomMessages',
          Format('Debug message #%d - Iteration details', [LIndex]));
    end;
  end;

  LazyLog.Log(Self, Format('Added %d random log messages', [LCount]));
end;

procedure TfrmLazyLogDemo.SpawnWorkerThreads;
var
  LThreadCount: Integer;
  LIndex: Integer;
  LDuration: Integer;
  LActionName: string;
  LThread: TLogWorkerThread;
begin
  Randomize;

  // Generate random number of threads (1 to 10)
  LThreadCount := Random(10) + 1;

  GThreadCountLock.Acquire;
  try
    FActiveThreadCount := FActiveThreadCount + LThreadCount;
  finally
    GThreadCountLock.Release;
  end;

  LazyLog.Log(Self, Format('Spawning %d worker threads', [LThreadCount]));

  for LIndex := 1 to LThreadCount do
  begin
    // Random duration between 30 and 120 seconds
    LDuration := Random(91) + 30;
    LActionName := Format('WorkerThread_%d', [GetTickCount + LIndex]);

    LThread := TLogWorkerThread.Create(LActionName, LDuration);
    LThread.OnTerminate := ThreadFinished;
  end;

  UpdateLogDisplay;
end;

procedure TfrmLazyLogDemo.ThreadFinished(Sender: TObject);
begin
  GThreadCountLock.Acquire;
  try
    Dec(FActiveThreadCount);
  finally
    GThreadCountLock.Release;
  end;
end;

procedure TfrmLazyLogDemo.UpdateLogDisplay;
var
  LLogText: string;
  LActiveCount: Integer;
begin
  // Get log text from memory handler
  LLogText := LazyLogCache;

  // Only update if content changed to reduce flicker
  if memoLog.Text <> LLogText then
  begin
    memoLog.Lines.BeginUpdate;
    try
      memoLog.Text := LLogText;
      // Scroll to top to see latest messages (memory log inserts at top)
      memoLog.SelStart := 0;
      memoLog.SelLength := 0;
    finally
      memoLog.Lines.EndUpdate;
    end;
  end;

  // Update status label
  GThreadCountLock.Acquire;
  try
    LActiveCount := FActiveThreadCount;
  finally
    GThreadCountLock.Release;
  end;

  lblStatus.Caption := Format('Active Threads: %d | Log Entries: %d | Active Actions: %d',
    [LActiveCount, memoLog.Lines.Count, lvActions.Items.Count]);
end;

procedure TfrmLazyLogDemo.InitializeActionsListView;
var
  LColumn: TListColumn;
begin
  lvActions.ViewStyle := vsReport;
  lvActions.RowSelect := True;
  lvActions.ReadOnly := True;
  lvActions.GridLines := True;
  
  // Add columns
  LColumn := lvActions.Columns.Add;
  LColumn.Caption := 'Thread ID';
  LColumn.Width := 80;
  
  LColumn := lvActions.Columns.Add;
  LColumn.Caption := 'Action Name';
  LColumn.Width := 150;
  
  LColumn := lvActions.Columns.Add;
  LColumn.Caption := 'Status';
  LColumn.Width := 80;
  
  LColumn := lvActions.Columns.Add;
  LColumn.Caption := 'Progress';
  LColumn.Width := 70;
  
  LColumn := lvActions.Columns.Add;
  LColumn.Caption := 'Duration';
  LColumn.Width := 80;
  
  LColumn := lvActions.Columns.Add;
  LColumn.Caption := 'Message';
  LColumn.Width := 300;
end;

procedure TfrmLazyLogDemo.UpdateActionsDisplay;
var
  LActions: TArray<TLazyLogAction>;
  LAction: TLazyLogAction;
  LItem: TListItem;
  LStatusText: string;
  LIndex: Integer;
begin
  // Get all active actions
  LActions := LazyLog.GetAllActions;
  
  lvActions.Items.BeginUpdate;
  try
    lvActions.Clear;
    
    for LIndex := Low(LActions) to High(LActions) do
    begin
      LAction := LActions[LIndex];
      
      // Convert action status to text
      case LAction.ActionStatus of
        asBegin: LStatusText := 'Begin';
        asProgress: LStatusText := 'Progress';
        asEnd: LStatusText := 'End';
      else
        LStatusText := 'Unknown';
      end;
      
      // Add item to ListView
      LItem := lvActions.Items.Add;
      LItem.Caption := IntToStr(LAction.ThreadID);
      LItem.SubItems.Add(LAction.ActionName);
      LItem.SubItems.Add(LStatusText);
      LItem.SubItems.Add(Format('%d%%', [LAction.ActionProgress]));
      LItem.SubItems.Add(LAction.GetDuration);
      LItem.SubItems.Add(LAction.ActionMessage);
    end;
  finally
    lvActions.Items.EndUpdate;
  end;
  
  // Update status label
  UpdateLogDisplay;
end;

procedure TfrmLazyLogDemo.timerActionsTimer(Sender: TObject);
begin
  UpdateActionsDisplay;
end;

initialization

GThreadCountLock := TCriticalSection.Create;

finalization

GThreadCountLock.Free;

end.
