unit MessagingExampleFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Messaging,
  Lazy.Log, Lazy.Log.Messaging, Lazy.Types;

type
  /// <summary>
  /// Example class that demonstrates class-filtered logging
  /// </summary>
  TExampleClass = class(TLZObject)
  public
    procedure DoSomething;
    procedure DoSomethingElse;
    procedure ProcessData(const AData: string);
  end;

  TfrmMessagingExample = class(TForm)
    pnlTop: TPanel;
    memoAllMessages: TMemo;
    Splitter1: TSplitter;
    memoErrorsOnly: TMemo;
    Panel1: TPanel;
    lblAllMessages: TLabel;
    Panel2: TPanel;
    lblErrorsOnly: TLabel;
    Panel4: TPanel;
    chkSubscribeAll: TCheckBox;
    chkSubscribeErrors: TCheckBox;
    chkSubscribeWarnings: TCheckBox;
    chkSubscribeToExampleClass: TCheckBox;
    lblExampleClassMessages: TLabel;
    Panel5: TPanel;
    btnClear: TButton;
    btnExampleClassLog: TButton;
    btnLogDebug: TButton;
    btnLogError: TButton;
    btnLogInfo: TButton;
    btnLogWarning: TButton;
    btnTestProgress: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLogInfoClick(Sender: TObject);
    procedure btnLogWarningClick(Sender: TObject);
    procedure btnLogErrorClick(Sender: TObject);
    procedure btnLogDebugClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnTestProgressClick(Sender: TObject);
    procedure chkSubscribeAllClick(Sender: TObject);
    procedure chkSubscribeErrorsClick(Sender: TObject);
    procedure chkSubscribeWarningsClick(Sender: TObject);
    procedure btnExampleClassLogClick(Sender: TObject);
    procedure chkSubscribeToExampleClassClick(Sender: TObject);
  private
    FAllMessagesSubscription: TLZLogMessageSubscription;
    FErrorSubscription: TLZLogMessageSubscription;
    FWarningSubscription: TLZLogMessageSubscription;
    FExampleClassSubscription: TLZLogMessageSubscription;
    FExampleClass: TExampleClass;
    procedure HandleAllMessages(const Sender: TObject; const M: TMessage);
    procedure HandleErrorMessages(const Sender: TObject; const M: TMessage);
    procedure HandleWarningMessages(const Sender: TObject; const M: TMessage);
    procedure HandleExampleClassMessages(const Sender: TObject; const M: TMessage);
    procedure SubscribeToAllMessages;
    procedure SubscribeToErrorMessages;
    procedure SubscribeToWarningMessages;
    procedure SubscribeToExampleClassMessages;
    procedure UnsubscribeAll;
  public
  end;

var
  frmMessagingExample: TfrmMessagingExample;

implementation

{$R *.dfm}

{ TExampleClass }

procedure TExampleClass.DoSomething;
begin
  Log( 'TExampleClass is doing something');
  Debug( 'DoSomething', 'Processing started');
end;

procedure TExampleClass.DoSomethingElse;
begin
  Log( 'TExampleClass is doing something else');
  Warning( 'This operation might take a while');
end;

procedure TExampleClass.ProcessData(const AData: string);
begin
  Log( Format('Processing data: %s', [AData]));
  Debug( 'ProcessData', Format('Data length: %d characters', [Length(AData)]));
end;

{ TfrmMessagingExample }

procedure TfrmMessagingExample.FormCreate(Sender: TObject);
begin
  // Initialize logging
  LazyLog.ApplicationName := 'MessagingExample';
  LazyLog.LogLevel := logDebug;

  // Add the messaging handler
  LazyLog.AddHandler('Messaging', TLZLogMessaging);
  
  // Create example class instance
  FExampleClass := TExampleClass.Create;

  // Subscribe to messages by default
  chkSubscribeAll.Checked := True;
  chkSubscribeErrors.Checked := True;
  chkSubscribeWarnings.Checked := False;
  chkSubscribeToExampleClass.Checked := False;

  SubscribeToAllMessages;
  SubscribeToErrorMessages;

  Log(Self, 'Application started - Log messaging example');
end;

procedure TfrmMessagingExample.FormDestroy(Sender: TObject);
begin
  UnsubscribeAll;
  FreeAndNil(FExampleClass);
  Log(Self, 'Application closing');
end;

procedure TfrmMessagingExample.SubscribeToAllMessages;
begin
  if Assigned(FAllMessagesSubscription) then
    Exit;

  FAllMessagesSubscription := TLZLogSubscriber.SubscribeToAll
    (HandleAllMessages);
end;

procedure TfrmMessagingExample.SubscribeToErrorMessages;
begin
  if Assigned(FErrorSubscription) then
    Exit;

  FErrorSubscription := TLZLogSubscriber.SubscribeToErrors(HandleErrorMessages);
end;

procedure TfrmMessagingExample.SubscribeToWarningMessages;
begin
  if Assigned(FWarningSubscription) then
    Exit;

  FWarningSubscription := TLZLogSubscriber.SubscribeToWarnings
    (HandleWarningMessages);
end;

procedure TfrmMessagingExample.SubscribeToExampleClassMessages;
begin
  if Assigned(FExampleClassSubscription) then
    Exit;

  // Subscribe to ALL messages, but filter by TExampleClass only
  FExampleClassSubscription := TLZLogSubscriber.SubscribeToAll(
    HandleExampleClassMessages, 'TExampleClass');
  
  lblExampleClassMessages.Caption := 'Listening to TExampleClass messages...';
end;

procedure TfrmMessagingExample.UnsubscribeAll;
begin
  if Assigned(FAllMessagesSubscription) then
    FreeAndNil(FAllMessagesSubscription);

  if Assigned(FErrorSubscription) then
    FreeAndNil(FErrorSubscription);

  if Assigned(FWarningSubscription) then
    FreeAndNil(FWarningSubscription);
    
  if Assigned(FExampleClassSubscription) then
    FreeAndNil(FExampleClassSubscription);
end;

procedure TfrmMessagingExample.HandleAllMessages(const Sender: TObject;
  const M: TMessage);
var
  LLogMsg: TLazyLogMessage;
  LText: string;
begin
  if M is TLZLogMessageWrapper then
  begin
    LLogMsg := (M as TLZLogMessageWrapper).LogMessage;
    // Format the message

    if LLogMsg.LogType = ltProgress then
    begin
      LText := Format('[%s] [%s] Action: %s - %s',
        [FormatDateTime('hh:nn:ss', LLogMsg.DateTime), LLogMsg.LogClass, LLogMsg.ActionName,
        LLogMsg.LogMessage]);
    end
    else
    begin
      LText := Format('[%s] [%s] %s',
        [FormatDateTime('hh:nn:ss', LLogMsg.DateTime), LLogMsg.LogClass,
        LLogMsg.LogMessage]);
    end;

    // Update UI on main thread
    TThread.Queue(nil,
      procedure
      begin
        memoAllMessages.Lines.Add(LText);
      end);
  end;
end;

procedure TfrmMessagingExample.HandleErrorMessages(const Sender: TObject;
const M: TMessage);
var
  LLogMsg: TLazyLogMessage;
  LText: string;
  LExceptionClass: string;
  LExceptionMsg: string;
  LHasException: Boolean;
begin
  if M is TLZLogMessageError then
  begin
    LLogMsg := (M as TLZLogMessageError).LogMessage;

    // Format the message
    LText := Format('[%s] ERROR: %s',
      [FormatDateTime('hh:nn:ss', LLogMsg.DateTime), LLogMsg.LogMessage]);

    // Capture exception details before switching threads
    LHasException := Assigned(LLogMsg.Exception);
    if LHasException then
    begin
      LExceptionClass := LLogMsg.Exception.ClassName;
      LExceptionMsg := LLogMsg.Exception.Message;
    end;

    // Update UI on main thread
    TThread.Queue(nil,
      procedure
      begin
        memoErrorsOnly.Lines.Add(LText);

        // Add exception details if present
        if LHasException then
        begin
          memoErrorsOnly.Lines.Add('  Exception: ' + LExceptionClass);
          memoErrorsOnly.Lines.Add('  Message: ' + LExceptionMsg);
        end;
      end);
  end;
end;

procedure TfrmMessagingExample.HandleWarningMessages(const Sender: TObject;
const M: TMessage);
var
  LLogMsg: TLazyLogMessage;
  LText: string;
begin
  if M is TLZLogMessageWarning then
  begin
    LLogMsg := (M as TLZLogMessageWarning).LogMessage;

    // Format the message
    LText := Format('[%s] WARNING: %s',
      [FormatDateTime('hh:nn:ss', LLogMsg.DateTime), LLogMsg.LogMessage]);

    // Update UI on main thread
    TThread.Queue(nil,
      procedure
      begin
        memoErrorsOnly.Lines.Add(LText);
      end);
  end;
end;

procedure TfrmMessagingExample.HandleExampleClassMessages(const Sender: TObject;
  const M: TMessage);
var
  LLogMsg: TLazyLogMessage;
  LText: string;
  LCount: Integer;
begin
  if M is TLZLogMessageWrapper then
  begin
    LLogMsg := (M as TLZLogMessageWrapper).LogMessage;

    // Format the message
    LText := Format('[%s] %s', 
      [FormatDateTime('hh:nn:ss', LLogMsg.DateTime), LLogMsg.LogMessage]);

    // Update UI on main thread
    TThread.Queue(nil,
      procedure
      begin
        // Keep only last 5 messages in the label
        LCount := lblExampleClassMessages.Tag;
        Inc(LCount);
        lblExampleClassMessages.Tag := LCount;
        lblExampleClassMessages.Caption := Format('TExampleClass Message #%d: %s', 
          [LCount, LText]);
      end);
  end;
end;

procedure TfrmMessagingExample.btnLogInfoClick(Sender: TObject);
begin
  Log(Self, 'This is an information message');
end;

procedure TfrmMessagingExample.btnLogWarningClick(Sender: TObject);
begin
  Warning(Self, 'This is a warning message - something might be wrong!');
end;

procedure TfrmMessagingExample.btnLogErrorClick(Sender: TObject);
var
  LException: Exception;
begin
  try
    // Simulate an error
    raise Exception.Create('This is a simulated error for demonstration');
  except
    on E: Exception do
    begin
      LException := Exception(AcquireExceptionObject);
      try
        Error(Self, LException, 'Error occurred while processing');
      finally
        LException.Free;
      end;
    end;
  end;
end;

procedure TfrmMessagingExample.btnLogDebugClick(Sender: TObject);
begin
  Debug(Self, 'btnLogDebugClick', 'Debug message - detailed information');
end;

procedure TfrmMessagingExample.btnTestProgressClick(Sender: TObject);
begin
  // Run the progress test in a background thread
  TThread.CreateAnonymousThread(
    procedure
    var
      LIdx: Integer;
      LTotal: Integer;
    begin
      LazyLog.LogActionBegin(Self, 'TestAction',
        'Starting test action in thread');
      try
        LTotal := Random(90) + 10;
        for LIdx := 0 to LTotal do
        begin
          Sleep(Random(1000) + 10);
          LazyLog.LogActionProgress(Self, 'TestAction', LIdx * LTotal,
            Format('Processing step %d of %d', [LIdx, LTotal]));
        end;
      finally
        LazyLog.LogActionEnd(Self, 'TestAction');
      end;
    end).Start;
end;

procedure TfrmMessagingExample.btnClearClick(Sender: TObject);
begin
  memoAllMessages.Clear;
  memoErrorsOnly.Clear;
end;

procedure TfrmMessagingExample.chkSubscribeAllClick(Sender: TObject);
begin
  if chkSubscribeAll.Checked then
    SubscribeToAllMessages
  else
  begin
    if Assigned(FAllMessagesSubscription) then
      FreeAndNil(FAllMessagesSubscription);
  end;
end;

procedure TfrmMessagingExample.chkSubscribeErrorsClick(Sender: TObject);
begin
  if chkSubscribeErrors.Checked then
    SubscribeToErrorMessages
  else
  begin
    if Assigned(FErrorSubscription) then
      FreeAndNil(FErrorSubscription);
  end;
end;

procedure TfrmMessagingExample.chkSubscribeWarningsClick(Sender: TObject);
begin
  if chkSubscribeWarnings.Checked then
    SubscribeToWarningMessages
  else
  begin
    if Assigned(FWarningSubscription) then
      FreeAndNil(FWarningSubscription);
  end;
end;

procedure TfrmMessagingExample.btnExampleClassLogClick(Sender: TObject);
begin
  // Call various methods on TExampleClass which will generate log messages
  FExampleClass.DoSomething;
  FExampleClass.DoSomethingElse;
  FExampleClass.ProcessData('Sample data from button click');
end;

procedure TfrmMessagingExample.chkSubscribeToExampleClassClick(Sender: TObject);
begin
  if chkSubscribeToExampleClass.Checked then
  begin
    SubscribeToExampleClassMessages;
  end
  else
  begin
    if Assigned(FExampleClassSubscription) then
      FreeAndNil(FExampleClassSubscription);
    lblExampleClassMessages.Caption := 'Not listening to TExampleClass';
    lblExampleClassMessages.Tag := 0;
  end;
end;

end.
