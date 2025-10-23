unit WinApi.NamedPipes.Server;

interface

uses
  Lazy.Types,
  WinApi.NamedPipes.Types,
  Classes, Windows;

type
  //TOnLog = procedure(ASender : TObject; AMessage : String) of object;
  TOnMessage = procedure(ASender : TObject; AKind : Integer; AMessage : String) of object;

type
  TLZNamedPipeServer = class(TThread)
  private
    FOnLog : TOnLog;
    FOnMessage : TOnMessage;
    FHandle: THandle;
    FPipeName: String;
    FLogMessage : String;
    FPIPEMessage : RPIPEMessage;
  protected
    procedure SyncLog;
    procedure Log(AMessage : String);
    procedure SyncMessage;
    procedure PIPEMessage(APIPEMessage : RPIPEMessage);
    procedure StartLZNamedPipeServer;
    procedure ShutDownPipeServer;
  public
    constructor Create(APipe: String; AOnLog : TOnLog; AOnMessage : TOnMessage);  reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  SysUtils;

{ TLZNamedPipeServer }

constructor TLZNamedPipeServer.Create(APipe: String; AOnLog : TOnLog; AOnMessage : TOnMessage);
begin
  FPipeName := Format(cPipeFormat, ['.', aPipe]);
  FOnLog := AOnLog;
  FOnMessage := AOnMessage;
  FHandle := INVALID_HANDLE_VALUE;
  StartLZNamedPipeServer;
  inherited Create(False);
end;

destructor TLZNamedPipeServer.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    begin
      ShutDownPipeServer;
    end;
  inherited Destroy;
end;

procedure TLZNamedPipeServer.SyncLog;
begin
  FOnLog(Self,FLogMessage);
end;

procedure TLZNamedPipeServer.Log(AMessage : String);
begin
  FLogMessage := AMessage;
  if Assigned(FOnLog) then
    begin
      Synchronize(SyncLog);
    end;
end;

procedure TLZNamedPipeServer.SyncMessage;
begin
  FOnMessage(Self,FPIPEMessage.Kind,FPIPEMessage.Message);
end;

procedure TLZNamedPipeServer.PIPEMessage(APIPEMessage : RPIPEMessage);
begin
  FPIPEMessage := APIPEMessage;
  if Assigned(FOnMessage) then
    begin
      Synchronize(SyncMessage);  
    end;
end;

procedure TLZNamedPipeServer.Execute;
var
  Written: Cardinal;
  InMsg, OutMsg: RPIPEMessage;
begin
  while not Terminated do
  begin
    if FHandle = INVALID_HANDLE_VALUE then
    begin
      // suspend thread for 250 milliseconds and try again
      Sleep(250);
    end else begin
      if ConnectNamedPipe(FHandle, nil) then
      try
        // read data from pipe
        InMsg.Size := SizeOf(InMsg);
        ReadFile(FHandle, InMsg, InMsg.Size, InMsg.Size, nil);
        if (InMsg.Kind = 0) and (StrPas(InMsg.Message) = cShutDownMsg + FPipeName) then
          begin
            Log('PIPE Shutdown command recieved.');
            // process shut down
            OutMsg.Kind := 0;
            OutMsg.Count := 3;
            OutMsg.Message := 'OK'#0;
            Self.Terminate;
        end else
        begin
          PIPEMessage(InMsg);
          {// data send to pipe should be processed here
          OutMsg := InMsg;
          // we'll just reverse the data sent, byte-by-byte
          for I := 0 to Pred(InMsg.Count) do
            OutMsg.Data[Pred(InMsg.Count) - I] := InMsg.Data[I];}
          OutMsg.Kind := 0;
          OutMsg.Count := 3;
          OutMsg.Message := 'OK'#0;
        end;
        CalcMsgSize(OutMsg);
        WriteFile(FHandle, OutMsg, OutMsg.Size, Written, nil); 
      finally
        DisconnectNamedPipe(FHandle);
      end;
    end;
  end;
end; 

procedure TLZNamedPipeServer.ShutDownPipeServer;
var
  BytesRead: Cardinal;
  OutMsg, InMsg: RPIPEMessage;
  ShutDownMsg: String;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    // server still has pipe opened
    OutMsg.Size := SizeOf(OutMsg);
    // prepare shut down message
    with InMsg do
    begin
      Kind := 0;
      ShutDownMsg := cShutDownMsg + FPipeName;
      Count := Succ(Length(ShutDownMsg));
      StrPCopy(Message, ShutDownMsg);
    end;
    CalcMsgSize(InMsg);
    // send shut down message
    CallNamedPipe(
      PChar(FPipeName), @InMsg, InMsg.Size, @OutMsg, OutMsg.Size, BytesRead, 100
    );
    // close pipe on server
    CloseHandle(FHandle);
    // clear handle
    FHandle := INVALID_HANDLE_VALUE;
  end;
end; 

procedure TLZNamedPipeServer.StartLZNamedPipeServer;
begin
  Log('Starting: ' + FPipeName);
  // check whether pipe does exist
  if WaitNamedPipe(PChar(FPipeName), 100) then
    raise Exception.CreateFmt('Requested PIPE %s exists already.',[FPipeName]);
  // create the pipe
  FHandle := CreateNamedPipe(
    PChar(FPipeName), PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    PIPE_UNLIMITED_INSTANCES, SizeOf(RPIPEMessage), SizeOf(RPIPEMessage),
    NMPWAIT_USE_DEFAULT_WAIT, nil
  );
  // check if pipe was created
  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Could not create PIPE.'); 
end;

end.

