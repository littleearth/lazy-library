unit VCL.Lazy.ThreadTimer;

interface

uses
  Windows, Messages, System.SyncObjs,
  System.SysUtils, System.Variants, System.Classes;

type
  TLazyThreadTimer = class(TThread)
  private
    FActive: boolean;
    FOnTimer: TNotifyEvent;
    FTimeout: integer;
    procedure SyncOnTimer;
  public
    constructor Create(AOnTimer: TNotifyEvent; ATimeout: integer = 1000);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{ TLazyThreadTimer }

constructor TLazyThreadTimer.Create(AOnTimer: TNotifyEvent;
  ATimeout: integer = 1000);
begin
  FOnTimer := AOnTimer;
  FTimeout := ATimeout;
  inherited Create(false);
  FreeOnTerminate := True;
end;

destructor TLazyThreadTimer.Destroy;
begin
  FActive := false;
  inherited;
end;

procedure TLazyThreadTimer.SyncOnTimer;
begin
  if Assigned(FOnTimer) and (FActive) then
  begin
    try
      FOnTimer(Self);
    except
    end;
  end;
end;

procedure TLazyThreadTimer.Execute;
begin
  FActive := True;
  while (not Terminated) do
  begin
    try
      if not Terminated then
      begin
        Synchronize(SyncOnTimer);
      end;
      WaitForSingleObject(Self.Handle, FTimeout);
    except

    end;
  end;
end;

end.
