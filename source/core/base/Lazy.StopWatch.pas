unit Lazy.StopWatch;

interface

uses
  Lazy.Types, Lazy.Utils,
  System.SysUtils, System.DateUtils, System.Generics.Collections;

type
  TLZStopWatch = class;

  TLZTimeElapsedEntry = class(TLZObject)
  private
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    function GetElapsedMilliseconds: Int64;
    function GetIsRunning: boolean;
  protected
    procedure Start;
    procedure Stop;
  public
    constructor Create;
    property IsRunning: boolean read GetIsRunning;
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
  end;

  TLZTimeElapsed = TObjectList<TLZTimeElapsedEntry>;

  TLZStopWatch = class(TLZObject)
  private
    FTimeElapsed: TLZTimeElapsed;
    FActiveTimeElapsedEntry: TLZTimeElapsedEntry;
    FElapsedFormat: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    function GetElapsed: string;
    function GetIsRunning: boolean;
    function GetIsPaused: boolean;
    function GetElapsedEntry(AIndex: integer): TLZTimeElapsedEntry;
    procedure SetElapsedFormat(const Value: string);
    function GetStatus: string;
    function GetRunTimeMilliseconds: Int64;
    function GetRunTimeElapsed: string;
  protected
    function GetCount: integer;
    function GetElapsedMilliseconds: Int64;
  public
    constructor Create(const AStartOnCreate: boolean = false);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;
    procedure Reset;
    property Count: integer read GetCount;
    property ElapsedEntry[AIndex: integer]: TLZTimeElapsedEntry
      read GetElapsedEntry;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property RuntimeMilliseconds: Int64 read GetRunTimeMilliseconds;
    property Elapsed: string read GetElapsed;
    property RuntimeElapsed: string read GetRunTimeElapsed;
    property ElapsedFormat: string read FElapsedFormat write SetElapsedFormat;
    property Status: string read GetStatus;
    property IsRunning: boolean read GetIsRunning;
    property IsPaused: boolean read GetIsPaused;
    property StartTime: TDateTime read FStartTime;
    property EndTime: TDateTime read FEndTime;
  end;

implementation

constructor TLZStopWatch.Create(const AStartOnCreate: boolean);
begin
  inherited Create;
  FTimeElapsed := TLZTimeElapsed.Create;
  FActiveTimeElapsedEntry := nil;
  FStartTime := 0;
  FEndTime := 0;
  FElapsedFormat := 'hh:nn:ss.z';
  if AStartOnCreate then
    Start;
end;

function TLZStopWatch.GetElapsedMilliseconds: Int64;
var
  LEntry: TLZTimeElapsedEntry;
begin
  result := 0;
  for LEntry in FTimeElapsed do
  begin
    result := result + LEntry.GetElapsedMilliseconds;
  end;
end;

function TLZStopWatch.GetIsPaused: boolean;
begin
  result := false;
  if IsRunning then
  begin
    result := not FActiveTimeElapsedEntry.IsRunning;
  end;
end;

function TLZStopWatch.GetIsRunning: boolean;
begin
  result := Assigned(FActiveTimeElapsedEntry);
end;

function TLZStopWatch.GetRunTimeElapsed: string;
begin
  result := TLZDateTime.DurationFromMilliseconds(RuntimeMilliseconds, false,
    FElapsedFormat);
end;

function TLZStopWatch.GetRunTimeMilliseconds: Int64;
var
  LEndTime: TDateTime;
begin
  LEndTime := FEndTime;
  if LEndTime = 0 then
    LEndTime := Now;
  result := MilliSecondsBetween(LEndTime, FStartTime);
end;

function TLZStopWatch.GetStatus: string;
begin
  result := 'Stopped';
  if IsRunning then
  begin
    result := 'Running';
    if IsPaused then
      result := 'Paused';
  end;
end;

procedure TLZStopWatch.Pause;
begin
  if IsRunning then
  begin
    FActiveTimeElapsedEntry.Stop;
  end;
end;

procedure TLZStopWatch.Reset;
begin
  Stop;
  FStartTime := Now;
  FEndTime := 0;
  FTimeElapsed.Clear;
end;

procedure TLZStopWatch.Resume;
begin
  if Assigned(FActiveTimeElapsedEntry) then
  begin
    if FActiveTimeElapsedEntry.IsRunning then
      FActiveTimeElapsedEntry.Stop;
  end;
  FActiveTimeElapsedEntry := TLZTimeElapsedEntry.Create;
  FTimeElapsed.Add(FActiveTimeElapsedEntry);
  FActiveTimeElapsedEntry.Start;
end;

destructor TLZStopWatch.Destroy;
begin
  try
    Reset;
    FreeAndNil(FTimeElapsed);
  finally
    inherited;
  end;
end;

function TLZStopWatch.GetCount: integer;
begin
  result := FTimeElapsed.Count;
end;

function TLZStopWatch.GetElapsed: string;
begin
  result := TLZDateTime.DurationFromMilliseconds(ElapsedMilliseconds, false,
    FElapsedFormat);
end;

function TLZStopWatch.GetElapsedEntry(AIndex: integer): TLZTimeElapsedEntry;
begin
  result := FTimeElapsed.Items[AIndex];
end;

procedure TLZStopWatch.SetElapsedFormat(const Value: string);
begin
  FElapsedFormat := Value;
end;

procedure TLZStopWatch.Start;
begin
  if not IsRunning then
  begin
    Reset;
    Resume;
  end;
end;

procedure TLZStopWatch.Stop;
begin
  if IsRunning then
  begin
    FEndTime := Now;
    FActiveTimeElapsedEntry.Stop;
    FActiveTimeElapsedEntry := nil;
  end;
end;

{ TTimeElapsedEntry }

constructor TLZTimeElapsedEntry.Create;
begin
  FStartTime := 0;
  FEndTime := 0;
end;

function TLZTimeElapsedEntry.GetElapsedMilliseconds: Int64;
var
  LEndTime: TDateTime;
begin
  LEndTime := FEndTime;
  if LEndTime = 0 then
    LEndTime := Now;
  result := MilliSecondsBetween(LEndTime, FStartTime);
end;

function TLZTimeElapsedEntry.GetIsRunning: boolean;
begin
  result := FEndTime = 0;
end;

procedure TLZTimeElapsedEntry.Start;
begin
  FStartTime := Now;
end;

procedure TLZTimeElapsedEntry.Stop;
begin
  FEndTime := Now;
end;

end.
