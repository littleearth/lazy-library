unit Lazy.StopWatch;

interface

uses
  Lazy.Types, Lazy.Utils,
  System.SysUtils, System.DateUtils, System.Generics.Collections;

type
  TLazyStopWatch = class;

  TTimeElapsedEntry = class(TLazyObject)
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

  TTimeElapsed = TObjectList<TTimeElapsedEntry>;

  TLazyStopWatch = class(TLazyObject)
  private
    FTimeElapsed: TTimeElapsed;
    FActiveTimeElapsedEntry: TTimeElapsedEntry;
    FElapsedFormat: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    function GetElapsed: string;
    function GetIsRunning: boolean;
    function GetIsPaused: boolean;
    function GetElapsedEntry(AIndex: integer): TTimeElapsedEntry;
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
    property ElapsedEntry[AIndex: integer]: TTimeElapsedEntry
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

constructor TLazyStopWatch.Create(const AStartOnCreate: boolean);
begin
  inherited Create;
  FTimeElapsed := TTimeElapsed.Create;
  FActiveTimeElapsedEntry := nil;
  FStartTime := 0;
  FEndTime := 0;
  FElapsedFormat := 'hh:nn:ss.z';
  if AStartOnCreate then
    Start;
end;

function TLazyStopWatch.GetElapsedMilliseconds: Int64;
var
  LEntry: TTimeElapsedEntry;
begin
  result := 0;
  for LEntry in FTimeElapsed do
  begin
    result := result + LEntry.GetElapsedMilliseconds;
  end;
end;

function TLazyStopWatch.GetIsPaused: boolean;
begin
  result := false;
  if IsRunning then
  begin
    result := not FActiveTimeElapsedEntry.IsRunning;
  end;
end;

function TLazyStopWatch.GetIsRunning: boolean;
begin
  result := Assigned(FActiveTimeElapsedEntry);
end;

function TLazyStopWatch.GetRunTimeElapsed: string;
begin
  result := TLazyDateTime.DurationFromMilliseconds(RuntimeMilliseconds, false,
    FElapsedFormat);
end;

function TLazyStopWatch.GetRunTimeMilliseconds: Int64;
var
  LEndTime: TDateTime;
begin
  LEndTime := FEndTime;
  if LEndTime = 0 then
    LEndTime := Now;
  result := MilliSecondsBetween(LEndTime, FStartTime);
end;

function TLazyStopWatch.GetStatus: string;
begin
  result := 'Stopped';
  if IsRunning then
  begin
    result := 'Running';
    if IsPaused then
      result := 'Paused';
  end;
end;

procedure TLazyStopWatch.Pause;
begin
  if IsRunning then
  begin
    FActiveTimeElapsedEntry.Stop;
  end;
end;

procedure TLazyStopWatch.Reset;
begin
  Stop;
  FStartTime := Now;
  FEndTime := 0;
  FTimeElapsed.Clear;
end;

procedure TLazyStopWatch.Resume;
begin
  if Assigned(FActiveTimeElapsedEntry) then
  begin
    if FActiveTimeElapsedEntry.IsRunning then
      FActiveTimeElapsedEntry.Stop;
  end;
  FActiveTimeElapsedEntry := TTimeElapsedEntry.Create;
  FTimeElapsed.Add(FActiveTimeElapsedEntry);
  FActiveTimeElapsedEntry.Start;
end;

destructor TLazyStopWatch.Destroy;
begin
  try
    Reset;
    FreeAndNil(FTimeElapsed);
  finally
    inherited;
  end;
end;

function TLazyStopWatch.GetCount: integer;
begin
  result := FTimeElapsed.Count;
end;

function TLazyStopWatch.GetElapsed: string;
begin
  result := TLazyDateTime.DurationFromMilliseconds(ElapsedMilliseconds, false,
    FElapsedFormat);
end;

function TLazyStopWatch.GetElapsedEntry(AIndex: integer): TTimeElapsedEntry;
begin
  result := FTimeElapsed.Items[AIndex];
end;

procedure TLazyStopWatch.SetElapsedFormat(const Value: string);
begin
  FElapsedFormat := Value;
end;

procedure TLazyStopWatch.Start;
begin
  if not IsRunning then
  begin
    Reset;
    Resume;
  end;
end;

procedure TLazyStopWatch.Stop;
begin
  if IsRunning then
  begin
    FEndTime := Now;
    FActiveTimeElapsedEntry.Stop;
    FActiveTimeElapsedEntry := nil;
  end;
end;

{ TTimeElapsedEntry }

constructor TTimeElapsedEntry.Create;
begin
  FStartTime := 0;
  FEndTime := 0;
end;

function TTimeElapsedEntry.GetElapsedMilliseconds: Int64;
var
  LEndTime: TDateTime;
begin
  LEndTime := FEndTime;
  if LEndTime = 0 then
    LEndTime := Now;
  result := MilliSecondsBetween(LEndTime, FStartTime);
end;

function TTimeElapsedEntry.GetIsRunning: boolean;
begin
  result := FEndTime = 0;
end;

procedure TTimeElapsedEntry.Start;
begin
  FStartTime := Now;
end;

procedure TTimeElapsedEntry.Stop;
begin
  FEndTime := Now;
end;

end.
