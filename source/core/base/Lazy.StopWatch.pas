unit Lazy.StopWatch;

interface

uses
  System.SysUtils, System.DateUtils;

type
  TLazyStopWatch = class
  private
    fFrequency: Int64;
    fIsRunning: boolean;
    fStartCount, fStopCount: Int64;
    procedure SetTickStamp(var lInt: Int64);
    function GetElapsedTicks: Int64;
    function GetElapsedMilliseconds: Int64;
    function GetElapsed: string;
  public
    constructor Create(const startOnCreate: boolean = false);
    procedure Start;
    procedure Stop;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property Elapsed: string read GetElapsed;
    property IsRunning: boolean read fIsRunning;
  end;

implementation

constructor TLazyStopWatch.Create(const startOnCreate: boolean = false);
begin
  inherited Create;
  fIsRunning := false;
  fFrequency := MSecsPerSec;
  if startOnCreate then
    Start;
end;

function TLazyStopWatch.GetElapsedTicks: Int64;
begin
  result := fStopCount - fStartCount;
end;

procedure TLazyStopWatch.SetTickStamp(var lInt: Int64);
begin
  lInt := MilliSecondOf(Now);
end;

function TLazyStopWatch.GetElapsed: string;
var
  dt: TDateTime;
begin
  dt := ElapsedMilliseconds / MSecsPerSec / SecsPerDay;
  result := Format('%d days, %s', [trunc(dt), FormatDateTime('hh:nn:ss.z',
    Frac(dt))]);
end;

function TLazyStopWatch.GetElapsedMilliseconds: Int64;
begin
  result := (MSecsPerSec * (fStopCount - fStartCount)) div fFrequency;
end;

procedure TLazyStopWatch.Start;
begin
  SetTickStamp(fStartCount);
  fIsRunning := true;
end;

procedure TLazyStopWatch.Stop;
begin
  SetTickStamp(fStopCount);
  fIsRunning := false;
end;

end.
