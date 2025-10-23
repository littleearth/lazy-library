program DelphiStopWatchVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmStopWatchU in 'frmStopWatchU.pas' {frmStopWatch};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiStopWatchVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmStopWatch, frmStopWatch);
  Application.Run;

end.
