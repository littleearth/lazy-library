program DelphiStopWatchVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  frmStopWatchU in 'frmStopWatchU.pas' {frmStopWatch};

{$R *.res}

begin
  SetLazyLogClass(TLazyLogBasic);
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmStopWatch, frmStopWatch);
  Application.Run;

end.
