program DelphiPWAVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmPWAExampleU in 'frmPWAExampleU.pas' {frmPWAExample};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiPWAVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPWAExample, frmPWAExample);
  Application.Run;

end.
