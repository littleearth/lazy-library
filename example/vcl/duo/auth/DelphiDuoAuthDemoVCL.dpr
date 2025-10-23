program DelphiDuoAuthDemoVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmAuthDemoU in 'frmAuthDemoU.pas' {frmAuthDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiDuoAuthDemoVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Delphi DUO Auth Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAuthDemo, frmAuthDemo);
  Application.Run;

end.
