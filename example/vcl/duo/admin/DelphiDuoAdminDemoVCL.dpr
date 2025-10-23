program DelphiDuoAdminDemoVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmAdminDemoU in 'frmAdminDemoU.pas' {frmAdminDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiDuoAdminDemoVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Delphi DUO Admin Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAdminDemo, frmAdminDemo);
  Application.Run;

end.
