program DelphiMSGraphManagementVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  Vcl.MSGraphDemo in 'VCL.MSGraphDemo.pas' {frmMSGraphDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiMSGraphManagementVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMSGraphDemo, frmMSGraphDemo);
  Application.Run;

end.
