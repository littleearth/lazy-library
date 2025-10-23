program DelphiAzureManagementVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  Vcl.AzureDemo in 'VCL.AzureDemo.pas' {frmAzureDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiAzureManagementVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAzureDemo, frmAzureDemo);
  Application.Run;

end.
