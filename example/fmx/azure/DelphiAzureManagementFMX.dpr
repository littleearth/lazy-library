program DelphiAzureManagementFMX;

uses
  System.StartUpCopy,
  Lazy.Log,
  FMX.Forms,
  FMX.AzureDemo in 'FMX.AzureDemo.pas' {frmAzureDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiAzureManagementFMX';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := TRue;
  Application.Initialize;
  Application.CreateForm(TfrmAzureDemo, frmAzureDemo);
  Application.Run;

end.
