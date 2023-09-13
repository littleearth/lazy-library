program DelphiAzureManagementFMX;

uses
  System.StartUpCopy,
  Lazy.Log,
  Lazy.Log.Basic,
  FMX.Forms,
  FMX.AzureDemo in 'FMX.AzureDemo.pas' {frmAzureDemo};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := TRue;
  SetLazyLogClass(TLazyLogBasic);
  Application.Initialize;
  Application.CreateForm(TfrmAzureDemo, frmAzureDemo);
  Application.Run;

end.
