program DelphiAzureManagementVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  Vcl.AzureDemo in 'VCL.AzureDemo.pas' {frmAzureDemo};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  SetLazyLogClass(TLZLogBasic);
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAzureDemo, frmAzureDemo);
  Application.Run;

end.
