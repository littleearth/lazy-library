program DelphiDuoAdminDemoVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  frmAdminDemoU in 'frmAdminDemoU.pas' {frmAdminDemo};

{$R *.res}

begin
  SetLazyLogClass(TLZLogBasic);
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Delphi DUO Admin Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAdminDemo, frmAdminDemo);
  Application.Run;

end.
