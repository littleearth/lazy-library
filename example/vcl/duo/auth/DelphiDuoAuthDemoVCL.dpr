program DelphiDuoAuthDemoVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  frmAuthDemoU in 'frmAuthDemoU.pas' {frmAuthDemo};

{$R *.res}

begin
  SetLazyLogClass(TLazyLogBasic);
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Delphi DUO Auth Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAuthDemo, frmAuthDemo);
  Application.Run;

end.
