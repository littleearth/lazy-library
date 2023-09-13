program DelphiPWAVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  frmPWAExampleU in 'frmPWAExampleU.pas' {frmPWAExample};

{$R *.res}

begin
  SetLazyLogClass(TLazyLogBasic);
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPWAExample, frmPWAExample);
  Application.Run;

end.
