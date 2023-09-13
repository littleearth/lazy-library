program DelphiDuoAccountsDemoVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  frmAccountsDemoU in 'frmAccountsDemoU.pas' {frmAccountsDemo};

{$R *.res}

begin
  SetLazyLogClass(TLZLogBasic);
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Delphi DUO Accounts Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAccountsDemo, frmAccountsDemo);
  Application.Run;

end.
