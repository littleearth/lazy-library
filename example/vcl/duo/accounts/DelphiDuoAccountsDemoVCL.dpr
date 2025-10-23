program DelphiDuoAccountsDemoVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmAccountsDemoU in 'frmAccountsDemoU.pas' {frmAccountsDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiDuoAccountsDemoVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Title := 'Delphi DUO Accounts Demo';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAccountsDemo, frmAccountsDemo);
  Application.Run;

end.
