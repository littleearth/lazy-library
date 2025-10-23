program DelphiCSVJSONConverter;

uses
  Lazy.Log,
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiCSVJSONConverter';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;

end.
