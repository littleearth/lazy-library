program DelphiUniFiClientVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmUniFiClientU in 'frmUniFiClientU.pas' {frmUniFiClient};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiUniFiClientVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmUniFiClient, frmUniFiClient);
  Application.Run;

end.
