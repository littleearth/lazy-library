program DelphiSystemInformationVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmSystemInformationU in 'frmSystemInformationU.pas' {frmSystemInformation};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiSystemInformationVCL';
end;

begin
  InitialiseLog;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSystemInformation, frmSystemInformation);
  Application.Run;

end.
