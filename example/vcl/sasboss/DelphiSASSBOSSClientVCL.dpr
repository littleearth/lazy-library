program DelphiSASSBOSSClientVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmSASBOSSClientU in 'frmSASBOSSClientU.pas' {frmSASBOSSClient};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiSASSBOSSClientVCL';
end;

begin
  InitialiseLog;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSASBOSSClient, frmSASBOSSClient);
  Application.Run;
end.
