program DelphiSystemInformationVCL;

uses
  Vcl.Forms,
  frmSystemInformationU in 'frmSystemInformationU.pas' {frmSystemInformation};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSystemInformation, frmSystemInformation);
  Application.Run;
end.
