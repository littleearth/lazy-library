program DelphiSystemInformationVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  frmSystemInformationU in 'frmSystemInformationU.pas' {frmSystemInformation};

{$R *.res}

begin
  SetLazyLogClass(TLZLogBasic);
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSystemInformation, frmSystemInformation);
  Application.Run;

end.
