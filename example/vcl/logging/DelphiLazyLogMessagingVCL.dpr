program DelphiLazyLogMessagingVCL;

uses
  Vcl.Forms,
  MessagingExampleFormU in 'MessagingExampleFormU.pas' {frmMessagingExample};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMessagingExample, frmMessagingExample);
  Application.Run;
end.

