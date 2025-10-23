program DelphiExceptionDetailsVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  frmExceptionDetailsExampleU in 'frmExceptionDetailsExampleU.pas' {frmExceptionDetailsExample};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiExceptionDetailsVCL';
end;

begin
  InitialiseLog;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmExceptionDetailsExample, frmExceptionDetailsExample);
  Application.Run;
end.

