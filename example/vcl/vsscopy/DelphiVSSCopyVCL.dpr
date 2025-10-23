program DelphiVSSCopyVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  System.SysUtils,
  frmVSSCopyU in 'frmVSSCopyU.pas' {frmVSSCopy};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiVSSCopyVCL';
end;

begin
  InitialiseLog;
  Application.Initialize;
  Application.Title := 'VSS Copy Example';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVSSCopy, frmVSSCopy);
  Application.Run;

end.
