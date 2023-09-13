program DelphiVSSCopyVCL;

uses
  Lazy.Log,
  Lazy.Log.Basic,
  Vcl.Forms,
  System.SysUtils,
  frmVSSCopyU in 'frmVSSCopyU.pas' {frmVSSCopy};

{$R *.res}

begin
  SetLazyLogClass(TLZLogBasic);
  Application.Initialize;
  Application.Title := 'VSS Copy Example';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVSSCopy, frmVSSCopy);
  Application.Run;

end.
