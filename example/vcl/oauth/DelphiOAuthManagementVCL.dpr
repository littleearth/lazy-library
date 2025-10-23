program DelphiOAuthManagementVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  Vcl.OAuthDemo in 'VCL.OAuthDemo.pas' {frmOAuthDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiOAuthManagementVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOAuthDemo, frmOAuthDemo);
  Application.Run;

end.
