program DelphiOAuthVCL;

uses
  Lazy.Log,
  Vcl.Forms,
  VCL.OAuthDemo in 'VCL.OAuthDemo.pas' {frmOAuthDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiOAuthVCL';
end;

begin
  InitialiseLog;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOAuthDemo, frmOAuthDemo);
  Application.Run;

end.
