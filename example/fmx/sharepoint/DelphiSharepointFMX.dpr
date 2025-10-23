program DelphiSharepointFMX;

uses
  System.StartUpCopy,
  Lazy.Log,
  FMX.Forms,
  FMX.SharePointDemo in 'FMX.SharePointDemo.pas' {frmSharepointDemo};

{$R *.res}

procedure InitialiseLog;
begin
  LazyLog.ApplicationName := 'DelphiSharepointFMX';
end;

begin
  InitialiseLog;
  Application.Initialize;
  Application.CreateForm(TfrmSharepointDemo, frmSharepointDemo);
  Application.Run;
end.
