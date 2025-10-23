{ -----------------------------------------------------------------------------
  Program Name: DelphiLazyLogVCL
  Author: Tristan Marlow
  Purpose: Demonstration application for TLazyLog logging framework

  Features:
  - Multiple log handlers (File, Memory, SQLite)
  - Real-time log display
  - Random message generation
  - Multi-threaded action logging

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions
  ---------------------------------------------------------------------------- }
program DelphiLazyLogVCL;

uses
  Vcl.Forms,
  FireDAC.VCLUI.Wait,
  FireDAC.Phys.SQLite,
  frmLazyLogDemoU in 'frmLazyLogDemoU.pas' {frmLazyLogDemo},
  Lazy.Log in '..\..\..\source\core\log\Lazy.Log.pas',
  Lazy.Log.SQLite in '..\..\..\source\core\log\Lazy.Log.SQLite.pas',
  Lazy.Log.Memory in '..\..\..\source\core\log\Lazy.Log.Memory.pas',
  Lazy.Log.FileStream in '..\..\..\source\core\log\Lazy.Log.FileStream.pas',
  CustomLogHandler in 'CustomLogHandler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLazyLogDemo, frmLazyLogDemo);
  Application.Run;
end.
