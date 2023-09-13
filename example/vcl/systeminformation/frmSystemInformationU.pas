unit frmSystemInformationU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Winapi.SystemInformation, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmSystemInformation = class(TForm)
    memoSystemInformation: TMemo;
    TimerSystemInformation: TTimer;
    GridPanel1: TGridPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    ProgressBarMemoryLoad: TProgressBar;
    Label2: TLabel;
    ProgressBarCPULoad: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure TimerSystemInformationTimer(Sender: TObject);
  private
    FSystemInformation: TSystemInformation;
  public
    { Public declarations }
  end;

var
  frmSystemInformation: TfrmSystemInformation;

implementation

{$R *.dfm}

procedure TfrmSystemInformation.FormActivate(Sender: TObject);
begin
  TimerSystemInformation.Enabled := TRue;
  TimerSystemInformationTimer(Sender);
end;

procedure TfrmSystemInformation.FormCreate(Sender: TObject);
begin
  FSystemInformation := TSystemInformation.Create(Self);
end;

procedure TfrmSystemInformation.TimerSystemInformationTimer(Sender: TObject);
begin
  memoSystemInformation.Lines.Text := FSystemInformation.SimpleReport;
  ProgressBarMemoryLoad.Position := FSystemInformation.MemoryLoad;
  ProgressBarCPULoad.Position := Round(FSystemInformation.CPUUsage);
end;

end.
