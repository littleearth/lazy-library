unit frmStopWatchU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Lazy.StopWatch, Vcl.ComCtrls,
  Vcl.AppEvnts;

type
  TfrmStopWatch = class(TForm)
    GridPanel1: TGridPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ActionList: TActionList;
    ActionStart: TAction;
    ActionStop: TAction;
    ActionPause: TAction;
    ActionResume: TAction;
    pnlElapsedTime: TPanel;
    ListViewElapsedEntries: TListView;
    Timer: TTimer;
    ApplicationEvents: TApplicationEvents;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Panel1: TPanel;
    cbStayOnTop: TCheckBox;
    ActionStayOnTop: TAction;
    cbPauseOnLostFocus: TCheckBox;
    lblRuntimeDuration: TLabel;
    lblStarted: TLabel;
    lblEntryCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionStopUpdate(Sender: TObject);
    procedure ActionStartUpdate(Sender: TObject);
    procedure ActionStartExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionResumeExecute(Sender: TObject);
    procedure ActionResumeUpdate(Sender: TObject);
    procedure ActionPauseUpdate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ApplicationEventsDeactivate(Sender: TObject);
    procedure ApplicationEventsActivate(Sender: TObject);
    procedure ActionStayOnTopUpdate(Sender: TObject);
    procedure ActionStayOnTopExecute(Sender: TObject);
  private
    FStopWatch: TLZStopWatch;
    procedure UpdateElapsedEntries;
    procedure UpdateStatus;
  public
    { Public declarations }
  end;

var
  frmStopWatch: TfrmStopWatch;

implementation

{$R *.dfm}

uses
  Lazy.Log, VCL.Lazy.Utils.Windows;

procedure TfrmStopWatch.ActionPauseExecute(Sender: TObject);
begin
  FStopWatch.Pause;
  UpdateStatus;
end;

procedure TfrmStopWatch.ActionPauseUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FStopWatch.IsRunning) and
    (not FStopWatch.IsPaused);
end;

procedure TfrmStopWatch.ActionResumeExecute(Sender: TObject);
begin
  FStopWatch.Resume;
  UpdateStatus;
end;

procedure TfrmStopWatch.ActionResumeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FStopWatch.IsPaused;
end;

procedure TfrmStopWatch.ActionStartExecute(Sender: TObject);
begin
  FStopWatch.Start;
  UpdateStatus;
end;

procedure TfrmStopWatch.ActionStartUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not FStopWatch.IsRunning;
end;

procedure TfrmStopWatch.ActionStayOnTopExecute(Sender: TObject);
begin
  if FormStyle = fsStayOnTop then
  begin
    FormStyle := fsNormal;
  end
  else
  begin
    FormStyle := fsStayOnTop;
  end;
end;

procedure TfrmStopWatch.ActionStayOnTopUpdate(Sender: TObject);
begin
  ActionStayOnTop.Checked := FormStyle = fsStayOnTop;
end;

procedure TfrmStopWatch.ActionStopExecute(Sender: TObject);
begin
  FStopWatch.Stop;
  UpdateStatus;
end;

procedure TfrmStopWatch.ActionStopUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FStopWatch.IsRunning;
end;

procedure TfrmStopWatch.ApplicationEventsActivate(Sender: TObject);
begin
  if cbPauseOnLostFocus.Checked then
  begin
    ActionResume.Execute;
  end;
end;

procedure TfrmStopWatch.ApplicationEventsDeactivate(Sender: TObject);
begin
  if cbPauseOnLostFocus.Checked then
  begin
    ActionPause.Execute;
  end;
end;

procedure TfrmStopWatch.FormCreate(Sender: TObject);
begin
  FStopWatch := TLZStopWatch.Create;
  FStopWatch.ElapsedFormat := 'hh:nn:ss';
  UpdateStatus;
end;

procedure TfrmStopWatch.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FStopWatch);
end;

procedure TfrmStopWatch.TimerTimer(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmStopWatch.UpdateStatus;
begin
  if FStopWatch.IsRunning then
  begin
    lblRuntimeDuration.Caption := Format('Run time: %s',
      [FStopWatch.RuntimeElapsed]);
    lblStarted.Caption := TLZDateTime.DateTimeToString(FStopWatch.StartTime);
    lblEntryCount.Caption := Format('Entry count: %d', [FStopWatch.Count]);
    if FStopWatch.IsPaused then
    begin
      pnlElapsedTime.Color := $007A7AFE;
    end
    else
    begin
      pnlElapsedTime.Color := $00A9D83B;
    end;
  end
  else
  begin
    lblRuntimeDuration.Caption := '';
    lblStarted.Caption := '';
    lblEntryCount.Caption := '';
    pnlElapsedTime.Color := clBtnFace;
  end;
  pnlElapsedTime.Caption := Format('Status: %s, Elapsed: %s',
    [FStopWatch.Status, FStopWatch.Elapsed]);
  Application.ProcessMessages;
  UpdateElapsedEntries;
end;

procedure TfrmStopWatch.UpdateElapsedEntries;
var
  LIdx: integer;
  LListitem: TListItem;
begin
  ListViewElapsedEntries.Clear;
  ListViewElapsedEntries.Items.BeginUpdate;
  try
    for LIdx := 0 to Pred(FStopWatch.Count) do
    begin
      LListitem := ListViewElapsedEntries.Items.Add;
      LListitem.Caption := DateTimeToStr(FStopWatch.ElapsedEntry[LIdx]
        .StartTime);
      if FStopWatch.ElapsedEntry[LIdx].EndTime <> 0 then
      begin
        LListitem.SubItems.Add
          (DateTimeToStr(FStopWatch.ElapsedEntry[LIdx].EndTime));
      end
      else
      begin
        LListitem.SubItems.Add('Active');
      end;
      LListitem.SubItems.Add(TLZDateTime.DurationFromMilliseconds(FStopWatch.ElapsedEntry
        [LIdx].ElapsedMilliseconds, false, FStopWatch.ElapsedFormat));
    end;
  finally
    ListViewElapsedEntries.Items.EndUpdate;
  end;
end;

end.
