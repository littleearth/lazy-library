unit frmVSSCopyU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.UITypes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Mask, Winapi.VSS.Copy, System.Actions, Vcl.ActnList;

type
  TfrmVSSCopy = class(TForm)
    BackupProgressBar: TProgressBar;
    BackupLogMemo: TMemo;
    lblProgress: TLabel;
    GridPanel1: TGridPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    cbRecursive: TCheckBox;
    btnBackup: TBitBtn;
    GridPanel2: TGridPanel;
    editDestination: TEdit;
    BitBtn1: TBitBtn;
    ActionList: TActionList;
    FileOpenDialog: TFileOpenDialog;
    ActionBrowseDestination: TAction;
    GridPanel3: TGridPanel;
    editSource: TEdit;
    BitBtn2: TBitBtn;
    editFileMask: TEdit;
    ActionBrowseSource: TAction;
    procedure btnBackupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionBrowseDestinationExecute(Sender: TObject);
    procedure ActionBrowseSourceExecute(Sender: TObject);
  private
    FVSSCopy: TLZVSSCopy;

    procedure OnProgress(ASender: TObject; AProgress: integer; AMessage: string;
      var ACancel: boolean);
    procedure OnLog(ASender: TObject; AMessage: string);
  public
    { Public declarations }
  end;

var
  frmVSSCopy: TfrmVSSCopy;

implementation

{$R *.dfm}

uses
  Vcl.Lazy.Utils.Windows;

procedure TfrmVSSCopy.ActionBrowseDestinationExecute(Sender: TObject);
begin
  if FileOpenDialog.Execute then
  begin
    editDestination.Text := IncludeTrailingPathDelimiter
      (FileOpenDialog.FileName);
  end;

end;

procedure TfrmVSSCopy.ActionBrowseSourceExecute(Sender: TObject);
begin
  if FileOpenDialog.Execute then
  begin
    editSource.Text := IncludeTrailingPathDelimiter(FileOpenDialog.FileName);
  end;
end;

procedure TfrmVSSCopy.btnBackupClick(Sender: TObject);
begin
  if FVSSCopy.Backup(IncludeTrailingPathDelimiter(editSource.Text),
    editFileMask.Text, cbRecursive.Checked,
    IncludeTrailingPathDelimiter(editDestination.Text)) then
  begin
    MessageDlg('Backup complete.', mtInformation, [mbOK], 0);
  end
  else
  begin
    MessageDlg('Backup failed.', mtError, [mbOK], 0);
  end;
end;

procedure TfrmVSSCopy.FormCreate(Sender: TObject);
begin
  BackupLogMemo.Lines.Clear;
  FVSSCopy := TLZVSSCopy.Create;
  FVSSCopy.OnProgress := OnProgress;
  FVSSCopy.OnLog := OnLog;
  editSource.Text := TLZFile.GetPicturesFolder;
  editDestination.Text := IncludeTrailingPathDelimiter
    (TLZFile.GetPublicDocumentFolder + 'Backup');
end;

procedure TfrmVSSCopy.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVSSCopy);
end;

procedure TfrmVSSCopy.OnLog(ASender: TObject; AMessage: string);
begin
  BackupLogMemo.Lines.BeginUpdate;
  try
    BackupLogMemo.Lines.Insert(0, AMessage);
  finally
    BackupLogMemo.Lines.EndUpdate;
  end;
end;

procedure TfrmVSSCopy.OnProgress(ASender: TObject; AProgress: integer;
  AMessage: string; var ACancel: boolean);
begin
  BackupProgressBar.Position := AProgress;
  lblProgress.Caption := AMessage;
  Application.ProcessMessages;
end;

end.
