unit VCL.Lazy.Dialogs;

interface

uses
  Lazy.Types, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.UITypes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, Data.DB, VCL.DBCtrls, VCL.Lazy.MemoForm;

type

  TLZMemoFormDialogMode = VCL.Lazy.MemoForm.TLZMemoFormDialogMode;

  TLZDialogs = class
  protected
    class function GetOwner(AOwner: TObject): TComponent; overload;
    class function GetOwner(AOwner: TObject; var AHandle: THandle)
      : TComponent; overload;
    class function GetActiveFormOwner: TComponent;
    class function GetMainFormOwner: TComponent;
  public
    class function MessageDlgTimed(const AMessage: string; ATimeout: integer;
      ADefault: integer; AMsgDlgType: TMsgDlgType; AButtons: TMsgDlgButtons;
      AHelpCtx: Longint): integer;
    class function ErrorMessage(AException: Exception; AMessage: string = '';
      AButtons: TMsgDlgButtons = [mbOk]): integer; overload;
    class function ErrorMessage(AMessage: string; const AArgs: array of const;
      AButtons: TMsgDlgButtons = [mbOk]): integer; overload;
    class function ErrorMessage(AMessage: string;
      AButtons: TMsgDlgButtons = [mbOk]): integer; overload;
    class function ErrorMessageTimed(AMessage: string; ATimeout: integer = 30;
      ADefault: integer = mrOk; AButtons: TMsgDlgButtons = [mbOk]): integer;
    class function WarningMessage(AMessage: string; const AArgs: array of const;
      AButtons: TMsgDlgButtons = [mbYes, mbNo]): integer; overload;
    class function WarningMessage(AMessage: string;
      AButtons: TMsgDlgButtons = [mbYes, mbNo]): integer; overload;
    class function WarningMessageTimed(AMessage: string; ATimeout: integer = 30;
      ADefault: integer = mrYes; AButtons: TMsgDlgButtons = [mbYes, mbNo]
      ): integer;
    class function ConfirmationMessage(AMessage: string;
      const AArgs: array of const; AButtons: TMsgDlgButtons = [mbYes, mbNo])
      : integer; overload;
    class function ConfirmationMessage(AMessage: string;
      AButtons: TMsgDlgButtons = [mbYes, mbNo]): integer; overload;
    class function ConfirmationMessageTimed(AMessage: string;
      ATimeout: integer = 30; ADefault: boolean = true): boolean;
    class function InformationMessage(AMessage: string;
      const AArgs: array of const; AButtons: TMsgDlgButtons = [mbOk])
      : integer; overload;
    class procedure StatusMessage(AMessage: string; const AArgs: array of const;
      ASuccess: boolean = true); overload;
    class procedure StatusMessage(AMessage: string;
      ASuccess: boolean = true); overload;
    class function InformationMessage(AMessage: string;
      AButtons: TMsgDlgButtons = [mbYes, mbNo]): integer; overload;
    class function SaveDialog(var AFileName: TFileName; AFilter: string;
      ADefaultExt: string; AOwner: TComponent = nil;
      ATitle: string = ''): boolean;
    class function OpenDialog(var AFileName: TFileName;
      AFilter, ADefaultExt: string; AOwner: TComponent = nil;
      ATitle: string = ''): boolean;
    class function FindReplace(ACustomEdit: TCustomEdit): boolean;
    class procedure MemoDialogShow(AText: TStrings; ACaption: string = '';
      AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal); overload;
    class procedure MemoDialogShow(AText: string; ACaption: string = '';
      AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal); overload;
    class procedure MemoDialogShowFile(AFileName: TFileName;
      AMessage: string = ''; ADialogMode: TLZMemoFormDialogMode = dmModal);
  end;

implementation

uses
  Lazy.Log,
  Lazy.Utils.Windows,
  Winapi.MessageDlgTimed,
  VCL.Lazy.FindReplace;

class function TLZDialogs.ErrorMessage(AMessage: string;
  const AArgs: array of const; AButtons: TMsgDlgButtons): integer;
begin
  Application.NormalizeAllTopMosts;
  try
    Result := MessageDlg(Format(AMessage, AArgs), mtError, AButtons, 0);
    LazyLog.Error('TLZDialogs', Format(AMessage, AArgs));
  finally
    Application.RestoreTopMosts;
  end;
end;

class function TLZDialogs.SaveDialog(var AFileName: TFileName;
  AFilter, ADefaultExt: string; AOwner: TComponent; ATitle: string): boolean;
var
  LSaveDialog: TSaveDialog;
  LOwner: TComponent;
  LHandle: THandle;
begin
  Result := false;

  LOwner := GetOwner(AOwner, LHandle);
  Application.NormalizeAllTopMosts;
  LSaveDialog := TSaveDialog.Create(LOwner);
  try
    LSaveDialog.FileName := AFileName;
    LSaveDialog.Filter := AFilter;
    LSaveDialog.DefaultExt := ADefaultExt;
    if not TLZString.IsEmptyString(ATitle) then
      LSaveDialog.Title := ATitle;
    if LSaveDialog.Execute(LHandle) then
    begin
      AFileName := LSaveDialog.FileName;
      Result := true;
    end;
  finally
    FreeAndNil(LSaveDialog);
    Application.RestoreTopMosts;
  end;

end;

class function TLZDialogs.ConfirmationMessage(AMessage: string;
  AButtons: TMsgDlgButtons): integer;
begin
  Result := ConfirmationMessage(AMessage, [], AButtons);
end;

class function TLZDialogs.ConfirmationMessageTimed(AMessage: string;
  ATimeout: integer; ADefault: boolean): boolean;
var
  LDefault: integer;
begin
  LDefault := mrNo;
  if ADefault then
    LDefault := mrYes;
  Result := TLZMessageDialogTimed.Confirmation(AMessage, ATimeout,
    LDefault) = mrYes;
end;

class function TLZDialogs.ConfirmationMessage(AMessage: string;
  const AArgs: array of const; AButtons: TMsgDlgButtons): integer;
begin
  Application.NormalizeAllTopMosts;
  try
    Result := MessageDlg(Format(AMessage, AArgs), mtConfirmation, AButtons, 0);
  finally
    Application.RestoreTopMosts;
  end;
end;

class function TLZDialogs.ErrorMessage(AMessage: string;
  AButtons: TMsgDlgButtons): integer;
begin
  Result := ErrorMessage(AMessage, [], AButtons);
end;

class function TLZDialogs.ErrorMessageTimed(AMessage: string;
  ATimeout, ADefault: integer; AButtons: TMsgDlgButtons): integer;
begin
  Result := TLZMessageDialogTimed.Show(AMessage, ATimeout, ADefault, mtError,
    AButtons, 0);
end;

class function TLZDialogs.GetActiveFormOwner: TComponent;
begin
  Result := GetOwner(nil);
end;

class function TLZDialogs.GetMainFormOwner: TComponent;
begin
  Result := GetOwner(Application.MainForm);
end;

class function TLZDialogs.GetOwner(AOwner: TObject; var AHandle: THandle)
  : TComponent;
begin
  AHandle := 0;
  if Assigned(AOwner) and (AOwner is TComponent) then
  begin
    Result := AOwner as TComponent;
  end
  else
  begin
    Result := Screen.ActiveForm;
  end;

  if Assigned(Result) then
  begin
    if (Result is TCustomForm) then
    begin
      AHandle := (Result as TCustomForm).Handle;
    end;
  end;
end;

class function TLZDialogs.GetOwner(AOwner: TObject): TComponent;
var
  LHandle: THandle;
begin
  Result := GetOwner(AOwner, LHandle);
end;

class function TLZDialogs.ErrorMessage(AException: Exception; AMessage: string;
  AButtons: TMsgDlgButtons): integer;
begin
  Application.NormalizeAllTopMosts;
  try
    Result := MessageDlg(Format('%s %s', [AException.Message, AMessage]),
      mtError, AButtons, 0);
    LazyLog.Error('TLZDialogs', AException, AMessage);
  finally
    Application.RestoreTopMosts;
  end;
end;

class function TLZDialogs.InformationMessage(AMessage: string;
  AButtons: TMsgDlgButtons): integer;
begin
  Result := InformationMessage(AMessage, [], AButtons);
end;

class procedure TLZDialogs.MemoDialogShow(AText: TStrings;
  ACaption, AMessage: string; ADialogMode: TLZMemoFormDialogMode);
begin
  TLZMemoForm.Show(AText, ACaption, AMessage, ADialogMode);
end;

class procedure TLZDialogs.MemoDialogShow(AText, ACaption, AMessage: string;
  ADialogMode: TLZMemoFormDialogMode);
begin
  TLZMemoForm.Show(AText, ACaption, AMessage, ADialogMode);
end;

class procedure TLZDialogs.MemoDialogShowFile(AFileName: TFileName;
  AMessage: string; ADialogMode: TLZMemoFormDialogMode);
begin
  TLZMemoForm.ShowFile(AFileName, AMessage, ADialogMode);
end;

class function TLZDialogs.MessageDlgTimed(const AMessage: string;
  ATimeout: integer; ADefault: integer; AMsgDlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AHelpCtx: Longint): integer;
begin
  Result := TLZMessageDialogTimed.Show(AMessage, ATimeout, ADefault,
    AMsgDlgType, AButtons, AHelpCtx);
end;

class function TLZDialogs.OpenDialog(var AFileName: TFileName;
  AFilter, ADefaultExt: string; AOwner: TComponent; ATitle: string): boolean;
var
  LOpenDialog: TOpenDialog;
  LOwner: TComponent;
  LHandle: THandle;
begin
  Result := false;
  LOwner := GetOwner(AOwner, LHandle);
  Application.NormalizeAllTopMosts;
  LOpenDialog := TOpenDialog.Create(LOwner);
  try

    if not TLZString.IsEmptyString(ATitle) then
      LOpenDialog.Title := ATitle;

    LOpenDialog.Filter := AFilter;
    LOpenDialog.DefaultExt := ADefaultExt;
    if LOpenDialog.Execute(LHandle) then
    begin
      AFileName := LOpenDialog.FileName;
      Result := true;
    end;
  finally
    FreeAndNil(LOpenDialog);
    Application.RestoreTopMosts;
  end;
end;

class procedure TLZDialogs.StatusMessage(AMessage: string; ASuccess: boolean);
begin
  StatusMessage(AMessage, [], ASuccess);
end;

class procedure TLZDialogs.StatusMessage(AMessage: string;
  const AArgs: array of const; ASuccess: boolean);
var
  LDialogType: TMsgDlgType;
begin
  LDialogType := TMsgDlgType.mtInformation;
  if not ASuccess then
    LDialogType := TMsgDlgType.mtError;
  Application.NormalizeAllTopMosts;
  try
    MessageDlg(Format(AMessage, AArgs), LDialogType, [mbOk], 0);
  finally
    Application.RestoreTopMosts;
  end;
end;

class function TLZDialogs.InformationMessage(AMessage: string;
  const AArgs: array of const; AButtons: TMsgDlgButtons): integer;
begin
  Application.NormalizeAllTopMosts;
  try
    Result := MessageDlg(Format(AMessage, AArgs), mtInformation, AButtons, 0);
  finally
    Application.RestoreTopMosts;
  end;
end;

class function TLZDialogs.WarningMessage(AMessage: string;
  AButtons: TMsgDlgButtons): integer;
begin
  Result := WarningMessage(AMessage, [], AButtons);
end;

class function TLZDialogs.WarningMessageTimed(AMessage: string;
  ATimeout, ADefault: integer; AButtons: TMsgDlgButtons): integer;
begin
  Result := TLZMessageDialogTimed.Show(AMessage, ATimeout, ADefault, mtWarning,
    AButtons, 0);
end;

class function TLZDialogs.WarningMessage(AMessage: string;
  const AArgs: array of const; AButtons: TMsgDlgButtons): integer;
begin
  Application.NormalizeAllTopMosts;
  try
    Result := MessageDlg(Format(AMessage, AArgs), mtWarning, AButtons, 0);
  finally
    Application.RestoreTopMosts;
  end;
end;

class function TLZDialogs.FindReplace(ACustomEdit: TCustomEdit): boolean;
var
  LCustomEdit: TCustomEdit;
begin
  Result := false;
  LCustomEdit := ACustomEdit;
  if LCustomEdit = nil then
  begin
    if (Screen.ActiveControl is TCustomEdit) then
    begin
      LCustomEdit := (Screen.ActiveControl as TCustomEdit);
    end;
  end;
  if LCustomEdit <> nil then
  begin
    Application.NormalizeAllTopMosts;
    try
      Result := LazyFindReplace.Execute(LCustomEdit);
    finally
      Application.RestoreTopMosts;
    end;
  end;
end;

initialization

finalization

end.
