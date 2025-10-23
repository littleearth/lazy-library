{ -----------------------------------------------------------------------------
  Unit Name: VCL.Lazy.ExceptionDialog
  Author: Tristan Marlow
  Purpose: Exception display form (dynamically created, no DFM)

  ----------------------------------------------------------------------------
  Copyright (c) 2006 Tristan David Marlow
  Copyright (c) 2006 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History:

  ----------------------------------------------------------------------------- }
unit VCL.Lazy.ExceptionDialog;

interface

uses
  Lazy.Types,
  System.UITypes, Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, VCL.Graphics, VCL.Controls,
  VCL.Forms, VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.Buttons, VCL.ComCtrls,
  Clipbrd, DateUtils, System.Generics.Collections;

type
  TLZExceptionDialogMode = (dmModal, dmNormal);

  TLZExceptionDetailsHookRef = reference to procedure(ASender: TObject;
    AException: Exception; ADetails: TStrings);

  TLZExceptionDetailsHookList = TList<TLZExceptionDetailsHookRef>;

  TLZExceptionDialog = class(TLZObject)
  private
    class var FExceptionForm: TLZExceptionDialog;
    class var FModalMode: Boolean;
    class var FExceptionDetailsHooks: TLZExceptionDetailsHookList;
  private
    FForm: TForm;
    FMemoException: TMemo;
    FMemoInformation: TMemo;
    FBottomPanel: TPanel;
    FMainPanel: TPanel;
    FContentPanel: TPanel;
    FCloseButton: TBitBtn;
    FCopyButton: TBitBtn;
    FTerminateButton: TBitBtn;
    FDialogMode: TLZExceptionDialogMode;
    FCanClose: Boolean;
    FFormActive: Boolean;
    FAllowTerminateApplication: Boolean;
    FAllowTerminateApplicationDateTime: TDateTime;
    FException: Exception;
    FSenderObject: TObject;
    FTimerUpdate: TTimer;
    procedure SetDialogMode(const AValue: TLZExceptionDialogMode);
  protected
    procedure CreateForm;
    procedure DestroyForm;
    procedure ShowForm;
    procedure ScrollToTop(AMemo: TMemo);
    procedure UpdateExceptionDetails;
    procedure UpdateTerminateButton;
    procedure OnFormShow(ASender: TObject);
    procedure OnFormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnFormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OnFormActivate(ASender: TObject);
    procedure OnCloseButtonClick(ASender: TObject);
    procedure OnCopyToClipboardClick(ASender: TObject);
    procedure OnTerminateButtonClick(ASender: TObject);
    procedure OnTimerUpdate(ASender: TObject);
    procedure DoExecute(ASender: TObject; AException: Exception;
      AAllowTerminateApplication: Boolean; ADialogMode: TLZExceptionDialogMode);
    property DialogMode: TLZExceptionDialogMode read FDialogMode
      write SetDialogMode;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    class procedure Initialize;
    class procedure Finalize;
    class procedure SetModalMode(AValue: Boolean);
    class procedure Execute(ASender: TObject; AException: Exception;
      AAllowTerminateApplication: Boolean = True); overload;
    class procedure ForceException;
    class procedure SetApplicationExceptionHandler;
    class procedure AddExceptionDetailsHook(AHook: TLZExceptionDetailsHookRef);
    class procedure RemoveExceptionDetailsHook
      (AHook: TLZExceptionDetailsHookRef);
    class procedure ClearExceptionDetailsHooks;
    class procedure OnException(ASender: TObject; AException: Exception);
  end;

implementation

uses
  Lazy.Utils.Windows,
  Lazy.Exception.Details;

{ TLZExceptionDialog }

constructor TLZExceptionDialog.Create;
begin
  inherited Create;
  FDialogMode := dmModal;
  FCanClose := False;
  FFormActive := False;
  FAllowTerminateApplication := True;
  FAllowTerminateApplicationDateTime := 0;
  FException := nil;
  FSenderObject := nil;
end;

destructor TLZExceptionDialog.Destroy;
begin
  DestroyForm;
  inherited Destroy;
end;

procedure TLZExceptionDialog.CreateForm;
begin

  FForm := TForm.Create(nil);
  with FForm do
  begin
    BorderIcons := [biSystemMenu, biMaximize];
    Caption := 'Exception details';
    Position := poScreenCenter;
    PixelsPerInch := 96;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -11;
    Font.Name := 'Tahoma';
    Font.Style := [];
    Width := 800;
    Height := 600;
    Scaled := True;
    AutoSize := False;
    AutoScroll := False;
    OnShow := OnFormShow;
    OnClose := OnFormClose;
    OnCloseQuery := OnFormCloseQuery;
    OnActivate := OnFormActivate;
    FormStyle := fsStayOnTop;
  end;

  // Bottom panel with buttons
  FBottomPanel := TPanel.Create(FForm);
  with FBottomPanel do
  begin
    AlignWithMargins := True;
    Parent := FForm;
    Align := alBottom;
    BevelOuter := bvNone;
    Height := 35;
  end;

  // Close button
  FCloseButton := TBitBtn.Create(FForm);
  with FCloseButton do
  begin
    AlignWithMargins := True;
    Parent := FBottomPanel;
    Align := alRight;
    Width := 75;
    Caption := 'Close';
    Cancel := True;
    OnClick := OnCloseButtonClick;
  end;

  // Copy to clipboard button
  FCopyButton := TBitBtn.Create(FForm);
  with FCopyButton do
  begin
    AlignWithMargins := True;
    Parent := FBottomPanel;
    Align := alLeft;
    Width := 138;
    Caption := '&Copy to clipboard';
    OnClick := OnCopyToClipboardClick;
  end;

  // Terminate button
  FTerminateButton := TBitBtn.Create(FForm);
  with FTerminateButton do
  begin
    AlignWithMargins := True;
    Parent := FBottomPanel;
    Align := alLeft;
    Width := 138;
    Caption := 'Wait';
    Enabled := False;
    OnClick := OnTerminateButtonClick;
  end;

  // Main panel
  FMainPanel := TPanel.Create(FForm);
  with FMainPanel do
  begin
    AlignWithMargins := True;
    Parent := FForm;
    Align := alClient;
    BevelOuter := bvNone;
  end;

  // Content panel
  FContentPanel := TPanel.Create(FForm);
  with FContentPanel do
  begin
    Parent := FMainPanel;
    Align := alClient;
    BevelOuter := bvNone;
  end;

  // Exception details memo
  FMemoException := TMemo.Create(FForm);
  with FMemoException do
  begin
    AlignWithMargins := True;
    Parent := FContentPanel;
    Align := alClient;
    Font.Charset := ANSI_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -12;
    Font.Name := 'Segoe UI';
    Font.Style := [];
    ReadOnly := True;
    ScrollBars := ssBoth;
    Lines.Clear;
  end;

  // Information memo
  FMemoInformation := TMemo.Create(FForm);
  with FMemoInformation do
  begin
    AlignWithMargins := True;
    Parent := FContentPanel;
    Align := alBottom;
    BorderStyle := bsNone;
    Font.Charset := ANSI_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -12;
    Font.Name := 'Segoe UI';
    Font.Style := [];
    ParentColor := True;
    ReadOnly := True;
    Height := 109;
    Lines.Clear;
  end;

  // Timer for updating terminate button
  FTimerUpdate := TTimer.Create(FForm);
  with FTimerUpdate do
  begin
    Interval := 500;
    OnTimer := OnTimerUpdate;
    Enabled := True;
  end;

  FForm.ScaleForCurrentDPI(True);
  FForm.Width := Round(FForm.Width * (Screen.PixelsPerInch / 96));
  FForm.Height := Round(FForm.Height * (Screen.PixelsPerInch / 96));

end;

procedure TLZExceptionDialog.DestroyForm;
begin
  if Assigned(FForm) then
  begin
    if FForm.Visible then
      FForm.Close;
    FreeAndNil(FForm);
  end;
end;

class procedure TLZExceptionDialog.SetApplicationExceptionHandler;
begin
  Application.OnException := TLZExceptionDialog.OnException;
end;

class procedure TLZExceptionDialog.AddExceptionDetailsHook
  (AHook: TLZExceptionDetailsHookRef);
begin
  if not Assigned(FExceptionDetailsHooks) then
    FExceptionDetailsHooks := TLZExceptionDetailsHookList.Create;

  if not FExceptionDetailsHooks.Contains(AHook) then
    FExceptionDetailsHooks.Add(AHook);
end;

class procedure TLZExceptionDialog.RemoveExceptionDetailsHook
  (AHook: TLZExceptionDetailsHookRef);
begin
  if Assigned(FExceptionDetailsHooks) then
    FExceptionDetailsHooks.Remove(AHook);
end;

class procedure TLZExceptionDialog.ClearExceptionDetailsHooks;
begin
  if Assigned(FExceptionDetailsHooks) then
    FExceptionDetailsHooks.Clear;
end;

procedure TLZExceptionDialog.SetDialogMode(const AValue
  : TLZExceptionDialogMode);
begin
  FDialogMode := AValue;
  if Assigned(FForm) and FForm.Visible then
    FForm.Close;
end;

procedure TLZExceptionDialog.ScrollToTop(AMemo: TMemo);
begin
  AMemo.SetFocus;
  AMemo.SelStart := 0;
  AMemo.SelLength := 0;
  AMemo.Perform(EM_SCROLLCARET, 0, 0);
end;


procedure TLZExceptionDialog.UpdateExceptionDetails;
var
  LExceptionDetails: TLZExceptionDetails;
  LDetailsText: string;
begin
  if not Assigned(FException) then
    Exit;

  // Use TLZExceptionDetails to extract comprehensive exception information
  LExceptionDetails := TLZExceptionDetails.Create(FException, FSenderObject, ExceptAddr);
  try
    // Get formatted details - this includes all exception information
    LDetailsText := LExceptionDetails.AsString;
    
    with FMemoException.Lines do
    begin
      Clear;
      BeginUpdate;
      try
        // Display the comprehensive exception details
        Add(LDetailsText);
      finally
        EndUpdate;
      end;
    end;

    // Execute hooks outside of BeginUpdate/EndUpdate to allow hooks to modify the memo
    // Hooks can add additional custom information if needed
    if Assigned(FExceptionDetailsHooks) then
    begin
      for var LHook in FExceptionDetailsHooks do
      begin
        if Assigned(LHook) then
        begin
          try
            LHook(FSenderObject, FException, FMemoException.Lines);
          except
            // Silently ignore hook exceptions to prevent recursive errors
          end;
        end;
      end;
    end;
  finally
    LExceptionDetails.Free;
  end;

  with FMemoInformation.Lines do
  begin
    Clear;
    BeginUpdate;
    try
      Add('We apologize for this inconvenience. ' + Application.Title +
        ' has encountered an unexpected error.');
      Add('');
      Add('You may copy the error details to the clipboard for reporting.');
    finally
      EndUpdate;
    end;
  end;
end;

procedure TLZExceptionDialog.UpdateTerminateButton;
var
  LCurrentSeconds: Integer;
  LSecondsToWait: Integer;
begin
  if not Assigned(FTerminateButton) then
    Exit;

  FTerminateButton.Visible := FAllowTerminateApplication;
  if FTerminateButton.Visible then
  begin
    LSecondsToWait := 5;
    LCurrentSeconds := SecondsBetween(Now, FAllowTerminateApplicationDateTime);
    if LCurrentSeconds >= LSecondsToWait then
    begin
      FTerminateButton.Caption := 'Terminate';
      FTerminateButton.Enabled := True;
    end
    else
    begin
      FTerminateButton.Caption := 'Wait (' +
        IntToStr(LSecondsToWait - LCurrentSeconds) + ')';
      FTerminateButton.Enabled := False;
    end;
  end;
end;

procedure TLZExceptionDialog.ShowForm;
begin
  if Assigned(FForm) then
  begin
    UpdateExceptionDetails;

    FForm.Left := (Screen.Width - FForm.Width) div 2;
    FForm.Top := (Screen.Height - FForm.Height) div 2;

    if FDialogMode = dmModal then
    begin
      FForm.ShowModal;
    end
    else
    begin
      if not FForm.Visible then
      begin
        FForm.Show;
      end;
    end;
  end;
end;

procedure TLZExceptionDialog.DoExecute(ASender: TObject; AException: Exception;
  AAllowTerminateApplication: Boolean; ADialogMode: TLZExceptionDialogMode);
begin
  FSenderObject := ASender;
  FException := AException;
  FAllowTerminateApplication := AAllowTerminateApplication;
  FDialogMode := ADialogMode;
  FCanClose := False;
  FFormActive := False;

  CreateForm;
  ShowForm;
end;

procedure TLZExceptionDialog.OnFormShow(ASender: TObject);
begin
  FAllowTerminateApplicationDateTime := Now;
  MessageBeep(MB_ICONERROR);
end;

procedure TLZExceptionDialog.OnFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FDialogMode = dmNormal then
  begin
    Action := caFree;
    FForm := nil;
  end
  else
  begin
    Action := caHide;
  end;
end;

procedure TLZExceptionDialog.OnFormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FCanClose or (not FAllowTerminateApplication);
  if not CanClose then
  begin
    CanClose := MessageDlg('Are you sure you want to close this dialog?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end;
end;

procedure TLZExceptionDialog.OnFormActivate(ASender: TObject);
begin
  if not FFormActive then
  begin
    FFormActive := True;
    ScrollToTop(FMemoException);
    ScrollToTop(FMemoInformation);
    FCloseButton.SetFocus;
  end;
end;

procedure TLZExceptionDialog.OnCloseButtonClick(ASender: TObject);
begin
  FCanClose := True;
  if FDialogMode = dmModal then
    FForm.ModalResult := mrClose
  else
    FForm.Close;
end;

procedure TLZExceptionDialog.OnCopyToClipboardClick(ASender: TObject);
begin
  if Assigned(FMemoException) then
  begin
    FMemoException.SelectAll;
    FMemoException.CopyToClipboard;
  end;
end;

class procedure TLZExceptionDialog.OnException(ASender: TObject;
  AException: Exception);
begin
  Execute(ASender, AException);
end;

procedure TLZExceptionDialog.OnTerminateButtonClick(ASender: TObject);
begin
  FCanClose := True;
  if MessageDlg
    (Format('Terminating %s will lose any unsaved changes, are you sure you want to continue?',
    [Application.Title]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Application.Terminate;
  end;
end;

procedure TLZExceptionDialog.OnTimerUpdate(ASender: TObject);
begin
  UpdateTerminateButton;
end;

{ Class methods }

class procedure TLZExceptionDialog.Initialize;
begin
  FExceptionForm := nil;
  FModalMode := True;
  FExceptionDetailsHooks := TLZExceptionDetailsHookList.Create;
end;

class procedure TLZExceptionDialog.Finalize;
begin
  try
    if Assigned(FExceptionForm) then
      FreeAndNil(FExceptionForm);

    if Assigned(FExceptionDetailsHooks) then
      FreeAndNil(FExceptionDetailsHooks);
  except
    // Suppress exceptions during finalization
  end;
end;

class procedure TLZExceptionDialog.SetModalMode(AValue: Boolean);
begin
  FModalMode := AValue;
  if Assigned(FExceptionForm) then
  begin
    if FModalMode then
      FExceptionForm.DialogMode := dmModal
    else
      FExceptionForm.DialogMode := dmNormal;
  end;
end;

class procedure TLZExceptionDialog.Execute(ASender: TObject;
  AException: Exception; AAllowTerminateApplication: Boolean);
var
  LDialogMode: TLZExceptionDialogMode;
  LForm: TLZExceptionDialog;
begin
  try
    if FModalMode then
      LDialogMode := dmModal
    else
      LDialogMode := dmNormal;

    if LDialogMode = dmModal then
    begin
      LForm := TLZExceptionDialog.Create;
      try
        LForm.DoExecute(ASender, AException, AAllowTerminateApplication,
          LDialogMode);
      finally
        FreeAndNil(LForm);
      end;
    end
    else
    begin
      if not Assigned(FExceptionForm) then
        FExceptionForm := TLZExceptionDialog.Create;

      FExceptionForm.DoExecute(ASender, AException, AAllowTerminateApplication,
        LDialogMode);
    end;
  except
    on E: Exception do
    begin
      // Show standard error message - we don't want to try this again
      MessageDlg('Error displaying exception: ' + E.Message, mtError,
        [mbOK], 0);
    end;
  end;
end;

class procedure TLZExceptionDialog.ForceException;
begin
  raise Exception.Create('This was a forced exception');
end;

initialization

TLZExceptionDialog.Initialize;

finalization

TLZExceptionDialog.Finalize;

end.
