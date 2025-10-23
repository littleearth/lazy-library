{ -----------------------------------------------------------------------------
  Unit Name: VCL.Lazy.MemoForm
  Author: Tristan Marlow
  Purpose: Memo display form (dynamically created, no DFM)

  ----------------------------------------------------------------------------
  Copyright (c) 2006 Tristan David Marlow
  Copyright (c) 2006 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History:

  ----------------------------------------------------------------------------- }
unit VCL.Lazy.MemoForm;

interface

uses
  Lazy.Types,
  Windows, SysUtils, Forms, Classes, Controls, StdCtrls, ExtCtrls,
  System.UITypes, Dialogs, VCL.Buttons, ComCtrls, Clipbrd;

type
  TLZMemoFormDialogMode = (dmModal, dmNormal);

  TLZMemoForm = class(TLZObject)
  private
    FForm: TForm;
    FMemo: TMemo;
    FFileName: TFileName;
    FWordWrap: boolean;
    FDialogMode: TLZMemoFormDialogMode;
    FMessagePanel: TPanel;
    FMessageLabel: TLabel;
    FToolsPanel: TPanel;
    FCopyButton: TButton;
    FWordWrapCheckBox: TCheckBox;
    FZoomTrackBar: TTrackBar;
    FZoomLabel: TLabel;
    FBottomPanel: TPanel;
    FCloseButton: TBitBtn;
    FFontSize: integer;
    FFontName: string;
    FDefaultFontSize: integer;
    procedure SetWordWrap(const Value: boolean);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: integer);
  protected
    procedure CreateForm;
    procedure DestroyForm;
    procedure ShowForm(AText: string; ACaption: string; AMessage: string;
      ADialogMode: TLZMemoFormDialogMode);
    procedure LoadFile;
    procedure OnFormShow(ASender: TObject);
    procedure OnFormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnRefreshFileButtonClick(ASender: TObject);
    procedure OnCloseButtonClick(ASender: TObject);
    procedure OnCopyToClipboardClick(ASender: TObject);
    procedure OnWordWrapCheckBoxClick(ASender: TObject);
    procedure OnZoomTrackBarChange(ASender: TObject);
    procedure DoShow(AText: TStrings; ACaption: string = '';
      AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal); overload;
    procedure DoShow(AText: string; ACaption: string = '';
      AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal); overload;
    procedure DoShowFile(AFileName: TFileName; AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal);

    property DialogMode: TLZMemoFormDialogMode read FDialogMode
      write FDialogMode;
    property WordWrap: boolean read FWordWrap write SetWordWrap;
    property FontName: string read FFontName write SetFontName;
    property FontSize: integer read FFontSize write SetFontSize;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    class procedure Show(AText: TStrings; ACaption: string = '';
      AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal); overload;
    class procedure Show(AText: string; ACaption: string = '';
      AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal); overload;
    class procedure ShowFile(AFileName: TFileName; AMessage: string = '';
      ADialogMode: TLZMemoFormDialogMode = dmModal);
  end;

implementation

uses
  VCL.Lazy.Utils.Windows, Graphics;

constructor TLZMemoForm.Create;
begin
  inherited Create;
  FFileName := '';
  FWordWrap := True;
  FDialogMode := dmModal;
  FFontName := '';
  FFontSize := 0;
  FDefaultFontSize := 10;
end;

destructor TLZMemoForm.Destroy;
begin
  DestroyForm;
  inherited Destroy;
end;

procedure TLZMemoForm.CreateForm;
begin

  FForm := TForm.Create(nil);
  with FForm do
  begin
    FormStyle := fsNormal;
    BorderStyle := bsSizeable;
    Position := poScreenCenter;
    PixelsPerInch := 96;
    Font.Charset := DEFAULT_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -11;
    Font.Name := 'Tahoma';
    Font.Style := [];
    Width := 1024;
    Height := 768;
    Scaled := True;
    AutoSize := False;
    AutoScroll := False;
    OnShow := OnFormShow;
    OnClose := OnFormClose;
  end;

  FMessagePanel := TPanel.Create(FForm);
  with FMessagePanel do
  begin
    Align := alTop;
    AlignWithMargins := True;
    Parent := FForm;
    Caption := '';
    Height := 64;
  end;

  FMessageLabel := TLabel.Create(FForm);
  with FMessageLabel do
  begin
    AlignWithMargins := True;
    Parent := FMessagePanel;
    Align := alClient;
    Alignment := taCenter;
    Layout := tlCenter;
    WordWrap := True;
    Caption := '';
  end;

  FToolsPanel := TPanel.Create(FForm);
  with FToolsPanel do
  begin
    AlignWithMargins := True;
    Parent := FForm;
    Align := alBottom;
    Height := 40;
    BevelOuter := bvNone;
  end;

  FCopyButton := TButton.Create(FForm);
  with FCopyButton do
  begin
    AlignWithMargins := True;
    Parent := FToolsPanel;
    Align := alLeft;
    Width := 120;
    Caption := 'Copy to Clipboard';
    OnClick := OnCopyToClipboardClick;
  end;

  FWordWrapCheckBox := TCheckBox.Create(FForm);
  with FWordWrapCheckBox do
  begin
    AlignWithMargins := True;
    Parent := FToolsPanel;
    Align := alLeft;
    Width := 100;
    Caption := 'Word Wrap';
    Checked := FWordWrap;
    OnClick := OnWordWrapCheckBoxClick;
  end;

  FZoomLabel := TLabel.Create(FForm);
  with FZoomLabel do
  begin
    AlignWithMargins := True;
    Parent := FToolsPanel;
    Align := alRight;
    Width := 45;
    Caption := 'Zoom:';
    Layout := tlCenter;
  end;

  FZoomTrackBar := TTrackBar.Create(FForm);
  with FZoomTrackBar do
  begin
    AlignWithMargins := True;
    Parent := FToolsPanel;
    Align := alRight;
    Width := 250;
    Min := 6;
    Max := 24;
    Frequency := 2;
    Position := FDefaultFontSize;
    OnChange := OnZoomTrackBarChange;
  end;

  FBottomPanel := TPanel.Create(FForm);
  with FBottomPanel do
  begin
    AlignWithMargins := True;
    Parent := FForm;
    Height := 33;
    BevelOuter := bvNone;
    Top := FForm.Height - Height;
    Align := alBottom;
  end;

  FCloseButton := TBitBtn.Create(FForm);
  with FCloseButton do
  begin
    AlignWithMargins := True;
    Parent := FBottomPanel;
    Align := alRight;
    Caption := 'Close';
    OnClick := OnCloseButtonClick;
  end;

  FMemo := TMemo.Create(FForm);
  with FMemo do
  begin
    AlignWithMargins := True;
    Parent := FForm;
    Align := alClient;
    ReadOnly := True;
    WordWrap := FWordWrap;
    if WordWrap then
    begin
      ScrollBars := ssVertical;
    end
    else
    begin
      ScrollBars := TScrollStyle.ssBoth;
    end;
    Lines.Clear;
    if not TLZString.IsEmptyString(FFontName) then
    begin
      Font.Name := FFontName;
    end;
    if FFontSize > 0 then
    begin
      Font.Size := FFontSize;
      FDefaultFontSize := FFontSize;
    end
    else
    begin
      FDefaultFontSize := Font.Size;
    end;
  end;

  FZoomTrackBar.Position := FDefaultFontSize;

  if not TLZString.IsEmptyString(FFileName) then
  begin
    with TButton.Create(FForm) do
    begin
      Parent := FBottomPanel;
      Align := alLeft;
      AlignWithMargins := True;
      OnClick := OnRefreshFileButtonClick;
      Caption := 'Refresh';
    end;
  end;

  FForm.ScaleForCurrentDPI(True);
  FForm.Width := Round(FForm.Width * (Screen.PixelsPerInch / 96));
  FForm.Height := Round(FForm.Height * (Screen.PixelsPerInch / 96));

end;

procedure TLZMemoForm.DestroyForm;
begin
  if Assigned(FForm) then
  begin
    if FForm.Visible then
      FForm.Close;
    FreeAndNil(FForm);
  end;
end;

procedure TLZMemoForm.ShowForm(AText: string; ACaption, AMessage: string;
  ADialogMode: TLZMemoFormDialogMode);
begin
  if Assigned(FForm) then
  begin
    FDialogMode := ADialogMode;
    FForm.Caption := ACaption;
    FMemo.Lines.Text := AText;
    FMessageLabel.Caption := AMessage;
    FMessagePanel.Visible := not TLZString.IsEmptyString(AMessage);
    if FDialogMode = dmModal then
    begin
      FForm.ShowModal;
    end
    else
    begin
      FForm.FormStyle := fsStayOnTop;
      FForm.Show;
    end;
  end;
end;

procedure TLZMemoForm.SetFontName(const Value: string);
begin
  FFontName := Value;
end;

procedure TLZMemoForm.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
end;

procedure TLZMemoForm.SetWordWrap(const Value: boolean);
begin
  FWordWrap := Value;
  if Assigned(FForm) then
  begin
    FMemo.WordWrap := FWordWrap;
  end;
end;

procedure TLZMemoForm.DoShow(AText: string; ACaption, AMessage: string;
  ADialogMode: TLZMemoFormDialogMode);
begin
  CreateForm;
  ShowForm(AText, ACaption, AMessage, ADialogMode);
end;

procedure TLZMemoForm.DoShow(AText: TStrings; ACaption, AMessage: string;
  ADialogMode: TLZMemoFormDialogMode);
begin
  Show(AText.Text, ACaption, AMessage, ADialogMode);
end;

procedure TLZMemoForm.OnFormShow(ASender: TObject);
begin
  if not TLZString.IsEmptyString(FFileName) then
    LoadFile;
end;

procedure TLZMemoForm.OnCloseButtonClick(ASender: TObject);
begin
  if FDialogMode = dmModal then
    FForm.ModalResult := mrClose
  else
    FForm.Close;
end;

procedure TLZMemoForm.OnFormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TLZMemoForm.OnRefreshFileButtonClick(ASender: TObject);
begin
  LoadFile;
end;

procedure TLZMemoForm.LoadFile;
var
  fileStream: TFileStream;
begin
  if Assigned(FMemo) then
  begin
    if FileExists(FFileName) then
    begin
      fileStream := TFileStream.Create(FFileName, fmOpenRead or
        fmShareDenyNone);
      try
        fileStream.Position := 0;
        FMemo.Lines.LoadFromStream(fileStream);
      finally
        if Assigned(fileStream) then
          FreeAndNil(fileStream);
      end;
    end
    else
    begin
      FMemo.Clear;
    end;
  end;
end;

procedure TLZMemoForm.DoShowFile(AFileName: TFileName; AMessage: string;
  ADialogMode: TLZMemoFormDialogMode);
var
  LMessage: string;
begin
  LMessage := AMessage;
  if not FileExists(AFileName) then
  begin
    if not TLZString.IsEmptyString(LMessage) then
      LMessage := LMessage + sLineBreak;
    LMessage := LMessage + Format('"%s" does not exists.', [AFileName]);
  end;
  FFileName := AFileName;
  Show('', FFileName, LMessage, ADialogMode);
end;

class procedure TLZMemoForm.Show(AText: TStrings; ACaption: string = '';
  AMessage: string = ''; ADialogMode: TLZMemoFormDialogMode = dmModal);
var
  LForm: TLZMemoForm;
begin
  LForm := TLZMemoForm.Create;
  try
    LForm.DoShow(AText, ACaption, AMessage, ADialogMode);
  finally
    FreeAndNil(LForm);
  end;
end;

class procedure TLZMemoForm.Show(AText: string; ACaption: string = '';
  AMessage: string = ''; ADialogMode: TLZMemoFormDialogMode = dmModal);
var
  LForm: TLZMemoForm;
begin
  LForm := TLZMemoForm.Create;
  try
    LForm.DoShow(AText, ACaption, AMessage, ADialogMode);
  finally
    FreeAndNil(LForm);
  end;
end;

class procedure TLZMemoForm.ShowFile(AFileName: TFileName;
  AMessage: string = ''; ADialogMode: TLZMemoFormDialogMode = dmModal);
var
  LForm: TLZMemoForm;
begin
  LForm := TLZMemoForm.Create;
  try
    LForm.DoShowFile(AFileName, AMessage, ADialogMode);
  finally
    FreeAndNil(LForm);
  end;
end;

procedure TLZMemoForm.OnCopyToClipboardClick(ASender: TObject);
begin
  if Assigned(FMemo) then
  begin
    if FMemo.SelLength > 0 then
      Clipboard.AsText := FMemo.SelText
    else
      Clipboard.AsText := FMemo.Lines.Text;
  end;
end;

procedure TLZMemoForm.OnWordWrapCheckBoxClick(ASender: TObject);
begin
  if Assigned(FMemo) and Assigned(FWordWrapCheckBox) then
  begin
    FWordWrap := FWordWrapCheckBox.Checked;
    FMemo.WordWrap := FWordWrap;
    if FWordWrap then
    begin
      FMemo.ScrollBars := ssVertical;
    end
    else
    begin
      FMemo.ScrollBars := TScrollStyle.ssBoth;
    end;
  end;
end;

procedure TLZMemoForm.OnZoomTrackBarChange(ASender: TObject);
begin
  if Assigned(FMemo) and Assigned(FZoomTrackBar) then
  begin
    FMemo.Font.Size := FZoomTrackBar.Position;
  end;
end;

end.
