unit WinApi.MessageDlgTimed;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.UITypes, VCL.Dialogs, VCL.Forms,
  System.Generics.Collections;

type
  TLZMessageDialogTimer = class(TObject)
  private
    FID: NativeUInt;
    FTimeout: integer;
    FCaption: string;
    FActiveWnd: HWND;
    FKilled: boolean;
    procedure SetID(const Value: NativeUInt);
    procedure SetTimeout(const Value: integer);
    procedure SetCaption(const Value: string);
    procedure SetActiveWnd(const Value: HWND);
    procedure SetKilled(const Value: boolean);
  public
    constructor Create; reintroduce;
    property ID: NativeUInt read FID write SetID;
    property Timeout: integer read FTimeout write SetTimeout;
    property Caption: string read FCaption write SetCaption;
    property ActiveWnd: HWND read FActiveWnd write SetActiveWnd;
    property Killed: boolean read FKilled write SetKilled;
  end;

  TLZMessageDialogTimers = class(TObjectList<TLZMessageDialogTimer>)
  private
    function GetTimer(AID: NativeUInt): TLZMessageDialogTimer;
  public
    function New(AFNTimerProc: TFNTimerProc; ATimeout: integer): NativeUInt;
    procedure Kill(AWnd: HWND; AID: NativeUInt);
    procedure Flush;
    property Timer[AID: NativeUInt]: TLZMessageDialogTimer read GetTimer;

  end;

  TLZMessageDialogTimed = class(TObject)
  private
    FTimerID: NativeUInt;
  protected
    function DoMessageDlgTimed(const AMessage: string; ATimeout: integer;
      ADefault: integer; AMsgDlgType: TMsgDlgType; AButtons: TMsgDlgButtons;
      AHelpCtx: Longint): integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    class procedure SetCaptionTemplate(AString: string);
    class function Show(const AMessage: string; ATimeout: integer;
      ADefault: integer; AMsgDlgType: TMsgDlgType; AButtons: TMsgDlgButtons;
      AHelpCtx: Longint): integer;
    class function Confirmation(const AMessage: string; ATimeout: integer = 30;
      ADefault: integer = mrYes; AHelpCtx: Longint = 0): integer;
    class function Error(const AMessage: string; ATimeout: integer = 30;
      AHelpCtx: Longint = 0): integer;
  end;

var
  _MessageDialogTimers: TLZMessageDialogTimers;
  _MessageDialogCaptionTemplate: string;

implementation

uses
  Consts;

procedure MessageDlgTimerCallback(AWnd: HWND; AMsg: UINT; AIDEvent: NativeUInt;
  ATicks: DWORD); stdcall;
var
  wCaption: string;
  wCaptionLength: integer;
  LTimer: TLZMessageDialogTimer;
begin
  LTimer := _MessageDialogTimers.Timer[AIDEvent];
  if Assigned(LTimer) then
  begin
    LTimer.Timeout := LTimer.Timeout - 1;
    if LTimer.ActiveWnd = 0 then
    begin
      LTimer.ActiveWnd := GetActiveWindow;
    end;

    if LTimer.Timeout > 0 then
    begin
      if IsWindow(LTimer.ActiveWnd) AND IsWindowEnabled(LTimer.ActiveWnd) AND
        IsWindowVisible(LTimer.ActiveWnd) then
      begin
        wCaptionLength := GetWindowTextLength(LTimer.ActiveWnd);
        SetLength(wCaption, wCaptionLength);
        GetWindowText(LTimer.ActiveWnd, pChar(wCaption), 1 + wCaptionLength);
        if LTimer.Caption = '' then
          LTimer.Caption := wCaption;

        wCaption := Format(_MessageDialogCaptionTemplate,
          [LTimer.Caption, (LTimer.Timeout)]);

        SetWindowText(LTimer.ActiveWnd, wCaption);
      end
      else
        _MessageDialogTimers.Kill(AWnd, AIDEvent);
    end
    else
    begin
      _MessageDialogTimers.Kill(AWnd, AIDEvent);
      if IsWindow(LTimer.ActiveWnd) AND IsWindowEnabled(LTimer.ActiveWnd) then
        PostMessage(LTimer.ActiveWnd, WM_CLOSE, 0, 0);
    end;
  end;
end;

constructor TLZMessageDialogTimed.Create;
begin
  FTimerID := 0;
end;

destructor TLZMessageDialogTimed.Destroy;
begin
  try
    if FTimerID <> 0 then
      _MessageDialogTimers.Kill(0, FTimerID);
    _MessageDialogTimers.Flush;
  finally
    inherited;
  end;
end;

function TLZMessageDialogTimed.DoMessageDlgTimed(const AMessage: string;
  ATimeout: integer; ADefault: integer; AMsgDlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AHelpCtx: Longint): integer;
var
  LTimer: TLZMessageDialogTimer;
begin
  Application.NormalizeAllTopMosts;
  try
    if ATimeout > 0 then
    begin
      FTimerID := _MessageDialogTimers.New(@MessageDlgTimerCallback, ATimeout);
      Result := MessageDlg(AMessage, AMsgDlgType, AButtons, AHelpCtx);
      if (Result = mrCancel) then
      begin
        LTimer := _MessageDialogTimers.Timer[FTimerID];
        if Assigned(LTimer) and (LTimer.Timeout < 1) then
        begin
          Result := ADefault;
        end;
      end;
    end
    else
    begin
      Result := MessageDlg(AMessage, AMsgDlgType, AButtons, AHelpCtx);
    end;
  finally
    Application.RestoreTopMosts;
    _MessageDialogTimers.Flush;
  end;
end;

class function TLZMessageDialogTimed.Error(const AMessage: string;
  ATimeout: integer; AHelpCtx: Longint): integer;
begin
  with TLZMessageDialogTimed.Create do
  begin
    try
      Result := DoMessageDlgTimed(AMessage, ATimeout, mrOk, TMsgDlgType.mtError,
        [TMsgDlgBtn.mbOK], AHelpCtx);
    finally
      Free;
    end;
  end;
end;

class procedure TLZMessageDialogTimed.SetCaptionTemplate(AString: string);
begin
  _MessageDialogCaptionTemplate := AString;
end;

class function TLZMessageDialogTimed.Show(const AMessage: string;
  ATimeout: integer; ADefault: integer; AMsgDlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AHelpCtx: Longint): integer;
begin
  with TLZMessageDialogTimed.Create do
  begin
    try
      Result := DoMessageDlgTimed(AMessage, ATimeout, ADefault, AMsgDlgType,
        AButtons, AHelpCtx);
    finally
      Free;
    end;
  end;
end;

class function TLZMessageDialogTimed.Confirmation(const AMessage: string;
  ATimeout, ADefault: integer; AHelpCtx: Longint): integer;
begin
  with TLZMessageDialogTimed.Create do
  begin
    try
      Result := DoMessageDlgTimed(AMessage, ATimeout, ADefault,
        TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
        AHelpCtx);
    finally
      Free;
    end;
  end;
end;

{ TLZMessageDialogTimer }

constructor TLZMessageDialogTimer.Create;
begin
  FID := 0;
  FTimeout := 30;
  FCaption := '';
  FActiveWnd := 0;
  FKilled := False;
end;

procedure TLZMessageDialogTimer.SetActiveWnd(const Value: HWND);
begin
  FActiveWnd := Value;
end;

procedure TLZMessageDialogTimer.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TLZMessageDialogTimer.SetID(const Value: NativeUInt);
begin
  FID := Value;
end;

procedure TLZMessageDialogTimer.SetKilled(const Value: boolean);
begin
  FKilled := Value;
end;

procedure TLZMessageDialogTimer.SetTimeout(const Value: integer);
begin
  FTimeout := Value;
end;

{ TLZMessageDialogTimers }

procedure TLZMessageDialogTimers.Kill(AWnd: HWND; AID: NativeUInt);
var
  LTimer: TLZMessageDialogTimer;
begin
  KillTimer(AWnd, AID);
  LTimer := GetTimer(AID);
  if Assigned(LTimer) then
  begin
    LTimer.Killed := True;
  end;
end;

function TLZMessageDialogTimers.New(AFNTimerProc: TFNTimerProc;
  ATimeout: integer): NativeUInt;
var
  LTimer: TLZMessageDialogTimer;
begin
  LTimer := TLZMessageDialogTimer.Create;
  LTimer.Timeout := ATimeout;
  Self.Add(LTimer);
  LTimer.ID := SetTimer(0, 0, 1000, AFNTimerProc);
  Result := LTimer.ID;
end;

procedure TLZMessageDialogTimers.Flush;
var
  LTimer: TLZMessageDialogTimer;
begin
  for LTimer in Self do
  begin
    if (LTimer.Killed) or (LTimer.ID = 0) then
    begin
      Remove(LTimer);
    end;
  end;

end;

function TLZMessageDialogTimers.GetTimer(AID: NativeUInt)
  : TLZMessageDialogTimer;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (LIdx < Count) and (Result = nil) do
  begin
    if Items[LIdx].ID = AID then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

initialization

_MessageDialogTimers := TLZMessageDialogTimers.Create;
_MessageDialogCaptionTemplate := '%s (closing in %d second(s))';

finalization

_MessageDialogTimers.Free;

end.
