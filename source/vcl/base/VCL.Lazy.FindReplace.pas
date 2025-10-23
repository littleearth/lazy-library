unit VCL.Lazy.FindReplace;

interface

uses
  Lazy.Types,
  Winapi.Messages, Winapi.Windows,
  System.SysUtils, System.Classes, VCL.StdCtrls, VCL.Menus, VCL.Controls,
  VCL.ComCtrls, VCL.Graphics, VCL.Forms, VCL.ExtCtrls, VCL.Dialogs,
  DB, DBCtrls, VCL.Themes,
  System.Types, System.Math, System.UITypes;

type
  TSearchTypes = VCL.ComCtrls.TSearchTypes;

  TLZFindReplace = class(TLZObject)
  private
    FCustomEdit: TCustomEdit;
    FReplaceDialog: TReplaceDialog;
    FFindDialog: TFindDialog;
    function GetSelLength: integer;
    function GetSelStart: integer;
    function GetSelText: string;
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelText(const Value: string);
    function GetReadOnly: boolean;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    function AllowEdit: boolean; virtual;
    function AllowView: boolean; virtual;
    procedure DoReplaceDialogReplace(ASender: TObject); virtual;
    procedure DoReplaceDialogFind(ASender: TObject); virtual;
    function DoShowReplaceDialog: boolean;
    function DoFindText(const SearchText: string; StartPos, EndPos: integer;
      Options: TSearchTypes): integer;
    property CustomEdit: TCustomEdit read FCustomEdit;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    property ReadOnly: boolean read GetReadOnly;
    property Text: string read GetText write SetText;
    procedure SetFocus;
    procedure CreateDialogs(ACustomEdit: TCustomEdit);
    procedure DestroyDialogs;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Execute(ACustomEdit: TCustomEdit): boolean;
    class function FindText(ACustomEdit: TCustomEdit; const SearchText: string;
      StartPos, EndPos: integer; Options: TSearchTypes): integer;
  end;

var
  LazyFindReplace: TLZFindReplace;

implementation

uses
  Lazy.Utils.Windows, VCL.Lazy.Dialogs, System.TypInfo, System.Character;

function TLZFindReplace.AllowView: boolean;
begin
  Result := Assigned(CustomEdit);

  if Result then
  begin
    if (CustomEdit is TEdit) then
    begin
      Result := (CustomEdit as TEdit).PasswordChar = #0;
    end;
  end;

end;

function TLZFindReplace.AllowEdit;
var
  LDataSource: TDatasource;
begin
  Result := AllowView;

  if Result then
  begin
    if CustomEdit.ReadOnly then
    begin
      Result := False;
    end;
  end;

  if Result then
  begin
    if IsPublishedProp(CustomEdit, 'DataSource') then
    begin
      LDataSource := TDatasource(GetObjectProp(CustomEdit, 'DataSource'));
      if Assigned(LDataSource) then
      begin
        Result := Assigned(LDataSource);
        if Result then
          Result := Assigned(LDataSource.DataSet);
        if Result then
          Result := LDataSource.DataSet.Active;
        if Result then
        begin
          if LDataSource.DataSet.State in [dsBrowse] then
            LDataSource.DataSet.Edit;
        end;
        if Result then
          Result := LDataSource.State in [dsEdit, dsInsert];
      end;
    end;
  end;
end;

function TLZFindReplace.DoFindText(const SearchText: string;
  StartPos, EndPos: integer; Options: TSearchTypes): integer;
var
  LText, LSearchText, LMatchText: string;
  LStartPos: integer;
  LFoundPos: integer;
begin
  Result := -1;
  // Input validation
  if (SearchText = '') or (StartPos < 0) or (EndPos < StartPos) then
  begin
    Result := -1;
    Exit;
  end;

  LText := CustomEdit.Text;
  LSearchText := SearchText;
  if not(stMatchCase in Options) then
  begin
    LText := UpperCase(LText);
    LSearchText := UpperCase(LSearchText);
  end;

  LFoundPos := Pos(LSearchText, Copy(LText, StartPos, EndPos - StartPos + 1));
  if LFoundPos <> 0 then
  begin
    if stWholeWord in Options then
    begin
      // Check character before match (if not at start)
      LStartPos := LFoundPos + StartPos - 1;

      LMatchText := Copy(LText, LStartPos, Length(LSearchText) + 2);

      if LStartPos = 0 then
        LMatchText := ' ' + Copy(LMatchText, 1, Length(LMatchText) - 1);

      if ((LStartPos + Length(LSearchText) + 2) > Length(LText)) then
        LMatchText := LMatchText + ' ';

      if (LMatchText[1].IsLetterOrDigit) then
      begin
        Result := -1;
        Exit;
      end;

      // Check character after match
      if LMatchText[Length(LMatchText)].IsLetterOrDigit then
      begin
        Result := -1;
        Exit;
      end;

      Result := LStartPos;
    end
    else
    begin
      Result := LFoundPos + StartPos - 1;
    end;
  end;

  if (Result > EndPos) then
    Result := -1;
end;

function TLZFindReplace.GetReadOnly: boolean;
begin
  Result := FCustomEdit.ReadOnly;
end;

function TLZFindReplace.GetSelLength: integer;
begin
  Result := FCustomEdit.SelLength;
end;

function TLZFindReplace.GetSelStart: integer;
begin
  Result := FCustomEdit.SelStart;
end;

function TLZFindReplace.GetSelText: string;
begin
  Result := FCustomEdit.SelText;
end;

function TLZFindReplace.GetText: string;
begin
  Result := FCustomEdit.Text;
end;

constructor TLZFindReplace.Create;
begin
  FCustomEdit := nil;
  FReplaceDialog := nil;
  FFindDialog := nil;
end;

procedure TLZFindReplace.CreateDialogs(ACustomEdit: TCustomEdit);
begin
  DestroyDialogs;
  FCustomEdit := ACustomEdit;
  FReplaceDialog := TReplaceDialog.Create(nil);
  FReplaceDialog.OnReplace := DoReplaceDialogReplace;
  FReplaceDialog.OnFind := DoReplaceDialogFind;
  FFindDialog := TFindDialog.Create(nil);
  FFindDialog.OnFind := DoReplaceDialogFind;
end;

destructor TLZFindReplace.Destroy;
begin
  try
    DestroyDialogs;
  finally
    inherited;
  end;
end;

procedure TLZFindReplace.DestroyDialogs;
begin
  try
    FReplaceDialog.free;
  except
  end;
  try
    FFindDialog.free;
  except
  end;
  FReplaceDialog := nil;
  FFindDialog := nil;
  FCustomEdit := nil;
end;

procedure TLZFindReplace.DoReplaceDialogFind(ASender: TObject);
var
  FoundAt: Longint;
  StartPos, ToEnd: integer;
  LSearchTypes: TSearchTypes;
begin
  LSearchTypes := [];
  if frMatchCase in (ASender as TFindDialog).Options then
    LSearchTypes := LSearchTypes + [stMatchCase];
  if frWholeWord in (ASender as TFindDialog).Options then
    LSearchTypes := LSearchTypes + [stWholeWord];
  { Begin the search after the current selection, if there is one. }
  { Otherwise, begin at the start of the text. }
  if SelLength <> 0 then
    StartPos := SelStart + SelLength
  else
    StartPos := 0;
  { ToEnd is the length from StartPos through the end of the
    text in the rich edit control. }
  ToEnd := Length(Text) - StartPos;
  FoundAt := DoFindText((ASender as TFindDialog).FindText, StartPos, ToEnd,
    LSearchTypes);
  if FoundAt <> -1 then
  begin
    SetFocus;
    SelStart := FoundAt;
    SelLength := Length((ASender as TFindDialog).FindText);
  end
  else
  begin
    Beep;
    TLZDialogs.StatusMessage(Format('"%s" was not found.',
      [(ASender as TFindDialog).FindText]), False);

  end;
end;

procedure TLZFindReplace.DoReplaceDialogReplace(ASender: TObject);
var
  LSearchText, LReplaceText, LMemoText: string;
  LSearchStart, LFoundAt, LReplaceCount: integer;
  LSearchTypes: TSearchTypes;
begin

  if ReadOnly then
  begin
    TLZDialogs.ErrorMessage('Read only you cannot modify contents.');
    Exit;
  end;

  LSearchTypes := [];
  if frMatchCase in (ASender as TFindDialog).Options then
    LSearchTypes := LSearchTypes + [stMatchCase];
  if frWholeWord in (ASender as TFindDialog).Options then
    LSearchTypes := LSearchTypes + [stWholeWord];

  LSearchText := FReplaceDialog.FindText;
  LReplaceText := FReplaceDialog.ReplaceText;
  LMemoText := CustomEdit.Text;

  // Handle Replace All
  if frReplaceAll in FReplaceDialog.Options then
  begin
    LReplaceCount := 0;
    LSearchStart := 1;
    repeat

      LFoundAt := DoFindText(LSearchText, LSearchStart, Length(LMemoText),
        LSearchTypes);

      if LFoundAt >= 0 then
      begin
        Delete(LMemoText, LFoundAt, Length(LSearchText));
        Insert(LReplaceText, LMemoText, LFoundAt);
        Inc(LReplaceCount);
        LSearchStart := LFoundAt + Length(LReplaceText);
      end;
    until LFoundAt = -1;

    if LReplaceCount > 0 then
    begin
      Text := LMemoText;
      TLZDialogs.StatusMessage(Format('"%s" was replaced %d time(s).',
        [LSearchText, LReplaceCount]), True);
    end
    else
    begin
      Beep;
      TLZDialogs.StatusMessage(Format('"%s" was not found.',
        [LSearchText]), False);
    end;
  end
  // Handle single replace
  else
  begin
    LSearchStart := SelStart + SelLength + 1;

    LFoundAt := DoFindText(LSearchText, LSearchStart, Length(LMemoText),
      LSearchTypes);

    if LFoundAt >= 0 then
    begin
      SelStart := LFoundAt - 1;
      SelLength := Length(LSearchText);
      SelText := LReplaceText;
    end
    else
    begin
      TLZDialogs.StatusMessage(Format('"%s" was not found.',
        [LSearchText]), False);
    end;
  end;
end;

function TLZFindReplace.Execute(ACustomEdit: TCustomEdit): boolean;
begin
  CreateDialogs(ACustomEdit);
  Result := DoShowReplaceDialog;
end;

class function TLZFindReplace.FindText(ACustomEdit: TCustomEdit;
  const SearchText: string; StartPos, EndPos: integer;
  Options: TSearchTypes): integer;
var
  LFind: TLZFindReplace;
begin
  LFind := TLZFindReplace.Create;
  try
    LFind.CreateDialogs(ACustomEdit);
    Result := LFind.DoFindText(SearchText, StartPos, EndPos, Options);
  finally
    FreeAndNil(LFind);
  end;
end;

procedure TLZFindReplace.SetFocus;
begin
  FCustomEdit.SetFocus;
end;

procedure TLZFindReplace.SetSelLength(const Value: integer);
begin
  FCustomEdit.SelLength := Value;
end;

procedure TLZFindReplace.SetSelStart(const Value: integer);
begin
  FCustomEdit.SelStart := Value;
end;

procedure TLZFindReplace.SetSelText(const Value: string);
begin
  FCustomEdit.SelText := Value;
end;

procedure TLZFindReplace.SetText(const Value: string);
begin
  FCustomEdit.Text := Value;
end;

function TLZFindReplace.DoShowReplaceDialog: boolean;
begin
  Result := False;
  if SelLength > 0 then
  begin
    FReplaceDialog.FindText := SelText;
    FFindDialog.FindText := SelText;
  end;

  if (not ReadOnly) and AllowEdit then
  begin
    Result := FReplaceDialog.Execute(CustomEdit.Handle);
  end
  else
  begin
    if AllowView then
    begin
      Result := FFindDialog.Execute(CustomEdit.Handle);
    end
    else
    begin
      Beep;
    end;
  end;
end;

initialization

LazyFindReplace := TLZFindReplace.Create;

finalization

FreeAndNil(LazyFindReplace);

end.
