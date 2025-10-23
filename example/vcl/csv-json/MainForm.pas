unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, JSON,
  Lazy.Types, Lazy.CSVJSONUtils, Lazy.CSVtoJSON, Lazy.JSONtoCSV,
  Lazy.JSONFormatter;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    splMain: TSplitter;
    pnlInput: TPanel;
    pnlOutput: TPanel;
    splVertical: TSplitter;
    lblInput: TLabel;
    memoInput: TMemo;
    lblOutput: TLabel;
    memoOutput: TMemo;
    pnlStatus: TPanel;
    lblStatus: TLabel;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    Panel1: TGridPanel;
    btnLoadFile: TButton;
    btnSaveOutput: TButton;
    btnLoadSample: TButton;
    btnConvert: TButton;
    btnClear: TButton;
    pnlControls: TScrollBox;
    gbCSVOptions: TGroupBox;
    rgQuoteMode: TRadioGroup;
    Panel2: TPanel;
    lblDelimiter: TLabel;
    edtDelimiter: TEdit;
    chkIncludeHeaders: TCheckBox;
    gbJSONFormatOptions: TGroupBox;
    cbRemoveWrapperArray: TCheckBox;
    gbJSONOptions: TGroupBox;
    lblArrayName: TLabel;
    edtArrayName: TEdit;
    rgJSONFormatMode: TRadioGroup;
    rgConversionType: TRadioGroup;
    rgFieldNameCase: TRadioGroup;
    procedure btnConvertClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadSampleClick(Sender: TObject);
    procedure rgConversionTypeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveOutputClick(Sender: TObject);
    procedure btnLoadFileClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure UpdateControlsVisibility;
    procedure SetStatus(
      const AMessage: string;
      AError: Boolean = False);
    procedure LoadCSVSample;
    procedure LoadJSONSample;
    function GetFieldNameCase: TLZFieldNameCase;
    function GetQuoteMode: TLZCSVQuoteMode;
    procedure ConvertCSVToJSON;
    procedure ConvertJSONToCSV;
    function GetJSONFormatMode: TJSONFormatMode;
    procedure FormatJSON;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Initialize form
  Caption := 'CSV/JSON Converter';

  // Set default values
  rgConversionType.ItemIndex := 0; // CSV to JSON
  rgFieldNameCase.ItemIndex := 3; // PascalCase
  rgQuoteMode.ItemIndex := 0; // Minimal quoting
  rgJSONFormatMode.ItemIndex := 1; // Formatted
  edtDelimiter.Text := ',';
  chkIncludeHeaders.Checked := True;
  edtArrayName.Text := '';
  cbRemoveWrapperArray.Checked := False;

  // Configure open/save dialogs
  dlgOpen.Filter :=
    'CSV Files (*.csv)|*.csv|JSON Files (*.json)|*.json|Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
  dlgSave.Filter :=
    'CSV Files (*.csv)|*.csv|JSON Files (*.json)|*.json|Text Files (*.txt)|*.txt|All Files (*.*)|*.*';

  UpdateControlsVisibility;
  SetStatus('Ready');
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlInput.Width := Self.Width div 2;
end;

procedure TfrmMain.UpdateControlsVisibility;
var
  LIsCSVToJSON: Boolean;
  LIsJSONFormat: Boolean;
begin
  LIsCSVToJSON := rgConversionType.ItemIndex = 0;
  LIsJSONFormat := rgConversionType.ItemIndex = 2;

  // Show/hide relevant options
  gbJSONOptions.Visible := LIsCSVToJSON or LIsJSONFormat;
  gbCSVOptions.Visible := rgConversionType.ItemIndex = 1;
  gbJSONFormatOptions.Visible := LIsJSONFormat;

  // Update labels
  case rgConversionType.ItemIndex of
    0:
      begin // CSV to JSON
        lblInput.Caption := 'CSV Input:';
        lblOutput.Caption := 'JSON Output:';
      end;
    1:
      begin // JSON to CSV
        lblInput.Caption := 'JSON Input:';
        lblOutput.Caption := 'CSV Output:';
      end;
    2:
      begin // JSON Format
        lblInput.Caption := 'JSON Input:';
        lblOutput.Caption := 'Formatted JSON Output:';
      end;
  else
    begin
      if LIsCSVToJSON then
      begin
        lblInput.Caption := 'CSV Input:';
        lblOutput.Caption := 'JSON Output:';
      end
      else
      begin
        lblInput.Caption := 'JSON Input:';
        lblOutput.Caption := 'CSV Output:';
      end;
    end;
  end;
end;

procedure TfrmMain.SetStatus(
  const AMessage: string;
  AError: Boolean = False);
begin
  lblStatus.Caption := AMessage;
  if AError then
    lblStatus.Font.Color := clRed
  else
    lblStatus.Font.Color := clGreen;

  Application.ProcessMessages;
end;

procedure TfrmMain.rgConversionTypeClick(Sender: TObject);
begin
  UpdateControlsVisibility;
  btnClearClick(nil);
end;

function TfrmMain.GetFieldNameCase: TLZFieldNameCase;
begin
  case rgFieldNameCase.ItemIndex of
    0:
      Result := fncSnakeCase;
    1:
      Result := fncKebabCase;
    2:
      Result := fncCamelCase;
    3:
      Result := fncPascalCase;
  else
    Result := fncPascalCase;
  end;
end;

function TfrmMain.GetQuoteMode: TLZCSVQuoteMode;
begin
  case rgQuoteMode.ItemIndex of
    0:
      Result := qmMinimal;
    1:
      Result := qmAll;
    2:
      Result := qmNonNumeric;
    3:
      Result := qmNone;
  else
    Result := qmMinimal;
  end;
end;

function TfrmMain.GetJSONFormatMode: TJSONFormatMode;
begin
  if Assigned(rgJSONFormatMode) then
  begin
    case rgJSONFormatMode.ItemIndex of
      0:
        Result := jfmCompact;
      1:
        Result := jfmFormatted;
    else
      Result := jfmFormatted;
    end;
  end
  else
    Result := jfmFormatted;
end;

procedure TfrmMain.LoadCSVSample;
begin
  memoInput.Lines.Clear;
  memoInput.Lines.Add('Name,Age,Department,Skills,Active,Salary');
  memoInput.Lines.Add
    ('John Doe,30,Engineering,"[""Delphi"",""SQL""]",true,75000');
  memoInput.Lines.Add
    ('Jane Smith,25,Marketing,"[""Design"",""Analytics""]",true,65000');
  memoInput.Lines.Add
    ('Bob Johnson,35,Engineering,"[""Python"",""JavaScript""]",false,80000');
  memoInput.Lines.Add
    ('Alice Brown,28,HR,"[""Recruiting"",""Training""]",true,60000');
end;

procedure TfrmMain.LoadJSONSample;
begin
  memoInput.Lines.Clear;
  memoInput.Lines.Add('{');
  memoInput.Lines.Add('  "employees": [');
  memoInput.Lines.Add('    {');
  memoInput.Lines.Add('      "name": "John Doe",');
  memoInput.Lines.Add('      "age": 30,');
  memoInput.Lines.Add('      "department": "Engineering",');
  memoInput.Lines.Add('      "skills": ["Delphi", "SQL"],');
  memoInput.Lines.Add('      "active": true,');
  memoInput.Lines.Add('      "salary": 75000');
  memoInput.Lines.Add('    },');
  memoInput.Lines.Add('    {');
  memoInput.Lines.Add('      "name": "Jane Smith",');
  memoInput.Lines.Add('      "age": 25,');
  memoInput.Lines.Add('      "department": "Marketing",');
  memoInput.Lines.Add('      "skills": ["Design", "Analytics"],');
  memoInput.Lines.Add('      "active": true,');
  memoInput.Lines.Add('      "salary": 65000');
  memoInput.Lines.Add('    },');
  memoInput.Lines.Add('    {');
  memoInput.Lines.Add('      "name": "Bob Johnson",');
  memoInput.Lines.Add('      "age": 35,');
  memoInput.Lines.Add('      "department": "Engineering",');
  memoInput.Lines.Add('      "skills": ["Python", "JavaScript"],');
  memoInput.Lines.Add('      "active": false,');
  memoInput.Lines.Add('      "salary": 80000');
  memoInput.Lines.Add('    }');
  memoInput.Lines.Add('  ]');
  memoInput.Lines.Add('}');
end;

procedure TfrmMain.btnLoadSampleClick(Sender: TObject);
begin
  case rgConversionType.ItemIndex of
    0:
      LoadCSVSample; // CSV to JSON
    1:
      LoadJSONSample; // JSON to CSV
    2:
      LoadJSONSample; // JSON Format
  else
    if rgConversionType.ItemIndex = 0 then
      LoadCSVSample
    else
      LoadJSONSample;
  end;

  SetStatus('Sample data loaded');
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  memoInput.Clear;
  memoOutput.Clear;
  SetStatus('Cleared');
end;

procedure TfrmMain.btnLoadFileClick(Sender: TObject);
var
  LFileName: string;
begin
  if dlgOpen.Execute then
  begin
    LFileName := dlgOpen.FileName;
    try
      memoInput.Lines.LoadFromFile(LFileName);
      SetStatus('File loaded: ' + ExtractFileName(LFileName));
    except
      on E: Exception do
      begin
        SetStatus('Error loading file: ' + E.Message, True);
      end;
    end;
  end;
end;

procedure TfrmMain.btnSaveOutputClick(Sender: TObject);
var
  LFileName: string;
begin
  if memoOutput.Lines.Count = 0 then
  begin
    SetStatus('No output to save', True);
    Exit;
  end;

  if rgConversionType.ItemIndex = 0 then
  begin
    dlgSave.DefaultExt := '.json';
    dlgSave.FileName := 'output.json';
  end
  else
  begin
    dlgSave.DefaultExt := '.csv';
    dlgSave.FileName := 'output.csv';
  end;

  if dlgSave.Execute then
  begin
    LFileName := dlgSave.FileName;
    try
      memoOutput.Lines.SaveToFile(LFileName);
      SetStatus('Output saved: ' + ExtractFileName(LFileName));
    except
      on E: Exception do
      begin
        SetStatus('Error saving file: ' + E.Message, True);
      end;
    end;
  end;
end;

procedure TfrmMain.ConvertCSVToJSON;
var
  LCSVData: string;
  LJSONResult: TJSONValue;
  LArrayName: string;
begin
  LCSVData := memoInput.Text;

  if Trim(LCSVData) = '' then
  begin
    SetStatus('Please enter CSV data to convert', True);
    Exit;
  end;

  try
    SetStatus('Converting CSV to JSON...');

    LArrayName := Trim(edtArrayName.Text);
    LJSONResult := TLZCSVToJSON.Convert(LCSVData, GetFieldNameCase, LArrayName);
    try
      memoOutput.Text := TLZJSONFormatter.Format(LJSONResult,
        GetJSONFormatMode);
      SetStatus('CSV successfully converted to JSON');
    finally
      LJSONResult.Free;
    end;

  except
    on E: Exception do
    begin
      SetStatus('Error converting CSV to JSON: ' + E.Message, True);
    end;
  end;
end;

procedure TfrmMain.ConvertJSONToCSV;
var
  LJSONData: string;
  LCSVResult: string;
  LDelimiter: Char;
begin
  LJSONData := memoInput.Text;

  if Trim(LJSONData) = '' then
  begin
    SetStatus('Please enter JSON data to convert', True);
    Exit;
  end;

  try
    SetStatus('Converting JSON to CSV...');

    if Length(edtDelimiter.Text) > 0 then
      LDelimiter := edtDelimiter.Text[1]
    else
      LDelimiter := ',';

    LCSVResult := TLZJSONToCSV.Convert(LJSONData, GetFieldNameCase, LDelimiter,
      chkIncludeHeaders.Checked, GetQuoteMode);

    memoOutput.Text := LCSVResult;
    SetStatus('JSON successfully converted to CSV');

  except
    on E: Exception do
    begin
      SetStatus('Error converting JSON to CSV: ' + E.Message, True);
    end;
  end;
end;

procedure TfrmMain.FormatJSON;
var
  LJSONData: string;
  LFormattedResult: string;
  LArrayName: string;
begin
  LJSONData := memoInput.Text;

  if Trim(LJSONData) = '' then
  begin
    SetStatus('Please enter JSON data to format', True);
    Exit;
  end;

  try
    SetStatus('Formatting JSON...');

    LArrayName := Trim(edtArrayName.Text);

    LFormattedResult := TLZJSONFormatter.Format(LJSONData, GetFieldNameCase,
      LArrayName, cbRemoveWrapperArray.Checked, GetJSONFormatMode);

    memoOutput.Text := LFormattedResult;
    SetStatus('JSON successfully formatted');

  except
    on E: Exception do
    begin
      SetStatus('Error formatting JSON: ' + E.Message, True);
    end;
  end;
end;

procedure TfrmMain.btnConvertClick(Sender: TObject);
begin
  case rgConversionType.ItemIndex of
    0:
      ConvertCSVToJSON;
    1:
      ConvertJSONToCSV;
    2:
      FormatJSON;
  else
    if rgConversionType.ItemIndex = 0 then
      ConvertCSVToJSON
    else
      ConvertJSONToCSV;
  end;
end;

end.
