unit frmExceptionDetailsExampleU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Lazy.Exception.Details;

type
  TfrmExceptionDetailsExample = class(TForm)
    pnlTop: TPanel;
    lblTitle: TLabel;
    pgcMain: TPageControl;
    tsBasicException: TTabSheet;
    tsDivideByZero: TTabSheet;
    tsNilReference: TTabSheet;
    tsCustomHandler: TTabSheet;
    btnBasicException: TButton;
    memoBasicResult: TMemo;
    btnDivideByZero: TButton;
    memoDivideResult: TMemo;
    btnNilReference: TButton;
    memoNilResult: TMemo;
    btnRegisterHandler: TButton;
    btnUnregisterHandler: TButton;
    btnTestWithHandler: TButton;
    memoHandlerResult: TMemo;
    lblBasicInfo: TLabel;
    lblDivideInfo: TLabel;
    lblNilInfo: TLabel;
    lblHandlerInfo: TLabel;
    rgOutputFormat: TRadioGroup;
    pnlBasicButtons: TPanel;
    pnlDivideButtons: TPanel;
    pnlNilButtons: TPanel;
    pnlHandlerButtons: TPanel;
    btnClearBasic: TButton;
    btnClearDivide: TButton;
    btnClearNil: TButton;
    btnClearHandler: TButton;
    lblHandlerStatus: TLabel;
    procedure btnBasicExceptionClick(Sender: TObject);
    procedure btnDivideByZeroClick(Sender: TObject);
    procedure btnNilReferenceClick(Sender: TObject);
    procedure btnRegisterHandlerClick(Sender: TObject);
    procedure btnUnregisterHandlerClick(Sender: TObject);
    procedure btnTestWithHandlerClick(Sender: TObject);
    procedure btnClearBasicClick(Sender: TObject);
    procedure btnClearDivideClick(Sender: TObject);
    procedure btnClearNilClick(Sender: TObject);
    procedure btnClearHandlerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCustomHandler: ILZExceptionHandler;
    procedure ShowExceptionDetails(const AException: Exception; const ASender: TObject; const AMemo: TMemo);
    procedure UpdateHandlerStatus;
  public
    { Public declarations }
  end;

  // Custom handler for demonstration
  TCustomExceptionHandler = class(TInterfacedObject, ILZExceptionHandler)
  public
    procedure EnhanceExceptionDetails(const ADetails: TLZExceptionDetails);
    function GetHandlerName: string;
  end;

var
  frmExceptionDetailsExample: TfrmExceptionDetailsExample;

implementation

{$R *.dfm}

uses
  System.IOUtils;

{ TCustomExceptionHandler }

procedure TCustomExceptionHandler.EnhanceExceptionDetails(const ADetails: TLZExceptionDetails);
var
  LEnhancement: string;
begin
  LEnhancement := '=== Custom Handler Enhancement ===' + #13#10 +
    'Computer Name: ' + GetEnvironmentVariable('COMPUTERNAME') + #13#10 +
    'User Name: ' + GetEnvironmentVariable('USERNAME') + #13#10 +
    'OS Version: ' + TOSVersion.ToString + #13#10 +
    'Handler Timestamp: ' + DateTimeToStr(Now) + #13#10 +
    '==================================';

  if ADetails.StackTrace <> '' then
    ADetails.StackTrace := ADetails.StackTrace + #13#10#13#10 + LEnhancement
  else
    ADetails.StackTrace := LEnhancement;
end;

function TCustomExceptionHandler.GetHandlerName: string;
begin
  Result := 'Custom Demo Handler';
end;

{ TfrmExceptionDetailsExample }

procedure TfrmExceptionDetailsExample.FormCreate(Sender: TObject);
begin
  pgcMain.ActivePageIndex := 0;
  rgOutputFormat.ItemIndex := 0;
  UpdateHandlerStatus;
end;

procedure TfrmExceptionDetailsExample.FormDestroy(Sender: TObject);
begin
  if Assigned(FCustomHandler) then
    TLZExceptionDetails.UnregisterHandler(FCustomHandler);
  TLZExceptionDetails.ClearHandlers;
end;

procedure TfrmExceptionDetailsExample.ShowExceptionDetails(const AException: Exception; 
  const ASender: TObject; const AMemo: TMemo);
var
  LDetails: TLZExceptionDetails;
  LOutput: string;
begin
  LDetails := TLZExceptionDetails.Create(AException, ASender, ExceptAddr);
  try
    case rgOutputFormat.ItemIndex of
      0: LOutput := LDetails.AsString;
      1: LOutput := LDetails.AsXML;
      2: LOutput := LDetails.AsJSON;
    else
      LOutput := LDetails.AsString;
    end;

    AMemo.Lines.Text := LOutput;
  finally
    LDetails.Free;
  end;
end;

procedure TfrmExceptionDetailsExample.UpdateHandlerStatus;
begin
  if Assigned(FCustomHandler) then
    lblHandlerStatus.Caption := 'Handler Status: REGISTERED'
  else
    lblHandlerStatus.Caption := 'Handler Status: NOT REGISTERED';
end;

procedure TfrmExceptionDetailsExample.btnBasicExceptionClick(Sender: TObject);
begin
  try
    raise Exception.Create('This is a basic test exception');
  except
    on E: Exception do
      ShowExceptionDetails(E, Sender, memoBasicResult);
  end;
end;

procedure TfrmExceptionDetailsExample.btnDivideByZeroClick(Sender: TObject);
var
  LValue1: Integer;
  LValue2: Integer;
  LResult: Integer;
begin
  try
    LValue1 := 100;
    LValue2 := 0;
    LResult := LValue1 div LValue2;  // This will raise EDivByZero
  except
    on E: Exception do
      ShowExceptionDetails(E, Sender, memoDivideResult);
  end;
end;

procedure TfrmExceptionDetailsExample.btnNilReferenceClick(Sender: TObject);
var
  LObject: TObject;
begin
  try
    LObject := nil;
    LObject.Free;  // This will raise EAccessViolation
  except
    on E: Exception do
      ShowExceptionDetails(E, Sender, memoNilResult);
  end;
end;

procedure TfrmExceptionDetailsExample.btnRegisterHandlerClick(Sender: TObject);
begin
  if not Assigned(FCustomHandler) then
  begin
    FCustomHandler := TCustomExceptionHandler.Create;
    TLZExceptionDetails.RegisterHandler(FCustomHandler);
    ShowMessage('Custom handler registered successfully!');
    UpdateHandlerStatus;
  end
  else
    ShowMessage('Handler is already registered.');
end;

procedure TfrmExceptionDetailsExample.btnUnregisterHandlerClick(Sender: TObject);
begin
  if Assigned(FCustomHandler) then
  begin
    TLZExceptionDetails.UnregisterHandler(FCustomHandler);
    FCustomHandler := nil;
    ShowMessage('Custom handler unregistered successfully!');
    UpdateHandlerStatus;
  end
  else
    ShowMessage('No handler is currently registered.');
end;

procedure TfrmExceptionDetailsExample.btnTestWithHandlerClick(Sender: TObject);
begin
  if not Assigned(FCustomHandler) then
  begin
    ShowMessage('Please register the handler first by clicking "Register Handler".');
    Exit;
  end;

  try
    raise Exception.Create('Testing exception with custom handler');
  except
    on E: Exception do
      ShowExceptionDetails(E, Sender, memoHandlerResult);
  end;
end;

procedure TfrmExceptionDetailsExample.btnClearBasicClick(Sender: TObject);
begin
  memoBasicResult.Clear;
end;

procedure TfrmExceptionDetailsExample.btnClearDivideClick(Sender: TObject);
begin
  memoDivideResult.Clear;
end;

procedure TfrmExceptionDetailsExample.btnClearNilClick(Sender: TObject);
begin
  memoNilResult.Clear;
end;

procedure TfrmExceptionDetailsExample.btnClearHandlerClick(Sender: TObject);
begin
  memoHandlerResult.Clear;
end;

end.

