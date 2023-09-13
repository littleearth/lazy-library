unit TestBaseU;

interface

uses
  WinApi.Windows, Classes, DUnitX.TestFramework,
  VCL.Forms, VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls;

type
  TTestBase = class
  private
    function GetComponentRecursive<T: TComponent>(ARootName: string;
      AComponent: TComponent; AName: string): T;
  protected
    FFrm: TForm;
    FWndClass: TWndClass;
    FHwnd: HWND;
    function GetComponent<T: TComponent>(AComponent: TComponent; AName: string;
      ARecursive: boolean): T;
    property Form: TForm read FFrm;
  public
    [Setup]
    procedure Setup; virtual;
    [Teardown]
    procedure Teardown; virtual;
  end;

implementation

function TTestBase.GetComponentRecursive<T>(ARootName: string;
  AComponent: TComponent; AName: string): T;
var
  LCtrl: TComponent;
  i: integer;
begin
  Result := nil;
  LCtrl := AComponent.FindComponent(AName);

  if LCtrl = nil then
    LCtrl := AComponent.FindComponent(ARootName + '_' + AName);

  if LCtrl = nil then
  begin
    for i := 0 to AComponent.ComponentCount - 1 do
    begin
      Result := GetComponentRecursive<T>(AComponent.Components[i].Name,
        AComponent.Components[i], AName);

      if Result <> nil then
        break;
    end;
  end;
end;

function TTestBase.GetComponent<T>(AComponent: TComponent; AName: string;
  ARecursive: boolean): T;
var
  i: integer;
begin
  Result := AComponent.FindComponent(AName) as T;

  if Result = nil then
    Result := AComponent.FindComponent(AComponent.Name + '_' + AName) as T;

  if ARecursive and (Result = nil) then
  begin
    Result := GetComponentRecursive<T>(AComponent.Name, AComponent, AName);
  end;
end;

procedure TTestBase.Setup;
  function WindowProc(HWND, Msg: Longint; wParam: wParam; lParam: lParam)
    : Longint; stdcall;
  begin
    Result := DefWindowProc(HWND, Msg, wParam, lParam);
  end;

begin
  // Create invisible window
  FWndClass.hInstance := hInstance;
  with FWndClass do
  begin
    lpszClassName := 'LazyVCLTestWindow';
    Style := CS_PARENTDC or CS_BYTEALIGNCLIENT;
    hIcon := LoadIcon(hInstance, 'MAINICON');
    lpfnWndProc := @WindowProc;
    hbrBackground := COLOR_BTNFACE + 1;
    hCursor := LoadCursor(0, IDC_ARROW);
  end;
  WinApi.Windows.RegisterClass(FWndClass);
  FHwnd := CreateWindow(FWndClass.lpszClassName, 'Lazy VCL Test Window',
    WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU,
    (GetSystemMetrics(SM_CXSCREEN) div 2) - 190,
    (GetSystemMetrics(SM_CYSCREEN) div 2) - 170, 386, 200, 0, 0,
    hInstance, nil);

  FFrm := TForm.CreateParented(FHwnd);
end;

procedure TTestBase.Teardown;
begin
  FFrm.Free;
  if FHwnd <> 0 then
    DestroyWindow(FHwnd);
  WinApi.Windows.UnregisterClass(FWndClass.lpszClassName, hInstance);
end;

end.
