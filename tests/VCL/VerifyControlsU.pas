unit VerifyControlsU;

interface

uses
  DUnitX.TestFramework, VCL.Controls;

type
  TCtrlVerification = record
  public
    Name: string;
    ClassType: TClass;
    SubControls: array of TCtrlVerification;

    function SubControlCount: integer;
  end;

  TAssertHelper_VerifyControls = class helper for Assert
  public
    class procedure HasControls(AControl: TWinControl; AVerifyAgainst: array of TCtrlVerification);
  end;

function VerifyControl(AName: string; ASubControls: array of TCtrlVerification): TCtrlVerification; overload;
function VerifyControl(AClass: TClass; ASubControls: array of TCtrlVerification): TCtrlVerification; overload;
function VerifyControl(AName: string): TCtrlVerification; overload;
function VerifyControl(AClass: TClass): TCtrlVerification; overload;
//procedure AssertHasControls(AControl: TWinControl; AVerifyAgainst: array of TCtrlVerification);

implementation

uses
  SysUtils;

function TCtrlVerification.SubControlCount: integer;
begin
  Result := Length(SubControls);
end;

class procedure TAssertHelper_VerifyControls.HasControls(
  AControl: TWinControl;
  AVerifyAgainst: array of TCtrlVerification);

  procedure VerifyControlsRecursive(
    APathPrefix: string;
    APath: string;
    AControl: TWinControl;
    AVerifyAgainst: TCtrlVerification);
  var
    i: integer;
    LFoundName: boolean;
    LFoundCtrl: TWinControl;
  begin
    LFoundName := False;
    LFoundCtrl := nil;
    for i := 0 to AControl.ControlCount - 1 do
    begin
      if ((AVerifyAgainst.Name <> '') and (AControl.Controls[i].Name = APathPrefix + AVerifyAgainst.Name)) or
      ((AVerifyAgainst.Name = '') and (AControl.Controls[i].ClassType = AVerifyAgainst.ClassType)) then
      begin
        LFoundName := True;

        if AVerifyAgainst.SubControlCount > 0 then
          LFoundCtrl := AControl.Controls[i] as TWinControl;
      end;
    end;

    if not LFoundName then
      Assert.Fail(APathPrefix.Substring(0, Length(APathPrefix) - 1) + '>' + APath + ' was not found');

    for i := 0 to AVerifyAgainst.SubControlCount - 1 do
    begin
      VerifyControlsRecursive(APathPrefix, APath + '>' + AVerifyAgainst.SubControls[i].Name,
        LFoundCtrl, AVerifyAgainst.SubControls[i]);
    end;
  end;
var
  i: integer;
begin
  for i := 0 to Length(AVerifyAgainst) - 1 do
  begin
    VerifyControlsRecursive(AControl.Name + '_', AVerifyAgainst[i].Name, AControl, AVerifyAgainst[i]);
  end;
end;

function VerifyControl(AName: string; ASubControls: array of TCtrlVerification): TCtrlVerification;
var
  i: integer;
begin
  Result.Name := AName;

  SetLength(Result.SubControls, Length(ASubControls));
  for i := 0 to Length(ASubControls) - 1 do
  begin
    Result.SubControls[i] := ASubControls[i];
  end;
end;

function VerifyControl(AClass: TClass; ASubControls: array of TCtrlVerification): TCtrlVerification;
var
  i: integer;
begin
  Result.Name := '';
  Result.ClassType := AClass;

  SetLength(Result.SubControls, Length(ASubControls));
  for i := 0 to Length(ASubControls) - 1 do
  begin
    Result.SubControls[i] := ASubControls[i];
  end;
end;

function VerifyControl(AName: string): TCtrlVerification;
begin
  Result.Name := AName;
  SetLength(Result.SubControls, 0);
end;

function VerifyControl(AClass: TClass): TCtrlVerification;
begin
  Result.Name := '';
  Result.ClassType := AClass;
  SetLength(Result.SubControls, 0);
end;

procedure AssertHasControls(AControl: TWinControl; AVerifyAgainst: array of TCtrlVerification);
  procedure VerifyControlsRecursive(
    APathPrefix: string;
    APath: string;
    AControl: TWinControl;
    AVerifyAgainst: TCtrlVerification);
  var
    i: integer;
    LFoundName: boolean;
    LFoundCtrl: TWinControl;
  begin
    LFoundName := False;
    LFoundCtrl := nil;
    for i := 0 to AControl.ControlCount - 1 do
    begin
      if ((AVerifyAgainst.Name <> '') and (AControl.Controls[i].Name = APathPrefix + AVerifyAgainst.Name)) or
      ((AVerifyAgainst.Name = '') and (AControl.Controls[i].ClassType = AVerifyAgainst.ClassType)) then
      begin
        LFoundName := True;

        if AVerifyAgainst.SubControlCount > 0 then
          LFoundCtrl := AControl.Controls[i] as TWinControl;
      end;
    end;

    if not LFoundName then
      Assert.Fail(APathPrefix.Substring(0, Length(APathPrefix) - 1) + '>' + APath + ' was not found');

    for i := 0 to AVerifyAgainst.SubControlCount - 1 do
    begin
      VerifyControlsRecursive(APathPrefix, APath + '>' + AVerifyAgainst.SubControls[i].Name,
        LFoundCtrl, AVerifyAgainst.SubControls[i]);
    end;
  end;
var
  i: integer;
begin
  for i := 0 to Length(AVerifyAgainst) - 1 do
  begin
    VerifyControlsRecursive(AControl.Name + '_', AVerifyAgainst[i].Name, AControl, AVerifyAgainst[i]);
  end;
end;

end.
