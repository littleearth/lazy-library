{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Types
  Author: Tristan Marlow
  Purpose: Common types

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

  ----------------------------------------------------------------------------; }
unit Lazy.Types;

interface

uses
  System.SyncObjs, System.SysUtils, System.Variants, System.Classes,
  REST.Types, REST.Client;

type
  ELazyException = class(Exception);
  TLZLogLevel = (logError, logWarning, logInformation, logDebug);

  TLZTimeRounding = (trNearest, trUp, trDown);
  TLZTimeFormat = (tfDefault, tf12Hour, tf24Hour);

  TDateTimeFunc = reference to function: TDateTime;

  TRESTRequest = REST.Client.TRESTRequest;
  TRESTResponse = REST.Client.TRESTResponse;
  TRESTRequestMethod = REST.Types.TRESTRequestMethod;

  TOnLog = procedure(ASender: TObject; AMessage: string) of object;
  TOnWarning = procedure(ASender: TObject; AMessage: string) of object;
  TOnDebug = procedure(ASender: TObject; AProcedure, AMessage: string)
    of object;
  TOnProgress = procedure(ASender: TObject; AProgress: integer;
    AMessage: string; var ACancel: boolean) of object;
  TOnError = procedure(ASender: TObject; AMessage: string; AErrorCode: integer)
    of object;
  TOnProgressRef = reference to procedure(ASender: TObject; AProgress: integer;
    AMessage: string; var ACancel: boolean);

  TLZObject = class(TObject)
  protected
    procedure Log(AMessage: string); virtual;
    procedure Debug(AProcedure: string; AMessage: string); virtual;
    procedure Warning(AMessage: string); virtual;
    procedure Error(AMessage: string; AErrorCode: integer = 0);
      overload; virtual;
    procedure Error(AException: Exception; AMessage: string = '');
      overload; virtual;
  end;

  TLZPersistent = class(TPersistent)
  protected
    procedure Log(AMessage: string); virtual;
    procedure Debug(AProcedure: string; AMessage: string); virtual;
    procedure Warning(AMessage: string); virtual;
    procedure Error(AMessage: string; AErrorCode: integer = 0);
      overload; virtual;
    procedure Error(AException: Exception; AMessage: string = '');
      overload; virtual;
  end;

  TLZComponent = class(TComponent)
  protected
    procedure Log(AMessage: string); virtual;
    procedure Debug(AProcedure: string; AMessage: string); virtual;
    procedure Warning(AMessage: string); virtual;
    procedure Error(AMessage: string; AErrorCode: integer = 0);
      overload; virtual;
    procedure Error(AException: Exception; AMessage: string = '');
      overload; virtual;
  end;

  TApplicationVersionDetails = class(TLZComponent)
  private
    FMajorVersion: integer;
    FMinorVersion: integer;
    FReleaseVersion: integer;
    FBuildVersion: integer;
  protected
    procedure SetVersion(AVersionString: string);
    function GetVersion: string;
  public
    procedure Reset;
    function AsString: string;
    function IsCurrent(ACompareVersion: string): boolean;
  published
    property Version: string read GetVersion write SetVersion;
    property MajorVersion: integer read FMajorVersion write FMajorVersion;
    property MinorVersion: integer read FMinorVersion write FMinorVersion;
    property ReleaseVersion: integer read FReleaseVersion write FReleaseVersion;
    property BuildVersion: integer read FBuildVersion write FBuildVersion;
  end;

implementation

uses
  Lazy.Log;

{ TLZObject }

procedure TLZObject.Debug(AProcedure, AMessage: string);
begin
  LazyLog.Debug(Self, AProcedure, AMessage);
end;

procedure TLZObject.Error(AException: Exception; AMessage: string);
begin
  LazyLog.Error(Self, AException, AMessage);
end;

procedure TLZObject.Error(AMessage: string; AErrorCode: integer);
begin
  LazyLog.Error(Self, AMessage, AErrorCode);
end;

procedure TLZObject.Log(AMessage: string);
begin
  LazyLog.Log(Self, AMessage);
end;

procedure TLZObject.Warning(AMessage: string);
begin
  LazyLog.Warning(Self, AMessage);
end;

{ TLZComponent }

procedure TLZComponent.Debug(AProcedure, AMessage: string);
begin
  LazyLog.Debug(Self, AProcedure, AMessage);
end;

procedure TLZComponent.Error(AException: Exception; AMessage: string);
begin
  LazyLog.Error(Self, AException, AMessage);
end;

procedure TLZComponent.Error(AMessage: string; AErrorCode: integer);
begin
  LazyLog.Error(Self, AMessage, AErrorCode);
end;

procedure TLZComponent.Log(AMessage: string);
begin
  LazyLog.Log(Self, AMessage);
end;

procedure TLZComponent.Warning(AMessage: string);
begin
  LazyLog.Warning(Self, AMessage);
end;
{ TApplicationVersionDetails }

procedure TApplicationVersionDetails.Reset;
begin
  FMajorVersion := 0;
  FMinorVersion := 0;
  FReleaseVersion := 0;
  FBuildVersion := 0;
end;

function TApplicationVersionDetails.AsString: string;
begin
  Result := IntToStr(FMajorVersion) + '.' + IntToStr(FMinorVersion) + '.' +
    IntToStr(FReleaseVersion) + '.' + IntToStr(FBuildVersion);
end;

function TApplicationVersionDetails.GetVersion: string;
begin
  Result := AsString;
end;

procedure TApplicationVersionDetails.SetVersion(AVersionString: string);
var
  TempStr: string;
  Idx, VersionIdx: integer;
begin
  Reset;
  TempStr := '';
  Idx := 1;
  VersionIdx := 0;
  AVersionString := AVersionString + '.';
  while (Idx <= Length(AVersionString)) do
  begin
    if (AVersionString[Idx] = '.') or (AVersionString[Idx] = ' ') then
    begin
      case VersionIdx of
        0:
          FMajorVersion := StrToIntDef(TempStr, 0);
        1:
          FMinorVersion := StrToIntDef(TempStr, 0);
        2:
          FReleaseVersion := StrToIntDef(TempStr, 0);
        3:
          FBuildVersion := StrToIntDef(TempStr, 0);
      end;
      Inc(VersionIdx);
      TempStr := '';
    end
    else
    begin
      TempStr := TempStr + AVersionString[Idx];
    end;
    Inc(Idx);
  end;
end;

function TApplicationVersionDetails.IsCurrent(ACompareVersion: string): boolean;
var
  CompareVersionDetails: TApplicationVersionDetails;
  compareVersionMajorMinor: integer;
  compareVersionReleaseBuild: integer;
  currentVersionMajorMinor: integer;
  currentVersionReleaseBuild: integer;
begin
  Result := false;

  CompareVersionDetails := TApplicationVersionDetails.Create(nil);
  try
    CompareVersionDetails.Version := ACompareVersion;
    Debug('IsCurrent', Self.AsString + ' <> ' + CompareVersionDetails.AsString);

    compareVersionMajorMinor := CompareVersionDetails.MajorVersion * 1000 +
      CompareVersionDetails.MinorVersion;
    currentVersionMajorMinor := Self.MajorVersion * 1000 + Self.MinorVersion;

    if compareVersionMajorMinor > currentVersionMajorMinor then
    begin
      Result := True;
    end
    else
    begin
      if compareVersionMajorMinor = currentVersionMajorMinor then
      begin
        compareVersionReleaseBuild := CompareVersionDetails.ReleaseVersion *
          1000 + CompareVersionDetails.BuildVersion;
        currentVersionReleaseBuild := Self.ReleaseVersion * 1000 +
          Self.BuildVersion;

        if compareVersionReleaseBuild >= currentVersionReleaseBuild then
        begin
          Result := True;
        end;
      end;
    end;
  finally
    FreeAndNil(CompareVersionDetails)
  end;
end;

{ TLZPersistent }

procedure TLZPersistent.Debug(AProcedure, AMessage: string);
begin
  LazyLog.Debug(Self, AProcedure, AMessage);
end;

procedure TLZPersistent.Error(AException: Exception; AMessage: string);
begin
  LazyLog.Error(Self, AException, AMessage);
end;

procedure TLZPersistent.Error(AMessage: string; AErrorCode: integer);
begin
  LazyLog.Error(Self, AMessage, AErrorCode);
end;

procedure TLZPersistent.Log(AMessage: string);
begin
  LazyLog.Log(Self, AMessage);
end;

procedure TLZPersistent.Warning(AMessage: string);
begin
  LazyLog.Warning(Self, AMessage);
end;

end.
