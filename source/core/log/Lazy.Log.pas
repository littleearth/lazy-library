{ -----------------------------------------------------------------------------
  Unit Name: Lazy.ThreadedFileStream
  Author: Tristan Marlow
  Purpose: Simple log object; create descendant objects to incorporate into
  other logging systems or expand logging

  - See Lazy.Log.Basic and Lazy.Log.FileStream for examples

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions


  ----------------------------------------------------------------------------; }
unit Lazy.Log;

interface

uses
  SysUtils, Classes, Lazy.Types;

type
  TLazyLog = class(TObject)
  private
    FLogLevel: TLazyLogLevel;
    procedure SeTLazyLogLevel(const Value: TLazyLogLevel);
  protected
    procedure OutputToDebugger(const AMessage: String);
    function IsLogLevel(ALogLevel: TLazyLogLevel): boolean;
  public
    constructor Create;
    class function GetLogLevelText(ALogLevel: TLazyLogLevel): string;
    procedure Log(ASender: TObject; AMessage: string); virtual;
    procedure Debug(ASender: TObject; AProcedure: string;
      AMessage: string); virtual;
    procedure Warning(ASender: TObject; AMessage: string); virtual;
    procedure Error(ASender: TObject; AMessage: string;
      AErrorCode: integer = 0); overload; virtual;
    procedure Error(ASender: TObject; AException: Exception;
      AMessage: string = ''); overload; virtual;
    property LogLevel: TLazyLogLevel read FLogLevel write SeTLazyLogLevel;
  end;

  TLazyLogClass = class of TLazyLog;

var
  _LazyLog: TLazyLog;
  _LazyLogClass: TLazyLogClass;

procedure SetLazyLogClass(ALazyLogClass: TLazyLogClass);
function LazyLog: TLazyLog;

implementation

uses
  Winapi.Windows;

{ TLazyLog }

procedure TLazyLog.OutputToDebugger(const AMessage: String);
begin
  OutputDebugString(PChar(AMessage))
end;

procedure TLazyLog.SeTLazyLogLevel(const Value: TLazyLogLevel);
begin
  FLogLevel := Value;
end;

constructor TLazyLog.Create;
begin
  inherited;
{$IFDEF DEBUG}
  FLogLevel := logDebug;
{$ELSE}
  FLogLevel := logError;
{$ENDIF}
end;

procedure TLazyLog.Debug(ASender: TObject; AProcedure, AMessage: string);
begin
  // OutputToDebugger('DEBUG:' + AProcedure + ': ' + AMessage);
end;

procedure TLazyLog.Error(ASender: TObject; AException: Exception;
  AMessage: string);
begin
{$IFDEF DEBUG}
  OutputToDebugger('ERROR:' + AException.Message + ': ' + AMessage);
{$ENDIF}
end;

class function TLazyLog.GetLogLevelText(ALogLevel: TLazyLogLevel): string;
begin
  case ALogLevel of
    logDebug:
      Result := 'DEBUG';
    logInformation:
      Result := 'INFO';
    logWarning:
      Result := 'WARN';
  else
    begin
      Result := 'ERROR';
    end;
  end;
end;

function TLazyLog.IsLogLevel(ALogLevel: TLazyLogLevel): boolean;
begin
  Result := (ord(ALogLevel) <= ord(FLogLevel));
end;

procedure TLazyLog.Error(ASender: TObject; AMessage: string;
  AErrorCode: integer);
begin
end;

procedure TLazyLog.Log(ASender: TObject; AMessage: string);
begin

end;

procedure TLazyLog.Warning(ASender: TObject; AMessage: string);
begin

end;

procedure SetLazyLogClass(ALazyLogClass: TLazyLogClass);
begin
  _LazyLogClass := ALazyLogClass;
  try
    if Assigned(_LazyLog) then
    begin
      FreeAndNil(_LazyLog);
    end;
  except
    _LazyLog := nil;
  end;
end;

function LazyLog: TLazyLog;
begin
  Result := nil;
  if not Assigned(_LazyLog) then
  begin
    if Assigned(_LazyLogClass) then
    begin
      _LazyLog := _LazyLogClass.Create;
    end
    else
    begin
      _LazyLog := TLazyLog.Create;
    end;
  end;
  if Assigned(_LazyLog) then
  begin
    Result := _LazyLog;
  end;
end;

initialization

SetLazyLogClass(TLazyLog);

finalization

SetLazyLogClass(nil);

end.
