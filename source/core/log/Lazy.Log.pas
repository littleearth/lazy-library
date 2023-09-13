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
  TLZLog = class(TObject)
  private
    FLogLevel: TLZLogLevel;
    procedure SeTLZLogLevel(const Value: TLZLogLevel);
  protected
    procedure OutputToDebugger(const AMessage: String);
    function IsLogLevel(ALogLevel: TLZLogLevel): boolean;
  public
    constructor Create;
    class function GetLogLevelText(ALogLevel: TLZLogLevel): string;
    procedure Log(ASender: TObject; AMessage: string); virtual;
    procedure Debug(ASender: TObject; AProcedure: string;
      AMessage: string); virtual;
    procedure Warning(ASender: TObject; AMessage: string); virtual;
    procedure Error(ASender: TObject; AMessage: string;
      AErrorCode: integer = 0); overload; virtual;
    procedure Error(ASender: TObject; AException: Exception;
      AMessage: string = ''); overload; virtual;
    property LogLevel: TLZLogLevel read FLogLevel write SeTLZLogLevel;
  end;

  TLZLogClass = class of TLZLog;

var
  _LazyLog: TLZLog;
  _LazyLogClass: TLZLogClass;

procedure SetLazyLogClass(ALazyLogClass: TLZLogClass);
function LazyLog: TLZLog;

implementation

uses
  Winapi.Windows;

{ TLZLog }

procedure TLZLog.OutputToDebugger(const AMessage: String);
begin
  OutputDebugString(PChar(AMessage))
end;

procedure TLZLog.SeTLZLogLevel(const Value: TLZLogLevel);
begin
  FLogLevel := Value;
end;

constructor TLZLog.Create;
begin
  inherited;
{$IFDEF DEBUG}
  FLogLevel := logDebug;
{$ELSE}
  FLogLevel := logError;
{$ENDIF}
end;

procedure TLZLog.Debug(ASender: TObject; AProcedure, AMessage: string);
begin
  // OutputToDebugger('DEBUG:' + AProcedure + ': ' + AMessage);
end;

procedure TLZLog.Error(ASender: TObject; AException: Exception;
  AMessage: string);
begin
{$IFDEF DEBUG}
  OutputToDebugger('ERROR:' + AException.Message + ': ' + AMessage);
{$ENDIF}
end;

class function TLZLog.GetLogLevelText(ALogLevel: TLZLogLevel): string;
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

function TLZLog.IsLogLevel(ALogLevel: TLZLogLevel): boolean;
begin
  Result := (ord(ALogLevel) <= ord(FLogLevel));
end;

procedure TLZLog.Error(ASender: TObject; AMessage: string; AErrorCode: integer);
begin
end;

procedure TLZLog.Log(ASender: TObject; AMessage: string);
begin

end;

procedure TLZLog.Warning(ASender: TObject; AMessage: string);
begin

end;

procedure SetLazyLogClass(ALazyLogClass: TLZLogClass);
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

function LazyLog: TLZLog;
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
      _LazyLog := TLZLog.Create;
    end;
  end;
  if Assigned(_LazyLog) then
  begin
    Result := _LazyLog;
  end;
end;

initialization

SetLazyLogClass(TLZLog);

finalization

SetLazyLogClass(nil);

end.
