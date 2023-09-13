{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.FileStream
  Author: Tristan Marlow
  Purpose: Log output to a file; default file name is %project%.log

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

----------------------------------------------------------------------------; }
unit Lazy.Log.FileStream;

interface

uses
  SysUtils, Classes, System.SyncObjs, Lazy.Log, Lazy.Types,
  Lazy.ThreadedFileStream, Lazy.Utils;

type
  TLZLogFileStream = class(TLZLog)
  private
    FLogFileLocker: TCriticalSection;
    FLog: TLZThreadedFileStream;
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  protected
    procedure StartLog;
    procedure StopLog;
    procedure LogMessage(ALogLevel: TLZLogLevel; AMessage: string);
    function GetDefaultFileName: TFileName; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ClearLogFile;
    procedure FlushLogFile;
    procedure Log(ASender: TObject; AMessage: string); override;
    procedure Debug(ASender: TObject; AProcedure: string;
      AMessage: string); override;
    procedure Warning(ASender: TObject; AMessage: string); override;
    procedure Error(ASender: TObject; AMessage: string;
      AErrorCode: integer = 0); overload; override;
    procedure Error(ASender: TObject; AException: Exception;
      AMessage: string = ''); overload; override;
    property FileName: TFileName read FFileName write SetFileName;
  end;

implementation

{ TLZLogBasic }

procedure TLZLogFileStream.AfterConstruction;
begin
  inherited;
  FFileName := GetDefaultFileName;
  FLog := nil;
  FLogFileLocker := TCriticalSection.Create;
end;

procedure TLZLogFileStream.BeforeDestruction;
begin
  try
    if Assigned(FLogFileLocker) then
    begin
      FLogFileLocker.Free;
      FLogFileLocker := nil;
    end;
  finally
    inherited;
  end;
end;

procedure TLZLogFileStream.Debug(ASender: TObject;
  AProcedure, AMessage: string);
begin
  LogMessage(logDebug, Format('[%s] %s', [AProcedure, AMessage]));
end;

procedure TLZLogFileStream.Error(ASender: TObject; AException: Exception;
  AMessage: string);
begin
  LogMessage(logError, Format('%s %s', [AException.Message, AMessage]));
end;

procedure TLZLogFileStream.Error(ASender: TObject; AMessage: string;
  AErrorCode: integer);
begin
  LogMessage(logError, Format('(%d) %s', [AErrorCode, AMessage]));
end;

procedure TLZLogFileStream.Log(ASender: TObject; AMessage: string);
begin
  LogMessage(logInformation, AMessage);

end;

procedure TLZLogFileStream.LogMessage(ALogLevel: TLZLogLevel;
  AMessage: string);
var
  LMessage: string;
begin
  if IsLogLevel(ALogLevel) then
  begin
    FLogFileLocker.Acquire;
    try
      if not Assigned(FLog) then
        StartLog;
      if Assigned(FLog) then
      begin
        LMessage := Format('%s;%s;%s', [FormatDateTime('yyyy-mm-dd hh:nn:ss',
          Now), GetLogLevelText(ALogLevel), AMessage]) + #13#10;
        FLog.AppendStr(LMessage);
      end;
    finally
      FLogFileLocker.Release;
    end;
  end;
end;

procedure TLZLogFileStream.SetFileName(const Value: TFileName);
begin
  if not SameText(Value, FFileName) then
  begin
    StopLog;
  end;
  FFileName := Value;
end;

procedure TLZLogFileStream.StartLog;
begin
  if not Assigned(FLog) then
    FLog := TLZThreadedFileStream.Create(FFileName);
  ClearLogFile;
end;

procedure TLZLogFileStream.StopLog;
begin
  if Assigned(FLog) then
  begin
    try
      FLog.Flush;
      FLog.Terminate;
      FLog.Free;
    finally
      FLog := nil;
    end;
  end;
end;

procedure TLZLogFileStream.Warning(ASender: TObject; AMessage: string);
begin
  LogMessage(logWarning, AMessage);
end;

procedure TLZLogFileStream.FlushLogFile;
begin
  if Assigned(FLog) then
    FLog.Flush;
end;

function TLZLogFileStream.GetDefaultFileName: TFileName;
begin
  Result := ChangeFileExt(ParamStr(0), '.log');
end;

procedure TLZLogFileStream.ClearLogFile;
begin
  if Assigned(FLog) then
    FLog.Clear;
end;

initialization

SetLazyLogClass(TLZLogFileStream);

finalization

SetLazyLogClass(nil);

end.
