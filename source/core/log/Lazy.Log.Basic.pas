{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.Basic
  Author: Tristan Marlow
  Purpose: Simple in memory log

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

  ----------------------------------------------------------------------------; }
unit Lazy.Log.Basic;

interface

uses
  SysUtils, Classes, Lazy.Log, Lazy.Types, Lazy.ThreadedStringList;

type
  TLZLogBasic = class(TLZLog)
  private
    FLog: TLZThreadStringList;
  protected
    procedure LogMessage(ALogLevel: TLZLogLevel; AMessage: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Log(ASender: TObject; AMessage: string); override;
    procedure Debug(ASender: TObject; AProcedure: string;
      AMessage: string); override;
    procedure Warning(ASender: TObject; AMessage: string); override;
    procedure Error(ASender: TObject; AMessage: string;
      AErrorCode: integer = 0); overload; override;
    procedure Error(ASender: TObject; AException: Exception;
      AMessage: string = ''); overload; override;
    function LogText: string;
    class function Text: string;
  end;


implementation

{ TLZLogBasic }

procedure TLZLogBasic.AfterConstruction;
begin
  inherited;
  FLog := TLZThreadStringList.Create;
  FLog.Sorted := False;
end;

procedure TLZLogBasic.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FLog);
end;

procedure TLZLogBasic.Debug(ASender: TObject; AProcedure, AMessage: string);
begin
  LogMessage(logDebug, Format('[%s] %s', [AProcedure, AMessage]));
end;

procedure TLZLogBasic.Error(ASender: TObject; AException: Exception;
  AMessage: string);
begin
  LogMessage(logError, Format('%s %s', [AException.Message, AMessage]));
end;

procedure TLZLogBasic.Error(ASender: TObject; AMessage: string;
  AErrorCode: integer);
begin
  LogMessage(logError, Format('(%d) %s', [AErrorCode, AMessage]));
end;

procedure TLZLogBasic.Log(ASender: TObject; AMessage: string);
begin
  LogMessage(logInformation, AMessage);

end;

procedure TLZLogBasic.LogMessage(ALogLevel: TLZLogLevel; AMessage: string);
begin
  if IsLogLevel(ALogLevel) then
  begin
    FLog.Insert(0, GetLogLevelText(ALogLevel) + ':' + AMessage);
  end;
end;

function TLZLogBasic.LogText: string;
begin
  if Assigned(FLog) then
    Result := FLog.Text;

end;

class function TLZLogBasic.Text: string;
begin
  Result := '';
  if (LazyLog is TLZLogBasic) then
    Result := (LazyLog as TLZLogBasic).LogText;
end;

procedure TLZLogBasic.Warning(ASender: TObject; AMessage: string);
begin
  LogMessage(logWarning, AMessage);
end;

end.
