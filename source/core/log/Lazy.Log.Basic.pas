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
  TLazyLogBasic = class(TLazyLog)
  private
    FLog: TThreadStringList;
  protected
    procedure LogMessage(ALogLevel: TLazyLogLevel; AMessage: string);
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
  end;

implementation

{ TLazyLogBasic }

procedure TLazyLogBasic.AfterConstruction;
begin
  inherited;
  FLog := TThreadStringList.Create;
  FLog.Sorted := False;
end;

procedure TLazyLogBasic.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FLog);
end;

procedure TLazyLogBasic.Debug(ASender: TObject; AProcedure, AMessage: string);
begin
  LogMessage(logDebug, Format('[%s] %s', [AProcedure, AMessage]));
end;

procedure TLazyLogBasic.Error(ASender: TObject; AException: Exception;
  AMessage: string);
begin
  LogMessage(logError, Format('%s %s', [AException.Message, AMessage]));
end;

procedure TLazyLogBasic.Error(ASender: TObject; AMessage: string;
  AErrorCode: integer);
begin
  LogMessage(logError, Format('(%d) %s', [AErrorCode, AMessage]));
end;

procedure TLazyLogBasic.Log(ASender: TObject; AMessage: string);
begin
  LogMessage(logInformation, AMessage);

end;

procedure TLazyLogBasic.LogMessage(ALogLevel: TLazyLogLevel; AMessage: string);
begin
  if IsLogLevel(ALogLevel) then
  begin
    FLog.Insert(0, GetLogLevelText(ALogLevel) + ':' + AMessage);
  end;
end;

function TLazyLogBasic.LogText: string;
begin
  if Assigned(FLog) then
    Result := FLog.Text;

end;

procedure TLazyLogBasic.Warning(ASender: TObject; AMessage: string);
begin
  LogMessage(logWarning, AMessage);
end;

end.
