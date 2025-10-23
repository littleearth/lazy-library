unit CustomLogHandler;

interface

uses
  SysUtils, Classes, Lazy.Log, Lazy.Types;

type
  TCustomLogHandler = class(TLZLogHandler)
  private
  protected

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetFormattedMessage(ALogMessage: TLazyLogMessage; APrefixLogLevel : boolean): string; override;
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
  end;

implementation

uses
  Lazy.Utils;

{ TCustomLogHandler }

procedure TCustomLogHandler.AfterConstruction;
begin
  inherited;
end;

procedure TCustomLogHandler.BeforeDestruction;
begin
  inherited;
end;

function TCustomLogHandler.GetFormattedMessage(ALogMessage: TLazyLogMessage; APrefixLogLevel : boolean): string;
begin
    Result := inherited GetFormattedMessage(ALogMessage, APrefixLogLevel);
    if TLZString.IsEmptyString(Result) then
      Exit;
    Result :=  Result + ' - Custom Log Handler';
end;

procedure TCustomLogHandler.ProcessLogMessage(ALogMessage: TLazyLogMessage); 
var
    LFormattedMessage : string;
begin
    LFormattedMessage := GetFormattedMessage(ALogMessage, true);
    case ALogMessage.LogLevel of
        logError:
            begin
                // Pass off to my usual log hander
            end;
        logWarning:
            begin
                // Pass off to my usual log hander
            end;
        logInformation:
            begin
               // Pass off to my usual log hander
            end;
        logDebug:
            begin
              // Pass off to my usual log hander
            end;
    end;
end;


initialization


end.
