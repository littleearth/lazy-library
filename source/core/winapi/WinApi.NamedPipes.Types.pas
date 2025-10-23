unit WinApi.NamedPipes.Types;

interface

uses
  Classes, Windows;

const
  cShutDownMsg = 'TERMINATE ';
  cPipeFormat = '\\%s\pipe\%s';

type
  RPIPEMessage = record
    Size: DWORD;
    Kind: Byte;
    Count: DWORD;
    //Data: array[0..8095] of Char;
    Message: array[0..MAXWORD-1] of Char;
  end;

procedure CalcMsgSize(var Msg: RPIPEMessage);

implementation

procedure CalcMsgSize(var Msg: RPIPEMessage);
begin
  Msg.Size :=
    SizeOf(Msg.Size) +
    SizeOf(Msg.Kind) +
    SizeOf(Msg.Count) +
    Msg.Count +
    3;
end;

end.

