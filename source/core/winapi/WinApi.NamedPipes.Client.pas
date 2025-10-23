unit WinApi.NamedPipes.Client;

interface

uses
  Lazy.Types, WinApi.NamedPipes.Types,
  Classes, Windows;

type
  TLZNamedPipeClient = class(TLZObject)
  private
    FPipeName: String;
    function ProcessMsg(aMsg: RPIPEMessage): RPIPEMessage;
  protected
  public
    constructor Create(aServer, aPipe: String);
    function SendMessage(AMessage: String): String; overload;
    function SendMessage(AKind : Byte; AMessage : String) : String ; overload;
  end;

implementation

uses
  SysUtils;

{ TLZNamedPipeClient }

constructor TLZNamedPipeClient.Create(aServer, aPipe: String);
begin
  inherited Create;
  if aServer = '' then
    FPipeName := Format(cPipeFormat, ['.', aPipe])
  else
    FPipeName := Format(cPipeFormat, [aServer, aPipe]);
end;

function TLZNamedPipeClient.ProcessMsg(aMsg: RPIPEMessage): RPIPEMessage;
begin
  CalcMsgSize(aMsg);
  Result.Size := SizeOf(Result);
  if WaitNamedPipe(PChar(FPipeName), 10) then
    if not CallNamedPipe(
      PChar(FPipeName), @aMsg, aMsg.Size, @Result, Result.Size, Result.Size, 500
    ) then
      raise Exception.Create('PIPE did not respond.')
    else
  else
    raise Exception.Create('PIPE does not exist.');
end;

function TLZNamedPipeClient.SendMessage(AKind : Byte; AMessage : String) : String;
var
  Msg: RPIPEMessage;
begin
  // prepare outgoing message
  Msg.Kind := AKind;
  Msg.Count := Length(AMessage);
  StrPCopy(Msg.Message, AMessage);
  // send message
  Msg := ProcessMsg(Msg);
  // return data send from server
  Result := Copy(Msg.Message, 1, Msg.Count);
end;

function TLZNamedPipeClient.SendMessage(AMessage : String) : String;
begin
  Result := SendMessage(1,AMessage);
end;

end.

