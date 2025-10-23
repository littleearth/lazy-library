{ -----------------------------------------------------------------------------
  Unit Name: Lazy.ThreadedFileStream
  Author: Tristan Marlow
  Purpose: Thread safe file stream

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

  ThreadedFileStream taken from the delphi-iocp-framework

  ----------------------------------------------------------------------------; }
unit Lazy.ThreadedFileStream;

interface

uses
  System.SysUtils, System.Variants,
  System.Classes, System.SyncObjs;

type
  TLZThreadedFileStream = class(TThread)
  private
    FFileStream: TFileStream;
    FFileTime: TDateTime;
    FLocker: TCriticalSection;
    FCacheBuffer, FCacheBufferA, FCacheBufferB: TMemoryStream;
    FFlushInterval: Cardinal;
    function CalcTickDiff(const StartTick, EndTick: LongWord): LongWord;
  protected
    procedure Lock;
    procedure Unlock;

    procedure Execute; override;
  public
    constructor Create(const AFileName: string); reintroduce;
    destructor Destroy; override;

    function Write(
      const Buffer;
      Count: Longint): Longint;
    procedure AppendStr(const S: RawByteString); overload;
    procedure AppendStr(const S: UTF8String); overload;
    procedure AppendStr(const S: UnicodeString); overload;
    procedure Flush;
    procedure Clear;

    property FileTime: TDateTime read FFileTime;
    property FlushInterval: Cardinal read FFlushInterval write FFlushInterval;
  end;

implementation

procedure TLZThreadedFileStream.AppendStr(const S: RawByteString);
begin
  Write(S[1], Length(S));
end;

procedure TLZThreadedFileStream.AppendStr(const S: UTF8String);
begin
  AppendStr(RawByteString(S));
end;

procedure TLZThreadedFileStream.AppendStr(const S: UnicodeString);
begin
  AppendStr(UTF8Encode(S));
end;

procedure TLZThreadedFileStream.Clear;
begin
  Lock;
  try
    FCacheBuffer.Clear;
    FFileStream.Size := 0;
  finally
    Unlock;
  end;
end;

constructor TLZThreadedFileStream.Create(const AFileName: string);
var
  UTF8Header: RawByteString;
begin
  if FileExists(AFileName) then
  begin
    FFileStream := TFileStream.Create(AFileName, fmOpenReadWrite or
      fmShareDenyWrite);
    UTF8Header := '';
  end
  else
  begin
    FFileStream := TFileStream.Create(AFileName, fmCreate);
    FFileStream.Free;
    FFileStream := TFileStream.Create(AFileName, fmOpenReadWrite or
      fmShareDenyWrite);
    UTF8Header := RawByteString(#$EF#$BB#$BF);
  end;

  inherited Create(true);

  FLocker := TCriticalSection.Create;
  FCacheBufferA := TMemoryStream.Create;
  FCacheBufferB := TMemoryStream.Create;
  FCacheBuffer := FCacheBufferA;
  FFileTime := Now;
  FFlushInterval := 1000;

  if (UTF8Header <> '') then
    AppendStr(UTF8Header);

  Suspended := false;
end;

destructor TLZThreadedFileStream.Destroy;
begin
  Lock;
  try
    FCacheBuffer := nil;

    if Assigned(FCacheBufferA) then
      FreeAndNil(FCacheBufferA);
    if Assigned(FCacheBufferB) then
      FreeAndNil(FCacheBufferB);
    if Assigned(FFileStream) then
      FreeAndNil(FFileStream);
  finally
    Unlock;
  end;
  if Assigned(FLocker) then
    FreeAndNil(FLocker);

  inherited Destroy;
end;

function TLZThreadedFileStream.CalcTickDiff(const StartTick, EndTick: LongWord)
  : LongWord;
begin
  if EndTick >= StartTick then
    Result := EndTick - StartTick
  else
    Result := High(LongWord) - StartTick + EndTick;
end;

procedure TLZThreadedFileStream.Execute;
var
  t: Cardinal;
begin
  t := GetTickCount;

  while not Terminated do
  begin
    if (CalcTickDiff(t, GetTickCount) >= FFlushInterval) then
    begin
      Flush;
      t := GetTickCount;
    end
    else
      Sleep(1000);
    // SleepEx(100, True);
  end;

  Flush;
end;

procedure TLZThreadedFileStream.Flush;
var
  Buffer: TMemoryStream;
begin
  if (FCacheBuffer = nil) or (FCacheBuffer.Position <= 0) then
    Exit;

  Lock;
  Buffer := FCacheBuffer;
  if (FCacheBuffer = FCacheBufferA) then
    FCacheBuffer := FCacheBufferB
  else
    FCacheBuffer := FCacheBufferA;
  FCacheBuffer.Position := 0;
  Unlock;

  FFileStream.Seek(0, soEnd);
  FFileStream.Write(Buffer.Memory^, Buffer.Position);
  // FlushFileBuffers(FFileStream.Handle);
  Buffer.Position := 0;
end;

procedure TLZThreadedFileStream.Lock;
begin
  FLocker.Enter;
end;

procedure TLZThreadedFileStream.Unlock;
begin
  FLocker.Leave;
end;

function TLZThreadedFileStream.Write(
  const Buffer;
  Count: integer): Longint;
begin
  Lock;
  try
    Result := FCacheBuffer.Write(Buffer, Count);
  finally
    Unlock;
  end;
end;

end.
