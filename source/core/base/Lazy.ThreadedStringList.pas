{ -----------------------------------------------------------------------------
  Unit Name: Lazy.ThreadedFileStream
  Author: Tristan Marlow
  Purpose: Thread safe string list

  ----------------------------------------------------------------------------
  Copyright (c) 2023 Tristan David Marlow
  Copyright (c) 2023 Little Earth Solutions

----------------------------------------------------------------------------; }
unit Lazy.ThreadedStringList;


interface

uses
  Windows, Messages,
  System.SyncObjs, System.SysUtils, System.Variants, System.Classes;

type
  TLZThreadStringList = class
  private
    FStringList: TStringList;
    FLock: TCriticalSection;
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(dup: TDuplicates);
    function GetCapacity: integer;
    procedure SetCapacity(capa: integer);
    function GetCommaText: string;
    procedure SetCommaText(const S: string);
    function GetCount: integer;
    function GetDelimiter: Char;
    procedure SetDelimiter(delim: Char);
    function GetDelimitedText: string;
    procedure SetDelimitedText(const S: string);
    function GetNames(Index: integer): string;
    function GetValues(const Name: string): string;
    procedure SetValues(const Name: string; S: string);
    function GetStrings(Index: integer): string;
    procedure SetStrings(Index: integer; S: string);
    function GetAsText: string;
    procedure SetAsText(S: string);
    function GetSorted: boolean;
    procedure SetSorted(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function LockList: boolean;
    procedure UnlockList;
    function Add(const S: string): integer;
    procedure AddStrings(Strings: TStrings);
    procedure Delete(Index: integer);
    procedure Clear;
    procedure Exchange(Index1, Index2: integer);
    function Find(const S: string; var Index: integer): boolean;
    procedure Insert(Index: integer; const S: string);
    function IndexOf(const S: string): integer;
    function IndexOfName(const Name: string): integer;
    procedure Sort;
    function GetText: PChar;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Capacity: integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: integer read GetCount;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Names[Index: integer]: string read GetNames;
    property Values[const Name: string]: string read GetValues write SetValues;
    property Strings[Index: integer]: string read GetStrings
      write SetStrings; default;
    property Text: string read GetAsText write SetAsText;
    property Sorted: boolean read GetSorted write SetSorted;
  end;

implementation


constructor TLZThreadStringList.Create;
begin
  FLock := TCriticalSection.Create;
  FStringList := TStringList.Create;
  FStringList.Sorted := True;
  FStringList.Duplicates := dupIgnore;
end;

destructor TLZThreadStringList.Destroy;
begin
  if LockList then
  begin
    try
      FStringList.Free;
    finally
      UnlockList;
    end;
  end;

  try
    FreeAndNil(FLock);
  finally
    inherited Destroy;
  end;

end;

function TLZThreadStringList.LockList: boolean;
begin
  Result := false;
  if Assigned(FLock) and Assigned(FStringList) then
  begin
    try
      FLock.Acquire;
      Result := True;
    except

    end;
  end;
end;

procedure TLZThreadStringList.UnlockList;
begin
  if Assigned(FLock) then
  begin
    FLock.Release;
  end;
end;

function TLZThreadStringList.Add(const S: string): integer;
begin
  Result := -1;
  if not LockList then
    Exit;
  try
    Result := FStringList.Add(S);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.AddStrings(Strings: TStrings);
begin
  if not LockList then
    Exit;
  try
    FStringList.AddStrings(Strings);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.Delete(Index: integer);
begin
  if not LockList then
    Exit;
  try
    FStringList.Delete(Index);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.Clear;
begin
  if not LockList then
    Exit;
  try
    FStringList.Clear;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.Exchange(Index1, Index2: integer);
begin
  if not LockList then
    Exit;
  try
    FStringList.Exchange(Index1, Index2);
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.Find(const S: string; var Index: integer): boolean;
begin
  Result := false;
  if not LockList then
    Exit;
  try
    Result := FStringList.Find(S, Index);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.Insert(Index: integer; const S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Insert(Index, S);
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.IndexOf(const S: string): integer;
begin
  Result := -1;
  if not LockList then
    Exit;
  LockList;
  try
    Result := FStringList.IndexOf(S);
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.IndexOfName(const Name: string): integer;
begin
  Result := -1;
  if not LockList then
    Exit;
  try
    Result := FStringList.IndexOfName(Name);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.Sort;
begin
  if not LockList then
    Exit;
  try
    FStringList.Sort;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetText: PChar;
begin
  Result := nil;
  if not LockList then
    Exit;
  try
    Result := FStringList.GetText;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.LoadFromFile(const FileName: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.LoadFromFile(FileName);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.LoadFromStream(Stream: TStream);
begin
  if not LockList then
    Exit;
  try
    FStringList.LoadFromStream(Stream);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SaveToFile(const FileName: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.SaveToFile(FileName);
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SaveToStream(Stream: TStream);
begin
  if not LockList then
    Exit;
  try
    FStringList.SaveToStream(Stream);
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetDuplicates: TDuplicates;
begin
  Result := dupIgnore;
  if not LockList then
    Exit;
  try
    Result := FStringList.Duplicates;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetDuplicates(dup: TDuplicates);
begin
  if not LockList then
    Exit;
  try
    FStringList.Duplicates := dup;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetCapacity: integer;
begin
  Result := 0;
  if not LockList then
    Exit;
  try
    Result := FStringList.Capacity;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetCapacity(capa: integer);
begin
  if not LockList then
    Exit;
  try
    FStringList.Capacity := capa;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetCommaText: string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.CommaText;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetCommaText(const S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.CommaText := S;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetCount: integer;
begin
  Result := 0;
  if not LockList then
    Exit;
  try
    Result := FStringList.Count;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetDelimiter: Char;
begin
  Result := ',';
  if not LockList then
    Exit;
  try
    Result := FStringList.Delimiter;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetDelimiter(delim: Char);
begin
  if not LockList then
    Exit;
  try
    FStringList.Delimiter := delim;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetDelimitedText: string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.DelimitedText;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetDelimitedText(const S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.DelimitedText := S;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetNames(Index: integer): string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Names[Index];
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetValues(const Name: string): string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Values[Name];
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetValues(const Name: string; S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Values[Name] := S;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetSorted: boolean;
begin
  Result := false;
  if not LockList then
    Exit;
  try
    Result := FStringList.Sorted;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetStrings(Index: integer): string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Strings[Index];
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetSorted(const Value: boolean);
begin
  if not LockList then
    Exit;
  try
    if Value <> FStringList.Sorted then
    begin
      FStringList.Sorted := Value;
    end;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetStrings(Index: integer; S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Strings[Index] := S;
  finally
    UnlockList;
  end;
end;

function TLZThreadStringList.GetAsText: string;
begin
  if not LockList then
    Exit;
  try
    Result := FStringList.Text;
  finally
    UnlockList;
  end;
end;

procedure TLZThreadStringList.SetAsText(S: string);
begin
  if not LockList then
    Exit;
  try
    FStringList.Text := S;
  finally
    UnlockList;
  end;
end;

end.
