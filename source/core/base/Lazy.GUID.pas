unit Lazy.GUID;

interface

uses System.SysUtils;

type
  TLazyGUID = class
    // Creates and returns a new globally unique identifier
    class function NewGuid: TGuid;
    // sometimes we need to have an "empty" value, like NULL
    class function EmptyGuid: TGuid;
    // Checks whether a Guid is EmptyGuid
    class function IsEmptyGuid(GUID: TGuid): boolean;
    // Convert to string
    class function ToString(GUID: TGuid): string; reintroduce;
    // convert to quoted string
    class function ToQuotedString(GUID: TGuid): string;
    // return a GUID from string
    class function FromString(Value: string): TGuid;
    // Indicates whether two TGUID values are the same
    class function EqualGuids(Guid1, Guid2: TGuid): boolean;
  end;

implementation

{ TLazyGUID }

class function TLazyGUID.EmptyGuid: TGuid;
begin
  result := FromString('{00000000-0000-0000-0000-000000000000}');
end;

class function TLazyGUID.EqualGuids(Guid1, Guid2: TGuid): boolean;
begin
  result := IsEqualGUID(Guid1, Guid2);
end;

class function TLazyGUID.FromString(Value: string): TGuid;
begin
  result := StringToGuid(Value);
end;

class function TLazyGUID.IsEmptyGuid(GUID: TGuid): boolean;
begin
  result := EqualGuids(GUID, EmptyGuid);
end;

class function TLazyGUID.NewGuid: TGuid;
var
  GUID: TGuid;
begin
  CreateGUID(GUID);
  result := GUID;
end;

class function TLazyGUID.ToQuotedString(GUID: TGuid): string;
begin
  result := QuotedStr(ToString(GUID));
end;

class function TLazyGUID.ToString(GUID: TGuid): string;
begin
  result := GuidToString(GUID);
end;

end.
