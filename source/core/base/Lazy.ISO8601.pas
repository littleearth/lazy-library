unit Lazy.Iso8601;

interface

uses
  Classes;

type
  TLZUtc = class(TObject)
  public
    class function FromUtc(const Value: TDateTime): TDateTime; static;
    class function ToUtc(const Value: TDateTime): TDateTime; static;
    class function UtcNow: TDateTime; static;
  end;

  TLZToIso8601 = class(TLZUtc)
  public
    class function DateTimeToIso8601(
      const Value: TDateTime;
      AReturnUTC: boolean = false): string; static;
    class function DateToIso8601(const Value: TDate): string; static;
    class function TimeToIso8601(const Value: TTime): string; static;
    class function UtcTimeToIso8601(const Value: TTime): string; static;
  end;

  TLZIso8601 = class(TLZToIso8601)
  public
    class function DateFromIso8601(const Value: string): TDate; static;
    class function DateTimeFromIso8601(
      const Value: string;
      AReturnUTC: boolean = false): TDateTime; static;
    class function TimeFromIso8601(const Value: string): TTime; static;
    class function UtcDateTimeToIso8601(const Value: TDateTime): string; static;
  end;

implementation

uses
  XSBuiltIns, System.DateUtils, SysUtils, IdGlobal, IdGlobalProtocols;

class function TLZIso8601.DateFromIso8601(const Value: string): TDate;
begin
  with TXSDate.Create() do
    try
      try
        XSToNative(Value); // convert from WideString
        Result := AsDate; // convert to TDate
      except
        Result := 0;
      end;
    finally
      Free();
    end;
end;

class function TLZIso8601.DateTimeFromIso8601(
  const Value: string;
  AReturnUTC: boolean): TDateTime;
begin
  try
    Result := System.DateUtils.ISO8601ToDate(Value, AReturnUTC);
  except
    with TXSDateTime.Create() do
      try
        try
          XSToNative(Value);
          Result := AsDateTime;
          if AReturnUTC then
            Result := ToUtc(Result);
        except
          Result := 0;
        end;
      finally
        Free();
      end;
  end;
end;

class function TLZIso8601.TimeFromIso8601(const Value: string): TTime;
begin
  with TXSTime.Create() do
    try
      try
        XSToNative(Value);
        Result := AsTime;
      except
        Result := 0;
      end;
    finally
      Free();
    end;
end;

class function TLZIso8601.UtcDateTimeToIso8601(const Value: TDateTime): string;
begin
  with TXSDateTime.Create() do
    try
      try
        AsUTCDateTime := Value;
        Result := NativeToXS;
      except
        Result := '';
      end;
    finally
      Free();
    end;
end;

class function TLZUtc.FromUtc(const Value: TDateTime): TDateTime;
begin
  // Result := Value - TimeZoneBias;
  Result := UTCTimeToLocalTime(Value);
end;

class function TLZUtc.ToUtc(const Value: TDateTime): TDateTime;
begin
  // Result := Value + TimeZoneBias;
  Result := LocalTimeToUTCTime(Value);
end;

class function TLZUtc.UtcNow: TDateTime;
begin
  Result := ToUtc(Now);
end;

class function TLZToIso8601.DateTimeToIso8601(
  const Value: TDateTime;
  AReturnUTC: boolean): string;
begin
  try
    Result := System.DateUtils.DateToIso8601(Value, AReturnUTC);
  except
    with TXSDateTime.Create() do
      try
        try
          if AReturnUTC then
          begin
            AsDateTime := ToUtc(Value);
          end
          else
          begin
            AsDateTime := Value;
          end;
          Result := NativeToXS;
        except
          Result := '';
        end;
      finally
        Free();
      end;
  end;
end;

class function TLZToIso8601.DateToIso8601(const Value: TDate): string;
begin
  with TXSDate.Create() do
    try
      try
        AsDate := Value;
        Result := NativeToXS;
      except
        Result := '';
      end;
    finally
      Free();
    end;
end;

class function TLZToIso8601.TimeToIso8601(const Value: TTime): string;
begin
  with TXSTime.Create() do
    try
      try
        AsTime := Value;
        Result := NativeToXS;
      except
        Result := '';
      end;
    finally
      Free();
    end;
end;

class function TLZToIso8601.UtcTimeToIso8601(const Value: TTime): string;
begin
  with TXSTime.Create() do
    try
      try
        AsTime := ToUtc(Value);
        Result := NativeToXS;
      except
        Result := '';
      end;
    finally
      Free();
    end;
end;

end.
