unit Lazy.Iso8601;

interface

uses
  Classes;

type
  TUtc = class(TObject)
  public
    class function FromUtc(const Value: TDateTime): TDateTime; static;
    class function ToUtc(const Value: TDateTime): TDateTime; static;
    class function UtcNow: TDateTime; static;
  end;

  TToIso8601 = class(TUtc)
  public
    class function DateTimeToIso8601(const Value: TDateTime;
      AReturnUTC: boolean = false): string; static;
    class function DateToIso8601(const Value: TDate): string; static;
    class function TimeToIso8601(const Value: TTime): string; static;
    class function UtcTimeToIso8601(const Value: TTime): string; static;
  end;

  TIso8601 = class(TToIso8601)
  public
    class function DateFromIso8601(const Value: string): TDate; static;
    class function DateTimeFromIso8601(const Value: string;
      AReturnUTC: boolean = false): TDateTime; static;
    class function TimeFromIso8601(const Value: string): TTime; static;
    class function UtcDateTimeToIso8601(const Value: TDateTime): string; static;
  end;

implementation

uses
  XSBuiltIns, System.DateUtils, SysUtils, IdGlobalProtocols;

class function TIso8601.DateFromIso8601(const Value: string): TDate;
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

class function TIso8601.DateTimeFromIso8601(const Value: string;
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

class function TIso8601.TimeFromIso8601(const Value: string): TTime;
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

class function TIso8601.UtcDateTimeToIso8601(const Value: TDateTime): string;
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

class function TUtc.FromUtc(const Value: TDateTime): TDateTime;
begin
  Result := Value - TimeZoneBias;
end;

class function TUtc.ToUtc(const Value: TDateTime): TDateTime;
begin
  Result := Value + TimeZoneBias;
end;

class function TUtc.UtcNow: TDateTime;
begin
  Result := ToUtc(Now);
end;

class function TToIso8601.DateTimeToIso8601(const Value: TDateTime;
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

class function TToIso8601.DateToIso8601(const Value: TDate): string;
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

class function TToIso8601.TimeToIso8601(const Value: TTime): string;
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

class function TToIso8601.UtcTimeToIso8601(const Value: TTime): string;
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
