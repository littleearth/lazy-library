unit Lazy.SNMP.Client;

interface

uses
  Lazy.Types, Lazy.Utils,
  System.SysUtils, System.Variants,
  System.Classes, IdSNMP;

type
  TLZSNMPClient = class(TLZObject)
  private
    FCommunity: string;
    FHostname: string;
    FReceiveTimeout: integer;
    FMaxRetries: integer;
    procedure SetCommunity(const Value: string);
    procedure SetHostname(const Value: string);
    procedure SetReceiveTimeout(const Value: integer);
    procedure SetMaxRetries(const Value: integer);
  protected
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function GetMIBValue(
      AMIB: string;
      ADefaultValue: string = ''): string;
    function GetMIBValues(
      AMIB: string;
      AStrings: TStrings;
      AIncludeMIB: Boolean = false;
      AOnProgress: TOnProgressRef = nil): Boolean; overload;
    function GetMIBValues(
      AMIB: string;
      ADefaultValue: string = '';
      AIndex: integer = -1;
      AOnProgress: TOnProgressRef = nil): string; overload;
    property Hostname: string read FHostname write SetHostname;
    property Community: string read FCommunity write SetCommunity;
    property ReceiveTimeout: integer read FReceiveTimeout
      write SetReceiveTimeout;
    property MaxRetries: integer read FMaxRetries write SetMaxRetries;
  end;

  TSNMPQuery = class(TLZSNMPClient);

implementation

// TSNMPQuery

uses
  IdGlobal, System.Math;

{ TSNMPQueryBase }

function TLZSNMPClient.GetMIBValue(
  AMIB: string;
  ADefaultValue: string): string;
var
  IdSNMP: TIdSNMP;
  Success: Boolean;
  CheckCount: integer;
  LValueIdx: integer;
  LMaxCheckCount: integer;
begin
  Result := ADefaultValue;
  Success := false;
  CheckCount := 1;
  LMaxCheckCount := FMaxRetries;

  IdSNMP := TIdSNMP.Create(nil);
  try
    IdSNMP.Community := Community;
    IdSNMP.Host := Hostname;
    IdSNMP.TrapPort := 0;
    IdSNMP.ReceiveTimeout := FReceiveTimeout;
    Repeat
      try
        IdSNMP.Query.Clear;
        IdSNMP.Query.MIBAdd(AMIB, '');
        IdSNMP.Query.PDUType := PDUGetRequest;
        try
          if IdSNMP.SendQuery then
          begin
            Result := '';
            for LValueIdx := 0 to PRed(IdSNMP.Reply.ValueCount) do
            begin
              Result := Trim(Result + ' ' +
                (TLZString.StringCleaner(IdSNMP.Reply.Value[LValueIdx],
                true, true)));
            end;
            Success := true;
          end;
        except
          on E: Exception do
          begin
            Error(E);
          end;
        end;

      finally
        Inc(CheckCount);
      end;
    Until (Success) or (CheckCount > LMaxCheckCount);
  finally
    FreeAndNil(IdSNMP);
  end;
end;

function TLZSNMPClient.GetMIBValues(
  AMIB, ADefaultValue: string;
  AIndex: integer;
  AOnProgress: TOnProgressRef): string;
var
  LValues: TStringList;
begin
  Result := '';
  LValues := TStringList.Create;
  try
    GetMIBValues(AMIB, LValues, false, AOnProgress);
    if AIndex = -1 then
    begin
      Result := LValues.Text;
    end
    else
    begin
      if InRange(0, LValues.Count, AIndex) then
      begin
        Result := LValues[AIndex];
      end;
    end;
  finally
    FreeAndNil(LValues);
  end;
end;

function TLZSNMPClient.GetMIBValues(
  AMIB: string;
  AStrings: TStrings;
  AIncludeMIB: Boolean;
  AOnProgress: TOnProgressRef): Boolean;
var
  IdSNMP: TIdSNMP;
  LOID, LBaseOID: string;
  LSuccess: Boolean;
  LCancel: Boolean;
  LRetryCount: integer;
  LValueIdx: integer;
  LValue: string;
  LMaxRetryCount: integer;
begin
  Result := false;
  if Assigned(AStrings) then
  begin
    LMaxRetryCount := FMaxRetries;
    IdSNMP := TIdSNMP.Create(nil);
    try
      AStrings.Clear;
      IdSNMP.Community := Community;
      IdSNMP.Host := Hostname;
      IdSNMP.TrapPort := 0;
      IdSNMP.ReceiveTimeout := FReceiveTimeout;

      LOID := AMIB;
      if Copy(LOID, Length(LOID) - 1, 2) = '.0' then
        LOID := Copy(LOID, 1, Length(LOID) - 2);
      LBaseOID := LOID;
      IdSNMP.Query.Clear;
      IdSNMP.Query.MIBAdd(LOID, '');
      IdSNMP.Query.PDUType := PDUGetNextRequest;

      LSuccess := true;
      LCancel := false;

      while (LSuccess) and (not LCancel) do
      begin
        LRetryCount := 1;
        Repeat
          if Assigned(AOnProgress) then
          begin
            AOnProgress(Self, 50, Format('Scanning %s, %s', [Hostname, LOID]
              ), LCancel);
          end;
          try
            LSuccess := IdSNMP.SendQuery;
          except
            on E: Exception do
            begin
              Error(E);
            end;
          end;
          Inc(LRetryCount);
        Until (LSuccess) or (LRetryCount > LMaxRetryCount);

        if LSuccess then
        begin
          if (IdSNMP.Reply.MIBOID.Count > 0) and
            (Copy(IdSNMP.Reply.MIBOID[0], 1, Length(LBaseOID)) <> LBaseOID) then
            break;

          for LValueIdx := 0 to PRed(IdSNMP.Reply.ValueCount) do
          begin
            LValue := TLZString.StringCleaner(IdSNMP.Reply.Value[LValueIdx],
              true, true);
          end;

          if AIncludeMIB then
          begin
            if IdSNMP.Reply.MIBOID.Count > 0 then
            begin
              AStrings.AddPair(IdSNMP.Reply.MIBOID[0], LValue);
            end;
          end
          else
          begin
            AStrings.Add(LValue);
          end;

          Result := true;

          if IdSNMP.Reply.ValueCount > 0 then
          begin
            LOID := IdSNMP.Reply.ValueOID[0];
            IdSNMP.Query.Clear;
            IdSNMP.Query.MIBAdd(LOID, '');
            IdSNMP.Query.PDUType := PDUGetNextRequest;
          end
          else
          begin
            LSuccess := false;
          end;
        end;

      end;
    finally
      FreeAndNil(IdSNMP);
    end;
  end;
end;

constructor TLZSNMPClient.Create;
begin
  inherited;
  FReceiveTimeout := 15000;
  FMaxRetries := 2;
end;

destructor TLZSNMPClient.Destroy;
begin
  try
  finally
    inherited;
  end;
end;

procedure TLZSNMPClient.SetCommunity(const Value: string);
begin
  if not SameText(FCommunity, Value) then
  begin
    FCommunity := Value;
  end;
end;

procedure TLZSNMPClient.SetHostname(const Value: string);
begin
  FHostname := Value;
end;

procedure TLZSNMPClient.SetMaxRetries(const Value: integer);
begin
  FMaxRetries := Value;
end;

procedure TLZSNMPClient.SetReceiveTimeout(const Value: integer);
begin
  FReceiveTimeout := Value;
end;

end.
