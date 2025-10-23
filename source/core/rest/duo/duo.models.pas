unit DUO.Models;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  System.JSON, DUO.api, System.Generics.Collections;

type
  TLZDUODevice = class(TLZDuoModel)
  private
    FDisplayName: string;
    FDeviceType: string;
    FDevice: string;
    FDeviceName: string;
    FNumber: string;
    FSMSNextCode: string;
    FCapabilities: TStringList;
    procedure SetDevice(const Value: string);
    procedure SetDeviceName(const Value: string);
    procedure SetDeviceType(const Value: string);
    procedure SetDisplayName(const Value: string);
    procedure SetNumber(const Value: string);
    procedure SetSMSNextCode(const Value: string);
    function GetCapabilities: TStrings;
    procedure SetCapabilities(const Value: TStrings);
  protected
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    constructor CreateEx(
      ADevice: string;
      ADisplayName: string;
      ADeviceName: string;
      ADeviceType: string;
      ANumber: string;
      ASMSNextCode: string;
      ACapabilities: TStrings = nil); overload;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property Device: string read FDevice write SetDevice;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property DeviceName: string read FDeviceName write SetDeviceName;
    property DeviceType: string read FDeviceType write SetDeviceType;
    property Number: string read FNumber write SetNumber;
    property SMSNextCode: string read FSMSNextCode write SetSMSNextCode;
    property Capabilities: TStrings read GetCapabilities write SetCapabilities;
  end;

  TLZDUODevices = class(TLZDuoModelList<TLZDUODevice>)
  protected
  public
    function Add(
      ADevice, ADisplayName, ADeviceName, ADeviceType, ANumber,
      ASMSNextCode: string;
      ACapabilities: TStrings;
      AUpdateExisting: boolean = true): TLZDUODevice; reintroduce; overload;
    function Find(ADevice: string): TLZDUODevice;
    function FindByNumber(ANumber: string): TLZDUODevice;
  end;

  TLZDUOPhone = class(TLZDuoModel)
  private
    FPhoneType: string;
    FTampered: string;
    FScreenLock: string;
    FLastSeen: TDateTIme;
    FModel: string;
    FSMSPasscodesSent: boolean;
    FPostDelay: string;
    FExtension: string;
    FActivated: boolean;
    FPhoneName: string;
    FPlatform: string;
    FNumber: string;
    FFingerprint: string;
    FEncrypted: string;
    FPreDelay: string;
    FPhoneID: string;
    FCapabilities: TStringList;
    function GetCapabilities: TStrings;
    procedure SetActivated(const Value: boolean);
    procedure SetEncrypted(const Value: string);
    procedure SetExtension(const Value: string);
    procedure SetFingerprint(const Value: string);
    procedure SetLastSeen(const Value: TDateTIme);
    procedure SetModel(const Value: string);
    procedure SetNumber(const Value: string);
    procedure SetPhoneID(const Value: string);
    procedure SetPhoneName(const Value: string);
    procedure SetPhoneType(const Value: string);
    procedure SetPlatform(const Value: string);
    procedure SetPostDelay(const Value: string);
    procedure SetPreDelay(const Value: string);
    procedure SetScreenLock(const Value: string);
    procedure SetSMSPasscodesSent(const Value: boolean);
    procedure SetTampered(const Value: string);
    procedure SetCapabilities(const Value: TStrings);
  protected
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property Activated: boolean read FActivated write SetActivated;
    property Capabilities: TStrings read GetCapabilities write SetCapabilities;
    property Encrypted: string read FEncrypted write SetEncrypted;
    property Extension: string read FExtension write SetExtension;
    property Fingerprint: string read FFingerprint write SetFingerprint;
    property LastSeen: TDateTIme read FLastSeen write SetLastSeen;
    property Model: string read FModel write SetModel;
    property PhoneName: string read FPhoneName write SetPhoneName;
    property Number: string read FNumber write SetNumber;
    property PhoneID: string read FPhoneID write SetPhoneID;
    property Platform: string read FPlatform write SetPlatform;
    property PostDelay: string read FPostDelay write SetPostDelay;
    property PreDelay: string read FPreDelay write SetPreDelay;
    property ScreenLock: string read FScreenLock write SetScreenLock;
    property SMSPasscodesSent: boolean read FSMSPasscodesSent
      write SetSMSPasscodesSent;
    property Tampered: string read FTampered write SetTampered;
    property PhoneType: string read FPhoneType write SetPhoneType;
  end;

  TLZDUOPhones = class(TLZDuoModelList<TLZDUOPhone>)
  public
    function Find(APhoneID: string): TLZDUOPhone;
  end;

  TLZDUOUser = class(TLZDuoModel)
  private
    FCreatedTimeStamp: TDateTIme;
    FLastName: string;
    FEmail: string;
    FNotes: string;
    FUserID: string;
    FIsEnrolled: boolean;
    FRealName: string;
    FAliases: TStringList;
    FPhones: TLZDUOPhones;
    FFirstName: string;
    FUsername: string;
    FLastLogin: TDateTIme;
    FLastDirectorySync: TDateTIme;
    FStatus: string;
    procedure SetCreatedTimeStamp(const Value: TDateTIme);
    procedure SetEmail(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetIsEnrolled(const Value: boolean);
    procedure SetLastDirectorySync(const Value: TDateTIme);
    procedure SetLastLogin(const Value: TDateTIme);
    procedure SetLastName(const Value: string);
    procedure SetNotes(const Value: string);
    procedure SetRealName(const Value: string);
    procedure SetUserID(const Value: string);
    procedure SetUsername(const Value: string);
    function GetAliases: TStrings;
    procedure SetStatus(const Value: string);
    function GetPhones: TLZDUOPhones;
  protected
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    function HasAlias(AAlias: string): boolean;
    function HasPhone(APhoneID: string): boolean;
    property UserID: string read FUserID write SetUserID;
    property Username: string read FUsername write SetUsername;
    property Aliases: TStrings read GetAliases;
    property Phones: TLZDUOPhones read GetPhones;
    property CreatedTimeStamp: TDateTIme read FCreatedTimeStamp
      write SetCreatedTimeStamp;
    property Email: string read FEmail write SetEmail;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property RealName: string read FRealName write SetRealName;
    property IsEnrolled: boolean read FIsEnrolled write SetIsEnrolled;
    property LastDirectorySync: TDateTIme read FLastDirectorySync
      write SetLastDirectorySync;
    property LastLogin: TDateTIme read FLastLogin write SetLastLogin;
    property Notes: string read FNotes write SetNotes;
    property Status: string read FStatus write SetStatus;
  end;

  TLZDUOUsers = class(TLZDuoModelList<TLZDUOUser>)
  public
    function Find(AUsername: string): TLZDUOUser;
    function FindUserID(AUserID: string): TLZDUOUser;
    function FindAlias(AAlias: string): TLZDUOUser;
  end;

  TLZDUOAccount = class(TLZDuoModel)
  private
    FAPIHostname: string;
    FAccountID: string;
    FAccountName: string;
    FBillingEdition: string;
    FTeleponyCredits: integer;
    procedure SetAccountID(const Value: string);
    procedure SetAccountName(const Value: string);
    procedure SetAPIHostname(const Value: string);
    procedure SetBillingEdition(const Value: string);
    procedure SetTeleponyCredits(const Value: integer);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property AccountID: string read FAccountID write SetAccountID;
    property AccountName: string read FAccountName write SetAccountName;
    property APIHostname: string read FAPIHostname write SetAPIHostname;
    property BillingEdition: string read FBillingEdition
      write SetBillingEdition;
    property TeleponyCredits: integer read FTeleponyCredits
      write SetTeleponyCredits;
  end;

  TLZDUOAccounts = class(TLZDuoModelList<TLZDUOAccount>);

implementation

uses
  Lazy.Utils;

{ TDUODevice }

procedure TLZDUODevice.AfterModelCreated;
begin
  inherited;
  FCapabilities := TStringList.Create;
end;

procedure TLZDUODevice.BeforeModelDestroyed;
begin
  FreeAndNil(FCapabilities);
  inherited;
end;

constructor TLZDUODevice.CreateEx(
  ADevice, ADisplayName, ADeviceName, ADeviceType, ANumber,
  ASMSNextCode: string;
  ACapabilities: TStrings);
begin
  inherited Create;
  FDevice := ADevice;
  FDisplayName := ADisplayName;
  FDeviceName := ADeviceName;
  FDeviceType := ADeviceType;
  FNumber := ANumber;
  FSMSNextCode := ASMSNextCode;
  if Assigned(ACapabilities) then
  begin
    FCapabilities.Assign(ACapabilities);
  end;
end;

procedure TLZDUODevice.FromJSONValue(AJSONValue: TJSONValue);
var
  LCapabilitiesArray: TJSONArray;
  LCapabilityIdx: integer;
  LCapability: string;
begin
  FCapabilities.Clear;
  LCapabilitiesArray := AJSONValue.GetValue<TJSONArray>('capabilities');
  for LCapabilityIdx := 0 to Pred(LCapabilitiesArray.Count) do
  begin
    LCapability := LCapabilitiesArray.Items[LCapabilityIdx].Value;
    FCapabilities.Add(LCapability);
  end;
  Device := AJSONValue.GetValue<string>('device');
  DisplayName := AJSONValue.GetValue<string>('display_name');
  DeviceName := AJSONValue.GetValue<string>('name', '');
  Number := AJSONValue.GetValue<string>('number');
  SMSNextCode := AJSONValue.GetValue<string>('sms_nextcode', '');
  DeviceType := AJSONValue.GetValue<string>('type');
end;

function TLZDUODevice.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
  LJSONArray: TJSONArray;
  LIdx: integer;
begin
  LJSONObject := TJSONObject.Create;
  try
    // Capabilities array
    if FCapabilities.Count > 0 then
    begin
      LJSONArray := TJSONArray.Create;
      for LIdx := 0 to FCapabilities.Count - 1 do
      begin
        LJSONArray.Add(FCapabilities[LIdx]);
      end;
      LJSONObject.AddPair('capabilities', LJSONArray);
    end;
    
    if not TLZString.IsEmptyString(FDevice) then
      LJSONObject.AddPair('device', FDevice);
    
    if not TLZString.IsEmptyString(FDisplayName) then
      LJSONObject.AddPair('display_name', FDisplayName);
    
    if not TLZString.IsEmptyString(FDeviceName) then
      LJSONObject.AddPair('name', FDeviceName);
    
    if not TLZString.IsEmptyString(FNumber) then
      LJSONObject.AddPair('number', FNumber);
    
    if not TLZString.IsEmptyString(FSMSNextCode) then
      LJSONObject.AddPair('sms_nextcode', FSMSNextCode);
    
    if not TLZString.IsEmptyString(FDeviceType) then
      LJSONObject.AddPair('type', FDeviceType);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

function TLZDUODevice.GetCapabilities: TStrings;
begin
  Result := FCapabilities;
end;

procedure TLZDUODevice.SetCapabilities(const Value: TStrings);
begin
  FCapabilities.Assign(Value);
end;

procedure TLZDUODevice.SetDevice(const Value: string);
begin
  FDevice := Value;
end;

procedure TLZDUODevice.SetDeviceName(const Value: string);
begin
  FDeviceName := Value;
end;

procedure TLZDUODevice.SetDeviceType(const Value: string);
begin
  FDeviceType := Value;
end;

procedure TLZDUODevice.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZDUODevice.SetNumber(const Value: string);
begin
  FNumber := Value;
end;

procedure TLZDUODevice.SetSMSNextCode(const Value: string);
begin
  FSMSNextCode := Value;
end;

{ TDUODevices }

function TLZDUODevices.Add(
  ADevice, ADisplayName, ADeviceName, ADeviceType, ANumber,
  ASMSNextCode: string;
  ACapabilities: TStrings;
  AUpdateExisting: boolean): TLZDUODevice;
begin
  Result := Find(ADevice);
  if not Assigned(Result) then
  begin
    Result := TLZDUODevice.CreateEx(ADevice, ADisplayName, ADeviceName,
      ADeviceType, ANumber, ASMSNextCode, ACapabilities);
    Add(Result);
  end
  else
  begin
    if AUpdateExisting then
    begin
      Result.DisplayName := ADisplayName;
      Result.DeviceName := ADeviceName;
      Result.DeviceType := ADeviceType;
      Result.Number := ANumber;
      Result.SMSNextCode := ASMSNextCode;
    end;
  end;
end;

function TLZDUODevices.Find(ADevice: string): TLZDUODevice;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].Device, ADevice) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZDUODevices.FindByNumber(ANumber: string): TLZDUODevice;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].Number, ANumber) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TDUOUser }

procedure TLZDUOUser.AfterModelCreated;
begin
  inherited;
  FAliases := TStringList.Create;
  FAliases.Duplicates := dupIgnore;
  FPhones := TLZDUOPhones.Create;
end;

procedure TLZDUOUser.BeforeModelDestroyed;
begin
  FreeAndNil(FAliases);
  FreeAndNil(FPhones);
  inherited;

end;

function TLZDUOUser.HasAlias(AAlias: string): boolean;
begin
  Result := FAliases.IndexOf(AAlias) <> -1;
end;

function TLZDUOUser.HasPhone(APhoneID: string): boolean;
begin
  Result := FPhones.Find(APhoneID) <> nil;
end;

procedure TLZDUOUser.FromJSONValue(AJSONValue: TJSONValue);
var
  LAliases: TJSONValue;
  LAlias: string;
  LAliasIdx: integer;
begin

  Phones.FromJSONValue(AJSONValue, false, 'phones');

  LAliases := AJSONValue.GetValue<TJSONValue>('aliases', nil);
  if Assigned(LAliases) then
  begin
    LAliasIdx := 1;
    Repeat
      LAlias := LAliases.GetValue<string>(Format('alias%d', [LAliasIdx]), '');
      if (LAlias <> '') then
      begin
        Aliases.Add(LAlias);
      end;
      Inc(LAliasIdx);
    Until LAlias = '';
  end;

  UserID := AJSONValue.GetValue<string>('user_id', '');
  Username := AJSONValue.GetValue<string>('username', '');
  CreatedTimeStamp := TLZDuoDateTimeHelpers.UnixTimestampToDateTime
    (AJSONValue.GetValue<string>('created', ''));
  LastLogin := TLZDuoDateTimeHelpers.UnixTimestampToDateTime
    (AJSONValue.GetValue<string>('last_login', ''));
  Email := AJSONValue.GetValue<string>('email', '');
  FirstName := AJSONValue.GetValue<string>('firstname', '');
  LastName := AJSONValue.GetValue<string>('lastname', '');
  RealName := AJSONValue.GetValue<string>('realname', '');
  LastDirectorySync := TLZDuoDateTimeHelpers.UnixTimestampToDateTime
    (AJSONValue.GetValue<string>('last_directory_sync', ''));
  IsEnrolled := AJSONValue.GetValue<boolean>('is_enrolled', false);
  Notes := AJSONValue.GetValue<string>('notes', '');
  Status := AJSONValue.GetValue<string>('status', '');

end;

function TLZDUOUser.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
  LAliasesObject: TJSONObject;
  LPhonesArray: TJSONValue;
  LIdx: integer;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FUserID) then
      LJSONObject.AddPair('user_id', FUserID);
    
    if not TLZString.IsEmptyString(FUsername) then
      LJSONObject.AddPair('username', FUsername);
    
    if FCreatedTimeStamp <> 0 then
      LJSONObject.AddPair('created', TLZDuoDateTimeHelpers.DateTimeToUnixTimestamp(FCreatedTimeStamp));
    
    if FLastLogin <> 0 then
      LJSONObject.AddPair('last_login', TLZDuoDateTimeHelpers.DateTimeToUnixTimestamp(FLastLogin));
    
    if not TLZString.IsEmptyString(FEmail) then
      LJSONObject.AddPair('email', FEmail);
    
    if not TLZString.IsEmptyString(FFirstName) then
      LJSONObject.AddPair('firstname', FFirstName);
    
    if not TLZString.IsEmptyString(FLastName) then
      LJSONObject.AddPair('lastname', FLastName);
    
    if not TLZString.IsEmptyString(FRealName) then
      LJSONObject.AddPair('realname', FRealName);
    
    if FLastDirectorySync <> 0 then
      LJSONObject.AddPair('last_directory_sync', TLZDuoDateTimeHelpers.DateTimeToUnixTimestamp(FLastDirectorySync));
    
    LJSONObject.AddPair('is_enrolled', TJSONBool.Create(FIsEnrolled));
    
    if not TLZString.IsEmptyString(FNotes) then
      LJSONObject.AddPair('notes', FNotes);
    
    if not TLZString.IsEmptyString(FStatus) then
      LJSONObject.AddPair('status', FStatus);
    
    // Aliases as object with alias1, alias2, etc.
    if FAliases.Count > 0 then
    begin
      LAliasesObject := TJSONObject.Create;
      for LIdx := 0 to FAliases.Count - 1 do
      begin
        LAliasesObject.AddPair(Format('alias%d', [LIdx + 1]), FAliases[LIdx]);
      end;
      LJSONObject.AddPair('aliases', LAliasesObject);
    end;
    
    // Phones array
    if FPhones.Count > 0 then
    begin
      LPhonesArray := TJSONArray.Create;
      for LIdx := 0 to FPhones.Count - 1 do
      begin
        (LPhonesArray as TJSONArray).AddElement(FPhones.Model[LIdx].ToJSONValue);
      end;
      LJSONObject.AddPair('phones', LPhonesArray);
    end;
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

function TLZDUOUser.GetAliases: TStrings;
begin
  Result := FAliases;
end;

function TLZDUOUser.GetPhones: TLZDUOPhones;
begin
  Result := FPhones;
end;

procedure TLZDUOUser.SetCreatedTimeStamp(const Value: TDateTIme);
begin
  FCreatedTimeStamp := Value;
end;

procedure TLZDUOUser.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TLZDUOUser.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TLZDUOUser.SetIsEnrolled(const Value: boolean);
begin
  FIsEnrolled := Value;
end;

procedure TLZDUOUser.SetLastDirectorySync(const Value: TDateTIme);
begin
  FLastDirectorySync := Value;
end;

procedure TLZDUOUser.SetLastLogin(const Value: TDateTIme);
begin
  FLastLogin := Value;
end;

procedure TLZDUOUser.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TLZDUOUser.SetNotes(const Value: string);
begin
  FNotes := Value;
end;

procedure TLZDUOUser.SetRealName(const Value: string);
begin
  FRealName := Value;
end;

procedure TLZDUOUser.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TLZDUOUser.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

procedure TLZDUOUser.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

{ TDUOUsers }

function TLZDUOUsers.Find(AUsername: string): TLZDUOUser;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].Username, AUsername) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZDUOUsers.FindAlias(AAlias: string): TLZDUOUser;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if Models[LIdx].HasAlias(AAlias) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TLZDUOUsers.FindUserID(AUserID: string): TLZDUOUser;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].UserID, AUserID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TDUOPhone }

procedure TLZDUOPhone.AfterModelCreated;
begin
  inherited;
  FCapabilities := TStringList.Create;
end;

procedure TLZDUOPhone.BeforeModelDestroyed;
begin
  FreeAndNil(FCapabilities);
  inherited;

end;

procedure TLZDUOPhone.FromJSONValue(AJSONValue: TJSONValue);
begin
  PhoneID := AJSONValue.GetValue<string>('phone_id', '');
  Number := AJSONValue.GetValue<string>('number', '');
end;

function TLZDUOPhone.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FPhoneID) then
      LJSONObject.AddPair('phone_id', FPhoneID);
    
    if not TLZString.IsEmptyString(FNumber) then
      LJSONObject.AddPair('number', FNumber);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

function TLZDUOPhone.GetCapabilities: TStrings;
begin
  Result := FCapabilities;
end;

procedure TLZDUOPhone.SetActivated(const Value: boolean);
begin
  FActivated := Value;
end;

procedure TLZDUOPhone.SetCapabilities(const Value: TStrings);
begin
  FCapabilities.Assign(Value);
end;

procedure TLZDUOPhone.SetEncrypted(const Value: string);
begin
  FEncrypted := Value;
end;

procedure TLZDUOPhone.SetExtension(const Value: string);
begin
  FExtension := Value;
end;

procedure TLZDUOPhone.SetFingerprint(const Value: string);
begin
  FFingerprint := Value;
end;

procedure TLZDUOPhone.SetLastSeen(const Value: TDateTIme);
begin
  FLastSeen := Value;
end;

procedure TLZDUOPhone.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TLZDUOPhone.SetNumber(const Value: string);
begin
  FNumber := Value;
end;

procedure TLZDUOPhone.SetPhoneID(const Value: string);
begin
  FPhoneID := Value;
end;

procedure TLZDUOPhone.SetPhoneName(const Value: string);
begin
  FPhoneName := Value;
end;

procedure TLZDUOPhone.SetPhoneType(const Value: string);
begin
  FPhoneType := Value;
end;

procedure TLZDUOPhone.SetPlatform(const Value: string);
begin
  FPlatform := Value;
end;

procedure TLZDUOPhone.SetPostDelay(const Value: string);
begin
  FPostDelay := Value;
end;

procedure TLZDUOPhone.SetPreDelay(const Value: string);
begin
  FPreDelay := Value;
end;

procedure TLZDUOPhone.SetScreenLock(const Value: string);
begin
  FScreenLock := Value;
end;

procedure TLZDUOPhone.SetSMSPasscodesSent(const Value: boolean);
begin
  FSMSPasscodesSent := Value;
end;

procedure TLZDUOPhone.SetTampered(const Value: string);
begin
  FTampered := Value;
end;

{ TDUOPhones }

function TLZDUOPhones.Find(APhoneID: string): TLZDUOPhone;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Models[LIdx].PhoneID, APhoneID) then
    begin
      Result := Models[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TDUOAccount }

procedure TLZDUOAccount.FromJSONValue(AJSONValue: TJSONValue);
begin
  BillingEdition := '';
  TeleponyCredits := 0;
  APIHostname := AJSONValue.GetValue<string>('api_hostname', '');
  AccountName := AJSONValue.GetValue<string>('name', '');
  AccountID := AJSONValue.GetValue<string>('account_id', '');
end;

function TLZDUOAccount.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FAPIHostname) then
      LJSONObject.AddPair('api_hostname', FAPIHostname);
    
    if not TLZString.IsEmptyString(FAccountName) then
      LJSONObject.AddPair('name', FAccountName);
    
    if not TLZString.IsEmptyString(FAccountID) then
      LJSONObject.AddPair('account_id', FAccountID);
    
    if not TLZString.IsEmptyString(FBillingEdition) then
      LJSONObject.AddPair('billing_edition', FBillingEdition);
    
    LJSONObject.AddPair('telephony_credits', TJSONNumber.Create(FTeleponyCredits));
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZDUOAccount.SetAccountID(const Value: string);
begin
  FAccountID := Value;
end;

procedure TLZDUOAccount.SetAccountName(const Value: string);
begin
  FAccountName := Value;
end;

procedure TLZDUOAccount.SetAPIHostname(const Value: string);
begin
  FAPIHostname := Value;
end;

procedure TLZDUOAccount.SetBillingEdition(const Value: string);
begin
  FBillingEdition := Value;
end;

procedure TLZDUOAccount.SetTeleponyCredits(const Value: integer);
begin
  FTeleponyCredits := Value;
end;

end.
