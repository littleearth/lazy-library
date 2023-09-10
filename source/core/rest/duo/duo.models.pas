unit duo.models;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  REST.Authenticator.Basic, System.JSON, duo.api, System.Generics.Collections;

type
  TDUODevice = class(TDuoModel)
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
  public
    constructor Create; reintroduce; overload;
    constructor Create(ADevice: string; ADisplayName: string;
      ADeviceName: string; ADeviceType: string; ANumber: string;
      ASMSNextCode: string; ACapabilities: TStrings = nil); overload;
    destructor Destroy; override;
    procedure Assign(ADUODevice: TDUODevice); reintroduce;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property Device: string read FDevice write SetDevice;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property DeviceName: string read FDeviceName write SetDeviceName;
    property DeviceType: string read FDeviceType write SetDeviceType;
    property Number: string read FNumber write SetNumber;
    property SMSNextCode: string read FSMSNextCode write SetSMSNextCode;
    property Capabilities: TStrings read GetCapabilities;
  end;

  TDUODevices = class(TDuoModelList<TDUODevice>)
  protected
  public
    function Add(ADevice, ADisplayName, ADeviceName, ADeviceType, ANumber,
      ASMSNextCode: string; ACapabilities: TStrings;
      AUpdateExisting: boolean = true): TDUODevice; reintroduce; overload;
    function Find(ADevice: string): TDUODevice;
    function FindByNumber(ANumber: string): TDUODevice;
  end;

  TDUOPhone = class(TDuoModel)
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
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Assign(ASource: TDUOPhone); reintroduce;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property Activated: boolean read FActivated write SetActivated;
    property Capabilities: TStrings read GetCapabilities;
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

  TDUOPhones = class(TDuoModelList<TDUOPhone>)
  public
    function Find(APhoneID: string): TDUOPhone;
  end;

  TDUOUser = class(TDuoModel)
  private
    FCreatedTimeStamp: TDateTIme;
    FLastName: string;
    FEmail: string;
    FNotes: string;
    FUserID: string;
    FIsEnrolled: boolean;
    FRealName: string;
    FAliases: TStringList;
    FPhones: TDUOPhones;
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
    function GetPhones: TDUOPhones;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Assign(ASource: TDUOUser); reintroduce;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function HasAlias(AAlias: string): boolean;
    function HasPhone(APhoneID: string): boolean;
    property UserID: string read FUserID write SetUserID;
    property Username: string read FUsername write SetUsername;
    property Aliases: TStrings read GetAliases;
    property Phones: TDUOPhones read GetPhones;
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

  TDUOUsers = class(TDuoModelList<TDUOUser>)
  public
    function Find(AUsername: string): TDUOUser;
    function FindUserID(AUserID: string): TDUOUser;
    function FindAlias(AAlias: string): TDUOUser;
  end;

  TDUOAccount = class(TDuoModel)
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
    procedure Assign(ASource: TDUOAccount); reintroduce;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property AccountID: string read FAccountID write SetAccountID;
    property AccountName: string read FAccountName write SetAccountName;
    property APIHostname: string read FAPIHostname write SetAPIHostname;
    property BillingEdition: string read FBillingEdition
      write SetBillingEdition;
    property TeleponyCredits: integer read FTeleponyCredits
      write SetTeleponyCredits;
  end;

  TDUOAccounts = class(TDuoModelList<TDUOAccount>);

implementation

{ TDUODevice }

procedure TDUODevice.Assign(ADUODevice: TDUODevice);
begin
  FDevice := ADUODevice.Device;
  FDisplayName := ADUODevice.DisplayName;
  FDeviceName := ADUODevice.DeviceName;
  FDeviceType := ADUODevice.DeviceType;
  FNumber := ADUODevice.Number;
  FSMSNextCode := ADUODevice.SMSNextCode;
  FCapabilities.Assign(ADUODevice.Capabilities);
end;

constructor TDUODevice.Create(ADevice, ADisplayName, ADeviceName, ADeviceType,
  ANumber, ASMSNextCode: string; ACapabilities: TStrings);
begin
  Create;
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

constructor TDUODevice.Create;
begin
  inherited Create;
  FCapabilities := TStringList.Create;
end;

destructor TDUODevice.Destroy;
begin
  try
    FreeAndNil(FCapabilities);
  finally
    inherited;
  end;
end;

procedure TDUODevice.FromJSONValue(AJSONValue: TJSONValue);
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

function TDUODevice.GetCapabilities: TStrings;
begin
  Result := FCapabilities;
end;

procedure TDUODevice.SetDevice(const Value: string);
begin
  FDevice := Value;
end;

procedure TDUODevice.SetDeviceName(const Value: string);
begin
  FDeviceName := Value;
end;

procedure TDUODevice.SetDeviceType(const Value: string);
begin
  FDeviceType := Value;
end;

procedure TDUODevice.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TDUODevice.SetNumber(const Value: string);
begin
  FNumber := Value;
end;

procedure TDUODevice.SetSMSNextCode(const Value: string);
begin
  FSMSNextCode := Value;
end;

{ TDUODevices }

function TDUODevices.Add(ADevice, ADisplayName, ADeviceName, ADeviceType,
  ANumber, ASMSNextCode: string; ACapabilities: TStrings;
  AUpdateExisting: boolean): TDUODevice;
begin
  Result := Find(ADevice);
  if not Assigned(Result) then
  begin
    Result := TDUODevice.Create(ADevice, ADisplayName, ADeviceName, ADeviceType,
      ANumber, ASMSNextCode, ACapabilities);
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

function TDUODevices.Find(ADevice: string): TDUODevice;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Items[LIdx].Device, ADevice) then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TDUODevices.FindByNumber(ANumber: string): TDUODevice;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Items[LIdx].Number, ANumber) then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TDUOUser }

procedure TDUOUser.Assign(ASource: TDUOUser);
begin
  FCreatedTimeStamp := ASource.CreatedTimeStamp;
  FLastName := ASource.LastName;
  FEmail := ASource.Email;
  FNotes := ASource.Notes;
  FUserID := ASource.UserID;
  FIsEnrolled := ASource.IsEnrolled;
  FRealName := ASource.RealName;
  FAliases.Assign(ASource.Aliases);
  FFirstName := ASource.FirstName;
  FUsername := ASource.Username;
  FLastLogin := ASource.LastLogin;
  FLastDirectorySync := ASource.LastDirectorySync;
  FPhones.Assign(ASource.Phones);
end;

constructor TDUOUser.Create;
begin
  inherited Create;
  FAliases := TStringList.Create;
  FAliases.Duplicates := dupIgnore;
  FPhones := TDUOPhones.Create;
end;

destructor TDUOUser.Destroy;
begin
  try
    FreeAndNil(FAliases);
    FreeAndNil(FPhones);
  finally
    inherited;
  end;
end;

function TDUOUser.HasAlias(AAlias: string): boolean;
begin
  Result := FAliases.IndexOf(AAlias) <> -1;
end;

function TDUOUser.HasPhone(APhoneID: string): boolean;
begin
  Result := FPhones.Find(APhoneID) <> nil;
end;

procedure TDUOUser.FromJSONValue(AJSONValue: TJSONValue);
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
  CreatedTimeStamp := TDuoDateTimeHelpers.UnixTimestampToDateTime
    (AJSONValue.GetValue<string>('created', ''));
  LastLogin := TDuoDateTimeHelpers.UnixTimestampToDateTime
    (AJSONValue.GetValue<string>('last_login', ''));
  Email := AJSONValue.GetValue<string>('email', '');
  FirstName := AJSONValue.GetValue<string>('firstname', '');
  LastName := AJSONValue.GetValue<string>('lastname', '');
  RealName := AJSONValue.GetValue<string>('realname', '');
  LastDirectorySync := TDuoDateTimeHelpers.UnixTimestampToDateTime
    (AJSONValue.GetValue<string>('last_directory_sync', ''));
  IsEnrolled := AJSONValue.GetValue<boolean>('is_enrolled', false);
  Notes := AJSONValue.GetValue<string>('notes', '');
  Status := AJSONValue.GetValue<string>('status', '');

end;

function TDUOUser.GetAliases: TStrings;
begin
  Result := FAliases;
end;

function TDUOUser.GetPhones: TDUOPhones;
begin
  Result := FPhones;
end;

procedure TDUOUser.SetCreatedTimeStamp(const Value: TDateTIme);
begin
  FCreatedTimeStamp := Value;
end;

procedure TDUOUser.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TDUOUser.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TDUOUser.SetIsEnrolled(const Value: boolean);
begin
  FIsEnrolled := Value;
end;

procedure TDUOUser.SetLastDirectorySync(const Value: TDateTIme);
begin
  FLastDirectorySync := Value;
end;

procedure TDUOUser.SetLastLogin(const Value: TDateTIme);
begin
  FLastLogin := Value;
end;

procedure TDUOUser.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TDUOUser.SetNotes(const Value: string);
begin
  FNotes := Value;
end;

procedure TDUOUser.SetRealName(const Value: string);
begin
  FRealName := Value;
end;

procedure TDUOUser.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TDUOUser.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

procedure TDUOUser.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

{ TDUOUsers }

function TDUOUsers.Find(AUsername: string): TDUOUser;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Items[LIdx].Username, AUsername) then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TDUOUsers.FindAlias(AAlias: string): TDUOUser;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if Items[LIdx].HasAlias(AAlias) then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TDUOUsers.FindUserID(AUserID: string): TDUOUser;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Items[LIdx].UserID, AUserID) then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TDUOPhone }

procedure TDUOPhone.Assign(ASource: TDUOPhone);
begin
  FPhoneType := ASource.PhoneType;
  FTampered := ASource.Tampered;
  FScreenLock := ASource.ScreenLock;
  FLastSeen := ASource.LastSeen;
  FModel := ASource.Model;
  FSMSPasscodesSent := ASource.SMSPasscodesSent;
  FPostDelay := ASource.PostDelay;
  FExtension := ASource.Extension;
  FActivated := ASource.Activated;
  FPhoneName := ASource.PhoneName;
  FPlatform := ASource.Platform;
  FNumber := ASource.Number;
  FFingerprint := ASource.Fingerprint;
  FEncrypted := ASource.Encrypted;
  FPreDelay := ASource.PreDelay;
  FPhoneID := ASource.PhoneID;
  FCapabilities.Assign(ASource.Capabilities);
end;

constructor TDUOPhone.Create;
begin
  inherited Create;
  FCapabilities := TStringList.Create;
end;

destructor TDUOPhone.Destroy;
begin
  try
    FreeAndNil(FCapabilities);
  finally
    inherited;
  end;

end;

procedure TDUOPhone.FromJSONValue(AJSONValue: TJSONValue);
begin
  PhoneID := AJSONValue.GetValue<string>('phone_id', '');
  Number := AJSONValue.GetValue<string>('number', '');
end;

function TDUOPhone.GetCapabilities: TStrings;
begin
  Result := FCapabilities;
end;

procedure TDUOPhone.SetActivated(const Value: boolean);
begin
  FActivated := Value;
end;

procedure TDUOPhone.SetEncrypted(const Value: string);
begin
  FEncrypted := Value;
end;

procedure TDUOPhone.SetExtension(const Value: string);
begin
  FExtension := Value;
end;

procedure TDUOPhone.SetFingerprint(const Value: string);
begin
  FFingerprint := Value;
end;

procedure TDUOPhone.SetLastSeen(const Value: TDateTIme);
begin
  FLastSeen := Value;
end;

procedure TDUOPhone.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TDUOPhone.SetNumber(const Value: string);
begin
  FNumber := Value;
end;

procedure TDUOPhone.SetPhoneID(const Value: string);
begin
  FPhoneID := Value;
end;

procedure TDUOPhone.SetPhoneName(const Value: string);
begin
  FPhoneName := Value;
end;

procedure TDUOPhone.SetPhoneType(const Value: string);
begin
  FPhoneType := Value;
end;

procedure TDUOPhone.SetPlatform(const Value: string);
begin
  FPlatform := Value;
end;

procedure TDUOPhone.SetPostDelay(const Value: string);
begin
  FPostDelay := Value;
end;

procedure TDUOPhone.SetPreDelay(const Value: string);
begin
  FPreDelay := Value;
end;

procedure TDUOPhone.SetScreenLock(const Value: string);
begin
  FScreenLock := Value;
end;

procedure TDUOPhone.SetSMSPasscodesSent(const Value: boolean);
begin
  FSMSPasscodesSent := Value;
end;

procedure TDUOPhone.SetTampered(const Value: string);
begin
  FTampered := Value;
end;

{ TDUOPhones }

function TDUOPhones.Find(APhoneID: string): TDUOPhone;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (Result = nil) and (LIdx < Count) do
  begin
    if SameText(Items[LIdx].PhoneID, APhoneID) then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

{ TDUOAccount }

procedure TDUOAccount.Assign(ASource: TDUOAccount);
begin
  inherited;
  APIHostname := ASource.APIHostname;
  AccountID := ASource.AccountID;
  AccountName := ASource.AccountName;
  BillingEdition := ASource.BillingEdition;
  TeleponyCredits := ASource.TeleponyCredits;
end;

procedure TDUOAccount.FromJSONValue(AJSONValue: TJSONValue);
begin
  BillingEdition := '';
  TeleponyCredits := 0;
  APIHostname := AJSONValue.GetValue<string>('api_hostname', '');
  AccountName := AJSONValue.GetValue<string>('name', '');
  AccountID := AJSONValue.GetValue<string>('account_id', '');
end;

procedure TDUOAccount.SetAccountID(const Value: string);
begin
  FAccountID := Value;
end;

procedure TDUOAccount.SetAccountName(const Value: string);
begin
  FAccountName := Value;
end;

procedure TDUOAccount.SetAPIHostname(const Value: string);
begin
  FAPIHostname := Value;
end;

procedure TDUOAccount.SetBillingEdition(const Value: string);
begin
  FBillingEdition := Value;
end;

procedure TDUOAccount.SetTeleponyCredits(const Value: integer);
begin
  FTeleponyCredits := Value;
end;

end.
