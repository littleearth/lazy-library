unit MSGraph.Models;

interface

uses
  System.SysUtils, System.Variants, System.Classes, REST.Types, REST.client,
  System.JSON, MSGraph.api, System.Generics.Collections;

type

  TLZMSGraphDomain = class(TLZMSGraphModel)
  private
    FIsAdminManaged: boolean;
    FSupportedServices: string;
    FAuthenticationType: string;
    FIsDefault: boolean;
    FID: string;
    FIsInitial: boolean;
    FAvailabilityStatus: string;
    FIsRoot: boolean;
    procedure SetAuthenticationType(const Value: string);
    procedure SetAvailabilityStatus(const Value: string);
    procedure SetID(const Value: string);
    procedure SetIsAdminManaged(const Value: boolean);
    procedure SetIsDefault(const Value: boolean);
    procedure SetIsInitial(const Value: boolean);
    procedure SetIsRoot(const Value: boolean);
    procedure SetSupportedServices(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property AuthenticationType: string read FAuthenticationType
      write SetAuthenticationType;
    property AvailabilityStatus: string read FAvailabilityStatus
      write SetAvailabilityStatus;
    property IsAdminManaged: boolean read FIsAdminManaged
      write SetIsAdminManaged;
    property IsDefault: boolean read FIsDefault write SetIsDefault;
    property IsInitial: boolean read FIsInitial write SetIsInitial;
    property IsRoot: boolean read FIsRoot write SetIsRoot;
    property ID: string read FID write SetID;
    property SupportedServices: string read FSupportedServices
      write SetSupportedServices;
  end;

  TLZMSGraphDomains = class(TLZMSGraphModelList<TLZMSGraphDomain>)
  end;

  TLZMSGraphLicenseDetail = class(TLZMSGraphModel)
  private
    FSKUPartNumber: string;
    FSKUID: string;
    FID: string;
    procedure SetID(const Value: string);
    procedure SetSKUID(const Value: string);
    procedure SetSKUPartNumber(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property ID: string read FID write SetID;
    property SKUID: string read FSKUID write SetSKUID;
    property SKUPartNumber: string read FSKUPartNumber write SetSKUPartNumber;
  end;

  TLZMSGraphLicenseDetails = class(TLZMSGraphModelList<TLZMSGraphLicenseDetail>)

  end;

  TLZMSGraphCompanySubscription = class(TLZMSGraphModel)
  private
    FSKUPartNumber: string;
    FSKUID: string;
    FTotalLiceses: integer;
    FNextLifecycleDateTime: TDateTime;
    FOwnerTenantID: string;
    FID: string;
    FStatus: string;
    FOwnerID: string;
    FCreatedDateTime: TDateTime;
    FCommerceSubscriptionId: string;
    FOwnerType: string;
    FIsTrial: boolean;
    procedure SetCommerceSubscriptionId(const Value: string);
    procedure SetCreateDateTime(const Value: TDateTime);
    procedure SetID(const Value: string);
    procedure SetIsTrial(const Value: boolean);
    procedure SetNextLifecycleDateTime(const Value: TDateTime);
    procedure SetOwnerID(const Value: string);
    procedure SetOwnerTenantID(const Value: string);
    procedure SetOwnerType(const Value: string);
    procedure SetSKUID(const Value: string);
    procedure SetSKUPartNumber(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTotalLiceses(const Value: integer);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property ID: string read FID write SetID;
    property CommerceSubscriptionId: string read FCommerceSubscriptionId
      write SetCommerceSubscriptionId;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreateDateTime;
    property IsTrial: boolean read FIsTrial write SetIsTrial;
    property NextLifecycleDateTime: TDateTime read FNextLifecycleDateTime
      write SetNextLifecycleDateTime;
    property SKUID: string read FSKUID write SetSKUID;
    property SKUPartNumber: string read FSKUPartNumber write SetSKUPartNumber;
    property Status: string read FStatus write SetStatus;
    property TotalLicenses: integer read FTotalLiceses write SetTotalLiceses;
    property OwnerID: string read FOwnerID write SetOwnerID;
    property OwnerTenantID: string read FOwnerTenantID write SetOwnerTenantID;
    property OwnerType: string read FOwnerType write SetOwnerType;
  end;

  TLZMSGraphCompanySubscriptions = class
    (TLZMSGraphModelList<TLZMSGraphCompanySubscription>)

  end;

  TLZMSGraphUser = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FUserPrincipalName: string;
    FCreatedDateTime: TDateTime;
    FSignInActivity: TDateTime;
    FID: string;
    FProxyAddresses: string;
    FMail: string;
    FAccountEnabled: boolean;
    FDepartment: string;
    FCompanyName: string;
    FMobilePhone: string;
    FJobTitle: string;
    FUserType: string;
    procedure SetCreatedate(const Value: TDateTime);
    procedure SetDisplayName(const Value: string);
    procedure SetID(const Value: string);
    procedure SetProxyAddresses(const Value: string);
    procedure SetSignInActivity(const Value: TDateTime);
    procedure SetuserPrincipalName(const Value: string);
    procedure SetMail(const Value: string);
    procedure SetAccountEnabled(const Value: boolean);
    procedure SetCompanyName(const Value: string);
    procedure SetDepartment(const Value: string);
    procedure SetJobTitle(const Value: string);
    procedure SetMobilePhone(const Value: string);
    procedure SetUserType(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property ID: string read FID write SetID;
    property AccountEnabled: boolean read FAccountEnabled
      write SetAccountEnabled;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property UserPrincipalName: string read FUserPrincipalName
      write SetuserPrincipalName;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreatedate;
    property SignInActivity: TDateTime read FSignInActivity
      write SetSignInActivity;
    property ProxyAddresses: string read FProxyAddresses
      write SetProxyAddresses;
    property Mail: string read FMail write SetMail;
    property CompanyName: string read FCompanyName write SetCompanyName;
    property Department: string read FDepartment write SetDepartment;
    property JobTitle: string read FJobTitle write SetJobTitle;
    property MobilePhone: string read FMobilePhone write SetMobilePhone;
    property UserType: string read FUserType write SetUserType;
  end;

  TLZMSGraphUsers = class(TLZMSGraphModelList<TLZMSGraphUser>)
  public
    function Find(AID: string): TLZMSGraphUser;
  end;

  TLZMSGraphMailFolder = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FIsHidden: boolean;
    FTotalItemCount: integer;
    FChildFolderCount: integer;
    FID: string;
    FUnreadItemCount: integer;
    FParentFolderID: string;
    procedure SetChildFolderCount(const Value: integer);
    procedure SetDisplayName(const Value: string);
    procedure SetID(const Value: string);
    procedure SetIsHidden(const Value: boolean);
    procedure SetParentFolderID(const Value: string);
    procedure SetTotalItemCount(const Value: integer);
    procedure SetUnreadItemCount(const Value: integer);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property ID: string read FID write SetID;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property ParentFolderID: string read FParentFolderID
      write SetParentFolderID;
    property ChildFolderCount: integer read FChildFolderCount
      write SetChildFolderCount;
    property UnreadItemCount: integer read FUnreadItemCount
      write SetUnreadItemCount;
    property TotalItemCount: integer read FTotalItemCount
      write SetTotalItemCount;
    property IsHidden: boolean read FIsHidden write SetIsHidden;
  end;

  TLZMSGraphMailFolders = class(TLZMSGraphModelList<TLZMSGraphMailFolder>)
  public
    function Find(AID: string): TLZMSGraphMailFolder;
  end;

  TLZMSGraphGroup = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FCreatedDateTime: TDateTime;
    FID: string;
    FDescription: string;
    FProxyAddresses: string;
    FVisibility: string;
    FMail: string;
    FMailEnabled: boolean;
    FSecurityEnabled: boolean;
    procedure SetCreatedDateTime(const Value: TDateTime);
    procedure SetDescription(const Value: string);
    procedure SetDisplayName(const Value: string);
    procedure SetID(const Value: string);
    procedure SetMail(const Value: string);
    procedure SetMailEnabled(const Value: boolean);
    procedure SetProxyAddresses(const Value: string);
    procedure SetVisibility(const Value: string);
    procedure SetSecurityEnabled(const Value: boolean);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property ID: string read FID write SetID;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property Description: string read FDescription write SetDescription;
    property Mail: string read FMail write SetMail;
    property MailEnabled: boolean read FMailEnabled write SetMailEnabled;
    property Visibility: string read FVisibility write SetVisibility;
    property SecurityEnabled: boolean read FSecurityEnabled
      write SetSecurityEnabled;
    property ProxyAddresses: string read FProxyAddresses
      write SetProxyAddresses;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreatedDateTime;
  end;

  TLZMSGraphGroups = class(TLZMSGraphModelList<TLZMSGraphGroup>)
  public
    function Find(AID: string): TLZMSGraphGroup;
  end;

  TLZMSGraphGroupMember = class(TLZMSGraphModel)
  private
    FID: string;
    procedure SetID(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property ID: string read FID write SetID;
  end;

  TLZMSGraphGroupMembers = class(TLZMSGraphModelList<TLZMSGraphGroupMember>)
  public
    function Find(AID: string): TLZMSGraphGroupMember;
  end;

  TLZMSGraphAuthenticationMethodBase = class(TLZMSGraphModel)
  private
    FID: string;
    procedure SetID(const Value: string);
  protected
    class function GetResourceType: string; virtual; abstract;
    class function IsResourceType(AValue: string): boolean;
    function GetDetails: string; virtual;
  public
    property ResourceType: string read GetResourceType;
    property Details: string read GetDetails;
    property ID: string read FID write SetID;
  end;

  TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod = class
    (TLZMSGraphAuthenticationMethodBase)
  private
    FDeviceTag: string;
    FDisplayName: string;
    FCreatedDateTime: TDateTime;
    FPhoneAppVersion: string;
    procedure SetCreatedDateTime(const Value: TDateTime);
    procedure SetDeviceTag(const Value: string);
    procedure SetDisplayName(const Value: string);
    procedure SetPhoneAppVersion(const Value: string);
  protected
    class function GetResourceType: string; override;
    function GetDetails: string; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property DeviceTag: string read FDeviceTag write SetDeviceTag;
    property PhoneAppVersion: string read FPhoneAppVersion
      write SetPhoneAppVersion;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreatedDateTime;
  end;

  TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethods = class
    (TLZMSGraphModelList<
    TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod>)
  public
    function Find(AID: string)
      : TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod;
  end;

  TLZMSGraphUserSoftwareOathAuthenticationMethod = class
    (TLZMSGraphAuthenticationMethodBase)
  private
  protected
    class function GetResourceType: string; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
  end;

  TLZMSGraphUserSoftwareOathAuthenticationMethods = class
    (TLZMSGraphModelList<TLZMSGraphUserSoftwareOathAuthenticationMethod>)
  public
    function Find(AID: string): TLZMSGraphUserSoftwareOathAuthenticationMethod;
  end;

  TLZMSGraphUserPhoneAuthenticationMethod = class
    (TLZMSGraphAuthenticationMethodBase)
  private
    FPhoneNumber: string;
    FPhoneType: string;
    FSMSSignInState: string;
    procedure SetPhoneNumber(const Value: string);
    procedure SetPhoneType(const Value: string);
    procedure SetSMSSignInState(const Value: string);
  protected
    class function GetResourceType: string; override;
    function GetDetails: string; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property PhoneNumber: string read FPhoneNumber write SetPhoneNumber;
    property PhoneType: string read FPhoneType write SetPhoneType;
    property SMSSignInState: string read FSMSSignInState
      write SetSMSSignInState;
  end;

  TLZMSGraphUserPhoneAuthenticationMethods = class
    (TLZMSGraphModelList<TLZMSGraphUserPhoneAuthenticationMethod>)
  public
    function Find(AID: string): TLZMSGraphUserPhoneAuthenticationMethod;
  end;

  TLZMSGraphUserPasswordAuthenticationMethod = class
    (TLZMSGraphAuthenticationMethodBase)
  private
  protected
    class function GetResourceType: string; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
  end;

  TLZMSGraphUserPasswordAuthenticationMethods = class
    (TLZMSGraphModelList<TLZMSGraphUserPasswordAuthenticationMethod>)
  public
    function Find(AID: string): TLZMSGraphUserPasswordAuthenticationMethod;
  end;

  TLZMSGraphAuthenticationMethodClass = class of
    TLZMSGraphAuthenticationMethodBase;

  TLZMSGraphUserAuthenticationMethods = class
    (TLZMSGraphModelList<TLZMSGraphAuthenticationMethodBase>)
  protected
    function GetModelClass(AResourceType: string)
      : TLZMSGraphAuthenticationMethodClass;
  public
    class function CreateAuthenticationModel(AResourceType: string)
      : TLZMSGraphAuthenticationMethodBase;
    function Find(AID: string): TLZMSGraphAuthenticationMethodBase;
    procedure FromJSONValue(
      AJSONValue: TJSONValue;
      AClear: boolean = true;
      AArrayName: string = '';
      AFreeJSONValue: boolean = false); override;
  end;

  TLZMSGraphMailRule = class(TLZMSGraphModel)
  private
    FEnabled: boolean;
    FDisplayName: string;
    FID: string;
    FForwardTo: string;
    FMoveToFolder: string;
    FHasError: boolean;
    procedure SetDisplayName(const Value: string);
    procedure SetEnabled(const Value: boolean);
    procedure SetForwardTo(const Value: string);
    procedure SetHasError(const Value: boolean);
    procedure SetID(const Value: string);
    procedure SetMoveToFolder(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property ID: string read FID write SetID;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property Enabled: boolean read FEnabled write SetEnabled;
    property HasError: boolean read FHasError write SetHasError;
    property MoveToFolder: string read FMoveToFolder write SetMoveToFolder;
    property ForwardTo: string read FForwardTo write SetForwardTo;
  end;

  TLZMSGraphMailRules = class(TLZMSGraphModelList<TLZMSGraphMailRule>)
  public
    function Find(AID: string): TLZMSGraphMailRule;
  end;

  TLZMSGraphAuditSignIn = class(TLZMSGraphModel)
  private
    FUserPrincipalName: string;
    FDeviceIsCompliant: boolean;
    FLocationLatitude: double;
    FRiskLevelAggregated: string;
    FLocationCountryOrRegion: string;
    FDeviceBrowser: string;
    FClientAppUsed: string;
    FUserID: string;
    FLocationState: string;
    FDeviceTrustType: string;
    FAppID: string;
    FDeviceIsManaged: boolean;
    FCorrelationID: string;
    FStatusAdditionalDetails: string;
    FResourceDisplayName: string;
    FID: string;
    FDeviceDisplayName: string;
    FLocationLongitude: double;
    FRiskLevelDuringSignIn: string;
    FIPAddress: string;
    FStatusCode: integer;
    FLocationCity: string;
    FRiskDetail: string;
    FDeviceOperatingSystem: string;
    FResourceID: string;
    FRiskState: string;
    FIsInteractive: boolean;
    FDeviceID: string;
    FConditionalAccessStatus: string;
    FUserDisplayName: string;
    FAppDisplayName: string;
    FStatusFailureReason: string;
    FCreatedDateTime: TDateTime;
    procedure SetAppDisplayName(const Value: string);
    procedure SetAppID(const Value: string);
    procedure SetClientAppUsed(const Value: string);
    procedure SetConditionalAccessStatus(const Value: string);
    procedure SetCorrelationID(const Value: string);
    procedure SetDeviceBrowser(const Value: string);
    procedure SetDeviceDisplayName(const Value: string);
    procedure SetDeviceID(const Value: string);
    procedure SetDeviceIsCompliant(const Value: boolean);
    procedure SetDeviceIsManaged(const Value: boolean);
    procedure SetDeviceOperatingSystem(const Value: string);
    procedure SetDeviceTrustType(const Value: string);
    procedure SetID(const Value: string);
    procedure SetIPAddress(const Value: string);
    procedure SetIsInteractive(const Value: boolean);
    procedure SetLocationCity(const Value: string);
    procedure SetLocationCountryOrRegion(const Value: string);
    procedure SetLocationLatitude(const Value: double);
    procedure SetLocationLongitude(const Value: double);
    procedure SetLocationState(const Value: string);
    procedure SetResourceDisplayName(const Value: string);
    procedure SetResourceID(const Value: string);
    procedure SetRiskDetail(const Value: string);
    procedure SetRiskLevelAggregated(const Value: string);
    procedure SetRiskLevelDuringSignIn(const Value: string);
    procedure SetRiskState(const Value: string);
    procedure SetStatusAdditionalDetails(const Value: string);
    procedure SetStatusCode(const Value: integer);
    procedure SetStatusFailureReason(const Value: string);
    procedure SetUserDisplayName(const Value: string);
    procedure SetUserID(const Value: string);
    procedure SetuserPrincipalName(const Value: string);
    procedure SetCreatedDateTime(const Value: TDateTime);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreatedDateTime;
    property ID: string read FID write SetID;
    property UserID: string read FUserID write SetUserID;
    property AppID: string read FAppID write SetAppID;
    property AppDisplayName: string read FAppDisplayName
      write SetAppDisplayName;
    property UserDisplayName: string read FUserDisplayName
      write SetUserDisplayName;
    property UserPrincipalName: string read FUserPrincipalName
      write SetuserPrincipalName;
    property IPAddress: string read FIPAddress write SetIPAddress;
    property ClientAppUsed: string read FClientAppUsed write SetClientAppUsed;
    property CorrelationID: string read FCorrelationID write SetCorrelationID;
    property ConditionalAccessStatus: string read FConditionalAccessStatus
      write SetConditionalAccessStatus;
    property IsInteractive: boolean read FIsInteractive write SetIsInteractive;
    property RiskDetail: string read FRiskDetail write SetRiskDetail;
    property RiskLevelAggregated: string read FRiskLevelAggregated
      write SetRiskLevelAggregated;
    property RiskLevelDuringSignIn: string read FRiskLevelDuringSignIn
      write SetRiskLevelDuringSignIn;
    property RiskState: string read FRiskState write SetRiskState;
    property ResourceID: string read FResourceID write SetResourceID;
    property ResourceDisplayName: string read FResourceDisplayName
      write SetResourceDisplayName;
    property StatusCode: integer read FStatusCode write SetStatusCode;
    property StatusFailureReason: string read FStatusFailureReason
      write SetStatusFailureReason;
    property StatusAdditionalDetails: string read FStatusAdditionalDetails
      write SetStatusAdditionalDetails;
    property DeviceID: string read FDeviceID write SetDeviceID;
    property DeviceDisplayName: string read FDeviceDisplayName
      write SetDeviceDisplayName;
    property DeviceOperatingSystem: string read FDeviceOperatingSystem
      write SetDeviceOperatingSystem;
    property DeviceBrowser: string read FDeviceBrowser write SetDeviceBrowser;
    property DeviceIsCompliant: boolean read FDeviceIsCompliant
      write SetDeviceIsCompliant;
    property DeviceIsManaged: boolean read FDeviceIsManaged
      write SetDeviceIsManaged;
    property DeviceTrustType: string read FDeviceTrustType
      write SetDeviceTrustType;
    property LocationCity: string read FLocationCity write SetLocationCity;
    property LocationState: string read FLocationState write SetLocationState;
    property LocationCountryOrRegion: string read FLocationCountryOrRegion
      write SetLocationCountryOrRegion;
    property LocationLatitude: double read FLocationLatitude
      write SetLocationLatitude;
    property LocationLongitude: double read FLocationLongitude
      write SetLocationLongitude;
  end;

  TLZMSGraphAuditSignIns = class(TLZMSGraphModelList<TLZMSGraphAuditSignIn>)
  public
    function Find(AID: string): TLZMSGraphAuditSignIn;
  end;

  TLZMSGraphPasswordCredential = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FHint: string;
    FStartDateTime: TDateTime;
    FSecretText: string;
    FKeyID: string;
    FEndDateTime: TDateTime;
    procedure SetDisplayName(const Value: string);
    procedure SetEndDateTime(const Value: TDateTime);
    procedure SetHint(const Value: string);
    procedure SetKeyID(const Value: string);
    procedure SetSecretText(const Value: string);
    procedure SetStartDateTime(const Value: TDateTime);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property KeyID: string read FKeyID write SetKeyID;
    property Hint: string read FHint write SetHint;
    property SecretText: string read FSecretText write SetSecretText;
    property StartDateTime: TDateTime read FStartDateTime
      write SetStartDateTime;
    property EndDateTime: TDateTime read FEndDateTime write SetEndDateTime;
  end;

  TLZMSGraphPasswordCredentials = class
    (TLZMSGraphModelList<TLZMSGraphPasswordCredential>)
  public
    function Find(AID: string): TLZMSGraphPasswordCredential;
  end;

  TLZMSGraphApplication = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FCreatedDateTime: TDateTime;
    FID: string;
    FDescription: string;
    FPasswordCredentials: TLZMSGraphPasswordCredentials;
    procedure SetCreatedDateTime(const Value: TDateTime);
    procedure SetDescription(const Value: string);
    procedure SetDisplayName(const Value: string);
    procedure SetID(const Value: string);
    function GetPasswordCredentials: TLZMSGraphPasswordCredentials;
  protected
    procedure AfterModelCreated; override;
    procedure BeforeModelDestroyed; override;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreatedDateTime;
    property ID: string read FID write SetID;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property Description: string read FDescription write SetDescription;
    property PasswordCredentials: TLZMSGraphPasswordCredentials
      read GetPasswordCredentials;
  end;

  TLZMSGraphApplications = class(TLZMSGraphModelList<TLZMSGraphApplication>)
  public
    function Find(AID: string): TLZMSGraphApplication;
  end;

  TLZMSGraphSite = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FID: string;
    FIsPersonalSite: boolean;
    FWebURL: string;
    procedure SetDisplayName(const Value: string);
    procedure SetID(const Value: string);
    procedure SetIsPersonalSite(const Value: boolean);
    procedure SetWebURL(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property ID: string read FID write SetID;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property WebURL: string read FWebURL write SetWebURL;
    property IsPersonalSite: boolean read FIsPersonalSite
      write SetIsPersonalSite;
  end;

  TLZMSGraphSites = class(TLZMSGraphModelList<TLZMSGraphSite>)
  public
    function Find(AID: string): TLZMSGraphSite;
  end;

  TLZMSGraphDrive = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FCreatedDateTime: TDateTime;
    FDriveType: string;
    FWebURL: string;
    FQuotaState: string;
    FQuotaRemaining: Int64;
    FID: string;
    FQuotaTotal: Int64;
    FDescription: string;
    FQuotaUsed: Int64;
    FLastModifiedDateTime: TDateTime;
    procedure SetCreatedDateTime(const Value: TDateTime);
    procedure SetDescription(const Value: string);
    procedure SetDisplayName(const Value: string);
    procedure SetDriveType(const Value: string);
    procedure SetID(const Value: string);
    procedure SetLastModifiedDateTime(const Value: TDateTime);
    procedure SetQuotaRemaining(const Value: Int64);
    procedure SetQuotaState(const Value: string);
    procedure SetQuotaTotal(const Value: Int64);
    procedure SetQuotaUsed(const Value: Int64);
    procedure SetWebURL(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property ID: string read FID write SetID;
    property Description: string read FDescription write SetDescription;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreatedDateTime;
    property LastModifiedDateTime: TDateTime read FLastModifiedDateTime
      write SetLastModifiedDateTime;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property WebURL: string read FWebURL write SetWebURL;
    property DriveType: string read FDriveType write SetDriveType;
    property QuotaRemaining: Int64 read FQuotaRemaining write SetQuotaRemaining;
    property QuotaState: string read FQuotaState write SetQuotaState;
    property QuotaTotal: Int64 read FQuotaTotal write SetQuotaTotal;
    property QuotaUsed: Int64 read FQuotaUsed write SetQuotaUsed;

  end;

  TLZMSGraphDrives = class(TLZMSGraphModelList<TLZMSGraphDrive>)
  public
    function Find(AID: string): TLZMSGraphDrive;
  end;

  TLZMSGraphDriveItem = class(TLZMSGraphModel)
  private
    FDisplayName: string;
    FWebURL: string;
    FID: string;
    FSize: Int64;
    FCreatedDateTime: TDateTime;
    FLastModifiedDateTime: TDateTime;
    procedure SetCreatedDateTime(const Value: TDateTime);
    procedure SetDisplayName(const Value: string);
    procedure SetID(const Value: string);
    procedure SetLastModifiedDateTime(const Value: TDateTime);
    procedure SetSize(const Value: Int64);
    procedure SetWebURL(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property ID: string read FID write SetID;
    property DisplayName: string read FDisplayName write SetDisplayName;
    property WebURL: string read FWebURL write SetWebURL;
    property CreatedDateTime: TDateTime read FCreatedDateTime
      write SetCreatedDateTime;
    property LastModifiedDateTime: TDateTime read FLastModifiedDateTime
      write SetLastModifiedDateTime;
    property Size: Int64 read FSize write SetSize;
  end;

  TLZMSGraphDriveItems = class(TLZMSGraphModelList<TLZMSGraphDriveItem>)
  public
    function Find(AID: string): TLZMSGraphDriveItem;
  end;

implementation

uses
  Lazy.Utils, System.StrUtils, Lazy.REST.Types, Lazy.ISO8601, Lazy.Token;

{ TLZMSGraphUser }

procedure TLZMSGraphUser.FromJSONValue(AJSONValue: TJSONValue);
var
  LProxyAddresses: TJSONArray;
  LProxyAddress: TJSONValue;
  LEmailAddress, LProxyAddressList: string;
begin
  inherited;
  FID := AJSONValue.GetValue<string>('id');
  FUserPrincipalName := AJSONValue.GetValue<string>('userPrincipalName', '');
  FDisplayName := AJSONValue.GetValue<string>('displayName', '');

  FCompanyName := AJSONValue.GetValue<string>('companyName', '');
  FDepartment := AJSONValue.GetValue<string>('department', '');
  FJobTitle := AJSONValue.GetValue<string>('jobTitle', '');
  FMobilePhone := AJSONValue.GetValue<string>('mobilePhone', '');

  FCreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  FSignInActivity := AJSONValue.GetValue<TDateTime>('signInActivity', 0);

  FAccountEnabled := AJSONValue.GetValue<boolean>('accountEnabled', false);

  FUserType := AJSONValue.GetValue<string>('userType', '');

  FMail := AJSONValue.GetValue<string>('mail', '');
  LProxyAddresses := AJSONValue.FindValue('proxyAddresses') as TJSONArray;
  if Assigned(LProxyAddresses) then
  begin
    for LProxyAddress in LProxyAddresses do
    begin
      if not TLZString.IsEmptyString(LProxyAddressList) then
      begin
        LProxyAddressList := LProxyAddressList + ',';
      end;
      LEmailAddress := LProxyAddress.AsType<string>;
      LEmailAddress := AnsiDequotedStr(LEmailAddress, '"');
      LProxyAddressList := LProxyAddressList + LEmailAddress;

      if Pos('SMTP:', LEmailAddress) > 0 then
      begin
        FMail := StringReplace(LEmailAddress, 'SMTP:', '', []);
      end;
    end;
  end;
end;

function TLZMSGraphUser.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
  LJSONArray: TJSONArray;
  LToken: TLZToken;
  LIdx: integer;
begin
  LJSONObject := TJSONObject.Create;
  try
    LJSONObject.AddPair('id', FID);
    
    if not TLZString.IsEmptyString(FUserPrincipalName) then
      LJSONObject.AddPair('userPrincipalName', FUserPrincipalName);
    
    if not TLZString.IsEmptyString(FDisplayName) then
      LJSONObject.AddPair('displayName', FDisplayName);
    
    if not TLZString.IsEmptyString(FCompanyName) then
      LJSONObject.AddPair('companyName', FCompanyName);
    
    if not TLZString.IsEmptyString(FDepartment) then
      LJSONObject.AddPair('department', FDepartment);
    
    if not TLZString.IsEmptyString(FJobTitle) then
      LJSONObject.AddPair('jobTitle', FJobTitle);
    
    if not TLZString.IsEmptyString(FMobilePhone) then
      LJSONObject.AddPair('mobilePhone', FMobilePhone);
    
    if FCreatedDateTime <> 0 then
      LJSONObject.AddPair('createdDateTime', TLZIso8601.DateTimeToIso8601(FCreatedDateTime, true));
    
    if FSignInActivity <> 0 then
      LJSONObject.AddPair('signInActivity', TLZIso8601.DateTimeToIso8601(FSignInActivity, true));
    
    LJSONObject.AddPair('accountEnabled', TJSONBool.Create(FAccountEnabled));
    
    if not TLZString.IsEmptyString(FUserType) then
      LJSONObject.AddPair('userType', FUserType);
    
    if not TLZString.IsEmptyString(FMail) then
      LJSONObject.AddPair('mail', FMail);
    
    // Convert delimited proxy addresses back to JSON array
    if not TLZString.IsEmptyString(FProxyAddresses) then
    begin
      LJSONArray := TJSONArray.Create;
      LToken := TLZToken.Create(FProxyAddresses, ',');
      try
        for LIdx := 0 to LToken.Count - 1 do
        begin
          LJSONArray.Add(LToken.Tokens[LIdx]);
        end;
      finally
        LToken.Free;
      end;
      LJSONObject.AddPair('proxyAddresses', LJSONArray);
    end;
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZMSGraphUser.SetAccountEnabled(const Value: boolean);
begin
  FAccountEnabled := Value;
end;

procedure TLZMSGraphUser.SetCompanyName(const Value: string);
begin
  FCompanyName := Value;
end;

procedure TLZMSGraphUser.SetCreatedate(const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphUser.SetDepartment(const Value: string);
begin
  FDepartment := Value;
end;

procedure TLZMSGraphUser.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphUser.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphUser.SetJobTitle(const Value: string);
begin
  FJobTitle := Value;
end;

procedure TLZMSGraphUser.SetMail(const Value: string);
begin
  FMail := Value;
end;

procedure TLZMSGraphUser.SetMobilePhone(const Value: string);
begin
  FMobilePhone := Value;
end;

procedure TLZMSGraphUser.SetProxyAddresses(const Value: string);
begin
  FProxyAddresses := Value;
end;

procedure TLZMSGraphUser.SetSignInActivity(const Value: TDateTime);
begin
  FSignInActivity := Value;
end;

procedure TLZMSGraphUser.SetuserPrincipalName(const Value: string);
begin
  FUserPrincipalName := Value;
end;

procedure TLZMSGraphUser.SetUserType(const Value: string);
begin
  FUserType := Value;
end;

{ TLZMSGraphUsers }

function TLZMSGraphUsers.Find(AID: string): TLZMSGraphUser;
var
  LItem: TLZMSGraphUser;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphLicenseDetail }

procedure TLZMSGraphLicenseDetail.FromJSONValue(AJSONValue: TJSONValue);
begin
  FID := AJSONValue.GetValue<string>('id');
  FSKUID := AJSONValue.GetValue<string>('skuId');
  FSKUPartNumber := AJSONValue.GetValue<string>('skuPartNumber');
end;

function TLZMSGraphLicenseDetail.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    LJSONObject.AddPair('id', FID);
    LJSONObject.AddPair('skuId', FSKUID);
    LJSONObject.AddPair('skuPartNumber', FSKUPartNumber);
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZMSGraphLicenseDetail.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphLicenseDetail.SetSKUID(const Value: string);
begin
  FSKUID := Value;
end;

procedure TLZMSGraphLicenseDetail.SetSKUPartNumber(const Value: string);
begin
  FSKUPartNumber := Value;
end;

{ TLZMSGraphCompanySubscription }

procedure TLZMSGraphCompanySubscription.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  ID := AJSONValue.GetValue<string>('id');
  CreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  NextLifecycleDateTime := AJSONValue.GetValue<TDateTime>
    ('nextLifecycleDateTime', 0);
  CommerceSubscriptionId := AJSONValue.GetValue<string>
    ('commerceSubscriptionId', '');
  IsTrial := AJSONValue.GetValue<boolean>('isTrial', false);
  SKUID := AJSONValue.GetValue<string>('skuId', '');
  SKUPartNumber := AJSONValue.GetValue<string>('skuPartNumber', '');
  Status := AJSONValue.GetValue<string>('status', '');
  TotalLicenses := AJSONValue.GetValue<integer>('totalLicenses', 0);
  OwnerID := AJSONValue.GetValue<string>('ownerId', '');
  OwnerTenantID := AJSONValue.GetValue<string>('ownerTenantId', '');
  OwnerType := AJSONValue.GetValue<string>('ownerType', '');
end;

function TLZMSGraphCompanySubscription.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    LJSONObject.AddPair('id', FID);
    
    if FCreatedDateTime <> 0 then
      LJSONObject.AddPair('createdDateTime', TLZIso8601.DateTimeToIso8601(FCreatedDateTime, true));
    
    if FNextLifecycleDateTime <> 0 then
      LJSONObject.AddPair('nextLifecycleDateTime', TLZIso8601.DateTimeToIso8601(FNextLifecycleDateTime, true));
    
    if not TLZString.IsEmptyString(FCommerceSubscriptionId) then
      LJSONObject.AddPair('commerceSubscriptionId', FCommerceSubscriptionId);
    
    LJSONObject.AddPair('isTrial', TJSONBool.Create(FIsTrial));
    
    if not TLZString.IsEmptyString(FSKUID) then
      LJSONObject.AddPair('skuId', FSKUID);
    
    if not TLZString.IsEmptyString(FSKUPartNumber) then
      LJSONObject.AddPair('skuPartNumber', FSKUPartNumber);
    
    if not TLZString.IsEmptyString(FStatus) then
      LJSONObject.AddPair('status', FStatus);
    
    LJSONObject.AddPair('totalLicenses', TJSONNumber.Create(FTotalLiceses));
    
    if not TLZString.IsEmptyString(FOwnerID) then
      LJSONObject.AddPair('ownerId', FOwnerID);
    
    if not TLZString.IsEmptyString(FOwnerTenantID) then
      LJSONObject.AddPair('ownerTenantId', FOwnerTenantID);
    
    if not TLZString.IsEmptyString(FOwnerType) then
      LJSONObject.AddPair('ownerType', FOwnerType);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZMSGraphCompanySubscription.SetCommerceSubscriptionId
  (const Value: string);
begin
  FCommerceSubscriptionId := Value;
end;

procedure TLZMSGraphCompanySubscription.SetCreateDateTime
  (const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphCompanySubscription.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphCompanySubscription.SetIsTrial(const Value: boolean);
begin
  FIsTrial := Value;
end;

procedure TLZMSGraphCompanySubscription.SetNextLifecycleDateTime
  (const Value: TDateTime);
begin
  FNextLifecycleDateTime := Value;
end;

procedure TLZMSGraphCompanySubscription.SetOwnerID(const Value: string);
begin
  FOwnerID := Value;
end;

procedure TLZMSGraphCompanySubscription.SetOwnerTenantID(const Value: string);
begin
  FOwnerTenantID := Value;
end;

procedure TLZMSGraphCompanySubscription.SetOwnerType(const Value: string);
begin
  FOwnerType := Value;
end;

procedure TLZMSGraphCompanySubscription.SetSKUID(const Value: string);
begin
  FSKUID := Value;
end;

procedure TLZMSGraphCompanySubscription.SetSKUPartNumber(const Value: string);
begin
  FSKUPartNumber := Value;
end;

procedure TLZMSGraphCompanySubscription.SetStatus(const Value: string);
begin
  FStatus := Value;
end;

procedure TLZMSGraphCompanySubscription.SetTotalLiceses(const Value: integer);
begin
  FTotalLiceses := Value;
end;

{ TLZMSGraphMailFolder }

procedure TLZMSGraphMailFolder.FromJSONValue(AJSONValue: TJSONValue);
begin
  ID := AJSONValue.GetValue<string>('id');
  DisplayName := AJSONValue.GetValue<string>('displayName', '');
  ParentFolderID := AJSONValue.GetValue<string>('parentFolderId', '');
  ChildFolderCount := AJSONValue.GetValue<integer>('childFolderCount', 0);
  UnreadItemCount := AJSONValue.GetValue<integer>('unreadItemCount', 0);
  TotalItemCount := AJSONValue.GetValue<integer>('totalItemCount', 0);
  IsHidden := AJSONValue.GetValue<boolean>('isHidden', false);
end;

procedure TLZMSGraphMailFolder.SetChildFolderCount(const Value: integer);
begin
  FChildFolderCount := Value;
end;

procedure TLZMSGraphMailFolder.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphMailFolder.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphMailFolder.SetIsHidden(const Value: boolean);
begin
  FIsHidden := Value;
end;

procedure TLZMSGraphMailFolder.SetParentFolderID(const Value: string);
begin
  FParentFolderID := Value;
end;

procedure TLZMSGraphMailFolder.SetTotalItemCount(const Value: integer);
begin
  FTotalItemCount := Value;
end;

procedure TLZMSGraphMailFolder.SetUnreadItemCount(const Value: integer);
begin
  FUnreadItemCount := Value;
end;

{ TLZMSGraphDomain }

procedure TLZMSGraphDomain.FromJSONValue(AJSONValue: TJSONValue);
begin
  ID := AJSONValue.GetValue<string>('id');
  AuthenticationType := AJSONValue.GetValue<string>('authenticationType');
  AvailabilityStatus := AJSONValue.GetValue<string>('availabilityStatus');
  IsAdminManaged := AJSONValue.GetValue<boolean>('isAdminManaged');
  IsDefault := AJSONValue.GetValue<boolean>('isDefault');
  IsInitial := AJSONValue.GetValue<boolean>('isInitial');
  IsRoot := AJSONValue.GetValue<boolean>('isRoot');
  SupportedServices := JSONArrayToDelimitedString
    (AJSONValue.FindValue('proxyAddresses') as TJSONArray);
end;

function TLZMSGraphDomain.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
  LJSONArray: TJSONArray;
  LToken: TLZToken;
  LIdx: integer;
begin
  LJSONObject := TJSONObject.Create;
  try
    LJSONObject.AddPair('id', FID);
    LJSONObject.AddPair('authenticationType', FAuthenticationType);
    LJSONObject.AddPair('availabilityStatus', FAvailabilityStatus);
    LJSONObject.AddPair('isAdminManaged', TJSONBool.Create(FIsAdminManaged));
    LJSONObject.AddPair('isDefault', TJSONBool.Create(FIsDefault));
    LJSONObject.AddPair('isInitial', TJSONBool.Create(FIsInitial));
    LJSONObject.AddPair('isRoot', TJSONBool.Create(FIsRoot));
    
    // Convert delimited string back to JSON array
    if not TLZString.IsEmptyString(FSupportedServices) then
    begin
      LJSONArray := TJSONArray.Create;
      LToken := TLZToken.Create(FSupportedServices, ',');
      try
        for LIdx := 0 to LToken.Count - 1 do
        begin
          LJSONArray.Add(LToken.Tokens[LIdx]);
        end;
      finally
        LToken.Free;
      end;
      LJSONObject.AddPair('proxyAddresses', LJSONArray);
    end;
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

procedure TLZMSGraphDomain.SetAuthenticationType(const Value: string);
begin
  FAuthenticationType := Value;
end;

procedure TLZMSGraphDomain.SetAvailabilityStatus(const Value: string);
begin
  FAvailabilityStatus := Value;
end;

procedure TLZMSGraphDomain.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphDomain.SetIsAdminManaged(const Value: boolean);
begin
  FIsAdminManaged := Value;
end;

procedure TLZMSGraphDomain.SetIsDefault(const Value: boolean);
begin
  FIsDefault := Value;
end;

procedure TLZMSGraphDomain.SetIsInitial(const Value: boolean);
begin
  FIsInitial := Value;
end;

procedure TLZMSGraphDomain.SetIsRoot(const Value: boolean);
begin
  FIsRoot := Value;
end;

procedure TLZMSGraphDomain.SetSupportedServices(const Value: string);
begin
  FSupportedServices := Value;
end;

{ TLZMSGraphMailFolders }

function TLZMSGraphMailFolders.Find(AID: string): TLZMSGraphMailFolder;
var
  LItem: TLZMSGraphMailFolder;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if LItem.ID = AID then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphGroup }

procedure TLZMSGraphGroup.FromJSONValue(AJSONValue: TJSONValue);
var
  LProxyAddresses: TJSONArray;
  LProxyAddress: TJSONValue;
  LEmailAddress, LProxyAddressList: string;
begin
  FID := AJSONValue.GetValue<string>('id');
  FDescription := AJSONValue.GetValue<string>('description', '');
  FDisplayName := AJSONValue.GetValue<string>('displayName', '');
  FCreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  FMail := AJSONValue.GetValue<string>('mail', '');
  FMailEnabled := AJSONValue.GetValue<boolean>('mailEnabled', false);
  FSecurityEnabled := AJSONValue.GetValue<boolean>('securityEnabled', false);
  FVisibility := AJSONValue.GetValue<string>('visibility', '');

  LProxyAddresses := AJSONValue.FindValue('proxyAddresses') as TJSONArray;
  if Assigned(LProxyAddresses) then
  begin
    for LProxyAddress in LProxyAddresses do
    begin
      if not TLZString.IsEmptyString(LProxyAddressList) then
      begin
        LProxyAddressList := LProxyAddressList + ',';
      end;
      LEmailAddress := LProxyAddress.AsType<string>;
      LEmailAddress := AnsiDequotedStr(LEmailAddress, '"');
      LProxyAddressList := LProxyAddressList + LEmailAddress;

      if Pos('SMTP:', LEmailAddress) > 0 then
      begin
        FMail := StringReplace(LEmailAddress, 'SMTP:', '', []);
      end;
    end;
  end;
end;

procedure TLZMSGraphGroup.SetCreatedDateTime(const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphGroup.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TLZMSGraphGroup.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphGroup.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphGroup.SetMail(const Value: string);
begin
  FMail := Value;
end;

procedure TLZMSGraphGroup.SetMailEnabled(const Value: boolean);
begin
  FMailEnabled := Value;
end;

procedure TLZMSGraphGroup.SetProxyAddresses(const Value: string);
begin
  FProxyAddresses := Value;
end;

procedure TLZMSGraphGroup.SetSecurityEnabled(const Value: boolean);
begin
  FSecurityEnabled := Value;
end;

procedure TLZMSGraphGroup.SetVisibility(const Value: string);
begin
  FVisibility := Value;
end;

{ TLZMSGraphGroups }

function TLZMSGraphGroups.Find(AID: string): TLZMSGraphGroup;
var
  LItem: TLZMSGraphGroup;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphGroupMember }

procedure TLZMSGraphGroupMember.FromJSONValue(AJSONValue: TJSONValue);
begin
  FID := AJSONValue.GetValue<string>('id');
end;

procedure TLZMSGraphGroupMember.SetID(const Value: string);
begin
  FID := Value;
end;

{ TLZMSGraphGroupMembers }

function TLZMSGraphGroupMembers.Find(AID: string): TLZMSGraphGroupMember;
var
  LItem: TLZMSGraphGroupMember;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod }

procedure TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.FromJSONValue
  (AJSONValue: TJSONValue);
begin
  FID := AJSONValue.GetValue<string>('id');
  FDisplayName := AJSONValue.GetValue<string>('displayName', '');
  FCreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  FDeviceTag := AJSONValue.GetValue<string>('deviceTag', '');
  FPhoneAppVersion := AJSONValue.GetValue<string>('phoneAppVersion', '');
end;

function TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FID) then
      LJSONObject.AddPair('id', FID);
    
    if not TLZString.IsEmptyString(FDisplayName) then
      LJSONObject.AddPair('displayName', FDisplayName);
    
    if FCreatedDateTime <> 0 then
      LJSONObject.AddPair('createdDateTime', TLZIso8601.DateTimeToIso8601(FCreatedDateTime, true));
    
    if not TLZString.IsEmptyString(FDeviceTag) then
      LJSONObject.AddPair('deviceTag', FDeviceTag);
    
    if not TLZString.IsEmptyString(FPhoneAppVersion) then
      LJSONObject.AddPair('phoneAppVersion', FPhoneAppVersion);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

function TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.
  GetDetails: string;
begin
  Result := Format('%s - %s - %s', [DisplayName, FDeviceTag, FPhoneAppVersion]);
end;

class function TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.
  GetResourceType: string;
begin
  Result := 'microsoftAuthenticatorAuthenticationMethod';
end;

procedure TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.
  SetCreatedDateTime(const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.SetDeviceTag
  (const Value: string);
begin
  FDeviceTag := Value;
end;

procedure TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.
  SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.
  SetPhoneAppVersion(const Value: string);
begin
  FPhoneAppVersion := Value;
end;

{ TLZMSGraphUserSoftwareOathAuthenticationMethod }

procedure TLZMSGraphUserSoftwareOathAuthenticationMethod.FromJSONValue
  (AJSONValue: TJSONValue);
begin
  FID := AJSONValue.GetValue<string>('id');
end;

function TLZMSGraphUserSoftwareOathAuthenticationMethod.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FID) then
      LJSONObject.AddPair('id', FID);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

class function TLZMSGraphUserSoftwareOathAuthenticationMethod.
  GetResourceType: string;
begin
  Result := 'softwareOathAuthenticationMethod';
end;

{ TLZMSGraphUserPhoneAuthenticationMethod }

procedure TLZMSGraphUserPhoneAuthenticationMethod.FromJSONValue
  (AJSONValue: TJSONValue);
begin
  FID := AJSONValue.GetValue<string>('id');
  FPhoneNumber := AJSONValue.GetValue<string>('phoneNumber');
  FPhoneType := AJSONValue.GetValue<string>('phoneType');
  FSMSSignInState := AJSONValue.GetValue<string>('smsSignInState');
end;

function TLZMSGraphUserPhoneAuthenticationMethod.ToJSONValue: TJSONValue;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    if not TLZString.IsEmptyString(FID) then
      LJSONObject.AddPair('id', FID);
    
    if not TLZString.IsEmptyString(FPhoneNumber) then
      LJSONObject.AddPair('phoneNumber', FPhoneNumber);
    
    if not TLZString.IsEmptyString(FPhoneType) then
      LJSONObject.AddPair('phoneType', FPhoneType);
    
    if not TLZString.IsEmptyString(FSMSSignInState) then
      LJSONObject.AddPair('smsSignInState', FSMSSignInState);
    
    Result := LJSONObject;
  except
    LJSONObject.Free;
    raise;
  end;
end;

function TLZMSGraphUserPhoneAuthenticationMethod.GetDetails: string;
begin
  Result := Format('%s - %s - %s', [FPhoneNumber, FPhoneType, FSMSSignInState]);
end;

class function TLZMSGraphUserPhoneAuthenticationMethod.GetResourceType: string;
begin
  Result := 'phoneAuthenticationMethod';
end;

procedure TLZMSGraphUserPhoneAuthenticationMethod.SetPhoneNumber
  (const Value: string);
begin
  FPhoneNumber := Value;
end;

procedure TLZMSGraphUserPhoneAuthenticationMethod.SetPhoneType
  (const Value: string);
begin
  FPhoneType := Value;
end;

procedure TLZMSGraphUserPhoneAuthenticationMethod.SetSMSSignInState
  (const Value: string);
begin
  FSMSSignInState := Value;
end;

{ TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethods }

function TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethods.Find
  (AID: string): TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod;
var
  LItem: TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphUserSoftwareOathAuthenticationMethods }

function TLZMSGraphUserSoftwareOathAuthenticationMethods.Find(AID: string)
  : TLZMSGraphUserSoftwareOathAuthenticationMethod;
var
  LItem: TLZMSGraphUserSoftwareOathAuthenticationMethod;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphUserPhoneAuthenticationMethods }

function TLZMSGraphUserPhoneAuthenticationMethods.Find(AID: string)
  : TLZMSGraphUserPhoneAuthenticationMethod;
var
  LItem: TLZMSGraphUserPhoneAuthenticationMethod;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphMailRule }

procedure TLZMSGraphMailRule.FromJSONValue(AJSONValue: TJSONValue);
var
  LAction: TJSONValue;
  LForwards: TJSONArray;
  LForward: TJSONValue;
begin
  FID := AJSONValue.GetValue<string>('id');
  FDisplayName := AJSONValue.GetValue<string>('displayName');
  FEnabled := AJSONValue.GetValue<boolean>('isEnabled');
  FHasError := AJSONValue.GetValue<boolean>('hasError');
  LAction := AJSONValue.FindValue('actions');
  if Assigned(LAction) then
  begin

    FMoveToFolder := LAction.GetValue<string>('moveToFolder');

    LForwards := LAction.FindValue('forwardTo') as TJSONArray;
    if Assigned(LForwards) then
    begin
      FForwardTo := '';
      for LForward in LForwards do
      begin
        if Trim(FForwardTo) <> '' then
          FForwardTo := FForwardTo + ',';
        FForwardTo := FForwardTo + LForward.FindValue('emailAddress')
          .GetValue<string>('address');
      end;
    end;
  end;

end;

procedure TLZMSGraphMailRule.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphMailRule.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TLZMSGraphMailRule.SetForwardTo(const Value: string);
begin
  FForwardTo := Value;
end;

procedure TLZMSGraphMailRule.SetHasError(const Value: boolean);
begin
  FHasError := Value;
end;

procedure TLZMSGraphMailRule.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphMailRule.SetMoveToFolder(const Value: string);
begin
  FMoveToFolder := Value;
end;

{ TLZMSGraphMailRules }

function TLZMSGraphMailRules.Find(AID: string): TLZMSGraphMailRule;
var
  LItem: TLZMSGraphMailRule;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphUserPasswordAuthenticationMethod }

procedure TLZMSGraphUserPasswordAuthenticationMethod.FromJSONValue
  (AJSONValue: TJSONValue);
begin
  FID := AJSONValue.GetValue<string>('id');
end;

class function TLZMSGraphUserPasswordAuthenticationMethod.
  GetResourceType: string;
begin
  Result := 'passwordAuthenticationMethod';
end;

{ TLZMSGraphUserPasswordAuthenticationMethods }

function TLZMSGraphUserPasswordAuthenticationMethods.Find(AID: string)
  : TLZMSGraphUserPasswordAuthenticationMethod;
var
  LItem: TLZMSGraphUserPasswordAuthenticationMethod;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphAuditSignIn }

procedure TLZMSGraphAuditSignIn.FromJSONValue(AJSONValue: TJSONValue);
begin
  ID := AJSONValue.GetValue<string>('id', '');
  CreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  UserPrincipalName := AJSONValue.GetValue<string>('userPrincipalName', '');
  UserDisplayName := AJSONValue.GetValue<string>('userDisplayName', '');
  UserID := AJSONValue.GetValue<string>('userId', '');
  AppID := AJSONValue.GetValue<string>('appId', '');
  AppDisplayName := AJSONValue.GetValue<string>('appDisplayName', '');
  IPAddress := AJSONValue.GetValue<string>('ipAddress', '');
  ClientAppUsed := AJSONValue.GetValue<string>('clientAppUsed', '');
  IsInteractive := AJSONValue.GetValue<boolean>('isInteractive', false);
  RiskDetail := AJSONValue.GetValue<string>('riskDetail', '');
  RiskLevelAggregated := AJSONValue.GetValue<string>('riskLevelAggregated', '');
  RiskLevelDuringSignIn := AJSONValue.GetValue<string>
    ('riskLevelDuringSignIn', '');
  RiskState := AJSONValue.GetValue<string>('riskState', '');
  ResourceDisplayName := AJSONValue.GetValue<string>('resourceDisplayName', '');
  ResourceID := AJSONValue.GetValue<string>('resourceId', '');

  StatusCode := AJSONValue.GetValue<integer>('status.errorCode', -1);
  StatusFailureReason := AJSONValue.GetValue<string>
    ('status.failureReason', '');
  StatusAdditionalDetails := AJSONValue.GetValue<string>
    ('status.additionalDetails', '');

  LocationCity := AJSONValue.GetValue<string>('location.city', '');
  LocationState := AJSONValue.GetValue<string>('location.state', '');
  LocationCountryOrRegion := AJSONValue.GetValue<string>
    ('location.countryOrRegion', '');
  LocationLatitude := AJSONValue.GetValue<double>
    ('location.geoCoordinates.latitude', 0);
  LocationLongitude := AJSONValue.GetValue<double>
    ('location.geoCoordinates.longitude', 0);

  DeviceID := AJSONValue.GetValue<string>('deviceDetail.deviceId', '');
  DeviceDisplayName := AJSONValue.GetValue<string>
    ('deviceDetail.displayName', '');
  DeviceOperatingSystem := AJSONValue.GetValue<string>
    ('deviceDetail.operatingSystem', '');
  DeviceBrowser := AJSONValue.GetValue<string>('deviceDetail.browser', '');
  DeviceIsCompliant := AJSONValue.GetValue<boolean>
    ('deviceDetail.isCompliant', false);
  DeviceIsManaged := AJSONValue.GetValue<boolean>
    ('deviceDetail.isManaged', false);
  DeviceTrustType := AJSONValue.GetValue<string>('deviceDetail.trustType', '');

end;

procedure TLZMSGraphAuditSignIn.SetAppDisplayName(const Value: string);
begin
  FAppDisplayName := Value;
end;

procedure TLZMSGraphAuditSignIn.SetAppID(const Value: string);
begin
  FAppID := Value;
end;

procedure TLZMSGraphAuditSignIn.SetClientAppUsed(const Value: string);
begin
  FClientAppUsed := Value;
end;

procedure TLZMSGraphAuditSignIn.SetConditionalAccessStatus(const Value: string);
begin
  FConditionalAccessStatus := Value;
end;

procedure TLZMSGraphAuditSignIn.SetCorrelationID(const Value: string);
begin
  FCorrelationID := Value;
end;

procedure TLZMSGraphAuditSignIn.SetCreatedDateTime(const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphAuditSignIn.SetDeviceBrowser(const Value: string);
begin
  FDeviceBrowser := Value;
end;

procedure TLZMSGraphAuditSignIn.SetDeviceDisplayName(const Value: string);
begin
  FDeviceDisplayName := Value;
end;

procedure TLZMSGraphAuditSignIn.SetDeviceID(const Value: string);
begin
  FDeviceID := Value;
end;

procedure TLZMSGraphAuditSignIn.SetDeviceIsCompliant(const Value: boolean);
begin
  FDeviceIsCompliant := Value;
end;

procedure TLZMSGraphAuditSignIn.SetDeviceIsManaged(const Value: boolean);
begin
  FDeviceIsManaged := Value;
end;

procedure TLZMSGraphAuditSignIn.SetDeviceOperatingSystem(const Value: string);
begin
  FDeviceOperatingSystem := Value;
end;

procedure TLZMSGraphAuditSignIn.SetDeviceTrustType(const Value: string);
begin
  FDeviceTrustType := Value;
end;

procedure TLZMSGraphAuditSignIn.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphAuditSignIn.SetIPAddress(const Value: string);
begin
  FIPAddress := Value;
end;

procedure TLZMSGraphAuditSignIn.SetIsInteractive(const Value: boolean);
begin
  FIsInteractive := Value;
end;

procedure TLZMSGraphAuditSignIn.SetLocationCity(const Value: string);
begin
  FLocationCity := Value;
end;

procedure TLZMSGraphAuditSignIn.SetLocationCountryOrRegion(const Value: string);
begin
  FLocationCountryOrRegion := Value;
end;

procedure TLZMSGraphAuditSignIn.SetLocationLatitude(const Value: double);
begin
  FLocationLatitude := Value;
end;

procedure TLZMSGraphAuditSignIn.SetLocationLongitude(const Value: double);
begin
  FLocationLongitude := Value;
end;

procedure TLZMSGraphAuditSignIn.SetLocationState(const Value: string);
begin
  FLocationState := Value;
end;

procedure TLZMSGraphAuditSignIn.SetResourceDisplayName(const Value: string);
begin
  FResourceDisplayName := Value;
end;

procedure TLZMSGraphAuditSignIn.SetResourceID(const Value: string);
begin
  FResourceID := Value;
end;

procedure TLZMSGraphAuditSignIn.SetRiskDetail(const Value: string);
begin
  FRiskDetail := Value;
end;

procedure TLZMSGraphAuditSignIn.SetRiskLevelAggregated(const Value: string);
begin
  FRiskLevelAggregated := Value;
end;

procedure TLZMSGraphAuditSignIn.SetRiskLevelDuringSignIn(const Value: string);
begin
  FRiskLevelDuringSignIn := Value;
end;

procedure TLZMSGraphAuditSignIn.SetRiskState(const Value: string);
begin
  FRiskState := Value;
end;

procedure TLZMSGraphAuditSignIn.SetStatusAdditionalDetails(const Value: string);
begin
  FStatusAdditionalDetails := Value;
end;

procedure TLZMSGraphAuditSignIn.SetStatusCode(const Value: integer);
begin
  FStatusCode := Value;
end;

procedure TLZMSGraphAuditSignIn.SetStatusFailureReason(const Value: string);
begin
  FStatusFailureReason := Value;
end;

procedure TLZMSGraphAuditSignIn.SetUserDisplayName(const Value: string);
begin
  FUserDisplayName := Value;
end;

procedure TLZMSGraphAuditSignIn.SetUserID(const Value: string);
begin
  FUserID := Value;
end;

procedure TLZMSGraphAuditSignIn.SetuserPrincipalName(const Value: string);
begin
  FUserPrincipalName := Value;
end;

{ TLZMSGraphAuditSignIns }

function TLZMSGraphAuditSignIns.Find(AID: string): TLZMSGraphAuditSignIn;
var
  LItem: TLZMSGraphAuditSignIn;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphApplication }

procedure TLZMSGraphApplication.AfterModelCreated;
begin
  FPasswordCredentials := TLZMSGraphPasswordCredentials.Create;
end;

procedure TLZMSGraphApplication.BeforeModelDestroyed;
begin
  FreeAndNil(FPasswordCredentials);
end;

procedure TLZMSGraphApplication.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  FPasswordCredentials.Clear;
  ID := AJSONValue.GetValue<string>('appId', '');
  DisplayName := AJSONValue.GetValue<string>('displayName', '');
  Description := AJSONValue.GetValue<string>('description', '');
  CreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  FPasswordCredentials.FromJSONValue(AJSONValue, true, 'passwordCredentials');
end;

function TLZMSGraphApplication.GetPasswordCredentials
  : TLZMSGraphPasswordCredentials;
begin
  Result := FPasswordCredentials;
end;

procedure TLZMSGraphApplication.SetCreatedDateTime(const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphApplication.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TLZMSGraphApplication.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphApplication.SetID(const Value: string);
begin
  FID := Value;
end;

{ TLZMSGraphPasswordCredential }

procedure TLZMSGraphPasswordCredential.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  KeyID := AJSONValue.GetValue<string>('keyId', '');
  StartDateTime := AJSONValue.GetValue<TDateTime>('startDateTime', 0);
  EndDateTime := AJSONValue.GetValue<TDateTime>('endDateTime', 0);
  Hint := AJSONValue.GetValue<string>('hint', '');
  SecretText := AJSONValue.GetValue<string>('secretText', '');
  DisplayName := AJSONValue.GetValue<string>('displayName', '');
end;

procedure TLZMSGraphPasswordCredential.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphPasswordCredential.SetEndDateTime(const Value: TDateTime);
begin
  FEndDateTime := Value;
end;

procedure TLZMSGraphPasswordCredential.SetHint(const Value: string);
begin
  FHint := Value;
end;

procedure TLZMSGraphPasswordCredential.SetKeyID(const Value: string);
begin
  FKeyID := Value;
end;

procedure TLZMSGraphPasswordCredential.SetSecretText(const Value: string);
begin
  FSecretText := Value;
end;

procedure TLZMSGraphPasswordCredential.SetStartDateTime(const Value: TDateTime);
begin
  FStartDateTime := Value;
end;

{ TLZMSGraphPasswordCredentials }

function TLZMSGraphPasswordCredentials.Find(AID: string)
  : TLZMSGraphPasswordCredential;
var
  LItem: TLZMSGraphPasswordCredential;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.KeyID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphApplications }

function TLZMSGraphApplications.Find(AID: string): TLZMSGraphApplication;
var
  LItem: TLZMSGraphApplication;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphUserAuthenticationMethods }

class function TLZMSGraphUserAuthenticationMethods.CreateAuthenticationModel
  (AResourceType: string): TLZMSGraphAuthenticationMethodBase;
begin
  Result := nil;
  if TLZMSGraphUserPhoneAuthenticationMethod.IsResourceType(AResourceType) then
  begin
    Result := TLZMSGraphUserPhoneAuthenticationMethod.Create;
  end;
  if TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.IsResourceType
    (AResourceType) then
  begin
    Result := TLZMSGraphUserMicrosoftAuthenticatorAuthenticationMethod.Create;
  end;
  if TLZMSGraphUserSoftwareOathAuthenticationMethod.IsResourceType(AResourceType)
  then
  begin
    Result := TLZMSGraphUserSoftwareOathAuthenticationMethod.Create;
  end;
end;

function TLZMSGraphUserAuthenticationMethods.Find(AID: string)
  : TLZMSGraphAuthenticationMethodBase;
var
  LItem: TLZMSGraphAuthenticationMethodBase;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

procedure TLZMSGraphUserAuthenticationMethods.FromJSONValue(
  AJSONValue: TJSONValue;
  AClear: boolean;
  AArrayName: string;
  AFreeJSONValue: boolean);
var
  LArray: TJSONArray;
  LIdx: integer;
  LModel: TLZMSGraphAuthenticationMethodBase;
  LArrayName: string;
  LResourceType: string;
begin
  LArrayName := AArrayName;
  if TLZString.IsEmptyString(LArrayName) then
    LArrayName := GetDefaultArrayName;
  if AClear then
    Clear;
  LArray := AJSONValue.GetValue<TJSONArray>(LArrayName, nil);
  if Assigned(LArray) then
  begin
    for LIdx := 0 to Pred(LArray.Count) do
    begin
      LResourceType := TLZOData.GetValue(LArray.Items[LIdx], '@odata.type');

      LModel := CreateAuthenticationModel(LResourceType);
      if Assigned(LModel) then
      begin
        LModel.FromJSONValue(LArray.Items[LIdx]);
        Add(LModel);
      end;
    end;
  end;
  if AFreeJSONValue then
  begin
    AJSONValue.Free;
  end;

end;

function TLZMSGraphUserAuthenticationMethods.GetModelClass
  (AResourceType: string): TLZMSGraphAuthenticationMethodClass;
begin
  Result := nil;
end;

{ TLZMSGraphAuthenticationMethodBase }

function TLZMSGraphAuthenticationMethodBase.GetDetails: string;
begin
  Result := '';
end;

class function TLZMSGraphAuthenticationMethodBase.IsResourceType
  (AValue: string): boolean;
var
  LResourceType: string;
begin
  LResourceType := GetResourceType;
  Result := SameText(LResourceType, AValue) or
    SameText('#microsoft.graph.' + LResourceType, AValue)
end;

procedure TLZMSGraphAuthenticationMethodBase.SetID(const Value: string);
begin
  FID := Value;
end;

{ TLZMSGraphSite }

procedure TLZMSGraphSite.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  ID := AJSONValue.GetValue<string>('id', '');
  DisplayName := AJSONValue.GetValue<string>('name', '');
  FIsPersonalSite := AJSONValue.GetValue<boolean>('isPersonalSite', false);
  FWebURL := AJSONValue.GetValue<string>('webUrl', '');
end;

procedure TLZMSGraphSite.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphSite.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphSite.SetIsPersonalSite(const Value: boolean);
begin
  FIsPersonalSite := Value;
end;

procedure TLZMSGraphSite.SetWebURL(const Value: string);
begin
  FWebURL := Value;
end;

{ TLZMSGraphSites }

function TLZMSGraphSites.Find(AID: string): TLZMSGraphSite;
var
  LItem: TLZMSGraphSite;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphDrive }

procedure TLZMSGraphDrive.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  FID := AJSONValue.GetValue<string>('id', '');
  FDisplayName := AJSONValue.GetValue<string>('name', '');
  FCreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  FLastModifiedDateTime := AJSONValue.GetValue<TDateTime>
    ('lastModifiedDateTime', 0);
  FWebURL := AJSONValue.GetValue<string>('webUrl', '');
  FDriveType := AJSONValue.GetValue<string>('driveType', '');
  FDescription := AJSONValue.GetValue<string>('description', '');
  FQuotaState := AJSONValue.GetValue<string>('quota.state', '');
  FQuotaRemaining := AJSONValue.GetValue<Int64>('quota.remaining', 0);
  FQuotaTotal := AJSONValue.GetValue<Int64>('quota.total', 0);
  FQuotaUsed := AJSONValue.GetValue<Int64>('quota.used', 0);
end;

procedure TLZMSGraphDrive.SetCreatedDateTime(const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphDrive.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TLZMSGraphDrive.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphDrive.SetDriveType(const Value: string);
begin
  FDriveType := Value;
end;

procedure TLZMSGraphDrive.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphDrive.SetLastModifiedDateTime(const Value: TDateTime);
begin
  FLastModifiedDateTime := Value;
end;

procedure TLZMSGraphDrive.SetQuotaRemaining(const Value: Int64);
begin
  FQuotaRemaining := Value;
end;

procedure TLZMSGraphDrive.SetQuotaState(const Value: string);
begin
  FQuotaState := Value;
end;

procedure TLZMSGraphDrive.SetQuotaTotal(const Value: Int64);
begin
  FQuotaTotal := Value;
end;

procedure TLZMSGraphDrive.SetQuotaUsed(const Value: Int64);
begin
  FQuotaUsed := Value;
end;

procedure TLZMSGraphDrive.SetWebURL(const Value: string);
begin
  FWebURL := Value;
end;

{ TLZMSGraphDrives }

function TLZMSGraphDrives.Find(AID: string): TLZMSGraphDrive;
var
  LItem: TLZMSGraphDrive;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

{ TLZMSGraphDriveItem }

procedure TLZMSGraphDriveItem.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  FID := AJSONValue.GetValue<string>('id', '');
  FDisplayName := AJSONValue.GetValue<string>('name', '');
  FCreatedDateTime := AJSONValue.GetValue<TDateTime>('createdDateTime', 0);
  FLastModifiedDateTime := AJSONValue.GetValue<TDateTime>
    ('lastModifiedDateTime', 0);
  FWebURL := AJSONValue.GetValue<string>('webUrl', '');
  FSize := AJSONValue.GetValue<Int64>('size', 0);
end;

procedure TLZMSGraphDriveItem.SetCreatedDateTime(const Value: TDateTime);
begin
  FCreatedDateTime := Value;
end;

procedure TLZMSGraphDriveItem.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TLZMSGraphDriveItem.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TLZMSGraphDriveItem.SetLastModifiedDateTime(const Value: TDateTime);
begin
  FLastModifiedDateTime := Value;
end;

procedure TLZMSGraphDriveItem.SetSize(const Value: Int64);
begin
  FSize := Value;
end;

procedure TLZMSGraphDriveItem.SetWebURL(const Value: string);
begin
  FWebURL := Value;
end;

{ TLZMSGraphDriveItems }

function TLZMSGraphDriveItems.Find(AID: string): TLZMSGraphDriveItem;
var
  LItem: TLZMSGraphDriveItem;
begin
  Result := nil;
  for LItem in Self.Models do
  begin
    if SameText(LItem.ID, AID) then
    begin
      Result := LItem;
      Exit;
    end;
  end;
end;

end.
