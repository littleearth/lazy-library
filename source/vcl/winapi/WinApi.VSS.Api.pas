{ Delphi VssAPI interface
  =======================
  Windows Volume Shadow Service (VSS)

  Converted from the Windows 7 C++ header files
  see: Windows SDFK 7.0 - Include\vss.h, vsbackup.h , vswriter.h

  � Dr. J. Rathlev, D-24222 Schwentinental (info(a)rathlev-home.de)

  The contents of this file may be used under the terms of the
  Mozilla Public License ("MPL") or
  GNU Lesser General Public License Version 2 or later (the "LGPL")

  Software distributed under this License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  November 2014
}

unit WinApi.Vss.API;

interface

uses WinApi.Windows, WinApi.MSXML, WinApi.ActiveX;

{$WARN SYMBOL_PLATFORM OFF}

const
{$EXTERNALSYM INFINITE}
  INFINITE = $FFFFFFFF; // Infinite timeout

{$Z4}        // use DWORD (4 bytes for enumerations)
{$ALIGN on}  // align records

type
  _VSS_OBJECT_TYPE = (VSS_OBJECT_UNKNOWN, VSS_OBJECT_NONE,
    VSS_OBJECT_SNAPSHOT_SET, VSS_OBJECT_SNAPSHOT, VSS_OBJECT_PROVIDER,
    VSS_OBJECT_TYPE_COUNT);

  VSS_OBJECT_TYPE = _VSS_OBJECT_TYPE;
  TVssObjectType = _VSS_OBJECT_TYPE;

  PVSS_OBJECT_TYPE = ^_VSS_OBJECT_TYPE;
  PVssObjectType = ^_VSS_OBJECT_TYPE;

  _VSS_SNAPSHOT_STATE = (VSS_SS_UNKNOWN, VSS_SS_PREPARING,
    VSS_SS_PROCESSING_PREPARE, VSS_SS_PREPARED, VSS_SS_PROCESSING_PRECOMMIT,
    VSS_SS_PRECOMMITTED, VSS_SS_PROCESSING_COMMIT, VSS_SS_COMMITTED,
    VSS_SS_PROCESSING_POSTCOMMIT, VSS_SS_PROCESSING_PREFINALCOMMIT,
    VSS_SS_PREFINALCOMMITTED, VSS_SS_PROCESSING_POSTFINALCOMMIT, VSS_SS_CREATED,
    VSS_SS_ABORTED, VSS_SS_DELETED, VSS_SS_POSTCOMMITTED);

  VSS_SNAPSHOT_STATE = _VSS_SNAPSHOT_STATE;
  TVssSnapshotState = _VSS_SNAPSHOT_STATE;

  PVSS_SNAPSHOT_STATE = ^_VSS_SNAPSHOT_STATE;
  PVssSnapshotState = ^_VSS_SNAPSHOT_STATE;

  _VSS_WRITER_STATE = (VSS_WS_UNKNOWN, VSS_WS_STABLE, VSS_WS_WAITING_FOR_FREEZE,
    VSS_WS_WAITING_FOR_THAW, VSS_WS_WAITING_FOR_POST_SNAPSHOT,
    VSS_WS_WAITING_FOR_BACKUP_COMPLETE, VSS_WS_FAILED_AT_IDENTIFY,
    VSS_WS_FAILED_AT_PREPARE_BACKUP, VSS_WS_FAILED_AT_PREPARE_SNAPSHOT,
    VSS_WS_FAILED_AT_FREEZE, VSS_WS_FAILED_AT_THAW,
    VSS_WS_FAILED_AT_POST_SNAPSHOT, VSS_WS_FAILED_AT_BACKUP_COMPLETE,
    VSS_WS_FAILED_AT_PRE_RESTORE, VSS_WS_FAILED_AT_POST_RESTORE,
    VSS_WS_FAILED_AT_BACKUPSHUTDOWN, VSS_WS_COUNT);

  VSS_WRITER_STATE = _VSS_WRITER_STATE;
  TVssWriterState = _VSS_WRITER_STATE;

  PVSS_WRITER_STATE = ^_VSS_WRITER_STATE;
  PVssWriterState = ^_VSS_WRITER_STATE;

  _VSS_BACKUP_TYPE = (VSS_BT_UNDEFINED, VSS_BT_FULL, VSS_BT_INCREMENTAL,
    VSS_BT_DIFFERENTIAL, VSS_BT_LOG, VSS_BT_COPY, VSS_BT_OTHER);

  VSS_BACKUP_TYPE = _VSS_BACKUP_TYPE;
  TVssBackupType = _VSS_BACKUP_TYPE;

  PVSS_BACKUP_TYPE = ^_VSS_BACKUP_TYPE;
  PVssBackupType = ^_VSS_BACKUP_TYPE;

  _VSS_RESTORE_TYPE = (VSS_RTYPE_UNDEFINED, VSS_RTYPE_BY_COPY, VSS_RTYPE_IMPORT,
    VSS_RTYPE_OTHER);

  VSS_RESTORE_TYPE = _VSS_RESTORE_TYPE;
  TVssRestoreType = _VSS_RESTORE_TYPE;

  PVSS_RESTORE_TYPE = ^_VSS_RESTORE_TYPE;
  PVssRestoreType = ^_VSS_RESTORE_TYPE;

  _VSS_FILE_RESTORE_STATUS = (VSS_RS_UNDEFINED, VSS_RS_NONE, VSS_RS_ALL,
    VSS_RS_FAILED);

  VSS_FILE_RESTORE_STATUS = _VSS_FILE_RESTORE_STATUS;
  TVssRestoreStatus = _VSS_FILE_RESTORE_STATUS;

  _VSS_RESTORE_TARGET = (VSS_RT_UNDEFINED, VSS_RT_ORIGINAL, VSS_RT_ALTERNATE,
    VSS_RT_DIRECTED, VSS_RT_ORIGINAL_LOCATION);

  VSS_RESTORE_TARGET = _VSS_RESTORE_TARGET;
  TVssRestoreTarget = _VSS_RESTORE_TARGET;

  _VSS_ROLLFORWARD_TYPE = (VSS_RF_UNDEFINED, VSS_RF_NONE, VSS_RF_ALL,
    VSS_RF_PARTIAL);

  VSS_ROLLFORWARD_TYPE = _VSS_ROLLFORWARD_TYPE;
  TVssRollforwardType = _VSS_ROLLFORWARD_TYPE;

  PVSS_ROLLFORWARD_TYPE = ^_VSS_ROLLFORWARD_TYPE;
  PVssRollforwardType = ^_VSS_ROLLFORWARD_TYPE;

  _VSS_PROVIDER_TYPE = (VSS_PROV_UNKNOWN, VSS_PROV_SYSTEM, VSS_PROV_SOFTWARE,
    VSS_PROV_HARDWARE);

  VSS_PROVIDER_TYPE = _VSS_PROVIDER_TYPE;
  TVssProviderType = _VSS_PROVIDER_TYPE;

  PVSS_PROVIDER_TYPE = ^_VSS_PROVIDER_TYPE;
  PVssProviderType = ^_VSS_PROVIDER_TYPE;

  _VSS_USAGE_TYPE = (VSS_UT_UNDEFINED, VSS_UT_BOOTABLESYSTEMSTATE,
    VSS_UT_SYSTEMSERVICE, VSS_UT_USERDATA, VSS_UT_OTHER);

  VSS_USAGE_TYPE = _VSS_USAGE_TYPE;
  TVssUsageType = _VSS_USAGE_TYPE;

  PVSS_USAGE_TYPE = ^_VSS_USAGE_TYPE;
  PVssUsageType = ^_VSS_USAGE_TYPE;

  _VSS_SOURCE_TYPE = (VSS_ST_UNDEFINED, VSS_ST_TRANSACTEDDB,
    VSS_ST_NONTRANSACTEDDB, VSS_ST_OTHER);

  VSS_SOURCE_TYPE = _VSS_SOURCE_TYPE;
  TVssSourceType = _VSS_SOURCE_TYPE;

  PVSS_SOURCE_TYPE = ^_VSS_SOURCE_TYPE;
  PVssSourceType = ^_VSS_SOURCE_TYPE;

  _VSS_RESTOREMETHOD_ENUM = (VSS_RME_UNDEFINED, VSS_RME_RESTORE_IF_NOT_THERE,
    VSS_RME_RESTORE_IF_CAN_REPLACE, VSS_RME_STOP_RESTORE_START,
    VSS_RME_RESTORE_TO_ALTERNATE_LOCATION, VSS_RME_RESTORE_AT_REBOOT,
    VSS_RME_RESTORE_AT_REBOOT_IF_CANNOT_REPLACE, VSS_RME_CUSTOM,
    VSS_RME_RESTORE_STOP_START);

  VSS_RESTOREMETHOD_ENUM = _VSS_RESTOREMETHOD_ENUM;
  TVssRestoreMethodEnum = _VSS_RESTOREMETHOD_ENUM;

  PVSS_RESTOREMETHOD_ENUM = ^_VSS_RESTOREMETHOD_ENUM;
  PVssRestoreMethodEnum = ^_VSS_RESTOREMETHOD_ENUM;

  _VSS_WRITERRESTORE_ENUM = (VSS_WRE_UNDEFINED, VSS_WRE_NEVER,
    VSS_WRE_IF_REPLACE_FAILS, VSS_WRE_ALWAYS);

  VSS_WRITERRESTORE_ENUM = _VSS_WRITERRESTORE_ENUM;
  TVssWriterRestoreEnum = _VSS_WRITERRESTORE_ENUM;

  PVSS_WRITERRESTORE_ENUM = ^_VSS_WRITERRESTORE_ENUM;
  PVssWriterRestoreEnum = ^_VSS_WRITERRESTORE_ENUM;

  _VSS_SNAPSHOT_CONTEXT = DWORD;
  VSS_SNAPSHOT_CONTEXT = _VSS_SNAPSHOT_CONTEXT;
  TVssSnapshotContext = _VSS_SNAPSHOT_CONTEXT;

  PVSS_SNAPSHOT_CONTEXT = ^_VSS_SNAPSHOT_CONTEXT;
  PVssSnapshotContext = ^_VSS_SNAPSHOT_CONTEXT;

  _VSS_PROVIDER_CAPABILITIES = DWORD;
  VSS_PROVIDER_CAPABILITIES = _VSS_PROVIDER_CAPABILITIES;
  TVssProviderCapabilties = _VSS_PROVIDER_CAPABILITIES;

  PVSS_PROVIDER_CAPABILITIES = ^_VSS_PROVIDER_CAPABILITIES;
  PVssProviderCapabilities = ^_VSS_PROVIDER_CAPABILITIES;

  _VSS_HARDWARE_OPTIONS = DWORD;
  VSS_HARDWARE_OPTIONS = _VSS_HARDWARE_OPTIONS;
  TVssHardwareOptions = _VSS_HARDWARE_OPTIONS;

  PVSS_HARDWARE_OPTIONS = ^_VSS_HARDWARE_OPTIONS;
  PVssHardwareOptions = ^_VSS_HARDWARE_OPTIONS;

  _VSS_RECOVERY_OPTIONS = DWORD;
  VSS_RECOVERY_OPTIONS = _VSS_RECOVERY_OPTIONS;
  TVssRecoveryOptions = _VSS_RECOVERY_OPTIONS;

  PVSS_RECOVERY_OPTIONS = ^_VSS_RECOVERY_OPTIONS;
  PVssRecoveryOptions = ^_VSS_RECOVERY_OPTIONS;

  _VSS_APPLICATION_LEVEL = LONGINT;
  VSS_APPLICATION_LEVEL = _VSS_APPLICATION_LEVEL;
  TVssApplicationLevel = _VSS_APPLICATION_LEVEL;

  PVSS_APPLICATION_LEVEL = ^_VSS_APPLICATION_LEVEL;
  PVssApplicationLevel = ^_VSS_APPLICATION_LEVEL;

  _VSS_SNAPSHOT_PROPERTY_ID = DWORD;
  VSS_SNAPSHOT_PROPERTY_ID = _VSS_SNAPSHOT_PROPERTY_ID;
  TVssSnapshotPropertyId = _VSS_SNAPSHOT_PROPERTY_ID;

  PVSS_SNAPSHOT_PROPERTY_ID = ^_VSS_SNAPSHOT_PROPERTY_ID;
  PVssSnapshotPropertyId = ^_VSS_SNAPSHOT_PROPERTY_ID;

  _VSS_FILE_SPEC_BACKUP_TYPE = DWORD;
  VSS_FILE_SPEC_BACKUP_TYPE = _VSS_FILE_SPEC_BACKUP_TYPE;
  TVssFileSpecBackupType = _VSS_FILE_SPEC_BACKUP_TYPE;

  PVSS_FILE_SPEC_BACKUP_TYPE = ^_VSS_FILE_SPEC_BACKUP_TYPE;
  PVssFileSpecBackupType = ^_VSS_FILE_SPEC_BACKUP_TYPE;

  _VSS_BACKUP_SCHEMA = DWORD;
  VSS_BACKUP_SCHEMA = _VSS_BACKUP_SCHEMA;
  TVssBackupSchema = _VSS_BACKUP_SCHEMA;

  PVSS_BACKUP_SCHEMA = ^_VSS_BACKUP_SCHEMA;
  PVssBackupSchema = ^_VSS_BACKUP_SCHEMA;

const
  VssApiDll = 'vssapi.dll'; { see the C:\Windows\System32 directory }
  ResApiDll = 'ResUtils.Dll';

  // _VSS_SNAPSHOT_COMPATIBILITY
  VSS_SC_DISABLE_DEFRAG = 1;
  VSS_SC_DISABLE_CONTENTINDEX = 2;

  // _VSS_VOLUME_SNAPSHOT_ATTRIBUTES
  VSS_VOLSNAP_ATTR_PERSISTENT = $1;
  VSS_VOLSNAP_ATTR_CLIENT_ACCESSIBLE = $4;
  VSS_VOLSNAP_ATTR_NO_AUTO_RELEASE = $8;
  VSS_VOLSNAP_ATTR_NO_WRITERS = $10;
  VSS_VOLSNAP_ATTR_TRANSPORTABLE = $20;
  VSS_VOLSNAP_ATTR_NOT_SURFACED = $40;
  VSS_VOLSNAP_ATTR_HARDWARE_ASSISTED = $10000;
  VSS_VOLSNAP_ATTR_DIFFERENTIAL = $20000;
  VSS_VOLSNAP_ATTR_PLEX = $40000;
  VSS_VOLSNAP_ATTR_IMPORTED = $80000;
  VSS_VOLSNAP_ATTR_EXPOSED_LOCALLY = $100000;
  VSS_VOLSNAP_ATTR_EXPOSED_REMOTELY = $200000;
  VSS_VOLSNAP_ATTR_AUTORECOVER = $400000;
  VSS_VOLSNAP_ATTR_ROLLBACK_RECOVERY = $800000;
  VSS_VOLSNAP_ATTR_DELAYED_POSTSNAPSHOT = $1000000;
  VSS_VOLSNAP_ATTR_TXF_RECOVERY = $2000000;

  // _VSS_SNAPSHOT_CONTEXT
  VSS_CTX_BACKUP = 0;
  VSS_CTX_FILE_SHARE_BACKUP = VSS_VOLSNAP_ATTR_NO_WRITERS;
  VSS_CTX_NAS_ROLLBACK = VSS_VOLSNAP_ATTR_PERSISTENT or
    VSS_VOLSNAP_ATTR_NO_AUTO_RELEASE or VSS_VOLSNAP_ATTR_NO_WRITERS;
  VSS_CTX_APP_ROLLBACK = VSS_VOLSNAP_ATTR_PERSISTENT or
    VSS_VOLSNAP_ATTR_NO_AUTO_RELEASE;
  VSS_CTX_CLIENT_ACCESSIBLE = VSS_VOLSNAP_ATTR_PERSISTENT or
    VSS_VOLSNAP_ATTR_CLIENT_ACCESSIBLE or VSS_VOLSNAP_ATTR_NO_AUTO_RELEASE or
    VSS_VOLSNAP_ATTR_NO_WRITERS;
  VSS_CTX_CLIENT_ACCESSIBLE_WRITERS = VSS_VOLSNAP_ATTR_PERSISTENT or
    VSS_VOLSNAP_ATTR_CLIENT_ACCESSIBLE or VSS_VOLSNAP_ATTR_NO_AUTO_RELEASE;
  VSS_CTX_ALL = $FFFFFFFF;

  // _VSS_PROVIDER_CAPABILITIES
  VSS_PRV_CAPABILITY_LEGACY = $1;
  VSS_PRV_CAPABILITY_COMPLIANT = $2;
  VSS_PRV_CAPABILITY_LUN_REPOINT = $4;
  VSS_PRV_CAPABILITY_LUN_RESYNC = $8;
  VSS_PRV_CAPABILITY_OFFLINE_CREATION = $10;
  VSS_PRV_CAPABILITY_MULTIPLE_IMPORT = $20;
  VSS_PRV_CAPABILITY_RECYCLING = $40;
  VSS_PRV_CAPABILITY_PLEX = $80;
  VSS_PRV_CAPABILITY_DIFFERENTIAL = $100;
  VSS_PRV_CAPABILITY_CLUSTERED = $200;

  // _VSS_HARDWARE_OPTIONS
  // BreakSnasphotSetEx flags 0x000000FF
  VSS_BREAKEX_FLAG_MASK_LUNS = $00000001;
  // Mask on break - it gets deleted and provider sent OnLunStateChange
  VSS_BREAKEX_FLAG_MAKE_READ_WRITE = $00000002;
  // Makes the shadow copy luns R/W
  VSS_BREAKEX_FLAG_REVERT_IDENTITY_ALL = $00000004;
  // Operation cancels if disk identity revert is not possible
  VSS_BREAKEX_FLAG_REVERT_IDENTITY_NONE = $00000008;
  // Perform break with no reverting of disk identifiers

  // OnLunStateChange flags $0000FF00
  VSS_ONLUNSTATECHANGE_NOTIFY_READ_WRITE = $00000100;
  // Convert LUNS to R/W permanently
  VSS_ONLUNSTATECHANGE_NOTIFY_LUN_PRE_RECOVERY = $00000200;
  // Notifies provider before TxF recovery or VSS auto-recovery
  VSS_ONLUNSTATECHANGE_NOTIFY_LUN_POST_RECOVERY = $00000400;
  // After TxF or auto-recovery
  VSS_ONLUNSTATECHANGE_DO_MASK_LUNS = $00000800;
  // Shadow copy LUNs must be masked from this machine

  // _VSS_RECOVERY_OPTIONS
  // AddSnapshotToRecoverySet flags 0x000000FF
  // RecoverSet flags 0x0000FF00
  VSS_RECOVERY_REVERT_IDENTITY_ALL = $00000100;
  // Ensure final disk id is signature of the snapshotted original lun (source of snapshot)
  VSS_RECOVERY_NO_VOLUME_CHECK = $00000200; // Override volume safety checks

  // _VSS_APPLICATION_LEVEL
  VSS_APP_UNKNOWN = 0;
  VSS_APP_SYSTEM = 1;
  VSS_APP_BACK_END = 2;
  VSS_APP_FRONT_END = 3;
  VSS_APP_SYSTEM_RM = 4;
  VSS_APP_AUTO = -1;

  // _VSS_SNAPSHOT_PROPERTY_ID
  VSS_SPROPID_UNKNOWN = $00000000;
  VSS_SPROPID_SNAPSHOT_ID = $00000001;
  VSS_SPROPID_SNAPSHOT_SET_ID = $00000002;
  VSS_SPROPID_SNAPSHOTS_COUNT = $00000003;
  VSS_SPROPID_SNAPSHOT_DEVICE = $00000004;
  VSS_SPROPID_ORIGINAL_VOLUME = $00000005;
  VSS_SPROPID_ORIGINATING_MACHINE = $00000006;
  VSS_SPROPID_SERVICE_MACHINE = $00000007;
  VSS_SPROPID_EXPOSED_NAME = $00000008;
  VSS_SPROPID_EXPOSED_PATH = $00000009;
  VSS_SPROPID_PROVIDER_ID = $0000000A;
  VSS_SPROPID_SNAPSHOT_ATTRIBUTES = $0000000B;
  VSS_SPROPID_CREATION_TIMESTAMP = $0000000C;
  VSS_SPROPID_STATUS = $0000000D;

  // _VSS_FILE_SPEC_BACKUP_TYPE
  VSS_FSBT_FULL_BACKUP_REQUIRED = $00000001;
  VSS_FSBT_DIFFERENTIAL_BACKUP_REQUIRED = $00000002;
  VSS_FSBT_INCREMENTAL_BACKUP_REQUIRED = $00000004;
  VSS_FSBT_LOG_BACKUP_REQUIRED = $00000008;
  VSS_FSBT_FULL_SNAPSHOT_REQUIRED = $00000100;
  VSS_FSBT_DIFFERENTIAL_SNAPSHOT_REQUIRED = $00000200;
  VSS_FSBT_INCREMENTAL_SNAPSHOT_REQUIRED = $00000400;
  VSS_FSBT_LOG_SNAPSHOT_REQUIRED = $00000800;
  VSS_FSBT_ALL_BACKUP_REQUIRED = $0000000F;
  VSS_FSBT_ALL_SNAPSHOT_REQUIRED = $00000F00;

  // _VSS_BACKUP_SCHEMA
  VSS_BS_UNDEFINED = $00000000;
  VSS_BS_DIFFERENTIAL = $00000001;
  VSS_BS_INCREMENTAL = $00000002;
  VSS_BS_EXCLUSIVE_INCREMENTAL_DIFFERENTIAL = $00000004;
  VSS_BS_LOG = $00000008;
  VSS_BS_COPY = $00000010;
  VSS_BS_TIMESTAMPED = $00000020;
  VSS_BS_LAST_MODIFY = $00000040;
  VSS_BS_LSN = $00000080;
  VSS_BS_WRITER_SUPPORTS_NEW_TARGET = $00000100;
  VSS_BS_WRITER_SUPPORTS_RESTORE_WITH_MOVE = $00000200;
  VSS_BS_INDEPENDENT_SYSTEM_STATE = $00000400;
  VSS_BS_ROLLFORWARD_RESTORE = $00001000;
  VSS_BS_RESTORE_RENAME = $00002000;
  VSS_BS_AUTHORITATIVE_RESTORE = $00004000;
  VSS_BS_WRITER_SUPPORTS_PARALLEL_RESTORES = $00008000;

  //
  // VSS error codes.
  //
  //
  // Values are 32 bit values laid out as follows:
  //
  // 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
  // 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  // +---+-+-+-----------------------+-------------------------------+
  // |Sev|C|R|     Facility          |               Code            |
  // +---+-+-+-----------------------+-------------------------------+
  //
  // where
  //
  // Sev - is the severity code
  //
  // 00 - Success
  // 01 - Informational
  // 10 - Warning
  // 11 - Error
  //
  // C - is the Customer code flag
  //
  // R - is a reserved bit
  //
  // Facility - is the facility code
  //
  // Code - is the facility's status code
  //

  // A function call was made when the object was in an incorrect state
  // for that function
  VSS_E_BAD_STATE = HResult($80042301);

  // A Volume Shadow Copy Service component encountered an unexpected error.
  // Check the Application event log for more information.
  VSS_E_UNEXPECTED = HResult($80042302);

  // The provider has already been registered.
  VSS_E_PROVIDER_ALREADY_REGISTERED = HResult($80042303);

  // The volume shadow copy provider is not registered in the system.
  VSS_E_PROVIDER_NOT_REGISTERED = HResult($80042304);

  // The shadow copy provider had an error.
  // Check the System and Application event logs for more information.
  VSS_E_PROVIDER_VETO = HResult($80042306);

  // The shadow copy provider is currently in use and cannot be unregistered.
  VSS_E_PROVIDER_IN_USE = HResult($80042307);

  // The specified object was not found.
  VSS_E_OBJECT_NOT_FOUND = HResult($80042308);

  // The asynchronous operation is pending.
  VSS_S_ASYNC_PENDING = $00042309;

  // The asynchronous operation has completed.
  VSS_S_ASYNC_FINISHED = $0004230A;

  // The asynchronous operation has been cancelled.
  VSS_S_ASYNC_CANCELLED = $0004230B;

  // Shadow copying the specified volume is not supported.
  VSS_E_VOLUME_NOT_SUPPORTED = HResult($8004230C);

  // The object already exists.
  VSS_E_OBJECT_ALREADY_EXISTS = HResult($8004230D);

  // The given shadow copy provider does not support shadow copying the specified
  // volume.
  VSS_E_VOLUME_NOT_SUPPORTED_BY_PROVIDER = HResult($8004230E);

  // The shadow copy provider had an unexpected error while trying to process the
  // specified operation.
  VSS_E_UNEXPECTED_PROVIDER_ERROR = HResult($8004230F);

  // The given XML document is invalid.  It is either incorrectly-formed XML or it
  // does not match the schema.  This error code is deprecated.
  VSS_E_CORRUPT_XML_DOCUMENT = HResult($80042310);

  // The given XML document is invalid.  It is either incorrectly-formed XML or it
  // does not match the schema.
  VSS_E_INVALID_XML_DOCUMENT = HResult($80042311);

  // The maximum number of volumes for this operation has been reached.
  VSS_E_MAXIMUM_NUMBER_OF_VOLUMES_REACHED = HResult($80042312);

  // The shadow copy provider timed out while flushing data to the volume being shadow copied. This is probably due to excessive activity on the volume. Try again later when the volume is not being used so heavily.
  VSS_E_FLUSH_WRITES_TIMEOUT = HResult($80042313);

  // The shadow copy provider timed out while holding writes to the volume being shadow copied. This is probably due to excessive activity on the volume by an application or a system service. Try again later when activity on the volume is reduced.
  VSS_E_HOLD_WRITES_TIMEOUT = HResult($80042314);

  // VSS encountered problems while sending events to writers.
  VSS_E_UNEXPECTED_WRITER_ERROR = HResult($80042315);

  // Another shadow copy creation is already in progress. Wait a few moments and try again.
  VSS_E_SNAPSHOT_SET_IN_PROGRESS = HResult($80042316);

  // The specified volume has already reached its maximum number of shadow copies.
  VSS_E_MAXIMUM_NUMBER_OF_SNAPSHOTS_REACHED = HResult($80042317);

  // An error was detected in the Volume Shadow Copy Service (VSS). The problem occurred while trying to contact VSS writers.
  // Verify that the Event System service and the VSS service are running and check for associated errors in the event logs.
  VSS_E_WRITER_INFRASTRUCTURE = HResult($80042318);

  // A writer did not respond to a GatherWriterStatus call.  The writer may either have terminated
  // or it may be stuck.  Check the System and Application event logs for more information.
  VSS_E_WRITER_NOT_RESPONDING = HResult($80042319);

  // The writer has already successfully called the Subscribe function.  It cannot call
  // Subscribe multiple times.
  VSS_E_WRITER_ALREADY_SUBSCRIBED = HResult($8004231A);

  // The shadow copy provider does not support the specified shadow copy type.
  VSS_E_UNSUPPORTED_CONTEXT = HResult($8004231B);

  // The specified shadow copy storage association is in use and so can't be deleted.
  VSS_E_VOLUME_IN_USE = HResult($8004231D);

  // Maximum number of shadow copy storage associations already reached.
  VSS_E_MAXIMUM_DIFFAREA_ASSOCIATIONS_REACHED = HResult($8004231E);

  // Insufficient storage available to create either the shadow copy storage file or other shadow copy data.
  VSS_E_INSUFFICIENT_STORAGE = HResult($8004231F);

  // No shadow copies were successfully imported.
  VSS_E_NO_SNAPSHOTS_IMPORTED = HResult($80042320);

  // Some shadow copies were not successfully imported.
  VSS_S_SOME_SNAPSHOTS_NOT_IMPORTED = HResult($80042321);
  VSS_E_SOME_SNAPSHOTS_NOT_IMPORTED = HResult($80042321);

  // The maximum number of remote machines for this operation has been reached.
  VSS_E_MAXIMUM_NUMBER_OF_REMOTE_MACHINES_REACHED = HResult($80042322);

  // The remote server is unavailable.
  VSS_E_REMOTE_SERVER_UNAVAILABLE = HResult($80042323);

  // The remote server is running a version of the Volume Shadow Copy Service that does not
  // support remote shadow-copy creation.
  VSS_E_REMOTE_SERVER_UNSUPPORTED = HResult($80042324);

  // A revert is currently in progress for the specified volume.  Another revert
  // cannot be initiated until the current revert completes.
  VSS_E_REVERT_IN_PROGRESS = HResult($80042325);

  // The volume being reverted was lost during revert.
  VSS_E_REVERT_VOLUME_LOST = HResult($80042326);

  // A reboot is required after completing this operation.
  VSS_E_REBOOT_REQUIRED = HResult($80042327);

  // A timeout occurred while freezing a transaction manager.
  VSS_E_TRANSACTION_FREEZE_TIMEOUT = HResult($80042328);

  // Too much time elapsed between freezing a transaction manager and thawing
  // the transaction manager.
  VSS_E_TRANSACTION_THAW_TIMEOUT = HResult($80042329);

  // The volume being backed up is not mounted on the local host.
  VSS_E_VOLUME_NOT_LOCAL = HResult($8004232D);

  // A timeout occurred while preparing a cluster shared volume for backup.
  VSS_E_CLUSTER_TIMEOUT = HResult($8004232E);

  // The shadow-copy set contains only a subset of the
  // volumes needed to correctly backup the selected components
  // of the writer.
  VSS_E_WRITERERROR_INCONSISTENTSNAPSHOT = HResult($800423F0);

  // A resource allocation failed while processing this operation.
  VSS_E_WRITERERROR_OUTOFRESOURCES = HResult($800423F1);

  // The writer's timeout expired between the Freeze and Thaw events.
  VSS_E_WRITERERROR_TIMEOUT = HResult($800423F2);

  // The writer experienced a transient error.  If the backup process is retried,
  // the error may not reoccur.
  VSS_E_WRITERERROR_RETRYABLE = HResult($800423F3);

  // The writer experienced a non-transient error.  If the backup process is retried,
  // the error is likely to reoccur.
  VSS_E_WRITERERROR_NONRETRYABLE = HResult($800423F4);

  // The writer experienced an error while trying to recover the shadow-copy volume.
  VSS_E_WRITERERROR_RECOVERY_FAILED = HResult($800423F5);

  // The shadow copy set break operation failed because the disk/partition identities could not be reverted. The target identity already exists on the machine or cluster and must be masked before this operation can succeed.
  VSS_E_BREAK_REVERT_ID_FAILED = HResult($800423F6);

  // This version of the hardware provider does not support this operation.
  VSS_E_LEGACY_PROVIDER = HResult($800423F7);

  // An expected disk did not arrive in the system.
  VSS_E_MISSING_DISK = HResult($800423F8);

  // An expected hidden volume did not arrive in the system. Check the Application event log for more information.
  VSS_E_MISSING_HIDDEN_VOLUME = HResult($800423F9);

  // An expected volume did not arrive in the system. Check the Application event log for more information.
  VSS_E_MISSING_VOLUME = HResult($800423FA);

  // The autorecovery operation failed to complete on the shadow copy.
  VSS_E_AUTORECOVERY_FAILED = HResult($800423FB);

  // An error occurred in processing the dynamic disks involved in the operation.
  VSS_E_DYNAMIC_DISK_ERROR = HResult($800423FC);

  // The given Backup Components Document is for a non-transportable shadow copy. This operation can only be done on transportable shadow copies.
  VSS_E_NONTRANSPORTABLE_BCD = HResult($800423FD);

  // The MBR signature or GPT ID for one or more disks could not be set to the intended value. Check the Application event log for more information.
  VSS_E_CANNOT_REVERT_DISKID = HResult($800423FE);

  // The LUN resynchronization operation could not be started because another resynchronization operation is already in progress.
  VSS_E_RESYNC_IN_PROGRESS = HResult($800423FF);

  // The clustered disks could not be enumerated or could not be put into cluster maintenance mode. Check the System event log for cluster related events and the Application event log for VSS related events.
  VSS_E_CLUSTER_ERROR = HResult($80042400);

  // The requested operation would overwrite a volume that is not explicitly selected. For more information, check the Application event log.
  VSS_E_UNSELECTED_VOLUME = HResult($8004232A);

  // The shadow copy ID was not found in the backup components document for the shadow copy set.
  VSS_E_SNAPSHOT_NOT_IN_SET = HResult($8004232B);

  // The specified volume is nested too deeply to participate in the VSS operation.
  VSS_E_NESTED_VOLUME_LIMIT = HResult($8004232C);

  // The requested operation is not supported.
  VSS_E_NOT_SUPPORTED = HResult($8004232F);

  // The writer experienced a partial failure. Check the component level error state for more information.
  VSS_E_WRITERERROR_PARTIAL_FAILURE = HResult($80042336);


  // ASR error codes

  // There are too few disks on this computer or one or more of the disks is too small. Add or change disks so they match the disks in the backup, and try the restore again.
  VSS_E_ASRERROR_DISK_ASSIGNMENT_FAILED = HResult($80042401);

  // Windows cannot create a disk on this computer needed to restore from the backup. Make sure the disks are properly connected, or add or change disks, and try the restore again.
  VSS_E_ASRERROR_DISK_RECREATION_FAILED = HResult($80042402);

  // The computer needs to be restarted to finish preparing a hard disk for restore. To continue, restart your computer and run the restore again.
  VSS_E_ASRERROR_NO_ARCPATH = HResult($80042403);

  // The backup failed due to a missing disk for a dynamic volume. Ensure the disk is online and retry the backup.
  VSS_E_ASRERROR_MISSING_DYNDISK = HResult($80042404);

  // Automated System Recovery failed the shadow copy, because a selected critical volume is located on a cluster shared disk. This is an unsupported configuration.
  VSS_E_ASRERROR_SHARED_CRIDISK = HResult($80042405);

  // A data disk is currently set as active in BIOS. Set some other disk as active or use the DiskPart utility to clean the data disk, and then retry the restore operation.
  VSS_E_ASRERROR_DATADISK_RDISK0 = HResult($80042406);

  // The disk that is set as active in BIOS is too small to recover the original system disk. Replace the disk with a larger one and retry the restore operation.
  VSS_E_ASRERROR_RDISK0_TOOSMALL = HResult($80042407);

  // Failed to find enough suitable disks for recreating all critical disks. The number of available disks should be same or greater than the number of critical disks at time of backup, and each one of the disks must be of same or greater size.
  VSS_E_ASRERROR_CRITICAL_DISKS_TOO_SMALL = HResult($80042408);

  // Writer status is not available for one or more writers.  A writer may have reached the limit to the number of available backup-restore session states.
  VSS_E_WRITER_STATUS_NOT_AVAILABLE = HResult($80042409);

  // A critical dynamic disk is a Virtual Hard Disk (VHD). This is an unsupported configuration. Check the Application event log for more details.
  VSS_E_ASRERROR_DYNAMIC_VHD_NOT_SUPPORTED = HResult($8004240A);

  // A critical volume selected for backup exists on a disk which cannot be backed up by ASR.
  VSS_E_CRITICAL_VOLUME_ON_INVALID_DISK = HResult($80042411);

  // No disk that can be used for recovering the system disk can be found.
  // Try the following:
  // 1) A probable system disk may have been excluded by mistake.
  // a.  Review the list of disks that you have excluded from the recovery
  // for a likely disk.
  // b.  Type LIST DISK command in the DISKPART command interpreter. The
  // probable system disk is usually the first disk listed in the results.
  // c.  If possible, remove the disk from the exclusion list and then retry
  // the recovery.
  // 2) A USB disk may have been assigned as a system disk.
  // a.  Detach all USB disks from the computer.
  // b.  Reboot into Windows Recovery Environment (Win RE),
  // then reattach USB disks and retry the recovery.
  // 3) An invalid disk may have been assigned as system disk.
  // a.  Physically detach the disk from your computer. Then boot
  // into Win RE to retry the recovery.
  VSS_E_ASRERROR_RDISK_FOR_SYSTEM_DISK_NOT_FOUND = HResult($80042412);

  // Windows did not find any fixed disk that can be used to recreate volumes present in backup. Ensure disks are online, and disk drivers are installed to access the disk(s). 'diskpart.exe' tool with list disks command can be used to see the list of available fixed disks on the system.
  VSS_E_ASRERROR_NO_PHYSICAL_DISK_AVAILABLE = HResult($80042413);

  // Windows did not find any disk which it can use for recreating volumes present in backup. Offline disks, cluster shared disks or disks explicitly excluded by user will not be used by Windows. Ensure that disks are online and no disks are excluded by mistake.
  VSS_E_ASRERROR_FIXED_PHYSICAL_DISK_AVAILABLE_AFTER_DISK_EXCLUSION = HResult
    ($80042414);

  // Restore failed because a disk which was critical at backup is excluded. To continue you need to either remove the disk from exclusion list or detach it from machine or clean it using DiskPart utility, and then retry restore. If you cannot clean or detach it then change the disk signature using DiskPart command UNIQUEID DISK ID.
  VSS_E_ASRERROR_CRITICAL_DISK_CANNOT_BE_EXCLUDED = HResult($80042415);

  // System partition (partition marked "active") is hidden or contains an unrecognized file system. Backup does not support this configuration.
  VSS_E_ASRERROR_SYSTEM_PARTITION_HIDDEN = HResult($80042416);

type
  // BSTR = LPCWSTR = PWCHAR = PWideChar;

  TDynStringArray = array of string;

  _VSS_COMPONENT_TYPE = (VSS_CT_UNDEFINED, VSS_CT_DATABASE, VSS_CT_FILEGROUP);

  VSS_COMPONENT_TYPE = _VSS_COMPONENT_TYPE;
  TVssComponentType = _VSS_COMPONENT_TYPE;

  _VSS_ALTERNATE_WRITER_STATE = (VSS_AWS_UNDEFINED, VSS_AWS_NO_ALTERNATE_WRITER,
    VSS_AWS_ALTERNATE_WRITER_EXISTS, VSS_AWS_THIS_IS_ALTERNATE_WRITER);

  VSS_ALTERNATE_WRITER_STATE = _VSS_ALTERNATE_WRITER_STATE;
  TVssAlternateWriterState = _VSS_ALTERNATE_WRITER_STATE;

  _VSS_SNAPSHOT_PROP = record
    m_SnapshotId: TGUID;
    m_SnapshotSetId: TGUID;
    m_lSnapshotsCount: Integer;
    m_pwszSnapshotDeviceObject: PWideChar;
    m_pwszOriginalVolumeName: PWideChar;
    m_pwszOriginatingMachine: PWideChar;
    m_pwszServiceMachine: PWideChar;
    m_pwszExposedName: PWideChar;
    m_pwszExposedPath: PWideChar;
    m_ProviderId: TGUID;
    m_lSnapshotAttributes: Integer;
    m_tsCreationTimestamp: Int64;
    m_eStatus: VSS_SNAPSHOT_STATE;
  end;

  VSS_SNAPSHOT_PROP = _VSS_SNAPSHOT_PROP;
  TVssSnapshotProp = _VSS_SNAPSHOT_PROP;
  PVssSnapshotProp = ^_VSS_SNAPSHOT_PROP;

  _VSS_PROVIDER_PROP = record
    m_ProviderId: TGUID;
    m_pwszProviderName: PWideChar;
    m_eProviderType: TVssProviderType;
    m_pwszProviderVersion: PWideChar;
    m_ProviderVersionId: TGUID;
    m_ClassId: TCLSID;
  end;

  VSS_PROVIDER_PROP = _VSS_PROVIDER_PROP;
  TVssProviderProp = _VSS_PROVIDER_PROP;
  PVssProviderProp = ^_VSS_PROVIDER_PROP;

  VSS_OBJECT_UNION = record
    case Integer of
      1:
        (Snap: TVssSnapshotProp);
      2:
        (Prov: TVssProviderProp);
  end;

  _VSS_OBJECT_PROP = record
    PType: VSS_OBJECT_TYPE;
    PObj: VSS_OBJECT_UNION;
  end;

  VSS_OBJECT_PROP = _VSS_OBJECT_PROP;
  TVssObjectProp = _VSS_OBJECT_PROP;

  _VSS_COMPONENTINFO = record
    ciType: TVssComponentType; // either VSS_CT_DATABASE or VSS_CT_FILEGROUP
    bstrLogicalPath: PWideChar; // logical path to component
    bstrComponentName: PWideChar; // component name
    bstrCaption: PWideChar; // description of component
    pbIcon: PByte; // icon
    cbIcon: UINT; // icon
    bRestoreMetadata: ByteBool; // whether component supplies restore metadata
    bNotifyOnBackupComplete: ByteBool;
    // whether component needs to be informed if backup was successful
    bSelectable: ByteBool; // is component selectable
    bSelectableForRestore: ByteBool; // is component selectable for restore
    dwComponentFlags: DWORD; // extra attribute flags for the component
    cFileCount: UINT; // # of files in file group
    cDatabases: UINT; // # of database files
    cLogFiles: UINT; // # of log files
    cDependencies: UINT; // # of components that this component depends on
  end;

  VSS_COMPONENTINFO = _VSS_COMPONENTINFO;
  TVssComponentInfo = _VSS_COMPONENTINFO;
  PVssComponentInfo = ^_VSS_COMPONENTINFO;

  IVssAsync = Interface(IUnknown)
    ['{507C37B4-CF5B-4e95-B0AF-14EB9767467E}']
    function Cancel(): HResult; StdCall;
    function Wait(dwMilliseconds: DWORD): HResult; StdCall;
    function QueryStatus(var pHrResult: HResult; pReserved: PInteger)
      : HResult; StdCall;
  end;

  IVssWMFiledesc = Interface(IUnknown)
    ['{3b5be0f2-07a9-4e4b-bdd3-cfdc8e2c0d2d}']
    function GetPath(var pbstrPath: PWideChar): HResult; StdCall;
    function GetFilespec(var pbstrFilespec: PWideChar): HResult; StdCall;
    function GetRecursive(var pbRecursive: Bool): HResult; StdCall;
    function GetAlternateLocation(var pbstrAlternateLocation: PWideChar)
      : HResult; StdCall;
    function GetBackupTypeMask(var pdwTypeMask: DWORD): HResult; StdCall;
  end;

  IVssWMDependency = Interface(IUnknown)
    // ['{}']
    { TODO: Complete! }
  end;

  IVssWMComponent = Interface(IUnknown)
    function GetComponentInfo(out ppInfo: PVssComponentInfo): HResult; StdCall;
    function FreeComponentInfo(pInfo: PVssComponentInfo): HResult; StdCall;
    function GetFile(iFile: UINT; out ppFiledesc: IVssWMFiledesc)
      : HResult; StdCall;
    function GetDatabaseFile(iDBFile: UINT; out ppFiledesc: IVssWMFiledesc)
      : HResult; StdCall;
    function GetDatabaseLogFile(iDbLogFile: UINT;
      out ppFiledesc: IVssWMFiledesc): HResult; StdCall;
    function GetDependency(iDependency: UINT;
      out ppDependency: IVssWMDependency): HResult; StdCall;
  end;

  IVssExamineWriterMetadata = Interface(IUnknown)
    ['{902fcf7f-b7fd-42f8-81f1-b2e400b1e5bd}']
    function GetIdentity(var pidInstance, pidWriter: TGUID;
      var pbstrWriterName: PWideChar; var pUsage: VSS_USAGE_TYPE;
      var pSource: VSS_SOURCE_TYPE): HResult; StdCall;
    function GetFileCounts(var pcIncludeFiles, pcExcludeFiles,
      pcComponents: UINT): HResult; StdCall;
    function GetIncludeFile(iFile: UINT; out ppFiledesc: IVssWMFiledesc)
      : HResult; StdCall;
    function GetExcludeFile(iFile: UINT; out ppFiledesc: IVssWMFiledesc)
      : HResult; StdCall;
    function GetComponent(iComponent: UINT; out ppComponent: IVssWMComponent)
      : HResult; StdCall;
    function GetRestoreMethod(var pMethod: VSS_RESTOREMETHOD_ENUM;
      var pbstrService: PWideChar; var pbstrUserProcedure: PWideChar;
      var pwriterRestore: VSS_WRITERRESTORE_ENUM; var pbRebootRequired: Bool;
      var pcMappings: UINT): HResult; StdCall;
    function GetAlternateLocationMapping(iMapping: UINT;
      out ppFiledesc: IVssWMFiledesc): HResult; StdCall;
    function GetBackupSchema(pdwSchemaMask: DWORD): HResult; StdCall;
    function GetDocument(out pDoc: IXMLDOMDocument): HResult; StdCall;
    function SaveAsXML(pbstrXML: PWideChar): HResult; StdCall;
    function LoadFromXML(bstrXML: PWideChar): HResult; StdCall;
  end;

  IVssExamineWriterMetadataEx = Interface(IVssExamineWriterMetadata)
    ['{0c0e5ec0-ca44-472b-b702-e652db1c0451}']
    function GetIdentityEx(var pidInstance, pidWriter: TGUID;
      var pbstrWriterName, pbstrInstanceName: PWideChar;
      var pUsage: VSS_USAGE_TYPE; var pSource: VSS_SOURCE_TYPE)
      : HResult; StdCall;
  end;

  IVssExamineWriterMetadataEx2 = Interface(IVssExamineWriterMetadata)
    ['{ce115780-a611-431b-b57f-c38303ab6aee}']
    function GetVersion(var ajorVersion, pdwMinorVersion: DWORD)
      : HResult; StdCall;
    function GetExcludeFromSnapshotCount(var cludedFromSnapshot: UINT)
      : HResult; StdCall;
    function GetExcludeFromSnapshotFile(iFile: UINT;
      out ppFiledesc: IVssWMFiledesc): HResult; StdCall;
  end;

  IVssComponent = interface;

  IVssEnumObject = interface(IUnknown)
    ['{AE1C7110-2F60-11d3-8A39-00C04F72D8E3}']
    function Next(celt: ULONG; var rgelt: VSS_OBJECT_PROP;
      var pceltFetched: ULONG): HResult; StdCall;
    function Skip(var celt: ULONG): HResult; StdCall;
    function Reset: HResult; StdCall;
    function Clone(out ppenum: IVssEnumObject): HResult; StdCall;

  end;

  IVssWriterComponentsExt = Interface(IUnknown)
    ['{156c8b5e-f131-4bd7-9c97-d1923be7e1fa}']
    function GetComponentCount(var pcComponents: UINT): HResult; StdCall;
    function GetWriterInfo(var pidInstance, pidWriter: TGUID): HResult; StdCall;
    function GetComponent(iComponent: UINT; out ppComponent: IVssComponent)
      : HResult; StdCall;
  end;

  IVssBackupComponents = Interface(IUnknown)
    ['{665c1d5f-c218-414d-a05d-7fef5f9d5c86}']
    function GetWriterComponentsCount(var pcComponents: UINT): HResult; StdCall;
    function GetWriterComponents(iWriter: UINT;
      out ppWriter: IVssWriterComponentsExt): HResult; StdCall;
    function InitializeForBackup(bstrXML: PWideChar): HResult; StdCall;
    function SetBackupState(bSelectComponents, bBackupBootableSystemState: Bool;
      backupType: VSS_BACKUP_TYPE; bPartialFileSupport: Bool): HResult; StdCall;
    function InitializeForRestore(XML: PWideChar): HResult; StdCall;
    function SetRestoreState(restoreType: VSS_RESTORE_TYPE): HResult; StdCall;
    function GatherWriterMetadata(out pAsync: IVssAsync): HResult; StdCall;
    function GetWriterMetadataCount(var pcWriters: UINT): HResult; StdCall;
    function GetWriterMetadata(iWriter: UINT; var pidInstance: TGUID;
      out ppMetadata: IVssExamineWriterMetadata): HResult; StdCall;
    function FreeWriterMetadata: HResult; StdCall;
    function AddComponent(instanceId, writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar): HResult; StdCall;
    function PrepareForBackup(out pAsync: IVssAsync): HResult; StdCall;
    function AbortBackup: HResult; StdCall;
    function GatherWriterStatus(out pAsync: IVssAsync): HResult; StdCall;
    function GetWriterStatusCount(var pcWriters: UINT): HResult; StdCall;
    function FreeWriterStatus: HResult; StdCall;
    function GetWriterStatus(iWriter: UINT; var pidInstance, pidWriter: TGUID;
      var pbstrWriter: PWideChar; var pnStatus: VSS_WRITER_STATE;
      var phResultFailure: HResult): HResult; StdCall;
    function SetBackupSucceeded(instanceId, writerId: TGUID;
      ct: VSS_COMPONENT_TYPE; wszLogicalPath, wszComponentName: PWideChar;
      bSucceded: Bool): HResult; StdCall;
    function SetBackupOptions(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName, wszBackupOptions: PWideChar)
      : HResult; StdCall;
    function SetSelectedForRestore(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar; bSelectedForRestore: Bool)
      : HResult; StdCall;
    function SetRestoreOptions(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName, wszRestoreOptions: PWideChar)
      : HResult; StdCall;
    function SetAdditionalRestores(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar; bAdditionalRestores: Bool)
      : HResult; StdCall;
    function SetPreviousBackupStamp(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName, wszPreviousBackupStamp: PWideChar)
      : HResult; StdCall;
    function SaveAsXML(pbstrXML: PWideChar): HResult; StdCall;
    function BackupComplete(out pAsync: IVssAsync): HResult; StdCall;
    function AddAlternativeLocationMapping(writerId: TGUID;
      componentType: VSS_COMPONENT_TYPE; wszLogicalPath, wszComponentName,
      wszPath, wszFilespec: PWideChar; bRecursive: Bool;
      wszDestination: PWideChar): HResult; StdCall;
    function AddRestoreSubcomponent(writerId: TGUID;
      componentType: VSS_COMPONENT_TYPE; wszLogicalPath, wszComponentName,
      wszSubComponentLogicalPath, wszSubComponentName: PWideChar; bRepair: Bool)
      : HResult; StdCall;
    function SetFileRestoreStatus(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar;
      status: VSS_FILE_RESTORE_STATUS): HResult; StdCall;
    function AddNewTarget(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName, wszPath, wszFileName: PWideChar;
      bRecursive: Bool; wszAlternatePath: PWideChar): HResult; StdCall;
    function SetRangesFilePath(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar; iPartialFile: UINT;
      wszRangesFile: PWideChar): HResult; StdCall;
    function PreRestore(out pAsync: IVssAsync): HResult; StdCall;
    function PostRestore(out pAsync: IVssAsync): HResult; StdCall;
    function SetContext(lContext: Integer): HResult; StdCall;
    function StartSnapshotSet(var pSnapshotSetId: TGUID): HResult; StdCall;
    function AddToSnapshotSet(pwszVolumeName: PWideChar; ProviderId: TGUID;
      var pidSnapshot: TGUID): HResult; StdCall;
    function DoSnapshotSet(out pAsync: IVssAsync): HResult; StdCall;
    function DeleteSnapshots(SourceObjectId: TGUID;
      eSourceObjectType: VSS_OBJECT_TYPE; bForceDelete: Bool;
      var plDeletedSnapshots: LONG; var pNondeletedSnapshotID: TGUID)
      : HResult; StdCall;
    function ImportSnapshots(out pAsync: IVssAsync): HResult; StdCall;
    function BreakSnapshotSet(SnapshotSetId: TGUID): HResult; StdCall;
    function GetSnapshotProperties(SnapshotId: TGUID;
      var pProp: TVssSnapshotProp): HResult; StdCall;
    function Query(QueriedObjectId: TGUID;
      eQueriedObjectType, eReturnedObjectsType: VSS_OBJECT_TYPE;
      out ppenum: IVssEnumObject): HResult; StdCall;
    function IsVolumeSupported(ProviderId: TGUID; pwszVolumeName: PWideChar;
      var pbSupportedByThisProvider: Bool): HResult; StdCall;
    function DisableWriterClasses(rgWriterClassId: PGUIDLIST; cClassId: UINT)
      : HResult; StdCall;
    function EnableWriterClasses(rgWriterClassId: PGUIDLIST; cClassId: UINT)
      : HResult; StdCall;
    function DisableWriterInstances(rgWriterInstanceId: PGUIDLIST;
      cInstanceId: UINT): HResult; StdCall;
    function ExposeSnapshot(SnapshotId: TGUID; wszPathFromRoot: PWideChar;
      lAttributes: LONG; wszExpose: PWideChar; var pwszExposed: PWideChar)
      : HResult; StdCall;
    function RevertToSnapshot(SnapshotId: TGUID; bForceDismount: Bool)
      : HResult; StdCall;
    function QueryRevertStatus(pwszVolume: PWideChar; out ppAsync: IVssAsync)
      : HResult; StdCall;
  end;

  IVssBackupComponentsEx = Interface(IVssBackupComponents)
    ['{963f03ad-9e4c-4a34-ac15-e4b6174e5036}']
    // get writer metadata for a specific writer
    function GetWriterMetadataEx(iWriter: UINT; var pidInstance: TGUID;
      out ppMetadata: IVssExamineWriterMetadataEx): HResult; StdCall;
    // indicate that a given component is selected to be restored
    function SetSelectedForRestoreEx(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar; bSelectedForRestore: Bool;
      instanceId: TGUID): HResult; StdCall;
  end;

  IVssBackupComponentsEx2 = Interface(IVssBackupComponentsEx)
    ['{acfe2b3a-22c9-4ef8-bd03-2f9ca230084e}']
    function UnexposeSnapshot(SnapshotId: TGUID): HResult; StdCall;
    function SetAuthoritativeRestore(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar; bAuth: Bool)
      : HResult; StdCall;
    function SetRollForward(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName: PWideChar;
      rollType: VSS_ROLLFORWARD_TYPE; wszRollForwardPoint: PWideChar)
      : HResult; StdCall;
    function SetRestoreName(writerId: TGUID; ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName, wszRestoreName: PWideChar)
      : HResult; StdCall;
    function BreakSnapshotSetEx(SnapshotSetId: TGUID; dwBreakFlags: DWORD;
      out ppAsync: IVssAsync): HResult; StdCall;
    function PreFastRecovery(SnapshotSetId: TGUID;
      dwPreFastRecoveryFlags: DWORD; out ppAsync: IVssAsync): HResult; StdCall;
    function FastRecovery(SnapshotSetId: TGUID; dwPreFastRecoveryFlags: DWORD;
      out ppAsync: IVssAsync): HResult; StdCall;
  end;

  IVssBackupComponentsEx3 = Interface(IVssBackupComponentsEx2)
    ['{c191bfbc-b602-4675-8bd1-67d642f529d5}']
    function GetWriterStatusEx(iWriter: UINT; var pidInstance, pidWriter: TGUID;
      var pbstrWriter: PWideChar; var pnStatus: VSS_WRITER_STATE;
      var phrFailureWriter: HResult; var phrApplication: HResult;
      var pbstrApplicationMessage: PWideChar): HResult; StdCall;
    function AddSnapshotToRecoverySet(SnapshotSetId: TGUID; dwFlags: DWORD;
      pwszDestinationVolume: PWideChar = nil): HResult; StdCall;
    function RecoverSet(dwFlags: DWORD; out ppAsync: IVssAsync)
      : HResult; StdCall;
    function GetSessionId(var idSession: TGUID): HResult; StdCall;
  end;

  // +----------------------------------------------------------------------------
  //
  // Interface:  IVssComponent
  // -----------------------------------------------------------------------------
  IVssComponent = Interface(IUnknown)
    ['{d2c72c96-c121-4518-b627-e5a93d010ead}']
    // obtain logical path of component
    function GetLogicalPath(var pbstrPath: PWideChar): HResult; StdCall;

    // obtain component type(VSS_CT_DATABASE or VSS_CT_FILEGROUP)
    function GetComponentType(pct: VSS_COMPONENT_TYPE): HResult; StdCall;

    // get component name
    function GetComponentName(var pbstrName: PWideChar): HResult; StdCall;

    // determine whether the component was successfully backed up.
    function GetBackupSucceeded(var pbSucceeded: Bool): HResult; StdCall;

    // get altermative location mapping count
    function GetAlternateLocationMappingCount(var pcMapping: UINT)
      : HResult; StdCall;

    // get a paraticular alternative location mapping
    function GetAlternateLocationMapping(iMapping: UINT;
      out ppFiledesc: IVssWMFiledesc): HResult; StdCall;

    // set the backup metadata for a component
    function SetBackupMetadata(wszData: PWideChar): HResult; StdCall;

    // get the backup metadata for a component
    function GetBackupMetadata(var pbstrData: PWideChar): HResult; StdCall;

    // indicate that only ranges in the file are to be backed up
    function AddPartialFile(wszPath, wszFileName, wszRanges,
      wszMetadata: PWideChar): HResult; StdCall;

    // get count of partial file declarations
    function GetPartialFileCount(var pcPartialFiles: UINT): HResult; StdCall;

    // get a partial file declaration
    function GetPartialFile(iPartialFile: UINT; var pbstrPath, pbstrFilename,
      pbstrRange, pbstrMetadata: PWideChar): HResult; StdCall;

    // determine if the component is selected to be restored
    function IsSelectedForRestore(var pbSelectedForRestore: Bool)
      : HResult; StdCall;

    function GetAdditionalRestores(var pbAdditionalRestores: Bool)
      : HResult; StdCall;

    // get count of new target specifications
    function GetNewTargetCount(var pcNewTarget: UINT): HResult; StdCall;

    function GetNewTarget(iNewTarget: UINT; out ppFiledesc: IVssWMFiledesc)
      : HResult; StdCall;

    // add a directed target specification
    function AddDirectedTarget(wszSourcePath, wszSourceFilename,
      wszSourceRangeList, wszDestinationPath, wszDestinationFilename,
      wszDestinationRangeList: PWideChar): HResult; StdCall;

    // get count of directed target specifications
    function GetDirectedTargetCount(pcDirectedTarge: UINT): HResult; StdCall;

    // obtain a particular directed target specification
    function GetDirectedTarget(iDirectedTarget: UINT;
      var pbstrSourcePath, pbstrSourceFileName, pbstrSourceRangeList,
      pbstrDestinationPath, pbstrDestinationFilename, pbstrDestinationRangeList
      : PWideChar): HResult; StdCall;

    // set restore metadata associated with the component
    function SetRestoreMetadata(wszRestoreMetadata: PWideChar)
      : HResult; StdCall;

    // obtain restore metadata associated with the component
    function GetRestoreMetadata(var pbstrRestoreMetadata: PWideChar)
      : HResult; StdCall;

    // set the restore target
    function SetRestoreTarget(target: VSS_RESTORE_TARGET): HResult; StdCall;

    // obtain the restore target
    function GetRestoreTarget(var pTarget: VSS_RESTORE_TARGET)
      : HResult; StdCall;

    // set failure message during pre restore event
    function SetPreRestoreFailureMsg(wszPreRestoreFailureMsg: PWideChar)
      : HResult; StdCall;

    // obtain failure message during pre restore event
    function GetPreRestoreFailureMsg(var pbstrPreRestoreFailureMsg: PWideChar)
      : HResult; StdCall;

    // set the failure message during the post restore event
    function SetPostRestoreFailureMsg(wszPostRestoreFailureMsg: PWideChar)
      : HResult; StdCall;

    // obtain the failure message set during the post restore event
    function GetPostRestoreFailureMsg(var pbstrPostRestoreFailureMsg: PWideChar)
      : HResult; StdCall;

    // set the backup stamp of the backup
    function SetBackupStamp(wszBackupStamp: PWideChar): HResult; StdCall;

    // obtain the stamp of the backup
    function GetBackupStamp(var pbstrBackupStamp: PWideChar): HResult; StdCall;

    // obtain the backup stamp that the differential or incremental
    // backup is baed on
    function GetPreviousBackupStamp(var pbstrBackupStamp: PWideChar)
      : HResult; StdCall;

    // obtain backup options for the writer
    function GetBackupOptions(var pbstrBackupOptions: PWideChar)
      : HResult; StdCall;

    // obtain the restore options
    function GetRestoreOptions(var pbstrRestoreOptions: PWideChar)
      : HResult; StdCall;

    // obtain count of subcomponents to be restored
    function GetRestoreSubcomponentCount(var pcRestoreSubcomponent: UINT)
      : HResult; StdCall;

    // obtain a particular subcomponent to be restored
    function GetRestoreSubcomponent(iComponent: UINT;
      var pbstrLogicalPath, pbstrComponentName: PWideChar; var pbRepair: Bool)
      : HResult; StdCall;

    // obtain whether files were successfully restored
    function GetFileRestoreStatus(var pStatus: VSS_FILE_RESTORE_STATUS)
      : HResult; StdCall;

    // add differenced files by last modify time
    function AddDifferencedFilesByLastModifyTime(wszPath,
      wszFilespec: PWideChar; bRecursive: Bool; ftLastModifyTime: FILETIME)
      : HResult; StdCall;

    function AddDifferencedFilesByLastModifyLSN(wszPath, wszFilespecFILETIME
      : PWideChar; bRecursive: Bool; bstrLsnStringbRecursive: Bool)
      : HResult; StdCall;

    function GetDifferencedFilesCount(var pcDifferencedFiles: UINT)
      : HResult; StdCall;

    function GetDifferencedFile(iDifferencedFile: UINT;
      var pbstrPath, pbstrFilespec: PWideChar; var pbRecursive: Bool;
      var pbstrLsnString: PWideChar; var pftLastModifyTime: FILETIME)
      : HResult; StdCall;
  end;

  IVssComponentEx = Interface(IVssComponent)
    ['{156c8b5e-f131-4bd7-9c97-d1923be7e1fa}']
    // set a failure message during prepareforbackup
    function SetPrepareForBackupFailureMsg(wszFailureMsg: PWideChar)
      : HResult; StdCall;

    // set a failure message during postsnapshot
    function SetPostSnapshotFailureMsg(wszFailureMsg: PWideChar)
      : HResult; StdCall;

    // get the failure message set during prepareforbackup
    function GetPrepareForBackupFailureMsg(var pbstrFailureMsg: PWideChar)
      : HResult; StdCall;

    // get the failure message set during postsnapshot
    function GetPostSnapshotFailureMsg(var pbstrFailureMsg: PWideChar)
      : HResult; StdCall;

    // get the authoritative restore status
    function GetAuthoritativeRestore(var pbAuth: Bool): HResult; StdCall;

    // get the roll-forward status
    function GetRollForward(var pRollType: VSS_ROLLFORWARD_TYPE;
      pbstrPoint: PWideChar): HResult; StdCall;

    // get the restore name
    function GetRestoreName(var pbstrName: PWideChar): HResult; StdCall;
  end;

  IVssComponentEx2 = Interface(IVssComponentEx)
    ['{3b5be0f2-07a9-4e4b-bdd3-cfdc8e2c0d2d}']
    function SetFailure(hr, hrApplication: HResult;
      wszApplicationMessage: PWideChar; dwReserved: DWORD): HResult; StdCall;

    function GetFailure(var phr, phrApplication: HResult;
      var pbstrApplicationMessage: PWideChar; var pdwReserved: DWORD)
      : HResult; StdCall;
  end;

  IVssCreateWriterMetadata = interface(IUnknown)
    // add files to include to metadata document
    function AddIncludeFiles(wszPath, wszFilespec: PWideChar; bRecursive: Bool;
      wszAlternateLocation: PWideChar): HResult; StdCall;

    // add files to exclude to metadata document
    function AddExcludeFiles(wszPath, wszFilespec: PWideChar; bRecursive: Bool)
      : HResult; StdCall;

    // add component to metadata document
    function AddComponent(ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName, wszCaption: PWideChar;
      const pbIcon: BYTE; cbIcon: UINT;
      bRestoreMetadata, bNotifyOnBackupComplete, bSelectable: Bool;
      bSelectableForRestore: Bool = false; dwComponentFlags: DWORD = 0)
      : HResult; StdCall;

    // add physical database files to a database component
    function AddDatabaseFiles(wszLogicalPath, wszDatabaseName, wszPath,
      wszFilespec: PWideChar;
      dwBackupTypeMask: DWORD = (VSS_FSBT_ALL_BACKUP_REQUIRED or
      VSS_FSBT_ALL_SNAPSHOT_REQUIRED)): HResult; StdCall;

    // add log files to a database component
    function AddDatabaseLogFiles(wszLogicalPath, wszDatabaseName, wszPath,
      wszFilespec: PWideChar;
      dwBackupTypeMask: DWORD = (VSS_FSBT_ALL_BACKUP_REQUIRED or
      VSS_FSBT_ALL_SNAPSHOT_REQUIRED)): HResult; StdCall;

    // add files to a FILE_GROUP component
    function AddFilesToFileGroup(wszLogicalPath, wszGroupName, wszPath,
      wszFilespec: PWideChar; bRecursive: Bool; wszAlternateLocation: PWideChar;
      dwBackupTypeMask: DWORD = (VSS_FSBT_ALL_BACKUP_REQUIRED or
      VSS_FSBT_ALL_SNAPSHOT_REQUIRED)): HResult; StdCall;

    // create a restore method
    function SetRestoreMethod(method: VSS_RESTOREMETHOD_ENUM;
      wszService, wszUserProcedure: PWideChar;
      writerRestore: VSS_WRITERRESTORE_ENUM; bRebootRequired: Bool)
      : HResult; StdCall;

    // add alternative location mappings to the restore method
    function AddAlternateLocationMapping(wszSourcePath, wszSourceFilespec
      : PWideChar; bRecursive: Bool; wszDestination: PWideChar)
      : HResult; StdCall;

    // add a dependency to another writer's component
    function AddComponentDependency(wszForLogicalPath, wszForComponentName
      : PWideChar; onWriterId: TGUID; wszOnLogicalPath, wszOnComponentName
      : PWideChar): HResult; StdCall;

    // Set the schema used during backup
    function SetBackupSchema(dwSchemaMask: DWORD): HResult; StdCall;

    // obtain reference to actual XML document
    function GetDocument(pDoc: IXMLDOMDocument): HResult; StdCall;

    // save document as an XML string
    function SaveAsXML(pbstrXML: PWideChar): HResult; StdCall;

  end;

  IVssCreateWriterMetadataEx = interface(IVssCreateWriterMetadata)
    ['{9f21981d-d469-4349-b807-39e64e4674e1}']
    function AddExcludeFilesFromSnapshot(wszPath, wszFilespec: PWideChar;
      bRecursive: Bool): HResult; StdCall;
  end;

  IVssWriterImpl = interface(IUnknown)
    // initialize writer
    function Initialize(writerId: TGUID;
      wszWriterName, wszWriterInstanceName: PWideChar;
      dwMajorVersion, dwMinorVersion: DWORD; ut: VSS_USAGE_TYPE;
      st: VSS_SOURCE_TYPE; nLevel: VSS_APPLICATION_LEVEL; dwTimeout: DWORD;
      aws: VSS_ALTERNATE_WRITER_STATE; bIOThrottlingOnly: Bool)
      : HResult; StdCall;

    // subscribe to events
    function Subscribe(dwSubscribeTimeout, dwEventFlags: DWORD)
      : HResult; StdCall;

    // unsubscribe from events
    function Unsubscribe: HResult; StdCall;

    procedure Uninitialize; StdCall;

    // get array of volume names
    function GetCurrentVolumeArray: PWideChar; StdCall;

    // get # of volumes in volume array
    function GetCurrentVolumeCount: UINT; StdCall;

    // get the snapshot device name for a particular volume
    function GetSnapshotDeviceName(wszOriginalVolume: PWideChar;
      var ppwszSnapshotDevice: PWideChar): HResult; StdCall;

    // get id of snapshot set
    function GetCurrentSnapshotSetId: TGUID; StdCall;

    // get the current backup context
    function GetContext: LONG; StdCall;

    // determine which Freeze event writer responds to
    function GetCurrentLevel: VSS_APPLICATION_LEVEL; StdCall;

    // determine if path is included in the snapshot
    function IsPathAffected(wszPath: PWideChar): Bool; StdCall;

    // determine if bootable state is backed up
    function IsBootableSystemStateBackedUp: Bool; StdCall;

    // determine if the backup application is selecting components
    function AreComponentsSelected: Bool; StdCall;

    // determine the backup type for the backup
    function GetBackupType: VSS_BACKUP_TYPE; StdCall;

    // determine the type of restore
    function GetRestoreType: VSS_RESTORE_TYPE; StdCall;

    // let writer pass back indication of reason for failure
    function SetWriterFailure(hr: HResult): HResult; StdCall;

    // determine if requestor support partial file backups
    function IsPartialFileSupportEnabled: Bool; StdCall;

    function InstallAlternateWriter(idWriter: TGUID; clsid: TCLSID)
      : HResult; StdCall;

    // determine the current identity information
    function GetIdentityInformation: IVssExamineWriterMetadata; StdCall;

    // let writer pass back indication of reason for failure
    function SetWriterFailureEx(hr, hrApplication: HResult;
      wszApplicationMessage: PWideChar): HResult; StdCall;

    function GetSessionId(var idSession: TGUID): HResult; StdCall;

    function IsWriterShuttingDown: Bool; StdCall;
  end;

  IVssCreateExpressWriterMetadata = interface(IUnknown)
    ['{9c772e77-b26e-427f-92dd-c996f41ea5e3}']
    // add files to exclude to metadata document
    function AddExcludeFiles(wszPath, wszFilespec: PWideChar; bRecursive: Bool)
      : HResult; StdCall;

    // add component to metadata document
    function AddComponent(ct: VSS_COMPONENT_TYPE;
      wszLogicalPath, wszComponentName, wszCaption: PWideChar; pbIcon: BYTE;
      cbIcon: UINT; bRestoreMetadata, bNotifyOnBackupComplete,
      bSelectable: Bool; bSelectableForRestore: Bool = false;
      dwComponentFlags: DWORD = 0): HResult; StdCall;

    // add files to a FILE_GROUP component
    function AddFilesToFileGroup(wszLogicalPath, wszGroupName, wszPath,
      wszFilespec: PWideChar; bRecursive: Bool; wszAlternateLocation: PWideChar;
      dwBackupTypeMask: DWORD = (VSS_FSBT_ALL_BACKUP_REQUIRED or
      VSS_FSBT_ALL_SNAPSHOT_REQUIRED)): HResult; StdCall;

    // create a restore method
    function SetRestoreMethod(method: VSS_RESTOREMETHOD_ENUM;
      wszService, wszUserProcedure: PWideChar;
      writerRestore: VSS_WRITERRESTORE_ENUM; bRebootRequired: Bool)
      : HResult; StdCall;

    // add a dependency to another writer's component
    function AddComponentDependency(wszForLogicalPath, wszForComponentName
      : PWideChar; onWriterId: TGUID; wszOnLogicalPath, wszOnComponentName
      : PWideChar): HResult; StdCall;

    // Set the schema used during backup
    function SetBackupSchema(dwSchemaMask: DWORD): HResult; StdCall;

    // save document as an XML string
    function SaveAsXML(pbstrXML: PWideChar): HResult; StdCall;
  end;

  IVssExpressWriter = interface(IUnknown)
    ['{e33affdc-59c7-47b1-97d5-4266598f6235}']
    function CreateMetadata(writerId: TGUID; writerName: PWideChar;
      usageType: VSS_USAGE_TYPE; versionMajor, versionMinor, reserved: DWORD;
      out ppMetadata: IVssCreateExpressWriterMetadata): HResult; StdCall;

    function LoadMetadata(metadata: PWideChar; reserved: DWORD)
      : HResult; StdCall;

    function Register: HResult; StdCall;

    function Unregister(writerId: TGUID): HResult; StdCall;
  end;

  /// //////////////////////////////////////////////////////////////////////////
  // CVssWriter

  TCVssWriter = class
  end;

  // ['{}']

  // backup writer components interface (i.e., all components for an
  // individual writer
  // IVssWriterComponents = class
  {
    public:
    // get count of components
    function GetComponentCount(OUT UINT *pcComponents) : HRESULT; StdCall;

    // get information about the writer
    function GetWriterInfo)
    (
    OUT VSS_ID *pidInstance,
    OUT VSS_ID *pidWriter
    ) : HRESULT; StdCall;

    // obtain a specific component
    function GetComponent)
    (
    IN UINT iComponent,
    OUT IVssComponent **ppComponent
    ) : HRESULT; StdCall;
  }


  // function CreateVssBackupComponents(
  // var ppBackup : IVssBackupComponents) : HRESULT; StdCall;
  // External VSS_API_DLL name 'CreateVssBackupComponentsInternal'; { for Windows Server 2003 }

  // function IsVolumeSnapshotted(
  // pwszVolumeName : PWideChar; var pbSnapshotsPresent : Bool;
  // var plSnapshotCapability : Integer) : HRESULT; StdCall;
  // External VSS_API_DLL;

function CreateVssBackupComponents(out ppBackup: IVssBackupComponents)
  : HResult; stdcall;
procedure VssFreeSnapshotProperties(pProp: PVssSnapshotProp); stdcall;

function ClusterIsPathOnSharedVolume(lpszPathName: PWideChar): Bool;
WinApi;
function ClusterGetVolumePathName(lpszFileName: PWideChar;
  lpszVolumePathName: PWideChar; cchBufferLength: DWORD): Bool;
WinApi;
function ClusterGetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint
  : PWideChar; lpszVolumeName: PWideChar; cchBufferLength: DWORD): Bool;
WinApi;

function IsVssAvailable: boolean;

implementation

type
  TCreateVssBackupComponents = function(out ppBackup: IVssBackupComponents)
    : HResult; stdcall;
  TVssFreeSnapshotProperties = procedure(pProp: PVssSnapshotProp); stdcall;

  TClusterIsPathOnSharedVolume = function(lpszPathName: PWideChar): Bool;
WinApi;
TClusterGetVolumePathName =
function(lpszFileName: PWideChar; lpszVolumePathName: PWideChar;
  cchBufferLength: DWORD): Bool;
WinApi;
TClusterGetVolumeNameForVolumeMountPoint =
function(lpszVolumeMountPoint: PWideChar; lpszVolumeName: PWideChar;
  cchBufferLength: DWORD): Bool;
WinApi;

var
  VssApiHandle: THandle;
  ResApiHandle: THandle;
  FIsVssAvailable: boolean;
  FCreateVssBackupComponents: TCreateVssBackupComponents;
  FVssFreeSnapshotProperties: TVssFreeSnapshotProperties;
  FClusterIsPathOnSharedVolume: TClusterIsPathOnSharedVolume;
  FClusterGetVolumePathName: TClusterGetVolumePathName;
  FClusterGetVolumeNameForVolumeMountPoint
    : TClusterGetVolumeNameForVolumeMountPoint;

  // function CreateVssBackupComponents; external VssApiDll name 'CreateVssBackupComponentsInternal';
  // procedure VssFreeSnapshotProperties; external VssApiDll name 'VssFreeSnapshotPropertiesInternal';

function CreateVssBackupComponents(out ppBackup: IVssBackupComponents): HResult;
begin
  Result := 0;
  if assigned(FCreateVssBackupComponents) then
    Result := FCreateVssBackupComponents(ppBackup);
end;

procedure VssFreeSnapshotProperties(pProp: PVssSnapshotProp);
begin
  if assigned(FVssFreeSnapshotProperties) then
    FVssFreeSnapshotProperties(pProp);
end;

function ClusterIsPathOnSharedVolume(lpszPathName: PWideChar): Bool;
begin
  if assigned(@FClusterIsPathOnSharedVolume) then
    Result := FClusterIsPathOnSharedVolume(lpszPathName)
  else
    Result := false;
end;

function ClusterGetVolumePathName(lpszFileName: PWideChar;
  lpszVolumePathName: PWideChar; cchBufferLength: DWORD): Bool;
begin
  if assigned(@FClusterGetVolumePathName) then
    Result := FClusterGetVolumePathName(lpszFileName, lpszVolumePathName,
      cchBufferLength)
  else
    Result := false;
end;

function ClusterGetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint
  : PWideChar; lpszVolumeName: PWideChar; cchBufferLength: DWORD): Bool;
begin
  if assigned(@FClusterGetVolumePathName) then
    Result := FClusterGetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint,
      lpszVolumeName, cchBufferLength)
  else
    Result := false;
end;

function IsVssAvailable: boolean;
begin
  Result := FIsVssAvailable;
end;

{ ---------------------------------------------------------------- }
initialization

VssApiHandle := GetModuleHandle(VssApiDll);
if VssApiHandle = 0 then
  VssApiHandle := LoadLibrary(VssApiDll);
if VssApiHandle <> 0 then
begin // following entries are not available on Windows XP
  @FCreateVssBackupComponents := GetProcAddress(VssApiHandle,
    'CreateVssBackupComponentsInternal');
  @FVssFreeSnapshotProperties := GetProcAddress(VssApiHandle,
    'VssFreeSnapshotPropertiesInternal');
  FIsVssAvailable := assigned(FCreateVssBackupComponents) and
    assigned(FVssFreeSnapshotProperties);
end
else
  FIsVssAvailable := false;
ResApiHandle := GetModuleHandle(ResApiDll);
if ResApiHandle = 0 then
  ResApiHandle := LoadLibrary(ResApiDll);
if ResApiHandle <> 0 then
begin // only Windows Server 2008/12 R2 Enterprise, R2 Datacenter
  @FClusterIsPathOnSharedVolume := GetProcAddress(ResApiHandle,
    'ClusterIsPathOnSharedVolume');
  @FClusterGetVolumePathName := GetProcAddress(ResApiHandle,
    'ClusterGetVolumePathName');
  @FClusterGetVolumeNameForVolumeMountPoint := GetProcAddress(ResApiHandle,
    'ClusterGetVolumeNameForVolumeMountPoint');
end;

end.
