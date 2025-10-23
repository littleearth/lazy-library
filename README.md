# Lazy Library

A comprehensive Delphi library of reusable components, utilities, and REST API clients designed to make development easier and more productive.

## Overview

Lazy Library provides a robust collection of cross-platform tools organized into three main categories:

- **CORE**: Platform-independent components including utilities, REST clients, logging, and networking
- **VCL**: Windows-specific VCL components including WinAPI wrappers and forms
- **FMX**: FireMonkey cross-platform components and forms

## Key Features

### Core Utilities
- **String & Data Manipulation**: Comprehensive string utilities, CSV/JSON conversion, field name case conversion
- **Date/Time**: ISO8601 support, UTC conversion, duration calculations, time rounding
- **Encoding**: Base64, MD5 hashing, encrypted INI files, NATO phonetic alphabet
- **Models**: Generic model base classes with JSON serialization/deserialization
- **Nullable Types**: Generic nullable type support with helpers for common types
- **Threading**: Thread-safe file streams and string lists
- **Password Generation**: Secure password and passphrase generation with hashing
- **Comparison**: Version comparison, date/time/float/integer comparison utilities
- **File Operations**: File utilities, temp file management, directory operations

### REST API Clients
Pre-built OAuth2 and Basic Auth clients for popular services:
- **Microsoft Graph API**: Complete implementation for M365 services (users, groups, mail, sites, drives, audit logs)
- **Azure Management**: Azure resource management via REST API
- **DUO Security**: Two-factor authentication integration (Admin, Auth, and Accounts APIs)
- **UniFi Controller**: Ubiquiti UniFi network management
- **Pulseway (PWA)**: Remote monitoring and management notifications
- **SASBOSS**: Billing and provisioning APIs
- **Office 365 WebHooks**: Incoming webhook messages for Teams/Office 365

### Logging
- **Multi-Handler Architecture**: Register multiple logging handlers that process messages simultaneously
- **Enhanced Exception Logging**: Automatic extraction of comprehensive exception details when logging errors
  - Exception class, message, and address
  - Application context (executable path, version, product info)
  - UI state (active form and control details)
  - Stack traces with source location (via extensible handlers for JclDebug, MadExcept, EurekaLog)
  - Automatic integration when using `LazyLog.Error(Self, E)` - no extra code needed
- **TLazyLogMessage**: Unified message object passed to all handlers with application name, thread ID, timestamps, exception details, and stack traces
- **TLZExceptionDetails**: Extensible exception information extractor with handler interface for third-party tools
- **TLazyLogAction**: Real-time progress tracking for long-running operations - query active actions by thread
- **TLZLogHandler**: Abstract base class for custom logging implementations (override `ProcessLogMessage`)
- **Built-in Handlers**:
  - **TLZLogFileStream**: File-based logging with automatic flushing and duration tracking
  - **TLZLogMemory**: In-memory thread-safe logging (5000 line cache, configurable)
  - **TLZLogSQLite**: SQLite database logging with FireDAC (queryable, auto-purge, separate tables for messages and actions)
  - **TLZLogDebugger**: IDE debugger output via `OutputDebugString`
  - **TLZLogNTEventLog**: Windows Event Log integration (errors and warnings only)
  - **TLZLogConsole**: Console output with color support
  - **TLZLogSysLog**: Syslog network logging
- **Format Support**: All logging methods support `Format` arguments for easy string formatting
- **Progress Tracking**: `LogActionBegin`, `LogActionProgress`, and `LogActionEnd` with automatic duration calculation
- **Action Queries**: Get active actions via `GetAction`, `GetAllActions`, or `GetCurrentThreadActions`
- **Thread-Safe**: All handler management, file operations, and action tracking are thread-safe
- **LazyLog**: Global logging interface with dispatcher pattern

### Networking
- **Network Tools**: IP address utilities, network scanning, ping functionality
- **SNMP Client**: Simple SNMP operations

### Windows API Components (VCL)
- **System Information**: Hardware and OS information retrieval
- **File Version Information**: Read version info from executables
- **Volume Shadow Copy (VSS)**: Backup and snapshot operations
- **WTS Session Change**: Windows Terminal Services session monitoring
- **Named Pipes**: Server and client implementations
- **Directory Watch**: File system change monitoring
- **Device Notifier**: USB and device arrival/removal notifications
- **ICMP/Ping**: Native Windows ICMP implementation
- **Windows Services**: NT Service management
- **RunAs**: Execute processes with elevated privileges
- **Network List**: Network adapter and connection enumeration
- **Windows Messages**: Custom window message handling

### Forms
- **OAuth Authorization Browser**: Embedded browser for OAuth flows
- **DUO Pre-Authentication**: DUO Security pre-auth UI

## Installation

### Manual Installation

1. Add the library path to your Delphi Library Path
2. For design-time components, install the appropriate package:
   - Delphi 11.x: `packages\11.0\LazyLibrary.dpk`
   - Delphi 12.x: `packages\12.0\LazyLibrary.dpk`

### Package Structure

- **LazyLibrary.dpk**: Core components (platform-independent)
- **LazyLibraryVCL.dpk**: VCL-specific components
- **LazyLibraryCoreDesign.dpk**: Design-time components

## Quick Start

### Basic Usage

```pascal
uses
  Lazy.Utils, Lazy.Types, Lazy.ISO8601;

var
  LDate: TDateTime;
  LPassword: string;
begin
  // String utilities
  if not TLZString.IsEmptyString(MyString) then
    ShowMessage(TLZString.TitleCase(MyString));

  // Date/Time
  LDate := TLZIso8601.DateTimeFromIso8601('2024-01-15T10:30:00Z');
  
  // Password generation
  LPassword := TLZPasswords.Generate(16, true, true, true, '!@#$');
end;
```

### REST API Example

```pascal
uses
  MSGraph.API, MSGraph.Models;

var
  LMSGraph: TLZMSGraphAPI;
  LUsers: TLZMSGraphUsers;
begin
  LMSGraph := TLZMSGraphAPI.Create;
  try
    // Configure OAuth2 connection
    LMSGraph.Connection.ClientID := 'your-client-id';
    LMSGraph.Connection.TenantID := 'your-tenant-id';
    
    // Get users
    LUsers := TLZMSGraphUsers.Create;
    try
      LMSGraph.GetUsers(LUsers);
      // Process users...
    finally
      LUsers.Free;
    end;
  finally
    LMSGraph.Free;
  end;
end;
```

### Logging Example

```pascal
uses
  Lazy.Types, Lazy.Log, Lazy.Log.FileStream, Lazy.Exception.Details;

begin
  // Set application name
  TLZLog.ApplicationName := 'MyApplication';
  
  // Add logging handlers
  LazyLog.AddHandler('FileLog', TLZLogFileStream);
  LazyLog.AddHandler('Memory', TLZLogMemory);
  
  // Use in your classes derived from TLZObject/TLZComponent
  Log('Application started');
  Warning('Configuration not found, using defaults');
  Error('Connection failed', 1001);
  
  // Enhanced exception logging - automatically includes comprehensive details
  try
    ProcessData;
  except
    on E: Exception do
      LazyLog.Error(Self, E);  // Includes exception details, stack trace, app context
  end;
  
  // Optional: Register JclDebug handler for full stack traces with line numbers
  // TLZExceptionDetails.RegisterHandler(TJclDebugHandler.Create);
end;
```

## Documentation

For detailed API documentation and examples, see:
- **LAZYLIBRARY.MD**: Complete component catalog with all public/published members

## Examples

Example projects are included in the `example` directory demonstrating:
- Azure Management (VCL & FMX)
- SharePoint integration (VCL & FMX)
- CSV/JSON conversion
- DUO authentication
- MS Graph API usage
- OAuth flows
- Pulseway integration
- SASBOSS billing
- Stopwatch usage
- System information
- UniFi network management
- VSS Copy operations

## Requirements

- Embarcadero Delphi 11.0 or later (tested with Athens)
- Indy components (included with Delphi)
- For VCL components: Windows platform

## License

Copyright (c) Tristan David Marlow, Little Earth Solutions

## Author

Tristan Marlow

## Contributing

Contributions are welcome! This library is designed to be extended with new utilities and API clients.
