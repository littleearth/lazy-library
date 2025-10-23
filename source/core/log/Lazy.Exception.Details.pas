{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Exception.Details
  Author: Tristan Marlow
  Purpose: Enhanced exception details with stack trace and context information

  Overview:
  This unit provides comprehensive exception detail extraction including
  stack traces, source location information, and application context.
  It supports extensible handlers for different debugging tools like
  JclDebug, MadExcept, etc.

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ---------------------------------------------------------------------------- }

unit Lazy.Exception.Details;

interface

uses
  System.SysUtils,
  System.Classes,
{$IFDEF MSWINDOWS}
{$IF DEFINED(VCL)}
  Vcl.Forms,
  Vcl.Controls,
{$ELSEIF DEFINED(FMX)}
  FMX.Forms,
  FMX.Controls,
{$ENDIF}
{$ENDIF}
  Lazy.Types;

type
  /// <summary>
  /// Forward declaration for the exception details class
  /// </summary>
  TLZExceptionDetails = class;

  /// <summary>
  /// Handler interface that can be implemented to enhance exception details
  /// using external tools like JclDebug, MadExcept, etc.
  /// </summary>
  ILZExceptionHandler = interface
    ['{8B5A3C2D-4E1F-4A2B-9F3E-7C8D1E2F3A4B}']
    /// <summary>
    /// Called to enhance exception details with additional information
    /// </summary>
    procedure EnhanceExceptionDetails(const ADetails: TLZExceptionDetails);
    /// <summary>
    /// Returns the handler name for identification
    /// </summary>
    function GetHandlerName: string;
  end;

  /// <summary>
  /// Class that expands exception information with detailed context
  /// </summary>
  TLZExceptionDetails = class(TLZObject)
  private
    FSourceException: Exception;
    FExecutable: string;
    FApplicationTitle: string;
    FProductName: string;
    FFileVersion: string;
    FProductVersion: string;
    FModuleName: string;
    FProcedureName: string;
    FSourceName: string;
    FLineNumber: Integer;
    FStackTrace: string;
    FSenderClass: string;
    FExceptionClass: string;
    FAddress: string;
    FActiveFormClass: string;
    FActiveFormCaption: string;
    FActiveControlClass: string;
    FActiveControlName: string;
    FExceptionMessage: string;
    FTimestamp: TDateTime;
    procedure ExtractBasicInformation(const ASender: TObject;
      const AExceptAddr: Pointer);
    procedure ExtractVersionInformation;
    procedure ExtractActiveControlInformation;
    procedure ApplyRegisteredHandlers;
  public
    /// <summary>
    /// Creates a new exception details instance with comprehensive information
    /// </summary>
    constructor Create(const AException: Exception;
      const ASender: TObject = nil; const AExceptAddr: Pointer = nil);
      reintroduce;
    destructor Destroy; override;

    /// <summary>
    /// Registers a custom exception handler for enhancement
    /// </summary>
    class procedure RegisterHandler(const AHandler: ILZExceptionHandler);
    /// <summary>
    /// Unregisters a custom exception handler
    /// </summary>
    class procedure UnregisterHandler(const AHandler: ILZExceptionHandler);
    /// <summary>
    /// Clears all registered handlers
    /// </summary>
    class procedure ClearHandlers;

    /// <summary>
    /// Returns a formatted string representation of the exception details
    /// </summary>
    function AsString: string;
    /// <summary>
    /// Returns exception details as JSON format
    /// </summary>
    function AsJSON: string;

    /// <summary>
    /// The original exception object
    /// </summary>
    property SourceException: Exception read FSourceException;
    /// <summary>
    /// Path to the executable file
    /// </summary>
    property Executable: string read FExecutable write FExecutable;
    /// <summary>
    /// Title of the application
    /// </summary>
    property ApplicationTitle: string read FApplicationTitle
      write FApplicationTitle;
    /// <summary>
    /// Product name from version information
    /// </summary>
    property ProductName: string read FProductName write FProductName;
    /// <summary>
    /// File version from version information
    /// </summary>
    property FileVersion: string read FFileVersion write FFileVersion;
    /// <summary>
    /// Product version from version information
    /// </summary>
    property ProductVersion: string read FProductVersion write FProductVersion;
    /// <summary>
    /// Name of the module where the exception occurred
    /// </summary>
    property ModuleName: string read FModuleName write FModuleName;
    /// <summary>
    /// Name of the procedure where the exception occurred
    /// </summary>
    property ProcedureName: string read FProcedureName write FProcedureName;
    /// <summary>
    /// Name of the source file where the exception occurred
    /// </summary>
    property SourceName: string read FSourceName write FSourceName;
    /// <summary>
    /// Line number where the exception occurred
    /// </summary>
    property LineNumber: Integer read FLineNumber write FLineNumber;
    /// <summary>
    /// Complete stack trace information
    /// </summary>
    property StackTrace: string read FStackTrace write FStackTrace;
    /// <summary>
    /// Class name of the object that generated the exception
    /// </summary>
    property SenderClass: string read FSenderClass write FSenderClass;
    /// <summary>
    /// Class name of the exception
    /// </summary>
    property ExceptionClass: string read FExceptionClass write FExceptionClass;
    /// <summary>
    /// Memory address where the exception occurred
    /// </summary>
    property Address: string read FAddress write FAddress;
    /// <summary>
    /// Class name of the active form when the exception occurred
    /// </summary>
    property ActiveFormClass: string read FActiveFormClass
      write FActiveFormClass;
    /// <summary>
    /// Caption of the active form when the exception occurred
    /// </summary>
    property ActiveFormCaption: string read FActiveFormCaption
      write FActiveFormCaption;
    /// <summary>
    /// Class name of the active control when the exception occurred
    /// </summary>
    property ActiveControlClass: string read FActiveControlClass
      write FActiveControlClass;
    /// <summary>
    /// Name of the active control when the exception occurred
    /// </summary>
    property ActiveControlName: string read FActiveControlName
      write FActiveControlName;
    /// <summary>
    /// Message text from the exception
    /// </summary>
    property ExceptionMessage: string read FExceptionMessage
      write FExceptionMessage;
    /// <summary>
    /// Timestamp when the exception occurred
    /// </summary>
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  WinApi.FileVersionInformation.Base,
  Winapi.Windows;
{$ENDIF}

var
  GExceptionHandlers: TInterfaceList = nil;

  { TLZExceptionDetails }

constructor TLZExceptionDetails.Create(const AException: Exception;
  const ASender: TObject; const AExceptAddr: Pointer);
begin
  inherited Create;
  FSourceException := AException;
  FTimestamp := Now;
  FLineNumber := 0;

  ExtractBasicInformation(ASender, AExceptAddr);
  ExtractVersionInformation;
  ExtractActiveControlInformation;
  ApplyRegisteredHandlers;
end;

destructor TLZExceptionDetails.Destroy;
begin
  inherited;
end;

procedure TLZExceptionDetails.ExtractBasicInformation(const ASender: TObject;
  const AExceptAddr: Pointer);
begin
  // Extract exception information
  if Assigned(FSourceException) then
  begin
    FExceptionClass := FSourceException.ClassName;
    FExceptionMessage := FSourceException.Message;
  end
  else
  begin
    FExceptionClass := '';
    FExceptionMessage := '';
  end;

  // Extract executable path
  FExecutable := ParamStr(0);

  // Extract application title
{$IF DEFINED(VCL) OR DEFINED(FMX)}
  FApplicationTitle := Application.Title;
{$ELSE}
  FApplicationTitle := '';
{$ENDIF}
  // Extract sender information
  if Assigned(ASender) then
    FSenderClass := ASender.ClassName
  else
    FSenderClass := '';

  // Extract exception address
  if Assigned(AExceptAddr) then
    FAddress := Format('0x%p', [AExceptAddr])
  else
    FAddress := '';

  // Extract active form information
{$IF DEFINED(VCL) OR DEFINED(FMX)}
  if Assigned(Screen.ActiveForm) then
  begin
    FActiveFormClass := Screen.ActiveForm.ClassName;
    FActiveFormCaption := Screen.ActiveForm.Caption;
  end
  else
  begin
    FActiveFormClass := '';
    FActiveFormCaption := '';
  end;
{$ELSE}
  FActiveFormClass := '';
  FActiveFormCaption := '';
{$ENDIF}
end;

procedure TLZExceptionDetails.ExtractVersionInformation;
{$IFDEF MSWINDOWS}
var
  LFileVersionInformation: TLZFileVersionInformationBase;
begin
  LFileVersionInformation := TLZFileVersionInformationBase.Create;
  try
    try
      LFileVersionInformation.FileName := ParamStr(0);
      FProductName := LFileVersionInformation.ProductName;
      FProductVersion := LFileVersionInformation.ProductVersion;
      FFileVersion := LFileVersionInformation.FileVersion;
    except
      // Silently ignore version extraction errors
    end;
  finally
    FreeAndNil(LFileVersionInformation);
  end;
end;
{$ELSE}
begin
  FProductName := '';
  FProductVersion := '';
  FFileVersion := '';
end;
{$ENDIF}

procedure TLZExceptionDetails.ExtractActiveControlInformation;
begin
{$IF DEFINED(VCL) OR DEFINED(FMX)}
  if Assigned(Screen.ActiveControl) then
  begin
    FActiveControlClass := Screen.ActiveControl.ClassName;
    FActiveControlName := Screen.ActiveControl.Name;
  end
  else
  begin
    FActiveControlClass := '';
    FActiveControlName := '';
  end;
{$ELSE}
  FActiveControlClass := '';
  FActiveControlName := '';
{$ENDIF}
end;

procedure TLZExceptionDetails.ApplyRegisteredHandlers;
var
  LHandler: ILZExceptionHandler;
  i: Integer;
begin
  if not Assigned(GExceptionHandlers) then
    Exit;

  for i := 0 to GExceptionHandlers.Count - 1 do
  begin
    LHandler := GExceptionHandlers[i] as ILZExceptionHandler;
    if Assigned(LHandler) then
    begin
      try
        LHandler.EnhanceExceptionDetails(Self);
      except
        // Silently catch errors in handlers to prevent cascading exceptions
      end;
    end;
  end;
end;

class procedure TLZExceptionDetails.RegisterHandler(const AHandler
  : ILZExceptionHandler);
begin
  if not Assigned(GExceptionHandlers) then
    GExceptionHandlers := TInterfaceList.Create;

  if GExceptionHandlers.IndexOf(AHandler) = -1 then
    GExceptionHandlers.Add(AHandler);
end;

class procedure TLZExceptionDetails.UnregisterHandler(const AHandler
  : ILZExceptionHandler);
begin
  if Assigned(GExceptionHandlers) then
    GExceptionHandlers.Remove(AHandler);
end;

class procedure TLZExceptionDetails.ClearHandlers;
begin
  if Assigned(GExceptionHandlers) then
    GExceptionHandlers.Clear;
end;

function TLZExceptionDetails.AsString: string;
var
  LBuilder: TStringBuilder;
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendLine('Exception Details:');
    LBuilder.AppendLine('==================');
    LBuilder.AppendFormat('Timestamp: %s', [DateTimeToStr(FTimestamp)])
      .AppendLine;
    LBuilder.AppendFormat('Exception Class: %s', [FExceptionClass]).AppendLine;
    LBuilder.AppendFormat('Exception Message: %s', [FExceptionMessage])
      .AppendLine;
    LBuilder.AppendFormat('Address: %s', [FAddress]).AppendLine;
    LBuilder.AppendLine;

    LBuilder.AppendLine('Application Information:');
    LBuilder.AppendFormat('  Executable: %s', [FExecutable]).AppendLine;
    if Trim(FApplicationTitle) <> '' then
        LBuilder.AppendFormat('  Title: %s', [FApplicationTitle]).AppendLine;
    if Trim(FProductName) <> '' then    
      LBuilder.AppendFormat('  Product: %s', [FProductName]).AppendLine;
    if Trim(FFileVersion) <> '' then
      LBuilder.AppendFormat('  File Version: %s', [FFileVersion]).AppendLine;
    if Trim(FProductVersion) <> '' then
    LBuilder.AppendFormat('  Product Version: %s', [FProductVersion])
      .AppendLine;
    LBuilder.AppendLine;

    if (FModuleName <> '') or (FProcedureName <> '') or (FSourceName <> '') or
      (FLineNumber > 0) then
    begin
      LBuilder.AppendLine('Source Location:');
      if FModuleName <> '' then
        LBuilder.AppendFormat('  Unit: %s', [FModuleName]).AppendLine;
      if FProcedureName <> '' then
        LBuilder.AppendFormat('  Procedure: %s', [FProcedureName]).AppendLine;
      if FSourceName <> '' then
        LBuilder.AppendFormat('  Source File: %s', [FSourceName]).AppendLine;
      if FLineNumber > 0 then
        LBuilder.AppendFormat('  Line Number: %d', [FLineNumber]).AppendLine;
      LBuilder.AppendLine;
    end;

    if FSenderClass <> '' then
    begin
      LBuilder.AppendLine('Sender Information:');
      LBuilder.AppendFormat('  Class: %s', [FSenderClass]).AppendLine;
      LBuilder.AppendLine;
    end;

    if (FActiveFormClass <> '') or (FActiveFormCaption <> '') then
    begin
      LBuilder.AppendLine('Active Form:');
      if FActiveFormClass <> '' then
        LBuilder.AppendFormat('  Class: %s', [FActiveFormClass]).AppendLine;
      if FActiveFormCaption <> '' then
        LBuilder.AppendFormat('  Caption: %s', [FActiveFormCaption]).AppendLine;
      LBuilder.AppendLine;
    end;

    if (FActiveControlClass <> '') or (FActiveControlName <> '') then
    begin
      LBuilder.AppendLine('Active Control:');
      if FActiveControlClass <> '' then
        LBuilder.AppendFormat('  Class: %s', [FActiveControlClass]).AppendLine;
      if FActiveControlName <> '' then
        LBuilder.AppendFormat('  Name: %s', [FActiveControlName]).AppendLine;
      LBuilder.AppendLine;
    end;

    if FStackTrace <> '' then
    begin
      LBuilder.AppendLine('Stack Trace:');
      LBuilder.AppendLine(FStackTrace);
    end;

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TLZExceptionDetails.AsJSON: string;
var
  LBuilder: TStringBuilder;

  procedure AddProperty(const AName, AValue: string;
    const AIsLast: Boolean = False);
  var
    LEscapedValue: string;
  begin
    LEscapedValue := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
    LEscapedValue := StringReplace(LEscapedValue, '"', '\"', [rfReplaceAll]);
    LEscapedValue := StringReplace(LEscapedValue, #13, '\r', [rfReplaceAll]);
    LEscapedValue := StringReplace(LEscapedValue, #10, '\n', [rfReplaceAll]);
    LEscapedValue := StringReplace(LEscapedValue, #9, '\t', [rfReplaceAll]);

    if AIsLast then
      LBuilder.AppendFormat('    "%s": "%s"', [AName, LEscapedValue]).AppendLine
    else
      LBuilder.AppendFormat('    "%s": "%s",', [AName, LEscapedValue])
        .AppendLine;
  end;

  procedure AddIntProperty(const AName: string; const AValue: Integer;
    const AIsLast: Boolean = False);
  begin
    if AIsLast then
      LBuilder.AppendFormat('    "%s": %d', [AName, AValue]).AppendLine
    else
      LBuilder.AppendFormat('    "%s": %d,', [AName, AValue]).AppendLine;
  end;

begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendLine('{');
    LBuilder.AppendLine('  "exceptionDetails": {');

    AddProperty('timestamp', DateTimeToStr(FTimestamp));
    AddProperty('exceptionClass', FExceptionClass);
    AddProperty('exceptionMessage', FExceptionMessage);
    AddProperty('address', FAddress);
    AddProperty('executable', FExecutable);
    AddProperty('applicationTitle', FApplicationTitle);
    AddProperty('productName', FProductName);
    AddProperty('fileVersion', FFileVersion);
    AddProperty('productVersion', FProductVersion);
    AddProperty('ModuleName', FModuleName);
    AddProperty('procedureName', FProcedureName);
    AddProperty('sourceName', FSourceName);
    AddIntProperty('lineNumber', FLineNumber);
    AddProperty('senderClass', FSenderClass);
    AddProperty('activeFormClass', FActiveFormClass);
    AddProperty('activeFormCaption', FActiveFormCaption);
    AddProperty('activeControlClass', FActiveControlClass);
    AddProperty('activeControlName', FActiveControlName);
    AddProperty('stackTrace', FStackTrace, True);

    LBuilder.AppendLine('  }');
    LBuilder.AppendLine('}');

    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

initialization

finalization

FreeAndNil(GExceptionHandlers);

end.
