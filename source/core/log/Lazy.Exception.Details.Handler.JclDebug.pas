{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Exception.Details.Handler.JclDebug
  Author: Tristan Marlow
  Purpose: JclDebug integration for enhanced exception details

  Overview:
  This unit provides JclDebug integration for the Lazy Exception Details system.
  It automatically captures detailed stack traces with source file names and
  line numbers using the JCL Debug library.

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ---------------------------------------------------------------------------- }

unit Lazy.Exception.Details.Handler.JclDebug;

interface

uses
  System.SysUtils,
  System.Classes,
  Lazy.Exception.Details;

type
  /// <summary>
  /// JclDebug handler that integrates with JCL Debug library
  /// Provides detailed stack traces with source file names and line numbers
  /// Automatically registered globally at unit initialization
  /// </summary>
  TLZJclExceptionHandler = class(TInterfacedObject, ILZExceptionHandler)
  public
    /// <summary>
    /// Enhances exception details with JclDebug stack trace information
    /// </summary>
    procedure EnhanceExceptionDetails(const ADetails: TLZExceptionDetails);
    /// <summary>
    /// Returns the handler name for identification
    /// </summary>
    function GetHandlerName: string;
  end;

implementation

uses
  Winapi.Windows,
  JclDebug,
  Lazy.Utils.Windows;

{ TLZJclExceptionHandler }


procedure TLZJclExceptionHandler.EnhanceExceptionDetails(const ADetails: TLZExceptionDetails);
var
  LList: TJclStackInfoList;
  LStackTrace: TStringBuilder;
  i: Integer;
  LInfo: TJclLocationInfo;
  LPointer: Pointer;
begin
  // Get the stack trace list from JCL
  LList := JclLastExceptStackList;
  if Assigned(LList) and (LList.Count > 0) then
  begin
    LStackTrace := TStringBuilder.Create;
    try
      // Build the complete stack trace
      for i := 0 to LList.Count - 1 do
      begin
        LPointer := LList.Items[i].CallerAddr;
        if GetLocationInfo(LPointer, LInfo) then
        begin
          LStackTrace.AppendFormat('[%p] %s.%s (Line %d, "%s")', [
            LPointer,
            LInfo.UnitName,
            LInfo.ProcedureName,
            LInfo.LineNumber,
            LInfo.SourceName
          ]).AppendLine;
          
          // Populate individual fields from the top frame
          if i = 0 then
          begin
            if not TLZString.IsEmptyString(LInfo.UnitName) then
              ADetails.ModuleName := LInfo.UnitName;
            if not TLZString.IsEmptyString(LInfo.ProcedureName) then
              ADetails.ProcedureName := LInfo.ProcedureName;
            if not TLZString.IsEmptyString(LInfo.SourceName) then
              ADetails.SourceName := LInfo.SourceName;
            if LInfo.LineNumber > 0 then
              ADetails.LineNumber := LInfo.LineNumber;
          end;
        end
        else
        begin
          // If we can't get location info, at least show the address
          LStackTrace.AppendFormat('[%p]', [LPointer]).AppendLine;
        end;
      end;
      
      ADetails.StackTrace := LStackTrace.ToString;
    finally
      LStackTrace.Free;
    end;
  end
  else
  begin
    // Fallback: try to get location info directly from exception address
    try
      LPointer := Pointer(NativeUInt(ExceptAddr) - SizeOf(Pointer));
      if GetLocationInfo(LPointer, LInfo) then
      begin
        if not TLZString.IsEmptyString(LInfo.UnitName) then
        begin
          ADetails.ModuleName := LInfo.UnitName;
          ADetails.ProcedureName := LInfo.ProcedureName;
          ADetails.SourceName := LInfo.SourceName;
          ADetails.LineNumber := LInfo.LineNumber;
          
          ADetails.StackTrace := Format('[%p] %s.%s (Line %d, "%s")', [
            LPointer,
            LInfo.UnitName,
            LInfo.ProcedureName,
            LInfo.LineNumber,
            LInfo.SourceName
          ]);
        end;
      end;
    except
      // Fail silently
    end;
  end;
end;

function TLZJclExceptionHandler.GetHandlerName: string;
begin
  Result := 'JclDebug Stack Trace Handler';
end;


end.

