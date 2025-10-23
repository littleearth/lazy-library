{ -----------------------------------------------------------------------------
  Unit Name: Lazy.Log.Messaging
  Author: Tristan Marlow
  Purpose: Log handler that broadcasts messages via System.Messaging,
           allowing applications to subscribe to log messages

  Overview:
  This unit provides a messaging-based log handler that broadcasts log messages
  through Delphi's System.Messaging framework. This allows applications to
  subscribe to log messages and react to them in real-time, enabling features
  like custom UI updates, notifications, or external integrations.

  Features:
  - Thread-safe broadcasting via TMessageManager
  - Filterable subscriptions by log level
  - Filterable subscriptions by log class
  - Support for all log types (Error, Warning, Info, Debug, Progress)
  - Subscribers can use anonymous methods or class methods

  Usage Example:
  --------------
  // Add the messaging handler
  LazyLog.AddHandler('Messaging', TLZLogMessaging);

  // Subscribe to all log messages
  LSubscription := TLZLogSubscriber.SubscribeToAll(
    procedure(const Sender: TObject; const M: TMessage)
    var
      LLogMsg: TLazyLogMessage;
    begin
      LLogMsg := (M as TLZLogMessageWrapper).LogMessage;
      ShowMessage(LLogMsg.LogMessage);
    end);

  // Subscribe to errors only
  LSubscription := TLZLogSubscriber.SubscribeToErrors(
    procedure(const Sender: TObject; const M: TMessage)
    var
      LLogMsg: TLazyLogMessage;
    begin
      LLogMsg := (M as TLZLogMessageError).LogMessage;
      ShowMessage('Error: ' + LLogMsg.LogMessage);
    end);

  // Subscribe to errors from a specific class only
  LSubscription := TLZLogSubscriber.SubscribeToErrors(
    procedure(const Sender: TObject; const M: TMessage)
    var
      LLogMsg: TLazyLogMessage;
    begin
      LLogMsg := (M as TLZLogMessageError).LogMessage;
      ShowMessage('Service Error: ' + LLogMsg.LogMessage);
    end, 'TMyService');

  ----------------------------------------------------------------------------
  Copyright (c) Tristan David Marlow
  Copyright (c) Little Earth Solutions
  ---------------------------------------------------------------------------- }
unit Lazy.Log.Messaging;

interface

uses
  SysUtils, Classes, Lazy.Log, Lazy.Types, System.Messaging;

type
  /// <summary>
  /// Base message wrapper that contains the log message for broadcasting
  /// </summary>
  TLZLogMessageWrapper = class(TMessage)
  private
    FLogMessage: TLazyLogMessage;
    FOwnsMessage: Boolean;
  public
    /// <summary>
    /// Creates a new log message wrapper
    /// </summary>
    constructor Create(ALogMessage: TLazyLogMessage; AOwnsMessage: Boolean = False);
    destructor Destroy; override;
    /// <summary>
    /// The wrapped log message
    /// </summary>
    property LogMessage: TLazyLogMessage read FLogMessage;
  end;

  TLZLogMessageWrapperClass = class of TLZLogMessageWrapper;

  // Specific message types for each log level - allows filtered subscriptions
  /// <summary>
  /// Message wrapper for error log messages
  /// </summary>
  TLZLogMessageError = class(TLZLogMessageWrapper);
  /// <summary>
  /// Message wrapper for warning log messages
  /// </summary>
  TLZLogMessageWarning = class(TLZLogMessageWrapper);
  /// <summary>
  /// Message wrapper for information log messages
  /// </summary>
  TLZLogMessageInformation = class(TLZLogMessageWrapper);
  /// <summary>
  /// Message wrapper for debug log messages
  /// </summary>
  TLZLogMessageDebug = class(TLZLogMessageWrapper);
  /// <summary>
  /// Message wrapper for progress log messages
  /// </summary>
  TLZLogMessageProgress = class(TLZLogMessageWrapper);

  /// <summary>
  /// Log handler that broadcasts messages via System.Messaging
  /// </summary>
  TLZLogMessaging = class(TLZLogHandler)
  private
    FMessageManager: TMessageManager;
    FBroadcastAll: Boolean;
    FBroadcastByLevel: Boolean;
  protected
    /// <summary>
    /// Gets the appropriate message wrapper class for the log message
    /// </summary>
    function GetMessageWrapperClass(ALogMessage: TLazyLogMessage): 
      TLZLogMessageWrapperClass;
    /// <summary>
    /// Broadcasts a log message to all subscribers
    /// </summary>
    procedure BroadcastMessage(ALogMessage: TLazyLogMessage);
  public
    /// <summary>
    /// Default setting for broadcasting all messages
    /// </summary>
    class var DefaultBroadcastAll: Boolean;
    /// <summary>
    /// Default setting for broadcasting messages by level
    /// </summary>
    class var DefaultBroadcastByLevel: Boolean;
    procedure AfterConstruction; override;
    /// <summary>
    /// Processes a log message and broadcasts it to subscribers
    /// </summary>
    procedure ProcessLogMessage(ALogMessage: TLazyLogMessage); override;
    
    /// <summary>
    /// When true, broadcasts all messages as TLZLogMessageWrapper.
    /// Default: True
    /// </summary>
    property BroadcastAll: Boolean read FBroadcastAll write FBroadcastAll;
    
    /// <summary>
    /// When true, broadcasts messages by log level (Error, Warning, etc).
    /// Default: True
    /// </summary>
    property BroadcastByLevel: Boolean read FBroadcastByLevel write FBroadcastByLevel;
  end;



  /// <summary>
  /// Helper class for managing message subscriptions
  /// </summary>
  TLZLogMessageSubscription = class
  private
    FSubscriptionId: Integer;
    FMessageManager: TMessageManager;
    FMessageClass: TClass;
  public
    /// <summary>
    /// Creates a new subscription manager
    /// </summary>
    constructor Create(ASubscriptionId: Integer; 
      AMessageClass: TClass = nil;
      AMessageManager: TMessageManager = nil);
    destructor Destroy; override;
    /// <summary>
    /// Unsubscribes from the message
    /// </summary>
    procedure Unsubscribe;
    /// <summary>
    /// The subscription ID for this subscription
    /// </summary>
    property SubscriptionId: Integer read FSubscriptionId;
  end;

  /// <summary>
  /// Helper class with static subscription methods for easy message subscription
  /// </summary>
  TLZLogSubscriber = class
  public
    /// <summary>
    /// Subscribe to all log messages
    /// </summary>
    class function SubscribeToAll(
      AHandler: TMessageListenerMethod; ALogClass: string = ''): TLZLogMessageSubscription;
      
    /// <summary>
    /// Subscribe to error messages only
    /// </summary>
    class function SubscribeToErrors(
      AHandler: TMessageListenerMethod; ALogClass: string = ''): TLZLogMessageSubscription;
      
    /// <summary>
    /// Subscribe to warning messages only
    /// </summary>
    class function SubscribeToWarnings(
      AHandler: TMessageListenerMethod; ALogClass: string = ''): TLZLogMessageSubscription;
      
    /// <summary>
    /// Subscribe to information messages only
    /// </summary>
    class function SubscribeToInformation(
      AHandler: TMessageListenerMethod; ALogClass: string = ''): TLZLogMessageSubscription;
      
    /// <summary>
    /// Subscribe to debug messages only
    /// </summary>
    class function SubscribeToDebug(
      AHandler: TMessageListenerMethod; ALogClass: string = ''): TLZLogMessageSubscription;
      
    /// <summary>
    /// Subscribe to progress messages only
    /// </summary>
    class function SubscribeToProgress(
      AHandler: TMessageListenerMethod; ALogClass: string = ''): TLZLogMessageSubscription;
  end;

implementation

{ TLZLogMessageWrapper }

constructor TLZLogMessageWrapper.Create(ALogMessage: TLazyLogMessage; 
  AOwnsMessage: Boolean);
begin
  inherited Create;
  FLogMessage := ALogMessage;
  FOwnsMessage := AOwnsMessage;
end;

destructor TLZLogMessageWrapper.Destroy;
begin
  if FOwnsMessage and Assigned(FLogMessage) then
    FLogMessage.Free;
  inherited;
end;

{ TLZLogMessaging }

procedure TLZLogMessaging.AfterConstruction;
begin
  inherited;
  FMessageManager := TMessageManager.DefaultManager;
  FBroadcastAll := DefaultBroadcastAll;
  FBroadcastByLevel := DefaultBroadcastByLevel;
end;

function TLZLogMessaging.GetMessageWrapperClass(
  ALogMessage: TLazyLogMessage): TLZLogMessageWrapperClass;
begin
  case ALogMessage.LogLevel of
    logError:
      Result := TLZLogMessageError;
    logWarning:
      Result := TLZLogMessageWarning;
    logInformation:
      Result := TLZLogMessageInformation;
    logDebug:
      Result := TLZLogMessageDebug;
  else
    // Progress and other types
    if ALogMessage.LogType = ltProgress then
      Result := TLZLogMessageProgress
    else
      Result := TLZLogMessageWrapper;
  end;
end;

procedure TLZLogMessaging.BroadcastMessage(ALogMessage: TLazyLogMessage);
var
  LMessage: TLZLogMessageWrapper;
  LWrapperClass: TLZLogMessageWrapperClass;
begin
  // Broadcast to all subscribers (TLZLogMessageWrapper)
  if FBroadcastAll then
  begin
    LMessage := TLZLogMessageWrapper.Create(ALogMessage, False);
    // Pass True for ADispose so message manager handles cleanup
    FMessageManager.SendMessage(Self, LMessage, True);
  end;

  // Broadcast by log level (specific message types)
  if FBroadcastByLevel then
  begin
    LWrapperClass := GetMessageWrapperClass(ALogMessage);
    LMessage := LWrapperClass.Create(ALogMessage, False);
    // Pass True for ADispose so message manager handles cleanup
    FMessageManager.SendMessage(Self, LMessage, True);
  end;
end;

procedure TLZLogMessaging.ProcessLogMessage(ALogMessage: TLazyLogMessage);
begin
  if not Assigned(ALogMessage) then
    Exit;
    
  // Check if we should process this log level
  if not IsLogLevel(ALogMessage.LogLevel) then
    Exit;


  // Broadcast the message via System.Messaging
  BroadcastMessage(ALogMessage);
end;

{ TLZLogMessageSubscription }

constructor TLZLogMessageSubscription.Create(ASubscriptionId: Integer;
  AMessageClass: TClass;
  AMessageManager: TMessageManager);
begin
  inherited Create;
  FSubscriptionId := ASubscriptionId;
  FMessageClass := AMessageClass;
  if Assigned(AMessageManager) then
    FMessageManager := AMessageManager
  else
    FMessageManager := TMessageManager.DefaultManager;
end;

destructor TLZLogMessageSubscription.Destroy;
begin
  Unsubscribe;
  inherited;
end;

procedure TLZLogMessageSubscription.Unsubscribe;
begin
  if FSubscriptionId <> 0 then
  begin
    // Use the message class and subscription ID to properly unsubscribe
    if Assigned(FMessageClass) then
      FMessageManager.Unsubscribe(FMessageClass, FSubscriptionId, True)
    else
      FMessageManager.Unsubscribe(nil, FSubscriptionId, True);
    FSubscriptionId := 0;
  end;
end;

{ TLZLogSubscriber }

class function TLZLogSubscriber.SubscribeToAll(
  AHandler: TMessageListenerMethod; ALogClass: string): TLZLogMessageSubscription;
var
  LSubscriptionId: Integer;
  LLogClass: string;
begin
  LLogClass := Trim(ALogClass);
  
  // If no class filter, subscribe directly
  if LLogClass = '' then
  begin
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageWrapper, AHandler);
  end
  else
  begin
    // Wrap handler to filter by log class
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageWrapper,
      procedure(const Sender: TObject; const M: TMessage)
      var
        LLogMsg: TLazyLogMessage;
      begin
        if M is TLZLogMessageWrapper then
        begin
          LLogMsg := (M as TLZLogMessageWrapper).LogMessage;
          if SameText(LLogMsg.LogClass, LLogClass) then
            AHandler(Sender, M);
        end;
      end);
  end;
  
  Result := TLZLogMessageSubscription.Create(LSubscriptionId, TLZLogMessageWrapper);
end;

class function TLZLogSubscriber.SubscribeToErrors(
  AHandler: TMessageListenerMethod; ALogClass: string): TLZLogMessageSubscription;
var
  LSubscriptionId: Integer;
  LLogClass: string;
begin
  LLogClass := Trim(ALogClass);
  
  // If no class filter, subscribe directly
  if LLogClass = '' then
  begin
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageError, AHandler);
  end
  else
  begin
    // Wrap handler to filter by log class
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageError,
      procedure(const Sender: TObject; const M: TMessage)
      var
        LLogMsg: TLazyLogMessage;
      begin
        if M is TLZLogMessageError then
        begin
          LLogMsg := (M as TLZLogMessageError).LogMessage;
          if SameText(LLogMsg.LogClass, LLogClass) then
            AHandler(Sender, M);
        end;
      end);
  end;
  
  Result := TLZLogMessageSubscription.Create(LSubscriptionId, TLZLogMessageError);
end;

class function TLZLogSubscriber.SubscribeToWarnings(
  AHandler: TMessageListenerMethod; ALogClass: string): TLZLogMessageSubscription;
var
  LSubscriptionId: Integer;
  LLogClass: string;
begin
  LLogClass := Trim(ALogClass);
  
  // If no class filter, subscribe directly
  if LLogClass = '' then
  begin
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageWarning, AHandler);
  end
  else
  begin
    // Wrap handler to filter by log class
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageWarning,
      procedure(const Sender: TObject; const M: TMessage)
      var
        LLogMsg: TLazyLogMessage;
      begin
        if M is TLZLogMessageWarning then
        begin
          LLogMsg := (M as TLZLogMessageWarning).LogMessage;
          if SameText(LLogMsg.LogClass, LLogClass) then
            AHandler(Sender, M);
        end;
      end);
  end;
  
  Result := TLZLogMessageSubscription.Create(LSubscriptionId, TLZLogMessageWarning);
end;

class function TLZLogSubscriber.SubscribeToInformation(
  AHandler: TMessageListenerMethod; ALogClass: string): TLZLogMessageSubscription;
var
  LSubscriptionId: Integer;
  LLogClass: string;
begin
  LLogClass := Trim(ALogClass);
  
  // If no class filter, subscribe directly
  if LLogClass = '' then
  begin
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageInformation, AHandler);
  end
  else
  begin
    // Wrap handler to filter by log class
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageInformation,
      procedure(const Sender: TObject; const M: TMessage)
      var
        LLogMsg: TLazyLogMessage;
      begin
        if M is TLZLogMessageInformation then
        begin
          LLogMsg := (M as TLZLogMessageInformation).LogMessage;
          if SameText(LLogMsg.LogClass, LLogClass) then
            AHandler(Sender, M);
        end;
      end);
  end;
  
  Result := TLZLogMessageSubscription.Create(LSubscriptionId, TLZLogMessageInformation);
end;

class function TLZLogSubscriber.SubscribeToDebug(
  AHandler: TMessageListenerMethod; ALogClass: string): TLZLogMessageSubscription;
var
  LSubscriptionId: Integer;
  LLogClass: string;
begin
  LLogClass := Trim(ALogClass);
  
  // If no class filter, subscribe directly
  if LLogClass = '' then
  begin
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageDebug, AHandler);
  end
  else
  begin
    // Wrap handler to filter by log class
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageDebug,
      procedure(const Sender: TObject; const M: TMessage)
      var
        LLogMsg: TLazyLogMessage;
      begin
        if M is TLZLogMessageDebug then
        begin
          LLogMsg := (M as TLZLogMessageDebug).LogMessage;
          if SameText(LLogMsg.LogClass, LLogClass) then
            AHandler(Sender, M);
        end;
      end);
  end;
  
  Result := TLZLogMessageSubscription.Create(LSubscriptionId, TLZLogMessageDebug);
end;

class function TLZLogSubscriber.SubscribeToProgress(
  AHandler: TMessageListenerMethod; ALogClass: string): TLZLogMessageSubscription;
var
  LSubscriptionId: Integer;
  LLogClass: string;
begin
  LLogClass := Trim(ALogClass);
  
  // If no class filter, subscribe directly
  if LLogClass = '' then
  begin
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageProgress, AHandler);
  end
  else
  begin
    // Wrap handler to filter by log class
    LSubscriptionId := TMessageManager.DefaultManager.SubscribeToMessage(
      TLZLogMessageProgress,
      procedure(const Sender: TObject; const M: TMessage)
      var
        LLogMsg: TLazyLogMessage;
      begin
        if M is TLZLogMessageProgress then
        begin
          LLogMsg := (M as TLZLogMessageProgress).LogMessage;
          if SameText(LLogMsg.LogClass, LLogClass) then
            AHandler(Sender, M);
        end;
      end);
  end;
  
  Result := TLZLogMessageSubscription.Create(LSubscriptionId, TLZLogMessageProgress);
end;

initialization

TLZLogMessaging.DefaultBroadcastAll := True;
TLZLogMessaging.DefaultBroadcastByLevel := True;

end.

