unit frmPWAExampleU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Lazy.Types, PWA.Core, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.Actions, Vcl.ActnList, Vcl.Buttons, Vcl.AppEvnts;

type
  TfrmPWAExample = class(TForm)
    GridPanel1: TGridPanel;
    memoLog: TMemo;
    GridPanel2: TGridPanel;
    BitBtn1: TBitBtn;
    ActionList: TActionList;
    ActionSystems: TAction;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    editEndPoint: TEdit;
    editUsername: TEdit;
    editPassword: TEdit;
    BitBtn2: TBitBtn;
    ActionAssets: TAction;
    ActionSendNotification: TAction;
    BitBtn3: TBitBtn;
    ApplicationEvents: TApplicationEvents;
    BitBtn4: TBitBtn;
    ActionPublishSystems: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionSystemsExecute(Sender: TObject);
    procedure ActionAssetsExecute(Sender: TObject);
    procedure ActionSendNotificationExecute(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ActionPublishSystemsExecute(Sender: TObject);
  private
  public
    FPWACore: TLZPWACore;
  end;

var
  frmPWAExample: TfrmPWAExample;

implementation

uses
  Lazy.Log, Lazy.Log.Basic;

{$R *.dfm}

procedure TfrmPWAExample.ActionAssetsExecute(Sender: TObject);
begin
  FPWACore.Username := editUsername.Text;
  FPWACore.Password := editPassword.Text;
  FPWACore.EndPoint := editEndPoint.Text;
  FPWACore.Get('assets',
    procedure(Asender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        LazyLog.Log(Self, ARESTResponse.JSONValue.Format);
      end
      else
      begin
        LazyLog.Error(Self, AMessage);
      end;
    end)
end;

procedure TfrmPWAExample.ActionPublishSystemsExecute(Sender: TObject);
begin
  FPWACore.Username := editUsername.Text;
  FPWACore.Password := editPassword.Text;
  FPWACore.EndPoint := editEndPoint.Text;
  FPWACore.Post('systems',
    '{"instance_id":"dwayne-central-probe","name":"Dwayne Central Probe","description":"Dwaynes Central Probe","next_refresh_interval_minutes":5,"notify_when_offline":"true"}',
    procedure(Asender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        LazyLog.Log(Self, ARESTResponse.JSONValue.Format);
      end
      else
      begin
        LazyLog.Error(Self, AMessage);
      end;
    end)
end;

procedure TfrmPWAExample.ActionSendNotificationExecute(Sender: TObject);
begin
  FPWACore.Username := editUsername.Text;
  FPWACore.Password := editPassword.Text;
  FPWACore.EndPoint := editEndPoint.Text;
  FPWACore.Post('notifications',
    '{"instance_id":"dwayne-central-probe","title":"Dwayne Central Probe","message":"Matthew broke it! Matthew broke it!.","priority":"critical"}',
    procedure(Asender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        LazyLog.Log(Self, ARESTResponse.JSONValue.Format);
      end
      else
      begin
        LazyLog.Error(Self, AMessage);
      end;
    end)
end;

procedure TfrmPWAExample.ActionSystemsExecute(Sender: TObject);
begin
  FPWACore.Username := editUsername.Text;
  FPWACore.Password := editPassword.Text;
  FPWACore.EndPoint := editEndPoint.Text;
  FPWACore.Get('systems',
    procedure(Asender: TObject; ASuccess: Boolean; AMessage: string;
      ARESTResponse: TRESTResponse; ACustomData: string)
    begin
      if ASuccess then
      begin
        LazyLog.Log(Self, ARESTResponse.JSONValue.Format);
      end
      else
      begin
        LazyLog.Error(Self, AMessage);
      end;
    end)
end;

procedure TfrmPWAExample.ApplicationEventsIdle(Sender: TObject;
var Done: Boolean);
begin
  memoLog.Lines.Text := (LazyLog as TLZLogBasic).LogText;
end;

procedure TfrmPWAExample.FormCreate(Sender: TObject);
begin
  FPWACore := TLZPWACore.Create;
end;

procedure TfrmPWAExample.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPWACore);
end;

end.
