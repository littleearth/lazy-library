object frmPWAExample: TfrmPWAExample
  Left = 0
  Top = 0
  Caption = 'PWA Example'
  ClientHeight = 406
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 608
    Height = 105
    Align = alTop
    ColumnCollection = <
      item
        Value = 23.076923076923080000
      end
      item
        Value = 76.923076923076920000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Label1
        Row = 0
      end
      item
        Column = 0
        Control = Label3
        Row = 1
      end
      item
        Column = 0
        Control = Label5
        Row = 2
      end
      item
        Column = 1
        Control = editEndPoint
        Row = 0
      end
      item
        Column = 1
        Control = editUsername
        Row = 1
      end
      item
        Column = 1
        Control = editPassword
        Row = 2
      end>
    RowCollection = <
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333310000
      end>
    TabOrder = 0
    ExplicitWidth = 618
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 48
      Height = 15
      Align = alClient
      Caption = 'Endpoint'
    end
    object Label3: TLabel
      Left = 1
      Top = 35
      Width = 53
      Height = 15
      Align = alClient
      Caption = 'Username'
    end
    object Label5: TLabel
      Left = 1
      Top = 70
      Width = 50
      Height = 15
      Align = alClient
      Caption = 'Password'
    end
    object editEndPoint: TEdit
      Left = 143
      Top = 1
      Width = 474
      Height = 34
      Align = alClient
      TabOrder = 0
      Text = 'https://api.pulseway.com/v2/'
      ExplicitHeight = 23
    end
    object editUsername: TEdit
      Left = 143
      Top = 35
      Width = 474
      Height = 35
      Align = alClient
      TabOrder = 1
      ExplicitHeight = 23
    end
    object editPassword: TEdit
      Left = 143
      Top = 70
      Width = 474
      Height = 34
      Align = alClient
      PasswordChar = '*'
      TabOrder = 2
      ExplicitHeight = 23
    end
  end
  object memoLog: TMemo
    Left = 0
    Top = 146
    Width = 608
    Height = 260
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
    ExplicitWidth = 618
    ExplicitHeight = 278
  end
  object GridPanel2: TGridPanel
    Left = 0
    Top = 105
    Width = 608
    Height = 41
    Align = alTop
    ColumnCollection = <
      item
        Value = 25.000000000000000000
      end
      item
        Value = 25.000000000000000000
      end
      item
        Value = 25.000000000000000000
      end
      item
        Value = 25.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = BitBtn1
        Row = 0
      end
      item
        Column = 1
        Control = BitBtn2
        Row = 0
      end
      item
        Column = 2
        Control = BitBtn3
        Row = 0
      end
      item
        Column = 3
        Control = BitBtn4
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 1
    ExplicitWidth = 618
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 148
      Height = 33
      Action = ActionSystems
      Align = alClient
      Caption = 'Systems'
      TabOrder = 0
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 158
      Top = 4
      Width = 148
      Height = 33
      Action = ActionAssets
      Align = alClient
      Caption = 'Assets'
      TabOrder = 1
    end
    object BitBtn3: TBitBtn
      AlignWithMargins = True
      Left = 312
      Top = 4
      Width = 148
      Height = 33
      Action = ActionPublishSystems
      Align = alClient
      Caption = 'Publish system'
      TabOrder = 2
    end
    object BitBtn4: TBitBtn
      AlignWithMargins = True
      Left = 466
      Top = 4
      Width = 148
      Height = 33
      Action = ActionSendNotification
      Align = alClient
      Caption = 'Send notification'
      TabOrder = 3
    end
  end
  object ActionList: TActionList
    Left = 120
    Top = 224
    object ActionSystems: TAction
      Caption = 'Systems'
      OnExecute = ActionSystemsExecute
    end
    object ActionAssets: TAction
      Caption = 'Assets'
      OnExecute = ActionAssetsExecute
    end
    object ActionPublishSystems: TAction
      Caption = 'Publish system'
      OnExecute = ActionPublishSystemsExecute
    end
    object ActionSendNotification: TAction
      Caption = 'Send notification'
      OnExecute = ActionSendNotificationExecute
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnIdle = ApplicationEventsIdle
    Left = 164
    Top = 188
  end
end
