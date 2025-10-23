object frmMSGraphDemo: TfrmMSGraphDemo
  Left = 0
  Top = 0
  Caption = 'MSGraph Management Demo'
  ClientHeight = 595
  ClientWidth = 796
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 443
    Width = 796
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 156
    ExplicitWidth = 200
  end
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 796
    Height = 117
    Align = alTop
    ColumnCollection = <
      item
        Value = 20.000000000000000000
      end
      item
        Value = 80.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Label1
        Row = 0
      end
      item
        Column = 1
        Control = editTennantID
        Row = 0
      end
      item
        Column = 0
        Control = Label2
        Row = 1
      end
      item
        Column = 1
        Control = editClientID
        Row = 1
      end
      item
        Column = 0
        Control = Label3
        Row = 2
      end
      item
        Column = 1
        Control = editClientSecret
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
    ExplicitWidth = 786
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 49
      Height = 15
      Align = alClient
      Caption = 'Tenant ID'
    end
    object editTennantID: TEdit
      AlignWithMargins = True
      Left = 163
      Top = 4
      Width = 629
      Height = 32
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 161
      ExplicitWidth = 621
      ExplicitHeight = 23
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 42
      Width = 45
      Height = 15
      Align = alClient
      Caption = 'Client ID'
    end
    object editClientID: TEdit
      AlignWithMargins = True
      Left = 163
      Top = 42
      Width = 629
      Height = 33
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 161
      ExplicitWidth = 621
      ExplicitHeight = 23
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 81
      Width = 66
      Height = 15
      Align = alClient
      Caption = 'Client Secret'
    end
    object editClientSecret: TEdit
      AlignWithMargins = True
      Left = 163
      Top = 81
      Width = 629
      Height = 32
      Align = alClient
      PasswordChar = '*'
      TabOrder = 2
      ExplicitLeft = 161
      ExplicitWidth = 621
      ExplicitHeight = 23
    end
  end
  object GridPanel2: TGridPanel
    Left = 0
    Top = 117
    Width = 796
    Height = 76
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
      end
      item
        Column = 0
        Control = BitBtn5
        Row = 1
      end
      item
        Column = 1
        Control = BitBtn6
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 1
    ExplicitWidth = 786
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 192
      Height = 31
      Action = ActionMessageRules
      Align = alClient
      Caption = 'Message rules'
      TabOrder = 0
      ExplicitWidth = 190
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 202
      Top = 4
      Width = 193
      Height = 31
      Action = ActionDirectorySubscriptions
      Align = alClient
      Caption = 'Directory subscriptions'
      TabOrder = 1
      ExplicitLeft = 200
      ExplicitWidth = 190
    end
    object BitBtn3: TBitBtn
      AlignWithMargins = True
      Left = 401
      Top = 4
      Width = 192
      Height = 31
      Action = ActionUserLicenses
      Align = alClient
      Caption = 'User licenses'
      TabOrder = 2
      ExplicitLeft = 396
      ExplicitWidth = 190
    end
    object BitBtn4: TBitBtn
      AlignWithMargins = True
      Left = 599
      Top = 4
      Width = 193
      Height = 31
      Action = ActionSignIns
      Align = alClient
      Caption = 'Sign ins'
      TabOrder = 3
      ExplicitLeft = 592
      ExplicitWidth = 190
    end
    object BitBtn5: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 41
      Width = 192
      Height = 31
      Action = ActionApplications
      Align = alClient
      Caption = 'Applications'
      TabOrder = 4
      ExplicitWidth = 190
    end
    object BitBtn6: TBitBtn
      AlignWithMargins = True
      Left = 202
      Top = 41
      Width = 193
      Height = 31
      Action = ActionAuthenticationMethods
      Align = alClient
      Caption = 'Authentication methods'
      TabOrder = 5
      ExplicitLeft = 200
      ExplicitWidth = 190
    end
  end
  object pnlLog: TPanel
    Left = 0
    Top = 446
    Width = 796
    Height = 149
    Align = alBottom
    TabOrder = 4
    ExplicitTop = 428
    ExplicitWidth = 786
    object Label4: TLabel
      Left = 1
      Top = 1
      Width = 20
      Height = 15
      Align = alTop
      Caption = 'Log'
    end
    object memoLog: TMemo
      Left = 1
      Top = 16
      Width = 794
      Height = 132
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 394
    Width = 796
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 376
    ExplicitWidth = 786
    object lblProgress: TLabel
      Left = 0
      Top = 24
      Width = 3
      Height = 15
      Align = alClient
      Alignment = taCenter
      Layout = tlCenter
    end
    object ProgressBar: TProgressBar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 790
      Height = 18
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 780
    end
  end
  object PageControlOutput: TPageControl
    Left = 0
    Top = 193
    Width = 796
    Height = 201
    ActivePage = tabAuthenticationMethods
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 786
    ExplicitHeight = 183
    object tabMessageRules: TTabSheet
      Caption = 'Message rules'
      object ListViewMessageRules: TListView
        Left = 0
        Top = 0
        Width = 788
        Height = 171
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'userPrincipalName'
          end
          item
            AutoSize = True
            Caption = 'Rule Id'
          end
          item
            AutoSize = True
            Caption = 'Rule name'
          end
          item
            AutoSize = True
            Caption = 'Forward address'
          end
          item
            AutoSize = True
            Caption = 'Move folder'
          end>
        GridLines = True
        RowSelect = True
        PopupMenu = PopupMenuListView
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tabDirectorySubscriptions: TTabSheet
      Caption = 'Directory subscriptions'
      ImageIndex = 1
      object ListViewDirectorySubscriptions: TListView
        Left = 0
        Top = 0
        Width = 788
        Height = 171
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Id'
          end
          item
            AutoSize = True
            Caption = 'Status'
          end
          item
            AutoSize = True
            Caption = 'Total licenses'
          end
          item
            AutoSize = True
            Caption = 'SKU Id'
          end
          item
            AutoSize = True
            Caption = 'SKU part number'
          end
          item
            AutoSize = True
            Caption = 'Trial'
          end
          item
            AutoSize = True
            Caption = 'Create date/time'
          end
          item
            AutoSize = True
            Caption = 'next lifecycle date/time'
          end>
        GridLines = True
        RowSelect = True
        PopupMenu = PopupMenuListView
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tabUserLicenses: TTabSheet
      Caption = 'User licenses'
      ImageIndex = 2
      object ListViewUserLicenses: TListView
        Left = 0
        Top = 0
        Width = 788
        Height = 171
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Id'
          end
          item
            AutoSize = True
            Caption = 'User Id'
          end
          item
            AutoSize = True
            Caption = 'Display name'
          end
          item
            AutoSize = True
            Caption = 'User principal name'
          end
          item
            AutoSize = True
            Caption = 'SKU part number'
          end
          item
            AutoSize = True
            Caption = 'Created'
          end
          item
            AutoSize = True
            Caption = 'Mail'
          end>
        GridLines = True
        RowSelect = True
        PopupMenu = PopupMenuListView
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tabSignIns: TTabSheet
      Caption = 'Sign Ins'
      ImageIndex = 3
      object ListViewSignIns: TListView
        Left = 0
        Top = 0
        Width = 788
        Height = 171
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Id'
          end
          item
            AutoSize = True
            Caption = 'userPrincipalName'
          end
          item
            AutoSize = True
            Caption = 'clientAppUsed'
          end
          item
            AutoSize = True
            Caption = 'location.countryOrRegion'
          end
          item
            AutoSize = True
            Caption = 'location.state'
          end
          item
            AutoSize = True
            Caption = 'location.city'
          end
          item
            AutoSize = True
            Caption = 'status.errorCode'
          end
          item
            AutoSize = True
            Caption = 'status.failureReason'
          end
          item
            AutoSize = True
            Caption = 'status.additionalDetails'
          end
          item
            AutoSize = True
            Caption = 'resourceDisplayName'
          end
          item
            AutoSize = True
            Caption = 'ipAddress'
          end
          item
            AutoSize = True
            Caption = 'isInteractive'
          end
          item
            AutoSize = True
            Caption = 'riskDetail'
          end
          item
            AutoSize = True
            Caption = 'riskState'
          end
          item
            AutoSize = True
            Caption = 'userId'
          end
          item
            AutoSize = True
            Caption = 'deviceDetail.deviceId'
          end
          item
            AutoSize = True
            Caption = 'deviceDetail.displayName'
          end
          item
            AutoSize = True
            Caption = 'deviceDetail.operatingSystem'
          end
          item
            AutoSize = True
            Caption = 'deviceDetail.browser'
          end
          item
            AutoSize = True
            Caption = 'deviceDetail.trustType'
          end
          item
            AutoSize = True
            Caption = 'createdDateTime'
          end>
        GridLines = True
        RowSelect = True
        PopupMenu = PopupMenuListView
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tabApplications: TTabSheet
      Caption = 'Applications'
      ImageIndex = 4
      object ListViewApplications: TListView
        Left = 0
        Top = 0
        Width = 788
        Height = 171
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Id'
          end
          item
            AutoSize = True
            Caption = 'Display name'
          end
          item
            AutoSize = True
            Caption = 'Description'
          end
          item
            AutoSize = True
            Caption = 'Passwords'
          end
          item
            AutoSize = True
            Caption = 'Expiring passwords'
          end>
        GridLines = True
        RowSelect = True
        PopupMenu = PopupMenuListView
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tabAuthenticationMethods: TTabSheet
      Caption = 'Authentication methods'
      ImageIndex = 5
      object ListViewAuthenticationMethods: TListView
        Left = 0
        Top = 0
        Width = 788
        Height = 171
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Id'
          end
          item
            AutoSize = True
            Caption = 'Resource type'
          end
          item
            AutoSize = True
            Caption = 'Details'
          end>
        GridLines = True
        RowSelect = True
        PopupMenu = PopupMenuListView
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitWidth = 778
        ExplicitHeight = 153
      end
    end
  end
  object ActionList: TActionList
    Left = 88
    Top = 228
    object ActionMessageRules: TAction
      Caption = 'Message rules'
      OnExecute = ActionMessageRulesExecute
    end
    object ActionDirectorySubscriptions: TAction
      Caption = 'Directory subscriptions'
      OnExecute = ActionDirectorySubscriptionsExecute
    end
    object ActionUserLicenses: TAction
      Caption = 'User licenses'
      OnExecute = ActionUserLicensesExecute
    end
    object ActionSignIns: TAction
      Caption = 'Sign ins'
      OnExecute = ActionSignInsExecute
    end
    object ActionExportToCSV: TAction
      Caption = 'Export to CSV'
      OnExecute = ActionExportToCSVExecute
    end
    object ActionApplications: TAction
      Caption = 'Applications'
      OnExecute = ActionApplicationsExecute
    end
    object ActionAuthenticationMethods: TAction
      Caption = 'Authentication methods'
      OnExecute = ActionAuthenticationMethodsExecute
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnIdle = ApplicationEventsIdle
    Left = 228
    Top = 300
  end
  object PopupMenuListView: TPopupMenu
    Left = 284
    Top = 252
    object ActionExportToCSV1: TMenuItem
      Action = ActionExportToCSV
    end
  end
  object ListViewSaveDialog: TSaveDialog
    Filter = 'CSV Files|*.csv'
    Left = 404
    Top = 272
  end
end
