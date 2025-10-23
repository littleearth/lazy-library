object frmUniFiClient: TfrmUniFiClient
  Left = 0
  Top = 0
  Caption = 'UniFi Client VCL'
  ClientHeight = 730
  ClientWidth = 1012
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
  object pnlControls: TGridPanel
    Left = 0
    Top = 125
    Width = 1012
    Height = 80
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnlControls'
    ColumnCollection = <
      item
        Value = 16.666666666666670000
      end
      item
        Value = 16.666666666666670000
      end
      item
        Value = 16.666666666666670000
      end
      item
        Value = 16.666666666666670000
      end
      item
        Value = 16.666666666666670000
      end
      item
        Value = 16.666666666666650000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = btnSites
        Row = 0
      end
      item
        Column = 1
        Control = btnDevices
        Row = 0
      end
      item
        Column = 2
        Control = btnEvents
        Row = 0
      end
      item
        Column = 3
        Control = btnWLANConfs
        Row = 0
      end
      item
        Column = 4
        Control = btnSiteStats
        Row = 0
      end
      item
        Column = 5
        Control = BitBtn1
        Row = 0
      end
      item
        Column = 0
        Control = btnListAdmins
        Row = 1
      end
      item
        Column = 1
        Control = btnDeleteUser
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ShowCaption = False
    TabOrder = 1
    object btnSites: TBitBtn
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 163
      Height = 34
      Align = alClient
      Caption = 'Sites'
      TabOrder = 0
      OnClick = btnSitesClick
      ExplicitWidth = 162
      ExplicitHeight = 38
    end
    object btnDevices: TBitBtn
      AlignWithMargins = True
      Left = 172
      Top = 3
      Width = 162
      Height = 34
      Align = alClient
      Caption = 'Devices'
      TabOrder = 1
      OnClick = btnDevicesClick
      ExplicitLeft = 171
      ExplicitWidth = 161
      ExplicitHeight = 38
    end
    object btnEvents: TBitBtn
      AlignWithMargins = True
      Left = 340
      Top = 3
      Width = 163
      Height = 34
      Align = alClient
      Caption = 'Events'
      TabOrder = 2
      OnClick = btnEventsClick
      ExplicitLeft = 338
      ExplicitWidth = 162
      ExplicitHeight = 38
    end
    object btnWLANConfs: TBitBtn
      AlignWithMargins = True
      Left = 509
      Top = 3
      Width = 163
      Height = 34
      Align = alClient
      Caption = 'WLANs'
      TabOrder = 3
      OnClick = btnWLANConfsClick
      ExplicitLeft = 506
      ExplicitWidth = 162
      ExplicitHeight = 38
    end
    object btnSiteStats: TBitBtn
      AlignWithMargins = True
      Left = 678
      Top = 3
      Width = 162
      Height = 34
      Align = alClient
      Caption = 'Site Stats'
      TabOrder = 4
      OnClick = btnSiteStatsClick
      ExplicitLeft = 674
      ExplicitWidth = 161
      ExplicitHeight = 38
    end
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 846
      Top = 3
      Width = 163
      Height = 34
      Align = alClient
      Caption = 'Change passphrase'
      TabOrder = 5
      OnClick = BitBtn1Click
      ExplicitLeft = 841
      ExplicitWidth = 162
      ExplicitHeight = 38
    end
    object btnListAdmins: TBitBtn
      AlignWithMargins = True
      Left = 3
      Top = 43
      Width = 163
      Height = 34
      Align = alClient
      Caption = 'Admins'
      TabOrder = 6
      OnClick = btnListAdminsClick
      ExplicitLeft = 47
      ExplicitTop = 47
      ExplicitWidth = 162
      ExplicitHeight = 38
    end
    object btnDeleteUser: TBitBtn
      AlignWithMargins = True
      Left = 172
      Top = 43
      Width = 162
      Height = 34
      Align = alClient
      Caption = 'Delete user'
      TabOrder = 7
      OnClick = btnDeleteUserClick
      ExplicitLeft = 47
      ExplicitTop = 47
      ExplicitHeight = 38
    end
  end
  object pnlSettings: TGridPanel
    Left = 0
    Top = 0
    Width = 1012
    Height = 125
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnlSettings'
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = pnlBaseURL
        Row = 0
      end
      item
        Column = 1
        Control = pnlUser
        Row = 0
      end
      item
        Column = 0
        Control = pnlPassword
        Row = 1
      end
      item
        Column = 1
        Control = pnlSiteID
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 1006
    object pnlBaseURL: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 500
      Height = 56
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Base URL'
      ShowCaption = False
      TabOrder = 0
      ExplicitWidth = 497
      object Label4: TLabel
        Left = 0
        Top = 0
        Width = 48
        Height = 15
        Align = alTop
        Caption = 'Base URL'
      end
      object editBaseURL: TEdit
        Left = 0
        Top = 15
        Width = 500
        Height = 23
        Align = alTop
        TabOrder = 0
        Text = 'https://unifi:8443/'
        ExplicitWidth = 497
      end
    end
    object pnlUser: TPanel
      AlignWithMargins = True
      Left = 509
      Top = 3
      Width = 500
      Height = 56
      Align = alClient
      BevelOuter = bvNone
      Caption = 'User'
      ShowCaption = False
      TabOrder = 1
      ExplicitLeft = 506
      ExplicitWidth = 497
      object Label3: TLabel
        Left = 0
        Top = 0
        Width = 53
        Height = 15
        Align = alTop
        Caption = 'Username'
      end
      object editUser: TEdit
        Left = 0
        Top = 15
        Width = 500
        Height = 23
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 497
      end
    end
    object pnlPassword: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 65
      Width = 500
      Height = 57
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Password'
      ShowCaption = False
      TabOrder = 2
      ExplicitWidth = 497
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 50
        Height = 15
        Align = alTop
        Caption = 'Password'
      end
      object editPassword: TEdit
        Left = 0
        Top = 15
        Width = 500
        Height = 23
        Align = alTop
        PasswordChar = '*'
        TabOrder = 0
        ExplicitWidth = 497
      end
    end
    object pnlSiteID: TPanel
      AlignWithMargins = True
      Left = 509
      Top = 65
      Width = 500
      Height = 57
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Site Id'
      ShowCaption = False
      TabOrder = 3
      ExplicitLeft = 506
      ExplicitWidth = 497
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 19
        Height = 15
        Align = alTop
        Caption = 'Site'
      end
      object editSiteID: TComboBox
        Left = 0
        Top = 15
        Width = 500
        Height = 23
        Align = alTop
        TabOrder = 0
        OnChange = editSiteIDChange
      end
    end
  end
  object TextEditorResponse: TMemo
    Left = 0
    Top = 205
    Width = 1012
    Height = 525
    Cursor = crIBeam
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
