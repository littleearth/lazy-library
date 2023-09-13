object frmAccountsDemo: TfrmAccountsDemo
  Left = 0
  Top = 0
  Caption = 'DUO Accounts demo'
  ClientHeight = 441
  ClientWidth = 624
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
  object Panel1: TPanel
    Left = 0
    Top = 265
    Width = 624
    Height = 176
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitWidth = 614
    ExplicitHeight = 158
    object imgLogo: TImage
      Left = 518
      Top = 1
      Width = 105
      Height = 174
      Align = alRight
      Center = True
      Proportional = True
      Stretch = True
      ExplicitLeft = 456
      ExplicitTop = 28
      ExplicitHeight = 105
    end
    object memoLog: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 511
      Height = 168
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitWidth = 501
      ExplicitHeight = 150
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 265
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 614
    object GridPanel1: TGridPanel
      Left = 1
      Top = 204
      Width = 622
      Height = 60
      Align = alBottom
      ColumnCollection = <
        item
          Value = 20.000000000000000000
        end
        item
          Value = 20.000000000000000000
        end
        item
          Value = 20.000000000000000000
        end
        item
          Value = 20.000000000000000000
        end
        item
          Value = 20.000000000000000000
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
          Column = 4
          Control = BitBtn5
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      ExplicitWidth = 612
      object BitBtn1: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 118
        Height = 52
        Action = ActionGetAccounts
        Align = alClient
        Caption = 'Get accounts'
        TabOrder = 0
        ExplicitWidth = 116
      end
      object BitBtn2: TBitBtn
        AlignWithMargins = True
        Left = 128
        Top = 4
        Width = 118
        Height = 52
        Action = ActionGetBilling
        Align = alClient
        Caption = 'Get billing'
        TabOrder = 1
        ExplicitLeft = 126
        ExplicitWidth = 116
      end
      object BitBtn3: TBitBtn
        AlignWithMargins = True
        Left = 252
        Top = 4
        Width = 118
        Height = 52
        Action = ActionGetTelephonyCredits
        Align = alClient
        Caption = 'Get telephone credits'
        TabOrder = 2
        ExplicitLeft = 248
        ExplicitWidth = 116
      end
      object BitBtn4: TBitBtn
        AlignWithMargins = True
        Left = 376
        Top = 4
        Width = 118
        Height = 52
        Action = ActionGetLogo
        Align = alClient
        Caption = 'Get logo'
        TabOrder = 3
        ExplicitLeft = 370
        ExplicitWidth = 116
      end
      object BitBtn5: TBitBtn
        AlignWithMargins = True
        Left = 500
        Top = 4
        Width = 118
        Height = 52
        Action = ActionGetAllUsers
        Align = alClient
        Caption = 'Get all users'
        TabOrder = 4
        ExplicitLeft = 492
        ExplicitWidth = 116
      end
    end
    object GridPanel2: TGridPanel
      Left = 1
      Top = 1
      Width = 622
      Height = 203
      Align = alClient
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
          Control = Panel3
          Row = 0
        end
        item
          Column = 1
          Control = Panel4
          Row = 0
        end
        item
          Column = 2
          Control = Panel5
          Row = 0
        end
        item
          Column = 3
          Control = Panel6
          Row = 0
        end
        item
          Column = 0
          Control = Panel7
          Row = 1
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
      ExplicitWidth = 612
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 146
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 149
          Height = 15
          Align = alTop
          Caption = 'Integration Key'
          ExplicitWidth = 80
        end
        object editIntegrationKey: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
          ExplicitWidth = 146
        end
      end
      object Panel4: TPanel
        AlignWithMargins = True
        Left = 159
        Top = 4
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 156
        ExplicitWidth = 147
        object Label2: TLabel
          Left = 0
          Top = 0
          Width = 149
          Height = 15
          Align = alTop
          Caption = 'Secret Key'
          ExplicitWidth = 54
        end
        object editSecretKey: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          PasswordChar = '*'
          TabOrder = 0
          ExplicitWidth = 147
        end
      end
      object Panel5: TPanel
        AlignWithMargins = True
        Left = 314
        Top = 4
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitLeft = 309
        ExplicitWidth = 146
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 149
          Height = 15
          Align = alTop
          Caption = 'Hostname'
          ExplicitWidth = 55
        end
        object editHostname: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
          ExplicitWidth = 146
        end
      end
      object Panel6: TPanel
        AlignWithMargins = True
        Left = 469
        Top = 4
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitLeft = 461
        ExplicitWidth = 147
        object Label4: TLabel
          Left = 0
          Top = 0
          Width = 149
          Height = 15
          Align = alTop
          Caption = 'Account ID'
          ExplicitWidth = 59
        end
        object editAccountID: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
          ExplicitWidth = 147
        end
      end
      object Panel7: TPanel
        AlignWithMargins = True
        Left = 4
        Top = 71
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 4
        ExplicitWidth = 146
        object Label5: TLabel
          Left = 0
          Top = 0
          Width = 149
          Height = 15
          Align = alTop
          Caption = 'Account hostname'
          ExplicitWidth = 101
        end
        object editAccountAPIHostname: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
          ExplicitWidth = 146
        end
      end
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 44
    Top = 140
    object ActionGetAccounts: TAction
      Caption = 'Get accounts'
      OnExecute = ActionGetAccountsExecute
    end
    object ActionGetBilling: TAction
      Caption = 'Get billing'
      OnExecute = ActionGetBillingExecute
    end
    object ActionGetLogo: TAction
      Caption = 'Get logo'
      OnExecute = ActionGetLogoExecute
    end
    object ActionGetAllUsers: TAction
      Caption = 'Get all users'
      OnExecute = ActionGetAllUsersExecute
    end
    object ActionGetTelephonyCredits: TAction
      Caption = 'Get telephone credits'
      OnExecute = ActionGetTelephonyCreditsExecute
    end
  end
end
