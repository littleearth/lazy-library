object frmAuthDemo: TfrmAuthDemo
  Left = 0
  Top = 0
  Caption = 'DUO authentication demo'
  ClientHeight = 423
  ClientWidth = 614
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
    Width = 614
    Height = 158
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitWidth = 620
    ExplicitHeight = 175
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
      ExplicitWidth = 507
      ExplicitHeight = 167
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 614
    Height = 265
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 620
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
      ExplicitWidth = 618
      object BitBtn1: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 118
        Height = 52
        Action = ActionPing
        Align = alClient
        Caption = 'Ping'
        TabOrder = 0
      end
      object BitBtn2: TBitBtn
        AlignWithMargins = True
        Left = 128
        Top = 4
        Width = 118
        Height = 52
        Action = ActionCheck
        Align = alClient
        Caption = 'Check'
        TabOrder = 1
      end
      object BitBtn3: TBitBtn
        AlignWithMargins = True
        Left = 252
        Top = 4
        Width = 118
        Height = 52
        Action = ActionLogo
        Align = alClient
        Caption = 'Logo'
        TabOrder = 2
      end
      object BitBtn4: TBitBtn
        AlignWithMargins = True
        Left = 376
        Top = 4
        Width = 118
        Height = 52
        Action = ActionPreAuth
        Align = alClient
        Caption = 'Prompt login'
        TabOrder = 3
      end
      object BitBtn5: TBitBtn
        AlignWithMargins = True
        Left = 500
        Top = 4
        Width = 118
        Height = 52
        Action = ActionLogin
        Align = alClient
        Caption = 'Auto login'
        TabOrder = 4
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
        end
        item
          Column = 1
          Control = Panel8
          Row = 1
        end
        item
          Column = 2
          Control = Panel9
          Row = 1
        end
        item
          Column = 3
          Control = Panel10
          Row = 1
        end
        item
          Column = 0
          Control = Panel11
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
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 80
          Height = 15
          Align = alTop
          Caption = 'Integration Key'
        end
        object editIntegrationKey: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
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
        object Label2: TLabel
          Left = 0
          Top = 0
          Width = 54
          Height = 15
          Align = alTop
          Caption = 'Secret Key'
        end
        object editSecretKey: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          PasswordChar = '*'
          TabOrder = 0
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
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 55
          Height = 15
          Align = alTop
          Caption = 'Hostname'
        end
        object editHostname: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
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
        object Label4: TLabel
          Left = 0
          Top = 0
          Width = 53
          Height = 15
          Align = alTop
          Caption = 'Username'
        end
        object editUsername: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
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
        object Label5: TLabel
          Left = 0
          Top = 0
          Width = 71
          Height = 15
          Align = alTop
          Caption = 'Display name'
        end
        object editDisplayName: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
        end
      end
      object Panel8: TPanel
        AlignWithMargins = True
        Left = 159
        Top = 71
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 5
        object Label6: TLabel
          Left = 0
          Top = 0
          Width = 50
          Height = 15
          Align = alTop
          Caption = 'Push info'
        end
        object editPushInfo: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
          Text = 'login=desktop&ip=localhost'
        end
      end
      object Panel9: TPanel
        AlignWithMargins = True
        Left = 314
        Top = 71
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 6
        object Label7: TLabel
          Left = 0
          Top = 0
          Width = 24
          Height = 15
          Align = alTop
          Caption = 'Type'
        end
        object editType: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
          Text = 'Login request'
        end
      end
      object Panel10: TPanel
        AlignWithMargins = True
        Left = 469
        Top = 71
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 7
        object Label8: TLabel
          Left = 0
          Top = 0
          Width = 98
          Height = 15
          Align = alTop
          Caption = 'Request hostname'
        end
        object editRequestHostname: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
        end
      end
      object Panel11: TPanel
        AlignWithMargins = True
        Left = 4
        Top = 138
        Width = 149
        Height = 61
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 8
        object Label9: TLabel
          Left = 0
          Top = 0
          Width = 98
          Height = 15
          Align = alTop
          Caption = 'Request ip address'
        end
        object editRequestIPAddress: TEdit
          Left = 0
          Top = 15
          Width = 149
          Height = 23
          Align = alTop
          TabOrder = 0
        end
      end
    end
  end
  object ActionList: TActionList
    Left = 44
    Top = 140
    object ActionPing: TAction
      Caption = 'Ping'
      OnExecute = ActionPingExecute
    end
    object ActionCheck: TAction
      Caption = 'Check'
      OnExecute = ActionCheckExecute
    end
    object ActionPreAuth: TAction
      Caption = 'Prompt login'
      OnExecute = ActionPreAuthExecute
    end
    object ActionLogin: TAction
      Caption = 'Auto login'
      OnExecute = ActionLoginExecute
    end
    object ActionLogo: TAction
      Caption = 'Logo'
      OnExecute = ActionLogoExecute
    end
  end
end
