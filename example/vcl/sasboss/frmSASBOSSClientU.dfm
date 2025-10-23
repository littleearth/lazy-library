object frmSASBOSSClient: TfrmSASBOSSClient
  Left = 0
  Top = 0
  Caption = 'SASBOSS'
  ClientHeight = 442
  ClientWidth = 628
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
  object GridPanel1: TGridPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 622
    Height = 73
    Align = alTop
    ColumnCollection = <
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333340000
      end
      item
        Value = 33.333333333333310000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Panel1
        Row = 0
      end
      item
        Column = 1
        Control = Panel2
        Row = 0
      end
      item
        Column = 2
        Control = GridPanel3
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 0
    ExplicitWidth = 618
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 201
      Height = 65
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 199
      object Label1: TLabel
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
        Width = 201
        Height = 23
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 199
      end
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 211
      Top = 4
      Width = 200
      Height = 65
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 209
      object Label2: TLabel
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
        Width = 200
        Height = 23
        Align = alTop
        PasswordChar = '*'
        TabOrder = 0
      end
    end
    object GridPanel3: TGridPanel
      Left = 414
      Top = 1
      Width = 207
      Height = 71
      Align = alClient
      ColumnCollection = <
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = BitBtn3
          Row = 0
        end
        item
          Column = 0
          Control = BitBtn4
          Row = 1
        end>
      RowCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      TabOrder = 0
      ExplicitLeft = 412
      ExplicitWidth = 205
      object BitBtn3: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 199
        Height = 28
        Action = ActionLogin
        Align = alClient
        Caption = 'Login'
        TabOrder = 0
        ExplicitWidth = 197
      end
      object BitBtn4: TBitBtn
        AlignWithMargins = True
        Left = 4
        Top = 38
        Width = 199
        Height = 29
        Action = ActionLogout
        Align = alClient
        Caption = 'Logout'
        TabOrder = 1
        ExplicitWidth = 197
      end
    end
  end
  object GridPanel2: TGridPanel
    AlignWithMargins = True
    Left = 3
    Top = 82
    Width = 622
    Height = 41
    Align = alTop
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
        Value = 16.666666666666670000
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
        Control = BitBtn5
        Row = 0
      end
      item
        Column = 3
        Control = BitBtn6
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
      Width = 97
      Height = 33
      Action = ActionEnterprises
      Align = alClient
      Caption = 'Enterprises'
      TabOrder = 0
      ExplicitWidth = 82
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 107
      Top = 4
      Width = 98
      Height = 33
      Action = ActionGroups
      Align = alClient
      Caption = 'Groups'
      TabOrder = 1
      ExplicitLeft = 92
      ExplicitWidth = 82
    end
    object BitBtn5: TBitBtn
      AlignWithMargins = True
      Left = 211
      Top = 4
      Width = 97
      Height = 33
      Action = ActionInvoices
      Align = alClient
      Caption = 'Invoices'
      TabOrder = 2
      ExplicitLeft = 92
      ExplicitTop = 8
      ExplicitWidth = 75
      ExplicitHeight = 25
    end
    object BitBtn6: TBitBtn
      AlignWithMargins = True
      Left = 314
      Top = 4
      Width = 97
      Height = 33
      Action = ActionInvoiceCharges
      Align = alClient
      Caption = 'Invoice charges'
      TabOrder = 3
      ExplicitLeft = 92
      ExplicitTop = 8
      ExplicitWidth = 75
      ExplicitHeight = 25
    end
  end
  object ListViewData: TListView
    AlignWithMargins = True
    Left = 3
    Top = 129
    Width = 622
    Height = 310
    Align = alClient
    Columns = <>
    GridLines = True
    TabOrder = 2
    ViewStyle = vsReport
    ExplicitWidth = 618
    ExplicitHeight = 309
  end
  object ActionList: TActionList
    Left = 28
    Top = 152
    object ActionLogin: TAction
      Caption = 'Login'
      OnExecute = ActionLoginExecute
    end
    object ActionLogout: TAction
      Caption = 'Logout'
      OnUpdate = ActionLogoutUpdate
    end
    object ActionEnterprises: TAction
      Caption = 'Enterprises'
      OnExecute = ActionEnterprisesExecute
      OnUpdate = ActionLogoutUpdate
    end
    object ActionInvoices: TAction
      Caption = 'Invoices'
      OnExecute = ActionInvoicesExecute
      OnUpdate = ActionLogoutUpdate
    end
    object ActionGroups: TAction
      Caption = 'Groups'
      OnExecute = ActionGroupsExecute
      OnUpdate = ActionLogoutUpdate
    end
    object ActionInvoiceCharges: TAction
      Caption = 'Invoice charges'
      OnExecute = ActionInvoiceChargesExecute
      OnUpdate = ActionLogoutUpdate
    end
  end
end
