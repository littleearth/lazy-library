object frmAzureDemo: TfrmAzureDemo
  Left = 0
  Top = 0
  Caption = 'Azure Management Demo'
  ClientHeight = 576
  ClientWidth = 786
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
    Top = 424
    Width = 786
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 156
    ExplicitWidth = 200
  end
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 786
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
    object Label1: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 151
      Height = 32
      Align = alClient
      Caption = 'Tennant ID'
      ExplicitWidth = 56
      ExplicitHeight = 15
    end
    object editTennantID: TEdit
      AlignWithMargins = True
      Left = 161
      Top = 4
      Width = 621
      Height = 32
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 23
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 42
      Width = 151
      Height = 33
      Align = alClient
      Caption = 'Client ID'
      ExplicitWidth = 45
      ExplicitHeight = 15
    end
    object editClientID: TEdit
      AlignWithMargins = True
      Left = 161
      Top = 42
      Width = 621
      Height = 33
      Align = alClient
      TabOrder = 1
      ExplicitHeight = 23
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 81
      Width = 151
      Height = 32
      Align = alClient
      Caption = 'Client Secret'
      ExplicitWidth = 66
      ExplicitHeight = 15
    end
    object editClientSecret: TEdit
      AlignWithMargins = True
      Left = 161
      Top = 81
      Width = 621
      Height = 32
      Align = alClient
      PasswordChar = '*'
      TabOrder = 2
      ExplicitHeight = 23
    end
  end
  object GridPanel2: TGridPanel
    Left = 0
    Top = 117
    Width = 786
    Height = 39
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
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 1
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 190
      Height = 31
      Action = ActionExecute
      Align = alClient
      Caption = 'Execute'
      TabOrder = 0
    end
  end
  object ListViewVirtualMachines: TListView
    Left = 0
    Top = 156
    Width = 786
    Height = 268
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'VM Name'
      end
      item
        AutoSize = True
        Caption = 'CPU credits remaining'
      end
      item
        AutoSize = True
        Caption = 'CPU credits consumed'
      end
      item
        AutoSize = True
        Caption = 'Network Out Total'
      end>
    GridLines = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object pnlLog: TPanel
    Left = 0
    Top = 427
    Width = 786
    Height = 149
    Align = alBottom
    TabOrder = 3
    object Label4: TLabel
      Left = 1
      Top = 1
      Width = 784
      Height = 15
      Align = alTop
      Caption = 'Log'
      ExplicitWidth = 20
    end
    object memoLog: TMemo
      Left = 1
      Top = 16
      Width = 784
      Height = 132
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object ActionList: TActionList
    Left = 88
    Top = 228
    object ActionExecute: TAction
      Caption = 'Execute'
      OnExecute = ActionExecuteExecute
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnIdle = ApplicationEventsIdle
    Left = 132
    Top = 204
  end
end
