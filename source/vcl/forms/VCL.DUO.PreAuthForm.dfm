object FormDuoPreAuth: TFormDuoPreAuth
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'DUO authentication options'
  ClientHeight = 243
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 199
    Width = 608
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 217
    ExplicitWidth = 618
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 540
      Top = 3
      Width = 75
      Height = 35
      Action = ActionCancel
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 459
      Top = 3
      Width = 75
      Height = 35
      Action = ActionOk
      Align = alRight
      Caption = 'Ok'
      Default = True
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 50
    Width = 608
    Height = 143
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 618
    ExplicitHeight = 161
    object imgLogo: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 161
      Align = alLeft
      Center = True
      Proportional = True
      Stretch = True
      ExplicitLeft = 28
      ExplicitTop = 28
      ExplicitHeight = 105
    end
    object GridPanel1: TGridPanel
      Left = 105
      Top = 0
      Width = 513
      Height = 161
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Panel3
          Row = 0
        end
        item
          Column = 0
          Control = Panel4
          Row = 1
        end
        item
          Column = 0
          Control = pnlPasscode
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
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 507
        Height = 48
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 35
          Height = 15
          Align = alTop
          Caption = 'Device'
        end
        object comboDevices: TComboBox
          Left = 0
          Top = 15
          Width = 507
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 0
          OnChange = comboDevicesChange
        end
      end
      object Panel4: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 51
        Width = 497
        Height = 41
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitTop = 57
        ExplicitWidth = 507
        ExplicitHeight = 47
        object Label2: TLabel
          Left = 0
          Top = 0
          Width = 33
          Height = 15
          Align = alTop
          Caption = 'Factor'
        end
        object comboDeviceFactor: TComboBox
          Left = 0
          Top = 15
          Width = 507
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 0
          OnChange = comboDeviceFactorChange
        end
      end
      object pnlPasscode: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 98
        Width = 497
        Height = 42
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitTop = 110
        ExplicitWidth = 507
        ExplicitHeight = 48
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 49
          Height = 15
          Align = alTop
          Caption = 'Passcode'
        end
        object editPasscode: TEdit
          Left = 0
          Top = 15
          Width = 507
          Height = 23
          Align = alTop
          TabOrder = 0
          OnKeyPress = editPasscodeKeyPress
        end
      end
    end
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 608
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 618
    object imgHeader: TImage
      Left = 513
      Top = 0
      Width = 105
      Height = 41
      Align = alRight
      Center = True
      Proportional = True
      Stretch = True
      ExplicitLeft = 4
    end
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 99
      Height = 15
      Align = alClient
      Caption = 'Verify your identity'
      Layout = tlCenter
    end
  end
  object ActionList: TActionList
    Left = 44
    Top = 52
    object ActionOk: TAction
      Caption = 'Ok'
      OnExecute = ActionOkExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      OnExecute = ActionCancelExecute
    end
  end
end
