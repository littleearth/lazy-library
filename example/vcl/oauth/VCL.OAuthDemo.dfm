object frmOAuthDemo: TfrmOAuthDemo
  Left = 0
  Top = 0
  Caption = 'OAuth Demo'
  ClientHeight = 712
  ClientWidth = 1002
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
  object GridPanel2: TGridPanel
    Left = 0
    Top = 302
    Width = 1002
    Height = 45
    Align = alTop
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
        Control = BitBtn1
        Row = 0
      end
      item
        Column = 1
        Control = GridPanel6
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 2
    ExplicitWidth = 990
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 494
      Height = 37
      Action = ActionAuthenticate
      Align = alClient
      Caption = 'Authenticate'
      TabOrder = 1
    end
    object GridPanel6: TGridPanel
      Left = 501
      Top = 1
      Width = 500
      Height = 43
      Align = alClient
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
          Control = editAuthToken
          Row = 0
        end
        item
          Column = 1
          Control = editRefreshToken
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      object editAuthToken: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 243
        Height = 35
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 23
      end
      object editRefreshToken: TEdit
        AlignWithMargins = True
        Left = 253
        Top = 4
        Width = 243
        Height = 35
        Align = alClient
        TabOrder = 1
        ExplicitHeight = 23
      end
    end
  end
  object pnlLog: TPanel
    Left = 0
    Top = 610
    Width = 1002
    Height = 102
    Align = alBottom
    TabOrder = 5
    ExplicitTop = 590
    ExplicitWidth = 990
    object Label4: TLabel
      Left = 1
      Top = 1
      Width = 1000
      Height = 15
      Align = alTop
      Caption = 'Log'
      ExplicitWidth = 20
    end
    object memoLog: TMemo
      Left = 1
      Top = 16
      Width = 1000
      Height = 85
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object GridPanel3: TGridPanel
    Left = 0
    Top = 347
    Width = 1002
    Height = 140
    Align = alTop
    ColumnCollection = <
      item
        Value = 15.000000000000000000
      end
      item
        Value = 70.000000000000000000
      end
      item
        Value = 15.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Label6
        Row = 0
      end
      item
        Column = 1
        Control = editGet
        Row = 0
      end
      item
        Column = 2
        Control = BitBtn2
        Row = 0
      end>
    RowCollection = <
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
    TabOrder = 3
    ExplicitWidth = 990
    object Label6: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 144
      Height = 28
      Align = alClient
      Caption = 'Get'
      Layout = tlCenter
      ExplicitWidth = 18
      ExplicitHeight = 15
    end
    object editGet: TEdit
      AlignWithMargins = True
      Left = 154
      Top = 4
      Width = 694
      Height = 28
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 23
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 854
      Top = 4
      Width = 144
      Height = 28
      Action = ActionGet
      Align = alClient
      Caption = 'Get'
      TabOrder = 1
      ExplicitLeft = 844
      ExplicitWidth = 142
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 487
    Width = 1002
    Height = 123
    Align = alClient
    TabOrder = 4
    ExplicitWidth = 990
    ExplicitHeight = 103
    object Label9: TLabel
      Left = 1
      Top = 1
      Width = 1000
      Height = 15
      Align = alTop
      Caption = 'Response'
      ExplicitWidth = 50
    end
    object memoResponse: TMemo
      Left = 1
      Top = 16
      Width = 1000
      Height = 106
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object GridPanel4: TGridPanel
    Left = 0
    Top = 37
    Width = 1002
    Height = 265
    Align = alTop
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
        Control = GridPanel1
        Row = 0
      end
      item
        Column = 1
        Control = Panel2
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 1
    ExplicitWidth = 990
    object GridPanel1: TGridPanel
      Left = 1
      Top = 1
      Width = 500
      Height = 263
      Align = alClient
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
          Control = editClientID
          Row = 0
        end
        item
          Column = 0
          Control = Label2
          Row = 1
        end
        item
          Column = 1
          Control = editClientSecret
          Row = 1
        end
        item
          Column = 0
          Control = Label3
          Row = 2
        end
        item
          Column = 1
          Control = editScope
          Row = 2
        end
        item
          Column = 0
          Control = Label5
          Row = 3
        end
        item
          Column = 1
          Control = editTokenEndPoint
          Row = 3
        end
        item
          Column = 0
          Control = Label7
          Row = 4
        end
        item
          Column = 1
          Control = editRESTEndPoint
          Row = 4
        end
        item
          Column = 0
          Control = Label12
          Row = 5
        end
        item
          Column = 1
          Control = editAuthorizeEndPoint
          Row = 5
        end
        item
          Column = 0
          Control = Label13
          Row = 6
        end
        item
          Column = 1
          Control = editRedirectURL
          Row = 6
        end
        item
          Column = 0
          Control = Label15
          Row = 7
        end
        item
          Column = 1
          Control = editResource
          Row = 7
        end>
      RowCollection = <
        item
          Value = 12.500000000000000000
        end
        item
          Value = 12.500000000000000000
        end
        item
          Value = 12.500000000000000000
        end
        item
          Value = 12.500000000000000000
        end
        item
          Value = 12.500000000000000000
        end
        item
          Value = 12.500000000000000000
        end
        item
          Value = 12.500000000000000000
        end
        item
          Value = 12.500000000000000000
        end>
      TabOrder = 0
      object Label1: TLabel
        Left = 1
        Top = 1
        Width = 100
        Height = 33
        Align = alClient
        Caption = 'Client ID'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 45
        ExplicitHeight = 15
      end
      object editClientID: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 4
        Width = 392
        Height = 27
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 23
      end
      object Label2: TLabel
        Left = 1
        Top = 34
        Width = 100
        Height = 32
        Align = alClient
        Caption = 'Client Secret'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 66
        ExplicitHeight = 15
      end
      object editClientSecret: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 37
        Width = 392
        Height = 26
        Align = alClient
        PasswordChar = '*'
        TabOrder = 1
        ExplicitHeight = 23
      end
      object Label3: TLabel
        Left = 1
        Top = 66
        Width = 100
        Height = 33
        Align = alClient
        Caption = 'Scope'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 32
        ExplicitHeight = 15
      end
      object editScope: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 69
        Width = 392
        Height = 27
        Align = alClient
        TabOrder = 2
        Text = 'token'
        ExplicitHeight = 23
      end
      object Label5: TLabel
        Left = 1
        Top = 99
        Width = 100
        Height = 32
        Align = alClient
        Caption = 'Token end point'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 85
        ExplicitHeight = 15
      end
      object editTokenEndPoint: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 102
        Width = 392
        Height = 26
        Align = alClient
        TabOrder = 3
        ExplicitHeight = 23
      end
      object Label7: TLabel
        Left = 1
        Top = 131
        Width = 100
        Height = 33
        Align = alClient
        Caption = 'REST end point'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 79
        ExplicitHeight = 15
      end
      object editRESTEndPoint: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 134
        Width = 392
        Height = 27
        Align = alClient
        TabOrder = 4
        ExplicitHeight = 23
      end
      object Label12: TLabel
        Left = 1
        Top = 164
        Width = 100
        Height = 33
        Align = alClient
        Caption = 'Authorize end point'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 77
        ExplicitHeight = 30
      end
      object editAuthorizeEndPoint: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 167
        Width = 392
        Height = 27
        Align = alClient
        TabOrder = 5
        ExplicitHeight = 23
      end
      object Label13: TLabel
        Left = 1
        Top = 197
        Width = 100
        Height = 32
        Align = alClient
        Caption = 'Redirect URL'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 67
        ExplicitHeight = 15
      end
      object editRedirectURL: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 200
        Width = 392
        Height = 26
        Align = alClient
        TabOrder = 6
        ExplicitHeight = 23
      end
      object Label15: TLabel
        Left = 1
        Top = 229
        Width = 100
        Height = 33
        Align = alClient
        Caption = 'Resource'
        Layout = tlCenter
        WordWrap = True
        ExplicitWidth = 48
        ExplicitHeight = 15
      end
      object editResource: TEdit
        AlignWithMargins = True
        Left = 104
        Top = 232
        Width = 392
        Height = 27
        Align = alClient
        TabOrder = 7
        ExplicitHeight = 23
      end
    end
    object Panel2: TPanel
      Left = 501
      Top = 1
      Width = 500
      Height = 263
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object rgGrantType: TRadioGroup
        Left = 0
        Top = 0
        Width = 500
        Height = 80
        Align = alTop
        Caption = 'Grant type'
        Columns = 2
        ItemIndex = 1
        Items.Strings = (
          'gtAuthorizationCode'
          'gtClientCredentials'
          'gtRefreshToken'
          'gtCustom')
        ShowFrame = False
        TabOrder = 0
      end
      object GridPanel5: TGridPanel
        Left = 0
        Top = 80
        Width = 500
        Height = 183
        Align = alClient
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
            Control = Label11
            Row = 0
          end
          item
            Column = 1
            Control = editGrantTypeCustom
            Row = 0
          end
          item
            Column = 0
            Control = Label10
            Row = 1
          end
          item
            Column = 1
            Control = editTokenAuthorizationName
            Row = 1
          end
          item
            Column = 0
            Control = Label8
            Row = 2
          end
          item
            Column = 1
            Control = editHeaders
            Row = 2
          end
          item
            Column = 0
            Control = Label14
            Row = 3
          end
          item
            Column = 1
            Control = editURLVariables
            Row = 3
          end>
        RowCollection = <
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
        TabOrder = 1
        object Label11: TLabel
          Left = 1
          Top = 1
          Width = 100
          Height = 36
          Align = alClient
          Caption = 'Grant type custom'
          Layout = tlCenter
          WordWrap = True
          ExplicitWidth = 98
          ExplicitHeight = 15
        end
        object editGrantTypeCustom: TEdit
          AlignWithMargins = True
          Left = 104
          Top = 4
          Width = 392
          Height = 30
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 23
        end
        object Label10: TLabel
          Left = 1
          Top = 37
          Width = 100
          Height = 36
          Align = alClient
          Caption = 'Token Auth Name'
          Layout = tlCenter
          WordWrap = True
          ExplicitWidth = 95
          ExplicitHeight = 15
        end
        object editTokenAuthorizationName: TEdit
          AlignWithMargins = True
          Left = 104
          Top = 40
          Width = 392
          Height = 30
          Align = alClient
          TabOrder = 1
          Text = 'Bearer'
          ExplicitHeight = 23
        end
        object Label8: TLabel
          Left = 1
          Top = 73
          Width = 100
          Height = 37
          Align = alClient
          Caption = 'Headers (key=value;key=value)'
          Layout = tlCenter
          WordWrap = True
          ExplicitWidth = 119
          ExplicitHeight = 30
        end
        object editHeaders: TEdit
          AlignWithMargins = True
          Left = 104
          Top = 76
          Width = 392
          Height = 31
          Align = alClient
          TabOrder = 2
          ExplicitHeight = 23
        end
        object Label14: TLabel
          Left = 1
          Top = 110
          Width = 100
          Height = 36
          Align = alClient
          Caption = 'URL variables (key=value;key=value)'
          Layout = tlCenter
          WordWrap = True
          ExplicitWidth = 119
          ExplicitHeight = 30
        end
        object editURLVariables: TEdit
          AlignWithMargins = True
          Left = 104
          Top = 113
          Width = 392
          Height = 30
          Align = alClient
          TabOrder = 3
          ExplicitHeight = 23
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 1002
    Height = 37
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 990
    object GridPanel7: TGridPanel
      Left = 1
      Top = 1
      Width = 1000
      Height = 35
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
          Control = Panel4
          Row = 0
        end
        item
          Column = 1
          Control = Panel5
          Row = 0
        end
        item
          Column = 2
          Control = Panel6
          Row = 0
        end
        item
          Column = 3
          Control = comboBoxOptions
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      ExplicitWidth = 988
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 250
        Height = 33
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
      object Panel5: TPanel
        Left = 251
        Top = 1
        Width = 249
        Height = 33
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
      object Panel6: TPanel
        Left = 500
        Top = 1
        Width = 250
        Height = 33
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
      end
      object comboBoxOptions: TComboBox
        AlignWithMargins = True
        Left = 753
        Top = 4
        Width = 243
        Height = 23
        Align = alClient
        Style = csDropDownList
        TabOrder = 3
        OnChange = comboBoxOptionsChange
        Items.Strings = (
          'MS Graph'
          'Sophos'
          'Barracuda')
      end
    end
  end
  object ActionList: TActionList
    Left = 120
    Top = 364
    object ActionAuthenticate: TAction
      Caption = 'Authenticate'
      OnExecute = ActionAuthenticateExecute
    end
    object ActionGet: TAction
      Caption = 'Get'
      OnExecute = ActionGetExecute
      OnUpdate = ActionGetUpdate
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnIdle = ApplicationEventsIdle
    Left = 76
    Top = 300
  end
end
