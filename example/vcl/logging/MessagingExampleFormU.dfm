object frmMessagingExample: TfrmMessagingExample
  Left = 0
  Top = 0
  Caption = 'Lazy Log Messaging Example'
  ClientHeight = 600
  ClientWidth = 1008
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 267
    Width = 1008
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 321
    ExplicitWidth = 150
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1008
    Height = 97
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 800
    object Panel4: TPanel
      Left = 1
      Top = 55
      Width = 1006
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 798
      object chkSubscribeAll: TCheckBox
        AlignWithMargins = True
        Left = 189
        Top = 3
        Width = 150
        Height = 35
        Align = alLeft
        Caption = 'Subscribe to All Messages'
        TabOrder = 1
        OnClick = chkSubscribeAllClick
      end
      object chkSubscribeErrors: TCheckBox
        AlignWithMargins = True
        Left = 501
        Top = 3
        Width = 150
        Height = 35
        Align = alLeft
        Caption = 'Subscribe to Errors Only'
        TabOrder = 3
        OnClick = chkSubscribeErrorsClick
      end
      object chkSubscribeWarnings: TCheckBox
        AlignWithMargins = True
        Left = 345
        Top = 3
        Width = 150
        Height = 35
        Align = alLeft
        Caption = 'Subscribe to Warnings'
        TabOrder = 2
        OnClick = chkSubscribeWarningsClick
      end
      object chkSubscribeToExampleClass: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 180
        Height = 35
        Align = alLeft
        Caption = 'Subscribe to TExampleClass'
        TabOrder = 0
        OnClick = chkSubscribeToExampleClassClick
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 1006
      Height = 54
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 798
      object btnClear: TButton
        AlignWithMargins = True
        Left = 883
        Top = 3
        Width = 120
        Height = 48
        Align = alRight
        Caption = 'Clear'
        TabOrder = 6
        OnClick = btnClearClick
        ExplicitLeft = 675
      end
      object btnExampleClassLog: TButton
        AlignWithMargins = True
        Left = 507
        Top = 3
        Width = 120
        Height = 48
        Align = alLeft
        Caption = 'Log TExampleClass'
        TabOrder = 4
        OnClick = btnExampleClassLogClick
        ExplicitLeft = 611
        ExplicitTop = 0
      end
      object btnLogDebug: TButton
        AlignWithMargins = True
        Left = 381
        Top = 3
        Width = 120
        Height = 48
        Align = alLeft
        Caption = 'Log Debug'
        TabOrder = 3
        OnClick = btnLogDebugClick
      end
      object btnLogError: TButton
        AlignWithMargins = True
        Left = 255
        Top = 3
        Width = 120
        Height = 48
        Align = alLeft
        Caption = 'Log Error'
        TabOrder = 2
        OnClick = btnLogErrorClick
        ExplicitLeft = 268
        ExplicitTop = 16
        ExplicitHeight = 33
      end
      object btnLogInfo: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 120
        Height = 48
        Align = alLeft
        Caption = 'Log Information'
        TabOrder = 0
        OnClick = btnLogInfoClick
        ExplicitLeft = 129
      end
      object btnLogWarning: TButton
        AlignWithMargins = True
        Left = 129
        Top = 3
        Width = 120
        Height = 48
        Align = alLeft
        Caption = 'Log Warning'
        TabOrder = 1
        OnClick = btnLogWarningClick
        ExplicitLeft = 255
      end
      object btnTestProgress: TButton
        AlignWithMargins = True
        Left = 633
        Top = 3
        Width = 120
        Height = 48
        Align = alLeft
        Caption = 'Test Progress'
        TabOrder = 5
        OnClick = btnTestProgressClick
        ExplicitLeft = 561
      end
    end
  end
  object memoAllMessages: TMemo
    Left = 0
    Top = 127
    Width = 1008
    Height = 140
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
    ExplicitLeft = -1
    ExplicitTop = 124
  end
  object memoErrorsOnly: TMemo
    Left = 0
    Top = 300
    Width = 1008
    Height = 270
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 4
    ExplicitWidth = 800
  end
  object Panel1: TPanel
    Left = 0
    Top = 97
    Width = 1008
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 800
    object lblAllMessages: TLabel
      Left = 8
      Top = 8
      Width = 141
      Height = 13
      Caption = 'All Messages (All Subscribers)'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 570
    Width = 1008
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
    ExplicitWidth = 800
    object lblExampleClassMessages: TLabel
      Left = 8
      Top = 8
      Width = 172
      Height = 13
      Caption = 'Not listening to TExampleClass'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 270
    Width = 1008
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 800
    object lblErrorsOnly: TLabel
      Left = 8
      Top = 8
      Width = 157
      Height = 13
      Caption = 'Filtered Messages (By Log Level)'
    end
  end
end
