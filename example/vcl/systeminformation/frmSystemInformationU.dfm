object frmSystemInformation: TfrmSystemInformation
  Left = 0
  Top = 0
  Caption = 'System information'
  ClientHeight = 424
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  TextHeight = 15
  object memoSystemInformation: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 112
    Width = 612
    Height = 309
    Align = alClient
    Lines.Strings = (
      'memoSystemInformation')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 618
    Height = 109
    Align = alTop
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Panel1
        Row = 0
      end
      item
        Column = 0
        Control = Panel2
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
    ExplicitTop = 44
    ExplicitWidth = 628
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 626
      Height = 54
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 88
      ExplicitTop = 8
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 71
        Height = 15
        Align = alTop
        Caption = 'Memory load'
      end
      object ProgressBarMemoryLoad: TProgressBar
        AlignWithMargins = True
        Left = 3
        Top = 24
        Width = 620
        Height = 27
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 618
        ExplicitHeight = 17
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 55
      Width = 626
      Height = 53
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 88
      ExplicitTop = 8
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 49
        Height = 15
        Align = alTop
        Caption = 'CPU load'
      end
      object ProgressBarCPULoad: TProgressBar
        AlignWithMargins = True
        Left = 3
        Top = 24
        Width = 620
        Height = 26
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 6
        ExplicitTop = 27
      end
    end
  end
  object TimerSystemInformation: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = TimerSystemInformationTimer
    Left = 304
    Top = 224
  end
end
