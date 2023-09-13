object frmStopWatch: TfrmStopWatch
  Left = 0
  Top = 0
  Caption = 'Stop watch'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object GridPanel1: TGridPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 618
    Height = 173
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
        Control = Panel1
        Row = 1
      end
      item
        Column = 1
        Control = lblRuntimeDuration
        Row = 1
      end
      item
        Column = 2
        Control = lblStarted
        Row = 1
      end
      item
        Column = 3
        Control = lblEntryCount
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
    ExplicitWidth = 614
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 148
      Height = 80
      Action = ActionStart
      Align = alClient
      Caption = 'Start'
      TabOrder = 0
      ExplicitWidth = 147
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 158
      Top = 4
      Width = 148
      Height = 80
      Action = ActionStop
      Align = alClient
      Caption = 'Stop'
      TabOrder = 1
      ExplicitLeft = 157
      ExplicitWidth = 147
    end
    object BitBtn3: TBitBtn
      AlignWithMargins = True
      Left = 312
      Top = 4
      Width = 148
      Height = 80
      Action = ActionPause
      Align = alClient
      Caption = 'Pause'
      TabOrder = 2
      ExplicitLeft = 310
      ExplicitWidth = 147
    end
    object BitBtn4: TBitBtn
      AlignWithMargins = True
      Left = 466
      Top = 4
      Width = 148
      Height = 80
      Action = ActionResume
      Align = alClient
      Caption = 'Resume'
      TabOrder = 3
      ExplicitLeft = 463
      ExplicitWidth = 147
    end
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 90
      Width = 148
      Height = 79
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 4
      ExplicitWidth = 147
      object cbStayOnTop: TCheckBox
        Left = 0
        Top = 17
        Width = 148
        Height = 17
        Action = ActionStayOnTop
        Align = alTop
        TabOrder = 1
        ExplicitWidth = 147
      end
      object cbPauseOnLostFocus: TCheckBox
        Left = 0
        Top = 0
        Width = 148
        Height = 17
        Align = alTop
        Caption = 'Pause if form inactive'
        Checked = True
        State = cbChecked
        TabOrder = 0
        ExplicitWidth = 147
      end
    end
    object lblRuntimeDuration: TLabel
      Left = 155
      Top = 87
      Width = 154
      Height = 85
      Align = alClient
      Alignment = taCenter
      Caption = 'Run time'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 48
      ExplicitHeight = 15
    end
    object lblStarted: TLabel
      Left = 309
      Top = 87
      Width = 154
      Height = 85
      Align = alClient
      Alignment = taCenter
      Caption = 'Started'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 37
      ExplicitHeight = 15
    end
    object lblEntryCount: TLabel
      Left = 463
      Top = 87
      Width = 154
      Height = 85
      Align = alClient
      Alignment = taCenter
      Caption = 'Entry count'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 61
      ExplicitHeight = 15
    end
  end
  object pnlElapsedTime: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 182
    Width = 618
    Height = 80
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 614
  end
  object ListViewElapsedEntries: TListView
    Left = 0
    Top = 265
    Width = 624
    Height = 176
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Start'
      end
      item
        AutoSize = True
        Caption = 'End'
      end
      item
        AutoSize = True
        Caption = 'Duration'
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    ExplicitWidth = 620
    ExplicitHeight = 175
  end
  object ActionList: TActionList
    Left = 284
    Top = 296
    object ActionStart: TAction
      Caption = 'Start'
      OnExecute = ActionStartExecute
      OnUpdate = ActionStartUpdate
    end
    object ActionStop: TAction
      Caption = 'Stop'
      OnExecute = ActionStopExecute
      OnUpdate = ActionStopUpdate
    end
    object ActionPause: TAction
      Caption = 'Pause'
      OnExecute = ActionPauseExecute
      OnUpdate = ActionPauseUpdate
    end
    object ActionResume: TAction
      Caption = 'Resume'
      OnExecute = ActionResumeExecute
      OnUpdate = ActionResumeUpdate
    end
    object ActionStayOnTop: TAction
      Caption = 'Stay on stop'
      OnExecute = ActionStayOnTopExecute
      OnUpdate = ActionStayOnTopUpdate
    end
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 308
    Top = 228
  end
  object ApplicationEvents: TApplicationEvents
    OnActivate = ApplicationEventsActivate
    OnDeactivate = ApplicationEventsDeactivate
    Left = 88
    Top = 220
  end
end
