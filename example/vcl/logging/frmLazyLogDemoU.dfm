object frmLazyLogDemo: TfrmLazyLogDemo
  Left = 0
  Top = 0
  Caption = 'LazyLog Demo Application'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 221
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblStatus: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 205
      Width = 894
      Height = 13
      Align = alBottom
      Caption = 'Active Threads: 0 | Log Entries: 0'
      ExplicitWidth = 161
    end
    object btnAddRandomMessages: TButton
      Left = 16
      Top = 16
      Width = 185
      Height = 41
      Caption = 'Add Random Messages'
      TabOrder = 0
      OnClick = btnAddRandomMessagesClick
    end
    object btnSpawnThreads: TButton
      Left = 216
      Top = 16
      Width = 185
      Height = 41
      Caption = 'Spawn Worker Threads'
      TabOrder = 1
      OnClick = btnSpawnThreadsClick
    end
    object btnClearLog: TButton
      Left = 416
      Top = 16
      Width = 185
      Height = 41
      Caption = 'Clear All Logs'
      TabOrder = 2
      OnClick = btnClearLogClick
    end
    object btnPurgeLogs: TButton
      Left = 616
      Top = 16
      Width = 185
      Height = 41
      Caption = 'Purge Old Logs (7 days)'
      TabOrder = 3
      OnClick = btnPurgeLogsClick
    end
  end
  object lvActions: TListView
    Left = 0
    Top = 221
    Width = 900
    Height = 150
    Align = alTop
    Columns = <>
    GridLines = True
    Items.ItemData = {050000000000000000}
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object memoLog: TMemo
    Left = 0
    Top = 371
    Width = 900
    Height = 229
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object timerRefresh: TTimer
    Enabled = False
    OnTimer = timerRefreshTimer
    Left = 832
    Top = 24
  end
  object timerActions: TTimer
    Enabled = False
    OnTimer = timerActionsTimer
    Left = 832
    Top = 112
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 128
    Top = 140
  end
end
