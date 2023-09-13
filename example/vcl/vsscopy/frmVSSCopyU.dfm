object frmVSSCopy: TfrmVSSCopy
  Left = 0
  Top = 0
  Caption = 'ABit VSS Copy'
  ClientHeight = 865
  ClientWidth = 1222
  Color = clBtnFace
  ParentFont = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 25
  object lblProgress: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 206
    Width = 1216
    Height = 25
    Align = alTop
    Alignment = taCenter
    Layout = tlCenter
    ExplicitLeft = 8
    ExplicitTop = 251
  end
  object BackupProgressBar: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 172
    Width = 1216
    Height = 28
    Align = alTop
    TabOrder = 0
    ExplicitTop = 205
  end
  object BackupLogMemo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 237
    Width = 1216
    Height = 625
    Align = alClient
    TabOrder = 1
    WordWrap = False
    ExplicitTop = 93
    ExplicitWidth = 729
    ExplicitHeight = 299
  end
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 1222
    Height = 169
    Align = alTop
    BevelOuter = bvNone
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
        Control = Panel2
        Row = 0
      end
      item
        Column = 1
        Control = Panel3
        Row = 0
      end
      item
        Column = 0
        Control = Panel4
        Row = 1
      end
      item
        Column = 1
        Control = Panel5
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 2
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 605
      Height = 78
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 120
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 605
        Height = 25
        Align = alTop
        Caption = 'Source'
        Layout = tlCenter
        ExplicitWidth = 54
      end
      object GridPanel3: TGridPanel
        Left = 0
        Top = 25
        Width = 605
        Height = 53
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 38.461538461538460000
          end
          item
            Value = 23.076923076923080000
          end
          item
            Value = 38.461538461538460000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = editSource
            Row = 0
          end
          item
            Column = 1
            Control = BitBtn2
            Row = 0
          end
          item
            Column = 2
            Control = editFileMask
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAuto
          end>
        TabOrder = 0
        ExplicitTop = 28
        ExplicitHeight = 67
        object editSource: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 227
          Height = 33
          Align = alTop
          TabOrder = 0
          Text = 'editSource'
          ExplicitLeft = 40
          ExplicitTop = 24
          ExplicitWidth = 121
        end
        object BitBtn2: TBitBtn
          AlignWithMargins = True
          Left = 236
          Top = 3
          Width = 133
          Height = 47
          Action = ActionBrowseSource
          Align = alClient
          Caption = 'Browse'
          TabOrder = 1
          ExplicitLeft = 272
          ExplicitTop = 24
          ExplicitWidth = 75
          ExplicitHeight = 25
        end
        object editFileMask: TEdit
          AlignWithMargins = True
          Left = 375
          Top = 3
          Width = 227
          Height = 33
          Align = alTop
          TabOrder = 2
          Text = '*.jpg'
          ExplicitLeft = 11
          ExplicitTop = 11
          ExplicitWidth = 121
        end
      end
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 614
      Top = 3
      Width = 605
      Height = 78
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 120
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 605
        Height = 25
        Align = alTop
        Caption = 'Destination'
        Layout = tlCenter
        ExplicitWidth = 90
      end
      object GridPanel2: TGridPanel
        Left = 0
        Top = 25
        Width = 605
        Height = 53
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 79.841647918171970000
          end
          item
            Value = 20.158352081828020000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = editDestination
            Row = 0
          end
          item
            Column = 1
            Control = BitBtn1
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        ExplicitLeft = 64
        ExplicitTop = 32
        ExplicitWidth = 185
        ExplicitHeight = 41
        object editDestination: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 477
          Height = 33
          Align = alTop
          TabOrder = 0
          ExplicitLeft = 11
          ExplicitTop = 11
        end
        object BitBtn1: TBitBtn
          AlignWithMargins = True
          Left = 486
          Top = 3
          Width = 116
          Height = 47
          Action = ActionBrowseDestination
          Align = alClient
          Caption = 'Browse'
          TabOrder = 1
          ExplicitLeft = 504
          ExplicitTop = 8
          ExplicitWidth = 75
          ExplicitHeight = 25
        end
      end
    end
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 87
      Width = 605
      Height = 79
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 120
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object cbRecursive: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 599
        Height = 30
        Align = alTop
        Caption = 'Recursive'
        Checked = True
        State = cbChecked
        TabOrder = 0
        ExplicitWidth = 605
      end
    end
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 614
      Top = 87
      Width = 605
      Height = 79
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitLeft = 120
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object btnBackup: TBitBtn
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 599
        Height = 73
        Align = alClient
        Caption = 'Copy'
        TabOrder = 0
        OnClick = btnBackupClick
        ExplicitLeft = 11
        ExplicitTop = 6
        ExplicitWidth = 75
        ExplicitHeight = 91
      end
    end
  end
  object ActionList: TActionList
    Left = 232
    Top = 312
    object ActionBrowseDestination: TAction
      Caption = 'Browse'
      OnExecute = ActionBrowseDestinationExecute
    end
    object ActionBrowseSource: TAction
      Caption = 'Browse'
      OnExecute = ActionBrowseSourceExecute
    end
  end
  object FileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoForceFileSystem, fdoPathMustExist]
    Left = 224
    Top = 440
  end
end
