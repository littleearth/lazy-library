object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'CSV/JSON Converter'
  ClientHeight = 730
  ClientWidth = 1012
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 13
  object splMain: TSplitter
    Left = 0
    Top = 120
    Width = 1012
    Height = 5
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 105
    ExplicitWidth = 374
  end
  object splVertical: TSplitter
    Left = 495
    Top = 125
    Width = 5
    Height = 580
    ExplicitLeft = 200
    ExplicitTop = 105
    ExplicitHeight = 100
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1012
    Height = 120
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1228
    object Panel1: TGridPanel
      AlignWithMargins = True
      Left = 777
      Top = 3
      Width = 232
      Height = 114
      Align = alRight
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
          Control = btnLoadFile
          Row = 1
        end
        item
          Column = 1
          Control = btnSaveOutput
          Row = 1
        end
        item
          Column = 0
          Control = btnLoadSample
          Row = 2
        end
        item
          Column = 0
          Control = btnConvert
          Row = 0
        end
        item
          Column = 1
          Control = btnClear
          Row = 0
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
      TabOrder = 1
      ExplicitLeft = 993
      object btnLoadFile: TButton
        AlignWithMargins = True
        Left = 3
        Top = 41
        Width = 110
        Height = 32
        Align = alClient
        Caption = 'Load File'
        TabOrder = 2
        OnClick = btnLoadFileClick
      end
      object btnSaveOutput: TButton
        AlignWithMargins = True
        Left = 119
        Top = 41
        Width = 110
        Height = 32
        Align = alClient
        Caption = 'Save Output'
        TabOrder = 3
        OnClick = btnSaveOutputClick
      end
      object btnLoadSample: TButton
        AlignWithMargins = True
        Left = 3
        Top = 79
        Width = 110
        Height = 32
        Align = alClient
        Caption = 'Load Sample'
        TabOrder = 4
        OnClick = btnLoadSampleClick
      end
      object btnConvert: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 110
        Height = 32
        Align = alClient
        Caption = 'Convert'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = btnConvertClick
      end
      object btnClear: TButton
        AlignWithMargins = True
        Left = 119
        Top = 3
        Width = 110
        Height = 32
        Align = alClient
        Caption = 'Clear'
        TabOrder = 1
        OnClick = btnClearClick
      end
    end
    object pnlControls: TScrollBox
      Left = 0
      Top = 0
      Width = 774
      Height = 120
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ParentBackground = True
      ParentColor = False
      TabOrder = 0
      object gbCSVOptions: TGroupBox
        AlignWithMargins = True
        Left = 275
        Top = 3
        Width = 285
        Height = 114
        Align = alLeft
        Caption = 'CSV Options'
        TabOrder = 2
        object rgQuoteMode: TRadioGroup
          AlignWithMargins = True
          Left = 136
          Top = 18
          Width = 144
          Height = 91
          Align = alRight
          Caption = 'Quote Mode'
          ItemIndex = 0
          Items.Strings = (
            'Minimal'
            'All Fields'
            'Non-Numeric'
            'None')
          TabOrder = 1
        end
        object Panel2: TPanel
          Left = 2
          Top = 15
          Width = 131
          Height = 97
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object lblDelimiter: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 125
            Height = 13
            Align = alTop
            Caption = 'Delimiter:'
            ExplicitWidth = 45
          end
          object edtDelimiter: TEdit
            AlignWithMargins = True
            Left = 3
            Top = 22
            Width = 125
            Height = 21
            Align = alTop
            MaxLength = 1
            TabOrder = 0
            Text = ','
          end
          object chkIncludeHeaders: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 49
            Width = 125
            Height = 17
            Align = alTop
            Caption = 'Include Headers'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
        end
      end
      object gbJSONFormatOptions: TGroupBox
        AlignWithMargins = True
        Left = 752
        Top = 3
        Width = 180
        Height = 114
        Align = alLeft
        Caption = 'JSON Format Options'
        TabOrder = 4
        Visible = False
        object cbRemoveWrapperArray: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 170
          Height = 17
          Align = alTop
          Caption = 'Remove wrapper array'
          TabOrder = 0
        end
      end
      object gbJSONOptions: TGroupBox
        AlignWithMargins = True
        Left = 566
        Top = 3
        Width = 180
        Height = 114
        Align = alLeft
        Caption = 'JSON Options'
        TabOrder = 3
        object lblArrayName: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 170
          Height = 13
          Align = alTop
          Caption = 'Array Name:'
          ExplicitWidth = 61
        end
        object edtArrayName: TEdit
          AlignWithMargins = True
          Left = 5
          Top = 37
          Width = 170
          Height = 21
          Align = alTop
          TabOrder = 0
        end
        object rgJSONFormatMode: TRadioGroup
          AlignWithMargins = True
          Left = 5
          Top = 64
          Width = 170
          Height = 40
          Align = alTop
          Caption = 'Format Mode'
          Columns = 2
          ItemIndex = 1
          Items.Strings = (
            'Compact'
            'Formatted')
          TabOrder = 1
        end
      end
      object rgConversionType: TRadioGroup
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 120
        Height = 114
        Align = alLeft
        Caption = 'Conversion Type'
        ItemIndex = 0
        Items.Strings = (
          'CSV to JSON'
          'JSON to CSV'
          'JSON Format')
        TabOrder = 0
        OnClick = rgConversionTypeClick
      end
      object rgFieldNameCase: TRadioGroup
        AlignWithMargins = True
        Left = 129
        Top = 3
        Width = 140
        Height = 114
        Align = alLeft
        Caption = 'Field Name Case'
        ItemIndex = 3
        Items.Strings = (
          'snake_case'
          'kebab-case'
          'camelCase'
          'PascalCase')
        TabOrder = 1
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 705
    Width = 1012
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 704
    ExplicitWidth = 1228
    object pnlStatus: TPanel
      Left = 0
      Top = 0
      Width = 1012
      Height = 25
      Align = alClient
      BevelInner = bvLowered
      TabOrder = 0
      ExplicitWidth = 1228
      object lblStatus: TLabel
        Left = 8
        Top = 6
        Width = 31
        Height = 13
        Caption = 'Ready'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
    end
  end
  object pnlInput: TPanel
    Left = 0
    Top = 125
    Width = 495
    Height = 580
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 579
    object lblInput: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 489
      Height = 13
      Align = alTop
      Caption = 'CSV Input:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 58
    end
    object memoInput: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 22
      Width = 489
      Height = 555
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitHeight = 554
    end
  end
  object pnlOutput: TPanel
    Left = 500
    Top = 125
    Width = 512
    Height = 580
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 728
    ExplicitHeight = 579
    object lblOutput: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 506
      Height = 13
      Align = alTop
      Caption = 'JSON Output:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 73
    end
    object memoOutput: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 22
      Width = 506
      Height = 555
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitWidth = 722
      ExplicitHeight = 554
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 
      'CSV Files (*.csv)|*.csv|JSON Files (*.json)|*.json|Text Files (*' +
      '.txt)|*.txt|All Files (*.*)|*.*'
    Left = 328
    Top = 104
  end
  object dlgSave: TSaveDialog
    Filter = 
      'CSV Files (*.csv)|*.csv|JSON Files (*.json)|*.json|Text Files (*' +
      '.txt)|*.txt|All Files (*.*)|*.*'
    Left = 280
    Top = 108
  end
end
