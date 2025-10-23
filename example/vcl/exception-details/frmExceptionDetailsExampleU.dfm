object frmExceptionDetailsExample: TfrmExceptionDetailsExample
  Left = 0
  Top = 0
  Caption = 'Lazy Exception Details - VCL Demo'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Size = 8
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 12
      Width = 349
      Height = 23
      Caption = 'Lazy Exception Details - Demo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Size = 23
      Font.Style = [fsBold]
      ParentFont = False
    end
    object rgOutputFormat: TRadioGroup
      Left = 720
      Top = 8
      Width = 161
      Height = 49
      Caption = 'Output Format'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Text'
        'XML'
        'JSON')
      TabOrder = 0
    end
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 65
    Width = 900
    Height = 535
    ActivePage = tsBasicException
    Align = alClient
    TabOrder = 1
    object tsBasicException: TTabSheet
      Caption = 'Basic Exception'
      object lblBasicInfo: TLabel
        Left = 16
        Top = 16
        Width = 344
        Height = 13
        Caption = 
          'Click the button below to raise a basic exception and see the d' +
          'etails:'
      end
      object pnlBasicButtons: TPanel
        Left = 0
        Top = 467
        Width = 892
        Height = 40
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object btnBasicException: TButton
          Left = 16
          Top = 6
          Width = 150
          Height = 25
          Caption = 'Raise Basic Exception'
          TabOrder = 0
          OnClick = btnBasicExceptionClick
        end
        object btnClearBasic: TButton
          Left = 172
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 1
          OnClick = btnClearBasicClick
        end
      end
      object memoBasicResult: TMemo
        Left = 0
        Top = 48
        Width = 892
        Height = 419
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Size = 11
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
    object tsDivideByZero: TTabSheet
      Caption = 'Divide by Zero'
      ImageIndex = 1
      object lblDivideInfo: TLabel
        Left = 16
        Top = 16
        Width = 377
        Height = 13
        Caption = 
          'Click the button below to trigger a divide by zero exception an' +
          'd see the details:'
      end
      object pnlDivideButtons: TPanel
        Left = 0
        Top = 467
        Width = 892
        Height = 40
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object btnDivideByZero: TButton
          Left = 16
          Top = 6
          Width = 150
          Height = 25
          Caption = 'Divide by Zero'
          TabOrder = 0
          OnClick = btnDivideByZeroClick
        end
        object btnClearDivide: TButton
          Left = 172
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 1
          OnClick = btnClearDivideClick
        end
      end
      object memoDivideResult: TMemo
        Left = 0
        Top = 48
        Width = 892
        Height = 419
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Size = 11
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
    object tsNilReference: TTabSheet
      Caption = 'Nil Reference'
      ImageIndex = 2
      object lblNilInfo: TLabel
        Left = 16
        Top = 16
        Width = 404
        Height = 13
        Caption = 
          'Click the button below to trigger a nil reference exception and' +
          ' see the details:'
      end
      object pnlNilButtons: TPanel
        Left = 0
        Top = 467
        Width = 892
        Height = 40
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object btnNilReference: TButton
          Left = 16
          Top = 6
          Width = 150
          Height = 25
          Caption = 'Nil Reference'
          TabOrder = 0
          OnClick = btnNilReferenceClick
        end
        object btnClearNil: TButton
          Left = 172
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 1
          OnClick = btnClearNilClick
        end
      end
      object memoNilResult: TMemo
        Left = 0
        Top = 48
        Width = 892
        Height = 419
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Size = 11
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
    object tsCustomHandler: TTabSheet
      Caption = 'Custom Handler'
      ImageIndex = 3
      object lblHandlerInfo: TLabel
        Left = 16
        Top = 16
        Width = 573
        Height = 13
        Caption = 
          'Demonstrate custom exception handlers. Register a handler, then' +
          ' raise an exception to see the enhanced details:'
      end
      object lblHandlerStatus: TLabel
        Left = 16
        Top = 48
        Width = 181
        Height = 13
        Caption = 'Handler Status: NOT REGISTERED'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Size = 8
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pnlHandlerButtons: TPanel
        Left = 0
        Top = 467
        Width = 892
        Height = 40
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object btnRegisterHandler: TButton
          Left = 16
          Top = 6
          Width = 120
          Height = 25
          Caption = 'Register Handler'
          TabOrder = 0
          OnClick = btnRegisterHandlerClick
        end
        object btnUnregisterHandler: TButton
          Left = 142
          Top = 6
          Width = 120
          Height = 25
          Caption = 'Unregister Handler'
          TabOrder = 1
          OnClick = btnUnregisterHandlerClick
        end
        object btnTestWithHandler: TButton
          Left = 268
          Top = 6
          Width = 120
          Height = 25
          Caption = 'Test With Handler'
          TabOrder = 2
          OnClick = btnTestWithHandlerClick
        end
        object btnClearHandler: TButton
          Left = 394
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 3
          OnClick = btnClearHandlerClick
        end
      end
      object memoHandlerResult: TMemo
        Left = 0
        Top = 80
        Width = 892
        Height = 387
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Size = 11
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
        WordWrap = False
      end
    end
  end
end

