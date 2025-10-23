object LZAuthorizeBrowserForm: TLZAuthorizeBrowserForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Web browser'
  ClientHeight = 852
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 0
    Width = 684
    Height = 833
    Align = alClient
    TabOrder = 0
    SelectedEngine = EdgeIfAvailable
    OnTitleChange = WebBrowserTitleChange
    OnBeforeNavigate2 = WebBrowserBeforeNavigate2
    OnNavigateComplete2 = WebBrowserNavigateComplete2
    ControlData = {
      4C000000B2460000185600000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object StatusBarBrowser: TStatusBar
    Left = 0
    Top = 833
    Width = 684
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 200
      end
      item
        Width = 50
      end>
    ExplicitTop = 832
    ExplicitWidth = 680
  end
end
