object LZAuthorizeBrowserForm: TLZAuthorizeBrowserForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Web browser'
  ClientHeight = 777
  ClientWidth = 586
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  TextHeight = 15
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 0
    Width = 586
    Height = 777
    Align = alClient
    TabOrder = 0
    SelectedEngine = EdgeIfAvailable
    OnNavigateComplete2 = WebBrowserNavigateComplete2
    ExplicitLeft = 232
    ExplicitTop = 284
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C0000005C3C0000A74E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
