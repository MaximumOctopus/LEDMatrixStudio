object frameFont: TframeFont
  Left = 0
  Top = 0
  Width = 99
  Height = 480
  TabOrder = 0
  object sbChangeFont: TSpeedButton
    Left = 1
    Top = 2
    Width = 98
    Height = 22
    Caption = 'Courier New'
  end
  object lFont: TLabel
    Left = 0
    Top = 0
    Width = 99
    Height = 480
    Align = alClient
    Alignment = taCenter
    Caption = 'A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -120
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    OnClick = lFontClick
    ExplicitWidth = 72
    ExplicitHeight = 129
  end
  object fdMain: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 71
    Top = 48
  end
end
