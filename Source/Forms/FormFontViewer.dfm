object frmFontViewer: TfrmFontViewer
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 344
  ClientWidth = 483
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 292
    Align = alLeft
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitHeight = 286
    object bSelectFont: TLabel
      Left = 8
      Top = 15
      Width = 3
      Height = 15
      Caption = '.'
    end
    object cbFonts: TComboBox
      Left = 8
      Top = 32
      Width = 121
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbFontsChange
    end
    object cbRGBMode: TCheckBox
      Left = 14
      Top = 72
      Width = 113
      Height = 17
      Caption = '.'
      TabOrder = 1
      OnClick = cbRGBModeClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 292
    Width = 483
    Height = 52
    Align = alBottom
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 286
    ExplicitWidth = 477
    object lCharacterValue: TLabel
      Left = 8
      Top = 9
      Width = 3
      Height = 15
      Caption = '.'
    end
    object Label3: TLabel
      Left = 14
      Top = 25
      Width = 36
      Height = 15
      Caption = '(ASCII)'
    end
    object lCharacter: TLabel
      Left = 393
      Top = 9
      Width = 15
      Height = 19
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object tbFont: TTrackBar
      Left = 62
      Top = 6
      Width = 325
      Height = 35
      Max = 127
      Min = 32
      Position = 32
      PositionToolTip = ptTop
      TabOrder = 0
      TickStyle = tsNone
      OnChange = tbFontChange
    end
  end
  object pFont: TPanel
    Left = 145
    Top = 0
    Width = 338
    Height = 292
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 332
    ExplicitHeight = 286
  end
end
