object frmOptimise: TfrmOptimise
  Left = 0
  Top = 0
  Caption = 'frmOptimise'
  ClientHeight = 602
  ClientWidth = 1008
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    1008
    602)
  TextHeight = 15
  object Bevel1: TBevel
    Left = 8
    Top = 36
    Width = 982
    Height = 4
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
    ExplicitWidth = 1055
  end
  object sbOptimise: TSpeedButton
    Left = 8
    Top = 8
    Width = 113
    Height = 22
    OnClick = sbOptimiseClick
  end
  object sbCopyOutput: TSpeedButton
    Left = 263
    Top = 8
    Width = 130
    Height = 22
    OnClick = sbCopyOutputClick
  end
  object mMemo: TMemo
    Left = 263
    Top = 245
    Width = 737
    Height = 352
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object mData: TMemo
    Left = 263
    Top = 46
    Width = 737
    Height = 193
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Paste your data in here!')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 8
    Top = 46
    Width = 249
    Height = 550
    Anchors = [akLeft, akTop, akBottom]
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    ExplicitHeight = 544
    object gbOutputOptions: TGroupBox
      Left = 8
      Top = 6
      Width = 233
      Height = 113
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      object Label4: TLabel
        Left = 12
        Top = 27
        Width = 4
        Height = 13
        Caption = '.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 12
        Top = 54
        Width = 4
        Height = 13
        Caption = '.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 181
        Top = 82
        Width = 4
        Height = 13
        Caption = '.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 13
        Top = 82
        Width = 4
        Height = 13
        Caption = '.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object cbDataSize: TComboBox
        Left = 78
        Top = 23
        Width = 140
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object cbLanguageFormat: TComboBox
        Left = 78
        Top = 50
        Width = 140
        Height = 21
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object cbPerRow: TComboBox
        Left = 78
        Top = 77
        Width = 97
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
  end
end
