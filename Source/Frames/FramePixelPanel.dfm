object framePixel: TframePixel
  Left = 0
  Top = 0
  Width = 109
  Height = 480
  TabOrder = 0
  object Label1: TLabel
    Left = 8
    Top = 30
    Width = 7
    Height = 15
    Caption = 'X'
  end
  object Label2: TLabel
    Left = 8
    Top = 59
    Width = 7
    Height = 15
    Caption = 'Y'
  end
  object Label3: TLabel
    Left = 8
    Top = 88
    Width = 33
    Height = 15
    Caption = 'Group'
  end
  object Label4: TLabel
    Left = 8
    Top = 120
    Width = 30
    Height = 15
    Caption = 'Order'
  end
  object lOrder: TLabel
    Left = 48
    Top = 120
    Width = 49
    Height = 15
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label5: TLabel
    Left = 8
    Top = 155
    Width = 10
    Height = 15
    Caption = 'id'
  end
  object lId: TLabel
    Left = 48
    Top = 155
    Width = 49
    Height = 15
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
  end
  object Label6: TLabel
    Left = 3
    Top = 3
    Width = 32
    Height = 15
    Caption = 'Pixels'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ePositionX: TEdit
    Left = 48
    Top = 27
    Width = 49
    Height = 23
    Alignment = taRightJustify
    TabOrder = 0
    Text = '0'
    OnExit = ePositionXExit
  end
  object ePositionY: TEdit
    Left = 48
    Top = 56
    Width = 49
    Height = 23
    Alignment = taRightJustify
    TabOrder = 1
    Text = '0'
    OnExit = ePositionYExit
  end
  object eGroup: TEdit
    Left = 48
    Top = 85
    Width = 49
    Height = 23
    Alignment = taRightJustify
    TabOrder = 2
    Text = '0'
    OnExit = eGroupExit
  end
end
