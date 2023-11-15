object frameSimpleExport: TframeSimpleExport
  Left = 0
  Top = 0
  Width = 219
  Height = 480
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 219
    Height = 81
    Align = alTop
    TabOrder = 0
    object Label2: TLabel
      Left = 6
      Top = 6
      Width = 3
      Height = 13
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lHexPrefix: TLabel
      Left = 80
      Top = 28
      Width = 54
      Height = 15
      Caption = 'lHexPrefix'
      PopupMenu = PopupMenu1
    end
    object cbSource: TComboBox
      Left = 6
      Top = 25
      Width = 65
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbSourceChange
    end
    object bCopySourceData: TBitBtn
      Left = 187
      Top = 22
      Width = 25
      Height = 24
      Hint = 'Copy row data to clipboard'
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C21E0000C21E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFD39570CC8357C87646CA7B4ECB7B4ECA7B4ECA7B
        4ECA7B4ECA8155CD865CFF00FF4A80AB206398206398206398206398C98F67FC
        F3ECFAF1E8FAF0E7FBF1E9FBF2EAFBF2EAFBF2EBFDF4EECB83585588B174ADD8
        7BB2DD78AEDC75AADA74A9DADAA97DEFF1E7FFE9D9FFEADBFFE9D9FFE7D7FFE5
        D2FFE2CBEFF2E8CE815620639880B9E15395D15092D04E8ECE4D8CCDD6A97DFB
        F5EEFFE9D9FFEADBFFE9D9FFE7D7FFE5D2FFE2CBFBF6EFCC835620639884BFE2
        569AD35397D15092CF5091CFD6A97DFFF7F1FFE9D9FFEADBFFE9D9FFE7D7FFE5
        D2FFE2CBFFF7F1CB855620639888C4E6599FD6569BD35397D15395D1D7AC7FFF
        F7F0FFE7D5FDE7D6FDE6D4FCE4D0FBE3CBFADCC2FEF3E8CC86572063988BC9E7
        5CA5D759A0D5579CD3569AD3D7AC7FFFF7F2FEE7D5FEE7D5FDE5D1FAE0CAF9DE
        C4F7D9BCFDF2E7CC875820639891CDE95FA9D95DA5D85AA0D6599FD6D8AD81FE
        F7F1FCE5D2FCE4D1FBE2CCF9DDC4F6D7BBF3D1AFFAEFE4CC875920639898D2EB
        65AEDA60AAD95DA6D85CA5D7D9AF84FEF6F0FCE2CDFCE3CDFADFC8F7D9BCF5E9
        DDFAF3EBFBF8F3CA83542063989ED6ED6BB2DC66AFDC60AADA5FA9D9D9AF84FE
        F5EDFCDEC5FBE0C7F9DCC2F5D3B4FEF9F3FAE2C4ECC193D2986E206398A5DAEF
        6FB5DE68B0DC60A9D95FA9D9D7AD81FFFFFEFDF3E9FDF3EAFCF2E8FAEFE3FAF2
        E7EABB88D39469FF00FF206398AADDF174B9E070B6DF6CB3DD6BB2DCD9AF84D7
        AE81D7AC7FD7AC7FCCA070CD9F71B38F67D39B71FF00FFFF00FF206398B0E1F2
        79BDE24498DD4497DC4396DC4296DC4295DC4195DB539ED489C6E6206398FF00
        FFFF00FFFF00FFFF00FF3B76A595C9E0AEE2F24E9DDFB5EEFD75D4F075D4F0B5
        EEFD4B9BDE8ECBE993CDE92A6A9DFF00FFFF00FFFF00FFFF00FFFF00FF6392B7
        2063983775A4B6EFFE80DBF380DBF3B6EFFE2E6EA12063986F9ABCFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF20639820639820639820639820
        63982D6C9EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      TabOrder = 1
      OnClick = bCopySourceDataClick
    end
    object cbSourceDirection: TComboBox
      Left = 6
      Top = 52
      Width = 108
      Height = 23
      Style = csDropDownList
      TabOrder = 2
      OnChange = cbSourceDirectionChange
    end
    object cbSourceLSB: TComboBox
      Left = 120
      Top = 52
      Width = 92
      Height = 23
      Style = csDropDownList
      TabOrder = 3
    end
    object cbCombineNybbles: TCheckBox
      Left = 123
      Top = 26
      Width = 60
      Height = 17
      Caption = 'Combine'
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 81
    Width = 219
    Height = 399
    Align = alClient
    TabOrder = 1
    object mData: TMemo
      Left = 1
      Top = 1
      Width = 217
      Height = 397
      Align = alClient
      TabOrder = 0
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 64
    Top = 224
    object miHexNone: TMenuItem
      Caption = 'None'
      OnClick = miHexNoneClick
    end
    object N0xx1: TMenuItem
      Tag = 1
      Caption = '0x'
      OnClick = miHexNoneClick
    end
    object N1: TMenuItem
      Tag = 2
      Caption = '$'
      OnClick = miHexNoneClick
    end
  end
end
