object frmExport: TfrmExport
  Left = 0
  Top = 0
  Caption = 'frmExport'
  ClientHeight = 670
  ClientWidth = 1085
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnConstrainedResize = FormConstrainedResize
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 606
    Width = 1085
    Height = 64
    Align = alBottom
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 618
    ExplicitWidth = 1097
    DesignSize = (
      1085
      64)
    object gbProfiles: TGroupBox
      Left = 4
      Top = 2
      Width = 333
      Height = 55
      Caption = '.'
      TabOrder = 0
      object Bevel2: TBevel
        Left = 252
        Top = 28
        Width = 4
        Height = 12
        Shape = bsLeftLine
      end
      object sbSave: TBitBtn
        Left = 259
        Top = 21
        Width = 68
        Height = 25
        Glyph.Data = {
          E6040000424DE604000000000000360000002800000014000000140000000100
          180000000000B0040000232E0000232E00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFF
          FFFFFFFFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFF
          FF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFF00FFFF
          00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FF
          FFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFFFFFFFF
          FFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFF
          FFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFF
          FFFFFFFFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFF
          FF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF
          00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFF
          FFFFFFFFFFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FF}
        TabOrder = 0
        OnClick = sbSaveClick
      end
      object sbOpen: TBitBtn
        Left = 8
        Top = 21
        Width = 57
        Height = 25
        Glyph.Data = {
          E6040000424DE604000000000000360000002800000014000000140000000100
          180000000000B0040000232E0000232E00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF
          FF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00
          FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF
          00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFF
          FFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF
          FFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFF
          FFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FF}
        TabOrder = 1
        OnClick = sbOpenClick
      end
      object cbProfileList: TComboBox
        Left = 67
        Top = 23
        Width = 142
        Height = 23
        Style = csDropDownList
        TabOrder = 2
      end
      object sbDelete: TBitBtn
        Left = 215
        Top = 21
        Width = 31
        Height = 25
        Hint = 'delete'
        Glyph.Data = {
          E6040000424DE604000000000000360000002800000014000000140000000100
          180000000000B0040000232E0000232E00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00
          FFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFF
          FFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF
          00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFFFFFF
          FFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF
          FFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFF
          FFFFFFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFF
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF
          00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FF}
        TabOrder = 3
        OnClick = sbDeleteClick
      end
    end
    object GroupBox6: TGroupBox
      Left = 755
      Top = 2
      Width = 314
      Height = 55
      Anchors = [akRight, akBottom]
      Caption = '.'
      TabOrder = 1
      ExplicitLeft = 767
      object bCancel: TBitBtn
        Left = 228
        Top = 21
        Width = 75
        Height = 25
        Cancel = True
        Glyph.Data = {
          76050000424D7605000000000000360000002800000015000000150000000100
          18000000000040050000232E0000232E00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00
          FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFFFF
          FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF
          FFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF
          FFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFFFF
          FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00
          FFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00}
        ModalResult = 2
        TabOrder = 0
      end
      object bExport: TBitBtn
        Left = 10
        Top = 21
        Width = 87
        Height = 25
        Glyph.Data = {
          36060000424D3606000000000000360000002800000020000000100000000100
          18000000000000060000C21E0000C21E00000000000000000000FFFFFFFFFFFF
          E2C0AACC8D66C07140BC6B36BC6B36BC6B36BC6A36BC6A36BB6A35BB6A35BB69
          35BD6E3BCA8B63E3C2AEFFFFFFFFFFFFC0C0C08D8D8D7171716B6B6B6B6B6B6B
          6B6B6A6A6A6A6A6A6969696969696969696E6E6E8B8B8BC2C2C2FFFFFFFFFFFF
          C57C4DF8F2EBF7ECDFF6EBDEF6EADEF6EADCF6EADCFAF3EBFAF3EBFAF2EAFCF7
          F3FCF8F4FEFEFDC37A4DFFFFFFFFFFFF7C7C7CF1F1F1EAEAEAE9E9E9E9E9E9E8
          E8E8E8E8E8F2F2F2F2F2F2F1F1F1F7F7F7F8F8F8FEFEFE7A7A7AFFFFFFFFFFFF
          C27740F5EBDFFCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4
          D1FCE4D1FDFBF8BC6B37FFFFFFFFFFFF757575EAEAEAE2E2E2E2E2E2E2E2E2E2
          E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2FBFBFB6B6B6BFFFFFFFFFFFF
          C37C42F7EDE3FCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4D1FCE4
          D1FCE4D1FBF7F4BD6C37FFFFFFFFFFFF797979ECECECE2E2E2E2E2E2E2E2E2E2
          E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2F7F7F76C6C6CFFFFFFFFFFFF
          C77E44E09260E08C4CFCE4D1E2964FE28941FCE4D1FCE4D1FCE4D1FCE4D1FCE4
          D1FCE4D1FCF9F5C1743CFFFFFFFFFFFF7B7B7B909090888888E2E2E291919186
          8686E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2E2F9F9F9727272FFFFFFFCF4EF
          D78140E5A365E19158FDE5D3E59D5CE7A868E3975FF9DAC4FCE2CEFCE2CCFBE0
          C9FBE1C8FDFAF7C37A41FFFFFFF4F4F47E7E7E9E9E9E8E8E8EE3E3E3989898A2
          A2A2949494D8D8D8E0E0E0DFDFDFDDDDDDDEDEDEFAFAFA777777F2CFB4E6A25A
          E8AA6AE39B5DF9D8C3FDE7D6F9DBC3E5A05DE8AA6AE39B56EEB694FCE2CDFBE1
          CBFBE1C9FBF7F2C78045CECECE9C9C9CA4A4A4969696D6D6D6E5E5E5D8D8D89A
          9A9AA4A4A4969696B3B3B3E0E0E0DEDEDEDEDEDEF7F7F77D7D7DE8AC60ECB879
          E5A458F2D8C4FEE8D6FEE8D7FDE7D6F6D1B3E6A65AE9B275E49D58FAE0C8FADF
          C7FADFC6FAF2EAC88448A4A4A4B0B0B09D9D9DD6D6D6E6E6E6E6E6E6E5E5E5CD
          CDCD9F9F9FABABAB989898DDDDDDDCDCDCDCDCDCF1F1F1808080F4D5B7EAAE60
          EAB571E8A663FADBC5FEE8D8FBDDC5E9AB61EAB571E8A75CEFBA93FAE0C7F9DD
          C3F8DCC2FAF4EDC8864BD3D3D3A6A6A6ADADADA0A0A0D9D9D9E6E6E6DADADAA3
          A3A3ADADADA0A0A0B6B6B6DDDDDDDADADAD9D9D9F3F3F3828282FFFFFFFDF6F0
          DE9849EBB672E8A761FDE7D6ECB265ECBB76EAAC67F9DAC1FADFC7F8DCC2F6DA
          BDF6D8BBFAF4EFC8874CFFFFFFF6F6F6919191AEAEAEA0A0A0E5E5E5A9A9A9B2
          B2B2A5A5A5D7D7D7DCDCDCD9D9D9D6D6D6D4D4D4F4F4F4838383FFFFFFFFFFFF
          CA8D4FE9B16EE8AE5FFCE6D4ECB665ECB269F9DEC4FAE0C8F8DCC2F5D6BBF3D4
          B5F1D2B3F8F4F0C6864CFFFFFFFFFFFF888888A9A9A9A6A6A6E4E4E4ADADADAA
          AAAADBDBDBDDDDDDD9D9D9D2D2D2D0D0D0CECECEF4F4F4828282FFFFFFFFFFFF
          C88D51F8EFE6FCE3CFFBE4D0FCE4CFFCE3CDFAE1CAF9DDC4F6D9BCF4E9DFF7F2
          ECFBF7F3F5EFE9C38048FFFFFFFFFFFF888888EEEEEEE1E1E1E1E1E1E1E1E1E0
          E0E0DEDEDEDADADAD5D5D5E8E8E8F1F1F1F7F7F7EEEEEE7D7D7DFFFFFFFFFFFF
          C88D52F9F5F1FCE3CDFBE3CEFBE3CDFBE2CBF9E0C8F8DCC2F5D6BAFDFBF8FCE6
          CDFAE5C9E2B684D5A884FFFFFFFFFFFF888888F5F5F5E0E0E0E0E0E0E0E0E0DF
          DFDFDDDDDDD9D9D9D2D2D2FBFBFBE2E2E2E1E1E1B0B0B0A6A6A6FFFFFFFFFFFF
          CA925AFAF6F2FAE0C7FBE1C9FBE2C9FBE0C8F9DFC5F8DBC1F4D6B8FFFBF8F6D8
          B4E1B07DDC9669FDFBFAFFFFFFFFFFFF8D8D8DF6F6F6DDDDDDDEDEDEDFDFDFDD
          DDDDDCDCDCD8D8D8D2D2D2FBFBFBD3D3D3AAAAAA949494FBFBFBFFFFFFFFFFFF
          D2A274F8F3EDF8F4EEF8F4EDF8F3EDF8F3EDF8F3EDF8F2ECF7F2ECF2E6D7E2B2
          7DDC986BFDFBFAFFFFFFFFFFFFFFFFFF9E9E9EF2F2F2F3F3F3F3F3F3F2F2F2F2
          F2F2F2F2F2F1F1F1F1F1F1E4E4E4ACACAC959595FBFBFBFFFFFFFFFFFFFFFFFF
          E8CEB9D7AA7CCC945BCA9055CA9055CA9055CA9155CB9055C98F55CF9D69DDB1
          90FDFBFAFFFFFFFFFFFFFFFFFFFFFFFFCDCDCDA6A6A68E8E8E8A8A8A8A8A8A8A
          8A8A8B8B8B8B8B8B8A8A8A999999AFAFAFFBFBFBFFFFFFFFFFFF}
        NumGlyphs = 2
        TabOrder = 1
        OnClick = bExportClick
      end
      object bCopyToClipboard: TBitBtn
        Left = 99
        Top = 21
        Width = 31
        Height = 25
        Hint = 'copy to clipboard'
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
        TabOrder = 2
        OnClick = bCopyToClipboardClick
      end
      object bClose: TBitBtn
        Left = 148
        Top = 21
        Width = 75
        Height = 25
        ModalResult = 1
        TabOrder = 3
        OnClick = bCloseClick
      end
    end
    object cbAutoPreview: TCheckBox
      Left = 471
      Top = 24
      Width = 162
      Height = 17
      Caption = '.'
      TabOrder = 2
    end
    object bBuildCode: TBitBtn
      Left = 362
      Top = 21
      Width = 103
      Height = 25
      Glyph.Data = {
        E6040000424DE604000000000000360000002800000014000000140000000100
        180000000000B0040000232E0000232E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        FFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        00FFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00
        FFFFFFFFFFFFFF00000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000FFFFFFFFFFFFFF00FFFF00FFFFFFFF
        FFFFFF0000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000FFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFF00
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000FFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFF0000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000FFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFF000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00FFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFF00000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000FFFFFF
        FFFFFFFF00FFFF00FFFFFFFFFFFFFF0000000000000000000000000000000000
        00000000000000000000000000000000000000000000000000FFFFFFFFFFFFFF
        00FFFF00FFFFFFFFFFFFFF000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000FFFFFFFFFFFFFF00FFFF00
        FFFFFFFFFFFFFF00000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000FFFFFFFFFFFFFF00FFFF00FFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF}
      TabOrder = 3
      OnClick = bBuildCodeClick
    end
  end
  object pcExport: TPageControl
    Left = 0
    Top = 0
    Width = 1085
    Height = 606
    ActivePage = tsCode
    Align = alClient
    TabOrder = 1
    OnChange = cbOptimiseClick
    ExplicitWidth = 1097
    ExplicitHeight = 618
    object tsCode: TTabSheet
      Caption = '.'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 409
        Height = 588
        Align = alLeft
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object gbSource: TGroupBox
          Left = 15
          Top = 8
          Width = 185
          Height = 257
          Caption = '.'
          TabOrder = 0
          object sbDataRows: TSpeedButton
            Left = 16
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Down = True
            Caption = '.'
            OnClick = cbDirectionChange
          end
          object sbDataColumns: TSpeedButton
            Left = 93
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Caption = '.'
            OnClick = cbDirectionChange
          end
          object lFrame: TLabel
            Left = 16
            Top = 120
            Width = 3
            Height = 15
            Caption = '.'
          end
          object Label2: TLabel
            Left = 87
            Top = 142
            Width = 11
            Height = 15
            Caption = 'to'
          end
          object lSelectiveOutput: TLabel
            Left = 16
            Top = 172
            Width = 3
            Height = 15
            Caption = '.'
          end
          object Label9: TLabel
            Left = 87
            Top = 194
            Width = 11
            Height = 15
            Caption = 'to'
          end
          object cbDirection: TComboBox
            Left = 16
            Top = 55
            Width = 152
            Height = 23
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbDirectionChange
          end
          object cbScanDirection: TComboBox
            Left = 16
            Top = 84
            Width = 152
            Height = 23
            Style = csDropDownList
            TabOrder = 1
            OnChange = cbDirectionChange
          end
          object eFrameStart: TEdit
            Left = 16
            Top = 141
            Width = 55
            Height = 23
            TabOrder = 2
            Text = '1'
            OnExit = cbOptimiseClick
          end
          object eFrameEnd: TEdit
            Left = 113
            Top = 139
            Width = 55
            Height = 23
            TabOrder = 3
            Text = '1'
            OnExit = cbOptimiseClick
          end
          object cbOptimise: TCheckBox
            Left = 16
            Top = 229
            Width = 152
            Height = 17
            Caption = '.'
            TabOrder = 4
            OnClick = cbOptimiseClick
          end
          object eSelectiveStart: TEdit
            Left = 16
            Top = 191
            Width = 55
            Height = 23
            ParentCustomHint = False
            TabOrder = 5
            Text = '1'
            OnExit = cbOptimiseClick
          end
          object eSelectiveEnd: TEdit
            Left = 113
            Top = 191
            Width = 55
            Height = 23
            TabOrder = 6
            Text = '1'
            OnExit = cbOptimiseClick
          end
        end
        object gbLSB: TGroupBox
          Left = 210
          Top = 8
          Width = 185
          Height = 58
          Caption = '.'
          TabOrder = 1
          object sbLSBLeft: TSpeedButton
            Left = 12
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            OnClick = cbOptimiseClick
          end
          object sbLSBRight: TSpeedButton
            Left = 93
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Down = True
            OnClick = cbOptimiseClick
          end
        end
        object gbExportFormat: TGroupBox
          Left = 210
          Top = 72
          Width = 185
          Height = 81
          Caption = '.'
          TabOrder = 2
          object cbLanguageFormat: TComboBox
            Left = 16
            Top = 20
            Width = 152
            Height = 23
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbOptimiseClick
          end
          object cbIncludeExample: TCheckBox
            Left = 16
            Top = 54
            Width = 166
            Height = 17
            Caption = '.'
            TabOrder = 1
            OnClick = cbOptimiseClick
          end
        end
        object gbNumberFormat: TGroupBox
          Left = 210
          Top = 160
          Width = 185
          Height = 59
          Caption = '.'
          TabOrder = 3
          object sbNumberDecimal: TSpeedButton
            Left = 16
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            OnClick = cbOptimiseClick
          end
          object sbNumberBinary: TSpeedButton
            Left = 67
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            OnClick = cbOptimiseClick
          end
          object sbNumberHex: TSpeedButton
            Left = 118
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            Down = True
            OnClick = cbOptimiseClick
          end
        end
        object gbNumberGrouping: TGroupBox
          Left = 210
          Top = 330
          Width = 185
          Height = 122
          Caption = '.'
          TabOrder = 4
          object sbNumberSize8bit: TSpeedButton
            Left = 16
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            Down = True
            Caption = '8 bit'
            OnClick = sbNumberSize8bitClick
          end
          object sbNumberSize16bit: TSpeedButton
            Left = 67
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            Caption = '16 bit'
            OnClick = sbNumberSize8bitClick
          end
          object sbNumberSize32bit: TSpeedButton
            Left = 118
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            Caption = '32 bit'
            OnClick = sbNumberSize8bitClick
          end
          object sbNumberSize8bitSwap: TSpeedButton
            Left = 16
            Top = 55
            Width = 152
            Height = 25
            GroupIndex = 2
            OnClick = sbNumberSize8bitClick
          end
          object sbNumberSize16bitSwap: TSpeedButton
            Left = 16
            Top = 86
            Width = 152
            Height = 25
            GroupIndex = 2
            OnClick = sbNumberSize8bitClick
          end
        end
        object gbEachLine: TGroupBox
          Left = 15
          Top = 276
          Width = 185
          Height = 90
          Caption = '.'
          TabOrder = 5
          object sbOutputRow: TSpeedButton
            Tag = 1
            Left = 16
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Down = True
            Caption = '.'
            OnClick = cbDirectionChange
          end
          object sbOutputFrame: TSpeedButton
            Tag = 1
            Left = 97
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Caption = '.'
            OnClick = cbDirectionChange
          end
          object sbOutputBytes: TSpeedButton
            Tag = 1
            Left = 16
            Top = 55
            Width = 75
            Height = 25
            GroupIndex = 1
            Caption = '.'
            OnClick = cbDirectionChange
          end
          object cbLineCount: TComboBox
            Left = 93
            Top = 57
            Width = 75
            Height = 23
            Style = csDropDownList
            TabOrder = 0
            OnClick = cbDirectionChange
          end
        end
        object gbRGB: TGroupBox
          Left = 15
          Top = 381
          Width = 185
          Height = 160
          Caption = '.'
          TabOrder = 6
          Visible = False
          object sbRGB: TSpeedButton
            Left = 16
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Down = True
            Caption = 'RGB'
            OnClick = sbRGBClick
          end
          object sbBGR: TSpeedButton
            Left = 55
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Caption = 'BGR'
            OnClick = sbRGBClick
          end
          object sbGRB: TSpeedButton
            Left = 95
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Caption = 'GRB'
            OnClick = sbRGBClick
          end
          object shapeBackgroundPixels: TShape
            Left = 73
            Top = 90
            Width = 88
            Height = 24
            Brush.Color = clBlack
            OnMouseDown = shapeBackgroundPixelsMouseDown
          end
          object Label1: TLabel
            Left = 40
            Top = 96
            Width = 3
            Height = 15
            Caption = '.'
          end
          object sbBRG: TSpeedButton
            Left = 135
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Caption = 'BRG'
            OnClick = sbRGBClick
          end
          object Label6: TLabel
            Left = 16
            Top = 130
            Width = 3
            Height = 15
            Caption = '.'
          end
          object Label7: TLabel
            Left = 111
            Top = 130
            Width = 10
            Height = 15
            Caption = '%'
          end
          object cbChangeBackgroundPixels: TCheckBox
            Left = 16
            Top = 64
            Width = 152
            Height = 17
            Caption = '.'
            TabOrder = 0
            OnClick = cbOptimiseClick
          end
          object groupBoxRGBBrightness: TEdit
            Left = 73
            Top = 128
            Width = 32
            Height = 23
            TabOrder = 1
            Text = '100'
          end
        end
        object gbNumberGroupingRGB: TGroupBox
          Left = 210
          Top = 230
          Width = 185
          Height = 90
          Caption = '.'
          TabOrder = 7
          Visible = False
          object sbNumberSizeRGB8bits: TSpeedButton
            Left = 16
            Top = 24
            Width = 152
            Height = 25
            GroupIndex = 2
            Caption = '.'
            OnClick = sbNumberSize8bitClick
          end
          object sbNumberSizeRGB32bits: TSpeedButton
            Left = 16
            Top = 55
            Width = 152
            Height = 25
            GroupIndex = 2
            Down = True
            Caption = '.'
            OnClick = sbNumberSize8bitClick
          end
        end
        object gbRGBColourSpace: TGroupBox
          Left = 210
          Top = 458
          Width = 185
          Height = 90
          Caption = 'Colour Space'
          TabOrder = 8
          Visible = False
          object sbCSRGB32: TSpeedButton
            Left = 16
            Top = 24
            Width = 152
            Height = 25
            GroupIndex = 2
            Down = True
            Caption = 'RGB32'
            OnClick = cbOptimiseClick
          end
          object sbCSRGB565: TSpeedButton
            Left = 16
            Top = 55
            Width = 152
            Height = 25
            GroupIndex = 2
            Caption = 'RGB565'
            OnClick = cbOptimiseClick
          end
        end
      end
      object Panel4: TPanel
        Left = 409
        Top = 0
        Width = 668
        Height = 576
        Align = alClient
        TabOrder = 1
        ExplicitWidth = 680
        ExplicitHeight = 588
        object reExport: TRichEdit
          Left = 1
          Top = 1
          Width = 678
          Height = 563
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            ''
            ''
            ''
            
              '        Click the Build Code buttom below to generate the data..' +
              '.'
            ''
            
              '        The more pixels you have the longer it will take to buil' +
              'd.'
            
              '        Large displays with a lot of animation may take many sec' +
              'onds.'
            ''
            '        Please be patient!'
            ''
            
              '        This preview will show the first $X lines of output, thi' +
              's setting'
            '        can be altered in Preferences.'
            ''
            
              '        Auto-build will be enabled on this page if the number of' +
              ' pixels'
            
              '        is fewer than the setting entered on the Prefereces page' +
              '.'
            '        (File menu -> Preferences)')
          ParentFont = False
          PlainText = True
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object pPreviewStatus: TPanel
          Left = 1
          Top = 552
          Width = 666
          Height = 23
          Align = alBottom
          TabOrder = 1
          ExplicitTop = 564
          ExplicitWidth = 678
        end
      end
    end
    object tsBinary: TTabSheet
      Caption = '.'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 409
        Height = 582
        Align = alLeft
        BevelInner = bvRaised
        BevelOuter = bvLowered
        TabOrder = 0
        object gbSourceBinary: TGroupBox
          Left = 15
          Top = 8
          Width = 185
          Height = 257
          Caption = 'Source'
          TabOrder = 0
          object sbBinaryDataRows: TSpeedButton
            Left = 16
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Down = True
            OnClick = sbBinaryDataRowsClick
          end
          object sbBinaryDataColumns: TSpeedButton
            Left = 93
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            OnClick = sbBinaryDataRowsClick
          end
          object Label3: TLabel
            Left = 16
            Top = 120
            Width = 3
            Height = 15
            Caption = '.'
          end
          object Label4: TLabel
            Left = 87
            Top = 142
            Width = 11
            Height = 15
            Caption = 'to'
          end
          object lBinarySelectiveOutput: TLabel
            Left = 16
            Top = 173
            Width = 3
            Height = 15
            Caption = '.'
          end
          object Label10: TLabel
            Left = 87
            Top = 195
            Width = 11
            Height = 15
            Caption = 'to'
          end
          object cbBinaryDirection: TComboBox
            Left = 16
            Top = 55
            Width = 152
            Height = 23
            Style = csDropDownList
            TabOrder = 0
          end
          object cbBinaryScanDirection: TComboBox
            Left = 16
            Top = 82
            Width = 152
            Height = 23
            Style = csDropDownList
            TabOrder = 1
          end
          object eBinaryFrameStart: TEdit
            Left = 16
            Top = 139
            Width = 55
            Height = 23
            TabOrder = 2
            Text = '1'
            OnClick = cbOptimiseClick
            OnExit = cbOptimiseClick
          end
          object eBinaryFrameEnd: TEdit
            Left = 113
            Top = 139
            Width = 55
            Height = 23
            TabOrder = 3
            Text = '1'
            OnExit = cbOptimiseClick
          end
          object cbBinaryOptimise: TCheckBox
            Left = 16
            Top = 229
            Width = 152
            Height = 17
            Caption = '.'
            TabOrder = 4
            Visible = False
            OnClick = cbOptimiseClick
          end
          object eBinarySelectiveStart: TEdit
            Left = 16
            Top = 192
            Width = 55
            Height = 23
            ParentCustomHint = False
            TabOrder = 5
            Text = '1'
            OnExit = cbOptimiseClick
          end
          object eBinarySelectiveEnd: TEdit
            Left = 113
            Top = 192
            Width = 55
            Height = 23
            TabOrder = 6
            Text = '1'
            OnExit = cbOptimiseClick
          end
        end
        object gbLSBBinary: TGroupBox
          Left = 15
          Top = 279
          Width = 185
          Height = 58
          Caption = '.'
          TabOrder = 1
          object sbBinaryLSBLeft: TSpeedButton
            Left = 16
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Down = True
            OnClick = cbOptimiseClick
          end
          object sbBinaryLSBRight: TSpeedButton
            Left = 93
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            OnClick = cbOptimiseClick
          end
        end
        object gbNumberGroupingBinary: TGroupBox
          Left = 210
          Top = 8
          Width = 185
          Height = 63
          Caption = '.'
          TabOrder = 2
          object sbBinaryNumberSize8bit: TSpeedButton
            Left = 16
            Top = 24
            Width = 44
            Height = 25
            GroupIndex = 2
            Down = True
            Caption = '8 bit'
            OnClick = cbOptimiseClick
          end
          object sbBinaryNumberSize8bitSwap: TSpeedButton
            Left = 62
            Top = 24
            Width = 106
            Height = 25
            GroupIndex = 2
            Caption = '8 bit (swap nibbles)'
            OnClick = cbOptimiseClick
          end
          object sbBinaryNumberSize16bitSwap: TSpeedButton
            Left = 16
            Top = 86
            Width = 152
            Height = 25
            GroupIndex = 2
            Caption = '16 bit (swap bytes)'
          end
        end
        object gbBinaryRGB: TGroupBox
          Left = 15
          Top = 356
          Width = 185
          Height = 165
          Caption = '.'
          TabOrder = 3
          Visible = False
          object sbBinaryRGB: TSpeedButton
            Left = 16
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Down = True
            Caption = 'RGB'
            OnClick = sbRGBClick
          end
          object sbBinaryBGR: TSpeedButton
            Left = 55
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Caption = 'BGR'
            OnClick = sbRGBClick
          end
          object sbBinaryGRB: TSpeedButton
            Left = 95
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Caption = 'GRB'
            OnClick = sbRGBClick
          end
          object shapeBinaryBackgroundPixels: TShape
            Left = 73
            Top = 90
            Width = 88
            Height = 24
            Brush.Color = clBlack
            OnMouseDown = shapeBackgroundPixelsMouseDown
          end
          object Label5: TLabel
            Left = 40
            Top = 96
            Width = 3
            Height = 15
            Caption = '.'
          end
          object sbBinaryBRG: TSpeedButton
            Left = 135
            Top = 24
            Width = 40
            Height = 25
            GroupIndex = 2
            Caption = 'BRG'
            OnClick = sbRGBClick
          end
          object Label8: TLabel
            Left = 25
            Top = 138
            Width = 3
            Height = 15
            Caption = '.'
          end
          object Label11: TLabel
            Left = 119
            Top = 138
            Width = 10
            Height = 15
            Caption = '%'
          end
          object cbBinaryChangeBackgroundPixels: TCheckBox
            Left = 16
            Top = 64
            Width = 152
            Height = 17
            Caption = '.'
            TabOrder = 0
            OnClick = cbOptimiseClick
          end
          object groupBoxBinaryRGBBrightness: TEdit
            Left = 81
            Top = 136
            Width = 32
            Height = 23
            TabOrder = 1
            Text = '100'
            OnExit = cbOptimiseClick
          end
        end
        object gbNumberGroupingBinaryRGB: TGroupBox
          Left = 210
          Top = 172
          Width = 185
          Height = 67
          Caption = 'Number Grouping'
          TabOrder = 4
          Visible = False
          object sbBinaryNumberSizeRGB8bits: TSpeedButton
            Left = 16
            Top = 24
            Width = 152
            Height = 25
            GroupIndex = 2
            Down = True
            OnClick = sbNumberSize8bitClick
          end
        end
        object gbFileContents: TGroupBox
          Left = 210
          Top = 79
          Width = 185
          Height = 80
          Caption = '.'
          TabOrder = 5
          object rbSaveAnimation: TRadioButton
            Left = 24
            Top = 26
            Width = 113
            Height = 17
            Caption = '.'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = cbOptimiseClick
          end
          object rbSaveFrame: TRadioButton
            Left = 24
            Top = 50
            Width = 113
            Height = 17
            Caption = '.'
            TabOrder = 1
            OnClick = cbOptimiseClick
          end
        end
        object gbBinaryColourSpaceRGB: TGroupBox
          Left = 210
          Top = 252
          Width = 185
          Height = 90
          Caption = 'Colour Space'
          TabOrder = 6
          Visible = False
          object sbBCSRGB32: TSpeedButton
            Left = 16
            Top = 24
            Width = 152
            Height = 25
            GroupIndex = 2
            Down = True
            Caption = 'RGB32'
            OnClick = cbOptimiseClick
          end
          object sbBCSRGB565: TSpeedButton
            Left = 16
            Top = 55
            Width = 152
            Height = 25
            GroupIndex = 2
            Caption = 'RGB565'
            OnClick = cbOptimiseClick
          end
        end
      end
      object mBinary: TMemo
        Left = 409
        Top = 0
        Width = 668
        Height = 576
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
        ExplicitWidth = 674
        ExplicitHeight = 582
      end
    end
  end
  object sdExport: TSaveDialog
    DefaultExt = '.h'
    Filter = 'C-style header file (.h)|*.h|Include file (.inc)|*.inc'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 392
    Top = 16
  end
  object cdExport: TColorDialog
    Left = 432
    Top = 16
  end
end
