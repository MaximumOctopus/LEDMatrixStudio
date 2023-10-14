object frmExport: TfrmExport
  Left = 0
  Top = 0
  Caption = 'Export Matrix Data'
  ClientHeight = 659
  ClientWidth = 1184
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnConstrainedResize = FormConstrainedResize
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 595
    Width = 1184
    Height = 64
    Align = alBottom
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      1184
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
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C21E0000C21E00000000000000000000FF00FFCD9570
          BD7342B76835B56835B46734B26634B06533AE6433AC6332AA6232A96132A860
          31A76132AB693CBC8661C37D4FEBC6ADEAC5ADFEFBF8FEFBF8FEFBF8FEFBF8FE
          FBF8FEFBF8FEFBF8FEFBF8FEFBF8FEFBF8C89A7CC79879AD6B40BA6C38EDCAB3
          E0A27AFEFAF762C08862C08862C08862C08862C08862C08862C08862C088FDF9
          F6CA8D65C99B7CA76132BB6C38EECCB6E1A27AFEFAF7BFDCC2BFDCC2BFDCC2BF
          DCC2BFDCC2BFDCC2BFDCC2BFDCC2FDF9F6CD9068CC9E81A86132BB6B38EFCEB8
          E1A279FEFAF762C08862C08862C08862C08862C08862C08862C08862C088FDF9
          F6CF936ACEA384AA6132BA6A36EFD0BBE2A27AFEFBF8FEFBF8FEFBF8FEFBF8FE
          FBF8FEFBF8FEFBF8FEFBF8FEFBF8FEFBF8D3966DD2A78AAB6232BB6A36F0D2BE
          E2A37AE2A37AE1A37AE2A37BE1A37BE0A178DE9F77DD9F76DC9D74D99B72D899
          71D69970D5AB8EAD6333BB6A36F2D5C2E3A37AE3A37AE2A37BE2A37BE2A47BE1
          A279E0A178DEA077DE9E75DC9D74DA9B73D99B73DAB095AF6433BB6A36F2D8C5
          E3A47BE3A37AE3A47AE2A47BE2A37BE1A37BE1A279DFA077DE9F76DD9E74DB9C
          72DC9D74DDB59AB16534BB6B36F4D9C7E6A67DC88C64C98D65C98E67CB926CCB
          926DCA9069C88C65C88C64C88C64C88C64DA9C74E1BA9FB36634BB6C37F4DCC9
          E7A77DF9ECE1F9ECE1F9EDE3FCF4EEFDFAF7FDF7F3FAEDE5F7E7DBF7E5D9F6E5
          D8DEA077E4BEA4B46734BD6E3AF5DDCCE7A87EFAF0E8FAF0E8C98D66FAF0E9FD
          F8F3FEFAF8FCF4EFF9E9DFF7E7DBF7E5D9E0A278E7C2A9B66835C07442F6DFD0
          E8A87EFCF6F1FCF6F1C88C64FAF1E9FBF4EEFDFAF7FDF9F6FAF0E8F8E8DDF7E6
          DBE1A37AEFD5C3B76A36C68255F6DFD1E9AA80FEFAF6FDFAF6C88C64FBF3EEFB
          F1EAFCF6F2FEFBF8FCF6F1F9ECE2F8E7DBEED0BAECD0BDBD7443D6A585F6E0D1
          F7E0D1FEFBF8FEFBF7FDF9F6FCF5F0FAF0EAFBF2EDFDF9F6FDFAF7FBF1EBF8E9
          DFECD1BECD926AE2C5B1E1BDA6D9AB8DC9895EC07543BD6E3ABB6C37BB6B36BB
          6A36BB6A36BC6C39BD6E3BBB6D3ABF7444C98D65E7CEBCD8AB8E}
        TabOrder = 0
        OnClick = sbSaveClick
      end
      object sbOpen: TBitBtn
        Left = 8
        Top = 21
        Width = 57
        Height = 25
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C21E0000C21E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA2CAEE
          76B2E63E91DB348CD9348CD9348CD9348CD9348CD9348CD9348CD9348CD9348B
          D9398FDA85B9E9FFFFFFFFFFFF4799DDDEF1FAA8DDF49EDBF496DAF38ED8F386
          D7F37FD4F279D3F272D2F16CD0F169CFF1C2EAF83F95DBFFFFFFFFFFFF3B97DB
          EFFAFEA1E9F991E5F881E1F772DEF663DAF554D7F447D3F339D0F22ECDF126CB
          F0CAF2FB3B97DBFFFFFFFFFFFF3C9DDBF2FAFDB3EDFAA4E9F995E6F885E2F776
          DEF665DBF557D7F449D4F33BD1F230CEF1CCF2FB3B9BDBFFFFFFFFFFFF3BA3DB
          F6FCFEC8F2FCB9EFFBACECFA9CE8F98BE3F77CE0F66CDCF65DD9F54FD6F444D3
          F3D0F3FC3BA2DBFFFFFFFFFFFF3BA8DBFEFFFFF8FDFFF6FDFFF5FCFFF3FCFED8
          F6FC94E6F885E3F776DFF668DBF55CD8F4D7F4FC3BA7DBFFFFFFFFFFFF39ADDB
          E8F6FB94D4EF88CEEE73C1E9C9E9F6F2FCFEF3FCFEF2FCFEF0FCFEEFFBFEEEFB
          FEFEFFFF3CAEDBFFFFFFFFFFFF40AEDCF1FAFD94DEF593DCF481D5F26ACAED6C
          CBEA85D3EF80D2EF7AD0EF76CFEE72CFEEE9F7FB3EB2DCFFFFFFFFFFFF41B4DC
          F7FCFE8EE4F891DEF59FE0F5ACE1F6EFFBFEF4FDFEF3FCFEF1FCFEEFFBFEEEFB
          FEFAFDFF58BCE0FFFFFFFFFFFF3CB5DBFDFEFEFEFFFFFEFEFFFDFEFFFEFFFFEA
          F7FB6EC8E56FC9E46FC9E46FC9E47DCFE784D0E8BAE5F2FFFFFFFFFFFF59C2E0
          61C3E263C4E363C4E363C4E362C4E356C0E0EDF8FCF3FAFDF3FAFDF3FAFDF3FA
          FDF3FBFDFCFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        TabOrder = 1
        OnClick = sbOpenClick
      end
      object cbProfileList: TComboBox
        Left = 67
        Top = 23
        Width = 142
        Height = 21
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
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C21E0000C21E00000000000000000000FF00FFFF00FF
          FF00FFCBCBCB7D7D7D8888888D8D8DC3C3C3FF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFA6A6A6A7A7A7DADADAD8D8D8D3D3D367
          67677272727070706C6C6C686868686868757575FF00FFFF00FFFF00FFAFAFAF
          B5B5B5F3F3F3E9E9E9E3E3E3E4E4E4B6B6B6929292E5E5E5E4E4E4DEDEDED4D4
          D4A0A0A0727272FF00FFD9D9D9B5B5B5F4F4F4E5E5E5DBDBDBDADADADADADAF0
          F0F06E6E6ECFCFCFC9C9C9D0D0D08F8F8FB9B9B9666666FF00FFB8B8B8DBDBDB
          E8E8E8B7B7B7A9A9A9CCCCCCDADADAF5F5F5717171BFBFBFCBCBCBD1D1D18C8C
          8CAFAFAF6B6B6BFF00FFB2B2B2E6E6E6BFBFBFD0D0D0D5D5D5DADADAE5E5E5F5
          F5F5787878C6C6C6CDCDCDD3D3D3919191B1B1B16F6F6FFF00FFBDBDBDE2E2E2
          DCDCDCD8D8D8DDDDDDE5E5E5EFEFEFE9E9E9868686D7D7D7CDCDCDD3D3D39797
          97B4B4B4747474FF00FFE4E4E4C1C1C1EFEFEFE5E5E5EBEBEBF1F1F1EEEEEEB6
          B6B6A9A9A9D8D8D8CBCBCBD0D0D09D9D9DB5B5B5797979FF00FFFF00FFC3C3C3
          D7D7D7EEEEEEF3F3F3E3E3E3B6B6B6A9A9A9CCCCCCCDCDCDCACACAC6C6C6B6B6
          B6B6B6B67E7E7EFF00FFFF00FFFF00FFC9C9C9B1B1B1A9A9A9A1A1A1B9B9B9C7
          C7C7BEBEBEBFBFBFBBBBBBC2C2C2BABABAC6C6C6848484FF00FFFF00FFFF00FF
          FF00FFEBEBEBE9E9E9B0B0B0CBCBCBEFEFEFEFEFEFEDEDEDEFEFEFEFEFEFEFEF
          EFA6A6A6898989FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA6A6A6EFEFEFF0
          F0F0E6E6E6EDEDEDEEEEEEF9F9F9DDDDDDDEDEDE8D8D8DFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FF949494C4C4C4DFDFDFEBEBEBE8E8E8F6F6F6F0F0F0E9E9
          E9ABABAB8D8D8DFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCCCCCC858585BA
          BABAD8D8D8F4F4F4FDFDFDEAEAEABCBCBC7F7F7FC9C9C9FF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFDEDEDEB5B5B5A6A6A6E8E8E8F1F1F1D1D1D1B2B2
          B2DCDCDCFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFE4E4E4E0E0E0FF00FFFF00FFFF00FFFF00FFFF00FF}
        TabOrder = 3
        OnClick = sbDeleteClick
      end
    end
    object GroupBox6: TGroupBox
      Left = 864
      Top = 2
      Width = 314
      Height = 55
      Anchors = [akRight, akBottom]
      Caption = '.'
      TabOrder = 1
      object bCancel: TBitBtn
        Left = 228
        Top = 21
        Width = 75
        Height = 25
        Cancel = True
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C21E0000C21E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1313F20000F10000F100
          00F10000EF0000EF0000ED1212EEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF1313F61A20F53C4CF93A49F83847F83545F83443F73242F7141BF11717
          EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1313F81D23F94453FA2429F91212F70F
          0FF60C0CF50909F5161BF53343F7141BF11717EFFFFFFFFFFFFFFFFFFF1313F9
          1F25FA4A58FB4247FBC9C9FD3B3BF91313F71010F63333F7C5C5FD3035F73444
          F7141BF21717EFFFFFFFFFFFFF0000FB4F5DFD3237FBCBCBFEF2F2FFEBEBFE3B
          3BF93939F8EAEAFEF1F1FEC5C5FD181DF63343F70000EFFFFFFFFFFFFF0000FD
          525FFD2828FC4747FCECECFFF2F2FFECECFFECECFEF1F1FFEAEAFE3434F70B0B
          F53545F80000EFFFFFFFFFFFFF0000FD5562FE2C2CFD2929FC4848FCEDEDFFF2
          F2FFF2F2FFECECFE3A3AF91212F70F0FF63848F80000F1FFFFFFFFFFFF0000FD
          5764FE3030FD2D2DFD4B4BFCEDEDFFF2F2FFF2F2FFECECFF3D3DF91616F81313
          F73C4BF80000F1FFFFFFFFFFFF0000FF5A67FE3333FE5050FDEDEDFFF3F3FFED
          EDFFEDEDFFF2F2FFECECFE3E3EFA1717F83F4EF90000F1FFFFFFFFFFFF0000FF
          5B68FF4347FECFCFFFF3F3FFEDEDFF4C4CFC4A4AFCECECFFF2F2FFCACAFE2A2F
          FA4251FA0000F3FFFFFFFFFFFF1414FF262BFF5D6AFF585BFFCFCFFF5252FE2F
          2FFD2C2CFD4B4BFCCCCCFE484CFB4957FB1D23F91414F6FFFFFFFFFFFFFFFFFF
          1414FF262BFF5D6AFF4347FF3434FE3232FE3030FD2D2DFD383CFC4F5DFC1F25
          FA1414F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1414FF262BFF5C69FF5B68FF5A
          67FE5865FE5663FE5461FE2227FC0D0DFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF1313FF0000FF0000FF0000FF0000FD0000FD0000FD1313FDFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
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
      end
    end
    object cbAutoPreview: TCheckBox
      Left = 471
      Top = 24
      Width = 66
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
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF9A9A9A7C7C7C78787874
        74747070706C6C6C696969858585FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFA8A8A8FBFBFBF7F7F7F6F6F6F5F5F5F7F7F7FBFBFB939393FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8D8D8DB3B2B2C7C5C4C8
        C1C1C4BDBCCDC8C7A6A5A5757575FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFCECECEA8A8A8C8C6C5D9D5D4818181C1C1C1FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFCFCFCFB5B5B5B1B1B1AEAEAEA3A3A3BBBBBBAB
        ABABA7A7A7B3B3B38F8F8F9494949191918D8D8DB9B9B9FF00FFFF00FFAEAEAE
        E4E3E3D7D6D5D6D3D2DBD8D7C4C1C1D2CDCCD1CCCBBDBBBAD5D2D2CCC9C9CCC9
        C9D8D6D6727272FF00FFFF00FFB4B4B4E0DEDDA7724DA7724DA7724DA7724DA7
        724DA7724DA7724DA7724DA7724DA7724DDAD6D4787878FF00FFFF00FFBABABA
        DEDBDBA7724DC8926CCA946DCC966ECD9770CF9971D19B73D39D74D49E75A772
        4DD7D2D17E7E7EFF00FFFF00FFBFBFBFDFDCDBA7724DC7916BC9936CCB956ECD
        976FCE9872D4A27DD6A580D6A47FA7724DD7D3D3848484FF00FFFF00FFC4C4C4
        E1DEDCA7724DC7916AC8926CCA946DD09D78D5A888D8AC8CD7A784D6A37DA772
        4DD9D5D48A8A8AFF00FFFF00FFC9C9C9E1DEDEA7724DC6906AC7916BCE9E7AD7
        AE90D9AF91D6A989D5A582D6A47EA7724DDBD6D6909090FF00FFFF00FFCECECE
        E2DFDFA7724DC58F69CB9975D6AE92DAB59AD9B092D7AC8DD7A988D7A885A772
        4DDBD7D7969696FF00FFFF00FFD2D2D2E2E0DFA7724DA7724DA7724DA7724DA7
        724DA7724DA7724DA7724DA7724DA7724DDCD9D89D9D9DFF00FFFF00FFD5D5D5
        F1EFEFE3E1DFE2E0DFE2DFDFE2DFDEE1DEDEE1DEDDE0DDDCDFDCDCDFDCDBDEDB
        DBEEECECA3A3A3FF00FFFF00FFD8D8D8D6D6D6D4D4D4D1D1D1CECECECACACAC7
        C7C7C3C3C3BFBFBFBBBBBBB7B7B7B2B2B2AEAEAEA9A9A9FF00FF}
      TabOrder = 3
      OnClick = bBuildCodeClick
    end
  end
  object pcExport: TPageControl
    Left = 0
    Top = 0
    Width = 1184
    Height = 595
    ActivePage = tsCode
    Align = alClient
    TabOrder = 1
    OnChange = pcExportChange
    object tsCode: TTabSheet
      Caption = '.'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 409
        Height = 567
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
            OnClick = sbDataRowsClick
          end
          object sbDataColumns: TSpeedButton
            Left = 93
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Caption = '.'
            OnClick = sbDataRowsClick
          end
          object lFrame: TLabel
            Left = 16
            Top = 120
            Width = 4
            Height = 13
            Caption = '.'
          end
          object Label2: TLabel
            Left = 87
            Top = 142
            Width = 10
            Height = 13
            Caption = 'to'
          end
          object lSelectiveOutput: TLabel
            Left = 16
            Top = 172
            Width = 4
            Height = 13
            Caption = '.'
          end
          object Label9: TLabel
            Left = 87
            Top = 194
            Width = 10
            Height = 13
            Caption = 'to'
          end
          object cbDirection: TComboBox
            Left = 16
            Top = 55
            Width = 152
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbDirectionChange
          end
          object cbScanDirection: TComboBox
            Left = 16
            Top = 82
            Width = 152
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnChange = cbDirectionChange
          end
          object eFrameStart: TEdit
            Left = 16
            Top = 139
            Width = 55
            Height = 21
            TabOrder = 2
            Text = '1'
            OnExit = eFrameEndExit
          end
          object eFrameEnd: TEdit
            Left = 113
            Top = 139
            Width = 55
            Height = 21
            TabOrder = 3
            Text = '1'
            OnExit = eFrameEndExit
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
            Height = 21
            ParentCustomHint = False
            TabOrder = 5
            Text = '1'
            OnExit = eFrameEndExit
          end
          object eSelectiveEnd: TEdit
            Left = 113
            Top = 191
            Width = 55
            Height = 21
            TabOrder = 6
            Text = '1'
            OnExit = eFrameEndExit
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
            Left = 16
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            OnClick = sbLSBLeftClick
          end
          object sbLSBRight: TSpeedButton
            Left = 93
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Down = True
            OnClick = sbLSBLeftClick
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
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbLanguageFormatChange
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
            OnClick = sbNumberDecimalClick
          end
          object sbNumberBinary: TSpeedButton
            Left = 67
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            OnClick = sbNumberDecimalClick
          end
          object sbNumberHex: TSpeedButton
            Left = 118
            Top = 24
            Width = 50
            Height = 25
            GroupIndex = 2
            Down = True
            OnClick = sbNumberDecimalClick
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
            OnClick = sbDataRowsClick
          end
          object sbOutputFrame: TSpeedButton
            Tag = 1
            Left = 97
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            Caption = '.'
            OnClick = sbDataRowsClick
          end
          object sbOutputBytes: TSpeedButton
            Tag = 1
            Left = 16
            Top = 55
            Width = 75
            Height = 25
            GroupIndex = 1
            Caption = '.'
            OnClick = sbDataRowsClick
          end
          object cbLineCount: TComboBox
            Left = 93
            Top = 57
            Width = 75
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbDirectionChange
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
            Width = 4
            Height = 13
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
            Width = 4
            Height = 13
            Caption = '.'
          end
          object Label7: TLabel
            Left = 111
            Top = 130
            Width = 11
            Height = 13
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
            Height = 21
            TabOrder = 1
            Text = '100'
            OnExit = eFrameEndExit
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
            OnClick = sbNumberSize8bitClick
          end
          object sbCSRGB565: TSpeedButton
            Left = 16
            Top = 55
            Width = 152
            Height = 25
            GroupIndex = 2
            Caption = 'RGB565'
            OnClick = sbNumberSize8bitClick
          end
        end
      end
      object Panel4: TPanel
        Left = 409
        Top = 0
        Width = 767
        Height = 567
        Align = alClient
        TabOrder = 1
        object reExport: TRichEdit
          Left = 1
          Top = 1
          Width = 765
          Height = 542
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
          Zoom = 100
          OnMouseWheelDown = reExportMouseWheelDown
          OnMouseWheelUp = reExportMouseWheelUp
        end
        object pPreviewStatus: TPanel
          Left = 1
          Top = 543
          Width = 765
          Height = 23
          Align = alBottom
          TabOrder = 1
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
        Height = 567
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
            Width = 4
            Height = 13
            Caption = '.'
          end
          object Label4: TLabel
            Left = 87
            Top = 142
            Width = 10
            Height = 13
            Caption = 'to'
          end
          object lBinarySelectiveOutput: TLabel
            Left = 16
            Top = 173
            Width = 4
            Height = 13
            Caption = '.'
          end
          object Label10: TLabel
            Left = 87
            Top = 195
            Width = 10
            Height = 13
            Caption = 'to'
          end
          object cbBinaryDirection: TComboBox
            Left = 16
            Top = 55
            Width = 152
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbDirectionChange
          end
          object cbBinaryScanDirection: TComboBox
            Left = 16
            Top = 82
            Width = 152
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnChange = cbDirectionChange
          end
          object eBinaryFrameStart: TEdit
            Left = 16
            Top = 139
            Width = 55
            Height = 21
            TabOrder = 2
            Text = '1'
            OnExit = eFrameEndExit
          end
          object eBinaryFrameEnd: TEdit
            Left = 113
            Top = 139
            Width = 55
            Height = 21
            TabOrder = 3
            Text = '1'
            OnExit = eFrameEndExit
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
            Height = 21
            ParentCustomHint = False
            TabOrder = 5
            Text = '1'
            OnExit = eFrameEndExit
          end
          object eBinarySelectiveEnd: TEdit
            Left = 113
            Top = 192
            Width = 55
            Height = 21
            TabOrder = 6
            Text = '1'
            OnExit = eFrameEndExit
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
            OnClick = sbLSBLeftClick
          end
          object sbBinaryLSBRight: TSpeedButton
            Left = 93
            Top = 24
            Width = 75
            Height = 25
            GroupIndex = 1
            OnClick = sbLSBLeftClick
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
            OnClick = sbNumberSize8bitClick
          end
          object sbBinaryNumberSize8bitSwap: TSpeedButton
            Left = 62
            Top = 24
            Width = 106
            Height = 25
            GroupIndex = 2
            Caption = '8 bit (swap nibbles)'
            OnClick = sbNumberSize8bitClick
          end
          object sbBinaryNumberSize16bitSwap: TSpeedButton
            Left = 16
            Top = 86
            Width = 152
            Height = 25
            GroupIndex = 2
            Caption = '16 bit (swap bytes)'
            OnClick = sbNumberSize8bitClick
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
            Width = 4
            Height = 13
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
            Width = 4
            Height = 13
            Caption = '.'
          end
          object Label11: TLabel
            Left = 119
            Top = 138
            Width = 11
            Height = 13
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
            Height = 21
            TabOrder = 1
            Text = '100'
            OnExit = eFrameEndExit
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
          end
          object rbSaveFrame: TRadioButton
            Left = 24
            Top = 50
            Width = 113
            Height = 17
            Caption = '.'
            TabOrder = 1
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
            OnClick = sbNumberSize8bitClick
          end
          object sbBCSRGB565: TSpeedButton
            Left = 16
            Top = 55
            Width = 152
            Height = 25
            GroupIndex = 2
            Caption = 'RGB565'
            OnClick = sbNumberSize8bitClick
          end
        end
      end
      object mBinary: TMemo
        Left = 409
        Top = 0
        Width = 767
        Height = 567
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
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
