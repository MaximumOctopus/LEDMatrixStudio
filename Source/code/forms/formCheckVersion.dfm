object frmCheckVersion: TfrmCheckVersion
  Left = 615
  Top = 395
  BorderStyle = bsDialog
  Caption = 'LED Matrix Builder'
  ClientHeight = 271
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 0
    Top = 0
    Width = 496
    Height = 38
    Align = alTop
    Pen.Color = clWhite
  end
  object Label5: TLabel
    Left = 8
    Top = 7
    Width = 7
    Height = 25
    Caption = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object bClose: TBitBtn
    Left = 424
    Top = 240
    Width = 65
    Height = 25
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
  object bHistory: TBitBtn
    Left = 95
    Top = 238
    Width = 83
    Height = 25
    Enabled = False
    TabOrder = 1
    OnClick = bHistoryClick
  end
  object mHistory: TMemo
    Left = 8
    Top = 272
    Width = 481
    Height = 217
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object bWebsite: TBitBtn
    Left = 8
    Top = 238
    Width = 83
    Height = 25
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      18000000000000030000C21E0000C21E00000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFF7AB980579E5E619C6646784A59755B7C7C7C87878789
      89898B8B8B8C8C8C6969695C7D5F48864E6099643A7B3E598A5B6AB97366B470
      72BF7D62B46D5E9162E5E5E5828282A9A9A9ACACAC898989E2E2E276BC7E84CA
      8F74C18055A45E337638E2F3E5C3E3C77CBA82528E58A2C1A4F0F0F07E7E7EA4
      A4A4A6A6A6858585F0F0F0A1D6A859AF626AAE72A0C8A4C0D7C2FFFFFFFFFFFF
      FFFFFF8B8B8BF0F0F0EFEFEF7B7B7B9E9E9EA1A1A1818181EFEFEFF4F4F47171
      71E5E5E5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF909090F1F1F1EFEFEF77777799
      99999C9C9C7C7C7CEFEFEFF4F4F4767676E6E6E6FFFFFFFFFFFFFFFFFFC7DBEA
      679CC38D8F91F6F6F6EFEFEF747474767676777777787878EFEFEFF4F4F48A8B
      8D4E618EC1C9D9FFFFFFC7DCEC3B85BB5796C23F80B3DCDEE0EDEDEDEFEFEFEF
      EFEFEFEFEFEFEFEFEFEFEFDFE1E32D4B813A5F9027407AC1C9D9629BCA5395C6
      7AAFD35797C44387BACDCFD0EEEEEEEFEFEFEFEFEFEFEFEFD9DADB3864974470
      9F5C8CB13C6494566B97FAFCFD3F85BE5293C679AED35597C44287BACACBCCED
      EDEDEFEFEFD9DBDC3D76A64D80AE6B9ABD4775A2395D91FAFBFCFFFFFFFAFCFD
      3E81BE5091C676ADD35495C64189BCD2D4D5D2D4D54183B4558DBB77A5C75084
      B23E70A3FAFBFDFFFFFFFFFFFFFFFFFFFAFCFD3C80BC4D90C473ABD35294C63B
      83BA3C85B95695C27DACCF5591BC4380B1FAFCFDFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFAFCFD3C7EBB4B8DC370AAD271AAD274ACD27AAFD35697C44589BBFAFC
      FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFCFD3A7BB9488BC26A
      A6D06EA9D15193C64389BFFAFCFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFAFCFD427FBB3577B7367BB94788C1FAFCFDFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFCFDE9
      F0F7EAF0F7FAFCFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    TabOrder = 3
    OnClick = bWebsiteClick
  end
  object gbInstalledVersion: TGroupBox
    Left = 8
    Top = 49
    Width = 481
    Height = 65
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    object Label6: TLabel
      Left = 24
      Top = 24
      Width = 3
      Height = 13
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4063255
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 24
      Top = 40
      Width = 3
      Height = 13
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4063255
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lIVDate: TLabel
      Left = 104
      Top = 24
      Width = 25
      Height = 13
      Caption = '......'
    end
    object lIVVersion: TLabel
      Left = 104
      Top = 40
      Width = 25
      Height = 13
      Caption = '......'
    end
  end
  object gbLatestVersion: TGroupBox
    Left = 8
    Top = 120
    Width = 481
    Height = 65
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    object Label10: TLabel
      Left = 24
      Top = 24
      Width = 3
      Height = 13
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4063255
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 24
      Top = 40
      Width = 3
      Height = 13
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4063255
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lLADate: TLabel
      Left = 104
      Top = 24
      Width = 25
      Height = 13
      Caption = '......'
    end
    object lLAVersion: TLabel
      Left = 104
      Top = 40
      Width = 25
      Height = 13
      Caption = '......'
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 192
    Width = 481
    Height = 41
    TabOrder = 6
    object lWhat: TLabel
      Left = 21
      Top = 15
      Width = 12
      Height = 13
      Caption = '....'
    end
  end
  object httpMain: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.ContentType = 'text/html'
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 
      'Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, l' +
      'ike Gecko) Chrome/29.0.1547.2 Safari/537.36'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 312
  end
end
