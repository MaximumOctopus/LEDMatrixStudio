object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 877
  ClientWidth = 1490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = miMain
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnConstrainedResize = FormConstrainedResize
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnMouseMove = FormMouseMove
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  TextHeight = 13
  object Bevel20: TBevel
    Left = 0
    Top = 139
    Width = 1490
    Height = 4
    Align = alTop
    Shape = bsTopLine
    ExplicitTop = 135
    ExplicitWidth = 1347
  end
  object pAnimationToolbar: TPanel
    Left = 0
    Top = 828
    Width = 1490
    Height = 30
    Align = alBottom
    Color = clWhite
    ParentBackground = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    DesignSize = (
      1490
      30)
    object lFrame: TLabel
      Left = 204
      Top = 9
      Width = 89
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = '1 / 1'
    end
    object Bevel5: TBevel
      Left = 122
      Top = 10
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object Bevel7: TBevel
      Left = 371
      Top = 10
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object Bevel8: TBevel
      Left = 611
      Top = 10
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object Bevel9: TBevel
      Left = 567
      Top = 10
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object Bevel11: TBevel
      Left = 488
      Top = 10
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object bPlayAnimation: TBitBtn
      Left = 45
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Play animation (right mouse button for speed options)'
      DisabledImageIndex = 56
      Enabled = False
      ImageIndex = 0
      Images = ilActive
      PopupMenu = puAnimationSpeed
      TabOrder = 0
      OnClick = bPlayAnimationClick
    end
    object bStopAnimation: TBitBtn
      Tag = 1
      Left = 84
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Stop animation'
      DisabledImageIndex = 66
      Enabled = False
      ImageIndex = 1
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 1
      OnClick = bPlayAnimationClick
    end
    object bPreviousFrame: TBitBtn
      Tag = 3
      Left = 169
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Show previous frame'
      DisabledImageIndex = 45
      Enabled = False
      ImageIndex = 4
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 2
      OnClick = bPlayAnimationClick
    end
    object bNextFrame: TBitBtn
      Tag = 4
      Left = 295
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Show next frame'
      DisabledImageIndex = 46
      Enabled = False
      ImageIndex = 5
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 3
      OnClick = bPlayAnimationClick
    end
    object bAddFrame: TBitBtn
      Left = 380
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Insert a new empty frame'
      DisabledImageIndex = 93
      Enabled = False
      ImageIndex = 6
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 4
      OnClick = bAddFrameClick
    end
    object bLightbox: TBitBtn
      Left = 576
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Toggle lightbox'
      DisabledImageIndex = 75
      Enabled = False
      ImageIndex = 76
      Images = ilActive
      TabOrder = 5
      OnClick = bLightboxClick
    end
    object bDeleteFrame: TBitBtn
      Left = 495
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Delete current frame'
      DisabledImageIndex = 39
      Enabled = False
      ImageIndex = 7
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 6
      OnClick = bDeleteFrameClick
    end
    object bAddFrameCopy: TBitBtn
      Left = 415
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Add new frame as a copy of the current frame'
      DisabledImageIndex = 69
      Enabled = False
      ImageIndex = 31
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 7
      OnClick = bAddFrameCopyClick
    end
    object tbFrames: TTrackBar
      Left = 622
      Top = 1
      Width = 863
      Height = 28
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      Min = 1
      PageSize = 1
      Position = 1
      PositionToolTip = ptTop
      ShowSelRange = False
      TabOrder = 8
      TickStyle = tsNone
      OnChange = tbFramesChange
      OnTracking = tbFramesTracking
    end
    object bStartFrame: TBitBtn
      Tag = 2
      Left = 130
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Show the first frame'
      DisabledImageIndex = 62
      Enabled = False
      ImageIndex = 2
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 9
      OnClick = bPlayAnimationClick
    end
    object bEndFrame: TBitBtn
      Tag = 5
      Left = 334
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Show the last frame'
      DisabledImageIndex = 63
      Enabled = False
      ImageIndex = 3
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 10
      OnClick = bPlayAnimationClick
    end
    object bAddFrameMultiple: TBitBtn
      Left = 449
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Add new frame as a copy of the current frame'
      DisabledImageIndex = 70
      Enabled = False
      ImageIndex = 36
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 11
      OnClick = bAddFrameMultipleClick
    end
    object bDeleteMultipleFrames: TBitBtn
      Left = 528
      Top = 2
      Width = 33
      Height = 26
      Hint = 'Delete multiple frames'
      DisabledImageIndex = 71
      Enabled = False
      ImageIndex = 37
      Images = ilActive
      NumGlyphs = 2
      TabOrder = 12
      OnClick = bDeleteMultipleFramesClick
    end
    object bTimer: TBitBtn
      Left = 6
      Top = 2
      Width = 33
      Height = 26
      DisabledImageIndex = 67
      ImageIndex = 35
      Images = ilActive
      PopupMenu = puAnimationSpeed
      TabOrder = 13
      OnClick = bTimerClick
    end
  end
  object statusMain: TStatusBar
    Left = 0
    Top = 858
    Width = 1490
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pbFont: TPanel
    Left = 1186
    Top = 143
    Width = 100
    Height = 685
    Align = alRight
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    Visible = False
    object pASCIICode: TPanel
      Left = 1
      Top = 659
      Width = 98
      Height = 25
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object pRGBPalette: TPanel
    Left = 1286
    Top = 143
    Width = 204
    Height = 685
    Align = alRight
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    Visible = False
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 202
      Height = 683
      ActivePage = tsPalette
      Align = alClient
      TabOrder = 0
      object tsPalette: TTabSheet
        Caption = '.'
      end
      object tsGradients: TTabSheet
        Caption = '.'
        ImageIndex = 1
      end
    end
  end
  object panelTop: TPanel
    Left = 0
    Top = 0
    Width = 1490
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    Color = 15790320
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    object sbBuild: TSpeedButton
      Left = 4
      Top = 2
      Width = 58
      Height = 24
      Hint = 'Create new project'
      DisabledImageIndex = 40
      ImageIndex = 16
      Images = ilActive
      OnClick = sbBuildClick
    end
    object Bevel3: TBevel
      Left = 469
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbPreset: TSpeedButton
      Left = 568
      Top = 2
      Width = 60
      Height = 24
      Hint = 'Select a pre-saved preset'
      Caption = 'Preset'
      Glyph.Data = {
        F6000000424DF600000000000000360000002800000008000000080000000100
        180000000000C0000000232E0000232E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00
        FFFF00FFFF00FFFF00FFFF00FFFFFDFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      OnClick = sbPresetClick
    end
    object sbPixelSize: TSpeedButton
      Tag = 2
      Left = 402
      Top = 2
      Width = 60
      Height = 24
      Hint = 'Select pixel size'
      Glyph.Data = {
        F6000000424DF600000000000000360000002800000008000000080000000100
        180000000000C0000000232E0000232E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00
        FFFF00FFFF00FFFF00FFFF00FFFFFDFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      OnClick = sbPixelSizeClick
    end
    object Bevel17: TBevel
      Left = 205
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object lMemoryUsage: TLabel
      Left = 652
      Top = 9
      Width = 24
      Height = 13
      Caption = '......'
    end
    object sbPixelShape: TSpeedButton
      Tag = 2
      Left = 478
      Top = 2
      Width = 73
      Height = 24
      Hint = 'Select pixel shape'
      Glyph.Data = {
        F6000000424DF600000000000000360000002800000008000000080000000100
        180000000000C0000000232E0000232E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFFFFFFFFFFFFFF00
        FFFF00FFFF00FFFF00FFFF00FFFFFDFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF
        00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      OnClick = sbPixelShapeClick
    end
    object Bevel19: TBevel
      Left = 559
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbSave: TSpeedButton
      Left = 137
      Top = 2
      Width = 62
      Height = 24
      Hint = 'Save the current projeect'
      DisabledImageIndex = 61
      ImageIndex = 15
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      Transparent = False
      OnClick = sbSaveClick
    end
    object Bevel10: TBevel
      Left = 641
      Top = 11
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbExport: TSpeedButton
      Tag = 2
      Left = 214
      Top = 2
      Width = 69
      Height = 24
      Hint = 'Export data to file'
      DisabledImageIndex = 55
      ImageIndex = 32
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbExportClick
    end
    object Bevel4: TBevel
      Left = 393
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object Bevel16: TBevel
      Left = 289
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbGenerateCode: TSpeedButton
      Tag = 2
      Left = 298
      Top = 2
      Width = 88
      Height = 24
      Hint = 'Generate code from matrix data'
      Enabled = False
      OnClick = sbGenerateCodeClick
    end
    object sbOpen: TSpeedButton
      Left = 69
      Top = 2
      Width = 62
      Height = 24
      Hint = 'Open a new project'
      DisabledImageIndex = 47
      ImageIndex = 14
      Images = ilActive
      NumGlyphs = 2
      OnClick = sbOpenClick
    end
  end
  object panelMiddle: TPanel
    Left = 0
    Top = 27
    Width = 1490
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentColor = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    object sbClear: TSpeedButton
      Left = 4
      Top = 2
      Width = 50
      Height = 24
      Hint = 'Clear this frame across all layers (can be undone)'
      Enabled = False
      OnClick = sbClearClick
    end
    object Bevel1: TBevel
      Left = 60
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbMirror: TSpeedButton
      Left = 68
      Top = 2
      Width = 50
      Height = 24
      Hint = 'Mirror pixels in the y-axis'
      Enabled = False
      OnClick = sbMirrorClick
    end
    object sbFlip: TSpeedButton
      Tag = 1
      Left = 118
      Top = 2
      Width = 50
      Height = 24
      Hint = 'Flip pixels in the x-axis'
      Enabled = False
      OnClick = sbMirrorClick
    end
    object Bevel2: TBevel
      Left = 225
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbScrollLeft: TSpeedButton
      Left = 232
      Top = 2
      Width = 40
      Height = 24
      Hint = 'Shift the pixels left'
      DisabledImageIndex = 49
      ImageIndex = 8
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbScrollLeftClick
    end
    object sbScrollRight: TSpeedButton
      Tag = 1
      Left = 272
      Top = 2
      Width = 40
      Height = 24
      Hint = 'Shift the pixels right'
      DisabledImageIndex = 60
      ImageIndex = 9
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbScrollLeftClick
    end
    object sbScrollUp: TSpeedButton
      Tag = 2
      Left = 312
      Top = 2
      Width = 40
      Height = 24
      Hint = 'Shift the pixels up'
      DisabledImageIndex = 68
      ImageIndex = 10
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbScrollLeftClick
    end
    object sbScrollDown: TSpeedButton
      Tag = 3
      Left = 352
      Top = 2
      Width = 40
      Height = 24
      Hint = 'Shift the pixels down'
      DisabledImageIndex = 41
      ImageIndex = 11
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbScrollLeftClick
    end
    object sbInvert: TSpeedButton
      Tag = 2
      Left = 168
      Top = 2
      Width = 50
      Height = 24
      Hint = 'Invert pixels'
      Enabled = False
      OnClick = sbMirrorClick
    end
    object Bevel6: TBevel
      Left = 398
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbRotateL: TSpeedButton
      Left = 406
      Top = 2
      Width = 40
      Height = 24
      Hint = 'Rotate pixels anti-clockwise 90 degrees'
      DisabledImageIndex = 64
      ImageIndex = 12
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbRotateLClick
    end
    object sbRotateR: TSpeedButton
      Tag = 1
      Left = 445
      Top = 2
      Width = 40
      Height = 24
      Hint = 'Rotate pixels clockwise 90 degrees'
      DisabledImageIndex = 65
      ImageIndex = 13
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbRotateLClick
    end
    object sbRotateAny: TSpeedButton
      Left = 500
      Top = 2
      Width = 50
      Height = 24
      Hint = 
        'Rotate current frame by specified amount for specified number of' +
        ' frames'
      Enabled = False
      OnClick = sbRotateAnyClick
    end
    object Bevel13: TBevel
      Left = 491
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object Bevel14: TBevel
      Left = 662
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object cbRotateAngle: TComboBox
      Left = 555
      Top = 4
      Width = 52
      Height = 21
      Hint = 'Rotation angle'
      Style = csDropDownList
      Enabled = False
      TabOrder = 0
    end
    object cbRotateCount: TComboBox
      Left = 612
      Top = 4
      Width = 42
      Height = 21
      Hint = 'Number of frames'
      Style = csDropDownList
      Enabled = False
      TabOrder = 1
    end
    object bLockFrame: TBitBtn
      Left = 671
      Top = 2
      Width = 33
      Height = 24
      Hint = 'Toggle frame lock/unlock'
      Enabled = False
      Images = ilActive
      TabOrder = 2
      OnClick = bLockFrameClick
    end
  end
  object paneTools: TPanel
    Left = 0
    Top = 54
    Width = 1490
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentColor = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    object sbMouseMode: TSpeedButton
      Left = 0
      Top = 3
      Width = 30
      Height = 24
      Hint = 'Select normal drawing mode (RMB for sizes)'
      GroupIndex = 1
      Down = True
      DisabledImageIndex = 73
      ImageIndex = 17
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      PopupMenu = puBrushSize
      OnClick = sbMouseModeClick
    end
    object sbFilledRectangle: TSpeedButton
      Tag = 1
      Left = 100
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Draw a filled rectangle'
      GroupIndex = 1
      DisabledImageIndex = 59
      ImageIndex = 19
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbLine: TSpeedButton
      Tag = 3
      Left = 228
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Draw a line between two points'
      GroupIndex = 1
      DisabledImageIndex = 50
      ImageIndex = 28
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbFrame: TSpeedButton
      Tag = 2
      Left = 132
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Draw an empty rectangle'
      GroupIndex = 1
      DisabledImageIndex = 58
      ImageIndex = 20
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbCopy: TSpeedButton
      Tag = 10
      Left = 68
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Select and paste part of your matrix'
      GroupIndex = 1
      DisabledImageIndex = 72
      ImageIndex = 23
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object Bevel15: TBevel
      Left = 491
      Top = 12
      Width = 5
      Height = 10
      Shape = bsLeftLine
    end
    object sbGradient: TSpeedButton
      Left = 388
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Add a gradient to your matrix'
      Enabled = False
      Glyph.Data = {
        36060000424D3606000000000000360000002800000020000000100000000100
        18000000000000060000C21E0000C21E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE80000E80000
        E80000E80000E80000E80000E80000E80000E80000E80000E80000E80000E800
        00E80000E80000FF00FF3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E
        3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3EFF00FFC92900C92900
        C92900C92900C92900C92900C92900C92900C92900C92900C92900C92900C929
        00C92900C92900FF00FF3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F
        3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3FFF00FF706C00706C00
        706C00706C00706C00706C00706C00706C00706C00706C00706C00706C00706C
        00706C00706C00FF00FF60606060606060606060606060606060606060606060
        6060606060606060606060606060606060606060606060FF00FF16AF0016AF00
        16AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF
        0016AF0016AF00FF00FF97979797979797979797979797979797979797979797
        9797979797979797979797979797979797979797979797FF00FF00D14600D146
        00D14600D14600D14600D14600D14600D14600D14600D14600D14600D14600D1
        4600D14600D146FF00FFB7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7
        B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7FF00FF00E9A500E9A5
        00E9A500E9A500E9A500E9A500E9A500E9A500E9A500E9A500E9A500E9A500E9
        A500E9A500E9A5FF00FFD6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6
        D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6D6FF00FF00FCFF00FCFF
        00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FC
        FF00FCFF00FCFFFF00FFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7
        F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7FF00FF01BDFF01BDFF
        01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BD
        FF01BDFF01BDFFFF00FFC9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9
        C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9C9FF00FF017BFF017BFF
        017BFF017BFF017BFF017BFF017BFF017BFF017BFF017BFF017BFF017BFF017B
        FF017BFF017BFFFF00FFA3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3
        A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3A3FF00FF0018FF0018FF
        0018FF0018FF0018FF0018FF0018FF0018FF0018FF0018FF0018FF0018FF0018
        FF0018FF0018FFFF00FF83838383838383838383838383838383838383838383
        8383838383838383838383838383838383838383838383FF00FF80007E80007E
        80007E80007E80007E80007E80007E80007E80007E80007E80007E80007E8000
        7E80007E80007EFF00FF45454545454545454545454545454545454545454545
        4545454545454545454545454545454545454545454545FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      NumGlyphs = 2
      PopupMenu = puGradients
      OnClick = sbGradientClick
    end
    object sbRandomDraw: TSpeedButton
      Tag = 7
      Left = 420
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Paint with random shades of the selected colours'
      GroupIndex = 1
      DisabledImageIndex = 57
      ImageIndex = 30
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      PopupMenu = puRandom
      OnClick = sbMouseModeClick
    end
    object sbEmptyCircle: TSpeedButton
      Tag = 2
      Left = 196
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Draw an empty circle'
      GroupIndex = 1
      DisabledImageIndex = 42
      ImageIndex = 22
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbMultiDraw: TSpeedButton
      Tag = 8
      Left = 260
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Draw on all frames simultaneously'
      GroupIndex = 1
      DisabledImageIndex = 52
      ImageIndex = 34
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPicker: TSpeedButton
      Tag = 9
      Left = 452
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Use mouse buttons to select colour'
      GroupIndex = 1
      DisabledImageIndex = 44
      ImageIndex = 18
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      PopupMenu = puRandom
      OnClick = sbMouseModeClick
    end
    object sbFont: TSpeedButton
      Tag = 4
      Left = 324
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Draw text on the matrix (RMB for font selection)'
      GroupIndex = 1
      DisabledImageIndex = 48
      ImageIndex = 29
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      PopupMenu = puFonts
      OnClick = sbMouseModeClick
    end
    object sbFilledCircle: TSpeedButton
      Tag = 2
      Left = 164
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Draw an filled circle'
      GroupIndex = 1
      DisabledImageIndex = 43
      ImageIndex = 21
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbNewBrush: TSpeedButton
      Tag = 10
      Left = 36
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Create a custom brush'
      GroupIndex = 1
      DisabledImageIndex = 74
      ImageIndex = 27
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbNewBrushClick
    end
    object Bevel18: TBevel
      Left = 912
      Top = 12
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object sbGradientBrush: TSpeedButton
      Tag = 12
      Left = 356
      Top = 2
      Width = 30
      Height = 24
      GroupIndex = 1
      DisabledImageIndex = 52
      ImageIndex = 26
      Images = ilActive
      Enabled = False
      OnClick = sbMouseModeClick
    end
    object sbFloodFill: TSpeedButton
      Tag = 3
      Left = 292
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Fill a section of pixels with a new colour'
      GroupIndex = 1
      DisabledImageIndex = 54
      ImageIndex = 33
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternSpiral: TSpeedButton
      Tag = 14
      Left = 502
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Spiral pattern'
      GroupIndex = 1
      DisabledImageIndex = 85
      ImageIndex = 77
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternCircle: TSpeedButton
      Tag = 15
      Left = 534
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Circle (when viewed radially)'
      GroupIndex = 1
      DisabledImageIndex = 86
      ImageIndex = 78
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternSplitRing: TSpeedButton
      Tag = 16
      Left = 566
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Split ring (when viewed radially)'
      GroupIndex = 1
      DisabledImageIndex = 87
      ImageIndex = 79
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternPetals: TSpeedButton
      Tag = 17
      Left = 598
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Petals (when viewed radially)'
      GroupIndex = 1
      DisabledImageIndex = 88
      ImageIndex = 80
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternGrid: TSpeedButton
      Tag = 18
      Left = 630
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Petals (when viewed radially)'
      GroupIndex = 1
      DisabledImageIndex = 89
      ImageIndex = 81
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternPyramid: TSpeedButton
      Tag = 19
      Left = 662
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Petals (when viewed radially)'
      GroupIndex = 1
      DisabledImageIndex = 90
      ImageIndex = 82
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternRightTriangle: TSpeedButton
      Tag = 21
      Left = 726
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Petals (when viewed radially)'
      GroupIndex = 1
      DisabledImageIndex = 92
      ImageIndex = 84
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object sbPatternLeftTriangle: TSpeedButton
      Tag = 20
      Left = 694
      Top = 2
      Width = 30
      Height = 24
      Hint = 'Petals (when viewed radially)'
      GroupIndex = 1
      DisabledImageIndex = 91
      ImageIndex = 83
      Images = ilActive
      Enabled = False
      NumGlyphs = 2
      OnClick = sbMouseModeClick
    end
    object lSelectedTool: TLabel
      Left = 924
      Top = 8
      Width = 4
      Height = 13
      Hint = 'Current draw mode'
      Caption = '.'
    end
    object lMirror: TLabel
      Left = 775
      Top = 8
      Width = 4
      Height = 13
      Hint = 'Current draw mode'
      Caption = '.'
    end
    object Bevel21: TBevel
      Left = 764
      Top = 12
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object cbMirrorMode: TComboBox
      Left = 821
      Top = 4
      Width = 84
      Height = 21
      Hint = 'Mirror draw mode'
      Style = csDropDownList
      Enabled = False
      TabOrder = 0
      OnChange = cbMirrorModeChange
    end
  end
  object pCanvas: TPanel
    Left = 0
    Top = 143
    Width = 737
    Height = 685
    Align = alClient
    BevelOuter = bvNone
    Color = 3355443
    DoubleBuffered = True
    FullRepaint = False
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 7
    StyleElements = [seFont, seBorder]
    OnMouseDown = pCanvasMouseDown
    OnMouseMove = pCanvasMouseMove
  end
  object pLayers: TPanel
    Left = 737
    Top = 143
    Width = 232
    Height = 685
    Align = alRight
    TabOrder = 8
    Visible = False
  end
  object pCurrentColours: TPanel
    Left = 0
    Top = 81
    Width = 1490
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentColor = True
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    Visible = False
    object sSelectionMMB: TShape
      Tag = 2
      Left = 213
      Top = 6
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour drawn with the middle mouse button'
      Visible = False
      OnMouseDown = sColour3MouseDown
    end
    object iMMBGradient: TImage
      Left = 213
      Top = 6
      Width = 18
      Height = 18
      Hint = 'gradient mode active, use middle mouse button to draw gradients'
      AutoSize = True
      Picture.Data = {
        07544269746D617026040000424D260400000000000036000000280000001200
        0000120000000100180000000000F0030000130B0000130B0000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000800080
        8000808000808000808000808000808000808000808000808000808000808000
        8080008080008080008080008000000000000000008000588000588000588000
        5880005880005880005880005880005880005880005880005880005880005880
        0058800058000000000000000080002E80002E80002E80002E80002E80002E80
        002E80002E80002E80002E80002E80002E80002E80002E80002E80002E000000
        0000000000800006800006800006800006800006800006800006800006800006
        8000068000068000068000068000068000068000060000000000000000B00000
        B00000B00000B00000B00000B00000B00000B00000B00000B00000B00000B000
        00B00000B00000B00000B000000000000000000000E80000E80000E80000E800
        00E80000E80000E80000E80000E80000E80000E80000E80000E80000E80000E8
        0000E800000000000000000000C92900C92900C92900C92900C92900C92900C9
        2900C92900C92900C92900C92900C92900C92900C92900C92900C92900000000
        0000000000706C00706C00706C00706C00706C00706C00706C00706C00706C00
        706C00706C00706C00706C00706C00706C00706C00000000000000000016AF00
        16AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF0016AF
        0016AF0016AF0016AF0016AF00000000000000000000D14600D14600D14600D1
        4600D14600D14600D14600D14600D14600D14600D14600D14600D14600D14600
        D14600D146000000000000000000E9A500E9A500E9A500E9A500E9A500E9A500
        E9A500E9A500E9A500E9A500E9A500E9A500E9A500E9A500E9A500E9A5000000
        000000000000FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF
        00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF00FCFF000000000000000001BDFF
        01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BDFF01BD
        FF01BDFF01BDFF01BDFF01BDFF0000000000000000017BFF017BFF017BFF017B
        FF017BFF017BFF017BFF017BFF017BFF017BFF017BFF017BFF017BFF017BFF01
        7BFF017BFF00000000000000000018FF0018FF0018FF0018FF0018FF0018FF00
        18FF0018FF0018FF0018FF0018FF0018FF0018FF0018FF0018FF0018FF000000
        000000000080007E80007E80007E80007E80007E80007E80007E80007E80007E
        80007E80007E80007E80007E80007E80007E80007E0000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000}
      Visible = False
    end
    object lPixelColour: TLabel
      Left = 660
      Top = 9
      Width = 4
      Height = 13
      Caption = '.'
    end
    object sSelectionRMB: TShape
      Tag = 1
      Left = 268
      Top = 6
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour drawn with the right mouse button'
      Brush.Color = clBlack
      Visible = False
      OnMouseDown = sColour3MouseDown
    end
    object sSelectionLMB: TShape
      Tag = 3
      Left = 158
      Top = 6
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour drawn with the left mouse button'
      Brush.Color = clBlack
      Visible = False
      OnMouseDown = sColour3MouseDown
    end
    object sColour0: TShape
      Left = 93
      Top = 6
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 0 (off), click with either mouse button to select'
      Brush.Color = clBlack
      Visible = False
      OnMouseDown = sColour3MouseDown
    end
    object sColour1: TShape
      Tag = 1
      Left = 69
      Top = 6
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 1, click with either mouse button to select'
      Brush.Color = clBlack
      Visible = False
      OnMouseDown = sColour3MouseDown
    end
    object lBackground: TLabel
      Left = 24
      Top = 8
      Width = 56
      Height = 13
      Caption = 'Background'
      Visible = False
    end
    object sColour3: TShape
      Tag = 3
      Left = 21
      Top = 6
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 3, click with either mouse button to select'
      Brush.Color = clBlack
      Visible = False
      OnMouseDown = sColour3MouseDown
    end
    object sColour2: TShape
      Tag = 2
      Left = 45
      Top = 6
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 2, click with either mouse button to select'
      Brush.Color = clBlack
      Visible = False
      OnMouseDown = sColour3MouseDown
    end
    object Bevel12: TBevel
      Left = 121
      Top = 10
      Width = 5
      Height = 14
      Shape = bsLeftLine
    end
    object iColoursLeft: TImage
      Left = 136
      Top = 5
      Width = 16
      Height = 18
      AutoSize = True
      Picture.Data = {
        07544269746D617096030000424D960300000000000036000000280000001000
        000012000000010018000000000060030000120B0000120B0000000000000000
        0000FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000FF00FFFF00FF000000D3D3D3D3D3D3D3D3
        D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3000000FF
        00FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFCFCFC000000000000000000FFFFFFFEFEFEFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        00000000002A00FF2A00FF2A00FF2A00FF2A00FF2A00FFFFFFFFFFFFFFB1B7B7
        B2B6B6B4B5B5B2B7B7B2B7B7B2B7B70000000000002A00FF2A00FF2A00FF2A00
        FF2A00FF2A00FFFFFFFFFDFDFDB2B7B7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        00000000002A00FF2A00FF2A00FF2A00FF2A00FF2A00FFFFFFFFFFFFFFB2B7B7
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000002A00FF2A00FF2A00FF2A00
        FF2A00FFFFFFFFFFFFFFFFFFFFFFFFFFB2B7B7FFFFFFFFFFFFFFFFFFFFFFFF00
        00000000002A00FF2A00FF2A00FF2A00FF2A00FFFFFFFFFFFFFFFFFFFFFFFFFF
        B2B7B7FFFFFFFFFFFFFFFFFFFFFFFF000000000000A4A4A42A00FF2A00FF2A00
        FF2A00FFFFFFFFFFFFFFFFFFFFFFFFFFB2B7B7FFFFFFFFFFFFFFFFFFA7A7A700
        0000000000181818E3E3E32A00FF2A00FF2A00FFFFFFFFFFFFFFFFFFFFFFFFFF
        B4B5B5FFFFFFFFFFFFE4E4E4191919000000000000000000202020C5C5C52A00
        FF2A00FF2A00FFFFFFFFFFFFFFB2B7B7FFFFFFFFFFFFC6C6C620202000000000
        0000FF00FF0000000000000101014B4B4BA5A5A5DBDDDDF5F5F5F4F5F5C2D5D5
        A5A5A54B4B4B010101000000000000FF00FFFF00FFFF00FFFF00FF0000000000
        00000000000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FF}
      Transparent = True
    end
    object iColoursMiddle: TImage
      Left = 191
      Top = 5
      Width = 16
      Height = 18
      AutoSize = True
      Picture.Data = {
        07544269746D617096030000424D960300000000000036000000280000001000
        000012000000010018000000000060030000120B0000120B0000000000000000
        0000FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000FF00FFFF00FF000000D3D3D3D3D3D3D3D3
        D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3000000FF
        00FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFCFCFC000000000000000000FFFFFFFEFEFEFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FEFEFEFEFEFEFFFFFFFFFF
        FFFFFFFFFFFFFFFDFDFFFAF9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000B2B7B7B2B7B7B1B6B8B0B4B8B0B4B9B0B4B92F06FF3108FFB2B7B7
        B2B7B7B2B7B7B2B7B7B2B7B7B2B7B7000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFB0B4B92A00FF2A00FFB2B7B7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFEFEFFB0B4B92A00FF2A00FFB2B7B7
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFB0B4B92A00FF2A00FF2A00FF2A00FFB2B7B7FFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFB2B7B72A00FF2A00FF2A00FF2A00FF
        B2B7B7FFFFFFFFFFFFFFFFFFFFFFFF000000000000A4A4A4FFFFFFFFFFFFFFFF
        FFB2B7B72A00FF2A00FF2A00FF2A00FFB2B7B7FFFFFFFFFFFFFFFFFFA7A7A700
        0000000000181818E4E4E4FFFFFFFFFFFFB2B7B72A00FF2A00FF2A00FF2A00FF
        B2B7B7FFFFFFFFFFFFE4E4E4191919000000000000000000202020C5C5C5FFFF
        FFFFFFFFB2B7B72A00FF2A00FFB2B7B7FFFFFFFFFFFFC6C6C620202000000000
        0000FF00FF0000000000000101014F4F4FAFAFAFD7DBDBF5F5F5F4F5F5C0D2D2
        A5A5A54B4B4B010101000000000000FF00FFFF00FFFF00FFFF00FF0000000000
        00000000000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FF}
      Transparent = True
    end
    object iColoursRight: TImage
      Left = 246
      Top = 5
      Width = 16
      Height = 18
      AutoSize = True
      Picture.Data = {
        07544269746D617096030000424D960300000000000036000000280000001000
        000012000000010018000000000060030000120B0000120B0000000000000000
        0000FF00FF000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000FF00FFFF00FF000000D3D3D3D3D3D3D3D3
        D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3D3000000FF
        00FF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFCFCFC000000000000000000FFFFFFFEFEFEFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FEFEFEFEFEFEFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000000000B2B7B7B2B7B7B1B6B8B0B4B8AFB4B9B1B6B7FFFFFFFFFFFF2A00FF
        2A00FF2A00FF2A00FF2A00FF2A00FF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFB2B7B7FDFEFEFDFDFD2A00FF2A00FF2A00FF2A00FF2A00FF2A00FF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFEFEFFB2B7B7FEFEFEFFFFFF2A00FF
        2A00FF2A00FF2A00FF2A00FF2A00FF000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFB0B4B9FFFFFFFFFFFFFFFFFFFFFFFF2A00FF2A00FF2A00FF2A00FF2A00FF00
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFB2B7B7FFFFFFFFFFFFFFFFFFFFFFFF
        2A00FF2A00FF2A00FF2A00FF2A00FF000000000000A4A4A4FFFFFFFFFFFFFFFF
        FFB2B7B7FEFEFEFFFFFFFFFFFFFFFFFF2A00FF2A00FF2A00FF2A00FFA7A7A700
        0000000000181818E4E4E4FFFFFFFFFFFFB2B7B7FFFFFFFFFFFFFFFFFFFFFFFF
        2A00FF2A00FF2A00FFE4E4E4191919000000000000000000202020C5C5C5FFFF
        FFFFFFFFB2B7B7FFFFFFFFFFFF2A00FF2A00FF2A00FFC6C6C620202000000000
        0000FF00FF0000000000000101014F4F4FAFAFAFD7DBDBF5F5F5F4F5F5C2D5D5
        A5A5A54B4B4B010101000000000000FF00FFFF00FFFF00FFFF00FF0000000000
        00000000000000000000000000000000000000000000000000FF00FFFF00FFFF
        00FF}
      Transparent = True
    end
  end
  object panelRGBPalette: TPanel
    Left = 0
    Top = 105
    Width = 1490
    Height = 34
    Align = alTop
    BevelOuter = bvNone
    Color = 3355443
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    ParentShowHint = False
    ShowCaption = False
    ShowHint = True
    TabOrder = 10
    object sRGBPalette1: TShape
      Left = 4
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette2: TShape
      Tag = 1
      Left = 24
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette3: TShape
      Tag = 2
      Left = 44
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette4: TShape
      Tag = 3
      Left = 64
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette5: TShape
      Tag = 4
      Left = 84
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette6: TShape
      Tag = 5
      Left = 104
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette7: TShape
      Tag = 6
      Left = 124
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette8: TShape
      Tag = 7
      Left = 144
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette9: TShape
      Tag = 8
      Left = 164
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette10: TShape
      Tag = 9
      Left = 184
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette11: TShape
      Tag = 10
      Left = 204
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette12: TShape
      Tag = 11
      Left = 224
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette13: TShape
      Tag = 12
      Left = 244
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette14: TShape
      Tag = 13
      Left = 264
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette15: TShape
      Tag = 14
      Left = 284
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sRGBPalette16: TShape
      Tag = 15
      Left = 304
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 
        'click with either mouse button to select (colour picker to chang' +
        'e colour)'
      Brush.Color = clGray
      OnMouseDown = sRGBPalette1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape37: TShape
      Tag = 8
      Left = 641
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (red)'
      Brush.Color = clRed
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape38: TShape
      Tag = 7
      Left = 801
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (lilac)'
      Brush.Color = 16744703
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape39: TShape
      Tag = 6
      Left = 781
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (purple)'
      Brush.Color = 16711808
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape40: TShape
      Tag = 5
      Left = 761
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (light blue)'
      Brush.Color = 16744448
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape41: TShape
      Tag = 4
      Left = 741
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (blue)'
      Brush.Color = clBlue
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape42: TShape
      Tag = 3
      Left = 721
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (green)'
      Brush.Color = 4259584
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape43: TShape
      Tag = 2
      Left = 701
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (yellow)'
      Brush.Color = clYellow
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape44: TShape
      Tag = 1
      Left = 681
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (orange)'
      Brush.Color = 4227327
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape45: TShape
      Left = 661
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (pink)'
      Brush.Color = 8421631
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Shape46: TShape
      Tag = 9
      Left = 821
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'click with either mouse button to select (white)'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade1: TShape
      Tag = 999
      Left = 344
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 0 (off), click with either mouse button to select'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade2: TShape
      Tag = 999
      Left = 361
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 0 (off), click with either mouse button to select'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade3: TShape
      Tag = 999
      Left = 378
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 0 (off), click with either mouse button to select'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade4: TShape
      Tag = 999
      Left = 395
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 3'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade5: TShape
      Tag = 999
      Left = 412
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 4'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade6: TShape
      Tag = 999
      Left = 429
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 5'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade7: TShape
      Tag = 999
      Left = 446
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 6'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade8: TShape
      Tag = 999
      Left = 463
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 7'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade9: TShape
      Tag = 999
      Left = 480
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 8'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade10: TShape
      Tag = 999
      Left = 497
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 9'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade16: TShape
      Tag = 999
      Left = 599
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 15'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade15: TShape
      Tag = 999
      Left = 582
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 14'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade14: TShape
      Tag = 999
      Left = 565
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 13'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade13: TShape
      Tag = 999
      Left = 548
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Palette/gradient colour 12'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade12: TShape
      Tag = 999
      Left = 531
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 0 (off), click with either mouse button to select'
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object sShade11: TShape
      Tag = 999
      Left = 514
      Top = 14
      Width = 18
      Height = 18
      Cursor = crHandPoint
      Hint = 'Colour 0 (off), click with either mouse button to select'
      Brush.Color = clBlack
      OnMouseDown = sShade1MouseDown
      OnMouseMove = sRGBPalette1MouseMove
    end
    object Label1: TLabel
      Left = 349
      Top = 1
      Width = 6
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 366
      Top = 1
      Width = 6
      Height = 13
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 383
      Top = 1
      Width = 6
      Height = 13
      Caption = '2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 400
      Top = 1
      Width = 6
      Height = 13
      Caption = '3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 417
      Top = 1
      Width = 6
      Height = 13
      Caption = '4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 589
      Top = 1
      Width = 6
      Height = 13
      Caption = 'E'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 572
      Top = 1
      Width = 7
      Height = 13
      Caption = 'D'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 555
      Top = 1
      Width = 7
      Height = 13
      Caption = 'C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 538
      Top = 1
      Width = 6
      Height = 13
      Caption = 'B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 521
      Top = 1
      Width = 7
      Height = 13
      Caption = 'A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 434
      Top = 1
      Width = 6
      Height = 13
      Caption = '5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 451
      Top = 1
      Width = 6
      Height = 13
      Caption = '6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel
      Left = 468
      Top = 1
      Width = 6
      Height = 13
      Caption = '7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 485
      Top = 1
      Width = 6
      Height = 13
      Caption = '8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label16: TLabel
      Left = 503
      Top = 1
      Width = 6
      Height = 13
      Caption = '9'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label17: TLabel
      Left = 606
      Top = 1
      Width = 6
      Height = 13
      Caption = 'F'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object pRGB_3BPP: TPanel
      Left = 892
      Top = 4
      Width = 880
      Height = 33
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
      object sRGB3pp1: TShape
        Left = 9
        Top = 3
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        Brush.Color = clBlack
        OnMouseDown = sRGB3pp1MouseDown
      end
      object sRGB3pp2: TShape
        Tag = 1
        Left = 30
        Top = 3
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        Brush.Color = clBlue
        OnMouseDown = sRGB3pp1MouseDown
      end
      object sRGB3pp3: TShape
        Tag = 2
        Left = 51
        Top = 3
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        Brush.Color = clLime
        OnMouseDown = sRGB3pp1MouseDown
      end
      object sRGB3pp4: TShape
        Tag = 3
        Left = 72
        Top = 3
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        Brush.Color = clAqua
        OnMouseDown = sRGB3pp1MouseDown
      end
      object sRGB3pp5: TShape
        Tag = 4
        Left = 93
        Top = 3
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        Brush.Color = clRed
        OnMouseDown = sRGB3pp1MouseDown
      end
      object sRGB3pp6: TShape
        Tag = 5
        Left = 114
        Top = 3
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        Brush.Color = clFuchsia
        OnMouseDown = sRGB3pp1MouseDown
      end
      object sRGB3pp7: TShape
        Tag = 6
        Left = 135
        Top = 3
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        Brush.Color = clYellow
        OnMouseDown = sRGB3pp1MouseDown
      end
      object sRGB3pp8: TShape
        Tag = 7
        Left = 159
        Top = 2
        Width = 18
        Height = 18
        Cursor = crHandPoint
        Hint = 
          'click with either mouse button to select (colour picker to chang' +
          'e colour)'
        OnMouseDown = sRGB3pp1MouseDown
      end
    end
  end
  object pUndoToolbar: TPanel
    Left = 1125
    Top = 168
    Width = 161
    Height = 619
    TabOrder = 11
    Visible = False
  end
  object pQuickData: TPanel
    Left = 969
    Top = 143
    Width = 217
    Height = 685
    Align = alRight
    TabOrder = 12
    Visible = False
  end
  object miMain: TMainMenu
    AutoLineReduction = maManual
    Images = ilMenu
    Left = 712
    object File1: TMenuItem
      Caption = '.'
      object New1: TMenuItem
        Caption = '.'
        ImageIndex = 8
        ShortCut = 16462
        OnClick = New1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Load1: TMenuItem
        Caption = '.'
        ImageIndex = 3
        ShortCut = 16463
        OnClick = sbOpenClick
      end
      object miReopenMenu: TMenuItem
        Caption = '.'
      end
      object N55: TMenuItem
        Caption = '-'
      end
      object miImportFromBitmap: TMenuItem
        Caption = '.'
        ShortCut = 119
        OnClick = miImportFromBitmapClick
      end
      object miImportFromGIF: TMenuItem
        Caption = '.'
        ShortCut = 120
        OnClick = miImportFromGIFClick
      end
      object miImportInToCurrent: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miImportInToCurrentClick
      end
      object N31: TMenuItem
        Caption = '-'
      end
      object miAppend: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16449
        OnClick = miAppendClick
      end
      object miMerge: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miMergeClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miSave: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16467
        OnClick = sbSaveClick
      end
      object miSaveAs: TMenuItem
        Caption = '.'
        Enabled = False
        ImageIndex = 2
        OnClick = miSaveAsClick
      end
      object miSaveSingleFrame: TMenuItem
        Caption = '.'
        Enabled = False
        ImageIndex = 1
        OnClick = miSaveSingleFrameClick
      end
      object miSaveRange: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miSaveRangeClick
      end
      object miSaveAsFont: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miSaveAsFontClick
      end
      object N41: TMenuItem
        Caption = '-'
      end
      object miExportToBitmap: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miExportToBitmapClick
      end
      object miExportAnimationToBitmap: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miExportAnimationToBitmapClick
      end
      object miExportToGIF: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miExportToGIFClick
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object Preferences1: TMenuItem
        Caption = '.'
        ShortCut = 24698
        OnClick = Preferences1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '.'
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '.'
      object miUndo: TMenuItem
        Caption = '.'
        Enabled = False
        ImageIndex = 0
        ShortCut = 16474
        OnClick = miUndoClick
      end
      object miRedo: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 24666
        OnClick = miRedoClick
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object miCopy: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16451
        OnClick = miCopyClick
      end
      object miCopyFromPrevious: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16450
        OnClick = miCopyFromPreviousClick
      end
      object miCopyMultiple: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miCopyMultipleClick
      end
      object miPaste: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16470
        OnClick = miPasteClick
      end
      object miPasteSpecial: TMenuItem
        Caption = '.'
        Enabled = False
        object Copyandshiftleft1: TMenuItem
          Caption = '.'
          ShortCut = 32805
          OnClick = Copyandshiftleft1Click
        end
        object Copyandshiftright1: TMenuItem
          Tag = 1
          Caption = '.'
          ShortCut = 32807
          OnClick = Copyandshiftleft1Click
        end
        object Copyandshiftup1: TMenuItem
          Tag = 2
          Caption = '.'
          ShortCut = 32806
          OnClick = Copyandshiftleft1Click
        end
        object Copyandshiftdown1: TMenuItem
          Tag = 3
          Caption = '.'
          ShortCut = 32808
          OnClick = Copyandshiftleft1Click
        end
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object miBrushActions: TMenuItem
        Caption = '.'
        Enabled = False
        object Rotateanticlockwise1: TMenuItem
          Caption = 'Rotate anti-clockwise'
          ShortCut = 41179
          OnClick = Rotateanticlockwise1Click
        end
        object Rotateclockwise1: TMenuItem
          Caption = 'Rotate clockwise'
          ShortCut = 41181
          OnClick = Rotateclockwise1Click
        end
        object N45: TMenuItem
          Caption = '-'
        end
        object miBrushFlip: TMenuItem
          Caption = 'Flip'
          ShortCut = 41009
          OnClick = miBrushFlipClick
        end
        object Mirror1: TMenuItem
          Tag = 1
          Caption = 'Mirror'
          ShortCut = 41010
          OnClick = miBrushFlipClick
        end
        object Invert1: TMenuItem
          Tag = 2
          Caption = 'Invert'
          ShortCut = 41011
          OnClick = miBrushFlipClick
        end
        object N46: TMenuItem
          Caption = '-'
        end
        object Pasteintoeveryframe1: TMenuItem
          Caption = 'Paste in to every frame'
          ShortCut = 41017
          OnClick = Pasteintoeveryframe1Click
        end
        object Pasteintoeveryframetransparent1: TMenuItem
          Caption = 'Paste in to every frame (transparent) '
          ShortCut = 41008
          OnClick = Pasteintoeveryframetransparent1Click
        end
      end
      object N37: TMenuItem
        Caption = '-'
      end
      object miShiftLeft: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16421
        OnClick = miShiftLeftClick
      end
      object miShiftRight: TMenuItem
        Tag = 1
        Caption = '.'
        Enabled = False
        ShortCut = 16423
        OnClick = miShiftLeftClick
      end
      object miShiftUp: TMenuItem
        Tag = 2
        Caption = '.'
        Enabled = False
        ShortCut = 16422
        OnClick = miShiftLeftClick
      end
      object miShiftDown: TMenuItem
        Tag = 3
        Caption = '.'
        Enabled = False
        ShortCut = 16424
        OnClick = miShiftLeftClick
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object miRotateL: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16603
        OnClick = miRotateLClick
      end
      object miRotateR: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16605
        OnClick = miRotateLClick
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object miFlip: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16454
        OnClick = miFlipClick
      end
      object miMirror: TMenuItem
        Tag = 1
        Caption = '.'
        Enabled = False
        ShortCut = 16461
        OnClick = miFlipClick
      end
      object miInvert: TMenuItem
        Tag = 2
        Caption = '.'
        Enabled = False
        ShortCut = 16457
        OnClick = miFlipClick
      end
      object N26: TMenuItem
        Caption = '-'
      end
      object miAddComment: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miAddCommentClick
      end
    end
    object View1: TMenuItem
      Caption = '.'
      object miShowAnimationToolbar: TMenuItem
        AutoCheck = True
        Caption = '.'
        Checked = True
        OnClick = miShowAnimationToolbarClick
      end
      object miPaletteGradientToolbar: TMenuItem
        Caption = '.'
        OnClick = miPaletteGradientToolbarClick
      end
      object miQuickData: TMenuItem
        AutoCheck = True
        Caption = '.'
        OnClick = miQuickDataClick
      end
      object miUndoToolbar: TMenuItem
        AutoCheck = True
        Caption = '.'
        Visible = False
        OnClick = miUndoToolbarClick
      end
      object N57: TMenuItem
        Caption = '-'
      end
      object Backgroundcolour1: TMenuItem
        Caption = '.'
        object miCustomBackground: TMenuItem
          Caption = '.'
          OnClick = miCustomBackgroundClick
        end
        object N42: TMenuItem
          Caption = '-'
        end
        object Black1: TMenuItem
          Caption = '.'
          OnClick = Black1Click
        end
        object Darkgrey1: TMenuItem
          Tag = 6
          Caption = '.'
          OnClick = Black1Click
        end
        object Grey1: TMenuItem
          Tag = 1
          Caption = '.'
          OnClick = Black1Click
        end
        object Green1: TMenuItem
          Tag = 2
          Caption = '.'
          OnClick = Black1Click
        end
        object Purple1: TMenuItem
          Tag = 3
          Caption = '.'
          OnClick = Black1Click
        end
        object Red1: TMenuItem
          Tag = 4
          Caption = '.'
          OnClick = Black1Click
        end
        object White1: TMenuItem
          Tag = 5
          Caption = '.'
          OnClick = Black1Click
        end
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object miFontMode: TMenuItem
        AutoCheck = True
        Caption = '.'
        OnClick = miFontModeClick
      end
      object miASCIIStartCode: TMenuItem
        Caption = '.'
        OnClick = miASCIIStartCodeClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object miPreviousFrame: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 113
        OnClick = miPreviousFrameClick
      end
      object miNextFrame: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 114
        OnClick = miNextFrameClick
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object miGridToggle: TMenuItem
        Caption = '.'
        Checked = True
        OnClick = miGridToggleClick
      end
    end
    object Preview1: TMenuItem
      Caption = '.'
      object miPreview: TMenuItem
        AutoCheck = True
        Caption = '.'
        ShortCut = 16465
        OnClick = miPreviewClick
      end
      object PreviewSize1: TMenuItem
        Caption = '.'
        object miPreviewx1: TMenuItem
          AutoCheck = True
          Caption = 'x1'
          Checked = True
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx2: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'x2'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx3: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = 'x3'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx4: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = 'x4'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx5: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = 'x5'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx6: TMenuItem
          Tag = 5
          AutoCheck = True
          Caption = 'x6'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx8: TMenuItem
          Tag = 6
          AutoCheck = True
          Caption = 'x8'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx10: TMenuItem
          Tag = 7
          AutoCheck = True
          Caption = 'x10'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx12: TMenuItem
          Tag = 8
          AutoCheck = True
          Caption = 'x12'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx15: TMenuItem
          Tag = 9
          AutoCheck = True
          Caption = 'x15'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx20: TMenuItem
          Tag = 10
          AutoCheck = True
          Caption = 'x20'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx25: TMenuItem
          Tag = 11
          AutoCheck = True
          Caption = 'x25'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx30: TMenuItem
          Tag = 12
          AutoCheck = True
          Caption = 'x30'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx40: TMenuItem
          Tag = 13
          AutoCheck = True
          Caption = 'x40'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object miPreviewx50: TMenuItem
          Tag = 14
          AutoCheck = True
          Caption = 'x50'
          GroupIndex = 30
          RadioItem = True
          OnClick = miPreviewx1Click
        end
        object N51: TMenuItem
          Caption = '-'
          GroupIndex = 30
        end
        object miIncrementRadially: TMenuItem
          AutoCheck = True
          Caption = '.'
          GroupIndex = 30
          OnClick = miIncrementRadiallyClick
        end
      end
      object miPreviewView: TMenuItem
        Caption = '.'
        object miPreviewViewSquare: TMenuItem
          AutoCheck = True
          Caption = '.'
          Checked = True
          RadioItem = True
          OnClick = miPreviewViewSquareClick
        end
        object miPreviewViewRadial: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = '.'
          RadioItem = True
          OnClick = miPreviewViewSquareClick
        end
        object miPreviewViewRadialTQ: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = '.'
          RadioItem = True
          OnClick = miPreviewViewSquareClick
        end
        object miPreviewViewSemiCircle: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = '.'
          RadioItem = True
          OnClick = miPreviewViewSquareClick
        end
        object miPreviewViewSemiCircleInverted: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = '.'
          RadioItem = True
          OnClick = miPreviewViewSquareClick
        end
      end
      object PreviewVoidRadial1: TMenuItem
        Caption = '.'
        object miPreviewVoid10: TMenuItem
          AutoCheck = True
          Caption = '10 (default)'
          Checked = True
          Default = True
          RadioItem = True
          OnClick = miPreviewVoid10Click
        end
        object miPreviewVoid15: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = '15'
          RadioItem = True
          OnClick = miPreviewVoid10Click
        end
        object miPreviewVoid20: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = '20'
          RadioItem = True
          OnClick = miPreviewVoid10Click
        end
        object miPreviewVoid25: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = '25'
          RadioItem = True
          OnClick = miPreviewVoid10Click
        end
        object miPreviewVoid30: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = '30'
          RadioItem = True
          OnClick = miPreviewVoid10Click
        end
        object miPreviewVoid40: TMenuItem
          Tag = 5
          AutoCheck = True
          Caption = '40'
          RadioItem = True
          OnClick = miPreviewVoid10Click
        end
        object miPreviewVoid50: TMenuItem
          Tag = 6
          AutoCheck = True
          Caption = '50'
          RadioItem = True
          OnClick = miPreviewVoid10Click
        end
      end
      object Previewoffsetradialsemicircle1: TMenuItem
        Caption = '.'
        object miRadialOffset0: TMenuItem
          AutoCheck = True
          Caption = '0'#176
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object miRadialOffset45: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = '45'#176
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object miRadialOffset90: TMenuItem
          Tag = 2
          AutoCheck = True
          Caption = '90'#176
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object miRadialOffset135: TMenuItem
          Tag = 3
          AutoCheck = True
          Caption = '135'#176
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object miRadialOffset180: TMenuItem
          Tag = 4
          AutoCheck = True
          Caption = '180'#176
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object miRadialOffset225: TMenuItem
          Tag = 5
          AutoCheck = True
          Caption = '225'#176
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object miRadialOffset270: TMenuItem
          Tag = 6
          AutoCheck = True
          Caption = '270'#176
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object miRadialOffset315: TMenuItem
          Tag = 7
          AutoCheck = True
          Caption = '315'#176
          GroupIndex = 1
          RadioItem = True
          OnClick = miRadialOffset45Click
        end
        object N48: TMenuItem
          Caption = '-'
          GroupIndex = 1
        end
        object miPreviewOffsetReverse: TMenuItem
          AutoCheck = True
          Caption = '.'
          GroupIndex = 1
          OnClick = miPreviewOffsetReverseClick
        end
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object miPopoutPreview: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16464
        OnClick = miPopoutPreviewClick
      end
      object N65: TMenuItem
        Caption = '-'
      end
      object miPreviewAllowDrawing: TMenuItem
        AutoCheck = True
        Caption = '.'
        OnClick = miPreviewAllowDrawingClick
      end
    end
    object Project1: TMenuItem
      Caption = '.'
      object miClearAllFramesLayer: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miClearAllFramesLayerClick
      end
      object miClearAllFrames: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miClearAllFramesClick
      end
      object N22: TMenuItem
        Caption = '-'
      end
      object miFlipAllFrames: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 49222
        OnClick = miFlipAllFramesClick
      end
      object miMirrorAllFrames: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 49229
        OnClick = miMirrorAllFramesClick
      end
      object miInvertAllFrames: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 49225
        OnClick = miInvertAllFramesClick
      end
      object miGradientAllFrames: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 49223
        OnClick = miGradientAllFramesClick
      end
      object N36: TMenuItem
        Caption = '-'
      end
      object miIgnoredPixels: TMenuItem
        Caption = '.'
        Enabled = False
        object miSetIgnoredPixels: TMenuItem
          Caption = '.'
          Enabled = False
          ShortCut = 123
          OnClick = miSetIgnoredPixelsClick
        end
        object miSetIgnoredFromPattern: TMenuItem
          Caption = '.'
          Enabled = False
          OnClick = miSetIgnoredFromPatternClick
        end
        object N44: TMenuItem
          Caption = '-'
        end
        object miClearAllIgnoredPixels: TMenuItem
          Caption = '.'
          Enabled = False
          OnClick = miClearAllIgnoredPixelsClick
        end
        object N56: TMenuItem
          Caption = '-'
        end
        object miSaveIgnoredPixelsAsPattern: TMenuItem
          Caption = '.'
          OnClick = miSaveIgnoredPixelsAsPatternClick
        end
        object miLoadIgnoredPixelsAsPattern: TMenuItem
          Caption = '.'
          OnClick = miLoadIgnoredPixelsAsPatternClick
        end
        object N66: TMenuItem
          Caption = '-'
          Visible = False
        end
        object miHideIgnoredPixels: TMenuItem
          AutoCheck = True
          Caption = '.'
          Visible = False
          OnClick = miHideIgnoredPixelsClick
        end
      end
      object N43: TMenuItem
        Caption = '-'
      end
      object miFadeFirstLast: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miFadeFirstLastClick
      end
      object N35: TMenuItem
        Caption = '-'
      end
      object miExport: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 115
        OnClick = sbExportClick
      end
      object miCodeTemplates: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 116
        OnClick = sbGenerateCodeClick
      end
      object N50: TMenuItem
        Caption = '-'
      end
      object miUnlockAll: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 32844
        OnClick = miUnlockAllClick
      end
      object miLockAll: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 49228
        OnClick = miLockAllClick
      end
      object miToggleLockStatus: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miToggleLockStatusClick
      end
    end
    object Draw1: TMenuItem
      Caption = '.'
      object miMouseMode: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 32833
        OnClick = miMouseModeClick
      end
      object miNewBrush: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 32834
        OnClick = sbNewBrushClick
      end
      object miDrawCopy: TMenuItem
        Tag = 10
        Caption = '.'
        Enabled = False
        ShortCut = 32835
        OnClick = miMouseModeClick
      end
      object N59: TMenuItem
        Caption = '-'
      end
      object miFilledRectangle: TMenuItem
        Tag = 1
        Caption = '.'
        Enabled = False
        ShortCut = 32836
        OnClick = miMouseModeClick
      end
      object miFrame: TMenuItem
        Tag = 2
        Caption = '.'
        Enabled = False
        ShortCut = 32837
        OnClick = miMouseModeClick
      end
      object miFilledCircle: TMenuItem
        Tag = 6
        Caption = '.'
        Enabled = False
        ShortCut = 32838
        OnClick = miMouseModeClick
      end
      object miEmptyCircle: TMenuItem
        Tag = 5
        Caption = '.'
        Enabled = False
        ShortCut = 32839
        OnClick = miMouseModeClick
      end
      object N60: TMenuItem
        Caption = '-'
      end
      object miLine: TMenuItem
        Tag = 3
        Caption = '.'
        Enabled = False
        ShortCut = 32840
        OnClick = miMouseModeClick
      end
      object miMultiDraw: TMenuItem
        Tag = 8
        Caption = '.'
        Enabled = False
        ShortCut = 32841
        OnClick = miMouseModeClick
      end
      object miFloodFill: TMenuItem
        Tag = 3
        Caption = '.'
        Enabled = False
        ShortCut = 32842
        OnClick = miMouseModeClick
      end
      object miFont: TMenuItem
        Tag = 4
        Caption = '.'
        Enabled = False
        ShortCut = 32843
        OnClick = miMouseModeClick
      end
      object miGradientBrush: TMenuItem
        Tag = 12
        Caption = '.'
        Enabled = False
        ShortCut = 32844
        OnClick = miMouseModeClick
      end
      object miGradient: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 32845
        OnClick = sbGradientClick
      end
      object miRandomDraw: TMenuItem
        Tag = 7
        Caption = '.'
        Enabled = False
        ShortCut = 32846
        OnClick = miMouseModeClick
      end
      object miPicker: TMenuItem
        Tag = 9
        Caption = '.'
        Enabled = False
        ShortCut = 32848
        OnClick = miMouseModeClick
      end
      object N58: TMenuItem
        Caption = '-'
      end
      object miPatternSpiral: TMenuItem
        Tag = 14
        Caption = '.'
        Enabled = False
        ShortCut = 32849
        OnClick = miMouseModeClick
      end
      object miPatternCircle: TMenuItem
        Tag = 15
        Caption = '.'
        Enabled = False
        ShortCut = 32850
        OnClick = miMouseModeClick
      end
      object miPatternSplitRing: TMenuItem
        Tag = 16
        Caption = '.'
        Enabled = False
        ShortCut = 32851
        OnClick = miMouseModeClick
      end
      object miPatternPetals: TMenuItem
        Tag = 17
        Caption = '.'
        Enabled = False
        ShortCut = 32852
        OnClick = miMouseModeClick
      end
      object miPatternGrid: TMenuItem
        Tag = 18
        Caption = '.'
        Enabled = False
        ShortCut = 32853
        OnClick = miMouseModeClick
      end
      object miPatternPyramid: TMenuItem
        Tag = 19
        Caption = '.'
        Enabled = False
        ShortCut = 32854
        OnClick = miMouseModeClick
      end
      object miPatternLeftTriangle: TMenuItem
        Tag = 20
        Caption = '.'
        Enabled = False
        ShortCut = 32855
        OnClick = miMouseModeClick
      end
      object miPatternRightTriangle: TMenuItem
        Tag = 21
        Caption = '.'
        Enabled = False
        ShortCut = 32856
        OnClick = miMouseModeClick
      end
    end
    object Frames1: TMenuItem
      Caption = '.'
      object miAddFrame: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 24641
        OnClick = bAddFrameClick
      end
      object miAddFrameCopy: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 24643
        OnClick = bAddFrameCopyClick
      end
      object miAddFrameMultiple: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 24653
        OnClick = bAddFrameMultipleClick
      end
      object N61: TMenuItem
        Caption = '-'
      end
      object miDeleteFrame: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 24644
        OnClick = bDeleteFrameClick
      end
      object miDeleteMultipleFrames: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 24664
        OnClick = bDeleteMultipleFramesClick
      end
    end
    object Layers1: TMenuItem
      Caption = '.'
      object miToggleLayoutPanel: TMenuItem
        Caption = '.'
        ShortCut = 118
        OnClick = miToggleLayoutPanelClick
      end
      object N53: TMenuItem
        Caption = '-'
      end
      object miClearLayer: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miClearLayerClick
      end
      object N54: TMenuItem
        Caption = '-'
      end
      object miFlattenLayers: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16460
        OnClick = miFlattenLayersClick
      end
    end
    object Colours1: TMenuItem
      Caption = '.'
      object miChangeColoursFrame: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 16471
        OnClick = miChangeColoursFrameClick
      end
      object miChangeColoursLayer: TMenuItem
        Tag = 1
        Caption = '.'
        Enabled = False
        ShortCut = 16472
        OnClick = miChangeColoursFrameClick
      end
      object miChangeColoursAll: TMenuItem
        Tag = 2
        Caption = '.'
        Enabled = False
        ShortCut = 16473
        OnClick = miChangeColoursFrameClick
      end
      object N29: TMenuItem
        Caption = '-'
      end
      object miCountColours: TMenuItem
        Caption = '.'
        Enabled = False
        object Currentframe1: TMenuItem
          Caption = '.'
          OnClick = Currentframe1Click
        end
        object Animation1: TMenuItem
          Caption = '.'
          OnClick = Animation1Click
        end
      end
    end
    object N34: TMenuItem
      Caption = '.'
      object miClearAllFramesGradient: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miClearAllFramesGradientClick
      end
      object N49: TMenuItem
        Caption = '-'
      end
      object miGradientFillFrame: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miGradientFillFrameClick
      end
      object N62: TMenuItem
        Caption = '-'
      end
      object miGradientLoad: TMenuItem
        Caption = '.'
      end
      object miGradientSave: TMenuItem
        Caption = '.'
        OnClick = miSaveGradientClick
      end
    end
    object Buffer1: TMenuItem
      Caption = '.'
      object miCopyCurrentTo: TMenuItem
        Caption = '.'
        Enabled = False
        object miMemory1: TMenuItem
          Caption = 'Memory #1'
          ShortCut = 16433
          OnClick = miMemory1Click
        end
        object miMemory2: TMenuItem
          Tag = 1
          Caption = 'Memory #2'
          ShortCut = 16434
          OnClick = miMemory1Click
        end
        object miMemory3: TMenuItem
          Tag = 2
          Caption = 'Memory #3'
          ShortCut = 16435
          OnClick = miMemory1Click
        end
        object miMemory4: TMenuItem
          Tag = 3
          Caption = 'Memory #4'
          ShortCut = 16436
          OnClick = miMemory1Click
        end
        object miMemory5: TMenuItem
          Tag = 4
          Caption = 'Memory #5'
          ShortCut = 16437
          OnClick = miMemory1Click
        end
        object miMemory6: TMenuItem
          Tag = 5
          Caption = 'Memory #6'
          ShortCut = 16438
          OnClick = miMemory1Click
        end
        object miMemory7: TMenuItem
          Tag = 6
          Caption = 'Memory #7'
          ShortCut = 16439
          OnClick = miMemory1Click
        end
        object miMemory8: TMenuItem
          Tag = 7
          Caption = 'Memory #8'
          ShortCut = 16440
          OnClick = miMemory1Click
        end
        object miMemory9: TMenuItem
          Tag = 8
          Caption = 'Memory #9'
          ShortCut = 16441
          OnClick = miMemory1Click
        end
        object miMemory10: TMenuItem
          Tag = 9
          Caption = 'Memory #10'
          ShortCut = 16432
          OnClick = miMemory1Click
        end
      end
      object miRestoreCurrentFrom: TMenuItem
        Caption = '.'
        Enabled = False
        object miMemoryR1: TMenuItem
          Caption = 'Memory #1'
          ShortCut = 32817
          OnClick = miMemoryR1Click
        end
        object miMemoryR2: TMenuItem
          Tag = 1
          Caption = 'Memory #2'
          ShortCut = 32818
          OnClick = miMemoryR1Click
        end
        object miMemoryR3: TMenuItem
          Tag = 2
          Caption = 'Memory #3'
          ShortCut = 32819
          OnClick = miMemoryR1Click
        end
        object miMemoryR4: TMenuItem
          Tag = 3
          Caption = 'Memory #4'
          ShortCut = 32820
          OnClick = miMemoryR1Click
        end
        object miMemoryR5: TMenuItem
          Tag = 4
          Caption = 'Memory #5'
          ShortCut = 32821
          OnClick = miMemoryR1Click
        end
        object miMemoryR6: TMenuItem
          Tag = 5
          Caption = 'Memory #6'
          ShortCut = 32822
          OnClick = miMemoryR1Click
        end
        object miMemoryR7: TMenuItem
          Tag = 6
          Caption = 'Memory #7'
          ShortCut = 32823
          OnClick = miMemoryR1Click
        end
        object miMemoryR8: TMenuItem
          Tag = 7
          Caption = 'Memory #8'
          ShortCut = 32824
          OnClick = miMemoryR1Click
        end
        object miMemoryR9: TMenuItem
          Tag = 8
          Caption = 'Memory #9'
          ShortCut = 32825
          OnClick = miMemoryR1Click
        end
        object miMemoryR10: TMenuItem
          Tag = 9
          Caption = 'Memory #10'
          ShortCut = 32816
          OnClick = miMemoryR1Click
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miExportUserMemories: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miExportUserMemoriesClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miClearAllUserMemories: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miClearAllUserMemoriesClick
      end
    end
    object ools1: TMenuItem
      Caption = '.'
      object miAutoSave: TMenuItem
        Caption = '.'
        RadioItem = True
        OnClick = miAutoSaveClick
      end
      object Autosaveinterval1: TMenuItem
        Caption = '.'
        object miAutosave2: TMenuItem
          Caption = '.'
          GroupIndex = 2
          RadioItem = True
          OnClick = miAutosave2Click
        end
        object miAutosave5: TMenuItem
          Tag = 1
          Caption = '.'
          GroupIndex = 2
          RadioItem = True
          OnClick = miAutosave2Click
        end
        object miAutosave10: TMenuItem
          Tag = 2
          Caption = '.'
          GroupIndex = 2
          RadioItem = True
          OnClick = miAutosave2Click
        end
      end
      object N32: TMenuItem
        Caption = '-'
      end
      object Openautosavefolder1: TMenuItem
        Caption = '.'
        OnClick = Openautosavefolder1Click
      end
      object N38: TMenuItem
        Caption = '-'
      end
      object miAutomate: TMenuItem
        Caption = '.'
        Enabled = False
        ShortCut = 117
        OnClick = miAutomateClick
      end
      object N27: TMenuItem
        Caption = '-'
      end
      object miOptimiseData: TMenuItem
        Caption = '.'
        Enabled = False
        OnClick = miOptimiseDataClick
      end
      object N52: TMenuItem
        Caption = '-'
      end
      object miFontViewer: TMenuItem
        Caption = '.'
        OnClick = miFontViewerClick
      end
    end
    object miHelp: TMenuItem
      Caption = '.'
      object Help1: TMenuItem
        Caption = '.'
        ShortCut = 112
        OnClick = Help1Click
      end
      object Showshortcutkeys1: TMenuItem
        Caption = '.'
        OnClick = Showshortcutkeys1Click
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object miLanguage: TMenuItem
        Caption = '.'
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Examples1: TMenuItem
        Caption = '.'
        OnClick = Examples1Click
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object Checkforupdates1: TMenuItem
        Caption = '.'
        OnClick = Checkforupdates1Click
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object witter1: TMenuItem
        Caption = '.'
        ImageIndex = 6
        Visible = False
        OnClick = witter1Click
      end
      object Website1: TMenuItem
        Caption = '.'
        OnClick = Website1Click
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object miAbout: TMenuItem
        Caption = '.'
        ImageIndex = 4
        OnClick = miAboutClick
      end
    end
    object miDebug: TMenuItem
      Caption = 'Debug'
      Visible = False
      object RenderMode1: TMenuItem
        Caption = 'Render Mode...'
        OnClick = RenderMode1Click
      end
      object MonoColours1: TMenuItem
        Caption = 'Mono Colours...'
        OnClick = MonoColours1Click
      end
      object CurrentLayerFrame1: TMenuItem
        Caption = 'Current Layer/Frame'
        OnClick = CurrentLayerFrame1Click
      end
      object Controls1: TMenuItem
        Caption = 'Controls'
        OnClick = Controls1Click
      end
      object N63: TMenuItem
        Caption = '-'
      end
      object PaintBox1: TMenuItem
        Caption = 'PaintBox'
        OnClick = PaintBox1Click
      end
      object Preview2: TMenuItem
        Caption = 'Preview'
        OnClick = Preview2Click
      end
      object N64: TMenuItem
        Caption = '-'
      end
      object miDrawTestPattern: TMenuItem
        Caption = 'Draw test pattern'
        OnClick = miDrawTestPatternClick
      end
    end
  end
  object colorDialog: TColorDialog
    Options = [cdFullOpen]
    Left = 784
    Top = 208
  end
  object timerAnimate: TTimer
    Tag = 1
    Enabled = False
    Interval = 500
    OnTimer = timerAnimateTimer
    Left = 1304
    Top = 56
  end
  object ilMain: TImageList
    BkColor = clFuchsia
    Left = 1312
    Bitmap = {
      494C010108009802040010001000FF00FF00FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00CA8A6100C3845800D38B6800E18F7000DC8D6C00DA8B
      6D00D78A6E00CD8B6C00AB6D4400A65F2E00FF00FF00FF00FF00FF00FF00FDFE
      FD009FC2A200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00AAAAAA00A7A7A700A3A3
      A300A0A0A0009D9D9D009A9A9A00989898009595950092929200909090008E8E
      8E008C8C8C008B8B8B0089898900FF00FF00FF00FF00369DD9003199D8002C94
      D7002890D600238CD5001E88D4001A84D3001580D200117CD1000E79D1000A76
      D0000773CF000470CF00016ECE00FF00FF004E4E4E002D2D2D0050505000FF00
      FF00FF00FF00FF00FF00C6835500EFCEBA00DDFFFF0087EEC700A2F4D700A2F6
      D7008CEEC700E0FFFF00DDA28500AB6A3E00FF00FF00FF00FF00FF00FF0086B6
      8B005A985F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00AEAEAE00ECECEC00EDED
      ED00F0F0F000F5F5F500F8F8F800F9F9F900F8F8F800F3F3F300EBEBEB00E7E7
      E700E5E5E500E5E5E5008B8B8B00FF00FF00FF00FF003DA3DA00BCEBFA00BCEB
      FC00BFEEFE00C6F4FF00CEF8FF00D3FAFF00D0F8FF00C7F2FF00BAE9FC00B3E4
      F900B0E2F800B0E2F8000571CF00FF00FF0038383800B0B0B000353535005757
      57005757570057575700C37F5100EFB69A00EAF3E80051BF84006FC9980071C9
      990054BF8400E4F4E900DD9C7B00AA693A00FF00FF00FF00FF0094C198005AA4
      620058A15E0037833E00347E3A00317A36002E7533003C7D4000659668009FBD
      A100E6EEE600FF00FF00FF00FF00FF00FF00FF00FF00B2B2B200EEEEEE00D4D4
      D400BCBCBC00C4C4C400CACACA00CDCDCD00CBCBCB00C1C1C100B3B3B300AAAA
      AA00C2C2C200E6E6E6008E8E8E00FF00FF00FF00FF0043A8DB00BFECFB0059CF
      F50041B0EC004EBAEF005AC2EF0060C6EF005CC4EF004CB6EF0037A5E6002A9A
      E10038B8EE00B1E3F8000975D000FF00FF003A3A3A0062626200383838006FDF
      9D0078E0A30074E09E00C4815400EAB69700F3F3EA00EDF1E600EFF1E600EFF0
      E600EDF1E500F3F5ED00D59C7900B0704400FF00FF00A1CCA50062AC6A0085C8
      8D0085C78B0082C688007FC486007CC2820079C1800071B978005FA865004991
      4E0069986C00CEDDCF00FF00FF00FF00FF00FF00FF00B5B5B500EFEFEF00D8D8
      D800DFDFDF00E7E7E700EEEEEE00F1F1F100EEEEEE00E3E3E300D3D3D300C6C6
      C600C3C3C300E6E6E60090909000FF00FF00FF00FF0049ADDC00C1EEFB005FD3
      F7006CDBFC007FE5FF008FEDFF0097F2FF0093EDFF007CDFFF005BCCF80046BE
      EF003CBAEE00B3E3F9000E79D100FF00FF0063636300ACACAC00575757004FCB
      5C0057D77A0042D16A00C98B6100E6B59200E2A78100E1A78100DEA37D00DCA1
      7B00DB9F7900D99E7700D49A7300BB7E5700FF00FF009FCCA40066B06E008ACA
      920089CA900086C88D0083C68A0080C587007EC384007BC281006DBB740076BE
      7C0059A05D0069996C00E6EEE600FF00FF00FF00FF00BABABA00F0F0F000DADA
      DA00C0C0C000C6C6C600ECECEC00A7A7A7008B8B8B00E4E4E400B7B7B700AEAE
      AE00C6C6C600E8E8E80093939300FF00FF00FF00FF004EB2DD00C3EFFB0065D6
      F8004CB6EC005ABDEF0095EBFF003097DD004D82AB0084E1FF0041A9E900329F
      E10042BEEF00B4E5F900137ED200FF00FF005C5C5C006D6D6D005F5F5F004FC4
      45004BBA2C00D8BD6000CA8D6500EAB89900DDA57E00DDA68000DBA37C00D9A0
      7A00D9A07900D89F7800D89E7800BF845D00FF00FF00FF00FF0098C99E0067B1
      6F0064AD6B0043944B00408F47003C8A430039854000549D5A0074BA7A0079C1
      7F0077BF7D004A914F00A0BEA200FF00FF00FF00FF00BDBDBD00F1F1F100DCDC
      DC00E5E5E500EAEAEA00EBEBEB00ADADAD0093939300E5E5E500D6D6D600CCCC
      CC00C8C8C800E9E9E90097979700FF00FF00FF00FF0053B7DE00C6F0FC006AD9
      F8007CE2FD0090E8FF0099E9FF00329FDF00548BB2008AE2FF006AD0F90050C5
      F10046C1F000B6E7F9001883D300FF00FF006565650089898900656565007FBF
      3600DDC56900FFC27000C8885D00EFBFA100FDFCFA00FEFCFB00FEFDFD00FEFD
      FC00FDFBFA00FDFCFB00DDA88500C17F5300FF00FF00FF00FF00FF00FF0091C6
      96006BAF7100FF00FF00FF00FF00FF00FF00FF00FF0091BB94004D8F520075BB
      7B0070BD770063AB690066986900FF00FF00FF00FF00C0C0C000F2F2F200DFDF
      DF00C4C4C400C6C6C600EAEAEA00B2B2B200B1B1B100E5E5E500B9B9B900B1B1
      B100CACACA00EAEAEA009A9A9A00FF00FF00FF00FF0058BBDF00C7F1FC006FDC
      F90056BBED0061BDEF009BE7FF0035A6E2004BA4E10090E2FF0049ADE90038A4
      E30049C4F000B8E8F9001E88D400FF00FF006E6E6E00A4A4A4006C6C6C00F8C6
      6E00FFC87700FFC57200C7865B00EFC09E00FFFFFF00CC936E00FFFFFF00FFFF
      FF00FFFBF700FFF8F100E4AF8C00C78A6100FF00FF00FF00FF00FF00FF00FDFE
      FD00AAD3AE00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0091BC940061A8
      67007BC2820076BC7C003E804300FF00FF00FF00FF00C3C3C300F3F3F300E1E1
      E100E8E8E800E9E9E900E8E8E800EFEFEF00EFEFEF00E7E7E700DADADA00D2D2
      D200CDCDCD00EBEBEB009E9E9E00FF00FF00FF00FF005CBFE000C8F3FC0075DF
      F90089E6FD0095E7FF009AE5FF00AAEEFF00A8EDFF0099E3FF0074D5F90059CC
      F3004FC8F100BBE9FA00248DD500FF00FF006969690071717100686868005757
      57005757570057575700CC8D6500F3CDB000FFFFFF00E3C7B300FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00EABFA100C9896000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0093BF970064AB
      6B007FC4860079BE810042854700FF00FF00FF00FF00C6C6C600F3F3F300F3F3
      F300F6F6F600F7F7F700F6F6F600F9F9F900F9F9F900F6F6F600A6A6A600A6A6
      A600A6A6A600ECECEC00A2A2A200FF00FF00FF00FF0060C2E100C9F3FC00CBF3
      FD00D4F6FE00D7F6FF00D8F4FF00E0F8FF00DFF8FF00DAF5FF00CDF1FC00C2ED
      FA00BDEBFA00BDEBFA002B93D600FF00FF0077777700AAAAAA007474740072E2
      9E007CE3A40078E3A000D4976E00D49E7B00D0987100D6A48200CD8E6800CD90
      6900D09A7500D1997300C88B6200EEDCD000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0097C59C00579E5F0081C3
      88007DC485006EB375006EA37200FF00FF00FF00FF00C6C6C600939393009191
      91008E8E8E00BEBEBE00BEBEBE00BCBCBC00B9B9B900B5B5B500C8C8C800DDDD
      DD00BFBFBF00A7A7A700A7A7A700FF00FF00FF00FF0061C3E10088A0A8009191
      91008E8E8E005AB9DC0055B8DF0051B5DE004DB1DD0049ADDC0046A8D7007878
      780076767600657E8D003199D800FF00FF007777770097979700777777004FCB
      5C0057D77A0042D16A0070C75E00B6B85000CBAE3F005B5B5B007C7C7C004B4B
      4B00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005FBA6A005CB6
      670059B3640056AE600053AA5D0050A659004DA1560068B1700088C890008DCC
      95008BCB92005DA56400A8C9AB00FF00FF00FF00FF00FF00FF0097979700B9B9
      B9009A9A9A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00B1B1B100C6C6
      C60094949400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF007D7D
      7D00ABABAB0096969600FF00FF00FF00FF0077777700858585007A7A7A004FC4
      45004BBA2C00D8BD6000FFBA6200FFB96500DBBB7D005E5E5E00656565004B4B
      4B00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0062BE6D00A5DA
      AE00A2D8AC00A1D8AA009ED6A7009CD5A50099D4A20097D29F008CCD950091CF
      990073B87B007BB28000E9F2EA00FF00FF00FF00FF00FF00FF0098989800BCBC
      BC0099999900FF00FF00FF00FF00FF00FF00FF00FF0098989800939393009A9A
      9A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00BCBCBC00C4C4
      C400A1A1A100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF008989
      8900A9A9A900A4A4A400FF00FF00FF00FF0083838300AFAFAF007C7C7C007FBF
      3600DDC56900FFC27000FFBF6700AECBAC0068E0F90060606000ACACAC005C5C
      5C00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0064C17000A6DB
      B000A6DAAF00A3D9AD00A2D8AB009FD7A8009CD5A50094D09D0083C58C006CB4
      740080B98600D5E7D700FF00FF00FF00FF00FF00FF00FF00FF00A3A3A300C0C0
      C000ACACAC00FF00FF00FF00FF00FF00FF00FF00FF008F8F8F00A2A2A2008D8D
      8D00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C7C7C700BBBB
      BB00BFBFBF00A6A6A600FF00FF00FF00FF00FF00FF00FF00FF0093939300A8A8
      A8009E9E9E00C3C3C300FF00FF00FF00FF006C6C6C007474740068686800E6BC
      7100EDBE7900ECBB7300ECB96D0084CDCF0084D8EB004B4B4B00686868003D3D
      3D00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0066C4720064C2
      700062BF6E0060BC6B005DB868005BB5650059B2630063B36C0083C18900B2D7
      B600EBF4EC00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CBCBCB00B7B7
      B700C7C7C700AAAAAA00FF00FF00FF00FF009F9F9F00B4B4B40098989800B7B7
      B700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00B4B4
      B400C3C3C300BEBEBE00A1A1A100969696009393930097979700AEAEAE00AEAE
      AE00A6A6A600FF00FF00FF00FF00FF00FF0073737300A6A6A600696969005757
      570057575700575757005757570057575700575757004D4D4D009A9A9A006262
      6200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00D9D9
      D900B0B0B000C8C8C800CACACA00CACACA00BEBEBE00A4A4A400CDCDCD00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C8C8
      C800B2B2B200BCBCBC00CACACA00CCCCCC00CACACA00C2C2C200ADADAD009F9F
      9F00C1C1C100FF00FF00FF00FF00FF00FF00898989007676760080808000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF006E6E6E00515151006666
      6600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00CCCCCC00BEBEBE00A2A2A2009E9E9E00B6B6B600CDCDCD00F812F800FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00D0D0D000BABABA00B1B1B100AEAEAE00B3B3B300C9C9C900FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CD957000BD734200B768
      3500B5683500B4673400B2663400B0653300AE643300AC633200AA623200A961
      3200A8603100A7613200AB693C00BC866100FF00FF0076B2E6003E91DB00348C
      D900348CD900348CD900348CD900348CD900348CD900348CD900348CD900348B
      D900398FDA0085B9E900FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00D4DFE6004F575C004546460040424200464D5000C9D6DF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00D4DFE6004F575C004546460040424200464D5000C9D6DF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00C37D4F00EBC6AD00EAC5AD00FEFB
      F800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFB
      F800FEFBF800C89A7C00C7987900AD6B40004799DD00DEF1FA00A8DDF4009EDB
      F40096DAF3008ED8F30086D7F3007FD4F20079D3F20072D2F1006CD0F10069CF
      F100C2EAF8003F95DB00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00838E9500BCBCBB00EBEAEA00CDCCCC00A3A19F005A656D00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00838E9500BCBCBB00EBEAEA00CDCCCC00A3A19F005A656D00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00BA6C3800EDCAB300E0A27A00FEFA
      F70062C0880062C0880062C0880062C0880062C0880062C0880062C0880062C0
      8800FDF9F600CA8D6500C99B7C00A76132003B97DB00EFFAFE00A1E9F90091E5
      F80081E1F70072DEF60063DAF50054D7F40047D3F30039D0F2002ECDF10026CB
      F000CAF2FB003B97DB00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0079868C00A6A5A200A8A2A2009D999800948F8B0052596000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0079868C00A6A5A200A8A2A2009D999800948F8B0052596000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00BB6C3800EECCB600E1A27A00FEFA
      F700BFDCC200BFDCC200BFDCC200BFDCC200BFDCC200BFDCC200BFDCC200BFDC
      C200FDF9F600CD906800CC9E8100A86132003C9DDB00F2FAFD00B3EDFA00A4E9
      F90095E6F80085E2F70081E1F7007AE0F7007CE0F70062DAF50054D6F30047D3
      F200E8F9FD003594DA00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00F3EDE3006683970098A19A006991860064887B00817F71002E445800F4EF
      E600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00DBE5EC00668397007A95A3003A8A9800357F8C00606E76002E445800DFE8
      EF00FF00FF00FF00FF00FF00FF00FF00FF00BB6B3800EFCEB800E1A27900FEFA
      F70062C0880062C0880062C0880062C0880062C0880062C0880062C0880062C0
      8800FDF9F600CF936A00CEA38400AA6132003BA3DB00F6FCFE00C8F2FC00B9EF
      FB0091E5F8008CE4F8008AE3F80082E1F70081E2F8006DDDF60061DAF50057D7
      F400E7F8FD003594DA00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00F8F4
      EE00D8C7A600CFBA9300D4C19E00D5C2A000D4C19D00D7C5A400C3A87600C9B1
      8500F4EFE500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00E3F1
      FB0070AFE100469DE6004BBEF70047E6FD0041E5FD0051C3FB00167CDE003D88
      D300D5E8F700FF00FF00FF00FF00FF00FF00BA6A3600EFD0BB00E2A27A00FEFB
      F800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFB
      F800FEFBF800D3966D00D2A78A00AB6232003BA8DB00FEFFFF00F8FDFF00F6FD
      FF00F6FDFF00E8FAFE00AFECFA008EE4F80089E4F8007DE0F70072DDF60068DB
      F500E9F9FD003594DA00FFFFFF00FFFFFF00FF00FF00FF00FF00F9F6F000D7C6
      A600E8DECB00D9C8A800DDCEB300DDCEB200DDCEB200DDCEB200D2BE9A00E5D9
      C400C6AC7D00F1EADE00FF00FF00FF00FF00FF00FF00FF00FF00E6F3FB0069B3
      E900A6D3F30065AEF00074E1F60073E1F60072E0F60071E0F6004CA3EC009CC3
      EF002C81D700C9E0F500FF00FF00FF00FF00BB6A3600F0D2BE00E2A37A00E2A3
      7A00E1A37A00E2A37B00E1A37B00E0A17800DE9F7700DD9F7600DC9D7400D99B
      7200D8997100D6997000D5AB8E00AD63330039ADDB00E8F6FB007EC5EA004AA3
      DF00459FDE004CA3DF00EDEFED00ECEBE900E7E4E200E7E5E200E7E3E000E7E5
      E200F2E9E3003594DA00C69F8500D7BFAF00FF00FF00FEFEFE00DED0B400E8DE
      CB00F7F3ED0091AEB300DDCEB300DBCBAE00D8C7A700DDCEB2007FA1A700F0EA
      DE00E9DFCD00C6AC7E00F9F7F200FF00FF00FF00FF00FDFEFF007FC0EC00A5D4
      F300DCFAFE0038A1EB0074E1F6006AE4F6005DE2F50072E0F6001691E800C0F5
      FD00ACCEF1002D83D700EAF3FB00FF00FF00BB6A3600F2D5C200E3A37A00E3A3
      7A00E2A37B00E2A37B00E2A47B00E1A27900E0A17800DEA07700DE9E7500DC9D
      7400DA9B7300D99B7300DAB09500AF64330040AEDC00F1FAFD0094DEF50093DC
      F40081D5F20060C0E9005696C5003594DA003594DA003594DA003594DA003594
      DA003594DA003594DA00F2EEEA00C89F8600FF00FF00ECE4D500E1D4BB00FBF9
      F600DFD1B6008DA9B000DDCEB300DDCEB300DACAAC00DDCEB2007799A100D2BE
      9A00F7F3ED00D9C8A800DBCBAD00FF00FF00FF00FF00B7DDF5008BC8EF00ECFC
      FE0077E1F7002F99EA0075E1F60074E1F60068DEF50073E1F6000986E60046D5
      F300DCFEFE006FAAE50079B3E600FF00FF00BB6A3600F2D8C500E3A47B00E3A3
      7A00E3A47A00E2A47B00E2A37B00E1A37B00E1A27900DFA07700DE9F7600DD9E
      7400DB9C7200DC9D7400DDB59A00B165340041B4DC00F7FCFE008EE4F80091DE
      F5009FE0F500ACE1F600C2957900F6F1ED0047A27000429D6B003C9766003691
      6000318B5A002C865500F7EDE600C59A7F00FF00FF00DECFB400F2ECE100F5F0
      E800DFD1B700E2D5BE00A7BDC300ACC0C500A9BFC4009CB4BB00E0D3BA00D7C5
      A500E4D8C100F2ECE100CAB28600FF00FF00FF00FF007FC5EE00C9E9F900D4F9
      FD007CE3F70086E5F80060B1EF0068B5EF0063B4EF004CA6EC0082E4F70059DC
      F5008AEBFA00CBE2F700398FDA00FF00FF00BB6B3600F4D9C700E6A67D00C88C
      6400C98D6500C98E6700CB926C00CB926D00CA906900C88C6500C88C6400C88C
      6400C88C6400DA9C7400E1BA9F00B36634003CB5DB00FDFEFE00FEFFFF00FEFE
      FF00FDFEFF00FEFFFF00C3967900F7F1EC00DCBE9E00DEBD9B004F9ECC003178
      A100DAB79100D6B28D00F6EEE600C59B8000FF00FF00DECFB400F7F3ED00F2EC
      E200F3EEE400F5F1E900F4EFE600F3EEE500F2ECE200F1EBE000EFE8DA00E5DA
      C500DDCEB200F8F5EF00C7AE8100FF00FF00FF00FF007EC6EE00DFF6FD00C8F5
      FC00CDF6FC00D6F7FD00D3F4FC00CFF2FC00CAF1FB00C4F0FC00BAF2FB0096EA
      F80072E5F700E2F4FD003289D800FF00FF00BB6C3700F4DCC900E7A77D00F9EC
      E100F9ECE100F9EDE300FCF4EE00FDFAF700FDF7F300FAEDE500F7E7DB00F7E5
      D900F6E5D800DEA07700E4BEA400B467340059C2E00061C3E20063C4E30063C4
      E30063C4E30062C4E300BD987E00F7F3EE00DFC7AE0065A66C0039AD6A002FA8
      5E004E974E00D2AE8900F6EEE700C7A08600FF00FF00E3D8C100F3EEE500F6F2
      EC00F7F3ED00FBF9F600FBF9F600FBF9F600FBF9F600FAF8F400F8F5EF00EEE7
      D900EAE1D000F2ECE100CCB48B00FF00FF00FF00FF0095D2F200D2EFFB00DBF9
      FE00DFF9FD00ECFBFE00EEFCFE00EFFCFE00EFFCFE00EBFBFE00E0F9FE00B8F1
      FB00A8F1FB00CBE5F8003E95DD00FF00FF00BD6E3A00F5DDCC00E7A87E00FAF0
      E800FAF0E800C98D6600FAF0E900FDF8F300FEFAF800FCF4EF00F9E9DF00F7E7
      DB00F7E5D900E0A27800E7C2A900B6683500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C79B8000F7F4EF00E1CFBB0040C08B004FC7990048C2
      8D0031AA6300CBAA8900F6EEE700C7A08600FF00FF00F1EBE000EDE5D600F9F7
      F200F6F2EC00F9F7F200FBFAF700FEFEFD00FEFEFD00FBF9F600F4EFE600EDE5
      D600FBF9F600D9C8A800E0D1B800FF00FF00FF00FF00C8E9F900B4E3F800E5FA
      FE00DBF8FD00E4FAFE00F0FCFE00F9FEFF00F9FEFF00EFFCFE00D2F6FD00B4F1
      FB00EDFDFF006BB3EA0088C2EC00FF00FF00C0744200F6DFD000E8A87E00FCF6
      F100FCF6F100C88C6400FAF1E900FBF4EE00FDFAF700FDF9F600FAF0E800F8E8
      DD00F7E6DB00E1A37A00EFD5C300B76A3600FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C3967A00F7F5F000E7D9C80048C89C005ACFA80053CA
      9D0039B47500D2B79A00F5EFE700C49B8000FF00FF00FEFEFD00ECE3D400F0EA
      DE00F8F5EF00F7F3ED00F5F1E900F3EEE500F2ECE200F2ECE100F5F1E900FAF8
      F400E3D7C000D1BD9800FCFAF800FF00FF00FF00FF00FCFEFF00B5E2F600C3EB
      FA00E2F9FD00E0F9FD00D5F7FD00CFF6FD00C9F4FC00C7F4FC00D6F9FD00EBFA
      FE0090CAF20050A9E600F3F9FD00FF00FF00C6825500F6DFD100E9AA8000FEFA
      F600FDFAF600C88C6400FBF3EE00FBF1EA00FCF6F200FEFBF800FCF6F100F9EC
      E200F8E7DB00EED0BA00ECD0BD00BD744300FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C79B8000F7F5F400E9DBCC005EBC90004ACA9F0045C4
      950058AE7700DCC8B100F7F0E900C8A08600FF00FF00FF00FF00FDFCFB00F0E9
      DD00EDE5D600F2ECE100F5F1E900F8F5EF00F8F5F000F5F1E900EDE5D600D9C8
      A900DAC9AA00F9F6F100FF00FF00FF00FF00FF00FF00FF00FF00F7FCFE00C4E7
      F900B7E4F800C7ECFB00D7F3FC00E1F7FD00E2F8FE00D8F0FC00B6DFF8006BBB
      ED006CB9EB00E8F4FC00FF00FF00FF00FF00D6A58500F6E0D100F7E0D100FEFB
      F800FEFBF700FDF9F600FCF5F000FAF0EA00FBF2ED00FDF9F600FDFAF700FBF1
      EB00F8E9DF00ECD1BE00CD926A00E2C5B100FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00C8A79100EBEBEA00F7F5F400F7F5F400F7F5F400F7F5
      F400F7F5F400F7F5F400F2EEE800C69F8500FF00FF00FF00FF00FF00FF00FF00
      FF00F8F5EF00EFE8DC00EAE0CE00E4D8C200E0D2B900DED0B600E1D3BA00EDE5
      D700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00E2F5FB00C1E6F800A8DDF60094D2F10085CCF00080C8F00087CBF000BBE1
      F600FF00FF00FF00FF00FF00FF00FF00FF00E1BDA600D9AB8D00C9895E00C075
      4300BD6E3A00BB6C3700BB6B3600BB6A3600BB6A3600BC6C3900BD6E3B00BB6D
      3A00BF744400C98D6500E7CEBC00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00D8C3B500C8A69200C69A7E00C79B7F00C3967800C79A
      7F00C79A7F00C79A7F00C59B8000D7BFAF00424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FC00E7FF800180011C00E7FF80018001
      0000C00780018001000080038001800100008001800180010000C00180018001
      0000E781800180010000E7C1800180010000FFC1800180010000FF8180018001
      000FC001C7FFC7E3000FC001C78FC7E3000FC003C78FC3C3000FC007C30FE007
      000FFFFFE01FE0071F8FFFFFF01FF81FFFFFFFFF80008000F81FF81F00000000
      F81FF81F00000000F81FF81F00000000F00FF00F00000000E007E00700000000
      C003C00300000000800180010000000080018001000000008001800100000000
      8001800100000000800180010000000080018001000000008001800100000000
      C003C00300000000F00FF00F0001000000000000000000000000000000000000
      000000000000}
  end
  object sdMain: TSaveDialog
    DefaultExt = '.leds'
    Filter = 'C-style header file (.h)|*.h|Include file (.inc)|*.inc'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 736
    Top = 160
  end
  object odMain: TOpenDialog
    DefaultExt = '.leds'
    Filter = 'Matrix Builder files (*.leds)|*.leds'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 736
    Top = 208
  end
  object ilMenu: TImageList
    Left = 1352
    Bitmap = {
      494C01010A000D00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000033333300333333002E5D
      7000333333003333330033333300333333003333330033333300333333003333
      3300244C67003235370033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E2C0AA00CC8D
      6600BC6B3600BC6B3600BC6A3600BC6A3600BB6A3500BB6A3500BB693500BD6E
      3B00CA8B6300E3C2AE00000000000000000034333400333333003333330029A7
      D70028A4D5002B728F0033333300333333003333330033333300265977001381
      C3000C81CB003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C57C4D00F8F1
      EA00F8F3ED00F8F3ED00F8F3ED00F8F3ED00FAF3EB00FAF2EA00F8F3ED00F8F3
      ED00F8F3ED00C37A4D0000000000000000003333330033333300333333002CA0
      CB0050CBEF0039B7E50025A4D800297B9E00297191001E96D1002AA3DD0039AE
      E5001285C9003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6804600F7F0
      E600F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3
      ED00F8F3ED00C1743C0000000000000000003333330033333300333333002E96
      BA004DCAEE0054CEF10050C8EF0040BAE80039B5E50046BDEB0044BAEB003EB3
      E8001986C3003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7844800F8F1
      E800F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3
      ED00F8F3ED00C37A410000000000000000003333330033333300333333003086
      A30049C8EC0049CCF10031C3ED0042C4EE0046C3ED002CB6EA0026B1E8003CB4
      E7002084B7003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C7864B00F8F2
      EB00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3
      ED00F8F3ED00C780450000000000000000003333330033333300333333003274
      890044C6EA0057D2F20027C2ED0023BDEC001FB7EA001BB3E90036B9EA003AB3
      E600267DA4003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C8884D00F9F3
      EC00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3
      ED00F8F3ED00C884480000000000000000003333330033333300333A3B0034B0
      D50059D5F20046D0F20029C4EE0025C0ED0022BBEB001DB6E90021B4E90046BD
      EB0024A0D500313F460033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C88C4F00F9F4
      ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3
      ED00F8F3ED00C8864B000000000000000000333333003336370037B1D2004ACE
      ED005EDBF50030CDF1002CC8EF0027C3EE0024BEEC0020B8EA001CB4E90049C0
      EC0037B2E400259FD400313D4200333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C88C5000F8F3
      ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3
      ED00FAF4EF00C8874C0000000000000000003333330039B2D0004CD2EE0063E1
      F60041D6F40032CFF2002ECBF0002AC6EF0026C1ED0022BCEB001FB7EA0020B4
      E90047BEEC0036B2E400259ED000323B3F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C88D5100F8F3
      ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3
      ED00F8F4F000C6864C0000000000000000003BB1CB004DD5EF0058DCF2005EDE
      F50060DEF5005FDCF60048D4F3002DC9EF0029C4EE0035C4ED0050C9EF004CC4
      EE0045BEEB003DB7E70033B0E200269DCE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C88D5100F8F3
      ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F7F2EC00FBF7
      F300F5EFE900C38048000000000000000000343A3C00376D79003995AA003AAF
      CC003ABFE10049CEED005BD9F40047D3F2003ACBF00055CFF1003EBFE8002EAF
      DB002E9FC8002E87A8002F677D00324046000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C88D5200F9F5
      F100F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00FCE6CD00FAE5
      C900E2B68400D5A8840000000000000000003333330033333300333333003333
      330033333300377C8E0049CEED005DD9F40059D6F30043C5EA00328BA6003333
      3300333333003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CA925A00FAF6
      F200F8F3ED00F8F3ED00F8F3ED00F8F3ED00F8F3ED00FFFBF800F6D8B400E1B0
      7D00DC9669000000000000000000000000003333330033333300333333003333
      330033333300333333003ABCDC0056D6F20059D6F30036BCE200333637003333
      3300333333003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D2A27400F8F3
      ED00F8F3ED00F8F3ED00F8F3ED00F8F2EC00F7F2EC00F2E6D700E2B27D00DC98
      6B00000000000000000000000000000000003333330033333300333333003333
      330033333300333333003666710048CEED0048CDED00367B8D00333333003333
      3300333333003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E8CEB900D7AA
      7C00CA905500CA905500CA915500CB905500C98F5500CF9D6900DDB190000000
      0000000000000000000000000000000000003333330033333300333333003333
      33003333330033333300333333003BB4CF003ABEDE0033333300333333003333
      3300333333003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003333330033333300333333003333
      33003333330033333300333333003551580036636E0033333300333333003333
      3300333333003333330033333300333333000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B6B6B600DDDD
      DD00CDCDCD00DEDEDE00DFDFDF00DFDFDF00DFDFDF00DDDDDD00D5D5D500C1C1
      C100989898005757570000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000A822A00037B
      1E00DEEEE1000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007F7F7F00E5E5
      E500F0F0F000F7F7F700FBFBFB00FBFBFB00FBFBFB00EFEFEF00E6E6E600F2F2
      F200F3F3F300CCCCCC005B5B5B00000000004E4E4E002D2D2D00505050000000
      000000000000000000000000000000000000000000002D2D2D00017E2A0043A1
      5F001B893700D8EBDC0000000000000000000000000000000000EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB53000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E3E3E300B2B2B200AEAEAE00DFDFDF00000000000000
      000000000000000000000000000000000000000000000000000000000000C2C2
      C200F4F4F400F7F7F700FCFCFC00FBFBFB00FBFBFB00D4D4D4006E6E6E007F7F
      7F00CECECE00F8F8F800000000000000000038383800B0B0B000353535005757
      57005757570057575700219751001B914900158F43000F8B3B003A9F5E0080C1
      960046A3620018893400D5E9D90000000000EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EEEEEE00BEBEBE009A9A9A00CDCDCD00BDBDBD00A1A1A100B8B8B800EAEA
      EA0000000000000000000000000000000000000000000000000005050500C4C4
      C400F6F6F600FBFBFB00FBFBFB00FCFCFC00F9F9F900B5B5B500000000000000
      00008F8F8F00F5F5F500AEAEAE00000000003A3A3A00626262003838380091DB
      99009EDB9E00A1DA9900299B5B0090CAA9008DC8A5008AC6A10088C59E006AB6
      850082C2970048A5660015883300CFE6D400EEAB5300EEA35B00EEA95500EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB53000000
      0000000000000000000000000000000000000000000000000000F7F7F700CCCC
      CC00A3A3A300C5C5C500EBEBEB00E8E8E800CCCCCC00CDCDCD00B3B3B3009A9A
      9A00C4C4C400F4F4F400000000000000000000000000000000005C5C5C00E4E4
      E400FBFBFB00FBFBFB00FBFBFB00FBFBFB00E1E1E10086868600000000000000
      0000A4A4A400F7F7F700919191000000000063636300ACACAC005757570075C5
      5A0085D076007DC86500319F630094CDAD006FBA8E006BB8890066B6850061B3
      800067B5820083C298003CA05C0003812800EFA35B000000000000000000EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      530000000000000000000000000000000000FDFDFD00DADADA00AEAEAE00C1C1
      C100E8E8E800EBEBEB00DEDEDE00D6D6D600BBBBBB00BBBBBB00CBCBCB00C7C7
      C700ABABAB00A1A1A100D1D1D100FBFBFB00000000000000000085858500F7F7
      F700FCFCFC00FBFBFB00FBFBFB00EEEEEE00B0B0B0002B2B2B00000000006E6E
      6E00E2E2E200E3E3E30061616100000000005C5C5C006D6D6D005F5F5F006FBE
      460070B13000E3BC5F0037A36B0096CEB00094CDAD0091CBAA0090CBA80074BC
      90008AC7A10046A5680009883700F0F8F3000000000000000000EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300000000000000000000000000BDBDBD00BEBEBE00E3E3E300F1F1
      F100E7E7E700DDDDDD00DADADA00D6D6D600BEBEBE00C1C1C100C1C1C100BBBB
      BB00CBCBCB00C0C0C000A6A6A600AEAEAE0000000000000000008C8C8C00F4F4
      F400FBFBFB00FBFBFB00E5E5E500A6A6A600363636000000000063636300D6D6
      D600F5F5F5009F9F9F00010101000000000065656500898989006565650092BA
      3900E5C36900FFC270003DA56F0038A36D0034A268002F9C600055AF7C0091CB
      AA004FAB740019904600F4FAF600000000000000000000000000EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB53000000000000000000A8A8A800F5F5F500F2F2F200E7E7
      E700E4E4E400DFDFDF00DBDBDB00D6D6D600C0C0C000C2C2C200C2C2C200C2C2
      C200BDBDBD00BEBEBE00C9C9C90098989800000000000000000066666600DFDF
      DF00FCFCFC00ECECEC008B8B8B000F0F0F000000000063636300DDDDDD00F6F6
      F600B4B4B4002525250000000000000000006E6E6E00A4A4A4006C6C6C00F9C5
      6F00FFC87800FFC57200FFC36D00B7D8C300BAE4DE004E4E4E00359F65005AB3
      810028985700F7FBF900000000000000000000000000EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB53000000000000000000AEAEAE00F3F3F300F0F0F000ECEC
      EC00EAEAEA00E4E4E400DADADA00D8D8D800C4C4C400BBBBBB00BEBEBE00C2C2
      C200C3C3C300C0C0C000CBCBCB009E9E9E00000000000000000016161600CFCF
      CF00FBFBFB00CDCDCD000D0D0D00000000002E2E2E00CECECE00F3F3F300A5A5
      A500252525000000000000000000000000006969690071717100686868005757
      570057575700575757005757570057575700575757003232320038A06A00319F
      6500F8FCF900000000000000000000000000EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB53000000000000000000AFAFAF00F3F3F300F1F1F100EEEE
      EE00E1E1E100C7C7C700CBCBCB00D1D1D100D1D1D100CCCCCC00B1B1B100BBBB
      BB00C0C0C000C0C0C000CECECE009F9F9F00000000000000000005050500D3D3
      D300F9F9F900A7A7A700000000000000000081818100F0F0F000B8B8B8001111
      11000000000000000000000000000000000077777700AAAAAA007474740072E2
      9E007CE3A40078E3A00063E095005AE1950055E2930056565600949494004B4B
      4B0000000000000000000000000000000000EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB530000000000B0B0B000F3F3F300E9E9E900DDDD
      DD00DEDEDE00C2C2C200DADADA00FDFDFD00FDFDFD00DADADA00C3C3C300D7D7
      D700BBBBBB00BBBBBB00CFCFCF00A0A0A000000000001818180097979700E8E8
      E800F8F8F800C3C3C300646464000000000000000000F6F6F6008C8C8C000000
      0000000000000000000000000000000000007777770097979700777777004FCB
      5C0057D77A0042D16A0070C75E00B6B85000CBAE3F005B5B5B007C7C7C004B4B
      4B000000000000000000000000000000000000000000EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB530000000000ADADAD00D8D8D800CDCDCD00CECE
      CE00D1D1D100ACACAC00FFFFFF00FFFFFF00FFFFFF00FFFFFF00ACACAC00D1D1
      D100CCCCCC00C1C1C100BEBEBE009D9D9D00000000007F7F7F00F1F2F200DEF9
      FB00FBFBFB00F9F9F900D0D0D0003434340085858500F0F0F000C2C2C2001C1C
      1C00000000007B7B7B005B5B5B000000000077777700858585007A7A7A004FC4
      45004BBA2C00D8BD6000FFBA6200FFB96500DBBB7D005E5E5E00656565004B4B
      4B0000000000000000000000000000000000EFB05E00EEAB5300EEAB5300EEAB
      5300EEAB53000000000000000000EEAB5300EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB530000000000CCCCCC00AEAEAE00D5D5D500FDFD
      FD00FDFDFD00C7C7C700B5B5B500A3A3A300A3A3A300B5B5B500C7C7C700FCFC
      FC00FBFBFB00D2D2D200AEAEAE00BFBFBF000000000000000000E9F5F6002AED
      FB00F1F7F70023ECFB00E3E3E3006161610032323200CACACA00F3F3F300A7A7
      A700A2A2A200EAEAEA00878787000000000083838300AFAFAF007C7C7C007FBF
      3600DDC56900FFC27000FFBF6700AECBAC0068E0F90060606000ACACAC005C5C
      5C0000000000000000000000000000000000EEAB5300EEAB5300EEAB53000000
      000000000000000000000000000000000000EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB53000000000000000000ABABAB00FFFFFF00FFFF
      FF00FFFFFF00E0E0E000ACACAC00D1D1D100D1D1D100ABABAB00E0E0E000FFFF
      FF00FFFFFF00FFFFFF00ABABAB00000000000D0D0D00C4C4C400FBFBFB00F6F6
      F600F9F9F900DAF6F800E9E9E900747474000000000051515100D2D2D200F3F3
      F300F3F3F300B3B3B3001A1A1A00000000006C6C6C007474740068686800E6BC
      7100EDBE7900ECBB7300ECB96D0084CDCF0084D8EB004B4B4B00686868003D3D
      3D0000000000000000000000000000000000EEAB5300EEAB5300000000000000
      000000000000000000000000000000000000EEAB5300EEAB5300EEAB5300EEAB
      5300EEAB5300EEAB5300EEAB53000000000000000000DFDFDF00BDBDBD00A3A3
      A300A2A2A200B1B1B100D6D6D600FDFDFD00FCFCFC00D3D3D300B1B1B100A2A2
      A200A3A3A300BDBDBD00DFDFDF000000000036363600D7D7D700C3C3C3008D8D
      8D008E8C8E009C9C9C00E8E8E8008383830000000000000000002D2D2D007777
      77006B6B6B000A0A0A00000000000000000073737300A6A6A600696969005757
      570057575700575757005757570057575700575757004D4D4D009A9A9A006262
      6200000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EEAB5300EEAB5300EFB1
      5E00000000000000000000000000000000000000000000000000000000000000
      0000F8F8F800ABABAB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00ABABAB00F6F6
      F6000000000000000000000000000000000000000000929292003F3F3F000000
      000000000000040404008F8F8F00000000000000000000000000000000000000
      0000000000000000000000000000000000008989890076767600808080000000
      000000000000000000000000000000000000000000006E6E6E00515151006666
      6600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DFDFDF00BBBBBB00A2A2A200A2A2A200BBBBBB00DFDFDF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009FC2A2000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CD957000BD734200B768
      3500B5683500B4673400B2663400B0653300AE643300AC633200AA623200A961
      3200A8603100A7613200AB693C00BC8661000000000000000000000000000000
      00000000000000000000CA8A6100C3845800D38B6800E18F7000DC8D6C00DA8B
      6D00D78A6E00CD8B6C00AB6D4400A65F2E000000000076B2E6003E91DB00348C
      D900348CD900348CD900348CD900348CD900348CD900348CD900348CD900348B
      D900398FDA0085B9E900000000000000000000000000000000000000000086B6
      8B005A985F000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C37D4F00EBC6AD00EAC5AD00FEFB
      F800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFB
      F800FEFBF800C89A7C00C7987900AD6B40004E4E4E002D2D2D00505050000000
      00000000000000000000C6835500EFCEBA00DDFFFF0087EEC700A2F4D700A2F6
      D7008CEEC700E0FFFF00DDA28500AB6A3E004799DD00DEF1FA00A8DDF4009EDB
      F40096DAF3008ED8F30086D7F3007FD4F20079D3F20072D2F1006CD0F10069CF
      F100C2EAF8003F95DB000000000000000000000000000000000094C198005AA4
      620058A15E0037833E00347E3A00317A36002E7533003C7D4000659668000000
      000000000000000000000000000000000000BA6C3800EDCAB300E0A27A00FEFA
      F70062C0880062C0880062C0880062C0880062C0880062C0880062C0880062C0
      8800FDF9F600CA8D6500C99B7C00A761320038383800B0B0B000353535005757
      57005757570057575700C37F5100EFB69A00EAF3E80051BF84006FC9980071C9
      990054BF8400E4F4E900DD9C7B00AA693A003B97DB00EFFAFE00A1E9F90091E5
      F80081E1F70072DEF60063DAF50054D7F40047D3F30039D0F2002ECDF10026CB
      F000CAF2FB003B97DB00000000000000000000000000A1CCA50062AC6A0085C8
      8D0085C78B0082C688007FC486007CC2820079C1800071B978005FA865004991
      4E0069986C00000000000000000000000000BB6C3800EECCB600E1A27A00FEFA
      F700BFDCC200BFDCC200BFDCC200BFDCC200BFDCC200BFDCC200BFDCC200BFDC
      C200FDF9F600CD906800CC9E8100A86132003A3A3A0062626200383838006FDF
      9D0078E0A30074E09E00C4815400EAB69700F3F3EA00EDF1E600EFF1E600EFF0
      E600EDF1E500F3F5ED00D59C7900B07044003C9DDB00F2FAFD00B3EDFA00A4E9
      F90095E6F80085E2F70081E1F7007AE0F7006FDDF60062DAF50054D6F30047D3
      F200E8F9FD003594DA000000000000000000000000009FCCA40066B06E008ACA
      920089CA900086C88D0083C68A0080C587007EC384007BC281006DBB740076BE
      7C0059A05D0069996C000000000000000000BB6B3800EFCEB800E1A27900FEFA
      F70062C0880062C0880062C0880062C0880062C0880062C0880062C0880062C0
      8800FDF9F600CF936A00CEA38400AA61320063636300ACACAC00575757004FCB
      5C0057D77A0042D16A00C98B6100E6B59200E2A78100E1A78100DEA37D00DCA1
      7B00DB9F7900D99E7700D49A7300BB7E57003BA3DB00F6FCFE00C8F2FC00B9EF
      FB00ACECFA008CE4F8008AE3F80082E1F70079DFF7006DDDF60061DAF50057D7
      F400E7F8FD003594DA000000000000000000000000000000000098C99E0067B1
      6F0064AD6B0043944B00408F47003C8A430039854000549D5A0074BA7A0079C1
      7F0077BF7D004A914F000000000000000000BA6A3600EFD0BB00E2A27A00FEFB
      F800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFBF800FEFB
      F800FEFBF800D3966D00D2A78A00AB6232005C5C5C006D6D6D005F5F5F004FC4
      45004BBA2C00D8BD6000CA8D6500EAB89900DDA57E00DDA68000DBA37C00D9A0
      7A00D9A07900D89F7800D89E7800BF845D003BA8DB00FEFFFF00F8FDFF00F6FD
      FF00F5FCFF00DEDBD100ADCAC500A6C5C000A4C3BD009EBDB60097BAB30092B8
      B200E1CBB7003594DA00C3845200D0A17D0000000000000000000000000091C6
      96006BAF71000000000000000000000000000000000091BB94004D8F520075BB
      7B0070BD770063AB69006698690000000000BB6A3600F0D2BE00E2A37A00E2A3
      7A00E1A37A00E2A37B00E1A37B00E0A17800DE9F7700DD9F7600DC9D7400D99B
      7200D8997100D6997000D5AB8E00AD6333006565650089898900656565007FBF
      3600DDC56900FFC27000C8885D00EFBFA100FDFCFA00FEFCFB00FEFDFD00FEFD
      FC00FDFBFA00FDFCFB00DDA88500C17F530039ADDB00E8F6FB007EC5EA005BAE
      E30051A8E10061ADDF00EDF6F700EDF5F600E7EFF300E5ECEE00E5EBED00E5EB
      ED00F8F3EF003594DA00F0E2D800C58B5E000000000000000000000000000000
      0000AAD3AE00000000000000000000000000000000000000000091BC940061A8
      67007BC2820076BC7C003E80430000000000BB6A3600F2D5C200E3A37A00E3A3
      7A00E2A37B00E2A37B00E2A47B00E1A27900E0A17800DEA07700DE9E7500DC9D
      7400DA9B7300D99B7300DAB09500AF6433006E6E6E00A4A4A4006C6C6C00F8C6
      6E00FFC87700FFC57200C7865B00EFC09E00FFFFFF00CC936E00FFFFFF00FFFF
      FF00FFFBF700FFF8F100E4AF8C00C78A610040AEDC00F1FAFD0094DEF50093DC
      F40081D5F200C0A9970091C1E4003594DA003594DA003594DA003594DA003594
      DA003594DA003594DA00F0E2D800C48654000000000000000000000000000000
      000000000000000000000000000000000000000000000000000093BF970064AB
      6B007FC4860079BE81004285470000000000BB6A3600F2D8C500E3A47B00E3A3
      7A00E3A47A00E2A47B00E2A37B00E1A37B00E1A27900DFA07700DE9F7600DD9E
      7400DB9C7200DC9D7400DDB59A00B16534006969690071717100686868005757
      57005757570057575700CC8D6500F3CDB000FFFFFF00E3C7B300FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00EABFA100C989600041B4DC00F7FCFE008EE4F80091DE
      F5009FE0F500E3B18C00FAF6F100EAC9AE00FFFFFF00E8C7AC00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F1E5DB00C68655000000000000000000000000000000
      0000000000000000000000000000000000000000000097C59C00579E5F0081C3
      88007DC485006EB375006EA3720000000000BB6B3600F4D9C700E6A67D00C88C
      6400C98D6500C98E6700CB926C00CB926D00CA906900C88C6500C88C6400C88C
      6400C88C6400DA9C7400E1BA9F00B366340077777700AAAAAA007474740072E2
      9E007CE3A40078E3A000D4976E00D49E7B00D0987100D6A48200CD8E6800CD90
      6900D09A7500D1997300C88B6200EEDCD0003CB5DB00FDFEFE00FEFFFF00FEFE
      FF00FDFEFF00E5B48F00FAF6F200E9C6AA00E9C6AC00E8C7AC00E8C7AC00E9C9
      B000E8C8B000E8CCB500F2E7DE00C88A590000000000000000005FBA6A005CB6
      670059B3640056AE600053AA5D0050A659004DA1560068B1700088C890008DCC
      95008BCB92005DA564000000000000000000BB6C3700F4DCC900E7A77D00F9EC
      E100F9ECE100F9EDE300FCF4EE00FDFAF700FDF7F300FAEDE500F7E7DB00F7E5
      D900F6E5D800DEA07700E4BEA400B46734007777770097979700777777004FCB
      5C0057D77A0042D16A0070C75E00B6B85000CBAE3F005B5B5B007C7C7C004B4B
      4B000000000000000000000000000000000059C2E00061C3E20063C4E30063C4
      E30063C4E300E7B79400FBF7F400E9C3A600FFFFFF00E8C7AC00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7F1EB00CB8F5F00000000000000000062BE6D00A5DA
      AE00A2D8AC00A1D8AA009ED6A7009CD5A50099D4A20097D29F008CCD950091CF
      990073B87B007BB280000000000000000000BD6E3A00F5DDCC00E7A87E00FAF0
      E800FAF0E800C98D6600FAF0E900FDF8F300FEFAF800FCF4EF00F9E9DF00F7E7
      DB00F7E5D900E0A27800E7C2A900B668350077777700858585007A7A7A004FC4
      45004BBA2C00D8BD6000FFBA6200FFB96500DBBB7D005E5E5E00656565004B4B
      4B00000000000000000000000000000000000000000000000000000000000000
      000000000000E9BA9800FBF7F400E9C3A600E9C3A600E9C3A600E9C3A600E9C3
      A600E9C3A600E9C3A600FBF7F400CE936400000000000000000064C17000A6DB
      B000A6DAAF00A3D9AD00A2D8AB009FD7A8009CD5A50094D09D0083C58C006CB4
      740080B98600000000000000000000000000C0744200F6DFD000E8A87E00FCF6
      F100FCF6F100C88C6400FAF1E900FBF4EE00FDFAF700FDF9F600FAF0E800F8E8
      DD00F7E6DB00E1A37A00EFD5C300B76A360083838300AFAFAF007C7C7C007FBF
      3600DDC56900FFC27000FFBF6700AECBAC0068E0F90060606000ACACAC005C5C
      5C00000000000000000000000000000000000000000000000000000000000000
      000000000000EBBD9B00FBF7F400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FBF7F400D1976A00000000000000000066C4720064C2
      700062BF6E0060BC6B005DB868005BB5650059B2630063B36C0083C189000000
      000000000000000000000000000000000000C6825500F6DFD100E9AA8000FEFA
      F600FDFAF600C88C6400FBF3EE00FBF1EA00FCF6F200FEFBF800FCF6F100F9EC
      E200F8E7DB00EED0BA00ECD0BD00BD7443006C6C6C007474740068686800E6BC
      7100EDBE7900ECBB7300ECB96D0084CDCF0084D8EB004B4B4B00686868003D3D
      3D00000000000000000000000000000000000000000000000000000000000000
      000000000000ECBF9E00FBF7F4009CD5A50098D3A1008BCB930082C689007EC3
      84007AC1800076BE7C00FBF7F400D49B6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D6A58500F6E0D100F7E0D100FEFB
      F800FEFBF700FDF9F600FCF5F000FAF0EA00FBF2ED00FDF9F600FDFAF700FBF1
      EB00F8E9DF00ECD1BE00CD926A00E2C5B10073737300A6A6A600696969005757
      570057575700575757005757570057575700575757004D4D4D009A9A9A006262
      6200000000000000000000000000000000000000000000000000000000000000
      000000000000EFC6A800FBF7F400FBF7F400FBF7F400FBF7F400FBF7F400FBF7
      F400FBF7F400FBF7F400FBF7F400D8A378000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E1BDA600D9AB8D00C9895E00C075
      4300BD6E3A00BB6C3700BB6B3600BB6A3600BB6A3600BC6C3900BD6E3B00BB6D
      3A00BF744400C98D6500E7CEBC00000000008989890076767600808080000000
      000000000000000000000000000000000000000000006E6E6E00515151006666
      6600000000000000000000000000000000000000000000000000000000000000
      000000000000F7E1D200F1C8AC00EDC09F00EBBE9D00E7B79300E4B28C00E2AF
      8800E0AC8400DDA98000DCA57D00E2B69600424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FFFF800000000000C003000000000000
      C003000000000000C003000000000000C003000000000000C003000000000000
      C003000000000000C003000000000000C003000000000000C003000000000000
      C003000000000000C003000000000000C007000000000000C00F000000000000
      C01F000000000000FFFF000000000000C003FFC7FFFFFFFFC0011F83C0FFFC3F
      E0030001003FF00FC0310000001FC003C0310000600F0000C0210000C0070000
      C0410001C0030000C083000380030000C107000700030000C30F000F00010000
      819F000F800100008001000F06010000C001000F1F0180010081000F3F018001
      00C3000FFF8FF00F99FF1F8FFFFFF81FF7FF8000FC008003E7FF00001C000003
      C01F00000000000380070000000000038003000000000003C003000000000000
      E781000000000000F7C1000000000000FFC1000000000000FF81000000000000
      C0030000000F0000C0030000000FF800C0070000000FF800C01F0000000FF800
      FFFF0000000FF800FFFF00011F8FF80000000000000000000000000000000000
      000000000000}
  end
  object puPresets: TPopupMenu
    Images = ilMain
    Left = 928
    object Presets1: TMenuItem
      Caption = 'Presets'
      Enabled = False
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object miLoadPreset: TMenuItem
      Caption = 'Load'
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object miPresetSaveCurrent: TMenuItem
      Caption = 'Save current'
      OnClick = miPresetSaveCurrentClick
    end
  end
  object puPixelSize: TPopupMenu
    Left = 984
    object PixelSize1: TMenuItem
      Caption = '.'
      Enabled = False
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object miPixelTiny: TMenuItem
      AutoCheck = True
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
    object miPixelSmall: TMenuItem
      Tag = 1
      AutoCheck = True
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
    object miPixelMedium: TMenuItem
      Tag = 2
      AutoCheck = True
      Caption = '.'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
    object miPixelLarge: TMenuItem
      Tag = 3
      AutoCheck = True
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
    object miPixelVeryLarge: TMenuItem
      Tag = 4
      AutoCheck = True
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
    object miPixelUltra: TMenuItem
      Tag = 5
      AutoCheck = True
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
    object miPixelMegaUltra: TMenuItem
      Tag = 6
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
    object N30: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object miPixelAuto: TMenuItem
      Tag = 99
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelTinyClick
    end
  end
  object puFonts: TPopupMenu
    Left = 880
    object MenuItem1: TMenuItem
      Caption = '.'
      Enabled = False
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object miLoadFont: TMenuItem
      Caption = '.'
    end
    object N47: TMenuItem
      Caption = '-'
    end
    object Fontviewer2: TMenuItem
      Caption = '.'
      OnClick = miFontViewerClick
    end
    object N28: TMenuItem
      Caption = '-'
    end
    object miFontWrap: TMenuItem
      AutoCheck = True
      Caption = '.'
      OnClick = miFontWrapClick
    end
  end
  object puAnimationSpeed: TPopupMenu
    Left = 952
    Top = 56
    object Playbackspeed1: TMenuItem
      Caption = '.'
      Enabled = False
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object miPlaybackSpeed1: TMenuItem
      Caption = '2 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed2: TMenuItem
      Tag = 1
      Caption = '1.5 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed3: TMenuItem
      Tag = 2
      Caption = '1 second'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed4: TMenuItem
      Tag = 3
      Caption = '0.5 seconds'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed5: TMenuItem
      Tag = 4
      Caption = '0.25 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed6: TMenuItem
      Tag = 5
      Caption = '0.2 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed7: TMenuItem
      Tag = 6
      Caption = '0.1 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed8: TMenuItem
      Tag = 7
      Caption = '0.05 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed9: TMenuItem
      Tag = 8
      Caption = '0.025 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed10: TMenuItem
      Tag = 9
      Caption = '0.020 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object miPlaybackSpeed11: TMenuItem
      Tag = 10
      Caption = '0.01 seconds'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPlaybackSpeed3Click
    end
    object N39: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object miPlaybackSpeedCustom: TMenuItem
      Tag = 20
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
    end
    object N40: TMenuItem
      Caption = '-'
      GroupIndex = 1
      OnClick = miPlaybackSpeed3Click
    end
    object Setcustomspeed1: TMenuItem
      Caption = '.'
      GroupIndex = 1
      OnClick = Setcustomspeed1Click
    end
  end
  object timerAutosave: TTimer
    Enabled = False
    OnTimer = timerAutosaveTimer
    Left = 1352
    Top = 56
  end
  object puGradient: TPopupMenu
    Left = 760
    object miGradientColour0: TMenuItem
      Caption = '.'
      OnClick = miGradientColour0Click
    end
    object Colour11: TMenuItem
      Tag = 1
      Caption = '.'
      OnClick = miGradientColour0Click
    end
    object Colour21: TMenuItem
      Tag = 2
      Caption = '.'
      OnClick = miGradientColour0Click
    end
    object Colour31: TMenuItem
      Tag = 3
      Caption = '.'
      OnClick = miGradientColour0Click
    end
  end
  object puGradients: TPopupMenu
    Left = 824
    object MenuItem3: TMenuItem
      Caption = '.'
      Enabled = False
    end
    object MenuItem4: TMenuItem
      Caption = '-'
    end
    object miLoadGradients: TMenuItem
      Caption = '.'
    end
    object MenuItem6: TMenuItem
      Caption = '-'
    end
    object miSaveGradient: TMenuItem
      Caption = '.'
      OnClick = miSaveGradientClick
    end
  end
  object puPixelShape: TPopupMenu
    Left = 1104
    object MenuItem5: TMenuItem
      Caption = '.'
      Enabled = False
    end
    object MenuItem7: TMenuItem
      Caption = '-'
    end
    object miPixelShapeSquare: TMenuItem
      AutoCheck = True
      Caption = '.'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelShapeSquareClick
    end
    object miPixelShapeRound: TMenuItem
      Tag = 1
      AutoCheck = True
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelShapeSquareClick
    end
    object miPixelShapeRoundRect: TMenuItem
      Tag = 2
      Caption = '.'
      GroupIndex = 1
      RadioItem = True
      OnClick = miPixelShapeSquareClick
    end
  end
  object puBrushSize: TPopupMenu
    Left = 1216
    object MenuItem8: TMenuItem
      Caption = '.'
      Enabled = False
    end
    object MenuItem9: TMenuItem
      Caption = '-'
    end
    object miBrushSizeSmall: TMenuItem
      Caption = '.'
      Checked = True
      RadioItem = True
      OnClick = miBrushSizeSmallClick
    end
    object Large1: TMenuItem
      Tag = 1
      Caption = '.'
      RadioItem = True
      OnClick = miBrushSizeSmallClick
    end
    object Large3x3pixels1: TMenuItem
      Tag = 2
      Caption = '.'
      RadioItem = True
      OnClick = miBrushSizeSmallClick
    end
    object N4x41: TMenuItem
      Tag = 3
      Caption = '(4x4)'
      RadioItem = True
      OnClick = miBrushSizeSmallClick
    end
    object N5x51: TMenuItem
      Tag = 4
      Caption = '(5x5)'
      RadioItem = True
      OnClick = miBrushSizeSmallClick
    end
  end
  object puGradientRGB: TPopupMenu
    OnPopup = puGradientRGBPopup
    Left = 1008
    Top = 56
    object miGradientSelectRGB: TMenuItem
      Caption = '.'
      OnClick = miGradientSelectRGBClick
    end
    object N33: TMenuItem
      Caption = '-'
    end
    object miGradSetRow: TMenuItem
      Caption = '.'
      OnClick = miGradSetRowClick
    end
    object miGradFrom: TMenuItem
      Caption = '.'
      OnClick = miGradFromClick
    end
    object miGradientBottomTop: TMenuItem
      Caption = '.'
      OnClick = miGradientBottomTopClick
    end
  end
  object puRandom: TPopupMenu
    Left = 1160
    object MenuItem10: TMenuItem
      Caption = '.'
      Enabled = False
    end
    object MenuItem11: TMenuItem
      Caption = '-'
    end
    object miRandomnessTiny: TMenuItem
      Tag = 20
      Caption = '.'
      Checked = True
      RadioItem = True
      OnClick = miRandomnessTinyClick
    end
    object Small2: TMenuItem
      Tag = 30
      Caption = '.'
      RadioItem = True
      OnClick = miRandomnessTinyClick
    end
    object Medium1: TMenuItem
      Tag = 40
      Caption = '.'
      RadioItem = True
      OnClick = miRandomnessTinyClick
    end
    object Large2: TMenuItem
      Tag = 50
      Caption = '.'
      RadioItem = True
      OnClick = miRandomnessTinyClick
    end
    object Massive1: TMenuItem
      Tag = 60
      Caption = '.'
      RadioItem = True
      OnClick = miRandomnessTinyClick
    end
  end
  object spdMain: TSavePictureDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 784
    Top = 160
  end
  object puPreview: TPopupMenu
    Left = 1040
    object Previewsize2: TMenuItem
      Caption = '.'
      object miPUPreviewx1: TMenuItem
        Caption = 'x1'
        Checked = True
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx2: TMenuItem
        Tag = 1
        Caption = 'x2'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx3: TMenuItem
        Tag = 2
        Caption = 'x3'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx4: TMenuItem
        Tag = 3
        Caption = 'x4'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx5: TMenuItem
        Tag = 4
        Caption = 'x5'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx6: TMenuItem
        Tag = 5
        Caption = 'x6'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx8: TMenuItem
        Tag = 6
        Caption = 'x8'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx10: TMenuItem
        Tag = 7
        Caption = 'x10'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx12: TMenuItem
        Tag = 8
        Caption = 'x12'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx15: TMenuItem
        Tag = 9
        Caption = 'x15'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx20: TMenuItem
        Tag = 10
        Caption = 'x20'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx25: TMenuItem
        Tag = 11
        Caption = 'x25'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx30: TMenuItem
        Tag = 12
        Caption = 'x30'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx40: TMenuItem
        Tag = 13
        Caption = 'x40'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
      object miPUPreviewx50: TMenuItem
        Tag = 14
        Caption = 'x50'
        GroupIndex = 30
        RadioItem = True
        OnClick = miPreviewx1Click
      end
    end
    object Previewview2: TMenuItem
      Caption = '.'
      object miPUPreviewViewSquare: TMenuItem
        Caption = '.'
        Checked = True
        RadioItem = True
        OnClick = miPreviewViewSquareClick
      end
      object miPUPreviewViewRadial: TMenuItem
        Tag = 1
        Caption = '.'
        RadioItem = True
        OnClick = miPreviewViewSquareClick
      end
      object miPUPreviewViewRadialTQ: TMenuItem
        Tag = 2
        Caption = '.'
        RadioItem = True
        OnClick = miPreviewViewSquareClick
      end
      object miPUPreviewViewSemiCircle: TMenuItem
        Tag = 3
        Caption = '.'
        RadioItem = True
        OnClick = miPreviewViewSquareClick
      end
      object miPUPreviewViewSemiCircleInverted: TMenuItem
        Tag = 4
        Caption = '.'
        RadioItem = True
        OnClick = miPreviewViewSquareClick
      end
    end
    object PreviewvoidRadialSemicircle1: TMenuItem
      Caption = '.'
      object miPUPreviewVoid10: TMenuItem
        Caption = '10'
        Checked = True
        Default = True
        RadioItem = True
        OnClick = miPreviewVoid10Click
      end
      object miPUPreviewVoid15: TMenuItem
        Tag = 1
        Caption = '15'
        RadioItem = True
        OnClick = miPreviewVoid10Click
      end
      object miPUPreviewVoid20: TMenuItem
        Tag = 2
        Caption = '20'
        RadioItem = True
        OnClick = miPreviewVoid10Click
      end
      object miPUPreviewVoid25: TMenuItem
        Tag = 3
        Caption = '25'
        RadioItem = True
        OnClick = miPreviewVoid10Click
      end
      object miPUPreviewVoid30: TMenuItem
        Tag = 4
        Caption = '30'
        RadioItem = True
        OnClick = miPreviewVoid10Click
      end
      object miPUPreviewVoid40: TMenuItem
        Tag = 5
        Caption = '40'
        RadioItem = True
        OnClick = miPreviewVoid10Click
      end
      object miPUPreviewVoid50: TMenuItem
        Tag = 6
        Caption = '50'
        RadioItem = True
        OnClick = miPreviewVoid10Click
      end
    end
  end
  object opdMain: TOpenPictureDialog
    Filter = 'GIF Image (*.gif)|*.gif'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 736
    Top = 260
  end
  object puGradientRGB_3BPP: TPopupMenu
    Left = 1112
    Top = 56
    object miGradientRGB3BPP1: TMenuItem
      Caption = 'Black (000)'
      OnClick = miGradientRGB3BPP1Click
    end
    object MenuItem15: TMenuItem
      Tag = 3
      Caption = 'Cyan (011)'
      OnClick = miGradientRGB3BPP1Click
    end
    object MenuItem13: TMenuItem
      Tag = 1
      Caption = 'Blue (001)'
      OnClick = miGradientRGB3BPP1Click
    end
    object MenuItem14: TMenuItem
      Tag = 2
      Caption = 'Green (010)'
      OnClick = miGradientRGB3BPP1Click
    end
    object Red1001: TMenuItem
      Tag = 4
      Caption = 'Red (100)'
      OnClick = miGradientRGB3BPP1Click
    end
    object Magenta1011: TMenuItem
      Tag = 5
      Caption = 'Magenta (101)'
      OnClick = miGradientRGB3BPP1Click
    end
    object Yellow1101: TMenuItem
      Tag = 6
      Caption = 'Yellow (110)'
      OnClick = miGradientRGB3BPP1Click
    end
    object White1111: TMenuItem
      Tag = 7
      Caption = 'White (111)'
      OnClick = miGradientRGB3BPP1Click
    end
  end
  object puMainCanvas: TPopupMenu
    Left = 32
    Top = 162
    object Workingareabackgroundcolour1: TMenuItem
      Caption = 'Working area background colour'
      object Custom1: TMenuItem
        Caption = 'Custom'
        OnClick = miCustomBackgroundClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Black2: TMenuItem
        Caption = 'Black'
        OnClick = Black1Click
      end
      object Darkgreydefault1: TMenuItem
        Tag = 6
        Caption = 'Dark grey  (default)'
        OnClick = Black1Click
      end
      object Grey2: TMenuItem
        Tag = 1
        Caption = 'Grey'
        OnClick = Black1Click
      end
      object Green2: TMenuItem
        Tag = 2
        Caption = 'Green'
        OnClick = Black1Click
      end
      object Purple2: TMenuItem
        Tag = 3
        Caption = 'Purple'
        OnClick = Black1Click
      end
      object Red2: TMenuItem
        Tag = 4
        Caption = 'Red'
        OnClick = Black1Click
      end
      object White2: TMenuItem
        Tag = 5
        Caption = 'White'
        OnClick = Black1Click
      end
    end
  end
  object ilActive: TImageList
    BkColor = clFuchsia
    DrawingStyle = dsTransparent
    Height = 20
    Masked = False
    Width = 20
    Left = 1440
    Top = 8
    Bitmap = {
      494C01015E006800040014001400FF00FF00FF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000050000000E001000001002000000000000058
      020000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF005555550055555500FF00
      FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF0055555500FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF0055555500FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF0055555500FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF005555550055555500FF00
      FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0006E9FF0008E7FF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0005EAFF0005EAFF0005EAFF0005EAFF0005EAFF0005EA
      FF0009E6FF0005EAFF0005EAFF000AE6FF0007E8FF0005EAFF0005EAFF0005EA
      FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0005EAFF0008E8FF0005EAFF0006E9FF0008E7FF000AE5
      FF000BE5FF0006E9FF0005EAFF0009E6FF0007E8FF0005EAFF0005EAFF0008E7
      FF0005EAFF0007E8FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0005EAFF0008E7FF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000DE2FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF000DE3FF0007E8FF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000DE2FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF000BE4FF000CE4FF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF000BE4FF000BE4FF0000EEFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0003ECFF0005EAFF0008E7FF0005EAFF0005EAFF0005EA
      FF0005EAFF0008E7FF0007E8FF0005EAFF0005EAFF0005EAFF0005EAFF0005EA
      FF0004EBFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0005EAFF0005EAFF000AE5FF0005EAFF0005EAFF0005EA
      FF0005EAFF0005EAFF0005EAFF0005EAFF0005EAFF0005EAFF0005EAFF0005EA
      FF0006E9FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000CE3
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000DE2
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EE
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00
      FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0005EAFF0005EAFF0005EAFF0005EAFF0007E8FF000AE6
      FF000AE5FF0006E9FF0005EAFF0005EAFF0005EAFF0005EAFF0005EAFF0005EA
      FF0005EAFF0008E7FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0005EAFF0005EAFF0005EAFF0005EAFF0005EAFF0005EA
      FF0005EAFF0005EAFF0003EBFF0005EAFF0005EAFF0005EAFF0008E7FF0005EA
      FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0005EAFF0005EAFF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF0005EA
      FF0005EAFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0003EBFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF0000EEFF0000EEFF00FF00
      FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0001EDFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000CE3FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF0000EE
      FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF0000EE
      FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0002ECFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF0005EAFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0006E8FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF0000EE
      FF0000EEFF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00
      FF0000EEFF00FF00FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EEFF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0000EEFF00FF00FF0000EEFF0000EEFF00FF00
      FF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000EE
      FF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF0000EEFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000EEFF0000EEFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500555555009D9D9D005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555009D9D9D009D9D9D005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555009D9D9D0055555500555555009D9D9D009D9D9D00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500555555009D9D9D0055555500555555009D9D9D009D9D9D005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555009D9D
      9D009D9D9D00555555009D9D9D009D9D9D009D9D9D009D9D9D009D9D9D005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55009D9D9D009D9D9D009D9D9D009D9D9D009D9D9D009D9D9D00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555009D9D9D009D9D9D009D9D9D009D9D9D0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555009D9D9D009D9D9D005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF005555550055555500FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF005555550055555500FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF005555550055555500FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FC00FC00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF0055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF005555
      5500FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF0055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555009D9D
      9D0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF0055555500555555009D9D9D005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF0055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55009D9D9D0055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500555555009D9D9D0055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF0055555500FF00FF005555
      5500FF00FF0055555500FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555009D9D9D005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555009D9D
      9D0055555500555555005555550055555500FF00FF00FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555009D9D9D009D9D9D005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555009D9D9D009D9D9D00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555009D9D9D009D9D9D009D9D
      9D00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555009D9D9D009D9D9D009D9D9D00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500555555009D9D9D009D9D
      9D009D9D9D00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555009D9D9D009D9D9D009D9D9D0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555009D9D
      9D009D9D9D009D9D9D00555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555009D9D9D009D9D9D009D9D9D00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55009D9D9D009D9D9D009D9D9D00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555009D9D9D009D9D9D009D9D9D005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555009D9D9D009D9D9D009D9D9D00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555009D9D9D009D9D9D009D9D
      9D00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555009D9D9D009D9D9D009D9D9D005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500555555009D9D9D009D9D
      9D009D9D9D005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555009D9D9D009D9D9D005353530055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555009D9D
      9D009D9D9D009D9D9D0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FFFF
      FF005555550055555500FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FFFF
      FF00FFFFFF005555550055555500FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FFFFFF00555555005555
      5500FFFFFF00FFFFFF005555550055555500FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FFFFFF005555
      550055555500FFFFFF00FFFFFF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500FFFFFF00FFFFFF0055555500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FFFFFF00FFFF
      FF00555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      55005555550055555500FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      5500FF00FF0055555500FF00FF0055555500FF00FF0055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      55005555550055555500FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00555555005555550055555500FF00FF0055555500555555005555
      55005555550055555500FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00555555005555
      550055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500555555005555550055555500FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500FF00FF00FF00FF00555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500555555005555550055555500555555005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      5500FF00FF0055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0055555500555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500555555005555550055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500FF00FF005555550055555500FF00FF005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500555555005555550055555500FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF0055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500555555005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500000000005555
      550055555500555555005555550000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500000000005555
      550055555500555555000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500000000005555
      5500555555000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00
      FF00555555005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0055555500555555005555
      550055555500555555005555550055555500555555005555550055555500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500000000005555
      55000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00555555005555550055555500555555005555
      550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF005555
      55005555550055555500555555005555550055555500FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000555555005555550055555500555555005555550055555500000000000000
      000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF0055555500555555005555550055555500555555005555
      5500555555005555550055555500555555005555550055555500555555005555
      55005555550055555500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF0000000000000000000000
      00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF0000000000000000001919
      1900000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FF00FF00FFFFFF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF000000000020202000FFFF
      FF0011111100000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF0000000000000000002020
      2000FFFFFF0016161600000000000000000000000000FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000212121000303030000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF00FFFFFF00FF00FF00FFFFFF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000000000000000000000000000000000FFFFFF00C8C8C8000000
      00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00000000000000000000000000DCDCDC00FFFFFF00BBBB
      BB00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000000000000ECECEC00FFFF
      FF00DEDEDE00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000000001010100EEEE
      EE00FFFFFF00E8E8E800090909000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000101
      0100C1C1C100FFFFFF00E6E6E600181818000101010000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
      00001A1A1A00CACACA00FFFFFF00E7E7E700181818000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00000000000022222200DEDEDE00FFFFFF00B6B6B6002929290000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000000000000000000000C8C8C800FFFFFF005353530000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000011111100494949000A0A0A0000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF26FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF26FF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF26
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF04
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF03
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF26FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF05FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF01FF00FF02FF00FF01FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF26FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF000400FF000400FF000400FF000400FF001D00FF000400FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000001919
      1900000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00F2FF0000F2FF0000F2FF0000F2FF0000F2FF0000F2FF0000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
      000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF000000000020202000FFFF
      FF0011111100000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0011FF000011FF000011FF000011FF000011FF000011FF0000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FFFF
      FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000002020
      2000FFFFFF0016161600000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF0000FFE50000FFE50000FFE50000FFE50000FFE50000FFE500FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FFFF
      FF00FFFFFF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000212121000303030000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF0000BFFF0000BFFF0000BFFF0000BFFF0000BFFF0000BFFF00FF00
      FF00FF00FF00FF00FF00FF00FF000000000000000000FFFFFF00000000000000
      0000FFFFFF00FFFFFF000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF002A00FF002A00FF002A00FF002A00FF002A00FF002A00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000000000000000000000000000000000FFFFFF00C8C8C8000000
      00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00000000000000000000000000DCDCDC00FFFFFF00BBBB
      BB00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000000000000ECECEC00FFFF
      FF00DEDEDE00000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FB00FB00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000000001010100EEEE
      EE00FFFFFF00E8E8E800090909000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000101
      0100C1C1C100FFFFFF00E6E6E600181818000101010000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
      00001A1A1A00CACACA00FFFFFF00E7E7E700181818000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000000000000000000000000
      00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      00000000000022222200DEDEDE00FFFFFF00B6B6B6002929290000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000000000000000000000
      0000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00000000000000000000000000C8C8C800FFFFFF005353530000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000000000000000000001000100FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000011111100494949000A0A0A0000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000000000
      0000000000000000000006000600FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF000000000011001100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FC00FC00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FF00FF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFBFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF0CFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF1CFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FFFF
      FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FF03FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00
      FF00FF00FF00FF00FF00FFFFFF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FFFFFF00FFFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00424D3E000000000000003E000000
      2800000050000000E00100000100010000000000801600000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFF00000000000000FFFFFFFF
      FF00000000000000FFFFFFF9FF00000000000000C0003FF9FF00000000000000
      C0003FF9FF00000000000000C0003FF9FF00000000000000FFFFFFF9FF000000
      00000000FC003FF9FF00000000000000FC003FF9FF00000000000000FC003C00
      0300000000000000FFFFFC000300000000000000FFC03FF9FF00000000000000
      FFC03FF9FF00000000000000FFC03FF9FF00000000000000FFFFFFF9FF000000
      00000000FFFC3FF9FF00000000000000FFFC3FF9FF00000000000000FFFC3FF9
      FF00000000000000FFFFFFFFFF00000000000000FFFFFFFFFF00000000000000
      FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFE1FE79E7FFFFFF
      FFFF0000FFDEFC0003C0003C00030000FFBF7C0003C0003C00030000FF7FBE79
      E7C0003C00030000FF7FBE79E7FFFFFFFFFF0000FF7FBE79E7F000FC003F0000
      F87FBE79E7F000FC003F0000F7BF7C0003F000FC003F0000EFDEFC0003FFFFFF
      FFFF0000DFE1FE79E7FC03FC03FF0000DFEFFE79E7FC03FC03FF0000DFEFFE79
      E7FC03FC03FF0000DFEFFE79E7FFFFFFFFFF0000EFDFFC0003FF0FFC3FFF0000
      F7BFFC0003FF0FFC3FFF0000F87FFE79E7FF0FFC3FFF0000FFFFFFFFFFFFFFFF
      FFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFF
      FFFFFFFFFFFF0000FFFFFFE07FFFFFFFF9FF0000C0003F9FBFFE97FFF9FF0000
      C0003F7FDFFAF5FFFFFF0000C0003EFFEFF3FCFFFFFF0000FFFFFEF9EFFFFFFF
      FFFF0000FC003EE0E7CFFF3FFFFF0000FC003ECE67DFFFBFFFFF0000FC003EDE
      EFFFFFFCFFF30000FFFFFED9CFDFFFBCFFF30000FFC03ECFDFFFFFFFFFFF0000
      FFC03EE71FCFFF3FFFFF0000FFC03F707BFFFFFFFFFF0000FFFFFF3FFBF3FCFF
      FFFF0000FFFC3F9FF7FAF5FFFFFF0000FFFC3FCFEFFE97FFF9FF0000FFFC3FE0
      1FFFFFFFF9FF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFE1FE79E7FFFFFF
      FFFF0000FFDEFC0003C0003C00030000FFBF7C0003C0003C00030000FF7FBE79
      E7C0003C00030000FF7FBE79E7FFFFFFFFFF0000FF7FBE79E7F000FC003F0000
      F87FBE79E7F000FC003F0000F7BF7C0003F000FC003F0000EFDEFC0003FFFFFF
      FFFF0000DFE1FE79E7FC03FC03FF0000DFEFFE79E7FC03FC03FF0000DFEFFE79
      E7FC03FC03FF0000DFEFFE79E7FFFFFFFFFF0000EFDFFC0003FF0FFC3FFF0000
      F7BFFC0003FF0FFC3FFF0000F87FFE79E7FF0FFC3FFF0000FFFFFFFFFFFFFFFF
      FFFF0000FFFFFFFFFFFFFFFFFFFF0000FF0FFFFFFFFFFFFFFFFF0000FF0FFFFF
      FFFFFFFFFFFF0000FFFFFFE07FFFFFFFF9FF0000FE07FF9FBFFE97FFF9FF0000
      FE07FF7FDFFAF5FFFFFF0000FC63FEFFEFF3FCFFFFFF0000FC63FEF9EFFFFFFF
      FFFF0000F8F1FEE0E7CFFF3FFFFF0000F1F8FECE67DFFFBFFFFF0000F3FCFEDE
      EFFFFFFCFFF30000C3FC3ED9CFDFFFBCFFF30000C3FC3ECFDFFFFFFFFFFF0000
      F3FCFEE71FCFFF3FFFFF0000F1F8FF707BFFFFFFFFFF0000F8F1FF3FFBF3FCFF
      FFFF0000F000FF9FF7FAF5FFFFFF0000E2047FCFEFFE97FFF9FF0000F7FEFFE0
      1FFFFFFFF9FF0000FF9FFFFFFFFFFFFFFFFF0000FF9FFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFF0FF0000FFFFFFFFFFFFFFFFF0FF0000F3333FFFFFFC7FFF
      FFFF0000F3333FCF9FF83FFFE07F0000CFFFFFC70FF01FFFE07F0000CFFFFFC2
      07E00FFFC63F0000FFFF3FC007C007FFC63F0000FFFF3F800FC003FF8F1F0000
      CFFFFF801FC003FF1F8F0000CFFFFF803FE003FF3FCF0000FFFF3F003FF003FC
      3FC30000FFFF3F000FF801FC3FC30000CFFFFF0007FC00FF3FCF0000CFFFFE00
      07FE007F1F8F0000FFFF3E003FFFE03F8F1F0000FFFF3E03FFFFF01F000F0000
      CCCCFE1FFFFFF81E20470000CCCCFFFFFFFFFC1F7FEF0000FFFFFFFFFFFFFF3F
      F9FF0000FFFFFFFFFFFFFFFFF9FF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFC0
      03FFFFFFFFFF0000FF9FFF8001FF9EFFFFFF0000FF9FFF9FF9FF9EFFFFFF0000
      FF9FFF9FF9FF983FFF830000FF9FFF9E79FF9EFFFFFF0000FF9FFF9E79FF9EFF
      FFFF0000FF9FFF9819FF9FFFFFFF0000E79E799819FF9FFFFFFF0000C39C399E
      79C0003C00030000C198399E79C0003C00030000E090799FF9FF9FFFFFFF0000
      F000F99FF9FF9FFFFFFF0000F801F98001FF9FFFFFFF0000FC03F9C003FF9FFF
      FFFF0000FE07F9FFFFFF9FFFFFFF0000FF0FF9FFFFFF9FFFFFFF0000FF9FF800
      FFFF9FFFFFFF0000FFFFFC00FFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000FF0FFFF0FFFFFFFFF8FF0000FC03FFC01FE0007F
      C01F0000F8F1FF8F1FC0003F800F0000F1FCFF3F8FC0003F0F870000E3FE7E7F
      C7C0003F1FC70000E7FE7E7FE7C0003E38E30000C7FF3CFFE3C0003E38E30000
      CFFF3CFFF3C0003E78F30000FFFF3CFFFFC0003E78F30000FFFF7EFFFFC0003E
      38E30000FFFE7E7FFFC0003E38E30000FEFC7E3F7FC0003F18C30000FCF8FF1F
      3FC0003F0F010000F8E1FF871FC0003F80010000F007FFE00FC0003FE03F0000
      F81FFFF81FC0003FFFFF0000FCFFFFFF3FE0007FF07F0000FEFFFFFF7FFFFFFF
      F07F0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFF
      FFFFFFFFFFFF0000FF9FFC00039FFE3C7FF90000FF0FFC80139FFC3C3FF90000
      FF07FC80139FF03C0FF90000FF83FC80139FC03C03F90000FFC1FC80139F003C
      00F90000FFE0FC80139E003C00790000FFF07CC03398003C00190000C0003C7F
      E390003C00090000C0003C000390003C00090000FFF07CFFC398003C00190000
      FFE0FCFC439E003C00790000FFC1FCFC439F003C00F90000FF83FCFC479FC03C
      03F90000FF07FCFC4F9FF03C0FF90000FF0FFCFFDF9FFC3C3FF90000FF9FFC00
      3F9FFE3C7FF90000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFDFFFFFFFFFFF0000E3FFFFFFCFFFFFFF
      FFFF0000E0FFFFFFE7FFFFFFFFFF0000E03FF83F03C0003C00030000E01FF81E
      03C0003C00030000E007FFCCE7CFFF3C00030000E001FFC9CFCFFF3C00030000
      E0007FE1DFCFFF3C00030000E0003FE3FFCFFF3C00030000E0003FE3FFCFFF3C
      00030000E0007FE1DFCFFF3C00030000E001FFC9CFCFFF3C00030000E007FFCC
      E7CFFF3C00030000E01FF81E03C0003C00030000E03FF81F03C0003C00030000
      E0FFFFFFE7FFFFFFFFFF0000E3FFFFFFCFFFFFFFFFFF0000FFFFFFFFDFFFFFFF
      FFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFEFFF3F3FFFFF000083F8183F
      EFFC1E1FFFFF000081F8181FABF80E1FFFFF000080F8180FC7F0061FFFFF0000
      8078180701E0023C3FC3000080381803C7C00138FFF10000C0181C01AB8000F8
      FFF10000E00FFE00EF800078FFF10000F007FF006F8FF878FFF10000F803FF80
      3F87F071FFF80000FC01FFC01FC3E0F1FFF80000FE00FFE00FE1C1F8FFF10000
      FF007FF007F083F8FFF10000FF803FF803F807F8FFF10000FFC01FFC01FC0FF8
      FFF10000FFE01FFE01FC1FFC3FC30000FFF01FFF01F83FFFFFFF0000FFF81FFF
      81F07FFFFFFF0000FFFC1FFFC1F0FFFFFFFF0000FFFFFFFFFFFBFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFF83000081E01FF9FFE7FFFF
      C7830000E7F87FF0FFC3FFFF83830000F7F87FE0FFC1FFFF01830000F7F8FFC1
      FFE0FFFE00830000F3F1FF83FFF07FFC007F0000FBF1FF07FFF83FFC003F0000
      F801FE0FFFFC1FFC003F0000FC03FC0003FE0FFE003F0000FDE3FC0003FF07FF
      003F0000FCC7FE0FFFFF83FF801F0000FEC7FF07FFFFC1FFC00F0000FE0FFF83
      FFFFE0FFE0070000FF0FFFC1FFFFF07FFE030000FF0FFFE0FFFFF83FFF010000
      FF1FFFF0FFFFFC3FFF810000FF9FFFF9FFFFFE7FFFC10000FFBFFFFFFFFFFFFF
      FFF30000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFF
      FFFFFFFFFFFF0000DFFFFFFFFFFFFFFC00030000F3FFFFFFFFFFFFF880010000
      E8FFFFFFFFFFFFF880010000E07FFFF3F3CFCFF882010000F03FFFC3C3C3C3F8
      87010000F01FFF0303C0C0F88F810000F80FFC0003C000388A810000FC07F800
      03C0001882010000FE03780003C0001882010000FF003C0003C00038FE3D0000
      FF807F0303C0C0F800010000FFC03FC3C3C3C3F800010000FFE11FF3F3CFCFF8
      00030000FFE31FFFFFFFFFF8007F0000FFC03FFFFFFFFFF9FCFF0000FFE87FFF
      FFFFFFFC01FF0000FFFCFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000E000FFFFFFFFFFFFFFFF0000E000FFF9FFFFFFFF
      FFFF0000E000FFF0FFFE07FFE07F0000E000FFE07FF8F1FF801F0000E000FFC0
      3FF3FCFF000F0000E000FF801FE7FE7E00070000E000FF000FCFFF3C00030000
      E000FE0907DFFFBC00030000E000FC1983DFFFBC00030000E000FC39C3DFFFBC
      00030000E000FE79E7DFFFBC00030000E000FFF9FFCFFF3C00030000E000FFF9
      FFE7FE7E00070000E000FFF9FFF3FCFF000F0000E001FFF9FFF8F1FF801F0000
      E003FFF9FFFE07FFE07F0000E007FFF9FFFFFFFFFFFF0000E00FFFFFFFFFFFFF
      FFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF0FFFFFFF0000FFFFFFFF
      FFFF0FFFFFFF0000FF9EFFFFFFFFFFFFFFFF0000FF9EFFFFFFFE07FFFFFF0000
      FF983FFF83FE07FFFFFF0000FF9EFFFFFFFC03FFFFFF0000FF9EFFFFFFFC03FF
      FFFF0000FF9FFFFFFFF801FFFFFF0000FF9FFFFFFFF000FFFFFF0000C0003C00
      03F000FC00030000C0003C0003C0003C00030000FF9FFFFFFFC0003FFFFF0000
      FF9FFFFFFFF000FFFFFF0000FF9FFFFFFFF000FFFFFF0000FF9FFFFFFFF801FF
      FFFF0000FF9FFFFFFFF000FFFFFF0000FF9FFFFFFFE2047FFFFF0000FF9FFFFF
      FFF7FEFFFFFF0000FFFFFFFFFFFF9FFFFFFF0000FFFFFFFFFFFF9FFFFFFF0000
      FFFFFFF3F3FFFEFFFFFF0000FFFFFFC1E183FEFFF8FF0000FFFFFF80E181FABF
      C01F0000FFFFFF006180FC7F800F0000C3FC3E002380701F0F8700008FFF1C00
      13803C7F1FC700008FFF18000FC01ABE38E300008FFF180007E00EFE38E30000
      8FFF18FF87F006FE78F300001FFF887F07F803FE78F300001FFF8C3E0FFC01FE
      38E300008FFF1E1C1FFE00FE38E300008FFF1F083FFF007F18C300008FFF1F80
      7FFF803F0F0100008FFF1FC0FFFFC01F80010000C3FC3FC1FFFFE01FE03F0000
      FFFFFF83FFFFF01FFFFF0000FFFFFF07FFFFF81FF07F0000FFFFFF0FFFFFFC1F
      F07F0000FFFFFFBFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFF
      FFFFFDFFC0030000E7FFF81E00FFFCFF80010000C3FFFE7F87FFFE7F9FF90000
      C1FFFE7F8783F03F9FF90000E0FFFF7F8F81E03F9E790000F07FFF3F1FFCCE7F
      9E790000F83FFFBF0FFC9CFF98090000FC1FFF801FFE1DF998090000FE0FFF80
      3FFE3FF99E790000FF07FFDE3FFE3FF99C790000FF83FFCC7FFE1DF99C790000
      FFC1FFEC7FFC9CF99FF90000FFE0FFE0FFFCCE7980010000FFF07FF07F81E039
      C0030000FFF83FF0FF81F039FFFF0000FFFC3FF1FFFFFE79FFFF0000FFFE7FF9
      FFFFFCF800FF0000FFFFFFFBFFFFFDFC00FF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000F000FF000F83F81FFFFF0000E0007E000781F81F
      C7FF0000E3FC7E3FC780F81F83FF0000E7FE7E7FE780781F01FF0000E7FE7E7F
      E780381E00FF0000E7FE7E7FE7C0181C007F0000E7FE7E7FE7E00FFC003F0000
      E7FE7E7FE7F007FC003F0000E3FC7E3FC7F803FE003F0000E0007E0007FC01FF
      003F0000F000FF000FFE00FF801F0000FCFFFFCE3FFF007FC00F0000FCFFFFCF
      3FFF803FE0070000FCF3FFCF3FFFC01FFE030000FCF3FFCF3FFFE01FFF010000
      FC63FFC63FFFF01FFF810000FE07FFE07FFFF81FFFC10000FF0FFFF0FFFFFC1F
      FFF30000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFF
      FFFFFFFFFFFF0000FFFFFFFFFFFFFFFF33330000FFFFFFE07FFE07FF33330000
      C0003F801FF8F1FCFFFF0000C0003F000FF3FCFCFFFF0000CFFF3E0007E7FE7F
      FFF30000CFFF3C0003CFFF3FFFF30000CFFF3C0003DFFFBCFFFF0000CFFF3C00
      03DFFFBCFFFF0000CFFF3C0003DFFFBFFFF30000CFFF3C0003DFFFBFFFF30000
      CFFF3C0003CFFF3CFFFF0000CFFF3E0007E7FE7CFFFF0000C0003F000FF3FCFF
      FFF30000C0003F801FF8F1FFFFF30000FFFFFFE07FFE07FCCCCF0000FFFFFFFF
      FFFFFFFCCCCF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000E000FFFFFFFFFFFFFFFF0000E000FFFFFFDFFFFF
      FFFF0000E000FFCF9FF3FFFFFFFF0000E000FFC70FE8FFFC00030000E000FFC2
      07E07FFC00030000E000FFC007F03FFC00030000E000FF800FF01FFC00030000
      E000FF801FF80FFC00030000E000FF803FFC07FC00030000E000FF003FFE037C
      00030000E000FF000FFF003C00030000E000FF0007FF807C00030000E000FE00
      07FFC03C00030000E000FE003FFFE11C00030000E001FE03FFFFE31C00030000
      E003FE1FFFFFC03FFFFF0000E007FFFFFFFFE87FFFFF0000E00FFFFFFFFFFCFF
      FFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FF0FFFF0
      FFFFFFFFFFFF0000FC03FFC01FC0003C00030000F8F1FF8F1F88001C80130000
      F1FCFF3F8F88001C80130000E3FE7E7FC788201C80130000E7FE7E7FE788701C
      80130000C7FF3CFFE388F81C80130000CFFF3CFFF388A81CC0330000FFFF3CFF
      FF88201C7FE30000FFFF7EFFFF88201C00030000FFFE7E7FFF8FE3DCFFC30000
      FEFC7E3F7F80001CFC430000FCF8FF1F3F80001CFC430000F8E1FF871F80003C
      FC470000F007FFE00F8007FCFC4F0000F81FFFF81F9FCFFCFFDF0000FCFFFFFF
      3FC01FFC003F0000FEFFFFFF7FFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FF9FFFF1FFFF9FFF
      F9FF0000FF0FFFF0FFFF9FFFF0FF0000FE0FFFF07FFF9FFFE07F0000FC1FFFF8
      3FFF9FFFC03F0000F83FFFFC1FFF9FFF801F0000F07FFFFE0FFF9FFF000F0000
      E0FFFFFF07E79E3E09070000C0003C0003C39C3C19830000C0003C0003C1983C
      39C30000E0FFFFFF07E0907C79E70000F07FFFFE0FF000FFF9FF0000F83FFFFC
      1FF801FFF9FF0000FC1FFFF83FFC03FFF9FF0000FE0FFFF07FFE07FFF9FF0000
      FF0FFFF0FFFF0FFFF9FF0000FF8FFFF9FFFF9FFFF9FF0000FFFFFFFFFFFFFFFF
      FFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFF
      FFFFFFFFFFFF0000FFFFFFFFFFFF9FFFFFFF0000FFFFFFFFFFFF9FFFFFFF0000
      FFFFFFFFFFFF9FFFFFFF0000FF3F3CFCFFFF9FFFFFFF0000FC3C3C3C3FFF9FFF
      FFFF0000F0303C0C0FFF9FFFFFFF0000C0003C0003FF9FFFFFFF000080003C00
      01C0003C0003000080003C0001C0003C00030000C0003C0003FF9FFFFFFF0000
      F0303C0C0FFF9FFFFFFF0000FC3C3C3C3FFF9FFFFFFF0000FF3F3CFCFFFF9FFF
      FFFF0000FFFFFFFFFFFF9FFFFFFF0000FFFFFFFFFFFF9FFFFFFF0000FFFFFFFF
      FFFF9FFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000
      FFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFF0000E3FFFC00039FFE3C
      7FF90000E0FFFC00039FFC3C3FF90000E03FFC00039FF03C0FF90000E01FFC00
      039FC03C03F90000E007FC00039F003C00F90000E001FC00039E003C00790000
      E0007C000398003C00190000E0003C000390003C00090000E0003C000390003C
      00090000E0007C000398003C00190000E001FC00039E003C00790000E007FC00
      039F003C00F90000E01FFC00039FC03C03F90000E03FFC00039FF03C0FF90000
      E0FFFC00039FFC3C3FF90000E3FFFE00039FFE3C7FF90000FFFFFFFFFFFFFFFF
      FFFF0000FFFFFFFFFFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
end
