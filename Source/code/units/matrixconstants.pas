// ===================================================================
//
// (c) Paul Alan Freshney 2012-2022
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section

//
// ===================================================================


unit matrixconstants;


interface


type
  TViewShape       = (vsSquare, vsRadial, vsRadial3Q, vsSemiCircle, vsSemiCircleInverted);
  TMirrorMode      = (mmOff, mmHorizontal, mmVertical);
  TSoftwareMode    = (smAnimation, smFont);

  TMatrixMode      = (mtMono, mtBiSequential, mtBiBitplanes, mtRGB, mtRGB3BPP);

  TPixelShape      = (psSquare, psCircle, psRoundRect);
  TBrushSize       = (bsSmall, bsMedium, bsLarge);

  TCustomShape     = (csNone, csCircle, csBorders, csTriangle);

  TGradientOption  = (goOff, goVertical, goHorizontal);

  TLoadMode        = (lmNew, lmAppend, lmMergeBottomPriority, lmMergeTopPriority, lmMergeNewLayer, lmMergeCurrentLayer);

  TLoadData        = (ldUnknown,
                      ldLoadBlockStartHeader, ldLoadBlockStartDeadPixel, ldLoadBlockBegin, ldLoadBlockEnd, ldLoadBlockBeginLayout, ldLoadBlockEndLayout, ldLoadBlockStartColours,
                      ldLoadHeaderSource, ldLoadHeaderSourceLSB, ldLoadHeaderSourceDirection, ldLoadHeaderPadMode, ldLoadHeaderHexFormat, ldLoadHeaderHexOutput, ldLoadHeaderBrackets,
                      ldLoadHeaderDataSource, ldLoadHeaderOrientation, ldLoadHeaderScanDirection, ldLoadHeaderLSB, ldLoadHeaderLanguage, ldLoadHeaderNumberFormat,  ldLoadHeaderNumberSize, ldLoadHeaderLineContent, ldLoadHeaderLineCount, ldLoadHeaderRGBMode, ldLoadHeaderRGBChangePixels, ldLoadHeaderRGBChangeColour, ldLoadHeaderOptimise,
                      ldLoadHeaderMatrixComment, ldLoadHeaderRGBBackground, ldLoadHeaderASCIIIndex, ldLoadHeaderAutomationFile,
                      ldLoadHeaderRGBBrightness,
                      ldLoadHeaderEnd,
                      ldLoadHeaderPreviewEnabled, ldLoadHeaderPreviewSize, ldLoadHeaderPreviewView, ldLoadHeaderPreviewVoid, ldLoadHeaderPreviewOffset, ldLoadHeaderPreviewOffsetDir, ldLoadHeaderPreviewIncRadially,
                      ldLoadHeaderLayerCount,
                      ldLoadMatrixWidth, ldLoadMatrixHeight, ldLoadMatrixData, ldLoadMatrixLocked,
                      ldLoadDeadPixelData,
                      ldLoadLayoutName, ldLoadLayoutWidth, ldLoadLayoutHeight, ldLoadLayoutLocked,
                      ldLoadColoursCustom, ldLoadCOloursDraw0, ldLoadCOloursDraw1, ldLoadCOloursDraw2, ldLoadColoursPaletteHistory);

  THexFormat       = (hfEnabled, hfDisabled);
  THexPrefix       = (hpNone, hpDollar, hpZeroX, hpAmpersand);
  TBinaryPrefix    = (bpNone, bpPercent, bpZeroB);
  TPadFormat       = (pfAuto, pf8Bits, pf16Bits, pf24Bits, pf32Bits, pf40Bits, pf48Bits, pf56Bits, pf64Bits);
  TBracketStyle    = (bsNone, bsNormal, bsCurly, bsSquare);

const
  _MaxWidth                = 256;
  _MaxHeight               = 256;

  modeFlipAll              = 0;
  modeMirrorAll            = 1;
  modeInvertAll            = 2;
  modeGradientAll          = 3;

  modeFlip                 = 0;
  modeMirror               = 1;
  modeInvert               = 2;

  modeScrollLeft           = 0;
  modeScrollRight          = 1;
  modeScrollUp             = 2;
  modeScrollDown           = 3;
  modeWipeHorizontalOut    = 4;
  modeWipeHorizontalIn     = 5;
  modeWipeVerticalOut      = 6;
  modeWipeVerticalIn       = 7;
  modeWipeLeftToRight      = 8;
  modeWipeRightToLeft      = 9;
  modeWipeUpToDown         = 10;
  modeWipeDownToUp         = 11;

  modeRevealLeftRight      = 0;
  modeRevealRightLeft      = 1;
  modeRevealTopBottom      = 2;
  modeRevealBottomTop      = 3;
  modeRevealCentreOut      = 4;
  modeRevealCentreIn       = 5;

  modeSplitScrollLeftRight = 0;
  modeSplitScrollRightLeft = 1;
  modeSplitScrollUpDown    = 2;
  modeSplitScrollDownUp    = 3;

  modeAlternateScrollUpDown= 0;
  modeAlternateScrollDownUp= 1;

  modeScrollRowLeft        = 0;
  modeScrollRowRight       = 1;

  modeScrollColumnUp       = 0;
  modeScrollColumnDown     = 1;

  modeRotateCW             = 0;
  modeRotateACW            = 1;

  customShapeNone          = 0;
  customShapeCircle        = 1;
  customShapeJustBorders   = 2;
  customShapeTriangle      = 3;

  previewPixelSizeAuto     = -1;

  CRoundRectCoeff          = 4;

  CTopOffset               = 4;
  CLeftOffset              = 4;


var
  DrawModes                  : array[0..21] of string = ('Draw', 'Filled box', 'Empty box', 'Line', 'Font', 'Empty Circle', 'Filled circle', 'Random brush',
                                                         'Multi-draw', 'Colour picker', 'Copy brush', 'Paste brush', 'Gradient brush',
                                                         'Flood fill',
                                                         'Spiral', 'Ring', 'Split ring', 'Petals', 'Grid', 'Pyramid', 'Left triangle', 'Right triangle');


implementation


end.
