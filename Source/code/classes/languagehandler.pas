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


unit languagehandler;


interface


uses SysUtils, Classes, Forms, System.UITypes, dialogs;


{$REGION language_constants}

const
  kExitLMS = 0;
  kAreYouSure = 1;
  kClearAllFramesQ = 2;
  kDoYouWishToContinue = 3;
  kAddBlankFrames = 4;
  kHowManyFrames = 5;
  kInvalidNumberFramesToAdd = 6;
  kCustom = 7;
  kAcceptDeadPixels = 8;
  kSetDeadPixels = 9;
  kOpeningNewMatrixWillClearCurrentProject = 10;
  kOpeningNewProjectWillClearCurrentProject = 11;
  kPresetFileName = 12;
  kName = 13;
  kCannotFindFont = 14;
  kReallyLoadThisPreset = 15;
  kCannotFindPresetFile = 16;
  kPlayAnimation = 17;
  kMatrixComment = 18;
  kAddCommentMatrix = 19;
  kASCIICode = 20;
  kStartASCIICodeFontMode = 21;
  kReallyClearEverything = 22;
  kBackgroundColourHasChanged = 23;
  kChangeAllBackgroundPixels = 24;
  kAutosavedCurrentMatrix = 25;
  kClearAllUserMatrixBuffers = 26;
  kDoesNotClearAnimationFrames = 27;
  kClearAllFramesFromTheSelectedLayer = 28;
  kThisCannotBeUndone = 29;
  kClearAllFramesAndLayers = 30;
  kClearAllFrames = 31;
  kBitmapImages = 32;
  kSaveGradient = 33;
  kMyGradient = 34;
  kAutosavedCurrentMatrixRange = 35;
  kHelpFileNotFound = 36;
  kShortcutHelpFileNotFound = 37;
  kReallyLoadThisGradient = 38;
  kCannotFindGradientFile = 39;
  kSetRowToSelectedColour = 40;
  kGradientFromTopBottom = 41;
  kSetColumnToSelectedColour = 42;
  kGradientFromLeftRight = 43;
  kData = 44;
  kFlattenAllLayersQ = 45;
  kUniqueColoursCurrentFrame = 46;
  kUniqueColoursAnimation = 47;
  kClearLayer = 48;
  kLEDMatrixStudioProjects = 49;
  kLEDMatrixStudioFont = 50;
  kLEDMatrixStudioIgnorePixelFiles = 51;
  kAreYouSureYouWantToDeleteTheCurrentMatrix = 52;
  kBrushDoesNotMatchCurrentMatrixType = 53;
  kNoSourcTargetColoursSelected = 54;
  kFile = 55;
  kEdit = 56;
  kView = 57;
  kProject = 58;
  kDraw = 59;
  kFrames = 60;
  kLayers = 61;
  kColours = 62;
  kMemories = 63;
  kTools = 64;
  kNew = 65;
  kOpen = 66;
  kRecentFiles = 67;
  kImportFromBitmap = 68;
  kImportFromGIF = 69;
  kImportIntoCurrentFrame = 70;
  kAppendToAnimation = 71;
  kMerge = 72;
  kSave = 73;
  kSaveAs = 74;
  kSaveSingleFrameAs = 75;
  kSaveRangeAs = 76;
  kSaveLEDMatrixStudioFont = 77;
  kExportToImages = 78;
  kExportAnimationToBitmap = 79;
  kExportAnimationToGIF = 80;
  kPreferences = 81;
  kExit = 82;
  kUndo = 83;
  kRedo = 84;
  kCopy = 85;
  kCopyFromPrevious = 86;
  kCopyMultiple = 87;
  kPaste = 88;
  kPasteSpecial = 89;
  kPasteShiftLeft = 90;
  kPasteShiftRight = 91;
  kPasteShiftUp = 92;
  kPasteShiftDown = 93;
  kBrushActions = 94;
  kRotateAnticlockwise = 95;
  kRotateClockwise = 96;
  kFlip = 97;
  kMirror = 98;
  kInvert = 99;
  kPasteEveryFrame = 100;
  kPasteEveryFrameTransparent = 101;
  kShiftLeft = 102;
  kShiftRight = 103;
  kShiftUp = 104;
  kShiftDown = 105;
  kEditComment = 106;
  kShowAnimationToolbar = 107;
  kPaletteGradientToolbar = 108;
  kUndoToolbar = 109;
  kWorkingAreaBackgroundColour = 110;
  kBlack = 111;
  kDarkGreyDefault = 112;
  kGrey = 113;
  kGreen = 114;
  kPurple = 115;
  kRed = 116;
  kWhite = 117;
  kFontMode = 118;
  kChangeStartASCIICode = 119;
  kPreviousFrame = 120;
  kNextFrame = 121;
  kGrid = 122;
  kPreview = 123;
  kPreviewSize = 124;
  kIncrementRadially = 125;
  kPreviewView = 126;
  kSquare = 127;
  kRadial = 128;
  kRadialThreeQuarters = 129;
  kSemicircle = 130;
  kSemicircleIinverted = 131;
  kPreviewVoidRRadialSsemicircle = 132;
  kPreviewOffsetRadialSemicircle = 133;
  kReverse = 134;
  kPopoutPreview = 135;
  kClearAllFramesCurrentLayer = 136;
  kClearAllFramesAllLayers = 137;
  kClearAllFramesWithGradient = 138;
  kFlipAllFrames = 139;
  kMirrorAllFrames = 140;
  kInvertAllFrames = 141;
  kApplyGradientToAllFrames = 142;
  kIgnoredPixels = 143;
  kSetIgnoredPixels = 144;
  kSetFromPattern = 145;
  kClearAllIgnoredPixels = 146;
  kSavePattern = 147;
  kLoadPattern = 148;
  kFadeFirstLast = 149;
  kExport = 150;
  kCodeTemplates = 151;
  kUnlockAllFrames = 152;
  kLockAllFrames = 153;
  kToggleLockStatusRange = 154;
  kFreehandBrush = 155;
  kCustomBrush = 156;
  kCopyPaste = 157;
  kFilledRectangle = 158;
  kEmptyRectangle = 159;
  kFilledCircle = 160;
  kEmptyCircle = 161;
  kLine = 162;
  kMultidrawOnEachFrame = 163;
  kFill = 164;
  kText = 165;
  kGradientBrush = 166;
  kGradient = 167;
  kRandom = 168;
  kColourPicker = 169;
  kPatternSpiral = 170;
  kPatternCircle = 171;
  kPatternSplitRing = 172;
  kPatternPetals = 173;
  kPatternGrid = 174;
  kPatternPyramid = 175;
  kPatternLeftTriangle = 176;
  kPatternRightTriangle = 177;
  kAddFrame = 178;
  kAddFrameCopy = 179;
  kAddFrameMultiple = 180;
  kDeleteFrame = 181;
  kDeleteMultipleFrames = 182;
  kToggleLayoutPanel = 183;
  kClearLayerAllFrames = 184;
  kFlattenAllLayers = 185;
  kChangeColoursInTheFrameLayer = 186;
  kChangeColoursGloballyCurrentLayer = 187;
  kChangeColoursGloballyAllLayersFrames = 188;
  kCountColours = 189;
  kCurrentFrame = 190;
  kAnimation = 191;
  kCopyCurrentTo = 192;
  kMemoryC = 193;
  kRestoreCurrentFrom = 194;
  kExportUserMemories = 195;
  kClearAllUserMemories = 196;
  kAutosave = 197;
  kAutosaveInterval = 198;
  kOpenAutosaveFolder = 199;
  kAutomate = 200;
  kOptimiseData = 201;
  kFontViewer = 202;
  kHelp = 203;
  kShowShortcutKeys = 204;
  kExampleCode = 205;
  kCheckForUpdates = 206;
  kWebsite = 207;
  kAbout = 208;
  kMinutes = 209;
  kGenerateCode = 210;
  kClear = 211;
  kRotate = 212;
  kColour = 213;
  kGradients = 214;
  kLoad = 215;
  kSaveCurrent = 216;
  kFonts = 217;
  kPresets = 218;
  kPixelSize = 219;
  kTiny = 220;
  kSmall = 221;
  kMedium = 222;
  kLarge = 223;
  kMassive = 224;
  kUltra = 225;
  kXUltra = 226;
  kAuto = 227;
  kPixelShape = 228;
  kRound = 229;
  kSquareR = 230;
  kRandomness = 231;
  kNone = 232;
  kHorizontal = 233;
  kVertical = 234;
  kSelectColour = 235;
  kOK = 236;
  kCancel = 237;
  kBrushSize = 238;
  kPlaybackSpeed = 239;
  kSetCustomSpeed = 240;
  kPalette = 241;
  kFromShades = 242;
  kFromCustom = 243;
  kCopyBrush = 244;
  kDeleteCurrentLayer = 245;
  kChangeLayerName = 246;
  kNewName = 247;
  kToggleFrameLockStatus = 248;
  kStart = 249;
  kEnd = 250;
  kLock = 251;
  kFrom = 252;
  kToC = 253;
  kColourChanger = 254;
  kWarningThisActionCannotBeUndone = 255;
  kFirstThirtyTwoColoursFromTheCurrentAnimation = 256;
  kCreate = 257;
  kCopyFrom = 258;
  kExtraLayersIncreaseApplicationMemoryUsage = 259;
  kAddNewLayer = 260;
  kCopyMultipleFrames = 261;
  kCopyTo = 262;
  kDestinationFramesMustExist = 263;
  kSource = 264;
  kDestination = 265;
  kAllLayers = 266;
  kWarningYouWillLoseData = 267;
  kCustomPlaybackSpeed = 268;
  k1000ms1Second = 269;
  kFileName = 270;
  kStartFrame = 271;
  kMergeInToAnimationBottomHasPriority = 272;
  kMergeInToAnimationTopHasPriority = 273;
  kMergeInToNewLayer = 274;
  kMergeInToCurrentLayer = 275;
  kPlatforms = 276;
  kCodeTemplate = 277;
  kSettings = 278;
  kScan = 279;
  kLSB = 280;
  kFormat = 281;
  kNumbers = 282;
  kGrouping = 283;
  kOutput = 284;
  kRGB = 285;
  kMinWidth = 286;
  kMaxWidth = 287;
  kMinHeight = 288;
  kMaxHeight = 289;
  kCopyToClipboard = 290;
  kClose = 291;
  kDirection = 292;
  kSelectFont = 293;
  kViewInRGBMode = 294;
  kCharacter = 295;
  kSizeCoefficientHelpText = 296;
  kBackground = 297;
  kCircle = 298;
  kDeleteMultipleFramesC = 299;
  kSetIgnoredPixelsFromPattern = 300;
  kUseCustomShape = 301;
  kBorder = 302;
  kPixels = 303;
  kNoCustomShape = 304;
  kFrameBorderNoCentre = 305;
  kSaveARangeOfFrames = 306;
  kBuiltInTypes = 307;
  kGradientHorizontalUp = 308;
  kGradientHorizontalDown = 309;
  kGradientVerticalRight = 310;
  kGradientVerticalLeft = 311;
  kGradientDiagonalUpRight = 312;
  kGradientDiagonalDownRight = 313;
  kChevronUp = 314;
  kChevronDown = 315;
  kChevronRight = 316;
  kChevronLeft = 317;
  kCheckerboard1x1 = 318;
  kCheckerboard2x2 = 319;
  kCheckerboard3x3 = 320;
  kCheckerboard4x4 = 321;
  kExportMatrixData = 322;
  kNewBrush = 323;
  kMatrixOptions = 324;
  kMisc = 325;
  kClearRecentFilesList = 326;
  kTriangle = 327;
  kCommon = 328;
  kAll = 329;
  kStartWith = 330;
  kAnimationFrames = 331;
  kClearAllAnimationData = 332;
  kFromPreset = 333;
  kType = 334;
  kWidth = 335;
  kHeight = 336;
  kResetToDefaults = 337;
  kNA = 338;
  kColumns = 339;
  kRows = 340;
  kDecimal = 341;
  kBinary = 342;
  kHex = 343;
  kDisabled = 344;
  kUnlimited = 345;
  kNoCodeTemplatesFound = 346;
  kErrorLoadingTemplate = 347;
  kSingleImage = 348;
  kSelect = 349;
  kNoImagesSelected = 350;
  kMultipleImages = 351;
  kFirstImage = 352;
  kFirstFrame = 353;
  kIndexLength = 354;
  kIndexLengthExample = 355;
  kPattern = 356;
  kImportSettings = 357;
  kRGBImport = 358;
  kCreateNewMatrixClearsAllData = 359;
  kFramesToImport = 360;
  kFrameWidth = 361;
  kFrameHeight = 362;
  kForNonRGBImport = 363;
  kExportCommaSeparated  = 364;
  kExportPICAXEEEPROM = 365;
  kExportCCpp1Dimensional = 366;
  kExportCCpp2Dimensional = 367;
  kExportCCppFastLED = 368;
  kExportPython1Dimensional = 369;
  kExportPython2Dimensional = 370;
  kExportMicrochip = 371;
  kExportPascal = 372;
  kCode = 373;
  kFramexs = 374;
  kRowxs = 375;
  kOptimiseOutputIfPossible = 376;
  kEachLineOfOutput = 377;
  kFrameC = 378;
  kBytesC = 379;
  kRow = 380;
  kRGBColourFormat = 381;
  kChangeBackgroundPixels = 382;
  kBrightness = 383;
  kLeastSignificantBitLSB = 384;
  kLeft = 385;
  kRight = 386;
  kExportFormat = 387;
  kIncludeExampleCode = 388;
  kNumberFormat = 389;
  kNumberGrouping = 390;
  k8BitsOneBytePerColour = 391;
  k32Bits = 392;
  k8BitSwapNybbles = 393;
  k16BitSwapBytes = 394;
  kBuildCode = 395;
  kAutoBuild = 396;
  kFileContents = 397;
  kEntireAnimation = 398;
  kFrameOnePerFile = 399;
  kProfiles = 400;
  kColourRepresenting1 = 401;
  kColourRepresenting2 = 402;
  kColourRepresenting3 = 403;
  kColourRepresentingLightbox = 404;
  kOFF = 405;
  kON = 406;
  kSelector = 407;
  kLightBox = 408;
  kExportAutoGenerateOnStartupLimiter = 409;
  kMaxPixels = 410;
  kPreviewShowFirst = 411;
  kLines = 412;
  kUpdateCheck = 413;
  kInstalledVersion = 414;
  kLatestAvailableOnline = 415;
  kAutomationFiles = 416;
  kHistory = 417;
  kDate = 418;
  kVersion = 419;
  kUnableToConnectTomaximumoctopuscom = 420;
  kNoNewVersionIsAvailable = 421;
  kANewVersionIsAvailable = 422;
  kError = 423;
  kSure24x16BoardNotAvailableInRGBMode = 424;
  kFrame = 425;
  kMemory = 426;
  kSingleColour  = 427;
  kBiColourSequential = 428;
  kBiColourBitplanes = 429;
  kUsing = 430;
  kRGB3BPP = 431;
  kLayer = 432;
  kColourLists = 433;
  kBrushes = 434;
  kYouMustAdCcoloursBelowBeforeYouCanGenerateAPattern = 435;
  kSyncAll = 436;
  kLanguage = 437;
  kColumn = 438;
  kAnimationFrame = 439;
  kBytes = 440;
  kLSBatTop = 441;
  kLSBatBottom = 442;
  kLSBatLeft = 443;
  kLSBatRight   = 444;
  k8Bits = 445;
  k16Bits = 446;
  k8BitsNybblesSwapped = 447;
  k16BitsBytesSwapped = 448;
  k64Bits = 449;
  kLeftToRight = 450;
  kRightToLeft = 451;
  kSure24x16 = 452;
  kTopToBottom = 453;
  kBottomToTop = 454;
  kComment = 455;
  kOriginalFile = 456;
  kFontCharacters = 457;
  kMemoryBuffers = 458;
  kTo = 459;
  kAlternateDownUp = 460;
  kAlternateUpDown = 461;
  kAlternateLeftRight = 462;
  kAlternateRightLeft = 463;
  kBuildingDataPleaseWait = 464;
  kTop = 465;
  kBottom = 466;
  kColumnxs = 467;
  kReallyDeleteThisProfile = 468;
  kCouldntDeleteProfile = 469;
  kProfileName = 470;
  kErrorLoadingProfile = 471;
  kActions = 472;
  kAvailableActions = 473;
  kWipe = 474;
  kReveal = 475;
  kProcess = 476;
  kScroll = 477;
  kJiggle = 478;
  kBounce = 479;
  kAlternate = 480;
  kBrushNo1 = 481;
  kBrushNo2 = 482;
  kColourCycle = 483;
  kEveryFrame = 484;
  kAccessWithLEDDataIndex = 485;
  kCyclingLinear = 486;
  kCyclingBounce = 487;
  kProcessingOptions = 488;
  kUseFirstFrameAsSource = 489;
  kEachFrameIndividually = 490;
  kEachFrameIndividuallyIncrement = 491;
  kEraseWipeJiggleModes = 492;
  kFrameStart = 493;
  kFrameEnd = 494;
  kActionList = 495;
  kProcessedOnEachFrame = 496;
  kRemove = 497;
  kPostProcessing = 498;
  kPostProcessingHelp = 499;
  kAutomateHelp = 500;
  kOptions = 501;
  kOptimise = 502;
  kTransparent = 503;
  kCopyColoursToSource = 504;
  kSourceColourxs = 505;
  kTargetColours = 506;
  kSkip = 507;
  kFirst32CcoloursFromCurrentAnimation = 508;
  kColourCyclingHelp = 509;
  kInvalidFrameStartFrameEndValues = 510;
  kNoCustomBrush1Selected = 511;
  kNoCustomBrush2Selected = 512;
  kBrush = 513;
  kColourCycling = 514;
  kCopyOutput = 515;
  kOutputOptions = 516;
  kDataSize = 517;
  kPerRow = 518;
  kNPModeMono = 519;
  kNPModeBiSequential = 520;
  kNPModeBiBitplane = 521;
  kNPModeRGB = 522;
  kNPModeRGB3BPP = 523;
  kSecond = 524;
  kSeconds = 525;
  kPreset = 526;
  kFilledBox = 527;
  kEmptyBox = 528;
  kFont = 529;
  kRandomBrush = 530;
  kMultiDraw = 531;
  kPasteBrush = 532;
  kFloodFill = 533;
  kSpiral = 534;
  kRing = 535;
  kSplitRing = 536;
  kPetals = 537;
  kPyramid = 538;
  kLeftTriangle = 539;
  kRightTriangle = 540;
  kBottomLayer = 541;
  kErrorWhileLoadingThisGIF = 542;
  kGIFDimensionsAreTooLarge = 543;
  kQuickDataToolbar = 544;
  kSimpleExport = 545;
  kHexFormat = 546;
  kErrorLoadingProject = 547;
  kGradientFlip = 548;
  kAnimationSpeed = 549;
  kAnimationSpeedHelp = 550;

  kLanguageConstantCount = 551;
  
  
{$ENDREGION}


type
  TLanguageHandler = class
  private
    FAppPath : string;
    FLanguage : string;
    FLoaded : boolean;

    function LoadLanguage: boolean;

  public
    Text : TStringList;

    constructor Create(aLanguage : string);
    destructor  Destroy; Override;

    property LanguageLoaded : boolean read FLoaded;
  end;


var
 GLanguageHandler : TLanguageHandler;


implementation


constructor TLanguageHandler.Create(aLanguage : string);
begin
  FAppPath  := ExtractFilePath(Application.ExeName);
  FLanguage := aLanguage;

  Text := TStringList.Create;

  FLoaded := LoadLanguage;
end;


destructor TLanguageHandler.Destroy;
begin
  Text.Free;
end;


// uncomment the code below to show the text ID instead of the actual text
// this is very useful if you're doing some debugging of your text
function TLanguageHandler.LoadLanguage: boolean;
var
  tf : TextFile;
  lFileName : string;
  s : string;
  t : integer;

begin
  Result := True;

  lFileName := FAppPath + '\language\' + FLanguage + '.txt';

  if FileExists(lFileName) then begin
//    t := 0;

    AssignFile(tf, lFileName);
    Reset(tf);

    while not(eof(tf)) do begin
      Readln(tf, s);

     Text.Add(s);
     // Text.Add(IntToStr(t)); // swap-out the above line with this one if you want to debug, and uncomment the inc(t) below

     // inc(t);
    end;

    CloseFile(tf);

    // ======================================================================

    if (Text.Count <> kLanguageConstantCount) then begin
      MessageDlg('Check your language files are up-to-date. They appear to have an incorrect number of entries!' + #13#10 + #13#10 + '<install>\language\', mtError, [mbOK], 0);

      for t := Text.Count to kLanguageConstantCount + 1 do
        Text.Add('missing #' + IntToStr(Text.Count));

      Result := False;
    end;

    // ======================================================================
  end
  else begin
    MessageDlg('Language files missing!' + #13#10 + #13#10 + '<install>\language\', mtError, [mbOK], 0);

    Result := False;
  end;
end;


end.
