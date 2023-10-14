// ===================================================================
//
// (c) Paul Alan Freshney 2012-2023
// www.freshney.org  paul@freshney.org  maximumoctopus.com
//
// https//github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section
//
// ===================================================================

unit fileconstants;


interface


const

  // =================================================================
  // == Generic ======================================================
  // =================================================================

  kDataBlockStart = '{';
  kDataBlockEnd = '}';

  // =================================================================
  // == Animation project file (.leds) ===============================
  // =================================================================

  kFilePrefixMono = 'anim';
  kFilePrefixBiSequential = 'anim2';
  kFilePrefixBiBitPlanes = 'anim3';
  kFilePrefixRGB = 'anim4';
  kFilePrefixRGB3BPP = 'anim5';
  
  kFileHeaderColours = 'colours';  
  kFileHeaderDeadPixel = 'deadpixel';
  kFileHeaderFontHeader = 'fontheader';
  kFileHeaderFontRGB = 'fontRGB';  
  kFileHeaderHeader = 'header';
  kFileHeaderLayer = 'layer';
  
  kFramePrefixMono = 'frame';
  kFramePrefixBiSequential = 'frame2';
  kFramePrefixBiBitPlanes = 'frame3';
  kFramePrefixRGB = 'frame4';
  kFramePrefixRGB3BPP = 'frame5';

  kAnimPadMode = '1';
  kAnimHexFormat = '2';
  kAnimHexOutput = '3';
  kAnimBrackets = '4';

  kAnimPreviewEnabled = '5';
  kAnimPreviewSize = '6';
  kAnimPreviewView = '7';
  kAnimPreviewVoid = '8';
  kAnimPreviewOffset = '9';
  kAnimPreviewDirection = '0';
  kAnimPreviewIncRadially = '!';
  
  kAnimDataSource = 'a';
  kAnimSourceLSB = 'b';
  kAnimSourceDirection = 'c';  

  kAnimSource = 'd';
  kAnimOrientation = 'e';
  kAnimScanDirection = 'f';
  kAnimLSB = 'g';
  kAnimLanguage = 'h';
  kAnimNumberFormat = 'i';
  kAnimNumberSize = 'j';
  kAnimLineContent = 'k';
  kAnimLineCount = 'l';

  kAnimRGBMode = 'm';
  kAnimRGBChangePixels = 'n';
  kAnimRGBChangeColour = 'o';
  kAnimRGBBrightness = 'q';

  kAnimOptimise = 'p';

  kAnimAutomationFileName = 'w';
  kAnimComment = 'x';
  kAnimASCIIIndex = 'y';
  kAnimRGBBackground = 'z';
  kAnimFrameRange = '?';
  kAnimLayerCount = '%';

  kAnimColoursCustom = 'c';
  kAnimColoursPaletteHistory = 'p';

  kAnimColoursLeft = 'l';
  kAnimColoursMiddle = 'm';
  kAnimColoursRight = 'r';

  kAnimLayerName = 'n';
  kAnimLayerWidth = 'w';
  kAnimLayerHeight = 'h';
  kAnimLayerLocked = 'l';

  kAnimWidth = 'w';
  kAnimHeight = 'h';
  kAnimRowData = 'r';
  kAnimFrameLocked = 'p';

  kAnimDeadPixelData = 'p';
  
  kAnimBrushColours = 'c';
  
  kAnimBlockEnd = '}';

  // =================================================================
  // == Automation File (.automation) ================================
  // =================================================================

  kFileHeaderData = 'data';
  kFileHeaderActions = 'actions';
  kFileHeaderPostProcessing = 'postprocessing';
  kFileHeaderBrush1 = 'brush1';
  kFileHeaderBrush2 = 'brush2';
  kFileHeaderSource = 'source';
  kFileHeaderTarget = 'target';

  kAutomationProcessMode = 'v';

  kAutomationStartFrame = 'w';
  kAutomationEndFrame = 'x';

  kAutomationErase = 'y';

  kAutomationActionItem = 'r';
  kAutomationPostProcessingItem = 'r';

  kAutomationBrushColour = 'a';
  kAutomationBrushTransparent = 'b';

  kAutomationBrushRowData = 'r';

  kAutomationColor = 'c';
  
  // =================================================================
  // == Brush ========================================================
  // =================================================================  
  
  kBrushPrefixMono = 'brush';
  kBrushPrefixBiSequential = 'brush2';
  kBrushPrefixBiBitPlanes = 'brush3';
  kBrushPrefixRGB = 'brush4';
  kBrushPrefixRGB3BPP = 'brush5';  
  
  // =================================================================
  // == Colours File (.colours) ======================================
  // =================================================================

  kColoursData = 'col';
  
  // =================================================================
  // == Export Profile (.ledsexport/.ledsexportrgb/.ledsexportrgb3bpp)
  // =================================================================
    
  kExportSource = 'a';
  kExportOrientation = 'b';
  kExportLSB = 'c';
  kExportLanguage = 'd';
  kExportNumberFormat = 'e';
  kExportNumberSize = 'f';
  kExportScanDirection = 'g';
  kExportLineContent = 'h';
  kExportLineCount = 'i';
  kExportRGBMode = 'r';
  kExportRGBChangePixels = 's';
  kExportRGBChangeColour = 't';
  kExportRGBBrightness = 'u';
  
  kExportMinWidth = 'v';
  kExportMaxWidth = 'w';
  kExportMinHeight = 'y';
  kExportMaxHeight = 'z';  
  
  kExportInformation = '!';

  kExportBinarySource = '1';
  kExportBinaryOrientation = '2';
  kExportBinaryLSB = '3';
  kExportBinaryScanDirection = '4';
  kExportBinaryRGBMode = '5';
  kExportBinaryRGBChangePixels = '6';
  kExportBinaryRGBChangeColour = '7';
  kExportBinaryRGBBrightness = '8';
  kExportBinaryFileContents = '9';
  
  // =================================================================
  // == RGB Font (.ledsfont) =========================================
  // =================================================================

  kFontPrefixMono = 'font';
  kFontPrefixBiSequential = 'font2';
  kFontPrefixBiBitPlanes = 'font3';
  kFontPrefixRGB = 'font4';
  kFontPrefixRGB3BPP = 'font5';  

  kFontPrefixChar = 'char';

  kRGBFontWidth = 'w';
  kRGBFontHeight = 'h';
  kRGBFontData = 'c';
  
  // =================================================================
  // == Gradient (.ledsgradient) =====================================
  // =================================================================  

  kGradientFileHeader = 'gradient';

  kGradientColour = 'g';
  
  // =================================================================
  // == Matrix Preset (.ledspreset) ================================
  // =================================================================    

  kMatrixPresetHeader = 'preset';

  kMatrixPresetWidth = 'w';
  kMatrixPresetHeight = 'h';
  kMatrixPresetPixelSize = 'e';
  kMatrixPresetMatrixMode = 'm';
  kMatrixPresetPixelShape = 's';

  // =================================================================
  // == Preset (.ledspreset) =========================================
  // =================================================================

  kPresetWidth = 'w';
  kPresetHeight = 'h';
  kPresetPixelSize = 'e';
  kPresetMatrixMode = 'm';
  kPresetPixelShape = 's';


implementation


end.
