// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2024
//   www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
//   https://github.com/MaximumOctopus/LEDMatrixStudio
//
//   https://maximumoctopus.hashnode.dev/
//
//   C++ Rewrite October 11th 2023
//
// ===================================================================

#pragma once


	// =================================================================
	// == Generic ======================================================
	// =================================================================

	static const wchar_t kDataBlockStart = L'{';
	static const wchar_t kDataBlockEnd = L'}';

	static const std::wstring kDataBlockStartS = L"{";
	static const std::wstring kDataBlockEndS = L"}";

	// =================================================================
	// == Animation project file (.leds) ===============================
	// =================================================================

	static const std::wstring kFilePrefixMono = L"anim";
	static const std::wstring kFilePrefixBiSequential = L"anim2";
	static const std::wstring kFilePrefixBiBitPlanes = L"anim3";
	static const std::wstring kFilePrefixRGB = L"anim4";
	static const std::wstring kFilePrefixRGB3BPP = L"anim5";

	static const std::wstring kFileHeaderColours = L"colours";
	static const std::wstring kFileHeaderIgnoredPixel = L"deadpixel";   // DO NOT EDIT THIS
	static const std::wstring kFileHeaderFontHeader = L"fontheader";
	static const std::wstring kFileHeaderFontRGB = L"fontRGB";
	static const std::wstring kFileHeaderHeader = L"header";
	static const std::wstring kFileHeaderLayer = L"layer";

	static const std::wstring kFramePrefixMono = L"frame";
	static const std::wstring kFramePrefixBiSequential = L"frame2";
	static const std::wstring kFramePrefixBiBitPlanes = L"frame3";
	static const std::wstring kFramePrefixRGB = L"frame4";
	static const std::wstring kFramePrefixRGB3BPP = L"frame5";

	static const std::wstring kAnimExportOptionsNotSet = L"-:()";

	static const wchar_t kAnimPadMode = L'1';
	static const wchar_t kAnimHexFormat = L'2';
	static const wchar_t kAnimHexOutput = L'3';
	static const wchar_t kAnimBrackets = L'4';

	static const wchar_t kAnimPreviewEnabled = L'5';
	static const wchar_t kAnimPreviewSize = L'6';
	static const wchar_t kAnimPreviewView = L'7';
	static const wchar_t kAnimPreviewVoid = L'8';
	static const wchar_t kAnimPreviewOffset = L'9';
	static const wchar_t kAnimPreviewDirection = L'0';
	static const wchar_t kAnimPreviewIncRadially = L'!';

	static const wchar_t kAnimDataSource = L'a';
	static const wchar_t kAnimSourceLSB = L'b';
	static const wchar_t kAnimSourceDirection = L'c';

	static const wchar_t kAnimSource = L'd';
	static const wchar_t kAnimOrientation = L'e';
	static const wchar_t kAnimScanDirection = L'f';
	static const wchar_t kAnimLSB = L'g';
	static const wchar_t kAnimLanguage = L'h';
	static const wchar_t kAnimNumberFormat = L'i';
	static const wchar_t kAnimNumberSize = L'j';
	static const wchar_t kAnimLineContent = L'k';
	static const wchar_t kAnimLineCount = L'l';

	static const wchar_t kAnimRGBMode = L'm';
	static const wchar_t kAnimRGBChangePixels = L'n';
	static const wchar_t kAnimRGBChangeColour = L'o';
	static const wchar_t kAnimRGBBrightness = L'q';

	static const wchar_t kAnimOptimise = L'p';

	static const wchar_t kAnimAutomationFileName = L'w';
	static const wchar_t kAnimComment = L'x';
	static const wchar_t kAnimASCIIIndex = L'y';
	static const wchar_t kAnimRGBBackground = L'z';
	static const wchar_t kAnimFrameRange = L'?';
	static const wchar_t kAnimLayerCount = L'%';

	static const wchar_t kAnimColoursCustom = L'c';
	static const wchar_t kAnimColoursPaletteHistory = L'p';

	static const wchar_t kAnimColoursLeft = L'l';
	static const wchar_t kAnimColoursMiddle = L'm';
	static const wchar_t kAnimColoursRight = L'r';

	static const wchar_t kAnimLayerName = L'n';
	static const wchar_t kAnimLayerWidth = L'w';
	static const wchar_t kAnimLayerHeight = L'h';
	static const wchar_t kAnimLayerLocked = L'l';

	static const wchar_t kAnimWidth = L'w';
	static const wchar_t kAnimHeight = L'h';
	static const wchar_t kAnimRowData = L'r';
	static const wchar_t kAnimFrameLocked = L'p';

	static const wchar_t kAnimIgnoredPixelData = L'p';

	static const wchar_t kAnimBrushColours = L'c';

	static const wchar_t kAnimBinary = L'@';

	static const wchar_t kAnimBlockEnd = L'}';

	static const std::wstring kAnimPadModeF = L"1:";
	static const std::wstring kAnimHexFormatF = L"2:";
	static const std::wstring kAnimHexOutputF = L"3:";
	static const std::wstring kAnimBracketsF = L"4:";

	static const std::wstring kAnimPreviewEnabledF = L"5:";
	static const std::wstring kAnimPreviewSizeF = L"6:";
	static const std::wstring kAnimPreviewViewF = L"7:";
	static const std::wstring kAnimPreviewVoidF = L"8:";
	static const std::wstring kAnimPreviewOffsetF = L"9:";
	static const std::wstring kAnimPreviewDirectionF = L"0:";
	static const std::wstring kAnimPreviewIncRadiallyF = L"!:";

	static const std::wstring kAnimDataSourceF = L"a:";
	static const std::wstring kAnimSourceLSBF = L"b:";
	static const std::wstring kAnimSourceDirectionF = L"c:";

	static const std::wstring kAnimSourceF = L"d:";
	static const std::wstring kAnimOrientationF = L"e:";
	static const std::wstring kAnimScanDirectionF = L"f:";
	static const std::wstring kAnimLSBF = L"g:";
	static const std::wstring kAnimLanguageF = L"h:";
	static const std::wstring kAnimNumberFormatF = L"i:";
	static const std::wstring kAnimNumberSizeF = L"j:";
	static const std::wstring kAnimLineContentF = L"k:";
	static const std::wstring kAnimLineCountF = L"l:";

	static const std::wstring kAnimRGBModeF = L"m:";
	static const std::wstring kAnimRGBChangePixelsF = L"n:";
	static const std::wstring kAnimRGBChangeColourF = L"o:";
	static const std::wstring kAnimRGBBrightnessF = L"q:";

	static const std::wstring kAnimOptimiseF = L"p:";

	static const std::wstring kAnimAutomationFileNameF = L"w:";
	static const std::wstring kAnimCommentF = L"x:";
	static const std::wstring kAnimASCIIIndexF = L"y:";
	static const std::wstring kAnimRGBBackgroundF = L"z:";
	static const std::wstring kAnimFrameRangeF = L"?:";
	static const std::wstring kAnimLayerCountF = L"%:";

	static const std::wstring kAnimColoursCustomF = L"c:";
	static const std::wstring kAnimColoursPaletteHistoryF = L"p:";

	static const std::wstring kAnimColoursLeftF = L"l:";
	static const std::wstring kAnimColoursMiddleF = L"m:";
	static const std::wstring kAnimColoursRightF = L"r:";

	static const std::wstring kAnimLayerNameF = L"n:";
	static const std::wstring kAnimLayerWidthF = L"w:";
	static const std::wstring kAnimLayerHeightF = L"h:";
	static const std::wstring kAnimLayerLockedF = L"l:";

	static const std::wstring kAnimWidthF = L"w:";
	static const std::wstring kAnimHeightF = L"h:";
	static const std::wstring kAnimRowDataF = L"r:";
	static const std::wstring kAnimFrameLockedF = L"p:";

	static const std::wstring kAnimIgnoredPixelDataF = L"p:";

	static const std::wstring kAnimBrushColoursF = L"c:";

	static const std::wstring kAnimBlockEndF = L"}:";

	static const std::wstring kAnimBinaryF = L"@:";

	// =================================================================
	// == Automation File (.automation) ================================
	// =================================================================

	static const std::wstring kFileHeaderData = L"data";
	static const std::wstring kFileHeaderActions = L"actions";
	static const std::wstring kFileHeaderPostProcessing = L"postprocessing";
	static const std::wstring kFileHeaderBrush1 = L"brush1";
	static const std::wstring kFileHeaderBrush2 = L"brush2";
	static const std::wstring kFileHeaderSource = L"source";
	static const std::wstring kFileHeaderTarget = L"target";

	static const std::wstring kAutomationProcessMode = L"v";

	static const std::wstring kAutomationStartFrame = L"w";
	static const std::wstring kAutomationEndFrame = L"x";

	static const std::wstring kAutomationErase = L"y";

	static const std::wstring kAutomationActionItem = L"r";
	static const std::wstring kAutomationPostProcessingItem = L"r";

	static const std::wstring kAutomationBrushColour = L"a";
	static const std::wstring kAutomationBrushTransparent = L"b";

	static const std::wstring kAutomationBrushRowData = L"r";

	static const std::wstring kAutomationColor = L"c";


	static const wchar_t kAutomationBrushColourF = L'a';
	static const wchar_t kAutomationBrushTransparentF = L'b';
	static const wchar_t kAutomationColorF = L'c';
	static const wchar_t kAutomationActionItemF = L'r';
	static const wchar_t kAutomationProcessModeF = L'v';
	static const wchar_t kAutomationStartFrameF = L'w';
	static const wchar_t kAutomationEndFrameF = L'x';
	static const wchar_t kAutomationEraseF = L'y';

	// =================================================================
	// == Brush ========================================================
	// =================================================================

	static const std::wstring kBrushPrefixMono = L"brush";
	static const std::wstring kBrushPrefixBiSequential = L"brush2";
	static const std::wstring kBrushPrefixBiBitPlanes = L"brush3";
	static const std::wstring kBrushPrefixRGB = L"brush4";
	static const std::wstring kBrushPrefixRGB3BPP = L"brush5";

	// =================================================================
	// == Colours File (.colours) ======================================
	// =================================================================

	static const std::wstring kColoursData = L"col";

	// =================================================================
	// == Export Profile (.ledsexport/.ledsexportrgb/.ledsexportrgb3bpp)
	// =================================================================

	static const std::wstring kExportSource = L"a";
	static const std::wstring kExportOrientation = L"b";
	static const std::wstring kExportLSB = L"c";
	static const std::wstring kExportLanguage = L"d";
	static const std::wstring kExportNumberFormat = L"e";
	static const std::wstring kExportNumberSize = L"f";
	static const std::wstring kExportScanDirection = L"g";
	static const std::wstring kExportLineContent = L"h";
	static const std::wstring kExportLineCount = L"i";
	static const std::wstring kExportRGBMode = L"r";
	static const std::wstring kExportRGBChangePixels = L"s";
	static const std::wstring kExportRGBChangeColour = L"t";
	static const std::wstring kExportRGBBrightness = L"u";

	static const std::wstring kExportMinWidth = L"v";
	static const std::wstring kExportMaxWidth = L"w";
	static const std::wstring kExportMinHeight = L"y";
	static const std::wstring kExportMaxHeight = L"z";

	static const std::wstring kExportInformation = L"!";

	static const std::wstring kExportBinarySource = L"1";
	static const std::wstring kExportBinaryOrientation = L"2";
	static const std::wstring kExportBinaryLSB = L"3";
	static const std::wstring kExportBinaryScanDirection = L"4";
	static const std::wstring kExportBinaryRGBMode = L"5";
	static const std::wstring kExportBinaryRGBChangePixels = L"6";
	static const std::wstring kExportBinaryRGBChangeColour = L"7";
	static const std::wstring kExportBinaryRGBBrightness = L"8";
	static const std::wstring kExportBinaryFileContents = L"9";

	// =================================================================
	// == RGB Font (.ledsfont) =========================================
	// =================================================================

	static const std::wstring kFontPrefixMono = L"font";
	static const std::wstring kFontPrefixBiSequential = L"font2";
	static const std::wstring kFontPrefixBiBitPlanes = L"font3";
	static const std::wstring kFontPrefixRGB = L"font4";
	static const std::wstring kFontPrefixRGB3BPP = L"font5";

	static const std::wstring kFontPrefixChar = L"char";

	static const wchar_t kRGBFontWidth = L'w';
	static const wchar_t kRGBFontHeight = L'h';
	static const wchar_t kRGBFontData = L'c';

	static const std::wstring kRGBFontWidthF = L"w:";
	static const std::wstring kRGBFontHeightF = L"h:";
	static const std::wstring kRGBFontDataF = L"c:";

	// =================================================================
	// == Gradient (.ledsgradient) =====================================
	// =================================================================

	static const std::wstring kGradientFileHeader = L"gradient";

	static const std::wstring kGradientColour = L"g";

	// =================================================================
	// == Matrix Preset (.ledspreset) ================================
	// =================================================================

	static const std::wstring kMatrixPresetHeader = L"preset";

	static const std::wstring kMatrixPresetWidth = L"w";
	static const std::wstring kMatrixPresetHeight = L"h";
	static const std::wstring kMatrixPresetPixelSize = L"e";
	static const std::wstring kMatrixPresetMatrixMode = L"m";
	static const std::wstring kMatrixPresetPixelShape = L"s";

	// =================================================================
	// == Preset (.ledspreset) =========================================
	// =================================================================

	static const std::wstring kPresetWidth = L"w";
	static const std::wstring kPresetHeight = L"h";
	static const std::wstring kPresetPixelSize = L"e";
	static const std::wstring kPresetMatrixMode = L"m";
	static const std::wstring kPresetPixelShape = L"s";
