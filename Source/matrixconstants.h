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

#include "ExportOptions.h"


	enum class ViewShape { kSquare = 0, kRadial, kRadial3Q, kSemiCircle, kSemiCircleInverted };
	enum class MirrorMode { kOff = 0, kHorizontal, kVertical };
	enum class SoftwareMode { kAnimation = 0, kFont };

	enum class MatrixMode { kNone = 0, kMono, kBiSequential, kBiBitplanes, kRGB, kRGB3BPP };

	enum class PixelShape { kSquare = 0, kCircle, kRoundRect };
	enum class BrushSize  { kSmall = 0, kMedium, kLarge, kBigLarge, kSuperLarge };

	enum class CustomShape { kNone = 0, kCircle, kBorders, kTriangle };

	enum class GradientOption { kOff = 0, kVertical, kHorizontal };

	enum class LoadMode { kNew = 0, kAppend, kMergeBottomPriority, kMergeTopPriority, kMergeNewLayer, kMergeCurrentLayer };

	enum class LoadData { kUnknown = 0,
					  kLoadBlockStartHeader, kLoadBlockStartDeadPixel, kLoadBlockBegin, kLoadBlockEnd, kLoadBlockBeginLayout, kLoadBlockEndLayout, kLoadBlockStartColours,
					  kLoadHeaderSource, kLoadHeaderSourceLSB, kLoadHeaderSourceDirection, kLoadHeaderPadMode, kLoadHeaderHexFormat, kLoadHeaderHexOutput, kLoadHeaderBrackets,
					  kLoadHeaderDataSource, kLoadHeaderOrientation, kLoadHeaderScanDirection, kLoadHeaderLSB, kLoadHeaderLanguage, kLoadHeaderNumberFormat, kLoadHeaderNumberSize, kLoadHeaderLineContent, kLoadHeaderLineCount, kLoadHeaderRGBMode, kLoadHeaderRGBChangePixels, kLoadHeaderRGBChangeColour, kLoadHeaderOptimise,
					  kLoadHeaderMatrixComment, kLoadHeaderRGBBackground, kLoadHeaderASCIIIndex, kLoadHeaderAutomationFile,
					  kLoadHeaderRGBBrightness,
					  kLoadHeaderEnd,
					  kLoadHeaderPreviewEnabled, kLoadHeaderPreviewSize, kLoadHeaderPreviewView, kLoadHeaderPreviewVoid, kLoadHeaderPreviewOffset, kLoadHeaderPreviewOffsetDir, kLoadHeaderPreviewIncRadially,
					  kLoadHeaderLayerCount,
					  kLoadMatrixWidth, kLoadMatrixHeight, kLoadMatrixData, kLoadMatrixLocked,
					  kLoadDeadPixelData,
					  kLoadLayoutName, kLoadLayoutWidth, kLoadLayoutHeight, kLoadLayoutLocked,
					  kLoadColoursCustom, kLoadColoursDraw0, kLoadColoursDraw1, kLoadColoursDraw2, kLoadColoursPaletteHistory,
					  kLoadHeaderBinaryData };

	enum class HexFormat { kEnabled = 0, kDisabled };
	enum class HexPrefix { kNone = 0, kDollar, kZeroX, kAmpersand };
	enum class BinaryPrefix { kNone = 0, kPercent, kZeroB };
	enum class PadFormat { kAuto = 0, k8Bits, k16Bits, k24Bits, k32Bits, k40Bits, k48Bits, k56Bits, k64Bits };
	enum class BracketStyle { kNone = 0, kNormal, kCurly, kSquare };

	static const int BrushSizePixels[] = { 1, 2, 3, 4, 5 };

	static const int __MaxWidth                = 256;
	static const int __MaxHeight               = 256;

	static const int modeFlipAll              = 0;
	static const int modeMirrorAll            = 1;
	static const int modeInvertAll            = 2;
	static const int modeGradientAll          = 3;

	static const int modeFlip                 = 0;
	static const int modeMirror               = 1;
	static const int modeInvert               = 2;

	static const int modeScrollLeft           = 0;
	static const int modeScrollRight          = 1;
	static const int modeScrollUp             = 2;
	static const int modeScrollDown           = 3;
	static const int modeWipeHorizontalOut    = 4;
	static const int modeWipeHorizontalIn     = 5;
	static const int modeWipeVerticalOut      = 6;
	static const int modeWipeVerticalIn       = 7;
	static const int modeWipeLeftToRight      = 8;
	static const int modeWipeRightToLeft      = 9;
	static const int modeWipeUpToDown         = 10;
	static const int modeWipeDownToUp         = 11;

	static const int modeRevealLeftRight      = 0;
	static const int modeRevealRightLeft      = 1;
	static const int modeRevealTopBottom      = 2;
	static const int modeRevealBottomTop      = 3;
	static const int modeRevealCentreOut      = 4;
	static const int modeRevealCentreIn       = 5;

	static const int modeSplitScrollLeftRight = 0;
	static const int modeSplitScrollRightLeft = 1;
	static const int modeSplitScrollUpDown    = 2;
	static const int modeSplitScrollDownUp    = 3;

	static const int modeAlternateScrollUpDown= 0;
	static const int modeAlternateScrollDownUp= 1;

	static const int modeScrollRowLeft        = 0;
	static const int modeScrollRowRight       = 1;

	static const int modeScrollColumnUp       = 0;
	static const int modeScrollColumnDown     = 1;

	static const int modeRotateCW             = 0;
	static const int modeRotateACW            = 1;

	static const std::wstring CDrawModes[] = { L"Draw", L"Filled box", L"Empty box", L"Line", L"Font", L"Empty Circle", L"Filled circle", L"Random brush",
											   L"Multi-draw", L"Colour picker", L"Copy brush", L"Paste brush", L"Gradient brush",
											   L"Flood fill",
											   L"Spiral", L"Ring", L"Split ring", L"Petals", L"Grid", L"Pyramid", L"Left triangle", L"Right triangle" };


	static const int customShapeNone          = 0;
	static const int customShapeCircle        = 1;
	static const int customShapeJustBorders   = 2;
	static const int customShapeTriangle      = 3;

	static const int previewPixelSizeAuto     = -1;

	static const int CRoundRectCoeff          = 4;

	static const int CTopOffset               = 4;
	static const int CLeftOffset              = 4;

	static const int lsbLeft             = 0;
	static const int lsbRight            = 1;

	static const int scanColTopToBottom  = 0;
	static const int scanColBottomToTop  = 1;
	static const int scanColAltDownUp    = 2;
	static const int scanColAltUpDown    = 3;

    static const int FontCharacterCount       = 96;

	static const unsigned __int64 powers[] = {
		1,2,4,8,16,32,64,128,
		256,512,1024,2048,4096,8192,16384,32768,
		65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608,
		16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648,
		4294967296, 8589934592, 17179869184, 34359738368, 68719476736, 137438953472, 274877906944, 549755813888,
		1099511627776, 2199023255552, 4398046511104, 8796093022208, 17592186044416, 35184372088832, 70368744177664, 140737488355328,
		281474976710656, 562949953421312, 1125899906842624, 2251799813685248, 4503599627370496, 9007199254740992, 18014398509481984, 36028797018963968,
		72057594037927936, 144115188075855872, 288230376151711744, 576460752303423488, 1152921504606846976, 2305843009213693952, 4611686018427387904, 9223372036854775808
	};

	static const unsigned __int64 powers16[] = {
		1, 16, 256, 4096, 65536, 1048576, 16777216, 268435456,
		4294967296, 68719476736, 1099511627776, 17592186044416,
		281474976710656, 4503599627370496, 72057594037927936, 1152921504606846976
	};

											//  black,   grey,    green,   purple,  red,     white,   dark grey,
	static const int backgroundColours[] = { 0x000000, 0xAAAAAA, 0x006600, 0x770077, 0x002288, 0xffffff, 0x333333 };

	static const int previewSizes[] = { 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 25, 30, 40, 50 };

	static const int previewVoids[] = { 10, 15, 20, 25, 30, 40, 50 };



	// 14..21
	static const int DefaultPatternParameter[]    = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  4,  0,  3,  8,  4,  1,  1,  1 };
	static const int DefaultPatternParameterMin[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  1,  1,  2,  1,  1,  1 };
	static const int DefaultPatternParameterMax[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64,  0, 64, 64, 64, 64, 64, 64 };

	static const int CDisplayClear  = 0;

	static const int CMouseLeft     = 1;
	static const int CMouseMiddle   = 2;
	static const int CMouseRight    = 3;

	static const int CDisplayMarker = 4;
	static const int CLightBoxShade = 5;

	static const int CPermanentLayer = 0;

	static const int CMOMCurrentOnly        = 0;
	static const int CMOMCurrentFrameLayers = 1;
	static const int CMOMCurrentLayerFrames = 2;
	static const int CMOMAll                = 3;

	static const int CZeroDegrees = 0;
	static const int C45Degrees =  45;
	static const int C90Degrees =  90;
	static const int C135Degrees = 135;
	static const int C180Degrees = 180;
	static const int C225Degrees = 225;
	static const int C270Degrees = 270;
	static const int C315Degrees = 315;


namespace ConstantsHelper
{
	BrushSize BrushFromInt(int);

	int PixelsFromBrushSize(BrushSize);

	PixelShape PixelShapeFromInt(int ps);

	std::wstring MatrixModeAsString(MatrixMode mode);
	int MatrixModeAsInt(MatrixMode mode);
	MatrixMode MatrixModeFromInt(int mm);
	std::wstring MatrixModeAsStringFromInt(int mm);
}
