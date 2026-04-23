// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2026
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

	enum class ImportColourMode { kMono = 0, kRGB = 1, kRGB3bpp = 2 };
	enum class ImportMode { kInvalid = 0, kSingleImage, kMultipleImages };

	enum class ViewShape { kSquare = 0, kRadial, kRadial3Q, kSemiCircle, kSemiCircleInverted };
	enum class MirrorMode { kOff = 0, kHorizontal, kVertical };
	enum class SoftwareMode { kAnimation = 0, kFont };

    enum class MatrixDrawMode { kGrid = 0, kFreeform };
	enum class MatrixColourMode { kNone = 0, kMono, kBiSequential, kBiBitplanes, kRGB, kRGB3BPP };

    enum class MergeFrameMode { kRetainGridValue = 0, kConvertForRender, kConvertForFileOutput };

	enum class PixelShape { kSquare = 0, kCircle, kRoundRect };
	enum class BrushSize  { kSmall = 0, kMedium, kLarge, kBigLarge, kSuperLarge };

	enum class CustomShape { kNone = 0, kCircle, kBorders, kTriangle };

	enum class GradientOption { kOff = 0, kVertical, kHorizontal };

	enum class LoadMode { kNew = 0, kAppend, kMergeBottomPriority, kMergeTopPriority, kMergeNewLayer, kMergeCurrentLayer };

	enum class LoadData { kUnknown = 0,
						  kLoadBlockStartHeader, kLoadBlockStartIgnoredPixel, kLoadBlockBegin, kLoadBlockEnd, kLoadBlockBeginLayout, kLoadBlockEndLayout, kLoadBlockStartColours,
						  kLoadHeaderSource, kLoadHeaderSourceLSB, kLoadHeaderSourceDirection, kLoadHeaderPadMode, kLoadHeaderHexFormat, kLoadHeaderHexOutput, kLoadHeaderBrackets,
						  kLoadHeaderDataSource, kLoadHeaderOrientation, kLoadHeaderScanDirection, kLoadHeaderLSB, kLoadHeaderLanguage, kLoadHeaderNumberFormat, kLoadHeaderNumberSize, kLoadHeaderLineContent, kLoadHeaderLineCount, kLoadHeaderRGBMode, kLoadHeaderRGBChangePixels, kLoadHeaderRGBChangeColour, kLoadHeaderOptimise,
						  kLoadHeaderMatrixComment, kLoadHeaderRGBBackground, kLoadHeaderASCIIIndex, kLoadHeaderAutomationFile,
						  kLoadHeaderRGBBrightness,
						  kLoadHeaderEnd,
						  kLoadHeaderPreviewEnabled, kLoadHeaderPreviewSize, kLoadHeaderPreviewView, kLoadHeaderPreviewVoid, kLoadHeaderPreviewOffset, kLoadHeaderPreviewOffsetDir, kLoadHeaderPreviewIncRadially,
						  kLoadHeaderLayerCount,
						  kLoadMatrixWidth, kLoadMatrixHeight, kLoadMatrixData, kLoadMatrixLocked,
						  kLoadIgnoredPixelData,
						  kLoadLayoutName, kLoadLayoutWidth, kLoadLayoutHeight, kLoadLayoutLocked,
						  kLoadColoursCustom, kLoadColoursDraw0, kLoadColoursDraw1, kLoadColoursDraw2, kLoadColoursPaletteHistory,
						  kLoadHeaderBinaryData,
						  kLoadPixelX, kLoadPixelY, kLoadPixelOrder, kLoadPixelColour, kLoadPixelGroup, kLoadBlockStartFrames, kLoadFrameLocked
						};

	enum class HexFormat { kEnabled = 0, kDisabled };
	enum class HexPrefix { kNone = 0, kDollar, kZeroX, kAmpersand };
	enum class BinaryPrefix { kNone = 0, kPercent, kZeroB };
	enum class PadFormat { kAuto = 0, k8Bits, k16Bits, k24Bits, k32Bits, k40Bits, k48Bits, k56Bits, k64Bits };
	enum class BracketStyle { kNone = 0, kNormal, kCurly, kSquare };

	static const int BrushSizePixels[] = { 1, 2, 3, 4, 5 };

	static const int __MaxWidth               = 1024;
	static const int __MaxHeight              = 1024;

	static const int kEffectFlipAll              = 0;
	static const int kEffectMirrorAll            = 1;
	static const int kEffectInvertAll            = 2;
	static const int kEffectGradientAll          = 3;

	static const int kEffectFlip                 = 0;
	static const int kEffectMirror               = 1;
	static const int kEffectInvert               = 2;

	static const int kEffectScrollLeft           = 0;
	static const int kEffectScrollRight          = 1;
	static const int kEffectScrollUp             = 2;
	static const int kEffectScrollDown           = 3;
	static const int kEffectWipeHorizontalOut    = 4;
	static const int kEffectWipeHorizontalIn     = 5;
	static const int kEffectWipeVerticalOut      = 6;
	static const int kEffectWipeVerticalIn       = 7;
	static const int kEffectWipeLeftToRight      = 8;
	static const int kEffectWipeRightToLeft      = 9;
	static const int kEffectWipeUpToDown         = 10;
	static const int kEffectWipeDownToUp         = 11;

	static const int kEffectRevealLeftRight      = 0;
	static const int kEffectRevealRightLeft      = 1;
	static const int kEffectRevealTopBottom      = 2;
	static const int kEffectRevealBottomTop      = 3;
	static const int kEffectRevealCentreOut      = 4;
	static const int kEffectRevealCentreIn       = 5;

	static const int kEffectSplitScrollLeftRight = 0;
	static const int kEffectSplitScrollRightLeft = 1;
	static const int kEffectSplitScrollUpDown    = 2;
	static const int kEffectSplitScrollDownUp    = 3;

	static const int kEffectAlternateScrollUpDown= 0;
	static const int kEffectAlternateScrollDownUp= 1;

	static const int kEffectScrollRowLeft        = 0;
	static const int kEffectScrollRowRight       = 1;

	static const int kEffectScrollColumnUp       = 0;
	static const int kEffectScrollColumnDown     = 1;

	static const int kEffectRotateCW             = 0;
	static const int kEffectRotateACW            = 1;

	static const std::wstring kDrawModes[] = { L"Draw", L"Filled box", L"Empty box", L"Line", L"Font", L"Empty Circle", L"Filled circle", L"Random brush",
											   L"Multi-draw", L"Colour picker", L"Copy brush", L"Paste brush", L"Gradient brush",
											   L"Flood fill",
											   L"Spiral", L"Ring", L"Split ring", L"Petals", L"Grid", L"Pyramid", L"Left triangle", L"Right triangle" };


	static const int kCustomShapeNone          = 0;
	static const int kCustomShapeCircle        = 1;
	static const int kCustomShapeJustBorders   = 2;
	static const int kCustomShapeTriangle      = 3;

	static const int kPreviewPixelSizeAuto     = -1;

	static const int kRoundRectCoeff          = 4;

	static const int kTopOffset               = 4;
	static const int kLeftOffset              = 4;

	static const int kLSBLeft             	  = 0;
	static const int kLSBRight            	  = 1;

	static const int kScanColTopToBottom 	  = 0;
	static const int kScanColBottomToTop  	  = 1;
	static const int kScanColAltDownUp    	  = 2;
	static const int kScanColAltUpDown    	  = 3;

	static const int kFontCharacterCount      = 96;

	static const unsigned __int64 kPowers[] = {
		1UL,2UL,4UL,8UL,16UL,32UL,64UL,128UL,
		256UL,512UL,1024UL,2048UL,4096UL,8192UL,16384UL,32768UL,
		65536UL, 131072UL, 262144UL, 524288UL, 1048576UL, 2097152UL, 4194304UL, 8388608UL,
		16777216UL, 33554432UL, 67108864UL, 134217728UL, 268435456UL, 536870912UL, 1073741824UL, 2147483648UL,
		4294967296UL, 8589934592UL, 17179869184UL, 34359738368UL, 68719476736UL, 137438953472UL, 274877906944UL, 549755813888UL,
		1099511627776UL, 2199023255552UL, 4398046511104UL, 8796093022208UL, 17592186044416UL, 35184372088832UL, 70368744177664UL, 140737488355328UL,
		281474976710656UL, 562949953421312UL, 1125899906842624UL, 2251799813685248UL, 4503599627370496UL, 9007199254740992UL, 18014398509481984UL, 36028797018963968UL,
		72057594037927936UL, 144115188075855872UL, 288230376151711744UL, 576460752303423488UL, 1152921504606846976UL, 2305843009213693952UL, 4611686018427387904UL, 9223372036854775808UL
	};

	static const unsigned __int64 kPowers16[] = {
		1UL, 16UL, 256UL, 4096UL, 65536UL, 1048576UL, 16777216UL, 268435456UL,
		4294967296UL, 68719476736UL, 1099511627776UL, 17592186044416UL,
		281474976710656UL, 4503599627370496UL, 72057594037927936UL, 1152921504606846976UL
	};

											//  black,   grey,    green,   purple,  red,     white,   dark grey,
	static const int kBackgroundColours[] = { 0x000000, 0xAAAAAA, 0x006600, 0x770077, 0x002288, 0xffffff, 0x333333 };

	static const int kPreviewSizes[] = { 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20, 25, 30, 40, 50 };

	static const int kPreviewVoids[] = { 10, 15, 20, 25, 30, 40, 50 };



	// 14..21
	static const int DefaultPatternParameter[]    = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  4,  0,  3,  8,  4,  1,  1,  1 };
	static const int DefaultPatternParameterMin[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0,  1,  1,  2,  1,  1,  1 };
	static const int DefaultPatternParameterMax[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64,  0, 64, 64, 64, 64, 64, 64 };

	static const int kDisplayClear  = 0;

	static const int kMouseLeft     = 1;
	static const int kMouseMiddle   = 2;
	static const int kMouseRight    = 3;

	static const int kDisplayMarker = 4;
	static const int kLightBoxShade = 5;

	static const int kPermanentLayer = 0;

	static const int kMOMCurrentOnly        = 0;
	static const int kMOMCurrentFrameLayers = 1;
	static const int kMOMCurrentLayerFrames = 2;
	static const int kMOMAll                = 3;

	static const int kZeroDegrees = 0;
	static const int k45Degrees =  45;
	static const int k90Degrees =  90;
	static const int k135Degrees = 135;
	static const int k180Degrees = 180;
	static const int k225Degrees = 225;
	static const int k270Degrees = 270;
	static const int k315Degrees = 315;


namespace ConstantsHelper
{
	BrushSize BrushFromInt(int);

	int PixelsFromBrushSize(BrushSize);

	PixelShape PixelShapeFromInt(int ps);

	std::wstring MatrixDrawModeAsString(MatrixDrawMode);
	std::wstring MatrixModeAsString(MatrixColourMode);
	int MatrixModeAsInt(MatrixColourMode);
	MatrixColourMode MatrixModeFromInt(int);
	std::wstring MatrixModeAsStringFromInt(int);
}
