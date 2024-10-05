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

#include <fstream>

#include "Formatting.h"
#include "FileUtility.h"


// ===========================================================================
// LED Matrix Studio files
// ===========================================================================

LoadData FileUtility::LoadDataParameterType(const std::wstring s, bool headermode, bool matrixmode, bool ignoredpixelmode, bool layermode, bool coloursmode)
{
	if (s.find(L"{header") != std::wstring::npos)
		return LoadData::kLoadBlockStartHeader;
	else if (s.find(L"{deadpixel") != std::wstring::npos)       // DO NOT EDIT THIS!!!
		return LoadData::kLoadBlockStartIgnoredPixel;
	else if (s.find(L"{colours") != std::wstring::npos)
		return LoadData::kLoadBlockStartColours;
	else if (s[0] == kDataBlockStart)
		return LoadData::kLoadBlockBegin;
	else if (s[0] == kDataBlockEnd)
		return LoadData::kLoadBlockEnd;
	else if (s[0] == L'[')
		return LoadData::kLoadBlockBeginLayout;
	else if (s[0] == L']')
		return LoadData::kLoadBlockEndLayout;
	else if (headermode)
	{
		switch (s[0])
		{
		case kAnimDataSource:
			return LoadData::kLoadHeaderSource;
		case kAnimSourceLSB:
			return LoadData::kLoadHeaderSourceLSB;
		case kAnimSourceDirection:
			return LoadData::kLoadHeaderSourceDirection;
		case kAnimPadMode:
			return LoadData::kLoadHeaderPadMode;
		case kAnimHexFormat:
			return LoadData::kLoadHeaderHexFormat;
		case kAnimHexOutput:
			return LoadData::kLoadHeaderHexOutput;
		case kAnimBrackets:
			return LoadData::kLoadHeaderBrackets;
		case kAnimSource:
			return LoadData::kLoadHeaderDataSource;
		case kAnimOrientation:
			return LoadData::kLoadHeaderOrientation;
		case kAnimScanDirection:
			return LoadData::kLoadHeaderScanDirection;
		case kAnimLSB:
			return LoadData::kLoadHeaderLSB;
		case kAnimLanguage:
			return LoadData::kLoadHeaderLanguage;
		case kAnimNumberFormat:
			return LoadData::kLoadHeaderNumberFormat;

		case kAnimNumberSize:
			return LoadData::kLoadHeaderNumberSize;
		case kAnimLineContent:
			return LoadData::kLoadHeaderLineContent;
		case kAnimLineCount:
			return LoadData::kLoadHeaderLineCount;
		case kAnimRGBMode:
			return LoadData::kLoadHeaderRGBMode;
		case kAnimRGBChangePixels:
			return LoadData::kLoadHeaderRGBChangePixels;
		case kAnimRGBChangeColour:
			return LoadData::kLoadHeaderRGBChangeColour;
		case kAnimOptimise:
			return LoadData::kLoadHeaderOptimise;
		case kAnimRGBBrightness:
			return LoadData::kLoadHeaderRGBBrightness;

		case kAnimAutomationFileName:
			return LoadData::kLoadHeaderAutomationFile;
		case kAnimComment:
			return LoadData::kLoadHeaderMatrixComment;
		case kAnimASCIIIndex:
			return LoadData::kLoadHeaderASCIIIndex;
		case kAnimRGBBackground:
			return LoadData::kLoadHeaderRGBBackground;

		case kAnimPreviewEnabled:
			return LoadData::kLoadHeaderPreviewEnabled;
		case kAnimPreviewSize:
			return LoadData::kLoadHeaderPreviewSize;
		case kAnimPreviewView:
			return LoadData::kLoadHeaderPreviewView;
		case kAnimPreviewVoid:
			return LoadData::kLoadHeaderPreviewVoid;
		case kAnimPreviewOffset:
			return LoadData::kLoadHeaderPreviewOffset;
		case kAnimPreviewDirection:
			return LoadData::kLoadHeaderPreviewOffsetDir;
		case kAnimPreviewIncRadially:
			return LoadData::kLoadHeaderPreviewIncRadially;
		case kAnimLayerCount:
			return LoadData::kLoadHeaderLayerCount;
		case kAnimBinary:
			return LoadData::kLoadHeaderBinaryData;

		case kAnimBlockEnd:
			return LoadData::kLoadHeaderEnd;
		}
	}
	else if (ignoredpixelmode)
	{
		switch (s[0])
		{
		case kAnimIgnoredPixelData:
			return LoadData::kLoadIgnoredPixelData;
		}
	}
	else if (matrixmode)
	{
		switch (s[0])
		{
		case kAnimWidth:
			return LoadData::kLoadMatrixWidth;
		case kAnimHeight:
			return LoadData::kLoadMatrixHeight;
		case kAnimRowData:
			return LoadData::kLoadMatrixData;
		case kAnimFrameLocked:
			return LoadData::kLoadMatrixLocked;
		}
	}
	else if (layermode)
	{
		switch (s[0])
		{
		case kAnimLayerName:
			return LoadData::kLoadLayoutName;
		case kAnimLayerWidth:
			return LoadData::kLoadLayoutWidth;
		case kAnimLayerHeight:
			return LoadData::kLoadLayoutHeight;
		case kAnimLayerLocked:
			return LoadData::kLoadLayoutLocked;
		}
	}
	else if (coloursmode)
	{
		switch (s[0])
		{
		case kAnimColoursCustom:
			return LoadData::kLoadColoursCustom;
		case kAnimColoursLeft:
			return LoadData::kLoadColoursDraw0;
		case kAnimColoursMiddle:
			return LoadData::kLoadColoursDraw1;
		case kAnimColoursRight:
			return LoadData::kLoadColoursDraw2;
		case kAnimColoursPaletteHistory:
			return LoadData::kLoadColoursPaletteHistory;
		}
	}

	return LoadData::kUnknown;
}


// ===========================================================================


MatrixMode FileUtility::GetMatrixModeFromFileChunk(const wchar_t c)
{
	switch (c)
	{
	case L'2':
		return MatrixMode::kBiSequential;
	case L'3':
		return MatrixMode::kBiBitplanes;
	case L'4':
		return MatrixMode::kRGB;
	case L'5':
		return MatrixMode::kRGB3BPP;
	}

	return MatrixMode::kMono;
}


bool FileUtility::SaveVector(const std::wstring file_name, const std::vector<std::wstring> &v)
{
	std::ofstream file(file_name);

	if (file)
	{
		for (int t = 0; t < v.size(); t++)
		{
			file << Formatting::to_utf8(v[t] + L"\n");
		}

		file.close();

		return true;
	}

    return false;
}
