// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2023
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

#include <vector>

#include "ExportOptions.h"


class ProfileHandler
{
	enum class LoadProfile { kUnknown = 0, kDataBegin, kDataEnd,
							 kSource, kOrientation, kLSB, kLanguage, kNumberFormat, kNumberSize, kScanDirection,
							 kLineContent, kLineCount,
							 kRGBMode, kRGBChangePixels, kRGBChangeColour, kRGBBrightness,
							 kMinWidth, kMaxWidth, kMinHeight, kMaxHeight,
							 kInformation,
							 kBinarySource, kBinaryOrientation, kBinaryLSB, kBinaryScanDirection,
							 kBinaryRGBMode, kBinaryRGBChangePixels, kBinaryRGBChangeColour, kBinaryRGBBrightness,
							 kBinaryFileContents };

	bool PopulateList(std::vector<std::wstring>&, const std::wstring);

    LoadProfile GetParameterType(const std::wstring);

public:

	std::vector<std::wstring> Profiles;
	std::vector<std::wstring> ProfilesRGB;

	ProfileHandler(const std::wstring);

	ExportOptions Load(const std::wstring);
	bool Save(const std::wstring, bool, ExportOptions&);

	bool DeleteExportProfile(const std::wstring);
};
