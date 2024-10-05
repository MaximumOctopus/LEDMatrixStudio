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

#include <string>

#include "MatrixConstants.h"


namespace ColourUtility
{
	static const std::wstring BiColoursLSBLeft[] = { L"00", L"01", L"10", L"11" };
	static const std::wstring BiColoursLSBRight[] = { L"00", L"10", L"01", L"11" };

	std::wstring RGBPlusInteger(int, int);
	std::wstring RGBColourNumberFormat(NumberFormat, int, unsigned int);
	int RGBConvertTo16(int, RGBMode, LeastSignificantBit, ColourSpace, int);
	int RGBConvertTo32(int, RGBMode, LeastSignificantBit, int);

	std::wstring RGBConvertToSplit(int, BinaryOptions&, const std::wstring,  const std::wstring);
	std::wstring RGBConvertToSplit(int, CodeOptions&, const std::wstring,  const std::wstring);
	std::wstring RGBConvertToSplit(int, RGBMode, int, NumberFormat, const std::wstring, const std::wstring, ColourSpace);

	std::wstring RGB3BPPFormatOutput(unsigned int, CodeOptions&, const std::wstring, const std::wstring, int);

	int HexToInt(const std::wstring);
	int DarkenRGB(int);

	int RandomColour(int, int);
}
