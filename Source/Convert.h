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


namespace Convert
{
    unsigned __int64 BinToInt(const std::wstring s);

	__int64 HexToInt(const std::wstring);
	char HexToByte(const std::wstring);

	std::wstring IntegerToBinary(int, __int64);
}
