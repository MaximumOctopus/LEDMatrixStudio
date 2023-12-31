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

#include <vector>


class FontHandler
{

public:

	std::vector<std::wstring> Fonts;

	FontHandler(const std::wstring);

	void PopulateFontList(const std::wstring);
};
