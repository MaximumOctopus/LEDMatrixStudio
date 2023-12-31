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
#include <vector>


class LanguageHandler
{
	bool LoadSuccess = false;

	bool LoadLanguage(const std::wstring);

public:

    std::wstring LastError = L"";

	std::vector<std::wstring> Text;

	LanguageHandler(const std::wstring);

	bool IsLoaded();
};
