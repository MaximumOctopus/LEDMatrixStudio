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


namespace Utility
{
	bool ValidateNumber(const std::wstring, int);

	std::wstring GetAutoSaveName();

	bool IsAlphaNumeric(const wchar_t);

	std::wstring RemoveExtension(const std::wstring);
	std::wstring GetFilePrefix(const std::wstring);
	std::wstring GetFileNameNoExt(const std::wstring);

	void ExecuteFile(const std::wstring);

	bool IsAlphaNumeric(const std::wstring, int);

	std::wstring ReplaceString(const std::wstring, const std::wstring&, const std::wstring&);

	System::UnicodeString WS2US(const std::wstring);
}
