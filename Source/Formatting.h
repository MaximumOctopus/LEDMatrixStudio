//
// PrettyChaos 1.0
//
// (c) Paul Alan Freshney 2023
//
// paul@freshney.org
//
// https://github.com/MaximumOctopus/PrettyChaos
//

#pragma once


#include <string>
#include <vector>


namespace Formatting
{
	[[nodiscard]] std::wstring AddLeadingSpace(std::wstring, int);
	[[nodiscard]] std::wstring AddTrailingSpace(std::wstring, int);

	std::wstring PadString(wchar_t, int);
	std::wstring PadToLength(const std::wstring, int);
	std::wstring PadZeroes(const std::wstring, int);

	std::wstring VectorToString(const std::vector<std::wstring> &);

	std::string to_utf8(const std::wstring& str);
	std::string to_utf8(const wchar_t* buffer, int len);
}