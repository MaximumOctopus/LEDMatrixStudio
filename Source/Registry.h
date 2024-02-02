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
#include <Windows.h>


namespace Registry
{
	[[nodiscard]] std::wstring ReadString(HKEY, const std::wstring, std::wstring);

	[[nodiscard]] int ReadInteger(HKEY, const std::wstring, int);

	[[nodiscard]] bool ReadBool(HKEY, const std::wstring, bool);

	bool WriteString(HKEY, const std::wstring&, const std::wstring&);
	bool WriteInteger(HKEY, const std::wstring&, const int &);

	[[nodiscard]] bool Delete(HKEY, const std::wstring&);
}