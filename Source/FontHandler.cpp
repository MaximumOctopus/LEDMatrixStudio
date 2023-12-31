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
#include <Windows.h>

#include "FontHandler.h"
#include "Utility.h"

FontHandler *GFontHandler;


FontHandler::FontHandler(const std::wstring path)
{
	PopulateFontList(path);
}


void FontHandler::PopulateFontList(const std::wstring path)
{
	Fonts.clear();

	std::wstring tmp = path + L"*";

	WIN32_FIND_DATAW file;

	HANDLE search_handle = FindFirstFileW(tmp.c_str(), &file);

	if (search_handle != INVALID_HANDLE_VALUE)
	{
		do
		{
			if (file.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
			{
			}
			else
			{
				std::wstring s = file.cFileName;

				Fonts.push_back(Utility::RemoveExtension(s));
			}

		} while (FindNextFileW(search_handle, &file));

		FindClose(search_handle);
	}
}
