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

#include "Formatting.h"
#include "PresetHandler.h"

PresetHandler *GPresetHandler;


PresetHandler::PresetHandler(const std::wstring path)
{
	PopulateList(path);
}


void PresetHandler::PopulateList(const std::wstring path)
{
	Presets.clear();

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

				Presets.push_back(Utility::RemoveExtension(s));
			}

		} while (FindNextFileW(search_handle, &file));

		FindClose(search_handle);
	}
}


MatrixPreset PresetHandler::Load(const std::wstring file_name)
{
	std::wifstream file(file_name);

	MatrixPreset mp;

	if (file)
	{
		std::wstring s(L"");

		while (std::getline(file, s))
		{
			if (s != L"")
			{
				if (s[0] == L'/' || s[0] == L'#')
				{
					// comment, do nothing
				}
				else
				{
					if (s == L"}") continue;

					std::wstring v = s.substr(2);

					switch (GetMatrixPresetParameterType(s))
					{
					case MatrixPresetParameter::kProjectWidth:
						mp.Width = stoi(v);
						break;
					case MatrixPresetParameter::kProjectHeight:
						mp.Height = stoi(v);
						break;
					case MatrixPresetParameter::kUnused:
						break;
					case MatrixPresetParameter::kPixelSize:
						mp.PixelSize = stoi(v);
						break;
					case MatrixPresetParameter::kMatrixType:
						mp.SetMatrixModeFromInt(stoi(v));
						break;
					}
				}
			}
		}

		mp.Configured = true;

		file.close();
	}

	return mp;
}


bool PresetHandler::Save(const std::wstring file_name, MatrixPreset& preset)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L'{' + kMatrixPresetHeader + L"\n");

		file << Formatting::to_utf8(kMatrixPresetWidth +      L":" + std::to_wstring(preset.Width) + L"\n");
		file << Formatting::to_utf8(kMatrixPresetHeight +     L":" + std::to_wstring(preset.Height) + L"\n");
		file << Formatting::to_utf8(kMatrixPresetPixelSize +  L":" + std::to_wstring(preset.PixelSize) + L"\n");
		file << Formatting::to_utf8(kMatrixPresetMatrixMode + L":" + std::to_wstring(ConstantsHelper::MatrixModeAsInt(preset.Mode)) + L"\n");
		file << Formatting::to_utf8(kMatrixPresetPixelShape + L":" + std::to_wstring(preset.PixelShape) + L"\n");
		file << Formatting::to_utf8(kDataBlockEndS + L"\n");

		file.close();

		return true;
	}

	return false;
}


PresetHandler::MatrixPresetParameter PresetHandler::GetMatrixPresetParameterType(const std::wstring s)
{
	if (s[0] == kDataBlockStart)
		return MatrixPresetParameter::kStructBegin;
	else if (s[0] == kDataBlockEnd)
		return MatrixPresetParameter::kStructEnd;
	else if (s[0] == kMatrixPresetWidth[0])
		return MatrixPresetParameter::kProjectWidth;
	else if (s[0] == kMatrixPresetHeight[0])
		return MatrixPresetParameter::kProjectHeight;
	else if (s[0] == kAnimDataSource)
		return MatrixPresetParameter::kSource;
	else if (s[0] == kAnimSourceLSB)
		return MatrixPresetParameter::kSourceLSB;
	else if (s[0] == kAnimSourceDirection)
		return MatrixPresetParameter::kSourceDirection;
	else if (s[0] == L'd')
		return MatrixPresetParameter::kUnused;
	else if (s[0] == kMatrixPresetPixelSize[0])
		return MatrixPresetParameter::kPixelSize;
	else if (s[0] == kMatrixPresetMatrixMode[0])
		return MatrixPresetParameter::kMatrixType;

	return MatrixPresetParameter::kUnknown;
}
