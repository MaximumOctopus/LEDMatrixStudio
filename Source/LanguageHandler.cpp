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

#include "LanguageConstants.h"
#include "LanguageHandler.h"


LanguageHandler *GLanguageHandler;


LanguageHandler::LanguageHandler(const std::wstring LanguageId)
{
	LoadSuccess = LoadLanguage(LanguageId);
}


bool LanguageHandler::IsLoaded()
{
	return LoadSuccess;
}


bool LanguageHandler::LoadLanguage(const std::wstring LanguageId)
{
	std::wstring path = ExtractFilePath(Application->ExeName).c_str();

	std::wifstream file(path + L"language\\" + LanguageId + L".txt");

	if (file)
	{
		std::wstring s(L"");

		while (std::getline(file, s))
		{
			if (s != L"")
			{
				Text.push_back(s);
			}
		}

		if (Text.size() != kLanguageConstantCount)
		{
			LastError = L"Check your language files are up-to-date. They appear to have an incorrect number of entries!";

			for (int t = Text.size(); t < kLanguageConstantCount + 1; t++)
			{
				Text.push_back(L"missing #" + std::to_wstring(Text.size()));
			}
		}

		return true;
	}

	for (int t = 0; t < kLanguageConstantCount; t++)
	{
		Text.push_back(L"# " + std::to_wstring(t));
	}

    LastError = L"Erorr loading language file!";

	return false;
}
