// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2023
//   www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
//   https://github.com/MaximumOctopus/LEDMatrixStudio
//
//   https://maximumoctopus.hashnode.dev/
//
//   C++ Rewrite October 11th 2023
//
// ===================================================================

#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;


namespace Utility
{
	bool IsAlphaNumeric(const wchar_t s)
	{
		if (isalpha(s) || isdigit(s))
		{
			return true;
		}

		return false;
	}


	bool ValidateNumber(const std::wstring s, int max)
	{
		try
		{
			int n = stoi(s);

			if (max != -1 && n <= max)
			{
				return true;
			}
		}
		catch(...)
		{
			return false;
		}

		return false;
	}


	std::wstring RemoveExtension(const std::wstring s)
	{
		auto i = s.rfind(L'.');

		if (i != std::wstring::npos)
		{
			return s.substr(0, i);
		}

		return s;
	}


	// file name with path, no extension
	std::wstring GetFileNameNoExt(const std::wstring file_name)
	{
		auto i = file_name.rfind(L'.');

		if (i != std::wstring::npos)
		{
			return file_name.substr(i - 1);
		}

		return file_name;
	}


	// file name without path, no extension
	std::wstring GetFilePrefix(const std::wstring file_name)
	{
		auto i = file_name.rfind(L'\\');

		if (i != std::wstring::npos)
		{
			return GetFileNameNoExt(file_name.substr(i + 1));
		}

		return GetFileNameNoExt(file_name);
	}


	std::wstring GetAutoSaveName()
	{
		struct tm newtime;
		time_t now = time(0);
		localtime_s(&newtime, &now);

		std::wstring year = std::to_wstring(newtime.tm_year + 1900);
		std::wstring month = std::to_wstring(newtime.tm_mon + 1);
		std::wstring day = std::to_wstring(newtime.tm_mday);
		std::wstring hour = std::to_wstring(newtime.tm_hour);
		std::wstring min = std::to_wstring(newtime.tm_min);
		std::wstring sec = std::to_wstring(newtime.tm_sec);

		if (newtime.tm_mon + 1 < 10)
		{
			month = L"0" + month;
		}

		if (newtime.tm_mday < 10)
		{
			day = L"0" + day;
		}

		if (newtime.tm_hour < 10)
		{
			hour = L"0" + hour;
		}

		if (newtime.tm_min < 10)
		{
			min = L"0" + min;
		}

		if (newtime.tm_sec < 10)
		{
			sec = L"0" + sec;
		}

		return L"autosave_" + year + month + day + L"_" + hour + min + sec + L".leds";
	}


   	std::wstring ReplaceString(const std::wstring subject, const std::wstring& search, const std::wstring& replace)
	{
		std::wstring output = subject;

		size_t pos = 0;

		while ((pos = output.find(search, pos)) != std::wstring::npos)
		{
			output.replace(pos, search.length(), replace);

			pos += replace.length();
		}

		return output;
	}


	void ExecuteFile(const std::wstring path)
	{
		ShellExecute(0, L"open", path.c_str(), 0, 0 , SW_SHOW );
	}


	std::wstring MatrixModeAsString(MatrixMode mode)
	{
		switch (mode)
		{
		case MatrixMode::kMono: 	    return GLanguageHandler->Text[kSingleColour];
		case MatrixMode::kBiSequential: return GLanguageHandler->Text[kBiColourSequential];
		case MatrixMode::kBiBitplanes:  return GLanguageHandler->Text[kBiColourBitplanes];
		case MatrixMode::kRGB:          return GLanguageHandler->Text[kRGB];
		case MatrixMode::kRGB3BPP:      return GLanguageHandler->Text[kRGB3BPP];

		default:
			return L"unknown :(";
		}
	}


	int MatrixModeAsInt(MatrixMode mode)
	{
		switch (mode)
		{
		case MatrixMode::kMono: 	    return 0;
		case MatrixMode::kBiSequential: return 1;
		case MatrixMode::kBiBitplanes:  return 2;
		case MatrixMode::kRGB:          return 3;
		case MatrixMode::kRGB3BPP:      return 4;
		default:
			return 0;
		}
	}


	MatrixMode MatrixModeFromInt(int mm)
	{
		switch (mm)
		{
		case 0:
			return MatrixMode::kMono;
		case 1:
			return MatrixMode::kBiSequential;
		case 2:
			return MatrixMode::kBiBitplanes;
		case 3:
			return MatrixMode::kRGB;
		case 4:
			return MatrixMode::kRGB3BPP;

		default:
			return MatrixMode::kMono;
		}
	}

	std::wstring MatrixModeAsStringFromInt(int mm)
	{
		switch (mm)
		{
		case 0:
			return GLanguageHandler->Text[kSingleColour];
		case 1:
			return GLanguageHandler->Text[kBiColourSequential];
		case 2:
			return GLanguageHandler->Text[kBiColourBitplanes];
		case 3:
			return GLanguageHandler->Text[kRGB];
		case 4:
			return GLanguageHandler->Text[kRGB3BPP];

		default:
			return L"unknown :(";
		}
	}

	PixelShape PixelShapeFromInt(int ps)
	{
		switch (ps)
		{
		case 0:
			return PixelShape::kSquare;
		case 1:
			return PixelShape::kCircle;
		case 2:
			return PixelShape::kRoundRect;
		}

        return PixelShape::kSquare;
	}

	System::UnicodeString WS2US(const std::wstring s)
	{
        return s.c_str();
	}
}

