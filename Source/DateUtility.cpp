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

#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "DateUtility.h"

extern LanguageHandler *GLanguageHandler;


namespace DateUtility
{
	std::wstring CreatedDate()
	{
		return GLanguageHandler->Text[kDate] + L" : " + GetDate();
	}


	std::wstring GetTime()
	{
		struct tm newtime;
		time_t now = time(0);
		localtime_s(&newtime, &now);

		std::wstring hour = std::to_wstring(newtime.tm_hour);
		std::wstring min = std::to_wstring(newtime.tm_min);
		std::wstring sec = std::to_wstring(newtime.tm_sec);

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

		return hour + L":" + min + L"." + sec;
	}


	std::wstring GetDate()
	{
		struct tm newtime;
		time_t now = time(0);
		localtime_s(&newtime, &now);

		std::wstring year = std::to_wstring(newtime.tm_year + 1900);
		std::wstring month = std::to_wstring(newtime.tm_mon + 1);
		std::wstring day = std::to_wstring(newtime.tm_mday);

		if (newtime.tm_mon + 1 < 10)
		{
			month = L"0" + month;
		}

		if (newtime.tm_mday < 10)
		{
			day = L"0" + day;
		}

		return year + L"/" + month + L"/" + day;
	}
}