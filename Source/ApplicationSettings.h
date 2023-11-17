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

#pragma once

#include <string>

#include "ExportOptions.h"
#include "MatrixConstants.h"

enum class AutoSaveInterval { kTwoMinutes = 0, kFiveMinutes, kTenMinutes };


struct AppSettings
{
    std::wstring LMSFilePath = L"";

	std::wstring DataFilename = L"";
	int ASCIIIndex = 32;
	PadFormat PadMode;
	int PadModeHexCol = 0;
	int PadModeHexRow = 0;
	int PadModeDecCol = 0;
	int PadModeDecRow = 0;
	std::wstring HexPrefix = L"";
	std::wstring BinaryPrefix = L"";
	std::wstring OpenBracket = L"(";
	std::wstring CloseBracket = L")";
	ExportOptions LastExport;

	std::wstring LastSaveLocation = L"";
	std::wstring LastLoadLocation = L"";
	std::wstring LastAutomationFileName = L"";

	bool AutoSaveEnabled;
	AutoSaveInterval AutoSave;

	int CustomSpeed = 100;

	int BackgroundColour = 0x000000;

	int ExportUpdateMaxPixels = 100000;
	int ExportPreviewSize = 0;

    bool IgnoreWarnings = false;

	std::wstring Language = L"";

	void SetPadModeHexColRow(int value)
	{
		PadModeHexRow = value;
		PadModeHexCol = value;
	}

	void SetPadModeHexRowFromWidth(int width)
	{
		if (width >= 1 && width <= 8)
		{
			PadModeHexRow = 2;
		}
		else if (width >= 9 && width <= 16)
		{
			PadModeHexRow = 4;
		}
		else if (width >= 17 && width <= 24)
		{
			PadModeHexRow = 6;
		}
		else if (width >= 25 && width <= 32)
		{
			PadModeHexRow = 8;
		}
		else if (width >= 33 && width <= 40)
		{
			PadModeHexRow = 10;
		}
		else if (width >= 41 && width <= 48)
		{
			PadModeHexRow = 12;
		}
		else if (width >= 49 && width <= 56)
		{
			PadModeHexRow = 14;
		}
		else if (width >= 57 && width <= 64)
		{
			PadModeHexRow = 16;
		}
		else
		{
			PadModeHexRow = 0;
		}
	}

	void SetPadModeHexColFromHeight(int height)
	{
		if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 2;
		}
		else if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 4;
		}
		else if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 6;
		}
		else if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 8;
		}
		else if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 10;
		}
		else if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 12;
		}
		else if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 14;
		}
		else if (height >= 1 && height <= 8)
		{
			PadModeHexCol = 16;
		}
		else
		{
			PadModeHexCol = 0;
		}
	}

	int AutoSaveIntervalToInt()
	{
		switch (AutoSave)
		{
		case AutoSaveInterval::kTwoMinutes:
			return 0;
		case AutoSaveInterval::kFiveMinutes:
			return 1;
		case AutoSaveInterval::kTenMinutes:
			return 2;
		}

		return 0;
	}
};
