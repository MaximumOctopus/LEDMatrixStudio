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

#include <Windows.h>

#include "Registry.h"
#include "SystemSettings.h"
#include "Utility.h"

SystemSettings *GSystemSettings;



SystemSettings::SystemSettings()
{
	App.LMSFilePath = ExtractFilePath(Application->ExeName).c_str();

	LoadSettings();

	BuildGradientList(App.LMSFilePath + L"gradients\\*.ledsgradient");

	BuildLanguageList(App.LMSFilePath + L"language\\*.txt");

	App.LastExport.clear(false);
}


bool SystemSettings::LoadSettings()
{
	LONG dwRet;

	dwRet = RegOpenKeyEx(HKEY_CURRENT_USER,
						 L"SOFTWARE\\freshney.org\\MatrixBuilder",
						 NULL,
						 KEY_QUERY_VALUE,
						 &hKey);

	if (dwRet != ERROR_SUCCESS)
	{
		DWORD dummy;

		RegCreateKeyEx(HKEY_CURRENT_USER,
					   L"SOFTWARE\\freshney.org\\MatrixBuilder",
					   0,
					   NULL,
					   0,
					   KEY_ALL_ACCESS,
					   NULL,
					   &hKey,
					   &dummy);

		return false;
	}

	// ===========================================================================

	Project.MatrixModeFromInt(Registry::ReadInteger(hKey, L"matrixtype", 0));
	Project.Width       = Registry::ReadInteger(hKey, L"gridwidth", 7);
	Project.Height      = Registry::ReadInteger(hKey, L"gridheight", 7);
	Project.PixelSize   = Registry::ReadInteger(hKey, L"pixelsize", CPixelSize20);
	Project.PixelShapeFromInt(Registry::ReadInteger(hKey, L"pixelshape", 0));

	// ===========================================================================

	LEDColoursSingle[0] = Registry::ReadInteger(hKey, L"offcolour", clBlack);
	LEDColoursSingle[1] = Registry::ReadInteger(hKey, L"oncolour", clWhite);
	LEDColoursSingle[2] = Registry::ReadInteger(hKey, L"oncolour2", clBlack);
	LEDColoursSingle[3] = Registry::ReadInteger(hKey, L"oncolour3", clBlack);
	LEDColoursSingle[4] = Registry::ReadInteger(hKey, L"selectcolour", clBlue);
	LEDColoursSingle[5] = Registry::ReadInteger(hKey, L"lightboxcolour", 0x00AAAAAA);

	LEDColoursBi[0] = Registry::ReadInteger(hKey, L"offcolourbi", clBlack);
	LEDColoursBi[1] = Registry::ReadInteger(hKey, L"oncolourbi", clRed);
	LEDColoursBi[2] = Registry::ReadInteger(hKey, L"oncolour2bi", clGreen);
	LEDColoursBi[3] = Registry::ReadInteger(hKey, L"oncolour3bi", clYellow);
	LEDColoursBi[4] = Registry::ReadInteger(hKey, L"selectcolourbi", clBlue);
	LEDColoursBi[5] = Registry::ReadInteger(hKey, L"lightboxcolourbi", 0x00AAAAAA);

	RGBBackground    = Registry::ReadInteger(hKey, L"rgbbackground", clBlack);
	LEDRGBColours[1] = Registry::ReadInteger(hKey, L"LEDRGBColoursLMB", clRed);
	LEDRGBColours[2] = Registry::ReadInteger(hKey, L"LEDRGBColoursMMB", clBlue);
	LEDRGBColours[3] = Registry::ReadInteger(hKey, L"LEDRGBColoursRMB", clYellow);

	// ===========================================================================

	SelectionColours[0] = Registry::ReadInteger(hKey, L"sSelectionLMB", 2);
	SelectionColours[1] = Registry::ReadInteger(hKey, L"sSelectionMMB", 1);
	SelectionColours[2] = Registry::ReadInteger(hKey, L"sSelectionRMB", 0);

	// ===========================================================================

	App.Language             = Registry::ReadString(hKey, L"language", L"English");

	if (App.Language[0] == L'&')
	{
		App.Language = App.Language.substr(1);
	}
	else
	{
		App.Language = App.Language;
	}

	// ===========================================================================

	App.BackgroundColour     = Registry::ReadInteger(hKey, L"displaybackground", clBtnFace);

	// ===========================================================================

	App.LastSaveLocation     = Registry::ReadString(hKey, L"savelocation", L"");
	App.LastLoadLocation     = Registry::ReadString(hKey, L"loadlocation", L"");

	if (App.LastSaveLocation.empty())
	{
		App.LastSaveLocation = App.LMSFilePath + L"saves\\";
	}

	if (App.LastLoadLocation.empty())
	{
		App.LastLoadLocation = App.LMSFilePath + L"saves\\";
    }

	App.CustomSpeed          = Registry::ReadInteger(hKey, L"customspeed", 1000);

	if (App.CustomSpeed <= 0)
	{
		App.CustomSpeed = 1000;
	}

	// ===========================================================================

	App.ExportUpdateMaxPixels = Registry::ReadInteger(hKey, L"exportupdatemaxpixels", 100000);
	App.ExportPreviewSize = Registry::ReadInteger(hKey, L"exportpreviewsize", 512);
	App.HexPrefix = Registry::ReadString(hKey, L"hexprefix2", L"0x");
	App.AnimSpeed = Registry::ReadInteger(hKey, L"animspeed", 1000);

	App.IgnoreWarnings = Registry::ReadBool(hKey, L"ignorewarnings", false);

	// ===========================================================================

	Bars.Animation  = Registry::ReadBool(hKey, L"showanimtoolbar", true);
	Bars.ColumnRow  = Registry::ReadBool(hKey, L"columnrowtoolbar", true);
	Bars.RGBPalette = Registry::ReadBool(hKey, L"rgbpalettetoolbar", true);
	Bars.Pattern    = Registry::ReadBool(hKey, L"patterntoolbar", true);

	// ===========================================================================

	UseFormatData            = Registry::ReadBool(hKey, L"useformatdata", true);

	// ===========================================================================

	App.AutoSaveEnabled = Registry::ReadBool(hKey, L"autosave", false);

	switch (Registry::ReadInteger(hKey, L"autosaveinterval", 0))
	{
	case 0:
		App.AutoSave = AutoSaveInterval::kTwoMinutes;
		break;
	case 1:
		App.AutoSave = AutoSaveInterval::kFiveMinutes;
		break;
	case 2:
		App.AutoSave = AutoSaveInterval::kTenMinutes;
		break;
	}

	// ===========================================================================

	Preview.Active   = Registry::ReadInteger(hKey, L"previewactive", 0);

	Preview.Size      = Registry::ReadInteger(hKey, L"previewsize", 1);
	Preview.Void      = Registry::ReadInteger(hKey, L"previewvoid", 15);
	Preview.Offset    = Registry::ReadInteger(hKey, L"previewoffset", 0);
	Preview.Direction = Registry::ReadBool(hKey, L"previewoffsetdirection", false);

	Preview.ViewShapeFromInt(Registry::ReadInteger(hKey, L"previewview", 0));

	// ===========================================================================

	for (int t = 0; t < 16; t++)
	{
		int colour = Registry::ReadInteger(hKey, L"rgbpalette" + std::to_wstring(t), -1);

		if (colour == -1)
		{
			colour = defaultRGBPalatte[t];
		}

		RGBPalette[t] = colour;
	}

	// ===========================================================================

	switch (Registry::ReadInteger(hKey, L"rowcolumndata", 0))
	{
	case 0:
		RowColumn = RowColumnData::kOff;
		break;
	case 1:
		RowColumn = RowColumnData::kData;
		break;
	case 2:
		RowColumn = RowColumnData::kIndex;
		break;
    }

	// ===========================================================================

	int t = 0;

	std::wstring s = Registry::ReadString(hKey, L"reopen_0", L"");

	while (!s.empty() && t < 20)
	{
		FileHistory.push_back(s);

		t++;

		s = Registry::ReadString(hKey, L"reopen_" + std::to_wstring(t), L"");
	}

	// ===========================================================================

	ExportGIF.FileName   = Registry::ReadString(hKey, L"exportgiffilename", L"");
	ExportGIF.PixelSize  = Registry::ReadInteger(hKey, L"exportgifpixelsize", 1);
	ExportGIF.PixelShape = Registry::ReadInteger(hKey, L"exportgifpixelshape", 0);
	ExportGIF.Background = Registry::ReadInteger(hKey, L"exportgifbackground", 0x000000);

	RegCloseKey(hKey);

	return true;
}


bool SystemSettings::SaveSettings()
{
	LONG dwRet;

	dwRet = RegOpenKeyEx(HKEY_CURRENT_USER,
						 L"SOFTWARE\\freshney.org\\MatrixBuilder",
						 NULL,
						 KEY_ALL_ACCESS,
						 &hKey);

	if (dwRet != ERROR_SUCCESS)
	{
		return false;
	}

	Registry::WriteInteger(hKey, L"matrixtype", Project.MatrixModeToInt());
	Registry::WriteInteger(hKey, L"gridwidth", Project.Width);
	Registry::WriteInteger(hKey, L"gridheight", Project.Height);
	Registry::WriteInteger(hKey, L"pixelshape", Project.PixelShapeToInt());
	Registry::WriteInteger(hKey, L"pixelsize", Project.PixelSize);

	Registry::WriteInteger(hKey, L"oncolour",         LEDColoursSingle[1]);
	Registry::WriteInteger(hKey, L"oncolour2",        LEDColoursSingle[2]);
	Registry::WriteInteger(hKey, L"oncolour3",        LEDColoursSingle[3]);
	Registry::WriteInteger(hKey, L"offcolour",        LEDColoursSingle[0]);
	Registry::WriteInteger(hKey, L"selectcolour",     LEDColoursSingle[4]);
	Registry::WriteInteger(hKey, L"lightboxcolour",   LEDColoursSingle[5]);

	Registry::WriteInteger(hKey, L"oncolourbi",       LEDColoursBi[1]);
	Registry::WriteInteger(hKey, L"oncolour2bi",      LEDColoursBi[2]);
	Registry::WriteInteger(hKey, L"oncolour3bi",      LEDColoursBi[3]);
	Registry::WriteInteger(hKey, L"offcolourbi",      LEDColoursBi[0]);
	Registry::WriteInteger(hKey, L"selectcolourbi",   LEDColoursBi[4]);
	Registry::WriteInteger(hKey, L"lightboxcolourbi", LEDColoursBi[5]);

	Registry::WriteInteger(hKey, L"rgbbackground",    RGBBackground);
	Registry::WriteInteger(hKey, L"LEDRGBColoursLMB", LEDRGBColours[1]);
	Registry::WriteInteger(hKey, L"LEDRGBColoursMMB", LEDRGBColours[2]);
	Registry::WriteInteger(hKey, L"LEDRGBColoursRMB", LEDRGBColours[3]);

	Registry::WriteString(hKey, L"language",          App.Language);

	Registry::WriteString(hKey, L"savelocation",      App.LastSaveLocation);
	Registry::WriteString(hKey, L"loadlocation",      App.LastLoadLocation);

	Registry::WriteInteger(hKey, L"customspeed",      App.CustomSpeed);

	Registry::WriteInteger(hKey, L"displaybackground",App.BackgroundColour);

	// =======================================================================

	Registry::WriteInteger(hKey, L"sSelectionLMB",    SelectionColours[0]);
	Registry::WriteInteger(hKey, L"sSelectionMMB",    SelectionColours[1]);
	Registry::WriteInteger(hKey, L"sSelectionRMB",    SelectionColours[2]);

	// =======================================================================

	Registry::WriteInteger(hKey, L"showanimtoolbar", Bars.Animation);
	Registry::WriteInteger(hKey, L"columnrowtoolbar", Bars.ColumnRow);
	Registry::WriteInteger(hKey, L"patterntoolbar", Bars.Pattern);
	Registry::WriteInteger(hKey, L"rgbpalettetoolbar", Bars.RGBPalette);

	Registry::WriteInteger(hKey, L"animspeed",  App.AnimSpeed);

    Registry::WriteInteger(hKey, L"ignorewarnings", App.IgnoreWarnings);

	// =======================================================================

	Registry::WriteInteger(hKey, L"autosave", App.AutoSaveEnabled);
	Registry::WriteInteger(hKey, L"autosaveinterval", App.AutoSaveIntervalToInt());

	// =======================================================================

	Registry::WriteInteger(hKey, L"useformatdata", UseFormatData);

	// =======================================================================

	Registry::WriteInteger(hKey, L"previewactive", Preview.Active);

	Registry::WriteInteger(hKey, L"previewsize", Preview.Size);
	Registry::WriteInteger(hKey, L"previewview", PreviewOptionsHelper::ViewShapeToInt(Preview.View));
	Registry::WriteInteger(hKey, L"previewvoid", Preview.Void);

	Registry::WriteInteger(hKey, L"previewoffset",Preview.Offset);
	Registry::WriteInteger(hKey, L"previewoffsetdirection", Preview.Direction);

	// =======================================================================

	for (int t = 0; t < 16; t++)
	{
		Registry::WriteInteger(hKey, L"rgbpalette" + std::to_wstring(t), RGBPalette[t]);
	}

	// =======================================================================

	int rc = 0;

	switch (RowColumn)
	{
	case RowColumnData::kData:
		rc = 1;
		break;
	case RowColumnData::kIndex:
		rc = 2;
		break;
	}

	Registry::WriteInteger(hKey, L"rowcolumndata", rc);

	// =======================================================================

	Registry::WriteInteger(hKey, L"exportupdatemaxpixels", App.ExportUpdateMaxPixels);
	Registry::WriteInteger(hKey, L"exportpreviewsize",     App.ExportPreviewSize);

	// =======================================================================

	Registry::WriteString(hKey, L"hexprefix2", App.HexPrefix);

	// =======================================================================

	if (FileHistory.size() != 0)
	{
		for (int t = 0; t < FileHistory.size(); t++)
		{
			if (!FileHistory[t].empty())
			{
				Registry::WriteString(hKey, L"reopen_" + std::to_wstring(t), FileHistory[t]);
			}
		}
	}
	else
	{
		for (int t = 0; t < 20; t++)
		{
			Registry::WriteString(hKey, L"reopen_" + std::to_wstring(t), L"");
		}
	}

	// =======================================================================

	Registry::WriteString(hKey, L"exportgiffilename",    ExportGIF.FileName);
	Registry::WriteInteger(hKey, L"exportgifpixelsize",  ExportGIF.PixelSize);
	Registry::WriteInteger(hKey, L"exportgifpixelshape", ExportGIF.PixelShape);
	Registry::WriteInteger(hKey, L"exportgifbackground", ExportGIF.Background);

	// =======================================================================

    RegCloseKey(hKey);

	return true;
}


void SystemSettings::BuildGradientList(const std::wstring path)
{
	Gradients.clear();

	WIN32_FIND_DATAW file;

	HANDLE search_handle = FindFirstFileW(path.c_str(), &file);

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

				Gradients.push_back(Utility::RemoveExtension(s));
			}

		} while (FindNextFileW(search_handle, &file));

		FindClose(search_handle);
	}
}


void SystemSettings::BuildLanguageList(const std::wstring path)
{
	Languages.clear();

	WIN32_FIND_DATAW file;

	HANDLE search_handle = FindFirstFileW(path.c_str(), &file);

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

				Languages.push_back(Utility::RemoveExtension(s));
			}

		} while (FindNextFileW(search_handle, &file));

		FindClose(search_handle);
	}
}


void SystemSettings::RecalculatePadding(MatrixMode mm, int width, int height)
{
	switch (App.PadMode)
	{
	case PadFormat::kAuto:
	{
		App.SetPadModeHexRowFromWidth(width);

		App.SetPadModeHexColFromHeight(height);

		if (mm != MatrixMode::kNone && mm != MatrixMode::kMono)
		{
			App.PadModeHexRow = App.PadModeHexRow * 2;
			App.PadModeHexCol = App.PadModeHexCol * 2;
		}
		break;
	}
	case PadFormat::k8Bits:
		App.SetPadModeHexColRow(2);
		break;
	case PadFormat::k16Bits:
		App.SetPadModeHexColRow(4);
		break;
	case PadFormat::k24Bits:
		App.SetPadModeHexColRow(6);
		break;
	case PadFormat::k32Bits:
		App.SetPadModeHexColRow(8);
		break;
	case PadFormat::k40Bits:
		App.SetPadModeHexColRow(10);
		break;
	case PadFormat::k48Bits:
		App.SetPadModeHexColRow(12);
		break;
	case PadFormat::k56Bits:
		App.SetPadModeHexColRow(14);
		break;
	case PadFormat::k64Bits:
		App.SetPadModeHexColRow(16);
		break;
	}
}
