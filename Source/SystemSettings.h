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

#include <vector>

#include "ApplicationSettings.h"
#include "ExportGIFSettings.h"
#include "PreviewSettings.h"
#include "ProjectSettings.h"
#include "Toolbars.h"


enum class RowColumnData { kOff = 0, kData, kIndex };
enum class PixelSize { kSizeAuto = 0, kSize10, kSize15, kSize20, kSize25, kSize30, kSize40, kSize50 };


static const int CPixelSizeAuto = 0;
static const int CPixelSize10   = 10;
static const int CPixelSize15   = 15;
static const int CPixelSize20   = 20;
static const int CPixelSize25   = 25;
static const int CPixelSize30   = 30;
static const int CPixelSize40   = 40;
static const int CPixelSize50   = 50;


class SystemSettings
{

private:

	static constexpr int defaultRGBPalatte[] = { 0x000000, 0xFFFFFF, 0x0000FF, 0x0088FF, 0x0044FF, 0x00FFFF, 0x88FFFF, 0x44FFFF,
										  0x00FF00, 0x88FF88, 0x44FF44, 0xFF0000, 0xFF8800, 0xFF00FF, 0xFF44FF, 0xFF88FF };

	HKEY hKey;

public:

	AppSettings App;
	ProjectSettings Project;

	int LEDColoursSingle[6] = { clBlack, clWhite, clBlack, clBlack, clBlue, 0x00AAAAAA };
	int LEDColoursBi[6] = { clBlack, clRed, clGreen, clYellow, clBlue, 0x00AAAAAA };
	int RGBBackground = clBlack;
	int LEDRGBColours[6] = { clRed, clBlue, clYellow, clBlack, clBlack, clBlack };
	int SelectionColours[3] = { 2, 1, 0 };
	Toolbars Bars;
	RowColumnData RowColumn = RowColumnData::kOff;
	bool UseFormatData = false;
	PreviewOptions Preview;
	int RGBPalette[16] = { 0x000000, 0xFFFFFF, 0x0000FF, 0x0088FF, 0x0044FF, 0x00FFFF, 0x88FFFF, 0x44FFFF,
						   0x00FF00, 0x88FF88, 0x44FF44, 0xFF0000, 0xFF8800, 0xFF00FF, 0xFF44FF, 0xFF88FF };

	ExportGIFSettings ExportGIF;

	std::vector<std::wstring> FileHistory;

	std::vector<std::wstring> Gradients;
	std::vector<std::wstring> Languages;

	SystemSettings();

	bool LoadSettings();
	bool SaveSettings();

	void BuildLanguageList(const std::wstring);
	void BuildGradientList(const std::wstring);

    void RecalculatePadding(MatrixMode, int, int);
};
