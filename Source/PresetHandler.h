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

#include "FileConstants.h"
#include "MatrixConstants.h"
#include "Utility.h"


struct MatrixPreset
{
	bool Configured = false;

	int Width = 0;
	int Height = 0;
	int PixelSize = 0;
	int PixelShape = 0;

	MatrixMode Mode = MatrixMode::kNone;
	std::wstring MatrixModeText = L"";
	int MatrixModeTag = 0;

	void SetMatrixModeFromInt(int mm)
	{
		MatrixModeTag = mm;

		Mode = ConstantsHelper::MatrixModeFromInt(mm);

		MatrixModeText = ConstantsHelper::MatrixModeAsString(Mode);
    }
};


class PresetHandler
{

private:

	enum class MatrixPresetParameter { kUnknown = 0, kStructBegin, kStructEnd, kProjectWidth, kProjectHeight,
		kSource, kSourceLSB, kSourceDirection, kUnused, kPixelSize, kMatrixType };

	MatrixPresetParameter GetMatrixPresetParameterType(const std::wstring);

public:

	std::vector<std::wstring> Presets;

	PresetHandler(const std::wstring);

	void PopulateList(const std::wstring);

	MatrixPreset Load(const std::wstring);

	bool Save(std::wstring, MatrixPreset&);
};
