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

#include "DataOut.h"
#include "ExportOptions.h"
#include "TheMatrix.h"


namespace ExportRGB
{
	bool CreateExportAnimationRGB(TheMatrix *matrix, ExportOptions, std::vector<std::wstring> &, int&, std::vector<std::wstring> &);

	DataOut ExportColumnDataRGB(TheMatrix *matrix, const std::wstring, ExportOptions, int, int, const std::wstring);
	DataOut ExportRowDataRGB(TheMatrix *matrix, const std::wstring, ExportOptions, int, int, const std::wstring);
}
