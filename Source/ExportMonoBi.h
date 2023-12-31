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

#include <string>
#include <vector>

#include "DataOut.h"
#include "ExportOptions.h"
#include "TheMatrix.h"


namespace ExportMonoBi
{
	bool CreateExportAnimation(TheMatrix *matrix, ExportOptions, std::vector<std::wstring> &, int&, std::vector<std::wstring> &);

	DataOut ExportColumnData(TheMatrix *matrix, ExportOptions, int, int, const std::wstring);
	DataOut ExportRowData(TheMatrix *matrix, ExportOptions, int, int, const std::wstring);

	// used only for quick GUI-based output
	DataOutDisplay SimpleExportMono(TheMatrix *matrix, int, int, int, int, bool, bool, bool);
	DataOutDisplay SimpleExportBiSequential(TheMatrix *matrix, int, int, int, int, bool, bool);
	DataOutDisplay SimpleExportBiBitplanes(TheMatrix *matrix, int, int, int, int, bool, bool);
}
