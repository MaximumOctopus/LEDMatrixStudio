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

#include "ExportOptions.h"
#include "TheMatrix.h"


namespace Optimiser
{
	bool OptimiseData(TheMatrix *thematrix, ExportOptions teo, std::vector<std::wstring> &data);
	bool OptimiseDataSimple(TheMatrix *thematrix, ExportOptions teo, std::vector<std::wstring> &data, std::vector<std::wstring> &output);

	void ProcessUnique(std::vector<std::wstring> &data, std::vector<std::wstring> &unique_items);
}
