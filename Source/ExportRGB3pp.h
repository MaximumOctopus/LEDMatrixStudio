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

// 3 bits per pixel mode
// the following 8 colours are set to the following bit values
//   000    black
//   001    blue
//   010    green
//   011    cyan
//   100    red
//   101    magenta
//   110    yellow
//   111    white

#pragma once

#include <vector>

#include "DataOut.h"
#include "TheMatrix.h"


namespace ExportRGB3BPP
{
	bool CreateExportAnimationRGB3BPP(TheMatrix *matrix, ExportOptions, std::vector<std::wstring> &, int&, std::vector<std::wstring> &);

	DataOut ExportColumnDataRGB3BPP(TheMatrix *matrix, const std::wstring, ExportOptions, int, int, const std::wstring, int);
	DataOut ExportRowDataRGB3BPP(TheMatrix *matrix, const std::wstring, ExportOptions, int, int, const std::wstring, int);
}
