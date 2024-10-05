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


namespace ExportOutputBinary
{
	bool BinaryCreateExportAnimation(TheMatrix *matrix, ExportOptions, std::vector<std::wstring> &, int&, std::vector<std::wstring> &);
	bool BinaryCreateExportAnimationRGB(TheMatrix *matrix, ExportOptions, std::vector<std::wstring> &, int&, std::vector<std::wstring> &);
	bool BinaryCreateExportAnimationRGB3bpp(TheMatrix *matrix, ExportOptions, std::vector<std::wstring> &, int &);

	DataOut BinaryExportRowData(TheMatrix *matrix, ExportOptions, int, int);
	DataOut BinaryExportRowDataRGB(TheMatrix *matrix, ExportOptions, int, int);
	DataOut BinaryExportFrameDataByRowRGB3bpp(TheMatrix *matrix, ExportOptions, int);

	DataOut BinaryExportColumnData(TheMatrix *matrix, ExportOptions, int, int);
	DataOut BinaryExportColumnDataRGB(TheMatrix *matrix, ExportOptions, int, int);
	DataOut BinaryExportFrameDataByColumnRGB3bpp(TheMatrix *matrix, ExportOptions, int);

	std::wstring BinaryGetRowData(Matrix *matrix, bool, int, int, int);
	std::wstring BinaryGetColumnData(Matrix *matrix, bool, int, int, int);

	void BinaryAddContentByFrame(ExportOptions, const std::wstring, int, std::vector<std::wstring> &);
}
