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

#include "MatrixConstants.h"


namespace FileUtility
{
	MatrixMode GetMatrixModeFromFileChunk(const wchar_t);

	bool SaveVector(const std::wstring, const std::vector<std::wstring>&);
}
