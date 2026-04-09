// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2026
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
	enum class FileBlock { kNone, kHeader, kIgnoredPixels, kMatrixData, kLayer, kColours, kFrames };

	// ===========================================================================
	// LED Matrix Studio files
	// ===========================================================================

	LoadData LoadDataParameterType(const std::wstring, FileBlock);

	// ===========================================================================

	MatrixColourMode GetMatrixModeFromFileChunk(const wchar_t);

	bool SaveVector(const std::wstring, const std::vector<std::wstring>&);
}
