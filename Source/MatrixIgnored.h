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

#include "FileConstants.h"
#include "MatrixConstants.h"


static const int PixelAlive = 0;
static const int PixelIgnored = 1;


class MatrixIgnored
{

private:

	enum class DataParameter { kUnknown = 0, kIgnoredPixelBegin, kIgnoredPixelEnd, kRowData };

	DataParameter LoadDataParameterType(std::wstring, bool);

    void SimpleLine(int, int, int, int);

public:

	int Width;
	int Height;

	int *Grid;

	MatrixIgnored(int, int);
    ~MatrixIgnored();

	bool Load(const std::wstring);
	bool Save(const std::wstring, int, int);

	void SetFromCustomShape(int, int, CustomShape, int);
	void SetAllPixels(int);
};
