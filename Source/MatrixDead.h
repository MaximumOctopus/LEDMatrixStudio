// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2023
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


enum class DataParameter { kUnknown = 0, kDeadPixelBegin, kDeadPixelEnd, kRowData };

static const int PixelAlive = 0;
static const int PixelDead = 1;


class MatrixDead
{

private:

	DataParameter LoadDataParameterType(std::wstring, bool);

    void SimpleLine(int, int, int, int);

public:

	int Width;
	int Height;

	int *Grid;

	MatrixDead(int, int);
    ~MatrixDead();

	bool Load(const std::wstring);
	bool Save(const std::wstring, int, int);

	void SetFromCustomShape(int, int, CustomShape, int);
	void SetAllPixels(int);
};
