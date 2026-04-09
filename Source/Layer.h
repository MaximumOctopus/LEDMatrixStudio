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

#include "FreeformHandler.h"
#include "MatrixGrid.h"


class Layer
{
private:

public:

	std::vector<MatrixGrid*> Cells;         // for Grid projects
	FreeformHandler* Freeform = nullptr;    // for Freeform projects (only ONE frame per PROJECT)
//    GridHandler* Grid = nullptr;

	std::wstring Name = L"";
	bool Locked = false;
	bool Visible = true;

	Layer(std::wstring);
    ~Layer();

	void Clear(int, MatrixColourMode, int);
	void ClearColour(int);

	void ChangePixels(int, int);

	int FindPixel(int, int, int);
};
