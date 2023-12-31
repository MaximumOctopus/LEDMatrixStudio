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

#include "matrix.h"


class Layer
{
private:

public:

	std::vector<Matrix*> Cells;

	std::wstring Name = L"";
	bool Locked = false;
	bool Visible = true;

	Layer(std::wstring name)
	{
		Name = name;
	}
};
