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

#include "MatrixConstants.h"


class MatrixGradient
{

public:

	GradientOption Option = GradientOption::kOff;

	int IY[__MaxHeight];            // gradient in y axis
	int IX[__MaxWidth];             // gradient in x axis

	int Width = 0;
    int Height = 0;

	bool Load(const std::wstring);
	bool Save(const std::wstring);

	void Clear(int colour);
};
