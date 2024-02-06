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

#include "matrixconstants.h"


struct ProjectSettings
{
	bool Valid;

	MatrixMode Mode = MatrixMode::kMono;
	int Width = 0;
	int Height = 0;
	bool Clear = false;
	int Special = 0;
	bool SizeType = false;
	int PixelSize = 20;
	PixelShape Shape = PixelShape::kSquare;

	CustomShape ShapeCustom = CustomShape::kNone;
	int CustomShapeParam = 0;

	int Background = 0;

	void CustomShapeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			ShapeCustom = CustomShape::kNone;
			break;
		case 1:
			ShapeCustom = CustomShape::kCircle;
			break;
		case 2:
			ShapeCustom = CustomShape::kBorders;
			break;
		case 3:
			ShapeCustom = CustomShape::kTriangle;
			break;
		}
	}

	void MatrixModeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Mode = MatrixMode::kNone;
			break;
		case 1:
			Mode = MatrixMode::kMono;
			break;
		case 2:
			Mode = MatrixMode::kBiSequential;
			break;
		case 3:
			Mode = MatrixMode::kBiBitplanes;
			break;
		case 4:
			Mode = MatrixMode::kRGB;
			break;
		case 5:
			Mode = MatrixMode::kRGB3BPP;
			break;

		default:
			Mode = MatrixMode::kNone;
		}
	}

	void PixelShapeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Shape = PixelShape::kSquare;
			break;
		case 1:
			Shape = PixelShape::kCircle;
			break;
		case 2:
			Shape = PixelShape::kRoundRect;
			break;
		}
	}

	int MatrixModeToInt()
	{
		switch (Mode)
		{
		case MatrixMode::kNone:
			return 0;
		case MatrixMode::kMono:
			return 1;
		case MatrixMode::kBiSequential:
			return 2;
		case MatrixMode::kBiBitplanes:
			return 3;
		case MatrixMode::kRGB:
			return 4;
		case MatrixMode::kRGB3BPP:
			return 5;
		}

		return 0;
	}

	int PixelShapeToInt()
	{
		switch (Shape)
		{
		case PixelShape::kSquare:
			return 0;
		case PixelShape::kCircle:
			return 1;
		case PixelShape::kRoundRect:
			return 2;
		}

        return 0;
	}
};
