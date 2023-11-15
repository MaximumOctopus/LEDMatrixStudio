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


static const int CDrawPointNone = 0;
static const int CDrawPointFirst = 1;
static const int CDrawPointLast = 2;


enum class DrawMode { kNone = 0,
					  kFilledBox, kEmptyBox,
					  kLine, kFont,
					  kEmptyCircle, kFilledCircle,
					  kRandom, kMulti, kPicker,
					  kCopy, kPaste,
					  kGradientBrush,
					  kFloodFill,
					  kSpiral, kRing, kSplitRing, kPetals, kGrid, kPyramid, kLeftTriangle, kRightTriangle };

enum class DrawPoint { kNone, kFirst, kLast };


struct DrawData
{
	DrawMode Mode = DrawMode::kNone;
	int Point = CDrawPointNone;
	int	Colour = 0x00000000;
	TPoint Coords[2] = { { -1, -1 } , { -1, -1 } };
	int	Special = -1;

	TPoint CopyPos = { -1, -1 };

	int	ParameterMin = -1;
	int	ParameterMax = -1;
	int	Parameter = -1;

	bool SinglePoint = false;		// this draw mode only requires a single click to render

	bool IsSinglePointMode(DrawMode mode)
	{
		switch (mode)
		{
		case DrawMode::kFloodFill:
		case DrawMode::kSpiral:
		case DrawMode::kRing:
		case DrawMode::kSplitRing:
		case DrawMode::kPetals:
		case DrawMode::kGrid:
		case DrawMode::kPyramid:
		case DrawMode::kLeftTriangle:
		case DrawMode::kRightTriangle:
			return true;
		}

		return false;
	}

	void SetModeFromInt(int m)
	{
		switch (m)
		{
		case 0:
			Mode = DrawMode::kNone;
			break;
		case 1:
			Mode = DrawMode::kFilledBox;
            break;
		case 2:
			Mode = DrawMode::kEmptyBox;
			break;
		case 3:
			Mode = DrawMode::kLine;
			break;
		case 4:
			Mode = DrawMode::kFont;
			break;
		case 5:
			Mode = DrawMode::kEmptyCircle;
			break;
		case 6:
			Mode = DrawMode::kFilledCircle;
			break;
		case 7:
			Mode = DrawMode::kRandom;
			break;
		case 8:
			Mode = DrawMode::kMulti;
			break;
		case 9:
			Mode = DrawMode::kPicker;
			break;
		case 10:
			Mode = DrawMode::kCopy;
			break;
		case 11:
			Mode = DrawMode::kPaste;
			break;
		case 12:
			Mode = DrawMode::kGradientBrush;
			break;
		case 13:
			Mode = DrawMode::kFloodFill;
			break;
		case 14:
			Mode = DrawMode::kSpiral;
			break;
		case 15:
			Mode = DrawMode::kRing;
			break;
		case 16:
			Mode = DrawMode::kSplitRing;
			break;
		case 17:
			Mode = DrawMode::kPetals;
			break;
		case 18:
			Mode = DrawMode::kGrid;
			break;
		case 19:
			Mode = DrawMode::kPyramid;
			break;
		case 20:
			Mode = DrawMode::kLeftTriangle;
			break;
		case 21:
			Mode = DrawMode::kRightTriangle;
			break;
		}
	}
};
