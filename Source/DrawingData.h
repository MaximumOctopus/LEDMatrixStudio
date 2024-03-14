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
					  kSpiral, kRing, kSplitRing, kPetals, kGrid, kPyramid, kLeftTriangle, kRightTriangle,
					  kLeftAngleLine, kRightAngleLine };

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

	void Clear()
	{
		Mode = DrawMode::kNone;

        Reset();
	}

	void Reset()
	{
		Point = CDrawPointNone;
		Coords[0].X = -1;
		Coords[0].Y = -1;
		Special = -1;
        CopyPos = { -1, -1 };
	}

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

	int DrawModeToInt(DrawMode dm)
		{
		switch (dm)
		{
		case DrawMode::kNone:
			return 0;
		case DrawMode::kFilledBox:
			return 1;
		case DrawMode::kEmptyBox:
			return 2;
		case DrawMode::kLine:
			return 3;
		case DrawMode::kFont:
			return 4;
		case DrawMode::kEmptyCircle:
			return 5;
		case DrawMode::kFilledCircle:
			return 6;
		case DrawMode::kRandom:
			return 7;
		case DrawMode::kMulti:
			return 8;
		case DrawMode::kPicker:
			return 9;
		case DrawMode::kCopy:
			return 10;
		case DrawMode::kPaste:
			return 11;
		case DrawMode::kGradientBrush:
			return 12;
		case DrawMode::kFloodFill:
			return 13;
		case DrawMode::kSpiral:
			return 14;
		case DrawMode::kRing:
			return 15;
		case DrawMode::kSplitRing:
			return 16;
		case DrawMode::kPetals:
			return 17;
		case DrawMode::kGrid:
			return 18;
		case DrawMode::kPyramid:
			return 19;
		case DrawMode::kLeftTriangle:
			return 20;
		case DrawMode::kRightTriangle:
			return 21;
		case DrawMode::kLeftAngleLine:
			return 22;
		case DrawMode::kRightAngleLine:
			return 23;
		}

		return 0;
	}
};
