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


static const int CDrawPointNone = 0;
static const int CDrawPointFirst = 1;
static const int CDrawPointLast = 2;


enum class ActionMode { kNone = 0,
					  kFilledBox, kEmptyBox,
					  kLine, kFont,
					  kEmptyCircle, kFilledCircle,
					  kRandom, kMulti, kPicker,
					  kCopy, kPaste,
					  kGradientBrush,
					  kFloodFill,
					  kSpiral, kRing, kSplitRing, kPetals, kGrid, kPyramid, kLeftTriangle, kRightTriangle,
					  kLeftAngleLine, kRightAngleLine,
					  kMovePixel, kDrawOrder };

enum class DrawPoint { kNone, kFirst, kLast };


struct ActionData
{
	ActionMode Mode = ActionMode::kNone;
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
		Mode = ActionMode::kNone;

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

	bool IsSinglePointMode(ActionMode mode)
	{
		switch (mode)
		{
		case ActionMode::kFloodFill:
		case ActionMode::kSpiral:
		case ActionMode::kRing:
		case ActionMode::kSplitRing:
		case ActionMode::kPetals:
		case ActionMode::kGrid:
		case ActionMode::kPyramid:
		case ActionMode::kLeftTriangle:
		case ActionMode::kRightTriangle:
			return true;

		default:
			break;
		}

		return false;
	}

	void SetModeFromInt(int m)
	{
		switch (m)
		{
		case 0:
			Mode = ActionMode::kNone;
			break;
		case 1:
			Mode = ActionMode::kFilledBox;
            break;
		case 2:
			Mode = ActionMode::kEmptyBox;
			break;
		case 3:
			Mode = ActionMode::kLine;
			break;
		case 4:
			Mode = ActionMode::kFont;
			break;
		case 5:
			Mode = ActionMode::kEmptyCircle;
			break;
		case 6:
			Mode = ActionMode::kFilledCircle;
			break;
		case 7:
			Mode = ActionMode::kRandom;
			break;
		case 8:
			Mode = ActionMode::kMulti;
			break;
		case 9:
			Mode = ActionMode::kPicker;
			break;
		case 10:
			Mode = ActionMode::kCopy;
			break;
		case 11:
			Mode = ActionMode::kPaste;
			break;
		case 12:
			Mode = ActionMode::kGradientBrush;
			break;
		case 13:
			Mode = ActionMode::kFloodFill;
			break;
		case 14:
			Mode = ActionMode::kSpiral;
			break;
		case 15:
			Mode = ActionMode::kRing;
			break;
		case 16:
			Mode = ActionMode::kSplitRing;
			break;
		case 17:
			Mode = ActionMode::kPetals;
			break;
		case 18:
			Mode = ActionMode::kGrid;
			break;
		case 19:
			Mode = ActionMode::kPyramid;
			break;
		case 20:
			Mode = ActionMode::kLeftTriangle;
			break;
		case 21:
			Mode = ActionMode::kRightTriangle;
			break;
		case 22:
			Mode = ActionMode::kLeftAngleLine;
			break;
		case 23:
			Mode = ActionMode::kRightAngleLine;
			break;
		case 24:
			Mode = ActionMode::kMovePixel;
			break;
		}
	}

	int DrawModeToInt(ActionMode dm)
		{
		switch (dm)
		{
		case ActionMode::kNone:
			return 0;
		case ActionMode::kFilledBox:
			return 1;
		case ActionMode::kEmptyBox:
			return 2;
		case ActionMode::kLine:
			return 3;
		case ActionMode::kFont:
			return 4;
		case ActionMode::kEmptyCircle:
			return 5;
		case ActionMode::kFilledCircle:
			return 6;
		case ActionMode::kRandom:
			return 7;
		case ActionMode::kMulti:
			return 8;
		case ActionMode::kPicker:
			return 9;
		case ActionMode::kCopy:
			return 10;
		case ActionMode::kPaste:
			return 11;
		case ActionMode::kGradientBrush:
			return 12;
		case ActionMode::kFloodFill:
			return 13;
		case ActionMode::kSpiral:
			return 14;
		case ActionMode::kRing:
			return 15;
		case ActionMode::kSplitRing:
			return 16;
		case ActionMode::kPetals:
			return 17;
		case ActionMode::kGrid:
			return 18;
		case ActionMode::kPyramid:
			return 19;
		case ActionMode::kLeftTriangle:
			return 20;
		case ActionMode::kRightTriangle:
			return 21;
		case ActionMode::kLeftAngleLine:
			return 22;
		case ActionMode::kRightAngleLine:
			return 23;
		case ActionMode::kMovePixel:
			return 24;
		}

		return 0;
	}
};
