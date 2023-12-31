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

#include "PreviewSettings.h"


ViewShape PreviewOptionsHelper::IntToViewShape(int v)
{
	switch (v)
	{
	case 0:
		return ViewShape::kSquare;
	case 1:
		return ViewShape::kRadial;
	case 2:
		return ViewShape::kRadial3Q;
	case 3:
		return ViewShape::kSemiCircle;
	case 4:
		return ViewShape::kSemiCircleInverted;
	}

	return ViewShape::kSquare;
}


int PreviewOptionsHelper::ViewShapeToInt(ViewShape vs)
{
	switch (vs)
	{
	case ViewShape::kSquare:
		return 0;
	case ViewShape::kRadial:
		return 1;
	case ViewShape::kRadial3Q:
		return 2;
	case ViewShape::kSemiCircle:
		return 3;
	case ViewShape::kSemiCircleInverted:
		return 4;
	}

	return 0;
}
