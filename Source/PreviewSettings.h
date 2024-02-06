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


struct PreviewOptions
{
	bool Active = false;
	bool CanDraw = false;                           // can the user draw directly to the preview canvas

	int Size = 1;
	ViewShape View = ViewShape::kSquare;

	PixelShape Shape = PixelShape::kSquare;         // selected by user
	PixelShape DisplayShape = PixelShape::kSquare;  // used to render preview (may change from above depending on pixel size)

	int Void = 15;
	int Offset = 0;
    int Direction = 0;

	int RPixel = 1;		// size of pixel in radial mode
	int ROffset = 15;	// size of the inner void (pixels, radius) for radial/semi-circle

	int OldSize = 0;
	bool IncrementRadially = false;

    bool Popout = false;

	void ViewShapeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			View = ViewShape::kSquare;
			break;
		case 1:
			View = ViewShape::kRadial;
			break;
		case 2:
			View = ViewShape::kRadial3Q;
			break;
		case 3:
			View = ViewShape::kSemiCircle;
			break;
		case 4:
			View = ViewShape::kSemiCircleInverted;
			break;
		}
	}
};


namespace PreviewOptionsHelper
{
	ViewShape IntToViewShape(int v);
	int ViewShapeToInt(ViewShape);
}
