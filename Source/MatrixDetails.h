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

#include "MatrixConstants.h"


struct MatrixDetails
{
	bool Available = false;

	MatrixDrawMode DrawMode = MatrixDrawMode::kGrid;
	MatrixColourMode ColourMode = MatrixColourMode::kNone;

	int Width = 0;			// actual width of matrix in pixels
	int Height = 0;			// actual height of matrix in pixels

	int CanvasWidth = 800;    // width of canvas in freeform mode
	int CanvasHeight = 600;   // height of canvas in freeform mode

	bool Grid = false;

	std::wstring Comment = L"";

	void SetNew(int w, int h, MatrixDrawMode mdm, MatrixColourMode mcm)
	{
		if (mdm == MatrixDrawMode::kGrid)
		{
			Width = w;
			Height = h;

			CanvasWidth = 800;
			CanvasHeight = 600;
		}
		else
		{
			Width = 0;
			Height = 0;

			CanvasWidth = w;
			CanvasHeight = h;
		}

		DrawMode = mdm;
		ColourMode = mcm;
	}
};


struct MatrixRendering
{
	int PixelSize = 1;
	int PixelSizeZ = 1;
	PixelShape Shape = PixelShape::kSquare;
	BrushSize Brush = BrushSize::kSmall;
	bool ShowPixelOrder = false;
	bool ShowPixelGroup = false;
	bool ApplyToGroup = false;
	bool ShowFrameCount = false;

	TPoint TopLeft = { 0,  0 };		// index of the top left pixel (on screen) in x and y direction
									// used when matrix is larger than display
	TPoint BottomRight = { 0, 0 };	//
									// used when matrix is larger than display

	TPoint ViewWindow = { 0, 0, };	// width and height, in pixels, of the display

	MatrixGradient Gradient;

	ActionData Action;

	void SetNew(PixelShape pixelshape, int width, int height)
	{
		Gradient.Option = GradientOption::kOff;
		Shape = pixelshape;

		TopLeft.X = 0;
		TopLeft.Y = 0;

		BottomRight.X = width - 1;
		BottomRight.Y = height - 1;
		ViewWindow.X = width - 1;
		ViewWindow.Y = height - 1;

		Action.Mode = ActionMode::kNone;
		Action.Point = CDrawPointNone;
		Action.Colour = 0;
		Action.Coords[0].X = -1;
		Action.Coords[0].Y = -1;
		Action.CopyPos.X = 0;
		Action.CopyPos.Y = 0;
	}
};