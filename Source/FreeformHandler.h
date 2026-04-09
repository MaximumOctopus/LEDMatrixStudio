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

#include "MatrixConstants.h"


class FreeformFrame
{
public:

	bool Locked = false;
};


class MatrixPixelHistory
{
public:
	std::vector<int> Colours;

	MatrixPixelHistory(std::vector<int> &colours)
	{
	}
};


class MatrixPixel
{
public:

	int OldX = 0;
    int OldY = 0;
	int X = 0;                              // remain static for each frame of the project
	int Y = 0;                              //
	int Order = 0;                          //
	int Group = 0;                          // to do...
	int Contrast = 0;

	std::vector<int> Colours;        		// one per frame of animation

	MatrixPixel()
	{
	}

	MatrixPixel(int x, int y, int frame_count, int order, int colour)
	{
		X = x;
		Y = y;
		Order = order;

		for (int f = 0; f < frame_count; f++)
		{
			Colours.push_back(colour);
		}
	}

	MatrixPixel(int x, int y, int frame_count, int order, int group, int colour)
	{
		X = x;
		Y = y;
		Order = order;

		for (int f = 0; f < frame_count; f++)
		{
			Colours.push_back(colour);
		}
	}
};


class FreeformHandler
{
public:

	std::vector<MatrixPixel*> Pixels;

	std::vector<FreeformFrame*> Frames;

	std::vector<int> Selection;

	int HistoryOffset = 0;

	int NextGroupId = 1;

	FreeformHandler();

    void ClearAll();
	void Clear(int, MatrixColourMode, int);
	void ClearColour(int, int);

	void ChangePixels(int, int, int);

	void Relocate(int, int);

	void SetAllGroupTo(int, int, int);
    void AddGroupToSelection(int);

	void ShiftColoursLeft(int);
    void ShiftColoursRight(int);

	void AddShapeCircle(int, int, int, int, int, int);
	void AddLineH(int, int, int, int, int);
	void AddLineV(int, int, int, int, int);
	void AddShapeSquare(int, int, int, int, int);
	void AddShapeSquareFilled(int, int, int, int, int, int);
	void AddShapeRectangle(int, int, int, int, int, int);
	void AddShapeRectangleFilled(int, int, int, int, int, int, int);

	void AddToHistory();
	void AddToHistory(MatrixPixelHistory&);
	void AddToHistory(MatrixPixelHistory *m);

	void Undo();
	void Redo();

	void SetFromUndo(int);

	void InsertBlankFrameAt(int, int);
	void InsertCopyFrameAt(int, int);

    void CopyFromPrevious(int);

    void DeleteFrame(int);

	void Sort();

	void AddToSelection(int);
	void Move(int, int, int);

	void SetOrder(int, int);
	void SetOrderSwap(int, int);

	void EnsurePixelCoherence();

	void CalculateContrastColour(int);
};
