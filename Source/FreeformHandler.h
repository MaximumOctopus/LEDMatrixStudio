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
#include "MatrixPixel.h"


class FreeformFrame
{
public:

	bool Locked = false;
};


class MatrixPixelHistory
{
public:
	int X = 0;
    int Y = 0;
	int Colour = 0;

	MatrixPixelHistory(int x, int y, int colour)
	{
		X = x;
		Y = y;
		Colour = colour;
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

	// == undo/redo
    void UpdateHistory();
	void Undo();
	void Redo();
	bool CanUndo();
	bool CanRedo();
	void SetFromUndo(int);
	void ClearAllHistory();

	void InsertBlankFrameAt(int, int);
	void InsertCopyFrameAt(int, int);

    void CopyFromPrevious(int);

    void DeleteFrame(int);

	void Sort();
    void AutoOrderPixels(int);

    void ClearSelection();
	void AddToSelection(int);
	void Move(int, int, int);

	void SetOrder(int, int);
	void SetOrderSwap(int, int);

	void EnsurePixelCoherence();

	void CalculateContrastColour(int);
};
