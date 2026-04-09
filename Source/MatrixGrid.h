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


static const int __MaximumHistory = 200;


class MatrixGridHistory
{
public:

	int *Grid = nullptr;

	MatrixGridHistory(int *OwnerGrid, int Width, int Height)
	{
		Grid = new int[Width * Height];

		for (int i = 0; i < Width * Height; i++)
		{
			Grid[i] = OwnerGrid[i];
		}
	}

	~MatrixGridHistory()
	{
		if (Grid != nullptr)
		{
			delete[] Grid;
        }
	}
};


class MatrixGrid
{
public:

	int *Grid = nullptr;

	bool Locked = false;

	int HistoryOffset = 0;

	int Width = 0;
	int Height = 0;

	std::vector<MatrixGridHistory*> History;

	MatrixGrid(int, int, MatrixColourMode, int);

	~MatrixGrid();

	void Clear(MatrixColourMode, int);
	void ClearColour(int);

	void ChangePixels(int, int);

	void SafePlot(int, int, int);

	void AddToHistory();
	void AddToHistory(MatrixGrid&);
	void AddToHistory(MatrixGrid *m);

	void Undo();
	void Redo();

	void SetFromUndo(int);
};
