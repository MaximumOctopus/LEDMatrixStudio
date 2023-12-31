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

#include <vector>

#include "MatrixConstants.h"


class Matrix;


class MatrixHistory
{
public:

	int *Grid = nullptr;

	MatrixHistory(int *OwnerGrid, int Width, int Height)
	{
		Grid = new int[Width * Height];

		for (int i = 0; i < Width * Height; i++)
		{
			Grid[i] = OwnerGrid[i];
		}
	}

	~MatrixHistory()
	{
		if (Grid != nullptr)
		{
			delete[] Grid;
        }
	}
};



class Matrix
{

public:

	int *Grid = nullptr;

	int Width = 0;
	int Height = 0;

	bool Locked = false;

	int HistoryOffset = 0;

	std::vector<MatrixHistory*> History;

	Matrix(int, int, MatrixMode, int);

	~Matrix();


	void Clear(MatrixMode, int);
	void ClearColour(int);

	void ChangePixels(int, int);

	void SafePlot(int, int, int);

	void AddToHistory();
	void AddToHistory(Matrix&);
	void AddToHistory(Matrix *m);

	void Undo();
	void Redo();

	void SetFromUndo(int);
};
