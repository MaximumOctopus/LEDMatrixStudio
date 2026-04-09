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

#include "MatrixGrid.h"


MatrixGrid::MatrixGrid(int width, int height, MatrixColourMode Mode, int Background)
{
	Width = width;
	Height = height;

	Grid = new int[width * height];

	if (Mode == MatrixColourMode::kRGB)
	{
		for (int z = 0; z < width * height; z++)
		{
			Grid[z] = Background;
		}
	}
	else
	{
		std::memset(Grid, 0, width * height * sizeof(int));
	}

	MatrixGridHistory *mh = new MatrixGridHistory(Grid, width, height);

	History.push_back(mh);
}


MatrixGrid::~MatrixGrid()
{
	if (Grid != nullptr)
	{
		delete[] Grid;
	}

	Grid = nullptr;
}


void MatrixGrid::Clear(MatrixColourMode Mode, int Background)
{
	if (Mode == MatrixColourMode::kRGB)
	{
		for (int z = 0; z < Width * Height; z++)
		{
			Grid[z] = Background;
		}
	}
	else
	{
		std::memset(Grid, 0, Width * Height * sizeof(int));
	}
}


void MatrixGrid::ClearColour(int Background)
{
	for (int z = 0; z < Width * Height; z++)
	{
		Grid[z] = Background;
	}
}


void MatrixGrid::ChangePixels(int colour_from, int colour_to)
{
	for (int z = 0; z < Width * Height; z++)
	{
		if (Grid[z] == colour_from)
		{
			Grid[z] = colour_to;
		}
	}
}


void MatrixGrid::SafePlot(int x, int y, int colour)
{
	if (x >= 0 && x < Width && y >= 0 && y < Height)
	{
		Grid[y * Width + x] = colour;
	}
}


void MatrixGrid::AddToHistory()
{
	if (HistoryOffset != History.size() - 1)
	{
		for (int t = History.size() - 1; t >= HistoryOffset + 1; t--)
		{
			delete History.back();
			History.pop_back();
		}
	}

	if (History.size() > __MaximumHistory)
	{
		delete History[0];
		History.erase(History.begin());
	}

	MatrixGridHistory *mh = new MatrixGridHistory(this->Grid, Width, Height);

	std::memcpy(mh->Grid, Grid, Width * Height * sizeof(int));

	History.push_back(mh);

	HistoryOffset = History.size() - 1;
}


void MatrixGrid::AddToHistory(MatrixGrid &m)
{
	if (HistoryOffset != History.size() - 1)
	{
		for (int t = History.size() - 1; t >= HistoryOffset + 1; t--)
		{
			History.pop_back();
		}
	}

	if (History.size() > __MaximumHistory)
	{
		delete History[0];
		History.erase(History.begin());
	}

	MatrixGridHistory *mh = new MatrixGridHistory(static_cast<MatrixGrid&>(m).Grid, static_cast<MatrixGrid&>(m).Width, static_cast<MatrixGrid&>(m).Height);

	std::memcpy(mh->Grid, static_cast<MatrixGrid&>(m).Grid, Width * Height * sizeof(int));

	History.push_back(mh);

	HistoryOffset = History.size() - 1;
}


void MatrixGrid::AddToHistory(MatrixGrid *m)
{
	if (HistoryOffset != History.size() - 1)
	{
		for (int t = History.size() - 1; t >= HistoryOffset + 1; t--)
		{
			History.pop_back();
		}
	}

	if (History.size() > __MaximumHistory)
	{
		delete History[0];
		History.erase(History.begin());
	}

	MatrixGridHistory *mh = new MatrixGridHistory(static_cast<MatrixGrid*>(m)->Grid, static_cast<MatrixGrid*>(m)->Width, static_cast<MatrixGrid*>(m)->Height);

	std::memcpy(mh->Grid, static_cast<MatrixGrid*>(m)->Grid, Width * Height * sizeof(int));

	History.push_back(mh);

	HistoryOffset = History.size() - 1;
}


void MatrixGrid::Undo()
{
	if (HistoryOffset != 0)
	{
		HistoryOffset--;
	}

	std::memcpy(Grid, History[HistoryOffset]->Grid, Width * Height * sizeof(int));
}


void MatrixGrid::Redo()
{
	if (HistoryOffset != History.size() - 1)
	{
		HistoryOffset++;
	}

	MatrixGridHistory *mh = History[HistoryOffset];

	std::memcpy(Grid, mh->Grid, Width * Height * sizeof(int));
}


void MatrixGrid::SetFromUndo(int undo)
{
	std::memcpy(Grid, History[undo]->Grid, Width * Height * sizeof(int));
}
