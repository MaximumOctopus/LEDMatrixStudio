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

#include "Matrix.h"

static const int __MaximumHistory = 200;


Matrix::Matrix(int width, int height, MatrixMode Mode, int Background)
{
	Width = width;
	Height = height;

	Grid = new int[width * height];

	if (Mode == MatrixMode::kRGB)
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

	MatrixHistory *mh = new MatrixHistory(Grid, width, height);

	History.push_back(mh);
}


Matrix::~Matrix()
{
	if (Grid != nullptr)
	{
		delete[] Grid;
	}

	Grid = nullptr;
}


void Matrix::Clear(MatrixMode Mode, int Background)
{
	if (Mode == MatrixMode::kRGB)
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


void Matrix::ClearColour(int Background)
{
	for (int z = 0; z < Width * Height; z++)
	{
		Grid[z] = Background;
	}
}


void Matrix::ChangePixels(int colour_from, int colour_to)
{
	for (int z = 0; z < Width * Height; z++)
	{
		if (Grid[z] == colour_from)
		{
	 		Grid[z] = colour_to;
		}
	}
}


void Matrix::SafePlot(int x, int y, int colour)
{
	if (x >= 0 && x < Width && y >= 0 && y < Height)
	{
		Grid[y * Width + x] = colour;
	}
}


void Matrix::AddToHistory()
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

	MatrixHistory *mh = new MatrixHistory(this->Grid, Width, Height);

	std::memcpy(mh->Grid, Grid, Width * Height * sizeof(int));

	History.push_back(mh);

	HistoryOffset = History.size() - 1;
}


void Matrix::AddToHistory(Matrix &m)
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

	MatrixHistory *mh = new MatrixHistory(m.Grid, m.Width, m.Height);

	std::memcpy(mh->Grid, m.Grid, Width * Height * sizeof(int));

	History.push_back(mh);

	HistoryOffset = History.size() - 1;
}


void Matrix::AddToHistory(Matrix *m)
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

	MatrixHistory *mh = new MatrixHistory(m->Grid, m->Width, m->Height);

	std::memcpy(mh->Grid, m->Grid, Width * Height * sizeof(int));

	History.push_back(mh);

	HistoryOffset = History.size() - 1;
}


void Matrix::Undo()
{
	if (HistoryOffset != 0)
	{
		HistoryOffset--;
	}

	std::memcpy(Grid, History[HistoryOffset]->Grid, Width * Height * sizeof(int));
}


void Matrix::Redo()
{
	if (HistoryOffset != History.size() - 1)
	{
		HistoryOffset++;
	}

	MatrixHistory *mh = History[HistoryOffset];

	std::memcpy(Grid, mh->Grid, Width * Height * sizeof(int));
}


void Matrix::SetFromUndo(int undo)
{
	std::memcpy(Grid, History[undo]->Grid, Width * Height * sizeof(int));
}
