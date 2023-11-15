// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2023
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

	for (int x = 0; x < width; x++)
	{
		for (int y = 0; y < height; y++)
		{
			if (Mode == MatrixMode::kRGB)
			{
				Grid[y * width + x] = Background;
			}
			else
			{
				Grid[y * width + x] = 0;
			}
		}
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
	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			if (Mode == MatrixMode::kRGB)
			{
				Grid[y * Width + x] = Background;
			}
			else
			{
				Grid[y * Width + x] = 0;
			}
		}
	}
}


void Matrix::ClearColour(int Background)
{
	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			Grid[y * Width + x] = Background;
		}
	}
}


void Matrix::ChangePixels(int colour_from, int colour_to)
{
	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			if (Grid[y * Width + x] == colour_from)
			{
				Grid[y * Width + x] = colour_to;
			}
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

	for (int y = 0; y < Height; y++)
	{
		for (int x = 0; x < Width; x++)
		{
			mh->Grid[y * Width + x] = Grid[y * Width + x];
		}
	}

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

	MatrixHistory *mh = new MatrixHistory(this->Grid, m.Width, m.Height);

	for (int y = 0; y < Height; y++)
	{
		for (int x = 0; x < Width; x++)
		{
			mh->Grid[y * Width + x] = m.Grid[y * Width + x];
		}
	}

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

	MatrixHistory *mh = new MatrixHistory(this->Grid, m->Width, m->Height);

	for (int y = 0; y < Height; y++)
	{
		for (int x = 0; x < Width; x++)
		{
			mh->Grid[y * Width + x] = m->Grid[y * Width + x];
		}
	}

	History.push_back(mh);

	HistoryOffset = History.size() - 1;
}


void Matrix::Undo()
{
	if (HistoryOffset != 0)
	{
		HistoryOffset--;
	}

	for (int y = 0; y < Height; y++)
	{
		for (int x = 0; x < Width; x++)
		{
			Grid[y * Width + x] = History[HistoryOffset]->Grid[y * Width + x];
		}
	}
}


void Matrix::Redo()
{
	if (HistoryOffset != History.size() - 1)
	{
		HistoryOffset++;
	}

	MatrixHistory *mh = History[HistoryOffset];

	for (int y = 0; y < Height; y++)
	{
		for (int x = 0; x < Width; x++)
		{
			Grid[y * Width + x] = mh->Grid[y * Width + x];
		}
	}
}


void Matrix::SetFromUndo(int undo)
{
	for (int x = 0; x < Width; x++)
	{
		for (int y = 0; y < Height; y++)
		{
			Grid[y * Width + x] = History[undo]->Grid[y * Width + x];
		}
	}
}
