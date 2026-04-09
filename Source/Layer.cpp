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

#include "Layer.h"


Layer::Layer(std::wstring name)
{
	Name = name;
}


Layer::~Layer()
{
	if (Cells.size() != 0)
	{
		for (int t = Cells.size() - 1; t >= 0; t--)
		{
		}
	}

	if (Freeform != nullptr)
	{
        delete Freeform;
	}
}


int Layer::FindPixel(int x, int y, int width)
{
	for (int p = 0; p < Freeform->Pixels.size(); p++)
	{
		MatrixPixel *pixel = Freeform->Pixels[p];

		if (x >= pixel->X && x <= pixel->X + width &&
			y >= pixel->Y && y <= pixel->Y + width)
		{
			return p;
		}
	}

	return -1;
}