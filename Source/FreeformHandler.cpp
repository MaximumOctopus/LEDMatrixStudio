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

#include <algorithm>

#include "ColourUtility.h"
#include "FreeformHandler.h"


bool sortByOrder(const MatrixPixel* lhs, const MatrixPixel* rhs)
{
	return lhs->Order < rhs->Order;
}


FreeformHandler::FreeformHandler()
{
	FreeformFrame *fff = new FreeformFrame();

    Frames.push_back(fff);
}


void FreeformHandler::ClearAll()
{
	Pixels.clear();

    Frames.clear();

    NextGroupId = 1;
}


void FreeformHandler::Clear(int frame, MatrixColourMode Mode, int Background)
{
	for (int p = 0; p < Pixels.size(); p++)
	{
		Pixels[p]->Colours[frame] = Background;
	}
}


void FreeformHandler::ClearColour(int frame, int Background)
{
	for (int p = 0; p < Pixels.size(); p++)
	{
		Pixels[p]->Colours[frame] = Background;
	}
}


void FreeformHandler::ChangePixels(int frame, int colour_from, int colour_to)
{
	for (int p = 0; p < Pixels.size(); p++)
	{
		if (Pixels[p]->Colours[frame] == colour_from)
		{
			Pixels[p]->Colours[frame] = colour_to;
		}
	}
}


void FreeformHandler::Relocate(int old_pixel_size, int new_pixel_size)
{
	double coeff = (double)new_pixel_size / (old_pixel_size);

	for (int p = 0; p < Pixels.size(); p++)
	{
		Pixels[p]->X = std::round(Pixels[p]->X * coeff);
		Pixels[p]->Y = std::round(Pixels[p]->Y * coeff);
	}
}


void FreeformHandler::SetAllGroupTo(int group, int frame, int colour)
{
	for (int p = 0; p < Pixels.size(); p++)
	{
		if (Pixels[p]->Group == group)
		{
			Pixels[p]->Colours[frame] = colour;
		}
	}
}


void FreeformHandler::ShiftColoursLeft(int frame)
{
	Sort();

	int temp = Pixels[0]->Colours[frame];

	for (int p = 0; p < Pixels.size() - 1; p++)
	{
		Pixels[p]->Colours[frame] = Pixels[p + 1]->Colours[frame];
	}

	Pixels.back()->Colours[frame] = temp;
}


void FreeformHandler::ShiftColoursRight(int frame)
{
    Sort();

	int temp = Pixels.back()->Colours[frame];

	for (int p = Pixels.size() - 1; p >= 1; p--)
	{
		Pixels[p]->Colours[frame] = Pixels[p - 1]->Colours[frame];
	}

	Pixels[0]->Colours[frame] = temp;
}


void FreeformHandler::AddShapeCircle(int size, int pixels, int pixel_width, int x, int y, int colour)
{
	double angle = 6.28318530718 / (double)pixels;

	double current_angle = 0;

	int hpw = std::round((double)pixel_width / 2);

	for (int t = 0; t < pixels; t++)
	{
		double px = (double)x + (double)size * std::cos(current_angle);
		double py = (double)y + (double)size * std::sin(current_angle);

		MatrixPixel *mp = new MatrixPixel(px - hpw, py - hpw, Frames.size(), Pixels.size(), NextGroupId, colour);

		Pixels.push_back(mp);

		current_angle += angle;
	}

	NextGroupId++;
}


void FreeformHandler::AddLineH(int size, int pixel_width, int x, int y, int colour)
{
	int current_x = x;

	for (int t = 0; t < size - 1; t++)
	{
		MatrixPixel *mp = new MatrixPixel(current_x, y, Frames.size(), Pixels.size(), NextGroupId, colour);

		Pixels.push_back(mp);

		current_x += pixel_width + 1;
	}

	NextGroupId++;
}


void FreeformHandler::AddLineV(int size, int pixel_width, int x, int y, int colour)
{
	int current_y = y;

	for (int t = 0; t < size - 1; t++)
	{
		MatrixPixel *mp = new MatrixPixel(x, current_y, Frames.size(), Pixels.size(), NextGroupId, colour);

		Pixels.push_back(mp);

		current_y += pixel_width + 1;
	}

	NextGroupId++;
}


void FreeformHandler::AddShapeSquare(int size, int pixel_width, int x, int y, int colour)
{
	int current_x = x;
	int current_y = y;

	for (int side = 0; side < 4; side++)
	{
		for (int t = 0; t < size - 1; t++)
		{
			MatrixPixel *mp = new MatrixPixel(current_x, current_y, Frames.size(), Pixels.size(), NextGroupId, colour);

			Pixels.push_back(mp);

			switch (side)
			{
			case 0:
				current_x += pixel_width + 1;
				break;
			case 1:
				current_y += pixel_width + 1;
				break;
			case 2:
				current_x -= pixel_width + 1;
				break;
			case 3:
				current_y -= pixel_width + 1;
				break;
			}
		}
	}

	NextGroupId++;
}


void FreeformHandler::AddShapeSquareFilled(int size, int direction, int pixel_width, int x, int y, int colour)
{
	int current_direction = direction;
	int current_x = x;
	int current_y = y;

	for (int x = 0; x < size; x++)
	{
		for (int y = 0; y < size; y++)
		{
			MatrixPixel *mp = new MatrixPixel(current_x, current_y, Frames.size(), Pixels.size(), NextGroupId, colour);

			Pixels.push_back(mp);

			if (y != size - 1)
			{
				switch (current_direction)
				{
				case 0:
					current_y -= pixel_width + 1;
					break;
				case 1:
					current_y += pixel_width + 1;
					break;
				}
			}
		}

		switch (current_direction)
		{
		case 0:
			current_direction = 1;
			break;
		case 1:
			current_direction = 0;
			break;
		}

		current_x += pixel_width + 1;
	}

	NextGroupId++;
}


void FreeformHandler::AddShapeRectangle(int sizex, int sizey, int pixel_width, int x, int y, int colour)
{
	int current_x = x;
	int current_y = y;
	int sidelength = sizex;

	for (int side = 0; side < 4; side++)
	{
		if (side % 2 == 0)
		{
			sidelength = sizex;
		}
		else
		{
			sidelength = sizey;
		}

		for (int t = 0; t < sidelength - 1; t++)
		{
			MatrixPixel *mp = new MatrixPixel(current_x, current_y, Frames.size(), Pixels.size(), NextGroupId, colour);

			Pixels.push_back(mp);

			switch (side)
			{
			case 0:
				current_x += pixel_width + 1;
				break;
			case 1:
				current_y += pixel_width + 1;
				break;
			case 2:
				current_x -= pixel_width + 1;
				break;
			case 3:
				current_y -= pixel_width + 1;
				break;
			}
		}
	}

    NextGroupId++;
}


void FreeformHandler::AddShapeRectangleFilled(int sizex, int sizey, int direction, int pixel_width, int x, int y, int colour)
{
	int current_direction = direction;
	int current_x = x;
	int current_y = y;

	for (int x = 0; x < sizex; x++)
	{
		for (int y = 0; y < sizey; y++)
		{
			MatrixPixel *mp = new MatrixPixel(current_x, current_y, Frames.size(), Pixels.size(), colour);

			Pixels.push_back(mp);

			if (y != sizey - 1)
			{
				switch (current_direction)
				{
				case 0:
					current_y -= pixel_width + 1;
					break;
				case 1:
					current_y += pixel_width + 1;
					break;
				}
			}
		}

		switch (current_direction)
		{
		case 0:
			current_direction = 1;
			break;
		case 1:
			current_direction = 0;
			break;
		}

		current_x += pixel_width + 1;
	}
}


void FreeformHandler::AddToHistory()
{
}


void FreeformHandler::AddToHistory(MatrixPixelHistory &m)
{
}


void FreeformHandler::AddToHistory(MatrixPixelHistory *m)
{
}


void FreeformHandler::Undo()
{
}


void FreeformHandler::Redo()
{
}


void FreeformHandler::SetFromUndo(int undo)
{
}


void FreeformHandler::Sort()
{
	std::sort(Pixels.begin(), Pixels.end(), sortByOrder);
}


void FreeformHandler::AddToSelection(int pixel)
{
	for (int t = 0; t < Selection.size(); t++)
	{
		if (Selection[t] == pixel) return;
	}

	Pixels[pixel]->OldX = Pixels[pixel]->X;
	Pixels[pixel]->OldY = Pixels[pixel]->Y;

    Selection.push_back(pixel);
}


void FreeformHandler::AddGroupToSelection(int group)
{
	Selection.clear();

	for (int p = 0; p < Pixels.size(); p++)
	{
		if (Pixels[p]->Group == group)
		{
			Selection.push_back(p);
		}
	}
}


void FreeformHandler::Move(int pixel, int x, int y)
{
	Pixels[pixel]->X = Pixels[pixel]->OldX + x;
	Pixels[pixel]->Y = Pixels[pixel]->OldY + y;
}


void FreeformHandler::InsertBlankFrameAt(int insertat, int colour)
{
	FreeformFrame *fff = new FreeformFrame();

	if (insertat >= Frames.size())
	{
		Frames.push_back(fff);

		for (int p = 0; p < Pixels.size(); p++)
		{
			Pixels[p]->Colours.push_back(colour);
		}
	}
	else
	{
		Frames.insert(Frames.begin() + insertat, fff);

		for (int p = 0; p < Pixels.size(); p++)
		{
			Pixels[p]->Colours.insert(Pixels[p]->Colours.begin() + insertat, colour);
		}
	}
}


void FreeformHandler::InsertCopyFrameAt(int source, int insertat)
{
	FreeformFrame *fff = new FreeformFrame();

	if (insertat >= Frames.size())
	{
		Frames.push_back(fff);

		for (int p = 0; p < Pixels.size(); p++)
		{
			Pixels[p]->Colours.push_back(Pixels[p]->Colours[source]);
		}
	}
	else
	{
		Frames.insert(Frames.begin() + insertat, fff);

		for (int p = 0; p < Pixels.size(); p++)
		{
			Pixels[p]->Colours.insert(Pixels[p]->Colours.begin() + insertat, Pixels[p]->Colours[source]);
		}
	}
}


void FreeformHandler::CopyFromPrevious(int frame_to)
{
//to do
}


void FreeformHandler::DeleteFrame(int frame)
{
	Frames.erase(Frames.begin() + frame);

    for (int p = 0; p < Pixels.size(); p++)
	{
		Pixels[p]->Colours.erase(Pixels[p]->Colours.begin() + frame);
	}
}


void FreeformHandler::SetOrder(int index, int new_order)
{
	int old = Pixels[index]->Order;

	for (int t = 0; t < Pixels.size(); t++)
	{
		if (Pixels[t]->Order >= old)
		{
			Pixels[t]->Order++;
		}
	}

    Pixels[index]->Order = new_order;
}


void FreeformHandler::SetOrderSwap(int index, int new_order)
{
	int old = Pixels[index]->Order;

	for (int t = 0; t < Pixels.size(); t++)
	{
		if (Pixels[t]->Order == new_order)
		{
			Pixels[t]->Order = old;
			break;
		}
	}

    Pixels[index]->Order = new_order;
}


void FreeformHandler::EnsurePixelCoherence()
{
	for (int p = 0; p < Pixels.size(); p++)
	{
		if (Pixels[p]->Colours.size() != Frames.size())
		{
			for (int z = Pixels[p]->Colours.size(); z <= Frames.size(); z++)
			{
				if (Pixels[p]->Colours.size() == 0)
				{
					Pixels[p]->Colours.push_back(0xffffff);
				}
				else
				{
					Pixels[p]->Colours.push_back(Pixels[p]->Colours.back());
				}
			}
		}
	}
}


void FreeformHandler::CalculateContrastColour(int frame)
{
	for (int pixel = 0; pixel < Pixels.size(); pixel++)
	{
		Pixels[pixel]->Contrast = ColourUtility::ContrastColour(Pixels[pixel]->Colours[frame]);
	}
}
