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


class MatrixPixel
{
public:

	int OldX = 0;
    int OldY = 0;
	int X = 0;                              // remain static for each frame of the project
	int Y = 0;                              //
	int Order = 0;                          //
	int Group = 0;                          //
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
        Group = group;

		for (int f = 0; f < frame_count; f++)
		{
			Colours.push_back(colour);
		}
	}
};
