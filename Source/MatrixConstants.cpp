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

#include "MatrixConstants.h"


namespace ConstantsHelper
{
	BrushSize BrushFromInt(int i)
	{
		switch (i)
		{
		case 0:
			return BrushSize::kSmall;
		case 1:
			return BrushSize::kMedium;
		case 2:
			return BrushSize::kLarge;
		case 3:
			return BrushSize::kBigLarge;
		case 4:
			return BrushSize::kSuperLarge;
		}

		return BrushSize::kSmall;
	}

	int PixelsFromBrushSize(BrushSize bs)
	{
		switch (bs)
		{
		case BrushSize::kSmall:
			return BrushSizePixels[0];
		case BrushSize::kMedium:
			return BrushSizePixels[1];
		case BrushSize::kLarge:
			return BrushSizePixels[2];
		case BrushSize::kBigLarge:
			return BrushSizePixels[3];
		case BrushSize::kSuperLarge:
			return BrushSizePixels[4];
		}

        return 1;
	}
}
