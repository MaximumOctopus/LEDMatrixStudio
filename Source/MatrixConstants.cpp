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
		}

		return BrushSize::kSmall;
	}
}
