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

#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "MatrixConstants.h"

extern LanguageHandler* GLanguageHandler;


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

	std::wstring MatrixDrawModeAsString(MatrixDrawMode mode)
	{
		switch (mode)
		{
		case MatrixDrawMode::kGrid:
			return L"Grid";
		case MatrixDrawMode::kFreeform:
			return L"Freeform";
		}

        return L"Unknown";
	}

	std::wstring MatrixModeAsString(MatrixColourMode mode)
	{
		switch (mode)
		{
		case MatrixColourMode::kMono:
			return GLanguageHandler->Text[kSingleColour];
		case MatrixColourMode::kBiSequential:
			return GLanguageHandler->Text[kBiColourSequential];
		case MatrixColourMode::kBiBitplanes:
			return GLanguageHandler->Text[kBiColourBitplanes];
		case MatrixColourMode::kRGB:
			return GLanguageHandler->Text[kRGB];
		case MatrixColourMode::kRGB3BPP:
			return GLanguageHandler->Text[kRGB3BPP];

		default:
			return L"unknown :(";
		}
	}


	int MatrixModeAsInt(MatrixColourMode mode)
	{
		switch (mode)
		{
		case MatrixColourMode::kMono:
			return 0;
		case MatrixColourMode::kBiSequential:
			return 1;
		case MatrixColourMode::kBiBitplanes:
			return 2;
		case MatrixColourMode::kRGB:
			return 3;
		case MatrixColourMode::kRGB3BPP:
			return 4;

		default:
			return 0;
		}
	}


	MatrixColourMode MatrixModeFromInt(int mm)
	{
		switch (mm)
		{
		case 0:
			return MatrixColourMode::kMono;
		case 1:
			return MatrixColourMode::kBiSequential;
		case 2:
			return MatrixColourMode::kBiBitplanes;
		case 3:
			return MatrixColourMode::kRGB;
		case 4:
			return MatrixColourMode::kRGB3BPP;

		default:
			return MatrixColourMode::kMono;
		}
	}

	std::wstring MatrixModeAsStringFromInt(int mm)
	{
		switch (mm)
		{
		case 0:
			return GLanguageHandler->Text[kSingleColour];
		case 1:
			return GLanguageHandler->Text[kBiColourSequential];
		case 2:
			return GLanguageHandler->Text[kBiColourBitplanes];
		case 3:
			return GLanguageHandler->Text[kRGB];
		case 4:
			return GLanguageHandler->Text[kRGB3BPP];

		default:
			return L"unknown :(";
		}
	}

	PixelShape PixelShapeFromInt(int ps)
	{
		switch (ps)
		{
		case 0:
			return PixelShape::kSquare;
		case 1:
			return PixelShape::kCircle;
		case 2:
			return PixelShape::kRoundRect;
		}

        return PixelShape::kSquare;
	}
}
