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

#include "colours.h"
#include "matrixconstants.h"


struct ImportDataPreview
{
	bool Enabled = false;

	int Size = 1;
	ViewShape View = ViewShape::kSquare;
	int Void = 1;

	bool IncrementRadially = false;

	bool Popout = false;

	int Offset = 0;
	bool OffsetDirection = false;

	int ViewToInt()
	{
		switch (View)
		{
		case ViewShape::kSquare:
			return 0;
		case ViewShape::kRadial:
			return 1;
		case ViewShape::kRadial3Q:
			return 2;
		case ViewShape::kSemiCircle:
			return 3;
		case ViewShape::kSemiCircleInverted:
			return 4;			
		}
		
		return 0;
	}

	void ViewShapeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			View = ViewShape::kSquare;
			break;
		case 1:
			View = ViewShape::kRadial;
			break;
		case 2:
			View = ViewShape::kRadial3Q;
			break;
		case 3:
			View = ViewShape::kSemiCircle;
			break;
		case 4:
			View = ViewShape::kSemiCircleInverted;
			break;
		}
	}
};


struct ImportData
{
	bool ImportOk = false;
	std::wstring ErrorString = L"";

	int Source = 0;
	int SourceLSB = 0;
	int SourceDirection = 0;
	PadFormat PadMode = PadFormat::kAuto;
	HexFormat FormatHex = HexFormat::kDisabled;
	HexPrefix OutputHex = HexPrefix::kNone;
	BracketStyle Brackets = BracketStyle::kNone;
	MatrixMode Mode = MatrixMode::kNone;
	int NewWidth = 8;
	int NewHeight = 8;
	int NewFrames = 1;
	int MaxFrames = 1;
	int ASCIIIndex = 32;
	bool FontMode = false;

	int BackgroundColour = 0;

	int RGBBrightness = 100;

	int StartFrame = 0;
	int EndFrame = 0;

	ImportDataPreview Preview;

	std::wstring AutomationFileName = L"";

	ProjectColours Colours;


	void BracketsFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Brackets = BracketStyle::kNone;
			break;
		case 1:
			Brackets = BracketStyle::kNormal;
			break;
		case 2:
			Brackets = BracketStyle::kCurly;
			break;
		case 3:
			Brackets = BracketStyle::kSquare;
			break;
		}
	}

	void HexFormatFromInt(int i)
	{
		switch (i)
		{
		case 0:
			FormatHex = HexFormat::kEnabled;
			break;
		case 1:
			FormatHex = HexFormat::kDisabled;
			break;
		}
	}

	void HexOutputFromInt(int i)
	{
		switch (i)
		{
		case 0:
			OutputHex = HexPrefix::kNone;
			break;
		case 1:
			OutputHex = HexPrefix::kDollar;
			break;
		case 2:
			OutputHex = HexPrefix::kZeroX;
			break;
		case 3:
			OutputHex = HexPrefix::kAmpersand;
			break;
		}
	}

	void PadModeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			PadMode = PadFormat::kAuto;
			break;
		case 1:
			PadMode = PadFormat::k8Bits;
			break;
		case 2:
			PadMode = PadFormat::k16Bits;
			break;
		case 3:
			PadMode = PadFormat::k24Bits;
			break;
		case 4:
			PadMode = PadFormat::k32Bits;
			break;
		case 5:
			PadMode = PadFormat::k40Bits;
			break;
		case 6:
			PadMode = PadFormat::k48Bits;
			break;
		case 7:
			PadMode = PadFormat::k56Bits;
			break;
		case 8:
			PadMode = PadFormat::k64Bits;
			break;
		}
    }
	
	int BracketsToInt()
	{
		switch (Brackets)
		{
		case BracketStyle::kNone:
			return 0;
		case BracketStyle::kNormal:
			return 1;
		case BracketStyle::kCurly:
			return 2;
		case BracketStyle::kSquare:
			return 3;
		}
		
		return 0;
	}

	int HexFormatToInt()
	{
		switch (FormatHex)
		{
		case HexFormat::kEnabled:
			return 0;
		case HexFormat::kDisabled:
			return 1;
		}
		
		return 0;
	}

	int HexOutputToInt()
	{
		switch (OutputHex)
		{
		case HexPrefix::kNone:
			return 0;
		case HexPrefix::kDollar:
			return 1;
		case HexPrefix::kZeroX:
			return 2;
		case HexPrefix::kAmpersand:
			return 3;
		}
		
		return 0;
	}

	int PadModeToInt()
	{
		switch (PadMode)
		{
		case PadFormat::kAuto:
			return 0;
		case PadFormat::k8Bits:
			return 1;
		case PadFormat::k16Bits:
			return 2;
		case PadFormat::k24Bits:
			return 3;
		case PadFormat::k32Bits:
			return 4;
		case PadFormat::k40Bits:
			return 5;
		case PadFormat::k48Bits:
			return 6;
		case PadFormat::k56Bits:
			return 7;
		case PadFormat::k64Bits:
			return 8;
		}
		
		return 0;
    }	
};
