//
// (c) Paul Alan Freshney 2012-2023
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://sourceforge.net/projects/led-matrix-studio/
//
// C++ Rewrite October 11th 2023
//
// Please do not redistribute the source code!
//

#pragma once


	enum class NumberFormat { kDecimal = 0, kBinary, kHex };
	enum class NumberSize { k8Bit = 0, k16bit, k32bit, k8bitSwap, k16bitSwap, k64bit, kRGB8bit, kRGB16bit, kRGB32bit };
	enum class ReadSource { kColumns = 0, kRows };
	enum class InputOrientation { kTopBottomLeftRight = 0, kBottomTopRightLeft, kSure24x16 };
	enum class RGBMode { kRGB = 0, kBGR, kGRB, kBRG, kRGBSimple };
	enum class LeastSignificantBit { kTopLeft = 0, kBottomRight };
	enum class ExportSource { kNone = 0, kAnimation, kUserMemories };
	enum class ExportLanguage { kCSV = 0, kPICAXE, kC1Dim, kC2Dim, kCFastLED, kPython1Dim, kPython2Dim, kMicrochip, kPascal, kSpecial };
	enum class LineContent { kRowCol = 0, kFrame, kBytes };
	enum class BinaryFileContents { kEntireAnimation = 0, kSingleFrame };
	enum class ColourSpace { kRGB32 = 0, kRGB565 };
	enum class ScanDirection { kRowLeftToRight = 0, kRowRightToLeft, kRowAltLeftRight, kRowAltRightLeft,
							   kColTopToBottom, kColBottomToTop, kColAltDownUp, kColAltUpDown };


struct ExportOptions
{
	bool Valid = false;

	bool IncludePreamble = true;
	bool CleanMode = false; // True = exclude everything from data output except the data!
	ExportSource ExportMode = ExportSource::kNone;
	int StartFrame = 1;
	int EndFrame = 1;
	int SelectiveStart;
	int SelectiveEnd;
	ReadSource Source = ReadSource::kRows;
	InputOrientation Orientation = InputOrientation::kTopBottomLeftRight;
	ScanDirection Direction = ScanDirection::kRowLeftToRight;
	LeastSignificantBit LSB = LeastSignificantBit::kBottomRight;
	ExportLanguage Language = ExportLanguage::kCSV;
	NumberFormat Format = NumberFormat::kHex;
	NumberSize Size = NumberSize::k8Bit;
	LineContent Content = LineContent::kRowCol;
	int LineCount = 10;
	bool FontMode = false;
	bool Optimise = false;

	int MinWidth = -1;
	int MaxWidth = -1;
	int MinHeight = -1;
	int MaxHeight = -1;

	bool RGBEnabled = false;
	RGBMode TextRGBMode = RGBMode::kRGB;
	bool RGBChangePixels = false;
	int RGBChangeColour = 0x00000000;
	int RGBBrightness = 100;

	std::wstring Description = L"";
	std::wstring Information = L"";
	std::wstring DataPadding = L"";

	ReadSource BinarySource = ReadSource::kRows;
	InputOrientation BinaryOrientation = InputOrientation::kTopBottomLeftRight;
	ScanDirection BinaryDirection = ScanDirection::kRowLeftToRight;
	LeastSignificantBit BinaryLSB = LeastSignificantBit::kTopLeft;

	RGBMode BinaryRGBMode;
	bool BinaryRGBChangePixels;
	int BinaryRGBChangeColour;
	int BinaryRGBBrightness;

	NumberSize BinarySize = NumberSize::k8Bit;
	BinaryFileContents BinaryContent;

	ColourSpace ColourSpaceRGB;
	ColourSpace BinaryColourSpaceRGB;

	bool Examples; // include code example to output (BETA!)

	//

	void Clear(bool IsRGB)
	{
		IncludePreamble = true;
		CleanMode = false;		             // True = exclude everything from data output except the data!
		ExportMode = ExportSource::kNone;
		StartFrame = 1;
		EndFrame = 1;
		Source = ReadSource::kRows;
		Orientation = InputOrientation::kTopBottomLeftRight;
		Direction = ScanDirection::kRowLeftToRight;
		LSB = LeastSignificantBit::kBottomRight;
		Language = ExportLanguage::kCSV;
		Format = NumberFormat::kHex;
		Size = NumberSize::k8Bit;
		Content = LineContent::kRowCol;
		LineCount = 10;

		FontMode = false;
		Optimise = false;

		MinWidth = -1;
		MaxWidth = -1;
		MinHeight = -1;
		MaxHeight = -1;

		if (IsRGB)
		{
			RGBEnabled = true;
			TextRGBMode = RGBMode::kRGB;
			RGBChangePixels = false;
			RGBChangeColour = 0x00000000;
		}
		else
		{
			RGBEnabled = false;
			TextRGBMode = RGBMode::kRGB;
			RGBChangePixels = false;
			RGBChangeColour = 0x00000000;
		}

		RGBBrightness = 100;

		Description = L"";
		DataPadding = L"";

		// == binary output settings =================================================

		BinarySource = ReadSource::kRows;
		BinaryOrientation = InputOrientation::kTopBottomLeftRight;
		BinaryDirection = ScanDirection::kRowLeftToRight;
		BinaryLSB = LeastSignificantBit::kBottomRight;

		if (IsRGB)
		{
			BinaryRGBMode = RGBMode::kRGB;
			BinaryRGBChangePixels = false;
			BinaryRGBChangeColour = 0x00000000;
		}
		else
		{
			BinaryRGBMode = RGBMode::kRGB;
			BinaryRGBChangePixels = false;
			BinaryRGBChangeColour = 0x00000000;
		}

		BinaryRGBBrightness = 100;

		BinarySize = NumberSize::k8Bit;

		BinaryContent =	BinaryFileContents::kEntireAnimation;
	}

	// returns size in bits 0-n
	int GetNumberSizeLength(NumberSize ns)
	{
		switch (ns)
		{
		case NumberSize::k8Bit: return 7;
		case NumberSize::k16bit: return 15;
		case NumberSize::k32bit: return 31;
		case NumberSize::k8bitSwap: return 7;
		case NumberSize::k16bitSwap: return 15;
		case NumberSize::k64bit: return 63;
		case NumberSize::kRGB8bit: return 7;
		case NumberSize::kRGB16bit: return 15;
		case NumberSize::kRGB32bit: return 31;
		}
	}

	int GetNumberSizeLengthBytes(NumberSize ns)
	{
		switch (ns)
		{
		case NumberSize::k8Bit: return 1;
		case NumberSize::k16bit: return 2;
		case NumberSize::k32bit: return 4;
		case NumberSize::k8bitSwap: return 1;
		case NumberSize::k16bitSwap: return 2;
		case NumberSize::k64bit: return 8;
		case NumberSize::kRGB8bit: return 1;
		case NumberSize::kRGB16bit: return 2;
		case NumberSize::kRGB32bit: return 4;
		}
	}

	// returns pad size
	int GetNumberSizePadLength(NumberSize ns)
	{
		switch (ns)
		{
		case NumberSize::k8Bit: return 2;
		case NumberSize::k16bit: return 4;
		case NumberSize::k32bit: return 8;
		case NumberSize::k8bitSwap: return 2;
		case NumberSize::k16bitSwap: return 4;
		case NumberSize::k64bit: return 16;
		case NumberSize::kRGB8bit: return 2;
		case NumberSize::kRGB16bit: return 4;
		case NumberSize::kRGB32bit: return 8;
		}
	}

	int SourceToInt()
	{
		switch (Source)
		{
		case ReadSource::kColumns:
			return 0;
		case ReadSource::kRows:
			return 1;
		}

		return 0;
	}

	int OrientationToInt()
	{
		switch (Orientation)
		{
		case InputOrientation::kTopBottomLeftRight:
			return 0;
		case InputOrientation::kBottomTopRightLeft:
			return 1;
		case InputOrientation::kSure24x16:
			return 2;
		}

		return 0;
	}

	int ScanDirectionToInt()
	{
		if (Source == ReadSource::kRows)
		{
			switch (Direction)
			{
			case ScanDirection::kRowLeftToRight:
				return 0;
			case ScanDirection::kRowRightToLeft:
				return 1;
			case ScanDirection::kRowAltLeftRight:
				return 2;
			case ScanDirection::kRowAltRightLeft:
				return 3;
			}
		}
		else
		{
			switch (Direction)
			{
			case ScanDirection::kColTopToBottom:
				return 0;
			case ScanDirection::kColBottomToTop:
				return 1;
			case ScanDirection::kColAltDownUp:
				return 2;
			case ScanDirection::kColAltUpDown:
				return 3;
			}
		}

		return 0;
	}

	int LSBToInt()
	{
		switch (LSB)
		{
		case LeastSignificantBit::kTopLeft:
			return 0;
		case LeastSignificantBit::kBottomRight:
			return 1;
		}

        return 0;
	}

	int LanguageToInt()
	{
		switch (Language)
		{
		case ExportLanguage::kCSV:
			return 0;
		case ExportLanguage::kPICAXE:
			return 1;
		case ExportLanguage::kC1Dim:
			return 2;
		case ExportLanguage::kC2Dim:
			return 3;
		case ExportLanguage::kCFastLED:
			return 4;
		case ExportLanguage::kPython1Dim:
			return 5;
		case ExportLanguage::kPython2Dim:
			return 6;
		case ExportLanguage::kMicrochip:
			return 7;
			break;
		case ExportLanguage::kPascal:
			return 8;
			break;
		case ExportLanguage::kSpecial:
			return 9;
		}

        return 0;
	}

	int NumberFormatToInt()
	{
		switch (Format)
		{
		case NumberFormat::kDecimal:
			return 0;
		case NumberFormat::kBinary:
			return 1;
		case NumberFormat::kHex:
			return 2;
		}
	}

	int NumberSizeToInt()
	{
		switch (Size)
		{
		case NumberSize::k8Bit:
			return 0;
		case NumberSize::k16bit:
			return 1;
		case NumberSize::k32bit:
			return 2;
		case NumberSize::k8bitSwap:
			return 3;
		case NumberSize::k16bitSwap:
			return 4;
		case NumberSize::k64bit:
			return 5;
		case NumberSize::kRGB8bit:
			return 6;
		case NumberSize::kRGB16bit:
			return 7;
		case NumberSize::kRGB32bit:
			return 8;
		}

		return 0;
	}

	int ContentToInt()
	{
		switch (Content)
		{
		case LineContent::kRowCol:
			return 0;
		case LineContent::kFrame:
			return 1;
		case LineContent::kBytes:
			return 2;
		}

		return 0;
	}

	int TextRGBModeToInt()
	{
		 switch (TextRGBMode)
		 {
		 case RGBMode::kRGB:
			return 0;
		 case RGBMode::kBGR:
			return 1;
		 case RGBMode::kGRB:
			return 2;
		 case RGBMode::kBRG:
			return 3;
		 case RGBMode::kRGBSimple:
			return 4;
		 }

		 return 0;
	}

	int BinarySourceToInt()
	{
		switch (BinarySource)
		{
		case ReadSource::kColumns:
			return 0;
		case ReadSource::kRows:
			return 1;
		}

		return 0;
	}

	int BinaryOrientationToInt()
	{
		switch (BinaryOrientation)
		{
		case InputOrientation::kTopBottomLeftRight:
			return 0;
		case InputOrientation::kBottomTopRightLeft:
			return 1;
		case InputOrientation::kSure24x16:
			return 2;
		}

		return 0;
	}

	int BinaryScanDirectionToInt()
	{
		if (BinarySource == ReadSource::kRows)
		{
			switch (BinaryDirection)
			{
			case ScanDirection::kRowLeftToRight:
				return 0;
			case ScanDirection::kRowRightToLeft:
				return 1;
			case ScanDirection::kRowAltLeftRight:
				return 2;
			case ScanDirection::kRowAltRightLeft:
				return 3;
			}
		}
		else
		{
			switch (BinaryDirection)
			{
			case ScanDirection::kColTopToBottom:
				return 0;
			case ScanDirection::kColBottomToTop:
				return 1;
			case ScanDirection::kColAltDownUp:
				return 2;
			case ScanDirection::kColAltUpDown:
				return 3;
			}
		}

		return 0;
	}

	int BinaryLSBToInt()
	{
		switch (BinaryLSB)
		{
		case LeastSignificantBit::kTopLeft:
			return 0;
		case LeastSignificantBit::kBottomRight:
			return 1;
		}

		return 0;
	}

	int BinaryRGBModeToInt()
	{
		 switch (BinaryRGBMode)
		 {
		 case RGBMode::kRGB:
			return 0;
		 case RGBMode::kBGR:
			return 1;
		 case RGBMode::kGRB:
			return 2;
		 case RGBMode::kBRG:
			return 3;
		 case RGBMode::kRGBSimple:
			return 4;
		 }

		 return 0;
	}

	int BinaryFileContentsToInt()
	{
		switch (BinaryContent)
		{
		case BinaryFileContents::kEntireAnimation:
			return 0;
		case BinaryFileContents::kSingleFrame:
			return 1;
		}

		return 0;
	}

	//

	void SourceFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Source = ReadSource::kColumns;
			break;
		case 1:
			Source = ReadSource::kRows;
			break;
		}
	}

	void OrientationFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Orientation = InputOrientation::kTopBottomLeftRight;
			break;
		case 1:
			Orientation = InputOrientation::kBottomTopRightLeft;
			break;
		case 2:
			Orientation = InputOrientation::kSure24x16;
			break;
		}
	}

	void ScanDirectionFromInt(ReadSource source, int i)
	{
		if (source == ReadSource::kRows)
		{
			switch (i)
			{
			case 0:
				Direction = ScanDirection::kRowLeftToRight;
				break;
			case 1:
				Direction = ScanDirection::kRowRightToLeft;
				break;
			case 2:
				Direction = ScanDirection::kRowAltLeftRight;
				break;
			case 3:
				Direction = ScanDirection::kRowAltRightLeft;
				break;
			}
		}
		else
		{
			switch (i)
			{
			case 0:
				Direction = ScanDirection::kColTopToBottom;
				break;
			case 1:
				Direction = ScanDirection::kColBottomToTop;
				break;
			case 2:
				Direction = ScanDirection::kColAltDownUp;
				break;
			case 3:
				Direction = ScanDirection::kColAltUpDown;
				break;
			}
		}
	}

	void LSBFromInt(int i)
	{
		switch (i)
		{
		case 0:
			LSB = LeastSignificantBit::kTopLeft;
			break;
		case 1:
			LSB = LeastSignificantBit::kBottomRight;
			break;
		}
	}

	void LanguageFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Language = ExportLanguage::kCSV;
			break;
		case 1:
			Language = ExportLanguage::kPICAXE;
			break;
		case 2:
			Language = ExportLanguage::kC1Dim;
			break;
		case 3:
			Language = ExportLanguage::kC2Dim;
			break;
		case 4:
			Language = ExportLanguage::kCFastLED;
			break;
		case 5:
			Language = ExportLanguage::kPython1Dim;
			break;
		case 6:
			Language = ExportLanguage::kPython2Dim;
			break;
		case 7:
			Language = ExportLanguage::kMicrochip;
			break;
		case 8:
			Language = ExportLanguage::kPascal;
			break;
		case 9:
			Language = ExportLanguage::kSpecial;
			break;
		}
	}

	void NumberFormatFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Format = NumberFormat::kDecimal;
			break;
		case 1:
			Format = NumberFormat::kBinary;
			break;
		case 2:
			Format = NumberFormat::kHex;
			break;
		}
	}

	void NumberSizeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Size = NumberSize::k8Bit;
			break;
		case 1:
			Size = NumberSize::k16bit;
			break;
		case 2:
			Size = NumberSize::k32bit;
			break;
		case 3:
			Size = NumberSize::k8bitSwap;
			break;
		case 4:
			Size = NumberSize::k16bitSwap;
			break;
		case 5:
			Size = NumberSize::k64bit;
			break;
		case 6:
			Size = NumberSize::kRGB8bit;
			break;
		case 7:
			Size = NumberSize::kRGB16bit;
			break;
		case 8:
			Size = NumberSize::kRGB32bit;
			break;
		}
	}

	void LineContentFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Content = LineContent::kRowCol;
			break;
		case 1:
			Content = LineContent::kFrame;
			break;
		case 2:
			Content = LineContent::kBytes;
			break;
		}
	}

	void RGBModeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			TextRGBMode = RGBMode::kRGB;
			break;
		case 1:
			TextRGBMode = RGBMode::kBGR;
			break;
		case 2:
			TextRGBMode = RGBMode::kGRB;
			break;
		case 3:
			TextRGBMode = RGBMode::kBRG;
			break;
		case 4:
			TextRGBMode = RGBMode::kRGBSimple;
			break;
		}
	}

	void BinarySourceFromInt(int i)
	{
		switch (i)
		{
		case 0:
			BinarySource = ReadSource::kColumns;
			break;
		case 1:
			BinarySource = ReadSource::kRows;
			break;
		}
	}

	void BinaryOrientationFromInt(int i)
	{
		switch (i)
		{
		case 0:
			BinaryOrientation = InputOrientation::kTopBottomLeftRight;
			break;
		case 1:
			BinaryOrientation = InputOrientation::kBottomTopRightLeft;
			break;
		case 2:
			BinaryOrientation = InputOrientation::kSure24x16;
			break;
		}
	}

	void BinaryScanDirectionFromInt(ReadSource source, int i)
	{
		if (source == ReadSource::kRows)
		{
			switch (i)
			{
			case 0:
				BinaryDirection = ScanDirection::kRowLeftToRight;
				break;
			case 1:
				BinaryDirection = ScanDirection::kRowRightToLeft;
				break;
			case 2:
				BinaryDirection = ScanDirection::kRowAltLeftRight;
				break;
			case 3:
				BinaryDirection = ScanDirection::kRowAltRightLeft;
				break;
			}
		}
		else
		{
			switch (i)
			{
			case 0:
				BinaryDirection = ScanDirection::kColTopToBottom;
				break;
			case 1:
				BinaryDirection = ScanDirection::kColBottomToTop;
				break;
			case 2:
				BinaryDirection = ScanDirection::kColAltDownUp;
				break;
			case 3:
				BinaryDirection = ScanDirection::kColAltUpDown;
				break;
			}
		}
	}

	void BinaryLSBFromInt(int i)
	{
		switch (i)
		{
		case 0:
			BinaryLSB = LeastSignificantBit::kTopLeft;
			break;
		case 1:
			BinaryLSB = LeastSignificantBit::kBottomRight;
			break;
		}
	}

	void BinaryRGBModeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			BinaryRGBMode = RGBMode::kRGB;
			break;
		case 1:
			BinaryRGBMode = RGBMode::kBGR;
			break;
		case 2:
			BinaryRGBMode = RGBMode::kGRB;
			break;
		case 3:
			BinaryRGBMode = RGBMode::kBRG;
			break;
		case 4:
			BinaryRGBMode = RGBMode::kRGBSimple;
			break;
		}
	}

	void BinaryFileContentsFromInt(int i)
	{
		switch (i)
		{
		case 0:
			BinaryContent = BinaryFileContents::kEntireAnimation;
			break;
		case 1:
			BinaryContent = BinaryFileContents::kSingleFrame;
			break;
		}
	}
};
