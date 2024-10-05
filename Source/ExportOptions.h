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

#include <fstream>
#include <sstream>

#include "FileConstants.h"
#include "Formatting.h"

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


struct BinaryOptions
{
	int StartFrame = 0;
	int EndFrame = 0;
	int SelectiveStart = 0;
	int SelectiveEnd = 0;

	ReadSource Source = ReadSource::kRows;
	ScanDirection Direction = ScanDirection::kRowLeftToRight;

	InputOrientation Orientation = InputOrientation::kTopBottomLeftRight;

	NumberSize Size = NumberSize::k8Bit;
	NumberFormat Format = NumberFormat::kHex;

	LeastSignificantBit LSB = LeastSignificantBit::kBottomRight;

	ColourSpace ColourSpaceRGB = ColourSpace::kRGB32;

	BinaryFileContents Content = BinaryFileContents::kEntireAnimation;

	bool RGBEnabled = false;
	RGBMode RGBFormat = RGBMode::kRGB;
	bool RGBChangePixels = false;
	int RGBChangeColour = 0x00000000;
	int RGBBrightness = 100;

	void clear(bool isRGB)
	{
		StartFrame = 0;
		EndFrame = 0;

		Source = ReadSource::kRows;
		Orientation = InputOrientation::kTopBottomLeftRight;
		Direction = ScanDirection::kRowLeftToRight;
		LSB = LeastSignificantBit::kBottomRight;
		Format = NumberFormat::kHex;
		Size = NumberSize::k8Bit;

		Content = BinaryFileContents::kEntireAnimation;

		if (isRGB)
		{
			RGBEnabled = true;
			RGBFormat = RGBMode::kRGB;
			RGBChangePixels = false;
			RGBChangeColour = 0x00000000;
		}
		else
		{
			RGBEnabled = false;
			RGBFormat = RGBMode::kRGB;
			RGBChangePixels = false;
			RGBChangeColour = 0x00000000;
		}


		RGBBrightness = 100;
	}
};


struct CodeOptions
{
	int StartFrame = 0;
	int EndFrame = 0;
	int SelectiveStart = 0;
	int SelectiveEnd = 0;

	ReadSource Source = ReadSource::kRows;
	ScanDirection Direction = ScanDirection::kRowLeftToRight;

	NumberSize Size = NumberSize::k8Bit;
	NumberFormat Format = NumberFormat::kHex;

	LeastSignificantBit LSB = LeastSignificantBit::kBottomRight;

	InputOrientation Orientation = InputOrientation::kTopBottomLeftRight;

	ExportLanguage Language = ExportLanguage::kCSV;

	ColourSpace ColourSpaceRGB = ColourSpace::kRGB32;

	LineContent Content = LineContent::kRowCol;

	int LineCount = 10;

	bool IncludePreamble = true;
	bool CleanMode = false; 		// true = exclude everything from data output except the data!

	bool RGBEnabled = false;
	RGBMode RGBFormat = RGBMode::kRGB;
	bool RGBChangePixels = false;
	int RGBChangeColour = 0x00000000;
	int RGBBrightness = 100;

	void clear(bool isRGB)
	{
		IncludePreamble = true;
		CleanMode = false;

		StartFrame = 0;
		EndFrame = 0;

		Source = ReadSource::kRows;
		Orientation = InputOrientation::kTopBottomLeftRight;
		Direction = ScanDirection::kRowLeftToRight;
		LSB = LeastSignificantBit::kBottomRight;
		Language = ExportLanguage::kCSV;
		Format = NumberFormat::kHex;
		Size = NumberSize::k8Bit;

		Content = LineContent::kRowCol;
		LineCount = 10;

		if (isRGB)
		{
			RGBEnabled = true;
			RGBFormat = RGBMode::kRGB;
			RGBChangePixels = false;
			RGBChangeColour = 0x00000000;
		}
		else
		{
			RGBEnabled = false;
			RGBFormat = RGBMode::kRGB;
			RGBChangePixels = false;
			RGBChangeColour = 0x00000000;
		}

		RGBBrightness = 100;
	}
};


struct ExportOptions
{
	bool Valid = false;

	BinaryOptions Binary;
	CodeOptions Code;

	ExportSource ExportMode = ExportSource::kNone;

	bool FontMode = false;
	bool Optimise = false;

	int MinWidth = -1;
	int MaxWidth = -1;
	int MinHeight = -1;
	int MaxHeight = -1;

	std::wstring Description = L"";
	std::wstring Information = L"";
	std::wstring DataPadding = L"";

	bool Examples = false; // include code example to output (BETA!)

	void clear(bool IsRGB)
	{
		Binary.clear(IsRGB);
		Code.clear(IsRGB);

		ExportMode = ExportSource::kNone;

		FontMode = false;
		Optimise = false;

		MinWidth = -1;
		MaxWidth = -1;
		MinHeight = -1;
		MaxHeight = -1;

		Description = L"";
		DataPadding = L"";
		Information = L"";
	}

	void SaveToFile(std::ofstream& ofile)
	{
		if (ExportMode != ExportSource::kNone)
		{
			ofile << Formatting::to_utf8(kAnimSourceF +           std::to_wstring(SourceToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimOrientationF +      std::to_wstring(OrientationToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimScanDirectionF +    std::to_wstring(ScanDirectionToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimLSBF +              std::to_wstring(LSBToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimLanguageF +         std::to_wstring(LanguageToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimNumberFormatF +     std::to_wstring(NumberFormatToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimNumberSizeF +       std::to_wstring(NumberSizeToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimLineContentF +      std::to_wstring(ContentToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimLineCountF +        std::to_wstring(Code.LineCount) + L"\n");

			ofile << Formatting::to_utf8(kAnimRGBModeF +          std::to_wstring(RGBFormatToInt()) + L"\n");
			ofile << Formatting::to_utf8(kAnimRGBChangePixelsF +  std::to_wstring(Code.RGBChangePixels) + L"\n");
			ofile << Formatting::to_utf8(kAnimRGBChangeColourF +  std::to_wstring(Code.RGBChangeColour) + L"\n");
			ofile << Formatting::to_utf8(kAnimRGBBrightnessF +    std::to_wstring(Code.RGBBrightness) + L"\n");

			ofile << Formatting::to_utf8(kAnimOptimiseF +         std::to_wstring(Optimise) + L"\n");

			std::wstring binary = std::to_wstring(BinarySourceToInt()) + L" " + std::to_wstring(BinaryOrientationToInt()) + L" " + std::to_wstring(BinaryScanDirectionToInt()) + L" ";
			binary += std::to_wstring(BinaryLSBToInt()) + L" " + std::to_wstring(BinaryFileContentsToInt()) + L" ";
			binary += std::to_wstring(BinaryRGBFormatToInt()) + L" " + std::to_wstring(Binary.RGBChangePixels) + L" " + std::to_wstring(Binary.RGBChangeColour) + L" " + std::to_wstring(Binary.RGBBrightness) + L" ";
			binary += std::to_wstring(BinaryNumberSizeToInt());

			ofile << Formatting::to_utf8(kAnimBinaryF + binary + L"\n");
		}
		else
		{
			ofile << Formatting::to_utf8(kAnimExportOptionsNotSet + L"\n");
		}
	}

	void SetBinaryFromFile(const std::wstring input)
	{
		if (input.empty()) return;

		std::vector<int> data;

		std::wstringstream ss(input);
		std::wstring token = L"";

		while (ss >> token)
		{
			data.push_back(stoi(token));
		}

		if (data.size() == 10)
		{
			BinarySourceFromInt(data[0]);
			BinaryOrientationFromInt(data[1]);
			BinaryScanDirectionFromInt(Binary.Source, data[2]);
			BinaryLSBFromInt(data[3]);
			BinaryFileContentsFromInt(data[4]);
			BinaryRGBFormatFromInt(data[5]);
			Binary.RGBChangePixels = data[6];
			Binary.RGBChangeColour = data[7];
			Binary.RGBBrightness = data[8];
			BinaryNumberSizeFromInt(data[9]);
		}
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
		switch (Code.Source)
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
		switch (Code.Orientation)
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
		switch (Code.Direction)
		{
		case ScanDirection::kRowLeftToRight:
			return 0;
		case ScanDirection::kRowRightToLeft:
			return 1;
		case ScanDirection::kRowAltLeftRight:
			return 2;
		case ScanDirection::kRowAltRightLeft:
			return 3;
		case ScanDirection::kColTopToBottom:
			return 0;
		case ScanDirection::kColBottomToTop:
			return 1;
		case ScanDirection::kColAltDownUp:
			return 2;
		case ScanDirection::kColAltUpDown:
			return 3;
		}

		return 0;
	}

	int LSBToInt()
	{
		switch (Code.LSB)
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
		switch (Code.Language)
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
		case ExportLanguage::kPascal:
			return 8;
		case ExportLanguage::kSpecial:
			return 9;
		}

        return 0;
	}

	int NumberFormatToInt()
	{
		switch (Code.Format)
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
		switch (Code.Size)
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
		switch (Code.Content)
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

	int RGBFormatToInt()
	{
		 switch (Code.RGBFormat)
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
		switch (Binary.Source)
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
		switch (Binary.Orientation)
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
		switch (Binary.Direction)
		{
		case ScanDirection::kRowLeftToRight:
			return 0;
		case ScanDirection::kRowRightToLeft:
			return 1;
		case ScanDirection::kRowAltLeftRight:
			return 2;
		case ScanDirection::kRowAltRightLeft:
			return 3;
		case ScanDirection::kColTopToBottom:
			return 0;
		case ScanDirection::kColBottomToTop:
			return 1;
		case ScanDirection::kColAltDownUp:
			return 2;
		case ScanDirection::kColAltUpDown:
			return 3;
		}

		return 0;
	}

	int BinaryLSBToInt()
	{
		switch (Binary.LSB)
		{
		case LeastSignificantBit::kTopLeft:
			return 0;
		case LeastSignificantBit::kBottomRight:
			return 1;
		}

		return 0;
	}

	int BinaryNumberSizeToInt()
	{
		switch (Binary.Size)
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


	int BinaryRGBFormatToInt()
	{
		 switch (Binary.RGBFormat)
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
		switch (Binary.Content)
		{
		case BinaryFileContents::kEntireAnimation:
			return 0;
		case BinaryFileContents::kSingleFrame:
			return 1;
		}

		return 0;
	}

	//


	int NybblesFromNumberSize()
	{
		switch (Code.Size)
		{
		case NumberSize::k8Bit:
			return 2;
		case NumberSize::k16bit:
			return 4;
		case NumberSize::k32bit:
			return 8;
		case NumberSize::k8bitSwap:
			return 2;
		case NumberSize::k16bitSwap:
			return 4;
		case NumberSize::k64bit:
			return 16;
		case NumberSize::kRGB8bit:
			return 2;
		case NumberSize::kRGB16bit:
			return 4;
		case NumberSize::kRGB32bit:
			return 8;
		}

		return 1;
	}

	void SourceFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.Source = ReadSource::kColumns;
			break;
		case 1:
			Code.Source = ReadSource::kRows;
			break;
		}
	}

	void OrientationFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.Orientation = InputOrientation::kTopBottomLeftRight;
			break;
		case 1:
			Code.Orientation = InputOrientation::kBottomTopRightLeft;
			break;
		case 2:
			Code.Orientation = InputOrientation::kSure24x16;
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
				Code.Direction = ScanDirection::kRowLeftToRight;
				break;
			case 1:
				Code.Direction = ScanDirection::kRowRightToLeft;
				break;
			case 2:
				Code.Direction = ScanDirection::kRowAltLeftRight;
				break;
			case 3:
				Code.Direction = ScanDirection::kRowAltRightLeft;
				break;
			}
		}
		else
		{
			switch (i)
			{
			case 0:
				Code.Direction = ScanDirection::kColTopToBottom;
				break;
			case 1:
				Code.Direction = ScanDirection::kColBottomToTop;
				break;
			case 2:
				Code.Direction = ScanDirection::kColAltDownUp;
				break;
			case 3:
				Code.Direction = ScanDirection::kColAltUpDown;
				break;
			}
		}
	}

	void LSBFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.LSB = LeastSignificantBit::kTopLeft;
			break;
		case 1:
			Code.LSB = LeastSignificantBit::kBottomRight;
			break;
		}
	}

	void LanguageFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.Language = ExportLanguage::kCSV;
			break;
		case 1:
			Code.Language = ExportLanguage::kPICAXE;
			break;
		case 2:
			Code.Language = ExportLanguage::kC1Dim;
			break;
		case 3:
			Code.Language = ExportLanguage::kC2Dim;
			break;
		case 4:
			Code.Language = ExportLanguage::kCFastLED;
			break;
		case 5:
			Code.Language = ExportLanguage::kPython1Dim;
			break;
		case 6:
			Code.Language = ExportLanguage::kPython2Dim;
			break;
		case 7:
			Code.Language = ExportLanguage::kMicrochip;
			break;
		case 8:
			Code.Language = ExportLanguage::kPascal;
			break;
		case 9:
			Code.Language = ExportLanguage::kSpecial;
			break;

		default:
			Code.Language = ExportLanguage::kC1Dim;
		}
	}

	void NumberFormatFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.Format = NumberFormat::kDecimal;
			break;
		case 1:
			Code.Format = NumberFormat::kBinary;
			break;
		case 2:
			Code.Format = NumberFormat::kHex;
			break;
		}
	}

	void NumberSizeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.Size = NumberSize::k8Bit;
			break;
		case 1:
			Code.Size = NumberSize::k16bit;
			break;
		case 2:
			Code.Size = NumberSize::k32bit;
			break;
		case 3:
			Code.Size = NumberSize::k8bitSwap;
			break;
		case 4:
			Code.Size = NumberSize::k16bitSwap;
			break;
		case 5:
			Code.Size = NumberSize::k64bit;
			break;
		case 6:
			Code.Size = NumberSize::kRGB8bit;
			break;
		case 7:
			Code.Size = NumberSize::kRGB16bit;
			break;
		case 8:
			Code.Size = NumberSize::kRGB32bit;
			break;
		}
	}

	void LineContentFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.Content = LineContent::kRowCol;
			break;
		case 1:
			Code.Content = LineContent::kFrame;
			break;
		case 2:
			Code.Content = LineContent::kBytes;
			break;
		}
	}

	void RGBModeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Code.RGBFormat = RGBMode::kRGB;
			break;
		case 1:
			Code.RGBFormat = RGBMode::kBGR;
			break;
		case 2:
			Code.RGBFormat = RGBMode::kGRB;
			break;
		case 3:
			Code.RGBFormat = RGBMode::kBRG;
			break;
		case 4:
			Code.RGBFormat = RGBMode::kRGBSimple;
			break;
		}
	}

	void BinarySourceFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Binary.Source = ReadSource::kColumns;
			break;
		case 1:
			Binary.Source = ReadSource::kRows;
			break;
		}
	}

	void BinaryOrientationFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Binary.Orientation = InputOrientation::kTopBottomLeftRight;
			break;
		case 1:
			Binary.Orientation = InputOrientation::kBottomTopRightLeft;
			break;
		case 2:
			Binary.Orientation = InputOrientation::kSure24x16;
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
				Binary.Direction = ScanDirection::kRowLeftToRight;
				break;
			case 1:
				Binary.Direction = ScanDirection::kRowRightToLeft;
				break;
			case 2:
				Binary.Direction = ScanDirection::kRowAltLeftRight;
				break;
			case 3:
				Binary.Direction = ScanDirection::kRowAltRightLeft;
				break;
			}
		}
		else
		{
			switch (i)
			{
			case 0:
				Binary.Direction = ScanDirection::kColTopToBottom;
				break;
			case 1:
				Binary.Direction = ScanDirection::kColBottomToTop;
				break;
			case 2:
				Binary.Direction = ScanDirection::kColAltDownUp;
				break;
			case 3:
				Binary.Direction = ScanDirection::kColAltUpDown;
				break;
			}
		}
	}

	void BinaryLSBFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Binary.LSB = LeastSignificantBit::kTopLeft;
			break;
		case 1:
			Binary.LSB = LeastSignificantBit::kBottomRight;
			break;
		}
	}

	void BinaryRGBFormatFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Binary.RGBFormat = RGBMode::kRGB;
			break;
		case 1:
			Binary.RGBFormat = RGBMode::kBGR;
			break;
		case 2:
			Binary.RGBFormat = RGBMode::kGRB;
			break;
		case 3:
			Binary.RGBFormat = RGBMode::kBRG;
			break;
		case 4:
			Binary.RGBFormat = RGBMode::kRGBSimple;
			break;
		}
	}

	void BinaryFileContentsFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Binary.Content = BinaryFileContents::kEntireAnimation;
			break;
		case 1:
			Binary.Content = BinaryFileContents::kSingleFrame;
			break;
		}
	}


	void BinaryNumberSizeFromInt(int i)
	{
		switch (i)
		{
		case 0:
			Binary.Size = NumberSize::k8Bit;
			break;
		case 1:
			Binary.Size = NumberSize::k16bit;
			break;
		case 2:
			Binary.Size = NumberSize::k32bit;
			break;
		case 3:
			Binary.Size = NumberSize::k8bitSwap;
			break;
		case 4:
			Binary.Size = NumberSize::k16bitSwap;
			break;
		case 5:
			Binary.Size = NumberSize::k64bit;
			break;
		case 6:
			Binary.Size = NumberSize::kRGB8bit;
			break;
		case 7:
			Binary.Size = NumberSize::kRGB16bit;
			break;
		case 8:
			Binary.Size = NumberSize::kRGB32bit;
			break;
		}
	}
};
