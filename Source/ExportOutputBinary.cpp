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

#include <algorithm>

#include "ColourUtility.h"
#include "Convert.h"
#include "ExportOutputBinary.h"
#include "SystemSettings.h"
#include "Utility.h"

extern SystemSettings *GSystemSettings;


namespace ExportOutputBinary
{
	bool BinaryCreateExportAnimation(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount, std::vector<std::wstring> &unique_items)
	{
		auto baaProcessUnique = [unique_items](const std::wstring s) -> std::wstring
		{
			if (unique_items.size() == 0)
			{
				return s;
			}
			else
			{
				std::wstring m = s;

				for (int t = 0; t < unique_items.size(); t++)
				{
					m = Utility::ReplaceString(m, unique_items[t], std::to_wstring(t));
				}

				return m;
			}
		};

		int MatrixDataCount = std::max(matrix->Details.Height, matrix->Details.Width);

		std::vector<std::wstring> *MatrixData[MatrixDataCount];

		for (int t = 0; t < MatrixDataCount; t++)
		{
			MatrixData[t] = new std::vector<std::wstring>;
		}

		DataOut dataout;

		// ===========================================================================

		std::wstring spacingstring = L" ";

		// =========================================================================

		std::wstring op = L"";

		teo.DataPadding = L"" ;//PadString(' ', length(vartype));

		entrycount = 0; // total of all entries added to data variable in output

		// =========================================================================
		// =========================================================================

		for (int t = teo.Binary.StartFrame; t <= teo.Binary.EndFrame; t++)
		{
			for (int i = 0; i < MatrixDataCount; i++)
			{
				MatrixData[i]->clear();
			}

			if (teo.Binary.Source == ReadSource::kRows)
			{
				for (int y = 0; y < matrix->Details.Height; y++)
				{
					dataout = BinaryExportRowData(matrix, teo, t, y, spacingstring);

					for (int i = 0; i < dataout.Count; i++)
					{
						if (dataout.Data[i] != L"")
						{
							MatrixData[y]->push_back(baaProcessUnique(dataout.Data[i]) + spacingstring);
						}
					}

					entrycount += dataout.Count;
				}
			}
			else if (teo.Binary.Source == ReadSource::kColumns)
			{
				for (int x = 0; x < matrix->Details.Width; x++)
				{
					dataout = BinaryExportColumnData(matrix, teo, t, x, spacingstring);

					for (int i = 0; i < dataout.Count; i++)
					{
						if (dataout.Data[i] != L"")
						{
							MatrixData[x]->push_back(baaProcessUnique(dataout.Data[i]) + spacingstring);
						}
					}

					entrycount += dataout.Count;
				}
			}

			// ===========================================================================
			// row data
			// ===========================================================================

			op = L"";

			if (teo.Binary.Source == ReadSource::kRows)
			{
				int start = 0;
				int delta = 0;

				if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
				{
					start = 0;
					delta   = 1;
				}
				else
				{
					start = matrix->Details.Height - 1;
					delta   = -1;
				}

				int y = start;

				while (y != 99)
				{
					for (int z = 0; z < MatrixData[y]->size(); z++)
					{
					  op += (*MatrixData[y])[z];
					}

					y += delta;

					if (y > matrix->Details.Height - 1 || y < 0)
					{
						y = 99;
					}
				}

				BinaryAddContentByFrame(teo, op, t, output);
			}

			// ===========================================================================
			// col data
			// ===========================================================================

			if (teo.Binary.Source == ReadSource::kColumns)
			{
				switch (teo.Binary.Orientation)
				{
				case InputOrientation::kTopBottomLeftRight:
				case InputOrientation::kBottomTopRightLeft:
				{
					int start = 0;
					int delta = 0;

					if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
					{
						  start= 0;
						  delta  = 1;
					}
					else
					{
						  start = matrix->Details.Width - 1;
						  delta   = -1;
					}

					int y = start;

					while (y != 99)
					{
						for (int z = 0; z < MatrixData[y]->size(); z++)
						{
							op += (*MatrixData[y])[z];
						}

						y += delta;

						if (y > matrix->Details.Width - 1 || y < 0)
						{
							y = 99;
						}
					}

					break;
				}
				case InputOrientation::kSure24x16:
				{
					for (int y = 7; y >= 0; y--)
					{
						for (int z = 0; z < MatrixData[y]->size(); z++)
							op += (*MatrixData[y])[z] + spacingstring;
					}

					for (int y = 15; y >= 8; y--)
					{
						for (int z = 0; z < MatrixData[y]->size(); z++)
							op += (*MatrixData[y])[z] + spacingstring;
					}

					for (int y = 23; y >= 16; y--)
					{
						for (int z = 0; z < MatrixData[y]->size(); z++)
							op += (*MatrixData[y])[z] + spacingstring;
					}

					break;
				}
				}

				BinaryAddContentByFrame(teo, op, t, output);
			}
		}

        return true;
	}


	bool BinaryCreateExportAnimationRGB(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount, std::vector<std::wstring> &unique_items)
	{
		auto baaProcessUnique = [unique_items](const std::wstring s) -> std::wstring
		{
			if (unique_items.size() == 0)
			{
				return s;
			}
			else
			{
				std::wstring m = s;

				for (int t = 0; t < unique_items.size(); t++)
				{
					m = Utility::ReplaceString(m, unique_items[t], std::to_wstring(t));
				}

				return m;
			}
		};

		int MatrixDataCount = std::max(matrix->Details.Height, matrix->Details.Width);

		std::wstring MatrixData[MatrixDataCount];

		DataOut dataout;
		std::wstring spacingstring = L" ";
		std::wstring s = L"";

		entrycount = 0;

		spacingstring   = L" ";

		teo.DataPadding = L"";

		// =========================================================================
		// =========================================================================

		for (int t = teo.Binary.StartFrame; t <= teo.Binary.EndFrame; t++)
		{
			for (int i = 0; i < MatrixDataCount; i++)
			{
				MatrixData[i] = L"";
			}

			if (teo.Binary.Source == ReadSource::kRows)
			{
				for (int y = 0; y < matrix->Details.Height; y++)
				{
					dataout = BinaryExportRowDataRGB(matrix, teo, t, y, spacingstring);

					MatrixData[y] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}

			if (teo.Binary.Source == ReadSource::kColumns)
			{
				for (int x = 0; x < matrix->Details.Width; x++)
				{
					dataout = BinaryExportColumnDataRGB(matrix, teo, t, x, spacingstring);

					MatrixData[x] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}

			// ===========================================================================
			// ===========================================================================
			// row data
			// ===========================================================================
			// ===========================================================================

			if (teo.Binary.Source == ReadSource::kRows)
			{
				if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
				{
					s = L"";

					for (int y = 0; y < matrix->Details.Height; y++)
					{
						s += MatrixData[y];
					}

					BinaryAddContentByFrame(teo, s, t, output);
				}
				else
				{
					s = L"";

					for (int y = matrix->Details.Height - 1; y >= 0; y--)
					{
						s += MatrixData[y];
					}

					BinaryAddContentByFrame(teo, s, t, output);
				}
			}

			// ===========================================================================
			// col data
			// ===========================================================================

			if (teo.Binary.Source == ReadSource::kColumns)
			{
				switch (teo.Binary.Orientation)
				{
				case InputOrientation::kTopBottomLeftRight:
				case InputOrientation::kBottomTopRightLeft:
				{
					if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
					{
						s = L"";

						for (int x = 0; x < matrix->Details.Width; x++)
						{
							s += MatrixData[x];
						}

						BinaryAddContentByFrame(teo, s, t, output);
					}
					else
					{
						s = L"";

						for (int x = matrix->Details.Width - 1; x >= 0; x++)
						{
							s += MatrixData[x];
						}

						BinaryAddContentByFrame(teo, s, t, output);
					}
					break;
				}
				case InputOrientation::kSure24x16:
					break; // sure 2416 not available in RGB!!
				}
			}
		}

		return true;
	}


	DataOut BinaryExportRowData(TheMatrix *matrix, ExportOptions teo, int frame, int row, const std::wstring spacingchar)
	{
		DataOut dataout;
		dataout.Clear();

		InternalArray ia;
		ia.Clear();

		int bits = teo.GetNumberSizeLength(teo.Binary.Size);
		int pads = teo.GetNumberSizePadLength(teo.Binary.Size);

		int bitcounter = 0;
		int dataindex  = 0;

        std::wstring s = L"";

		ScanDirection direction = teo.Binary.Direction;

		Matrix *selectedmatrix;

		// ===========================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}

		// ===========================================================================

		if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
		{
			switch (direction)
			{
			case ScanDirection::kRowAltLeftRight:
				if (row % 2 == 0)
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				else
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				break;
			case ScanDirection::kRowAltRightLeft:
				if (row % 2 == 0)
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				else
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				break;
			}
		}
		else if (teo.Binary.Orientation == InputOrientation::kBottomTopRightLeft)
		{
			switch (direction)
			{
			case ScanDirection::kRowAltLeftRight:
				if ((matrix->Details.Height - row - 1) % 2 == 0)
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				else
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				break;
			case ScanDirection::kRowAltRightLeft:
				if ((matrix->Details.Height - row - 1) % 2 == 0)
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				else
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				break;
			}
		}

		// ===========================================================================

		if (direction == ScanDirection::kRowLeftToRight) // left to right
		{
			for (int x = 0; x < matrix->Details.Width; x++)
			{
				if (matrix->MatrixDeadLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
				{
					if (selectedmatrix->Grid[row * matrix->Details.Width + x] == 1)
					{
						if (teo.Binary.LSB == LeastSignificantBit::kTopLeft)
						{
							ia.Data[dataindex] += powers[bitcounter];
						}
						else
						{
							ia.Data[dataindex] += powers[bits - bitcounter];
                        }
					}

					if (bitcounter == bits)
					{
						bitcounter = 0;
						dataindex++;

						if (x != matrix->Details.Width - 1)
						{
							ia.Data[dataindex] = 0;
						}

						dataout.Count++;
					}
					else
					{
						bitcounter++;
					}
				}
			}
		}
		else if (direction == ScanDirection::kRowRightToLeft)                  // right to left
		{
			for (int x = matrix->Details.Width - 1; x >= 0; x--)
			{
				if (matrix->MatrixDeadLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
				{
					if (selectedmatrix->Grid[row * matrix->Details.Width + x] == 1)
					{
						if (teo.Binary.LSB == LeastSignificantBit::kTopLeft)
						{
							ia.Data[dataindex] += powers[bitcounter];
						}
						else
						{
							ia.Data[dataindex] += powers[bits - bitcounter];
                        }
					}

					if (bitcounter == bits)
					{
						bitcounter = 0;
						dataindex++;

						if (x != 0)
						{
							ia.Data[dataindex] = 0;
						}

						dataout.Count++;
					}
					else
					{
						bitcounter++;
					}
				}
			}
		}

		dataout.Count = dataindex;

		// ===========================================================================

		for (int x = 0; x < dataout.Count; x++)
		{
			if (ia.Data[x] != -1)
			{
				switch (teo.Binary.Size)
				{
				case NumberSize::k8bitSwap: // swap nybbles
				{
					std::wstring b = L"XX";

					s = IntToHex(ia.Data[x], 2);

					b[0] = s[1];
					b[1] = s[0];

					ia.Data[x] = Convert::HexToInt(b);
					break;
				}
				case NumberSize::k16bitSwap: // swap bytes
				{
					std::wstring b = L"XXXX";

					s = IntToHex(ia.Data[x], 4);

					b[0] = s[2];
					b[1] = s[3];
					b[2] = s[0];
					b[3] = s[1];

					ia.Data[x] = Convert::HexToInt(b);
					break;
				}
				}

				switch (teo.Binary.Format)
				{
				case NumberFormat::kDecimal:
					dataout.Data[x] = std::to_wstring(ia.Data[x]);
					break;
				case NumberFormat::kBinary:
					dataout.Data[x] = Convert::IntegerToBinary(bits, ia.Data[x]);
					break;
				case NumberFormat::kHex:
					dataout.Data[x] = IntToHex(ia.Data[x], pads);
					break;
				}
			}
		}

		return dataout;
	}


	DataOut BinaryExportRowDataRGB(TheMatrix *matrix, ExportOptions teo, int frame, int row, const std::wstring spacingchar)
	{
		DataOut dataout;
		std::wstring output = L"";
		ScanDirection direction = teo.Binary.Direction;

		Matrix *selectedmatrix;

		// ===================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}

		// ===================================================================

		if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
		{
			switch (direction)
			{
			case ScanDirection::kRowAltLeftRight:
				if (row % 2 == 0)
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				else
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				break;
			case ScanDirection::kRowAltRightLeft:
				if (row % 2 == 0)
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				else
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				break;
			}
		}
		else if (teo.Binary.Orientation == InputOrientation::kBottomTopRightLeft)
		{
			switch (direction)
			{
			case ScanDirection::kRowAltLeftRight:
				if ((matrix->Details.Height - row - 1) % 2 == 0)
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				else
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				break;
			case ScanDirection::kRowAltRightLeft:
				if ((matrix->Details.Height - row - 1) % 2 == 0)
				{
					direction = ScanDirection::kRowRightToLeft;
				}
				else
				{
					direction = ScanDirection::kRowLeftToRight;
				}
				break;
			}
		}

		// ===========================================================================

		if (direction == ScanDirection::kRowLeftToRight)        // left to right
		{
			for (int x = 0; x < matrix->Details.Width; x++)
			{
				if (matrix->MatrixDeadLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
				{
					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + row] == matrix->RGBBackground)
						{
							output += ColourUtility::RGBConvertToSplit(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
						}
						else
						{
							output += ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[row * matrix->Details.Width + x], teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
						}

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + x] == matrix->RGBBackground)
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
						}
						else
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(selectedmatrix->Grid[row * matrix->Details.Width + x], teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
						}

			//          if x <> matrix->DetailsWidth - 1 then
						output += spacingchar;

						dataout.Count++;
					}
				}
			}

		}
		else if (direction == ScanDirection::kRowRightToLeft)        // right to left
		{
			for (int x = matrix->Details.Width - 1; x >= 0; x--)
			{
				if (matrix->MatrixDeadLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
				{
					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + x] == matrix->RGBBackground)
						{
							output += ColourUtility::RGBConvertToSplit(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
						}
						else
						{
							output += ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[row * matrix->Details.Width + x], teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
						}

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + x] == matrix->RGBBackground)
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
						}
						else
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(selectedmatrix->Grid[row * matrix->Details.Width + x], teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
                        }

		  //        if x <> 0 then
						output += spacingchar;

						dataout.Count++;
					}
				}
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	DataOut BinaryExportColumnData(TheMatrix *matrix, ExportOptions teo, int frame, int col, const std::wstring spacingchar)
	{
		std::wstring s = L"";
		DataOut dataout;
		ScanDirection direction = teo.Binary.Direction;
		InternalArray ia;
		ia.Clear();

		Matrix *selectedmatrix;

		int bitcounter = 0;
		int dataindex  = 0;

		int bits = teo.GetNumberSizeLength(teo.Binary.Size);
		int pads = teo.GetNumberSizePadLength(teo.Binary.Size);

		for (int y = 0; y < DataOutDataMax; y++)
		{
			dataout.Data[y] = L"";
		}

		// ===========================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
			selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
		else
			selectedmatrix = matrix->MatrixUser[frame];

		// ===========================================================================

		if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
		{
			switch (direction)
			{
			case ScanDirection::kColAltDownUp:
				if (col % 2 == 0)
					direction = ScanDirection::kColTopToBottom;
				else
					direction = ScanDirection::kColBottomToTop;
				break;
			case ScanDirection::kColAltUpDown:
				if (col % 2 == 0)
					direction = ScanDirection::kColBottomToTop;
				else
					direction = ScanDirection::kColTopToBottom;
				break;
			}
		}
		else if (teo.Binary.Orientation == InputOrientation::kBottomTopRightLeft)
		{
			switch (direction)
			{
			case ScanDirection::kColAltDownUp:
				if ((matrix->Details.Width - col - 1) % 2 == 0)
				   direction = ScanDirection::kColTopToBottom;
				else
				   direction = ScanDirection::kColBottomToTop;
				break;
			case ScanDirection::kColAltUpDown:
				if ((matrix->Details.Width - col - 1) % 2 == 0)
				   direction = ScanDirection::kColBottomToTop;
				else
				   direction = ScanDirection::kColTopToBottom;
				break;
			}
		}

		// ===========================================================================

		if (direction == ScanDirection::kColTopToBottom)
		{
			for (int y = 0; y < matrix->Details.Height; y++)
			{
				if (matrix->MatrixDeadLayout->Grid[y * matrix->Details.Width + col] == PixelAlive)
				{
					if (selectedmatrix->Grid[y * matrix->Details.Width + col] == 1)
					{
						if (teo.Binary.LSB == LeastSignificantBit::kTopLeft)
							ia.Data[dataindex] += powers[bitcounter];
						else
							ia.Data[dataindex] += powers[bits - bitcounter];
					}

					if (bitcounter == bits)
					{
						bitcounter = 0;
						dataindex++;

						if (y != matrix->Details.Height - 1)
						{
							ia.Data[dataindex] = 0;

							dataout.Count++;
						}
					}
					else
					{
						bitcounter++;
					}
				}
			}
		}
		else if (direction == ScanDirection::kColBottomToTop)
		{
			for (int y = matrix->Details.Height - 1; y >= 0; y++)
			{
				if (matrix->MatrixDeadLayout->Grid[y * matrix->Details.Width + col] == PixelAlive)
				{
					if (selectedmatrix->Grid[y * matrix->Details.Width + col] == 1)
					{
						if (teo.Binary.LSB == LeastSignificantBit::kTopLeft)
						{
							ia.Data[dataindex] += powers[bitcounter];
						}
						else
						{
							ia.Data[dataindex] += powers[bits - bitcounter];
						}
					}

					if (bitcounter == bits)
					{
						bitcounter = 0;
						dataindex++;

						if (y != 0)
						{
							ia.Data[dataindex] = 0;
						}

						dataout.Count++;
					}
					else
					{
						bitcounter++;
					}
				}
			}
		}

		dataout.Count = dataindex;

		// ===========================================================================

		for (int y = 0; y < dataout.Count; y++)
		{
			if (ia.Data[y] != -1)
			{
				switch (teo.Binary.Size)
				{
				case NumberSize::k8bitSwap:		// swap nybbles
				{
					std::wstring b = L"XX";

					s = IntToHex(ia.Data[y], 2);

					b[0] = s[1];
					b[1] = s[0];

					ia.Data[y] = Convert::HexToInt(b);
					break;
				}
				case NumberSize::k16bitSwap:	// swap bytes
				{
					std::wstring b = L"XXXX";

					s = IntToHex(ia.Data[y], 4);

					b[0] = s[2];
					b[1] = s[3];
					b[2] = s[0];
					b[3] = s[1];

					ia.Data[y] = Convert::HexToInt(b);
					break;
				}
				}

				switch (teo.Binary.Format)
				{
				case NumberFormat::kDecimal:
					dataout.Data[y] = std::to_wstring(ia.Data[y]);
					break;
				case NumberFormat::kBinary:
					dataout.Data[y] = Convert::IntegerToBinary(bits, ia.Data[y]);
					break;
				case NumberFormat::kHex:
					dataout.Data[y] = IntToHex(ia.Data[y], pads);
					break;
				}
			}
		}

		return dataout;
	}


	DataOut BinaryExportColumnDataRGB(TheMatrix *matrix, ExportOptions teo, int frame, int col, const std::wstring spacingchar)
	{
		Matrix *selectedmatrix;

		DataOut dataout;
		std::wstring output = L"";
		ScanDirection direction = teo.Binary.Direction;

		// ===========================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
			selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
		else
			selectedmatrix = matrix->MatrixUser[frame];

		// ===========================================================================

		if (teo.Binary.Orientation == InputOrientation::kTopBottomLeftRight)
		{
			switch (direction)
			{
			case ScanDirection::kColAltDownUp:
				if (col % 2 == 0)
				   direction = ScanDirection::kColTopToBottom;
				else
				   direction = ScanDirection::kColBottomToTop;
				break;
			case ScanDirection::kColAltUpDown:
				if (col % 2 == 0)
				   direction = ScanDirection::kColBottomToTop;
				else
				   direction = ScanDirection::kColTopToBottom;
				break;
			}
		}
		else if (teo.Binary.Orientation == InputOrientation::kBottomTopRightLeft)
		{
			switch (direction)
			{
			case ScanDirection::kColAltDownUp:
				if ((matrix->Details.Width - col - 1) % 2 == 0)
				   direction = ScanDirection::kColTopToBottom;
				else
				   direction = ScanDirection::kColBottomToTop;
				break;
			case ScanDirection::kColAltUpDown:
				if ((matrix->Details.Width - col - 1) % 2 == 0)
				   direction = ScanDirection::kColBottomToTop;
				else
				   direction = ScanDirection::kColTopToBottom;
				break;
			}
		}

		// ===========================================================================

		if (direction == ScanDirection::kColTopToBottom)             // top to bottom
		{
			for (int y = 0; y < matrix->Details.Height; y++)
			{
				if (matrix->MatrixDeadLayout->Grid[y * matrix->Details.Height + col] == PixelAlive)
				{
					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[y * matrix->Details.Height + col] == matrix->RGBBackground)
						{
							output += ColourUtility::RGBConvertToSplit(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
						}
						else
						{
							output += ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[y * matrix->Details.Width + col], teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
						}

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[y * matrix->Details.Height + col] == matrix->RGBBackground)
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
						}
						else
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(selectedmatrix->Grid[y * matrix->Details.Width + col], teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
                        }

						output += spacingchar;

						dataout.Count++;
					}
				}
			}
		}
		else if (direction == ScanDirection::kColBottomToTop)        // bottom to top
		{
			for (int y = matrix->Details.Height - 1; y >= 0; y--)
			{
				if (matrix->MatrixDeadLayout->Grid[y * matrix->Details.Height + col] == PixelAlive)
				{
					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[y * matrix->Details.Height + col] == matrix->RGBBackground)
						{
							output += ColourUtility::RGBConvertToSplit(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
						}
						else
						{
							output += ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[y * matrix->Details.Height + col], teo.Binary.RGBFormat, teo.Binary.RGBBrightness, teo.Binary.Format, L"", spacingchar, teo.Binary.ColourSpaceRGB);
                        }

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[y * matrix->Details.Height + col] == matrix->RGBBackground)
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(teo.Binary.RGBChangeColour, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
						}
						else
						{
							output += IntToHex(ColourUtility::RGBConvertTo32(selectedmatrix->Grid[y * matrix->Details.Height + col], teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);
						}

						output += spacingchar;

						dataout.Count++;
					}
				}
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	std::wstring BinaryGetRowData(TheMatrix *matrix, bool hexmode, int direction, int frame, int row)
	{
		std::wstring output = L"";
		unsigned __int64 total = 0;

		for (int x = 0; x < matrix->Details.Width; x++)
		{
			if (matrix->MatrixLayers[0]->Cells[frame]->Grid[row * matrix->Details.Width + x] == 1)
			{
				if (direction == 0)
					total += powers[x];
				else
					total += powers[matrix->Details.Width - x];
			}
		}

		if (hexmode)
			return IntToHex(total, GSystemSettings->App.PadModeHexRow).c_str();

		return std::to_wstring(total);
	}


	std::wstring BinaryGetColumnData(TheMatrix *matrix, bool hexmode, int direction, int frame, int col)
	{
		std::wstring output = L"";
		unsigned __int64 total = 0;

		for (int y = 0; y < matrix->Details.Height; y++)
		{
			if (matrix->MatrixLayers[0]->Cells[frame]->Grid[y * matrix->Details.Width + col] == 1)
			{
				if (direction == 0)
					total += powers[y];
				else
					total += powers[matrix->Details.Height - y];
			}
		}

		if (hexmode)
			return IntToHex(total, GSystemSettings->App.PadModeHexCol).c_str();

		return std::to_wstring(total);
	}


	void BinaryAddContentByFrame(ExportOptions teo, const std::wstring s, int frame, std::vector<std::wstring> &output)
	{
		std::wstring m = s.substr(0, s.length() - 1);

		output.push_back(m + L" ");
		output.push_back(L"");
    }
}
