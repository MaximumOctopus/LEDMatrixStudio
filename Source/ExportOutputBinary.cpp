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

#include <algorithm>

#include "ColourUtility.h"
#include "Convert.h"
#include "ExportOutputBinary.h"
#include "ExportUtility.h"
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

		std::wstring op = L"";

		teo.DataPadding = L"";

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
					dataout = BinaryExportRowData(matrix, teo, t, y);

					for (int i = 0; i < dataout.Count; i++)
					{
						if (dataout.Data[i] != L"")
						{
							MatrixData[y]->push_back(baaProcessUnique(dataout.Data[i]) + L" ");
						}
					}

					entrycount += dataout.Count;
				}
			}
			else if (teo.Binary.Source == ReadSource::kColumns)
			{
				for (int x = 0; x < matrix->Details.Width; x++)
				{
					dataout = BinaryExportColumnData(matrix, teo, t, x);

					for (int i = 0; i < dataout.Count; i++)
					{
						if (dataout.Data[i] != L"")
						{
							MatrixData[x]->push_back(baaProcessUnique(dataout.Data[i]) + L" ");
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
					delta = 1;
				}
				else
				{
					start = matrix->Details.Height - 1;
					delta = -1;
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
						  start = 0;
						  delta = 1;
					}
					else
					{
						  start = matrix->Details.Width - 1;
						  delta = -1;
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
						{
							op += (*MatrixData[y])[z] + L" ";
						}
					}

					for (int y = 15; y >= 8; y--)
					{
						for (int z = 0; z < MatrixData[y]->size(); z++)
						{
							op += (*MatrixData[y])[z] + L" ";
						}
					}

					for (int y = 23; y >= 16; y--)
					{
						for (int z = 0; z < MatrixData[y]->size(); z++)
						{
							op += (*MatrixData[y])[z] + L" ";
                        }
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
		std::wstring s = L"";

		entrycount = 0;

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
					dataout = BinaryExportRowDataRGB(matrix, teo, t, y);

					MatrixData[y] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}

			if (teo.Binary.Source == ReadSource::kColumns)
			{
				for (int x = 0; x < matrix->Details.Width; x++)
				{
					dataout = BinaryExportColumnDataRGB(matrix, teo, t, x);

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

						for (int x = matrix->Details.Width - 1; x >= 0; x--)
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


	bool BinaryCreateExportAnimationRGB3bpp(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount)
	{
		int MatrixDataCount = std::max(matrix->Details.Height, matrix->Details.Width);

		std::wstring MatrixData[MatrixDataCount];

		DataOut dataout;
		std::wstring s = L"";

		entrycount = 0;

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
				dataout = BinaryExportFrameDataByRowRGB3bpp(matrix, teo, t);
			}
			else if (teo.Binary.Source == ReadSource::kColumns)
			{
				dataout = BinaryExportFrameDataByColumnRGB3bpp(matrix, teo, t);
            }

			entrycount += dataout.Count;

			BinaryAddContentByFrame(teo, dataout.Data[0], t, output);
		}

		return true;
    }


	DataOut BinaryExportRowData(TheMatrix *matrix, ExportOptions teo, int frame, int row)
	{
		DataOut dataout;
		dataout.Clear();

		InternalArray ia;
		ia.Clear();

		int bits = teo.GetNumberSizeLength(teo.Binary.Size);
		int pads = teo.GetNumberSizePadLength(teo.Binary.Size);

		int bitcounter = 0;
		int dataindex  = 0;

		ia.Data[dataindex] = 0;

        std::wstring s = L"";

		ScanDirection direction = teo.Binary.Direction;

		Matrix *selectedmatrix;

		// ===========================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
			}
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}

		// ===========================================================================

		direction = ExportUtility::UpdateDirectionRow(direction, teo.Binary.Orientation, matrix->Details.Height, row);

		// ===========================================================================

		if (direction == ScanDirection::kRowLeftToRight)
		{
			for (int x = 0; x < matrix->Details.Width; x++)
			{
				if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
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
		else if (direction == ScanDirection::kRowRightToLeft)
		{
			for (int x = matrix->Details.Width - 1; x >= 0; x--)
			{
				if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
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
				ia.SwapData(x, teo.Binary.Size);

				dataout.Data[x] = ExportUtility::FormatDataAs(ia.Data[x], teo.Binary.Format, bits, pads);
			}
		}

		return dataout;
	}


	DataOut BinaryExportRowDataRGB(TheMatrix *matrix, ExportOptions teo, int frame, int row)
	{
		DataOut dataout;
		std::wstring output = L"";
		ScanDirection direction = teo.Binary.Direction;

		Matrix *selectedmatrix;

		// ===================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
			}
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}

		// ===================================================================

		direction = ExportUtility::UpdateDirectionRow(direction, teo.Binary.Orientation, matrix->Details.Height, row);

		// ===========================================================================

		if (direction == ScanDirection::kRowLeftToRight)        // left to right
		{
			for (int x = 0; x < matrix->Details.Width; x++)
			{
				if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[row * matrix->Details.Width + x];

					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && pixel_value == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += ColourUtility::RGBConvertToSplit(pixel_value, teo.Binary, L"", L" ");

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && pixel_value == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += IntToHex(ColourUtility::RGBConvertTo32(pixel_value, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);

						output += L" ";

						dataout.Count++;
					}
				}
			}

		}
		else if (direction == ScanDirection::kRowRightToLeft)        // right to left
		{
			for (int x = matrix->Details.Width - 1; x >= 0; x--)
			{
				if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + x] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[row * matrix->Details.Width + x];

					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && pixel_value == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += ColourUtility::RGBConvertToSplit(pixel_value, teo.Binary, L"", L" ");

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && pixel_value == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += IntToHex(ColourUtility::RGBConvertTo32(pixel_value, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);

						output += L" ";

						dataout.Count++;
					}
				}
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	DataOut BinaryExportFrameDataByRowRGB3bpp(TheMatrix *matrix, ExportOptions teo, int frame)
	{
		DataOut dataout;
		dataout.Clear();
		std::wstring output = L"";
		ScanDirection direction = teo.Code.Direction;

		Matrix *selectedmatrix;

		BitCounting bc;

		auto baaBitStream = [&output, &teo](BitCounting &bc, int pixel, int test) -> void
		{
			unsigned _int64 p = powers[bc.highbit - bc.bitcounter];

			if (teo.Code.LSB == LeastSignificantBit::kTopLeft)
			{
				p = powers[bc.bitcounter];
			}

			if ((pixel & test) == test)
			{
				bc.databyte += p;
			}

			if (bc.Next())
			{
				output += IntToHex(bc.databyte, 2) + L" ";

				bc.Reset();
			}
		};

		// ===================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
            }
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}


		if (direction == ScanDirection::kRowLeftToRight)
		{
			bc.SetDirection(_BitCountDirectionDown, 7);

			for (int row = 0; row < matrix->Details.Height; row++)
			{
				for (int pixel = 0; pixel < matrix->Details.Width; pixel++)
				{
					if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + pixel] == PixelAlive)
					{
						baaBitStream(bc, selectedmatrix->Grid[row * matrix->Details.Width + pixel], 4);
						baaBitStream(bc, selectedmatrix->Grid[row * matrix->Details.Width + pixel], 2);
						baaBitStream(bc, selectedmatrix->Grid[row * matrix->Details.Width + pixel], 1);
					}
				}
			}
		}
		else if (direction == ScanDirection::kRowRightToLeft)
		{
			bc.SetDirection(_BitCountDirectionUp, 7);

			for (int row = 0; row < matrix->Details.Height; row++)
			{
				for (int pixel = matrix->Details.Width - 1; pixel >= 0; pixel--)
				{
					if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + pixel] == PixelAlive)
					{
						baaBitStream(bc, selectedmatrix->Grid[row * matrix->Details.Width + pixel], 4);
						baaBitStream(bc, selectedmatrix->Grid[row * matrix->Details.Width + pixel], 2);
						baaBitStream(bc, selectedmatrix->Grid[row * matrix->Details.Width + pixel], 1);
					}
				}
			}
		}

		if (!bc.IsStartingPosition())
		{
			output += IntToHex(bc.databyte, 2) + L" ";

			bc.outputcount++;
		}

		// ===================================================================

		dataout.Count = bc.outputcount;

		dataout.Data[0] = output;

		return dataout;
	}


	DataOut BinaryExportColumnData(TheMatrix *matrix, ExportOptions teo, int frame, int col)
	{
		std::wstring s = L"";
		DataOut dataout;
		ScanDirection direction = teo.Binary.Direction;
		InternalArray ia;
		ia.Clear();

		Matrix *selectedmatrix;

		int bitcounter = 0;
		int dataindex  = 0;

		ia.Data[dataindex] = 0;

		int bits = teo.GetNumberSizeLength(teo.Binary.Size);
		int pads = teo.GetNumberSizePadLength(teo.Binary.Size);

		for (int y = 0; y < _DataOutDataMax; y++)
		{
			dataout.Data[y] = L"";
		}

		// ===================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
			}
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}

		// ===================================================================

		direction = ExportUtility::UpdateDirectionColumn(direction, teo.Binary.Orientation, matrix->Details.Width, col);

		// ===================================================================

		if (direction == ScanDirection::kColTopToBottom)
		{
			for (int y = 0; y < matrix->Details.Height; y++)
			{
				if (matrix->MatrixIgnoredLayout->Grid[y * matrix->Details.Width + col] == PixelAlive)
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
			for (int y = matrix->Details.Height - 1; y >= 0; y--)
			{
				if (matrix->MatrixIgnoredLayout->Grid[y * matrix->Details.Width + col] == PixelAlive)
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

		// ===================================================================

		for (int y = 0; y < dataout.Count; y++)
		{
			if (ia.Data[y] != -1)
			{
				ia.SwapData(y, teo.Binary.Size);

				dataout.Data[y] = ExportUtility::FormatDataAs(ia.Data[y], teo.Binary.Format, bits, pads);
			}
		}

		return dataout;
	}


	DataOut BinaryExportColumnDataRGB(TheMatrix *matrix, ExportOptions teo, int frame, int col)
	{
		Matrix *selectedmatrix;

		DataOut dataout;
		std::wstring output = L"";
		ScanDirection direction = teo.Binary.Direction;

		// ===================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
			}
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}

		// ===================================================================

		direction = ExportUtility::UpdateDirectionColumn(direction, teo.Binary.Orientation, matrix->Details.Width, col);

		// ===================================================================

		if (direction == ScanDirection::kColTopToBottom)             // top to bottom
		{
			for (int y = 0; y < matrix->Details.Height; y++)
			{
				if (matrix->MatrixIgnoredLayout->Grid[y * matrix->Details.Width + col] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[y * matrix->Details.Width + col];

					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[y * matrix->Details.Width + col] == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += ColourUtility::RGBConvertToSplit(pixel_value, teo.Binary, L"", L" ");

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[y * matrix->Details.Width + col] == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += IntToHex(ColourUtility::RGBConvertTo32(pixel_value, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);

						output += L" ";

						dataout.Count++;
					}
				}
			}
		}
		else if (direction == ScanDirection::kColBottomToTop)        // bottom to top
		{
			for (int y = matrix->Details.Height - 1; y >= 0; y--)
			{
				if (matrix->MatrixIgnoredLayout->Grid[y * matrix->Details.Width + col] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[y * matrix->Details.Width + col];

					if (teo.Binary.Size == NumberSize::kRGB8bit)
					{
						if (teo.Binary.RGBChangePixels && pixel_value == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += ColourUtility::RGBConvertToSplit(pixel_value, teo.Binary, L"", L" ");

						dataout.Count += 3;
					}
					else if (teo.Binary.Size == NumberSize::kRGB32bit)
					{
						if (teo.Binary.RGBChangePixels && selectedmatrix->Grid[y * matrix->Details.Width + col] == matrix->RGBBackground)
						{
							pixel_value = teo.Binary.RGBChangeColour;
						}

						output += IntToHex(ColourUtility::RGBConvertTo32(pixel_value, teo.Binary.RGBFormat, teo.Binary.LSB, teo.Binary.RGBBrightness), 8);

						output += L" ";

						dataout.Count++;
					}
				}
			}
		}

		// ===================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	DataOut BinaryExportFrameDataByColumnRGB3bpp(TheMatrix *matrix, ExportOptions teo, int frame)
	{
		DataOut dataout;
		dataout.Clear();
		std::wstring output = L"";
		ScanDirection direction = teo.Code.Direction;

		Matrix *selectedmatrix;

		BitCounting bc;

		auto baaBitStream = [&output, &teo](BitCounting &bc, int pixel, int test) -> void
		{
			unsigned _int64 p = powers[bc.highbit - bc.bitcounter];

			if (teo.Code.LSB == LeastSignificantBit::kTopLeft)
			{
				p = powers[bc.bitcounter];
			}

			if ((pixel & test) == test)
			{
				bc.databyte += p;
			}

			if (bc.Next())
			{
				output += IntToHex(bc.databyte, 2) + L" ";

				bc.Reset();
			}
		};

		// ===================================================================

		if (teo.ExportMode == ExportSource::kAnimation)
		{
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
            }
		}
		else
		{
			selectedmatrix = matrix->MatrixUser[frame];
		}

		if (direction == ScanDirection::kColTopToBottom)
		{
			bc.SetDirection(_BitCountDirectionDown, 7);

			for (int col = 0; col < matrix->Details.Width; col++)
			{
				for (int pixel = 0; pixel < matrix->Details.Height; pixel++)
				{
					if (matrix->MatrixIgnoredLayout->Grid[pixel * matrix->Details.Width + col] == PixelAlive)
					{
						baaBitStream(bc, selectedmatrix->Grid[pixel * matrix->Details.Width + col], 4);
						baaBitStream(bc, selectedmatrix->Grid[pixel * matrix->Details.Width + col], 2);
						baaBitStream(bc, selectedmatrix->Grid[pixel * matrix->Details.Width + col], 1);
					}
				}
			}
		}
		else if (direction == ScanDirection::kColBottomToTop)
		{
			bc.SetDirection(_BitCountDirectionUp, 7);

			for (int col = 0; col < matrix->Details.Width; col++)
			{
				for (int pixel = matrix->Details.Height - 1; pixel >= 0; pixel--)
				{
					if (matrix->MatrixIgnoredLayout->Grid[pixel * matrix->Details.Width + col] == PixelAlive)
					{
						baaBitStream(bc, selectedmatrix->Grid[pixel * matrix->Details.Width + col], 4);
						baaBitStream(bc, selectedmatrix->Grid[pixel * matrix->Details.Width + col], 2);
						baaBitStream(bc, selectedmatrix->Grid[pixel * matrix->Details.Width + col], 1);
					}
				}
			}
		}

		if (!bc.IsStartingPosition())
		{
			output += IntToHex(bc.databyte, 2) + L" ";

			bc.outputcount++;
		}

		// ===================================================================

		dataout.Count = bc.outputcount;

		dataout.Data[0] = output;

		return dataout;
	}


	std::wstring BinaryGetRowData(Matrix *matrix, bool hexmode, int direction, int frame, int row)
	{
		std::wstring output = L"";
		unsigned __int64 total = 0;

		for (int x = 0; x < matrix->Width; x++)
		{
			if (matrix->Grid[row * matrix->Width + x] == 1)
			{
				if (direction == 0)
				{
					total += powers[x];
				}
				else
				{
					total += powers[matrix->Width - x];
                }
			}
		}

		if (hexmode)
		{
			return IntToHex(total, GSystemSettings->App.PadModeHexRow).c_str();
		}

		return std::to_wstring(total);
	}


	std::wstring BinaryGetColumnData(Matrix *matrix, bool hexmode, int direction, int frame, int col)
	{
		std::wstring output = L"";
		unsigned __int64 total = 0;

		for (int y = 0; y < matrix->Height; y++)
		{
			if (matrix->Grid[y * matrix->Width + col] == 1)
			{
				if (direction == 0)
				{
					total += powers[y];
				}
				else
				{
					total += powers[matrix->Height - y];
                }
			}
		}

		if (hexmode)
		{
			return IntToHex(total, GSystemSettings->App.PadModeHexCol).c_str();
		}

		return std::to_wstring(total);
	}


	void BinaryAddContentByFrame(ExportOptions teo, const std::wstring s, int frame, std::vector<std::wstring> &output)
	{
		std::wstring m = s.substr(0, s.length() - 1);

		output.push_back(m + L" ");
		output.push_back(L"");
    }
}
