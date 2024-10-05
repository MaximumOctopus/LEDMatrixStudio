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
#include "ExportRGB.h"
#include "ExportUtility.h"
#include "SystemSettings.h"
#include "Utility.h"

extern SystemSettings *GSystemSettings;


namespace ExportRGB
{
	bool CreateExportAnimationRGB(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount, std::vector<std::wstring> &unique_items)
	{
		int MatrixDataCount = std::max(matrix->Details.Height, matrix->Details.Width);

		std::wstring MatrixData[MatrixDataCount];

		std::wstring s = L"";

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

		auto baaUpdateOutput = [teo, &output, &s](const std::wstring &data) -> void
		{
			switch (teo.Code.Content)
			{
				case LineContent::kRowCol:
					if (!data.empty())
					{
						ExportUtility::AddRowColContent(teo, data, output);
					}
					break;
				case LineContent::kFrame:
					s += data;
					break;
				case LineContent::kBytes:
					break;
			}
		};

		entrycount = 0; // total of all entries added to data variable in output

		std::wstring vartype = L"";
		std::wstring spacingstring = L"";
		std::wstring prefix = ExportUtility::GetNumberFormat(teo.Code.Language, teo.Code.Format);
		std::wstring cdescription = L"";
		DataOut dataout;

		// ===========================================================================

		if (teo.Code.CleanMode)
		{
			spacingstring = L" ";

			teo.Code.Language  = ExportLanguage::kSpecial;
		}
		else
		{
			spacingstring = L", ";
		}

		// ===========================================================================

		if (teo.Code.IncludePreamble)
		{
			if (teo.ExportMode == ExportSource::kAnimation)
			{
				cdescription = GLanguageHandler->Text[kFrame];
			}
			else
			{
				cdescription = GLanguageHandler->Text[kMemory];
			}

			// =========================================================================

			ExportUtility::GetPreamble(teo, output, false, matrix->Details.Comment);

			ExportUtility::GetSpacerLine(teo.Code.Language, output);
			output.push_back(L"");
		}

		// ===================================================================
		// ===================================================================
		// ===================================================================

		vartype = ExportUtility::GetSingleVariableStatement(teo.Code.Language, teo.Code.Size);

		if (vartype != L"")
		{
			output.push_back(vartype);
		}

		teo.DataPadding = ExportUtility::GetPadding(teo.Code.Language, vartype.length());

		// ===================================================================
		// ===================================================================

		for (int t = teo.Code.StartFrame; t <= teo.Code.EndFrame; t++)
		{
			if (teo.Code.Language == ExportLanguage::kCFastLED)
			{
				output.push_back(ExportUtility::GetVariableIDFrameIn(teo.Code.Language, t));
			}

			// =========================================================================

			for (int i = 0; i < std::max(matrix->Details.Height, matrix->Details.Width); i++)
			{
				MatrixData[i] = L"";
			}

			if (teo.Code.Source == ReadSource::kRows)
			{
				for (int y = teo.Code.SelectiveStart - 1; y < teo.Code.SelectiveEnd; y++)
				{
					dataout = ExportRowDataRGB(matrix, prefix, teo, t, y, spacingstring);

					MatrixData[y] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}
			else if (teo.Code.Source == ReadSource::kColumns)
			{
				for (int x = teo.Code.SelectiveStart - 1; x < teo.Code.SelectiveEnd; x++)
				{
					dataout = ExportColumnDataRGB(matrix, prefix, teo, t, x, spacingstring);

					MatrixData[x] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}

			// ===========================================================================
			// row data
			// ===========================================================================

			if (teo.Code.Source == ReadSource::kRows)
			{
				s = L"";

				if (teo.Code.Orientation == InputOrientation::kTopBottomLeftRight)
				{
					for (int y = 0; y < matrix->Details.Height; y++)
					{
						baaUpdateOutput(MatrixData[y]);
					}
				}
				else
				{
					for (int y = matrix->Details.Height - 1; y >= 0; y--)
					{
						baaUpdateOutput(MatrixData[y]);
					}
				}

				if (teo.Code.Content == LineContent::kFrame)
				{
					ExportUtility::AddContentByFrame(teo, s, t, output);
				}
			}

			// ===========================================================================
			// col data
			// ===========================================================================

			if (teo.Code.Source == ReadSource::kColumns)
			{
				switch (teo.Code.Orientation)
				{
				case InputOrientation::kTopBottomLeftRight:
				case InputOrientation::kBottomTopRightLeft:
				{
					s = L"";

					if (teo.Code.Orientation == InputOrientation::kTopBottomLeftRight)
					{
						for (int x = teo.Code.SelectiveStart - 1; x < teo.Code.SelectiveEnd; x++)
						{
							baaUpdateOutput(MatrixData[x]);
						}
					}
					else
					{
						for (int x = teo.Code.SelectiveEnd - 1; x >= teo.Code.SelectiveStart - 1; x--)
						{
							baaUpdateOutput(MatrixData[x]);
						}
					}

					if (teo.Code.Content == LineContent::kFrame)
					{
						ExportUtility::AddContentByFrame(teo, s, t, output);
					}
					break;
				}
				case InputOrientation::kSure24x16:
					s = GLanguageHandler->Text[kSure24x16BoardNotAvailableInRGBMode]; // sure 2416 not available in RGB!!
					break;
				}
			}

			if (teo.Code.Language == ExportLanguage::kCFastLED)
			{
				output.push_back(ExportUtility::GetVariableIDFrameOut(teo.Code.Language));

				output.push_back(L"");
			}
		}

		// =========================================================================

		ExportUtility::AddEnding(output, teo);

		if (teo.Code.IncludePreamble)
		{
			ExportUtility::GetSpacerLine(teo.Code.Language, output);
		}

        return true;
	}


	DataOut ExportColumnDataRGB(TheMatrix *matrix, const std::wstring prefix, ExportOptions teo, int frame, int col, const std::wstring spacingchar)
	{
		DataOut dataout;
		dataout.Count = 0;
		ScanDirection direction = teo.Code.Direction;
        std::wstring output = L"";

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

		direction = ExportUtility::UpdateDirectionColumn(direction, teo.Code.Orientation, matrix->Details.Width, col);

		// ===========================================================================

		if (direction == ScanDirection::kColTopToBottom)             // top to bottom
		{
			for (int pixel = 0; pixel < matrix->Details.Height; pixel++)
			{
				if (matrix->MatrixIgnoredLayout->Grid[pixel * matrix->Details.Width + col] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[pixel * matrix->Details.Width + col];

					if (teo.Code.RGBChangePixels && pixel_value == matrix->RGBBackground)
					{
						pixel_value = teo.Code.RGBChangeColour;
					}

					switch (teo.Code.Size)
					{
					case NumberSize::kRGB8bit:
						output = output + ColourUtility::RGBConvertToSplit(pixel_value, teo.Code, prefix, spacingchar);

						dataout.Count += 3;
						break;

					case NumberSize::kRGB16bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo16(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
						break;

					case NumberSize::kRGB32bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo32(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
						break;
					}
				}
			}
		}
		else if (direction == ScanDirection::kColBottomToTop)        // bottom to top
		{
			for (int pixel = matrix->Details.Height - 1; pixel >= 0; pixel--)
			{
				if (matrix->MatrixIgnoredLayout->Grid[pixel * matrix->Details.Width + col] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[pixel * matrix->Details.Width + col];

					if (teo.Code.RGBChangePixels && pixel_value == matrix->RGBBackground)
					{
						pixel_value = teo.Code.RGBChangeColour;
					}

					switch (teo.Code.Size)
					{
					case NumberSize::kRGB8bit:
						output = output + ColourUtility::RGBConvertToSplit(pixel_value, teo.Code, prefix, spacingchar);

						dataout.Count += 3;
						break;

					case NumberSize::kRGB16bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo16(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
						break;

					case NumberSize::kRGB32bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo32(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
						break;
					}
				}
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	DataOut ExportRowDataRGB(TheMatrix *matrix, const std::wstring prefix, ExportOptions teo, int frame, int row, const std::wstring spacingchar)
	{
		DataOut dataout;
		dataout.Count = 0;
		std::wstring output = L"";
		ScanDirection direction = teo.Code.Direction;

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

		direction = ExportUtility::UpdateDirectionRow(direction, teo.Code.Orientation, matrix->Details.Height, row);

		// ===========================================================================

		if (direction == ScanDirection::kRowLeftToRight)        // left to right
		{
			for (int pixel = 0; pixel < matrix->Details.Width; pixel++)
			{
				if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + pixel] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[row * matrix->Details.Width + pixel];

					if (teo.Code.RGBChangePixels && pixel_value == matrix->RGBBackground)
					{
						pixel_value = teo.Code.RGBChangeColour;
					}

					switch (teo.Code.Size)
					{
					case NumberSize::kRGB8bit:
						output = output + ColourUtility::RGBConvertToSplit(pixel_value, teo.Code, prefix, spacingchar);

						dataout.Count += 3;
						break;

					case NumberSize::kRGB16bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo16(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
                        break;

					case NumberSize::kRGB32bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo32(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
						break;
					}
				}
			}
		}
		else if (direction == ScanDirection::kRowRightToLeft)        // right to left
		{
			for (int pixel = matrix->Details.Width - 1; pixel >= 0; pixel--)
			{
				if (matrix->MatrixIgnoredLayout->Grid[row * matrix->Details.Width + pixel] == PixelAlive)
				{
					int pixel_value = selectedmatrix->Grid[row * matrix->Details.Width + pixel];

					if (teo.Code.RGBChangePixels && pixel_value == matrix->RGBBackground)
					{
						pixel_value = teo.Code.RGBChangeColour;
					}

					switch (teo.Code.Size)
					{
					case NumberSize::kRGB8bit:
						output = output + ColourUtility::RGBConvertToSplit(pixel_value, teo.Code, prefix, spacingchar);

						dataout.Count += 3;
						break;

					case NumberSize::kRGB16bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo16(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
						break;

					case NumberSize::kRGB32bit:
						output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, teo.NybblesFromNumberSize(), ColourUtility::RGBConvertTo32(pixel_value, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
						break;
					}
				}
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}
}
