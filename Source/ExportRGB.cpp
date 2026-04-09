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
					s += data; // to do
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
			switch (teo.ExportMode)
			{
			case ExportSource::kAnimationGrid:
				cdescription = GLanguageHandler->Text[kFrame];
				break;
			case ExportSource::kUserMemoriesGrid:
				cdescription = GLanguageHandler->Text[kMemory];
				break;

			default:
                cdescription = L"ERROR!!!";
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

		for (int frame = teo.Code.StartFrame; frame <= teo.Code.EndFrame; frame++)
		{
			if (teo.Code.Language == ExportLanguage::kCFastLED)
			{
				output.push_back(ExportUtility::GetVariableIDFrameIn(teo.Code.Language, frame));
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
					dataout = ExportRowDataRGB(matrix, prefix, teo, frame, y, spacingstring);

					MatrixData[y] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}
			else if (teo.Code.Source == ReadSource::kColumns)
			{
				for (int x = teo.Code.SelectiveStart - 1; x < teo.Code.SelectiveEnd; x++)
				{
					dataout = ExportColumnDataRGB(matrix, prefix, teo, frame, x, spacingstring);

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
					ExportUtility::AddContentByFrame(teo, s, frame, output);
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
						ExportUtility::AddContentByFrame(teo, s, frame, output);
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

		MatrixGrid *selectedmatrix;

		// ===========================================================================

		switch (teo.ExportMode)
		{
		case ExportSource::kAnimationGrid:
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
			}
			break;
		case ExportSource::kUserMemoriesGrid:
			selectedmatrix = static_cast<MatrixGrid*>(matrix->MatrixUser[frame]);
            break;
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

					default:
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

					default:
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

		MatrixGrid *selectedmatrix;

		// ===========================================================================

		switch (teo.ExportMode)
		{
		case ExportSource::kAnimationGrid:
			if (matrix->MatrixLayers.size() == 1)
			{
				selectedmatrix = matrix->MatrixLayers[0]->Cells[frame];
			}
			else
			{
				matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				selectedmatrix = matrix->MatrixMerge;
			}
			break;
		case ExportSource::kUserMemoriesGrid:
			selectedmatrix = static_cast<MatrixGrid*>(matrix->MatrixUser[frame]);
            break;
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

					default:
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

					default:
						break;
					}
				}
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	bool CreateExportFreeformRGB(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount, std::vector<std::wstring> &unique_items)
	{
		int MatrixDataCount = matrix->MatrixLayers[0]->Freeform->Pixels.size();

		std::wstring MatrixData[MatrixDataCount];

		std::vector<int> Colours;
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
				case LineContent::kFrame:
					s += data;
					break;
				case LineContent::kBytes:
                    s += data; // to do
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
			if (teo.ExportMode == ExportSource::kAnimationFreeform)
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

		for (int frame = teo.Code.StartFrame; frame <= teo.Code.EndFrame; frame++)
		{
			if (teo.Code.Language == ExportLanguage::kCFastLED)
			{
				output.push_back(ExportUtility::GetVariableIDFrameIn(teo.Code.Language, frame));
			}

			// =========================================================================

			for (int i = 0; i < MatrixDataCount; i++)
			{
				MatrixData[i] = L"";
			}

			dataout = ExportPixelDataRGB(matrix, prefix, teo, frame, spacingstring);

			MatrixData[frame] = baaProcessUnique(dataout.Data[0]);

			entrycount += dataout.Count;

			s = L"";

			for (int p = 0; p < MatrixDataCount; p++)
			{
				baaUpdateOutput(MatrixData[p]);
			}

			if (teo.Code.Content == LineContent::kFrame)
			{
				ExportUtility::AddContentByFrame(teo, s, frame, output);
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


	DataOut ExportPixelDataRGB(TheMatrix *matrix, const std::wstring prefix, ExportOptions teo, int frame, const std::wstring spacingchar)
	{
		DataOut dataout;
		dataout.Count = 0;
		std::wstring output = L"";

		std::vector<int> Colours;

		// ===========================================================================

		switch (teo.ExportMode)
		{
		case ExportSource::kAnimationFreeform:
			if (matrix->MatrixLayers.size() == 1)
			{
				for (int pixel = 0; pixel < matrix->MatrixLayers[0]->Freeform->Pixels.size(); pixel++)
				{
					Colours.push_back(matrix->MatrixLayers[0]->Freeform->Pixels[pixel]->Colours[frame]);
				}
			}
			else
			{
				//matrix->BuildMergedFrame(frame, MergeFrameMode::kRetainGridValue);

				//selectedmatrix = matrix->MatrixMerge; to do
			}
			break;
		case ExportSource::kUserMemoriesFreeform:
			for (int pixel = 0; pixel < matrix->MatrixUserFF[frame].size(); pixel++)
			{
				Colours.push_back(matrix->MatrixUserFF[frame][frame]);
			}
			break;
		}

		// ===========================================================================

		for (int pixel = 0; pixel < Colours.size(); pixel++)
		{
			int pixel_value = Colours[pixel];

			if (teo.Code.RGBChangePixels && pixel_value == matrix->RGBBackground)
			{
				pixel_value = teo.Code.RGBChangeColour;
			}

			switch (teo.Code.Size)
			{
			case NumberSize::kRGB8bit:
				output += ColourUtility::RGBConvertToSplit(pixel_value, teo.Code, prefix, spacingchar);

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

			default:
				output += L" UNKNOWN Code.Size ";
				break;
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}
}
