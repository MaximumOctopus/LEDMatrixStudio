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
#include "ExportRGB3pp.h"
#include "ExportUtility.h"
#include "Formatting.h"
#include "SystemSettings.h"
#include "Utility.h"

extern SystemSettings *GSystemSettings;


namespace ExportRGB3BPP
{
	bool CreateExportAnimationRGB3BPP(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount, std::vector<std::wstring> &unique_items)
	{
		int MatrixDataCount = std::max(matrix->Details.Height, matrix->Details.Width);

		std::wstring MatrixData[MatrixDataCount];

		entrycount = 0; // total of all entries added to data variable in output

		std::wstring s = L"";
		std::wstring vartype = L"";
		std::wstring spacingstring = L"";
		std::wstring prefix = ExportUtility::GetNumberFormat(teo.Code.Language, teo.Code.Format);
		std::wstring cdescription = L"";
		DataOut dataout;

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

		// ===================================================================

		if (teo.Code.CleanMode)
		{
			spacingstring = L" ";

			teo.Code.Language = ExportLanguage::kSpecial;
		}
		else
		{
			spacingstring = L", ";
		}

		// ===================================================================

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
			}

			// ===============================================================

			ExportUtility::GetPreamble(teo, output, false, matrix->Details.Comment);

			ExportUtility::GetSpacerLine(teo.Code.Language, output);

			output.push_back(L"");
		}

		// ===================================================================
		// ===================================================================
		// ===================================================================

		vartype = ExportUtility::GetVariableType(teo.Code.Language, teo.Code.Size) +
				  ExportUtility::GetVariableID(teo.Code.Language);

		if (vartype != L"")
		{
			output.push_back(vartype);
		}

		teo.DataPadding = Formatting::PadString(L' ', vartype.length());

		// ===================================================================
		// ===================================================================

		for (int frame = teo.Code.StartFrame; frame <= teo.Code.EndFrame; frame++)
		{
			if (teo.Code.Language == ExportLanguage::kCFastLED)
			{
				output.push_back(ExportUtility::GetVariableIDFrameIn(teo.Code.Language, frame));
			}

			// ===============================================================

			for (int i = 0; i < MatrixDataCount; i++)
			{
				MatrixData[i] = L"";
			}

			if (teo.Code.Source == ReadSource::kRows)
			{
				int nybbles = std::ceil(((double)matrix->Details.Width * 3) / 4); // 3bits per pixel / 4 bits per hex character

				for (int y = teo.Code.SelectiveStart - 1; y <= teo.Code.SelectiveEnd - 1; y++)
				{
					dataout = ExportRowDataRGB3BPP(matrix, prefix, teo, frame, y, spacingstring, nybbles);

					MatrixData[y] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}

			if (teo.Code.Source == ReadSource::kColumns)
			{
				int nybbles = std::ceil(((double)matrix->Details.Height * 3) / 4); // 3bits per pixel / 4 bits per hex character

				for (int x = teo.Code.SelectiveStart - 1; x < teo.Code.SelectiveEnd - 1; x++)
				{
					dataout = ExportColumnDataRGB3BPP(matrix, prefix, teo, frame, x, spacingstring, nybbles);

					MatrixData[x] = baaProcessUnique(dataout.Data[0]);

					entrycount += dataout.Count;
				}
			}

			// ===============================================================
			// row data
			// ===============================================================

			if (teo.Code.Source == ReadSource::kRows)
			{
				s = L"";

				if (teo.Code.Orientation == InputOrientation::kTopBottomLeftRight)
				{
					for (int y = teo.Code.SelectiveStart - 1; y < teo.Code.SelectiveEnd; y++)
					{
						baaUpdateOutput(MatrixData[y]);
					}
				}
				else
				{
					for (int y = teo.Code.SelectiveEnd - 1; y >= teo.Code.SelectiveStart - 1; y--)
					{
						baaUpdateOutput(MatrixData[y]);
					}
				}

				if (teo.Code.Content == LineContent::kFrame)
				{
					ExportUtility::AddContentByFrame(teo, s, frame, output);
				}
			}

			// ===============================================================
			// col data
			// ===============================================================

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
						for (int x = teo.Code.SelectiveStart - 1; x < teo.Code.SelectiveEnd ; x++)
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
					s = GLanguageHandler->Text[kSure24x16BoardNotAvailableInRGBMode]; // sure 2416 not available in RGB (it's only mono)!!
					break;
				}

				if (teo.Code.Language == ExportLanguage::kCFastLED)
				{
					output.push_back(ExportUtility::GetVariableIDFrameOut(teo.Code.Language));

					output.push_back(L"");
				}
			}
		}

		ExportUtility::AddEnding(output, teo);

		if (teo.Code.IncludePreamble)
		{
			ExportUtility::GetSpacerLine(teo.Code.Language, output);
		}

        return true;
	}


	DataOut ExportColumnDataRGB3BPP(TheMatrix *matrix, const std::wstring prefix, ExportOptions teo, int frame, int col, const std::wstring spacingchar, int nybbles)
	{
		DataOut dataout;
		dataout.Count = 0;
		std::wstring output = L"";
		ScanDirection direction = teo.Code.Direction;

		MatrixGrid *selectedmatrix;

		BitCounting bc;

		auto baaBitStream = [&output, &teo, prefix, spacingchar](BitCounting &bc, int pixel, int test) -> void
		{
			unsigned _int64 p = kPowers[bc.highbit - bc.bitcounter];

			if (teo.Code.LSB == LeastSignificantBit::kTopLeft)
			{
				p = kPowers[bc.bitcounter];
			}

			if ((pixel & test) == test)
			{
				bc.databyte += p;
			}

			if (bc.Next())
			{
				output += ColourUtility::RGB3BPPFormatOutput(bc.databyte, teo.Code, prefix, spacingchar, 8);

				bc.Reset();
			}
		};

		// ===================================================================

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

		// ===================================================================

		direction = ExportUtility::UpdateDirectionColumn(direction, teo.Code.Orientation, matrix->Details.Width, col);

		// ===================================================================

		if (direction == ScanDirection::kColTopToBottom)             // top to bottom
		{
			bc.SetDirection(_BitCountDirectionDown, 31);

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
		else if (direction == ScanDirection::kColBottomToTop)        // bottom to top
		{
			bc.SetDirection(_BitCountDirectionUp, 31);

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

		if (!bc.IsStartingPosition())
		{
			output += ColourUtility::RGB3BPPFormatOutput(bc.databyte, teo.Code, prefix, spacingchar, 8);

			bc.outputcount++;
		}

		// ===================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	DataOut ExportRowDataRGB3BPP(TheMatrix *matrix, const std::wstring prefix, ExportOptions teo, int frame, int row, const std::wstring spacingchar, int nybbles)
	{
		DataOut dataout;
		dataout.Clear();
		std::wstring output = L"";
		ScanDirection direction = teo.Code.Direction;

		MatrixGrid *selectedmatrix;

		BitCounting bc;

		auto baaBitStream = [&output, &teo, prefix, spacingchar](BitCounting &bc, int pixel, int test) -> void
		{
			unsigned _int64 p = kPowers[bc.highbit - bc.bitcounter];

			if (teo.Code.LSB == LeastSignificantBit::kTopLeft)
			{
				p = kPowers[bc.bitcounter];
			}

			if ((pixel & test) == test)
			{
				bc.databyte += p;
			}

			if (bc.Next())
			{
				output += ColourUtility::RGB3BPPFormatOutput(bc.databyte, teo.Code, prefix, spacingchar, 8);

				bc.Reset();
			}
		};

		// ===================================================================

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

		// ===================================================================

		direction = ExportUtility::UpdateDirectionRow(direction, teo.Code.Orientation, matrix->Details.Height, row);

		// ===================================================================

		if (direction == ScanDirection::kRowLeftToRight)
		{
			bc.SetDirection(_BitCountDirectionDown, 31);

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
		else if (direction == ScanDirection::kRowRightToLeft)
		{
			bc.SetDirection(_BitCountDirectionUp, 31);

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

		if (!bc.IsStartingPosition())
		{
			output += ColourUtility::RGB3BPPFormatOutput(bc.databyte, teo.Code, prefix, spacingchar, 8);

			bc.outputcount++;
		}

		dataout.Count++;

		// ===================================================================

		dataout.Data[0] = output;

		return dataout;
	}


	bool CreateExportFreeformRGB3BPP(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount, std::vector<std::wstring> &unique_items)
	{
		int MatrixDataCount = matrix->MatrixLayers[0]->Freeform->Pixels.size();

		std::wstring MatrixData[MatrixDataCount];

		entrycount = 0; // total of all entries added to data variable in output

		std::wstring s = L"";
		std::wstring vartype = L"";
		std::wstring spacingstring = L"";
		std::wstring prefix = ExportUtility::GetNumberFormat(teo.Code.Language, teo.Code.Format);
		std::wstring cdescription = L"";
		DataOut dataout;

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
					s += data;
					break;
			}
		};

		// ===================================================================

		if (teo.Code.CleanMode)
		{
			spacingstring = L" ";

			teo.Code.Language = ExportLanguage::kSpecial;
		}
		else
		{
			spacingstring = L", ";
		}

		// ===================================================================

		if (teo.Code.IncludePreamble)
		{
			switch (teo.ExportMode)
			{
			case ExportSource::kAnimationFreeform:
				cdescription = GLanguageHandler->Text[kFrame];
				break;
			case ExportSource::kUserMemoriesFreeform:
				cdescription = GLanguageHandler->Text[kMemory];
                break;
			}

			// ===============================================================

			ExportUtility::GetPreamble(teo, output, false, matrix->Details.Comment);

			ExportUtility::GetSpacerLine(teo.Code.Language, output);

			output.push_back(L"");
		}

		// ===================================================================
		// ===================================================================
		// ===================================================================

		vartype = ExportUtility::GetVariableType(teo.Code.Language, teo.Code.Size) +
				  ExportUtility::GetVariableID(teo.Code.Language);

		if (vartype != L"")
		{
			output.push_back(vartype);
		}

		teo.DataPadding = Formatting::PadString(L' ', vartype.length());

		// ===================================================================
		// ===================================================================

		for (int frame = teo.Code.StartFrame; frame <= teo.Code.EndFrame; frame++)
		{
			if (teo.Code.Language == ExportLanguage::kCFastLED)
			{
				output.push_back(ExportUtility::GetVariableIDFrameIn(teo.Code.Language, frame));
			}

			// ===============================================================

			for (int i = 0; i < MatrixDataCount; i++)
			{
				MatrixData[i] = L"";
			}

			int nybbles = std::ceil(((double)MatrixDataCount * 3) / 4); // 3bits per pixel / 4 bits per hex character

			dataout = ExportPixelDataRGB3BPP(matrix, prefix, teo, frame, spacingstring, nybbles);

			MatrixData[frame] = baaProcessUnique(dataout.Data[0]);

			entrycount += dataout.Count;

			if (teo.Code.Content == LineContent::kFrame)
			{
				ExportUtility::AddContentByFrame(teo, s, frame, output);
			}
		}

		ExportUtility::AddEnding(output, teo);

		if (teo.Code.IncludePreamble)
		{
			ExportUtility::GetSpacerLine(teo.Code.Language, output);
		}

        return true;
	}


	DataOut ExportPixelDataRGB3BPP(TheMatrix *matrix, const std::wstring prefix, ExportOptions teo, int frame, const std::wstring spacingchar, int nybbles)
	{
		DataOut dataout;
		dataout.Count = 0;
		std::wstring output = L"";

		std::vector<int> Colours;

		BitCounting bc;

		auto baaBitStream = [&output, &teo, prefix, spacingchar](BitCounting &bc, int pixel, int test) -> void
		{
			unsigned _int64 p = kPowers[bc.highbit - bc.bitcounter];

			if (teo.Code.LSB == LeastSignificantBit::kTopLeft)
			{
				p = kPowers[bc.bitcounter];
			}

			if ((pixel & test) == test)
			{
				bc.databyte += p;
			}

			if (bc.Next())
			{
				output += ColourUtility::RGB3BPPFormatOutput(bc.databyte, teo.Code, prefix, spacingchar, 8);

				bc.Reset();
			}
		};

		// ===================================================================

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

		// ===================================================================

		bc.SetDirection(_BitCountDirectionDown, 31);

		for (int pixel = 0; pixel < Colours.size(); pixel++)
		{
			int pixel_value = Colours[pixel];

			baaBitStream(bc, pixel_value, 4);
			baaBitStream(bc, pixel_value, 2);
			baaBitStream(bc, pixel_value, 1);
		}

		if (!bc.IsStartingPosition())
		{
			output += ColourUtility::RGB3BPPFormatOutput(bc.databyte, teo.Code, prefix, spacingchar, 8);

			bc.outputcount++;
		}

		// ===================================================================

		dataout.Data[0] = output;

		return dataout;
	}
}
