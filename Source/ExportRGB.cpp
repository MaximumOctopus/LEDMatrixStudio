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
#include "ExportRGB.h"
#include "ExportUtility.h"
#include "SystemSettings.h"
#include "Utility.h"

extern SystemSettings *GSystemSettings;


namespace ExportRGB
{
	bool CreateExportAnimationRGB(TheMatrix *matrix, ExportOptions teo, std::vector<std::wstring> &output, int &entrycount, std::vector<std::wstring> &unique_items)
	{
		auto baaAddContentByRowCol = [teo](const std::wstring s) -> std::wstring
		{
			std::wstring m = s.substr(0, s.length()); // trims last (and unnecessary) ", " from data

			switch (teo.Code.Language)
			{
			case ExportLanguage::kCSV:
				return GSystemSettings->App.OpenBracket + m + GSystemSettings->App.CloseBracket + L";";
			case ExportLanguage::kPICAXE:
				return L"EEPROM (" + m + L")";
			case ExportLanguage::kC1Dim:
				return teo.DataPadding + s;
			case ExportLanguage::kC2Dim:
				return teo.DataPadding + L"{" + s + L"},";
			case ExportLanguage::kCFastLED:
				return teo.DataPadding + s;
			case ExportLanguage::kPython1Dim:
				return teo.DataPadding + s;
			case ExportLanguage::kPython2Dim:
				return teo.DataPadding + L"[" + s + L"],";
			case ExportLanguage::kMicrochip:
				return L"dt " + m;
			case ExportLanguage::kPascal:
				return L"matrixdata : array[0..__LEDCount] of integer = (" + m + L");";
			case ExportLanguage::kSpecial:
				return s;
			}
		};

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

		entrycount = 0; // total of all entries added to data variable in output

		std::wstring s = L"";
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

		// =========================================================================
		// =========================================================================
		// =========================================================================

		vartype = ExportUtility::GetSingleVariableStatement(teo.Code.Language, teo.Code.Size);

		if (vartype != L"")
		{
			output.push_back(vartype);
		}

		teo.DataPadding = ExportUtility::GetPadding(teo.Code.Language, vartype.length());

		// ===========================================================================
		// ===========================================================================

		for (int t = teo.Code.StartFrame; t <= teo.Code.EndFrame; t++)
		{
			if (teo.Code.Language == ExportLanguage::kCFastLED)
			{
				output.push_back(ExportUtility::GetVariableIDFrameIn(teo.Code.Language, t));
			}

			// =========================================================================

			for (int i = 0; i < std::max(matrix->Details.Height, matrix->Details.Width); i++)
				MatrixData[i] = L"";

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
				if (teo.Code.Orientation == InputOrientation::kTopBottomLeftRight)
				{
					s = L"";

					for (int y = 0; y < matrix->Details.Height; y++)
					{
						switch (teo.Code.Content)
						{
						case LineContent::kRowCol:
							if (!MatrixData[y].empty())
							{
								output.push_back(baaAddContentByRowCol(MatrixData[y]));
							}
							break;
						case LineContent::kFrame:
							s += MatrixData[y];
							break;
						}
					}

					if (teo.Code.Content == LineContent::kFrame)
					{
						ExportUtility::AddContentByFrame(teo, s, t, output);
					}
				}
				else
				{
					s = L"";

					for (int y = matrix->Details.Height - 1; y >= 0; y++)
					{
						switch (teo.Code.Content)
						{
						case LineContent::kRowCol:
							if (!MatrixData[y].empty())
							{
								output.push_back(baaAddContentByRowCol(MatrixData[y]));
							}
							break;
						case LineContent::kFrame:
							s += MatrixData[y];
							break;
						}
					}

					if (teo.Code.Content == LineContent::kFrame)
					{
						ExportUtility::AddContentByFrame(teo, s, t, output);
					}
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
					if (teo.Code.Orientation == InputOrientation::kTopBottomLeftRight)
					{
						s = L"";

						for (int x = teo.Code.SelectiveStart - 1; x < teo.Code.SelectiveEnd ; x++)
						{
							switch (teo.Code.Content)
							{
							case LineContent::kRowCol:
								if (!MatrixData[x].empty())
								{
									output.push_back(baaAddContentByRowCol(MatrixData[x]));
								}
								break;
							case LineContent::kFrame:
								s += MatrixData[x];
								break;
							}
						}

						if (teo.Code.Content == LineContent::kFrame)
						{
							ExportUtility::AddContentByFrame(teo, s, t, output);
						}
					}
					else
					{
						s = L"";

						for (int x = teo.Code.SelectiveEnd - 1; x >= teo.Code.SelectiveStart - 1; x--)
						{
							switch (teo.Code.Content)
							{
							case LineContent::kRowCol:
								if (!MatrixData[x].empty())
								{
									output.push_back(baaAddContentByRowCol(MatrixData[x]));
								}
								break;
							case LineContent::kFrame:
								s += MatrixData[x];
								break;
							}
						}

						if (teo.Code.Content == LineContent::kFrame)
						{
							ExportUtility::AddContentByFrame(teo, s, t, output);
						}
					}
					break;
				}
				case InputOrientation::kSure24x16:
					s = GLanguageHandler->Text[kSure24x16BoardNotAvailableInRGBMode]; // sure 2416 not available in RGB!!
					break;
				}
			}
		}

		// =========================================================================

		if (teo.Code.Language == ExportLanguage::kCFastLED)
		{
			output.push_back(ExportUtility::GetVariableIDFrameOut(teo.Code.Language));

			output.push_back(L"");
		}

		switch (teo.Code.Language)
		{
		case ExportLanguage::kC1Dim:
		case ExportLanguage::kC2Dim:
			output.push_back(teo.DataPadding + L"};");
			break;
		case ExportLanguage::kCFastLED:
			break;
		case ExportLanguage::kPython1Dim:
		case ExportLanguage::kPython2Dim:
			output.push_back(teo.DataPadding + L"]");
			break;
		}

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

		if (teo.Code.Orientation == InputOrientation::kTopBottomLeftRight)
		{
			switch (direction)
			{
			case  ScanDirection::kColAltDownUp:
				if (col % 2 == 0)
				{
				   direction = ScanDirection::kColTopToBottom;
				}
				else
				{
				   direction = ScanDirection::kColBottomToTop;
				}
				break;
			case ScanDirection::kColAltUpDown:
				if (col % 2 == 0)
				{
				   direction = ScanDirection::kColBottomToTop;
				}
				else
				{
				   direction = ScanDirection::kColTopToBottom;
				}
				break;
			}
		}
		else if (teo.Code.Orientation == InputOrientation::kBottomTopRightLeft)
		{
			switch (direction)
			{
			case ScanDirection::kColAltDownUp:
				if ((matrix->Details.Width - col - 1) % 2 == 0)
				{
					direction = ScanDirection::kColTopToBottom;
				}
				else
				{
					direction = ScanDirection::kColBottomToTop;
                }
				break;
			case ScanDirection::kColAltUpDown:
				if ((matrix->Details.Width - col - 1) % 2 == 0)
				{
				   direction = ScanDirection::kColBottomToTop;
				}
				else
				{
				   direction = ScanDirection::kColTopToBottom;
				}
				break;
			}
		}

		// ===========================================================================

		if (direction == ScanDirection::kColTopToBottom)             // top to bottom
		{
			for (int pixel = 0; pixel < matrix->Details.Height; pixel++)
			{
				if (matrix->MatrixDeadLayout->Grid[pixel * matrix->Details.Width + col] == PixelAlive)
				{
					if (teo.Code.Size == NumberSize::kRGB8bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[pixel * matrix->Details.Width + col] == matrix->RGBBackground)
						{
							output += ColourUtility::RGBConvertToSplit(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);
						}
						else
						{
							output += ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[pixel * matrix->Details.Width + col], teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);
						}

						dataout.Count += 3;
					}
					else if (teo.Code.Size == NumberSize::kRGB16bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[pixel * matrix->Details.Width + col] == matrix->RGBBackground)
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));
						}
						else
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(selectedmatrix->Grid[pixel * matrix->Details.Width + col], teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));
						}

						output += spacingchar;

						dataout.Count++;
					}
					else if (teo.Code.Size == NumberSize::kRGB32bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[pixel * matrix->Details.Width + col] == matrix->RGBBackground)
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));
						}
						else
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(selectedmatrix->Grid[pixel * matrix->Details.Width + col], teo.Code.RGBFormat, teo.Code.LSB,  teo.Code.RGBBrightness));
						}

						output += spacingchar;

						dataout.Count++;
					}
				}
			}
		}
		else if (direction == ScanDirection::kColBottomToTop)        // bottom to top
		{
			for (int pixel = matrix->Details.Height - 1; pixel >= 0; pixel--)
			{
				if (matrix->MatrixDeadLayout->Grid[pixel * matrix->Details.Width + col] == PixelAlive)
				{
					if (teo.Code.Size == NumberSize::kRGB8bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[pixel * matrix->Details.Width + col] == matrix->RGBBackground)
							output += ColourUtility::RGBConvertToSplit(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);
						else
							output += ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[pixel * matrix->Details.Width + col], teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);

						dataout.Count += 3;
					}
					else if (teo.Code.Size == NumberSize::kRGB16bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[pixel * matrix->Details.Width + col] == matrix->RGBBackground)
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));
						else
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(selectedmatrix->Grid[pixel * matrix->Details.Width + col], teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));

						output += spacingchar;

						dataout.Count++;
					}
					else if (teo.Code.Size == NumberSize::kRGB32bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[pixel * matrix->Details.Width + col] == matrix->RGBBackground)
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));
						else
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(selectedmatrix->Grid[pixel * matrix->Details.Width + col], teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));

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

		if (teo.Code.Orientation == InputOrientation::kTopBottomLeftRight)
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
		else if (teo.Code.Orientation == InputOrientation::kBottomTopRightLeft)
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
		else
		{
//			MessageDlg('Error, unknown orientation ' + IntToStr(Ord(teo.Orientation)), mtError, [mbOK], 0);
		}

		// ===========================================================================

		if (direction == ScanDirection::kRowLeftToRight)        // left to right
		{
			for (int pixel = 0; pixel < matrix->Details.Width; pixel++)
			{
				if (matrix->MatrixDeadLayout->Grid[row * matrix->Details.Width + pixel] == PixelAlive)
				{
					if (teo.Code.Size == NumberSize::kRGB8bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + pixel] == matrix->RGBBackground)
						{
							output += ColourUtility::RGBConvertToSplit(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);
						}
						else
						{
							output += ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[row * matrix->Details.Width + pixel], teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);
						}

						dataout.Count += 3;
					}
					else if (teo.Code.Size == NumberSize::kRGB16bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + pixel] == matrix->RGBBackground)
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));
						}
						else
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(selectedmatrix->Grid[row * matrix->Details.Width + pixel], teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));
						}

						output += spacingchar;

						dataout.Count++;
					}
					else if (teo.Code.Size == NumberSize::kRGB32bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + pixel] == matrix->RGBBackground)
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));
						}
						else
						{
							output += prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(selectedmatrix->Grid[row * matrix->Details.Width + pixel], teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));
						}

						output += spacingchar;

						dataout.Count++;
					}
				}
			}
		}
		else if (direction == ScanDirection::kRowRightToLeft)        // right to left
		{
			for (int pixel = matrix->Details.Width - 1; pixel >= 0; pixel--)
			{
				if (matrix->MatrixDeadLayout->Grid[row * matrix->Details.Width + pixel] == PixelAlive)
				{
					if (teo.Code.Size == NumberSize::kRGB8bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + pixel] == matrix->RGBBackground)
						{
							output = output + ColourUtility::RGBConvertToSplit(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);
						}
						else
						{
							output = output + ColourUtility::RGBConvertToSplit(selectedmatrix->Grid[row * matrix->Details.Width + pixel], teo.Code.RGBFormat, teo.Code.RGBBrightness, teo.Code.Format, prefix, spacingchar, teo.Code.ColourSpaceRGB);
						}

						dataout.Count += 3;
					}
					else if (teo.Code.Size == NumberSize::kRGB16bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + pixel] == matrix->RGBBackground)
						{
							output = output + prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));
						}
						else
						{
							output = output + prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 4, ColourUtility::RGBConvertTo16(selectedmatrix->Grid[row * matrix->Details.Width + pixel], teo.Code.RGBFormat, teo.Code.LSB, teo.Code.ColourSpaceRGB, teo.Code.RGBBrightness));
						}

						output = output + spacingchar;

						dataout.Count++;
					}
					else if (teo.Code.Size == NumberSize::kRGB32bit)
					{
						if (teo.Code.RGBChangePixels && selectedmatrix->Grid[row * matrix->Details.Width + pixel] == matrix->RGBBackground)
						{
							output = output + prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(teo.Code.RGBChangeColour, teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));
						}
						else
						{
							output = output + prefix + ColourUtility::RGBColourNumberFormat(teo.Code.Format, 8, ColourUtility::RGBConvertTo32(selectedmatrix->Grid[row * matrix->Details.Width + pixel], teo.Code.RGBFormat, teo.Code.LSB, teo.Code.RGBBrightness));
						}

						output = output + spacingchar;

						dataout.Count++;
					}
				}
			}
		}

		// ===========================================================================

		dataout.Data[0] = output;

		return dataout;
	}
}
