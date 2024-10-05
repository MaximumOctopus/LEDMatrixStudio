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

#include "Convert.h"
#include "DateUtility.h"
#include "ExportUtility.h"
#include "Formatting.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "SystemSettings.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;
extern SystemSettings *GSystemSettings;


namespace ExportUtility
{
	std::wstring TitleWithComments(const std::wstring title, ExportLanguage language, bool includecomment)
	{
		if (includecomment)
		{
			return GetCommentCharacter(language) + title;
		}

		return L"";
	}


	void AddContentByFrame(ExportOptions teo, const std::wstring s, int frame, std::vector<std::wstring> &output)
	{
		if (s.empty()) return;

		std::wstring m = s.substr(0, s.length() - 2); // trims last (and unnecessary) ", " from data

		if (teo.FontMode)
		{
			wchar_t chr(GSystemSettings->App.ASCIIIndex + frame + teo.Code.StartFrame);
			std::wstring ascii = std::to_wstring(GSystemSettings->App.ASCIIIndex + frame + teo.Code.StartFrame);

			switch (teo.Code.Language)
			{
			case ExportLanguage::kCSV:
				output.push_back(GSystemSettings->App.OpenBracket + m + GSystemSettings->App.CloseBracket + L";  // " + chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kPICAXE:
				output.push_back(L"EEPROM (" + m + L")  ; " + chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kC1Dim:
				output.push_back(teo.DataPadding + m + L"  // " + chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kC2Dim:
				output.push_back(teo.DataPadding + L"{" + m + L"},  // " + chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kCFastLED:
				output.push_back(teo.DataPadding + s + L"  // " + chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kPython1Dim:
				output.push_back(teo.DataPadding + s + L"  # " + chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kPython2Dim:
				output.push_back(teo.DataPadding + L"[" + m + L"],  # " + chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kMicrochip:
				output.push_back(L"dt " + m + L" ; " +chr + L" ASCII " + ascii);
				break;
			case ExportLanguage::kPascal:
				output.push_back(L"matrixdata : array[0..__LEDCount] of integer = (" + m + L");");
				break;
			case ExportLanguage::kSpecial:
				output.push_back(s);
				break;
			}
		}
		else
		{
			switch (teo.Code.Language)
			{
			case ExportLanguage::kCSV:
				output.push_back(GSystemSettings->App.OpenBracket + m + GSystemSettings->App.CloseBracket + L";  // " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kPICAXE:
				output.push_back(L"EEPROM (" + m + L")  ; " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kC1Dim:
				output.push_back(teo.DataPadding + m + L"  // " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kC2Dim:
				output.push_back(teo.DataPadding + L"{" + m + L"},  // " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kCFastLED:
				output.push_back(teo.DataPadding + m + L"  // " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kPython1Dim:
				output.push_back(teo.DataPadding + s + L"  # " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kPython2Dim:
				output.push_back(teo.DataPadding + L"[" + m + L"],  # " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kMicrochip:
				output.push_back(L"dt " + m + L" ; " + teo.Description + L" " + std::to_wstring(frame));
				break;
			case ExportLanguage::kPascal:
				output.push_back(L"matrixdata : array[0..__LEDCount] of integer = (" + m + L");");
				break;
			case ExportLanguage::kSpecial:
				output.push_back(s);
				break;
			}
		}
	}


	void AddRowColContent(ExportOptions teo, const std::wstring s, std::vector<std::wstring> &output)
	{
		if (s.empty()) return;

		std::wstring m = s.substr(0, s.length() - 2); // trims last (and unnecessary) ", " from data

		switch (teo.Code.Language)
		{
		case ExportLanguage::kCSV:
			output.push_back(GSystemSettings->App.OpenBracket + m + GSystemSettings->App.CloseBracket + L';');
			break;
		case ExportLanguage::kPICAXE:
			output.push_back(L"'EEPROM (" + m + L")");
			break;
		case ExportLanguage::kC1Dim:
			output.push_back(teo.DataPadding + s);
			break;
		case ExportLanguage::kC2Dim:
			output.push_back(teo.DataPadding + L"{" + m + L"},");
			break;
		case ExportLanguage::kCFastLED:
			output.push_back(teo.DataPadding + s);
			break;
		case ExportLanguage::kPython1Dim:
			output.push_back(teo.DataPadding + s);
			break;
		case ExportLanguage::kPython2Dim:
			output.push_back(teo.DataPadding + L"[" + m + L"],");
			break;
		case ExportLanguage::kMicrochip:
			output.push_back(L"dt " + m);
			break;
		case ExportLanguage::kPascal:
			output.push_back(L"matrixdata : array[0..__LEDCount] of integer = (" + m + L");");
			break;
		case ExportLanguage::kSpecial:
			output.push_back(s);
			break;
		}
	}


	std::wstring GetRowData(int *grid, int width, bool hexmode, int direction, int frame, int row)
	{
		unsigned __int64 total = 0;

		for (int column = 0; column < width; column++)
		{
			if (grid[row * width + column] == 1)
			{
				if (direction == 0)
				{
					total += (powers[column]);
				}
				else
				{
					total += (powers[width - column]);
				}
			}
		}

		if (hexmode)
		{
			return IntToHex(total, GSystemSettings->App.PadModeHexRow).c_str();
		}
		else
		{
			return std::to_wstring(total);
		}
	}


	std::wstring GetColumnData(int *grid, int height, int width, bool hexmode, int direction, int frame, int col)
	{
		unsigned __int64 total = 0;

		for (int row = 0; row < height; row++)
		{
			if (grid[row * width + col] == 1)
			{
				if (direction == 0)
				{
					total += (powers[row]);
				}
				else
				{
					total += (powers[height - row]);
				}
			}
		}

		if (hexmode)
		{
			return IntToHex(total, GSystemSettings->App.PadModeHexCol).c_str();
		}
		else
		{
			return std::to_wstring(total);
		}
	}


	std::wstring GetCommentCharacter(ExportLanguage language)
	{
		switch (language)
		{
		case ExportLanguage::kCSV:
			return L"// ";
		case ExportLanguage::kPICAXE:
			return L"; ";
		case ExportLanguage::kC1Dim:
		case ExportLanguage::kC2Dim:
		case ExportLanguage::kCFastLED:
			return L"// ";
		case ExportLanguage::kPython1Dim:
		case ExportLanguage::kPython2Dim:
			return L"# ";
		case ExportLanguage::kMicrochip:
			return L"; ";
		case ExportLanguage::kPascal:
			return L"// ";

		default:
			return L"";
		}
	}


	std::wstring GetLanguage(ExportLanguage language, bool includecomment)
	{
		std::wstring cc = TitleWithComments(GLanguageHandler->Text[kLanguage] + L" : ", language, includecomment);

		switch (language)
		{
		case ExportLanguage::kCSV:
			return cc + GLanguageHandler->Text[kExportCommaSeparated];
		case ExportLanguage::kPICAXE:
			return cc + GLanguageHandler->Text[kExportPICAXEEEPROM];
		case ExportLanguage::kC1Dim:
			return cc + GLanguageHandler->Text[kExportCCpp1Dimensional];
		case ExportLanguage::kC2Dim:
			return cc + GLanguageHandler->Text[kExportCCpp2Dimensional];
		case ExportLanguage::kCFastLED:
			return cc + GLanguageHandler->Text[kExportCCppFastLED];
		case ExportLanguage::kPython1Dim:
			return cc + GLanguageHandler->Text[kExportPython1Dimensional];
		case ExportLanguage::kPython2Dim:
			return cc + GLanguageHandler->Text[kExportPython2Dimensional];
		case ExportLanguage::kMicrochip:
			return cc + GLanguageHandler->Text[kExportMicrochip];
		case ExportLanguage::kPascal:
			return cc + GLanguageHandler->Text[kExportPascal];

		default:
			return cc + L"UNKNOWN!!";
		}
	}


	std::wstring GetLineContent(ExportOptions teo, bool includecomment)
	{
		std::wstring cc = TitleWithComments(L"Line   : ", teo.Code.Language, includecomment);

		switch (teo.Code.Content)
		{
		case LineContent::kRowCol:
		{
			switch (teo.Code.Source)
			{
			case ReadSource::kColumns:
				return cc + GLanguageHandler->Text[kColumn];
			case ReadSource::kRows:
				return cc + GLanguageHandler->Text[kRow];
			}
			break;
		}
		case LineContent::kFrame:
			return cc + GLanguageHandler->Text[kAnimationFrame];
		case LineContent::kBytes:
			return cc + std::to_wstring(teo.Code.LineCount) + L" " + GLanguageHandler->Text[kBytes];

		default:
			return L"unknown!!!";
		}
	}


	std::wstring GetLSB(ExportOptions teo, bool includecomment)
	{
		std::wstring cc = TitleWithComments(L"Bits   : ", teo.Code.Language, includecomment);

		switch (teo.Code.Source)
		{
		case ReadSource::kColumns:
		{
			switch (teo.Code.LSB)
			{
			case LeastSignificantBit::kTopLeft:
				return cc + GLanguageHandler->Text[kLSBAtTop];
			case LeastSignificantBit::kBottomRight:
				return cc + GLanguageHandler->Text[kLSBAtBottom];
			}
			break;
		}
		case ReadSource::kRows:
		{
			switch (teo.Code.LSB)
			{
			case LeastSignificantBit::kTopLeft:
				return cc + GLanguageHandler->Text[kLSBAtLeft];
			case LeastSignificantBit::kBottomRight:
				return cc + GLanguageHandler->Text[kLSBAtRight];
			}
			break;
		}

		default:
			return L"unknown";
		}
	}


	std::wstring GetNumberFormat(ExportLanguage language, NumberFormat format)
	{
		switch (language)
		{
		case ExportLanguage::kCSV:
		{
			switch (format)
			{
			case NumberFormat::kDecimal:
				break;
			case NumberFormat::kBinary:
				return L"%";
			case NumberFormat::kHex:
				return L"$";
			}
			break;
		}
		case ExportLanguage::kPICAXE:
		{
			switch (format)
			{
			case NumberFormat::kDecimal:
				break;
			case NumberFormat::kBinary:
				return L"%";
			case NumberFormat::kHex:
				return L"$";
			}
			break;
		}
		case ExportLanguage::kC1Dim:
		case ExportLanguage::kC2Dim:
		{
			switch (format)
			{
			case NumberFormat::kDecimal:
				break;
			case NumberFormat::kBinary:
				return L"0B";
			case NumberFormat::kHex:
				return L"0x";
			}
			break;
		}
		case ExportLanguage::kCFastLED:
		{
			switch (format)
			{
			case NumberFormat::kDecimal:
				break;
			case NumberFormat::kBinary:
				return L"0B";
			case NumberFormat::kHex:
				return L"0x";
			}
			break;
		}
		case ExportLanguage::kPython1Dim:
		case ExportLanguage::kPython2Dim:
		{
			switch (format)
			{
			case NumberFormat::kDecimal:
				break;
			case NumberFormat::kBinary:
				return L"0B";
			case NumberFormat::kHex:
				return L"0x";
			}
			break;
		}
		case ExportLanguage::kMicrochip:
		{
			switch (format)
			{
			case NumberFormat::kDecimal:
				break;
			case NumberFormat::kBinary:
				return L"%";
			case NumberFormat::kHex:
				return L"0x";
			}
			break;
		}
		case ExportLanguage::kPascal:
		{
			switch (format)
			{
			case NumberFormat::kDecimal:
				break;
			case NumberFormat::kBinary:
				return L"%";
			case NumberFormat::kHex:
				return L"$";
			}
			break;
		}

		default:
			return L"unknown";
		}

		return L"";
	}


	std::wstring GetNumberSize(ExportLanguage language, NumberSize size, bool includecomment)
	{
		std::wstring cc = TitleWithComments(L"Size   : ", language, includecomment);

		switch (size)
		{
		case NumberSize::k8Bit:
			return cc + GLanguageHandler->Text[k8Bits];
		case NumberSize::k16bit:
			return cc + GLanguageHandler->Text[k16Bits];
		case NumberSize::k32bit:
			return cc + GLanguageHandler->Text[k32Bits];
		case NumberSize::k8bitSwap:
			return cc + GLanguageHandler->Text[k8BitsNybblesSwapped];
		case NumberSize::k16bitSwap:
			return cc + GLanguageHandler->Text[k16BitsBytesSwapped];
		case NumberSize::k64bit:
			return cc + GLanguageHandler->Text[k64Bits];
		case NumberSize::kRGB8bit:
			return cc + GLanguageHandler->Text[k8Bits];
		case NumberSize::kRGB32bit:
			return cc + GLanguageHandler->Text[k32Bits];

		default:
			return L"";
		}
	}


	std::wstring GetOrientation(ExportOptions teo, bool includecomment)
	{
		std::wstring cc = TitleWithComments(L"Order  : ", teo.Code.Language, includecomment);

		switch (teo.Code.Source)
		{
		case ReadSource::kColumns:
		{
			switch (teo.Code.Orientation)
			{
			case InputOrientation::kTopBottomLeftRight:
				return cc + GLanguageHandler->Text[kLeftToRight];
			case InputOrientation::kBottomTopRightLeft:
				return cc + GLanguageHandler->Text[kRightToLeft];
			case InputOrientation::kSure24x16:
				return cc + GLanguageHandler->Text[kSure24x16];

			default:
				return L"";
			}
			break;
		}
		case ReadSource::kRows:
		{
			switch (teo.Code.Orientation)
			{
			case InputOrientation::kTopBottomLeftRight:
				return cc + GLanguageHandler->Text[kTopToBottom];
			case InputOrientation::kBottomTopRightLeft:
				return cc + GLanguageHandler->Text[kBottomToTop];

			default:
				return L"";
			}
            break;
		}
		}
	}


	void GetPreamble(ExportOptions teo, std::vector<std::wstring> &output, bool simple, const std::wstring comment)
	{
		std::wstring cc = GetCommentCharacter(teo.Code.Language);

		output.push_back(cc + L"=================================================================");
		output.push_back(cc + L"LED Matrix Studio - (c) Paul A Freshney 2024");
		output.push_back(cc);
		output.push_back(cc + L"https://github.com/MaximumOctopus/LEDMatrixStudio");
		output.push_back(cc);
		output.push_back(cc + DateUtility::CreatedDate());

		if (!simple)
		{
			if (!comment.empty())
			{
				output.push_back(cc);
				output.push_back(cc + L"-----------------------------------------------------------------");
				output.push_back(cc + GLanguageHandler->Text[kComment] + L":");
				output.push_back(cc + L" " + comment);
			}

			if (!GSystemSettings->App.DataFilename.empty())
			{
				output.push_back(cc);
				output.push_back(cc + L"-----------------------------------------------------------------");
				output.push_back(cc + GLanguageHandler->Text[kOriginalFile] + L":");
				output.push_back(cc + L" " + GSystemSettings->App.DataFilename);
			}
		}

		output.push_back(cc);
		output.push_back(cc + L"=================================================================");
		output.push_back(cc);

		if (!simple)
		{
			if (teo.ExportMode == ExportSource::kAnimation)
			{
				if (teo.FontMode)
				{
					output.push_back(cc + GLanguageHandler->Text[kFontCharacters] + L" " + std::to_wstring(teo.Code.StartFrame + 1) + L" " + GLanguageHandler->Text[kTo] + L" " + std::to_wstring(teo.Code.StartFrame + 95));
				}
				else
				{
					if (teo.Code.StartFrame == teo.Code.EndFrame)
					{
						output.push_back(cc + GLanguageHandler->Text[kAnimationFrame] + L" #" + std::to_wstring(teo.Code.StartFrame + 1));
					}
					else
					{
						output.push_back(cc + GLanguageHandler->Text[kAnimationFrame] + L" #" + std::to_wstring(teo.Code.StartFrame + 1) + L" " + GLanguageHandler->Text[kTo] + L" #" + std::to_wstring(teo.Code.EndFrame + 1));
					}

					if (teo.Code.Source == ReadSource::kRows)
					{
						output.push_back(cc + GLanguageHandler->Text[kRows] + L" #" + std::to_wstring(teo.Code.SelectiveStart) + L" - #" + std::to_wstring(teo.Code.SelectiveEnd));
					}
					else
					{
						output.push_back(cc + GLanguageHandler->Text[kColumns] + L" #L" + std::to_wstring(teo.Code.SelectiveStart) + L" - #" + std::to_wstring(teo.Code.SelectiveEnd));
					}
				}
			}
		}
		else
		{
			output.push_back(cc + GLanguageHandler->Text[kMemoryBuffers] + L" #" + std::to_wstring(teo.Code.StartFrame + 1) + L" " + GLanguageHandler->Text[kTo] + L" #" + std::to_wstring(teo.Code.EndFrame + 1));
		}

		output.push_back(cc);
		output.push_back(cc + L"=================================================================");
		output.push_back(cc);

		output.push_back(GetSource(teo.Code.Language, teo.Code.Source));
		output.push_back(GetLineContent(teo, true));
		output.push_back(GetLSB(teo, true));
		output.push_back(GetOrientation(teo, true));
		output.push_back(GetScanDirection(teo, true));

		if (teo.Code.RGBEnabled)
		{
	        output.push_back(GetNumberSize(teo.Code.Language, teo.Code.Size, true));
			output.push_back(cc);
			output.push_back(GetRGBMode(teo, true));
			output.push_back(GetRGBBrightness(teo, true));
			output.push_back(GetColourSpace(teo, true));
		}
		else
		{
			output.push_back(GetNumberSize(teo.Code.Language, teo.Code.Size, true));
		}
	}


	std::wstring GetExampleCodeDisclaimer(ExportOptions teo)
	{
		std::wstring cc = GetCommentCharacter(teo.Code.Language);

		std::wstring s = L"\n";
		s += cc + L"=================================================================\n";
		s += cc + L"== Example code, use as a template, may not function 100%  ======\n";
		s += cc + L"=================================================================\n";
		s += L"\n";

		return s;
	}


	std::wstring GetRGBMode(ExportOptions teo, bool includecomment)
	{
		std::wstring cc = TitleWithComments(L"Colour Format: ", teo.Code.Language, includecomment);

		switch (teo.Code.RGBFormat)
		{
		case RGBMode::kRGB:
			return cc + L"RGB";
		case RGBMode::kBGR:
			return cc + L"BGR";
		case RGBMode::kGRB:
			return cc + L"GRB";
		case RGBMode::kBRG:
			return cc + L"BRG";
		}

		return L"";
	}


	std::wstring GetRGBBrightness(ExportOptions teo, bool includecomment)
	{
		std::wstring cc = TitleWithComments(L"Brightness   : ", teo.Code.Language, includecomment);

		if (teo.Code.RGBBrightness <= 0)
		{
			return cc + std::to_wstring(teo.Code.RGBBrightness) + L"% - " + GLanguageHandler->Text[kAreYouSure];
		}

		return cc + std::to_wstring(teo.Code.RGBBrightness) + L"%";
	}


	std::wstring GetColourSpace(ExportOptions teo, bool includecomment)
	{
		if (teo.Code.ColourSpaceRGB == ColourSpace::kRGB32)
		{
			return TitleWithComments(L"Colour Space : 32 bits", teo.Code.Language, includecomment);
		}

		return TitleWithComments(L"Colour Space : 5/6/5", teo.Code.Language, includecomment);
	}


	std::wstring GetScanDirection(ExportOptions teo, bool includecomment)
	{
		std::wstring cc = TitleWithComments(L"Scan   : ", teo.Code.Language, includecomment);

		switch (teo.Code.Source)
		{
		case ReadSource::kColumns:
		{
			switch (teo.Code.Direction)
			{
			case ScanDirection::kColTopToBottom:
				return cc + GLanguageHandler->Text[kTopToBottom];
			case ScanDirection::kColBottomToTop:
				return cc + GLanguageHandler->Text[kBottomToTop];
			case ScanDirection::kColAltDownUp:
				return cc + GLanguageHandler->Text[kAlternateDownUp];
			case ScanDirection::kColAltUpDown:
				return cc + GLanguageHandler->Text[kAlternateUpDown];

			default:
				return L"";
			}
			break;
		}
		case ReadSource::kRows:
		{
			switch (teo.Code.Direction)
			{
			case ScanDirection::kRowLeftToRight:
				return cc + GLanguageHandler->Text[kLeftToRight];
			case ScanDirection::kRowRightToLeft:
				return cc + GLanguageHandler->Text[kRightToLeft];
			case ScanDirection::kRowAltLeftRight:
				return cc + GLanguageHandler->Text[kAlternateLeftRight];
			case ScanDirection::kRowAltRightLeft:
				return cc + GLanguageHandler->Text[kAlternateRightLeft];

			default:
				return L"";
			}
			break;
		}
        }
	}


	std::wstring GetSource(ExportLanguage language, ReadSource savetype)
	{
		std::wstring cc = GetCommentCharacter(language) + L"Source : ";

		switch (savetype)
		{
		case ReadSource::kColumns:
			return cc + GLanguageHandler->Text[kColumns];
		case ReadSource::kRows:
			return cc + GLanguageHandler->Text[kRows];
		}

        return cc + L"unknown";
	}


	void GetSpacerLine(ExportLanguage language, std::vector<std::wstring> &output)
	{
		std::wstring cc = GetCommentCharacter(language);

		output.push_back(cc);
		output.push_back(cc + L"=================================================================");
		output.push_back(cc);
	}


	// only used when each line of data output includes a variable definition
	// OR
	// when there is only a single variable definition of the entire output

	std::wstring GetSingleVariableStatement(ExportLanguage language, NumberSize size)
	{
		return GetVariableType(language, size) + GetVariableID(language);
	}


	std::wstring GetVariableID(ExportLanguage language)
	{
		switch (language)
		{
		case ExportLanguage::kCSV:
		case ExportLanguage::kPICAXE:
			return L"";
		case ExportLanguage::kC1Dim:
			return L"ledarray[] = {";
		case ExportLanguage::kC2Dim:
			return L"ledarray[][] = {";
		case ExportLanguage::kCFastLED:
			return L"";
		case ExportLanguage::kPython1Dim:
			return L"ledarray[] = [";
		case ExportLanguage::kPython2Dim:
			return L"ledarray[][] = [";
		case ExportLanguage::kMicrochip:
			return L"";
		case ExportLanguage::kPascal:
			return L"";
		case ExportLanguage::kSpecial:
			return L"";

		default:
			return L"";
		}
	}


	std::wstring GetVariableIDFrameIn(ExportLanguage language, int frame)
	{
		switch (language)
		{
		case ExportLanguage::kC1Dim:
		case ExportLanguage::kCFastLED:
			return L"const uint32_t ledarray"  + std::to_wstring(frame) + L"[] PROGMEM = {";

		default:
			return L"";
		}
	}


	std::wstring GetVariableIDFrameOut(ExportLanguage language)
	{
		switch (language)
		{
		case ExportLanguage::kC1Dim:
		case ExportLanguage::kCFastLED:
			return L"};";

		default:
			return L"";
		}
	}


	std::wstring GetVariableType(ExportLanguage language, NumberSize size)
	{
		switch (language)
		{
		case ExportLanguage::kCSV:
		case ExportLanguage::kPICAXE:
			return L"";
		case ExportLanguage::kC1Dim:
		case ExportLanguage::kC2Dim:
		{
			switch (size)
			{
			case NumberSize::k8Bit:
				return L"byte ";
			case NumberSize::k16bit:
				return L"word ";
			case NumberSize::k32bit:
				return L"long ";
			case NumberSize::k8bitSwap:
				return L"byte ";
			case NumberSize::k16bitSwap:
				return L"word ";
			case NumberSize::k64bit:
				return L"uint64_t ";
			case NumberSize::kRGB8bit:
				return L"byte ";
			case NumberSize::kRGB16bit:
				return L"word ";
			case NumberSize::kRGB32bit:
				return L"long ";
			}
			break;
		}
		case ExportLanguage::kCFastLED:
			break;
		case ExportLanguage::kPython1Dim:
		case ExportLanguage::kPython2Dim:
			return L"";
		case ExportLanguage::kMicrochip:
			return L"";
		case ExportLanguage::kPascal:
			return L"";
		case ExportLanguage::kSpecial:
			return L"";

		default:
		  return L"";
		}

		return L"";
	}

	void AddEnding(std::vector<std::wstring> &output, ExportOptions &teo)
	{
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
    }

	std::wstring GetPadding(ExportLanguage language, int variable_definition)
	{
		if (variable_definition != 0)
		{
			return Formatting::PadString(L' ', variable_definition);
		}
		else
		{
			return L"\t\t\t\t"; // four tabs, customisable soon...
		}
	}

	std::wstring FormatDataAs(const unsigned __int64 input, NumberFormat nf, int bits, int pads)
	{
		switch (nf)
		{
			case NumberFormat::kDecimal:
				return std::to_wstring(input);
			case NumberFormat::kBinary:
				return Convert::IntegerToBinary(bits, input);
			case NumberFormat::kHex:
				return IntToHex(input, pads).c_str();
		}

		return L"";
	}

	ScanDirection UpdateDirectionColumn(ScanDirection direction, InputOrientation io, int width, int col)
	{
		if (io == InputOrientation::kTopBottomLeftRight)
		{
			switch (direction)
			{
			case  ScanDirection::kColAltDownUp:
				if (col % 2 == 0)
				{
					return ScanDirection::kColTopToBottom;
				}
				else
				{
					return ScanDirection::kColBottomToTop;
				}
				break;
			case ScanDirection::kColAltUpDown:
				if (col % 2 == 0)
				{
					return ScanDirection::kColBottomToTop;
				}
				else
				{
					return ScanDirection::kColTopToBottom;
				}
				break;
			}
		}
		else if (io == InputOrientation::kBottomTopRightLeft)
		{
			switch (direction)
			{
			case ScanDirection::kColAltDownUp:
				if ((width - col - 1) % 2 == 0)
				{
					return ScanDirection::kColTopToBottom;
				}
				else
				{
					return ScanDirection::kColBottomToTop;
				}
				break;
			case ScanDirection::kColAltUpDown:
				if ((width - col - 1) % 2 == 0)
				{
				   return ScanDirection::kColBottomToTop;
				}
				else
				{
				   return ScanDirection::kColTopToBottom;
				}
				break;
			}
		}

		return direction;
    }

	ScanDirection UpdateDirectionRow(ScanDirection direction, InputOrientation io, int height, int row)
	{
		if (io == InputOrientation::kTopBottomLeftRight)
		{
			switch (direction)
			{
			case ScanDirection::kRowAltLeftRight:
				if (row % 2 == 0)
				{
					return ScanDirection::kRowLeftToRight;
				}
				else
				{
					return ScanDirection::kRowRightToLeft;
				}
				break;
			case ScanDirection::kRowAltRightLeft:
				if (row % 2 == 0)
				{
					return ScanDirection::kRowRightToLeft;
				}
				else
				{
					return ScanDirection::kRowLeftToRight;
                }
				break;
			}
		}
		else if (io == InputOrientation::kBottomTopRightLeft)
		{
			switch (direction)
			{
			case ScanDirection::kRowAltLeftRight:
				if ((height - row - 1) % 2 == 0)
				{
					return ScanDirection::kRowLeftToRight;
				}
				else
				{
					return ScanDirection::kRowRightToLeft;
				}
				break;
			case ScanDirection::kRowAltRightLeft:
				if ((height - row - 1) % 2 == 0)
				{
					return ScanDirection::kRowRightToLeft;
				}
				else
				{
					return ScanDirection::kRowLeftToRight;
                }
				break;
			}
		}

        return direction;
    }
}
