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

#include <vector>

#include "ExportOptions.h"


namespace ExportUtility
{
	void AddContentByFrame(ExportOptions, const std::wstring, int, std::vector<std::wstring> &);
	void AddRowColContent(ExportOptions, const std::wstring, std::vector<std::wstring> &);

	std::wstring GetRowData(int *, int, int, bool, int, int, int);
	std::wstring GetColumnData(int *, int, bool, int, int, int);

	std::wstring GetCommentCharacter(ExportLanguage);
	std::wstring GetLanguage(ExportLanguage, bool);
	std::wstring GetLineContent(ExportOptions, bool);
	std::wstring GetLSB(ExportOptions, bool);
	std::wstring GetNumberFormat(ExportLanguage, NumberFormat);
	std::wstring GetNumberSize(ExportLanguage, NumberSize, bool);
	std::wstring GetOrientation(ExportOptions, bool);
	void GetPreamble(ExportOptions, std::vector<std::wstring> &, bool, const std::wstring);
	std::wstring GetExampleCodeDisclaimer(ExportOptions teo);
	std::wstring GetRGBMode(ExportOptions, bool);
	std::wstring GetRGBBrightness(ExportOptions, bool);
	std::wstring GetColourSpace(ExportOptions, bool);
	std::wstring GetScanDirection(ExportOptions, bool);
	std::wstring GetSource(ExportLanguage, ReadSource);
	void GetSpacerLine(ExportLanguage, std::vector<std::wstring> &);
	std::wstring GetSingleVariableStatement(ExportLanguage, NumberSize);
	std::wstring GetVariableID(ExportLanguage);
	std::wstring GetVariableIDFrameIn(ExportLanguage, int);
	std::wstring GetVariableIDFrameOut(ExportLanguage);
	std::wstring GetVariableType(ExportLanguage, NumberSize);

   	void AddEnding(std::vector<std::wstring> &, ExportOptions &);

	std::wstring GetPadding(ExportLanguage, int);

	std::wstring FormatDataAs(const unsigned __int64, NumberFormat, int, int);

	std::wstring TitleWithComments(const std::wstring, ExportLanguage, bool);

    ScanDirection UpdateDirectionColumn(ScanDirection, InputOrientation, int, int);
	ScanDirection UpdateDirectionRow(ScanDirection, InputOrientation, int, int);
}
