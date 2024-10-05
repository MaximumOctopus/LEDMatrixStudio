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

#include "ExportMonoBi.h"
#include "ExportRGB.h"
#include "ExportUtility.h"
#include "Formatting.h"
#include "Optimisation.h"


bool Optimiser::OptimiseData(TheMatrix *thematrix, ExportOptions teo, std::vector<std::wstring> &data)
{
	std::vector<std::wstring> unique_items;
	//lUniqueItems.Sorted := true;

	std::vector<std::wstring> output;

	// =======================================================================

	ProcessUnique(data, unique_items);

	// == now calculate whether the optimisation is worth it! ================

	int indexdatasize = 1;  // 1 byte per item

	if (unique_items.size() >= 256 && unique_items.size() < 65536)
	{
		indexdatasize = 2;  // 2 bytes per items
	}
	else
	{
		indexdatasize = 4;  // 4 bytes per items
	}

	// =======================================================================

	// calculate the size of the current animation after optimisation
	int os = (thematrix->DataSizeBytes() * unique_items.size()) +
			 ((thematrix->Details.Width * thematrix->Details.Height * thematrix->GetFrameCount()) * indexdatasize);

	if (os < thematrix->CalculateMemoryUsage())
	{
		data.clear();

		ExportUtility::GetPreamble(teo, data, false, thematrix->Details.Comment);

		ExportUtility::GetSpacerLine(teo.Code.Language, data);

		data.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L"Unoptimised size: " + std::to_wstring(thematrix->CalculateMemoryUsage()) + L" bytes");
		data.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L"  Optimised size: " + std::to_wstring(os) + L" bytes");
		data.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L"          Saving: " + std::to_wstring(thematrix->CalculateMemoryUsage() - os) + L" bytes (" +
										 std::to_wstring(std::round(((thematrix->CalculateMemoryUsage() - os) / thematrix->CalculateMemoryUsage()) * 100)) + L"%)");

		ExportUtility::GetSpacerLine(teo.Code.Language, data);

		data.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L" " + GLanguageHandler->Text[kAccessWithLEDDataIndex] + L" ");

		ExportUtility::GetSpacerLine(teo.Code.Language, data);

		data.push_back(L"");

		data.push_back(ExportUtility::GetVariableType(teo.Code.Language, teo.Code.Size) + L"leddataindex[] = {");

		std::wstring s = L"";
		for (int t = 0; t < unique_items.size(); t++)
		{
			s += unique_items[t];

			if (t != unique_items.size())
			{
				s += L", ";
			}
		}

		data.push_back(s + L"};");

		data.push_back(L"");

		// =========================================================================

		teo.Code.CleanMode = false;
		//teo.NumberSize := lIndexDataSize; // this was causing issues, but no idea what it does

        int i = 0;

		if (teo.Code.RGBEnabled)
		{
			if (ExportRGB::CreateExportAnimationRGB(thematrix, teo, output, i, unique_items))
			{
				for (int t = 0; t < output.size(); t++)
				{
					data.push_back(output[t]);
				}
			}
			else
			{
				data.push_back(GLanguageHandler->Text[kError]);
			}
		}
		else
		{
			if (ExportMonoBi::CreateExportAnimation(thematrix, teo, output, i, unique_items))
			{
				for (int t = 0; t < output.size(); t++)
				{
					data.push_back(output[t]);
				}
			}
			else
			{
				data.push_back(GLanguageHandler->Text[kError]);
			}
		}

		return true;
	}

	return false;
}


bool Optimiser::OptimiseDataSimple(TheMatrix *thematrix, ExportOptions teo, std::vector<std::wstring> &data, std::vector<std::wstring> &output)
{
	if (data.size() == 0)
	{
		output.push_back(L"No data.");

		return false;
	}

	std::vector<std::wstring> unique_items;
	//  lUniqueItems.Sorted := true;

	int indexdatasize = 0;

	// =======================================================================

	ProcessUnique(data, unique_items);

	// == now calculate whether the optimisation is worth it! ================

	if (unique_items.size() < 256)
	{
		indexdatasize = 1;
	}
	else if (unique_items.size() < 65536)
	{
		indexdatasize = 2;
	}
	else
	{
		indexdatasize = 4;
	}

	// =======================================================================

	int uos = data.size() * teo.GetNumberSizeLengthBytes(teo.Code.Size);

	int os  = teo.GetNumberSizeLengthBytes(teo.Code.Size) * unique_items.size() + (data.size() * indexdatasize);             // lut size of data

	output.clear();

	ExportUtility::GetPreamble(teo, output, true, thematrix->Details.Comment);

	ExportUtility::GetSpacerLine(teo.Code.Language, output);

	output.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L"Unoptimised size: " + std::to_wstring(uos) + L" bytes");
	output.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L"  Optimised size: " + std::to_wstring(os) + L" bytes");
	output.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L"          Saving: " + std::to_wstring(uos - os) + L" bytes (" +
												std::to_wstring(std::round((((double)uos - (double)os) / (double)uos) * 100)) + L"%)");

	ExportUtility::GetSpacerLine(teo.Code.Language, output);

	output.push_back(ExportUtility::GetCommentCharacter(teo.Code.Language) + L" Access with leddataindex[ledarray[x]] ");

	ExportUtility::GetSpacerLine(teo.Code.Language, output);

	// =======================================================================

	output.push_back(L"");

	output.push_back(ExportUtility::GetVariableType(teo.Code.Language, teo.Code.Size) + L"leddataindex[] = {");

	std::wstring s = L"";
	for (int t = 0; t < unique_items.size(); t++)
	{
		s += unique_items[t];

		if (t !=  unique_items.size() - 1)
		{
			s += L", ";
		}
	 }

	output.push_back(s);
	output.push_back(L"};");

	// =======================================================================

	output.push_back(L"");

	for (int t = 0; t < data.size(); t++)
	{
		for (int i = 0; i < unique_items.size(); i++)
		{
			if (data[t] == unique_items[i])
			{
				data[t] = std::to_wstring(i);
			}
		}
	}

	output.push_back(ExportUtility::GetVariableType(teo.Code.Language, teo.Code.Size) + L"ledarray[] = {");

	s = L"";
	int i = std::to_wstring(unique_items.size()).length();
	int count = 0;

	for (int t = 0; t < data.size(); t++)
	{
		s += Formatting::PadToLength(data[t], i) + L", ";

		count++;

		if (count == teo.Code.LineCount)
		{
			output.push_back(s);

			s = L"";

			count = 0;
		}
	}

	if (!s.empty())
	{
		output.push_back(s);

		output.push_back(L"};");
	}

	// =======================================================================

	return true;
}


void Optimiser::ProcessUnique(std::vector<std::wstring> &data, std::vector<std::wstring> &unique_items)
{
	for (int t = 0; t < data.size(); t++)
	{
		std::wstring s = L"";

		for (int i = 0; i < data[t].length(); i++)
		{
			if (data[t][i] == L' ')
			{
				if (std::find(unique_items.begin(), unique_items.end(), s) == unique_items.end())
				{
					unique_items.push_back(s);
				}

				s = L"";
			}
			else
			{
				s += data[t][i];
			}
		}

		if (s != L"")
		{
			if (std::find(unique_items.begin(), unique_items.end(), s) == unique_items.end())
			{
				unique_items.push_back(s);
			}
		}
	}
}
