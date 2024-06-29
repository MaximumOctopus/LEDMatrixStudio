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

#include <vcl.h>
#pragma hdrstop

#include "FormOptimise.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "Optimisation.h"
#include "Utility.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmOptimise *frmOptimise;

//---------------------------------------------------------------------------

void OpenOptimise(TheMatrix *tm)
{
	TfrmOptimise *frmOptimise = new TfrmOptimise(Application);

    frmOptimise->thematrix = tm;

	if (frmOptimise->ShowModal() == mrOk)
	{
	}

	delete frmOptimise;
}

//---------------------------------------------------------------------------

__fastcall TfrmOptimise::TfrmOptimise(TComponent* Owner)
	: TForm(Owner)
{
	cbDataSize->Items->Add(L"4 bits (one nybble)");
	cbDataSize->Items->Add(L"1 byte");
	cbDataSize->Items->Add(L"2 bytes");
	cbDataSize->Items->Add(L"4 byte");
	cbDataSize->Items->Add(L"8 bytes");

	cbDataSize->ItemIndex = 1;

	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCommaSeparated].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPICAXEEEPROM].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCpp1Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCpp2Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportCCppFastLED].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPython1Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPython2Dimensional].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportMicrochip].c_str());
	cbLanguageFormat->Items->Add(GLanguageHandler->Text[kExportPascal].c_str());

	cbLanguageFormat->ItemIndex = 0;

	cbPerRow->Items->Add(L"4");
	cbPerRow->Items->Add(L"5");
	cbPerRow->Items->Add(L"7");
	cbPerRow->Items->Add(L"8");
	cbPerRow->Items->Add(L"10");
	cbPerRow->Items->Add(L"16");
	cbPerRow->Items->Add(L"20");
	cbPerRow->Items->Add(L"32");
	cbPerRow->Items->Add(L"64");

	cbPerRow->ItemIndex = 4;

	SetGuiLanguageText();
}


void __fastcall TfrmOptimise::sbCopyOutputClick(TObject *Sender)
{
	mMemo->CopyToClipboard();
}


void __fastcall TfrmOptimise::sbOptimiseClick(TObject *Sender)
{
	std::vector<std::wstring> output;

	PopulateMatrixData();

	HexFormat  = true;
	ColumnsLSB = 0;
	RowsLSB    = 0;

	// ===========================================================================

	ExportOptions teo;

	teo.Code.Size = GetDataSize();
	teo.Code.Language  = ExportLanguage(cbLanguageFormat->ItemIndex);
	teo.Code.LineCount = cbPerRow->Text.ToIntDef(10);

	mMemo->Clear();

	if (!Optimiser::OptimiseDataSimple(thematrix, teo, MatrixData, output))
	{
		mMemo->Lines->Add(L"Error :(");
	}

	for (int t = 0; t < output.size(); t++)
	{
		mMemo->Lines->Add(output[t].c_str());
	}
}


void TfrmOptimise::SetGuiLanguageText()
{
	std::wstring cap = GLanguageHandler->Text[kOptimise] + L" (beta)";

	Caption = cap.c_str();

	sbOptimise->Caption = GLanguageHandler->Text[kOptimise].c_str();
	sbCopyOutput->Caption = GLanguageHandler->Text[kCopyOutput].c_str();

	gbOutputOptions->Caption = GLanguageHandler->Text[kOutputOptions].c_str();
	Label4->Caption = GLanguageHandler->Text[kDataSize].c_str();
	Label5->Caption = GLanguageHandler->Text[kLanguage].c_str();
	Label6->Caption = GLanguageHandler->Text[kOutput].c_str();
	Label7->Caption = GLanguageHandler->Text[kPerRow].c_str();
}


void TfrmOptimise::PopulateMatrixData()
{
	MatrixData.clear();

	for (int t = 0; t < mData->Lines->Count; t++)
	{
		std::wstring line = mData->Lines->Strings[t].c_str();

		std::wstring data = L"";
		bool incomment = false;

		if (!line.empty())
		{
			for (int l = 0; l < line.length(); l++)
			{
				if (!incomment)
				{
					if (line[l] == L',')
					{
						MatrixData.push_back(data);

						data = L"";
					}
					else
					{
						if (line[l] == L'\'' ||
							line[l] == L'/' ||
							line[l] == L';')
						{
							incomment = true;
						}
						else
						{
							if (Utility::IsAlphaNumeric(line[l]))
							{
								data += line[l];
							}
						}
					}
				}
			}

			if (!data.empty())
			{
				MatrixData.push_back(data);
			}
		}
	}
}


NumberSize TfrmOptimise::GetDataSize()
{
	switch (cbDataSize->ItemIndex)
	{
	case 0:
		return NumberSize::k8Bit;
	case 1:
		return NumberSize::k8Bit;
	case 2:
		return NumberSize::k16bit;
	case 3:
		return NumberSize::k32bit;
	case 4:
		return NumberSize::k64bit;

	default:
		return NumberSize::k8Bit;
	}
}
