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

#include "FrameQuickData.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "SystemSettings.h"

extern LanguageHandler *GLanguageHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TframeSimpleExport *frameSimpleExport;
//---------------------------------------------------------------------------
__fastcall TframeSimpleExport::TframeSimpleExport(TComponent* Owner)
	: TFrame(Owner)
{
}


void __fastcall TframeSimpleExport::miHexNoneClick(TObject *Sender)
{
	TMenuItem *mi = (TMenuItem*)Sender;

	switch (mi->Tag)
	{
	case 0:
		GSystemSettings->App.HexPrefix = L"";
		break;
	case 1:
		GSystemSettings->App.HexPrefix = L"0x";
		break;
	case 2:
		GSystemSettings->App.HexPrefix = L"$";
		break;
	}

	if (OnChange) OnChange(this);
}


void __fastcall TframeSimpleExport::bCopySourceDataClick(TObject *Sender)
{
	mData->SelectAll();
	mData->CopyToClipboard();
}


void __fastcall TframeSimpleExport::cbSourceChange(TObject *Sender)
{
	cbSourceLSB->Clear();
	cbSourceDirection->Clear();

	if (cbSource->ItemIndex == 0)
	{
		cbSourceLSB->Items->Add(GLanguageHandler->Text[kLSBAtLeft].c_str());
		cbSourceLSB->Items->Add(GLanguageHandler->Text[kLSBAtRight].c_str());

		cbSourceDirection->Items->Add(GLanguageHandler->Text[kTopToBottom].c_str());
		cbSourceDirection->Items->Add(GLanguageHandler->Text[kBottomToTop].c_str());
	}
	else
	{
		cbSourceLSB->Items->Add(GLanguageHandler->Text[kLSBAtTop].c_str());
		cbSourceLSB->Items->Add(GLanguageHandler->Text[kLSBAtBottom].c_str());

		cbSourceDirection->Items->Add(GLanguageHandler->Text[kLeftToRight].c_str());
		cbSourceDirection->Items->Add(GLanguageHandler->Text[kRightToLeft].c_str());
		cbSourceDirection->Items->Add(L"Sure 24x16");
	}

	cbSourceLSB->ItemIndex       = 0;
	cbSourceDirection->ItemIndex = 0;

	if (OnChange) OnChange(this);
}


void __fastcall TframeSimpleExport::cbSourceDirectionChange(TObject *Sender)
{
	if (OnChange) OnChange(this);
}


bool TframeSimpleExport::GetCombineNybbles()
{
	return cbCombineNybbles->Checked;
}


int TframeSimpleExport::GetDirection()
{
	return cbSourceDirection->ItemIndex;
}


// output always hex
bool TframeSimpleExport::GetHex()
{
	return true;
}


int TframeSimpleExport::GetLSB()
{
	return cbSourceLSB->ItemIndex;
}


int TframeSimpleExport::GetSource()
{
	return cbSource->ItemIndex;
}


void TframeSimpleExport::SetText(const std::wstring s)
{
	mData->Text = s.c_str();
}


void TframeSimpleExport::SetGuiLanguageText()
{
	Label2->Caption = GLanguageHandler->Text[kSimpleExport].c_str();

	cbSource->Items->Add(GLanguageHandler->Text[kRows].c_str());
	cbSource->Items->Add(GLanguageHandler->Text[kColumns].c_str());
	cbSource->ItemIndex = 0;

	if (GSystemSettings->App.HexPrefix == L"")
	{
		lHexPrefix->Caption = L"<none>";
	}
	else
	{
		lHexPrefix->Caption = GSystemSettings->App.HexPrefix.c_str();
	}

	lHexPrefix->Hint = GLanguageHandler->Text[kHexFormat].c_str();

	cbSourceChange(nullptr);
}
