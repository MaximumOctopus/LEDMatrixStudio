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

#include "FormColourChange.h"
#include "ColourUtility.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmColourChange *frmColourChange;

//---------------------------------------------------------------------------

ColourChange OpenColourChange(std::vector<int> &colours)
{
	TfrmColourChange *frmColourChange = new TfrmColourChange(Application);

	ColourChange cco;

	for (int t = 0; t < colours.size(); t++)
	{
		TObject *colour = (TObject*)colours[t];

		frmColourChange->clbUserFrom->AddItem(ColourUtility::RGBPlusInteger(colours[t], 100).c_str(), colour);
		frmColourChange->clbUserTo->AddItem(ColourUtility::RGBPlusInteger(colours[t], 100).c_str(), colour);
	}

	if (frmColourChange->ShowModal() == mrOk)
	{
		cco.Process    = true;
		cco.ColourFrom = frmColourChange->sFrom->Brush->Color;
		cco.ColourTo   = frmColourChange->sTo->Brush->Color;
	}

	delete frmColourChange;

	return cco;
}


__fastcall TfrmColourChange::TfrmColourChange(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void TfrmColourChange::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kColourChanger].c_str();
	Label1->Caption = GLanguageHandler->Text[kFrom].c_str();
	Label2->Caption = GLanguageHandler->Text[kToC].c_str();
	Label4->Caption = GLanguageHandler->Text[kWarningThisActionCannotBeUndone].c_str();
	Label3->Caption = GLanguageHandler->Text[kFirstThirtyTwoColoursFromTheCurrentAnimation].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


void __fastcall TfrmColourChange::clbUserFromDblClick(TObject *Sender)
{
	TColorListBox *clb = (TColorListBox*)Sender;

	if (clb->ItemIndex != -1)
	{
		if (clb->Tag == 0)
		{
			int colour = (int)clbUserFrom->Items->Objects[clbUserFrom->ItemIndex];

			sFrom->Brush->Color = TColor(colour);
		}
		else
		{
			int colour = (int)clbUserTo->Items->Objects[clbUserTo->ItemIndex];

			sTo->Brush->Color = TColor(colour);
		}
	}
}


void __fastcall TfrmColourChange::sFromMouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
	if (cdChanger->Execute())
	{
		TShape *shape = (TShape*)Sender;

		shape->Brush->Color = cdChanger->Color;
	}
}
