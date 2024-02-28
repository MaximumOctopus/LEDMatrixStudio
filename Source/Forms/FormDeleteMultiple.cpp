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

#include "FormDeleteMultiple.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmDeleteMultiple *frmDeleteMultiple;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

DeleteMultipleObject OpenDeleteMultiple(int maxframecount)
{
	TfrmDeleteMultiple *frmDeleteMultiple = new TfrmDeleteMultiple(Application);

    frmDeleteMultiple->FrameCount = maxframecount;

	DeleteMultipleObject dmo;

	if (frmDeleteMultiple->ShowModal() == mrOk)
	{
		dmo.Process = true;
		dmo.StartFrame = frmDeleteMultiple->eStartFrame->Text.ToIntDef(0) - 1;
		dmo.EndFrame = frmDeleteMultiple->eEndFrame->Text.ToIntDef(0) - 1;
	}

	delete frmDeleteMultiple;

	return dmo;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


__fastcall TfrmDeleteMultiple::TfrmDeleteMultiple(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmDeleteMultiple::eStartFrameChange(TObject *Sender)
{
	bOk->Enabled = ValidateInputs();
}


void TfrmDeleteMultiple::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kDeleteMultipleFramesC].c_str();

	lFrom->Caption = GLanguageHandler->Text[kFrom].c_str();
	lTo->Caption = GLanguageHandler->Text[kToC].c_str();
	lWarning->Caption = GLanguageHandler->Text[kWarningThisActionCannotBeUndone].c_str();

	bOk->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


bool TfrmDeleteMultiple::ValidateInputs()
{
	int sf = eStartFrame->Text.ToIntDef(-1);
	int ef = eEndFrame->Text.ToIntDef(-1);

	return (sf != -1 && ef != -1 && sf <= ef && sf >=1 && ef >=1 && ef <= FrameCount);
}
