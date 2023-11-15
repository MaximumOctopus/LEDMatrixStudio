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

#include <vcl.h>
#pragma hdrstop

#include "FormDeleteMultiple.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TForm7 *Form7;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

DeleteMultipleObject OpenDeleteMultiple()
{
	TForm7 *Form7 = new TForm7(Application);

	DeleteMultipleObject dmo;

	if (Form7->ShowModal() == mrOk)
	{
		dmo.Process = true;
		dmo.StartFrame = Form7->eStartFrame->Text.ToIntDef(0);
		dmo.EndFrame = Form7->eEndFrame->Text.ToIntDef(0);
	}

	delete Form7;

	return dmo;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


__fastcall TForm7::TForm7(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TForm7::eStartFrameChange(TObject *Sender)
{
	bOk->Enabled = ValidateInputs();
}


void TForm7::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kDeleteMultipleFramesC].c_str();

	lFrom->Caption = GLanguageHandler->Text[kFrom].c_str();
	lTo->Caption = GLanguageHandler->Text[kToC].c_str();
	lWarning->Caption = GLanguageHandler->Text[kWarningThisActionCannotBeUndone].c_str();

	bOk->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


bool TForm7::ValidateInputs()
{
	int sf = eStartFrame->Text.ToIntDef(-1);
	int ef = eEndFrame->Text.ToIntDef(-1);

	return (sf != -1 && ef != -1 && sf <= ef);
}
