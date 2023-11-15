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

#include "FormToggleLockStatus.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TForm22 *Form22;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

ToggleLockFrameRange OpenToggleLockStatus()
{
	TForm22 *Form22 = new TForm22(Application);

	ToggleLockFrameRange tlfr;

	if (Form22->ShowModal() == mrOk)
	{
		tlfr.Process = true;
		tlfr.LockStatus = Form22->cbLockStatus->Checked;
		tlfr.StartFrame = Form22->eStartFrame->Text.ToIntDef(1);
		tlfr.EndFrame   = Form22->eEndFrame->Text.ToIntDef(1);
	}

	delete Form22;

	return tlfr;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


__fastcall TForm22::TForm22(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TForm22::eStartFrameChange(TObject *Sender)
{
	bOk->Enabled = ValidateInputs;
}


bool TForm22::ValidateInputs()
{
	int sf = eStartFrame->Text.ToIntDef(-1);
	int ef = eEndFrame->Text.ToIntDef(-1);

	return (sf != -1 && ef != -1 && sf <= ef);
}


void TForm22::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kToggleFrameLockStatus].c_str();
	Label1->Caption = GLanguageHandler->Text[kStart].c_str();
	Label2->Caption = GLanguageHandler->Text[kEnd].c_str();
	cbLockStatus->Caption = GLanguageHandler->Text[kLock].c_str();

	bOk->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
