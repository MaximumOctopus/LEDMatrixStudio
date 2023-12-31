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

#include "FormToggleLockStatus.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmToggleLockStatus *frmToggleLockStatus;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

ToggleLockFrameRange OpenToggleLockStatus()
{
	TfrmToggleLockStatus *frmToggleLockStatus = new TfrmToggleLockStatus(Application);

	ToggleLockFrameRange tlfr;

	if (frmToggleLockStatus->ShowModal() == mrOk)
	{
		tlfr.Process = true;
		tlfr.LockStatus = frmToggleLockStatus->cbLockStatus->Checked;
		tlfr.StartFrame = frmToggleLockStatus->eStartFrame->Text.ToIntDef(1);
		tlfr.EndFrame   = frmToggleLockStatus->eEndFrame->Text.ToIntDef(1);
	}

	delete frmToggleLockStatus;

	return tlfr;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


__fastcall TfrmToggleLockStatus::TfrmToggleLockStatus(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmToggleLockStatus::eStartFrameChange(TObject *Sender)
{
	bOk->Enabled = ValidateInputs;
}


bool TfrmToggleLockStatus::ValidateInputs()
{
	int sf = eStartFrame->Text.ToIntDef(-1);
	int ef = eEndFrame->Text.ToIntDef(-1);

	return (sf != -1 && ef != -1 && sf <= ef);
}


void TfrmToggleLockStatus::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kToggleFrameLockStatus].c_str();
	Label1->Caption = GLanguageHandler->Text[kStart].c_str();
	Label2->Caption = GLanguageHandler->Text[kEnd].c_str();
	cbLockStatus->Caption = GLanguageHandler->Text[kLock].c_str();

	bOk->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
