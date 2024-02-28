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

#include "FormPlaybackSpeed.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmPlaybackSpeed *frmPlaybackSpeed;

extern LanguageHandler *GLanguageHandler;


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

int OpenCustomPlaybackSpeed(int oldspeed)
{
	TfrmPlaybackSpeed *frmPlaybackSpeed = new TfrmPlaybackSpeed(Application);

	int speed = oldspeed;

	frmPlaybackSpeed->eSpeed->Text = speed;

	if (frmPlaybackSpeed->ShowModal() == mrOk)
	{
		speed = frmPlaybackSpeed->eSpeed->Text.ToIntDef(0);
	}

	delete frmPlaybackSpeed;

	return speed;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


__fastcall TfrmPlaybackSpeed::TfrmPlaybackSpeed(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmPlaybackSpeed::bOKClick(TObject *Sender)
{
	int speed = eSpeed->Text.ToIntDef(0);

	if (speed > 0)
	{
		ModalResult = mrOk;
	}
}


void TfrmPlaybackSpeed::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kCustomPlaybackSpeed].c_str();
	lEquality->Caption = GLanguageHandler->Text[k1000ms1Second].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
