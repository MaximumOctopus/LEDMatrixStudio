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

#include "FormPlaybackSpeed.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TForm18 *Form18;

extern LanguageHandler *GLanguageHandler;


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

int OpenCustomPlaybackSpeed(int oldspeed)
{
	TForm18 *Form18 = new TForm18(Application);

	int speed = oldspeed;

	Form18->eSpeed->Text = speed;

	if (Form18->ShowModal() == mrOk)
	{
		speed = Form18->eSpeed->Text.ToIntDef(0);
	}

	delete Form18;

	return speed;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


__fastcall TForm18::TForm18(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TForm18::bOKClick(TObject *Sender)
{
	int speed = eSpeed->Text.ToIntDef(0);

	if (speed > 0)
	{
		ModalResult = mrOk;
	}
}


void TForm18::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kCustomPlaybackSpeed].c_str();
	lEquality->Caption = GLanguageHandler->Text[k1000ms1Second].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
