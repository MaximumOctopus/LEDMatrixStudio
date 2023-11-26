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

#include "FormMerge.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm13 *Form13;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

MergeObject OpenMerge()
{
	TForm13 *Form13 = new TForm13(Application);

	MergeObject mo;

	if (Form13->ShowModal() == mrOk)
	{
		mo.Process = true;

		mo.FileName = Form13->eFileName->Text;
		mo.StartFrame = Form13->eStartFrame->Text.ToIntDef(1);

		if (Form13->rbMergeBottom->Checked)
		{
			mo.Mode = MergeMode::kAnimationBottom;
		}
		else if (Form13->rbMergeTop->Checked)
		{
			mo.Mode = MergeMode::kAnimationTop;
		}
		else if (Form13->rbMergeNewLayer->Checked)
		{
			mo.Mode = MergeMode::kNewLayer;
		}
		else
		{
			mo.Mode = MergeMode::kCurrentFrame;
        }
	}

	delete Form13;

	return mo;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

__fastcall TForm13::TForm13(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TForm13::eFileNameChange(TObject *Sender)
{
	if (eFileName->Text != L"")
	{
		bOk->Enabled = true;
	}
	else
	{
		bOk->Enabled = false;
	}
}


void __fastcall TForm13::eStartFrameChange(TObject *Sender)
{
	if (eStartFrame->Text.ToIntDef(-1) == -1)
	{
		bOk->Enabled = false;
	}
	else
	{
		bOk->Enabled = true;
    }
}


void __fastcall TForm13::miMergeClick(TObject *Sender)
{
	if (odMain->Execute())
	{
		eFileName->Text = odMain->FileName;

		bOk->Enabled = true;
	}
}


void TForm13::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kMerge].c_str();
	Label1->Caption = GLanguageHandler->Text[kFileName].c_str();
	miMerge->Caption = GLanguageHandler->Text[kOpen].c_str();

	rbMergeBottom->Caption = GLanguageHandler->Text[kMergeInToAnimationBottomHasPriority].c_str();
	rbMergeTop->Caption = GLanguageHandler->Text[kMergeInToAnimationTopHasPriority].c_str();
	rbMergeNewLayer->Caption = GLanguageHandler->Text[kMergeInToNewLayer].c_str();
	rbMergeCurrentLayer->Caption = GLanguageHandler->Text[kMergeInToCurrentLayer].c_str();

	lStartFrame->Caption = GLanguageHandler->Text[kStartFrame].c_str();

	bOk->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}
