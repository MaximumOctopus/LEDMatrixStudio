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

#include "FormMerge.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMerge *frmMerge;

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

MergeObject OpenMerge()
{
	TfrmMerge *frmMerge = new TfrmMerge(Application);

	MergeObject mo;

	if (frmMerge->ShowModal() == mrOk)
	{
		mo.Process = true;

		mo.FileName = frmMerge->eFileName->Text;
		mo.StartFrame = frmMerge->eStartFrame->Text.ToIntDef(1);

		if (frmMerge->rbMergeBottom->Checked)
		{
			mo.Mode = MergeMode::kAnimationBottom;
		}
		else if (frmMerge->rbMergeTop->Checked)
		{
			mo.Mode = MergeMode::kAnimationTop;
		}
		else if (frmMerge->rbMergeNewLayer->Checked)
		{
			mo.Mode = MergeMode::kNewLayer;
		}
		else
		{
			mo.Mode = MergeMode::kCurrentFrame;
        }
	}

	delete frmMerge;

	return mo;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

__fastcall TfrmMerge::TfrmMerge(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void __fastcall TfrmMerge::eFileNameChange(TObject *Sender)
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


void __fastcall TfrmMerge::eStartFrameChange(TObject *Sender)
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


void __fastcall TfrmMerge::miMergeClick(TObject *Sender)
{
	if (odMain->Execute())
	{
		eFileName->Text = odMain->FileName;

		bOk->Enabled = true;
	}
}


void TfrmMerge::SetGuiLanguageText()
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
