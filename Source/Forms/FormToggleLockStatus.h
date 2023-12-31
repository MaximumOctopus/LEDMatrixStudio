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

#ifndef FormToggleLockStatusH
#define FormToggleLockStatusH

#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>


class TfrmToggleLockStatus : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TImage *Image1;
	TBitBtn *bOk;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TLabel *Label2;
	TEdit *eStartFrame;
	TEdit *eEndFrame;
	TCheckBox *cbLockStatus;
	void __fastcall eStartFrameChange(TObject *Sender);
private:

	bool ValidateInputs();
	void SetGuiLanguageText();

public:		// User declarations
	__fastcall TfrmToggleLockStatus(TComponent* Owner);
};


struct ToggleLockFrameRange
{
	bool Process = false;
	bool LockStatus = false;
	int StartFrame = 0;
	int EndFrame = 0;
};

ToggleLockFrameRange OpenToggleLockStatus();

//---------------------------------------------------------------------------
extern PACKAGE TfrmToggleLockStatus *frmToggleLockStatus;
//---------------------------------------------------------------------------
#endif
