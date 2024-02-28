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

#ifndef FormDeleteMultipleH
#define FormDeleteMultipleH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
//---------------------------------------------------------------------------
class TfrmDeleteMultiple : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TLabel *lWarning;
	TImage *Image1;
	TBitBtn *bOk;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *lFrom;
	TLabel *lTo;
	TEdit *eStartFrame;
	TEdit *eEndFrame;
	void __fastcall eStartFrameChange(TObject *Sender);
private:	// User declarations

	void SetGuiLanguageText();
	bool ValidateInputs();

public:		// User declarations
	__fastcall TfrmDeleteMultiple(TComponent* Owner);

	int FrameCount = 0;
};

struct DeleteMultipleObject
{
	bool Process = false;
	int StartFrame = 0;
	int EndFrame = 0;
};

//---------------------------------------------------------------------------

DeleteMultipleObject OpenDeleteMultiple(int);

//---------------------------------------------------------------------------
extern PACKAGE TfrmDeleteMultiple *frmDeleteMultiple;
//---------------------------------------------------------------------------
#endif
