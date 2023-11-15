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
class TForm7 : public TForm
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
	__fastcall TForm7(TComponent* Owner);
};

struct DeleteMultipleObject
{
	bool Process = false;
	int StartFrame = 0;
	int EndFrame = 0;
};

//---------------------------------------------------------------------------

DeleteMultipleObject OpenDeleteMultiple();

//---------------------------------------------------------------------------
extern PACKAGE TForm7 *Form7;
//---------------------------------------------------------------------------
#endif
