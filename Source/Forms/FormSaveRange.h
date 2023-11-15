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

#ifndef FormSaveRangeH
#define FormSaveRangeH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>


struct SaveFrameRangeObject
{
	bool Process = false;
	int StartFrame = 0;
	int EndFrame = 0;
};

class TForm20 : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TImage *Image1;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *lStart;
	TLabel *lEnd;
	TEdit *eStartFrame;
	TEdit *eEndFrame;
	void __fastcall eStartFrameChange(TObject *Sender);
private:	// User declarations

	void SetGuiLanguageText();
	bool ValidateInputs();

public:		// User declarations
	__fastcall TForm20(TComponent* Owner);
};

//---------------------------------------------------------------------------

SaveFrameRangeObject OpenFrameRange(int frame_count);


extern PACKAGE TForm20 *Form20;

#endif
