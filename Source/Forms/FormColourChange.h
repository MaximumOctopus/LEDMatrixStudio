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

#ifndef FormColourChangeH
#define FormColourChangeH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>

#include <vector>

//---------------------------------------------------------------------------
class TfrmColourChange : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TLabel *Label3;
	TImage *Image1;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TLabel *Label2;
	TShape *sFrom;
	TShape *sTo;
	TLabel *Label4;
	TColorListBox *clbUserFrom;
	TColorListBox *clbUserTo;
	TColorDialog *cdChanger;
	void __fastcall clbUserFromDblClick(TObject *Sender);
	void __fastcall sFromMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
private:

	void SetGuiLanguageText();

public:
	__fastcall TfrmColourChange(TComponent* Owner);
};


struct ColourChange
{
	bool Process = false;
	int ColourFrom = 0;
	int ColourTo = 0;
};


ColourChange OpenColourChange(std::vector<int> &);

//---------------------------------------------------------------------------
extern PACKAGE TfrmColourChange *frmColourChange;
//---------------------------------------------------------------------------
#endif
