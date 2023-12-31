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

#ifndef FrameFontPanelH
#define FrameFontPanelH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Dialogs.hpp>
//---------------------------------------------------------------------------
class TframeFont : public TFrame
{
__published:	// IDE-managed Components
	TSpeedButton *sbChangeFont;
	TLabel *lFont;
	TFontDialog *fdMain;
	void __fastcall lFontClick(TObject *Sender);
private:

	int FontAscii = 0;

public:		// User declarations
	__fastcall TframeFont(TComponent* Owner);

	void SetFontAscii(int);
};
//---------------------------------------------------------------------------
extern PACKAGE TframeFont *frameFont;
//---------------------------------------------------------------------------
#endif
