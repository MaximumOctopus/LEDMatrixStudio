// ===================================================================
//
//   (c) Paul Alan Freshney 2012-2026
//   www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
//   https://github.com/MaximumOctopus/LEDMatrixStudio
//
//   https://maximumoctopus.hashnode.dev/
//
//   C++ Rewrite October 11th 2023
//
// ===================================================================

#ifndef FormPlaybackSpeedH
#define FormPlaybackSpeedH

#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>

class TfrmPlaybackSpeed : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TLabel *lEquality;
	TEdit *eSpeed;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TEdit *eFPS;
	TLabel *Label2;
	void __fastcall bOKClick(TObject *Sender);
	void __fastcall eSpeedExit(TObject *Sender);
	void __fastcall eFPSExit(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:

	void SetGuiLanguageText();

public:		// User declarations
	__fastcall TfrmPlaybackSpeed(TComponent* Owner);
};

//---------------------------------------------------------------------------

int OpenCustomPlaybackSpeed(int oldspeed);


extern PACKAGE TfrmPlaybackSpeed *frmPlaybackSpeed;
//---------------------------------------------------------------------------
#endif
