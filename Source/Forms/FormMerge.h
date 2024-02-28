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

#ifndef FormMergeH
#define FormMergeH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
//---------------------------------------------------------------------------
class TfrmMerge : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TImage *Image1;
	TBitBtn *bOk;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TSpeedButton *miMerge;
	TLabel *lStartFrame;
	TRadioButton *rbMergeBottom;
	TRadioButton *rbMergeTop;
	TRadioButton *rbMergeNewLayer;
	TRadioButton *rbMergeCurrentLayer;
	TEdit *eFileName;
	TEdit *eStartFrame;
	TOpenDialog *odMain;
	void __fastcall eFileNameChange(TObject *Sender);
	void __fastcall eStartFrameChange(TObject *Sender);
	void __fastcall miMergeClick(TObject *Sender);
private:

	void SetGuiLanguageText();

public:
	__fastcall TfrmMerge(TComponent* Owner);
};

//---------------------------------------------------------------------------

enum class MergeMode { kAnimationBottom = 0, kAnimationTop, kNewLayer, kCurrentFrame };

struct MergeObject
{
	bool Process = false;
	std::wstring FileName = L"";
	MergeMode Mode;
	int StartFrame;
};

MergeObject OpenMerge();

//---------------------------------------------------------------------------
extern PACKAGE TfrmMerge *frmMerge;
//---------------------------------------------------------------------------
#endif
