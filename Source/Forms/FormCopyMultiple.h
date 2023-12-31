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

#ifndef FormCopyMultipleH
#define FormCopyMultipleH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>

#include <vector>

//---------------------------------------------------------------------------
class TfrmCopyMultiple : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TLabel *Label4;
	TImage *Image1;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label5;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *Label8;
	TLabel *lWarningMessage;
	TEdit *eStartFrame;
	TEdit *eEndFrame;
	TEdit *eCopyTo;
	TComboBox *cbSourceLayer;
	TComboBox *cbDestinationLayer;
	TCheckBox *cbAllLayers;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall cbAllLayersClick(TObject *Sender);
	void __fastcall eStartFrameChange(TObject *Sender);
private:

	bool ValidateInputs();
	void SetGuiLanguageText();

public:		// User declarations
	__fastcall TfrmCopyMultiple(TComponent* Owner);

    int FrameCount = 0;
};

struct CopyMultipleObject
{
	bool Process = false;
	int StartFrame = 0;
	int EndFrame = 0;
	int CopyTo = 0;
	int Source = 0;
	int Destination = 0;
	bool AllLayers = false;
};

CopyMultipleObject OpenCopyMultiple(int, std::vector<std::wstring> &);

//---------------------------------------------------------------------------
extern PACKAGE TfrmCopyMultiple *frmCopyMultiple;
//---------------------------------------------------------------------------
#endif
