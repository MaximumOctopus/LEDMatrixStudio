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

#ifndef FormAddLayerH
#define FormAddLayerH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>

#include "LanguageConstants.h"
#include "LanguageHandler.h"

extern LanguageHandler *GLanguageHandler;

//---------------------------------------------------------------------------
class TForm3 : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TImage *Image1;
	TLabel *Label2;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TLabel *Label6;
	TEdit *eName;
	TComboBox *cbSourceLayer;
	TCheckBox *cbCopyFrom;
	void __fastcall cbCopyFromClick(TObject *Sender);
private:
	void SetGuiLanguageText();
public:		// User declarations
	__fastcall TForm3(TComponent* Owner);
};


struct AddObject
{
	bool Process = false;
	std::wstring Name = L"";
	bool CopyFrom = false;
	int CopyLayer = 0;
};


AddObject OpenAddLayer(const std::vector<std::wstring> &layers);


//---------------------------------------------------------------------------
extern PACKAGE TForm3 *Form3;
//---------------------------------------------------------------------------
#endif
