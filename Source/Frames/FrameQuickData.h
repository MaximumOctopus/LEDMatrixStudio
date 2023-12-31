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

#ifndef FrameQuickDataH
#define FrameQuickDataH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
class TframeSimpleExport : public TFrame
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TLabel *Label2;
	TLabel *lHexPrefix;
	TComboBox *cbSource;
	TBitBtn *bCopySourceData;
	TComboBox *cbSourceDirection;
	TComboBox *cbSourceLSB;
	TCheckBox *cbCombineNybbles;
	TPanel *Panel2;
	TMemo *mData;
	TPopupMenu *PopupMenu1;
	TMenuItem *miHexNone;
	TMenuItem *N0xx1;
	TMenuItem *N1;
	void __fastcall miHexNoneClick(TObject *Sender);
	void __fastcall bCopySourceDataClick(TObject *Sender);
	void __fastcall cbSourceChange(TObject *Sender);
	void __fastcall cbSourceDirectionChange(TObject *Sender);
private:
public:
	__fastcall TframeSimpleExport(TComponent* Owner);

	bool GetCombineNybbles();
	int GetDirection();
	bool GetHex();
	int GetLSB();
	int GetSource();
	void SetText(const std::wstring);
	void SetGuiLanguageText();

	std::function<void(TframeSimpleExport*)> OnChange;
};


//---------------------------------------------------------------------------
extern PACKAGE TframeSimpleExport *frameSimpleExport;
//---------------------------------------------------------------------------
#endif
