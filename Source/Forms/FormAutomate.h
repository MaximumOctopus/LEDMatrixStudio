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

#ifndef FormAutomateH
#define FormAutomateH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>

#include <vector>

#include "ActionObject.h"
#include "MatrixConstants.h"
#include "RGBPaletteColours.h"


struct AutomationInput
{
	int FrameCurrent = 0;
	int FrameMax = 0;

	int Width = 0;
	int Height = 0;

	MatrixMode Mode = MatrixMode::kNone;
};


//---------------------------------------------------------------------------
class TfrmAutomate : public TForm
{
__published:	// IDE-managed Components
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TPageControl *PageControl1;
	TTabSheet *tsActions;
	TLabel *lSuggestion;
	TGroupBox *gbActions;
	TSpeedButton *sbMirror;
	TSpeedButton *sbFlip;
	TSpeedButton *sbInvert;
	TSpeedButton *sbScrollLeft;
	TSpeedButton *sbScrollRight;
	TSpeedButton *sbScrollUp;
	TSpeedButton *sbScrollDown;
	TSpeedButton *sbRotateL;
	TSpeedButton *sbRotateR;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *Label8;
	TLabel *Label9;
	TSpeedButton *SpeedButton1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton3;
	TSpeedButton *SpeedButton4;
	TLabel *Label10;
	TLabel *Label11;
	TSpeedButton *SpeedButton5;
	TSpeedButton *SpeedButton6;
	TLabel *lBrush;
	TSpeedButton *SpeedButton7;
	TSpeedButton *SpeedButton8;
	TLabel *Label12;
	TSpeedButton *sbCyclingLinear;
	TSpeedButton *SpeedButton10;
	TSpeedButton *sbLeftRight;
	TSpeedButton *sbRightLeft;
	TSpeedButton *sbUpDown;
	TSpeedButton *sbDownUp;
	TLabel *Label16;
	TSpeedButton *SpeedButton9;
	TSpeedButton *SpeedButton13;
	TLabel *Label19;
	TSpeedButton *SpeedButton14;
	TLabel *Label20;
	TShape *sRevealColour;
	TLabel *lColour;
	TSpeedButton *SpeedButton15;
	TSpeedButton *SpeedButton16;
	TBevel *Bevel2;
	TComboBox *cbWipe;
	TComboBox *cbReveal;
	TGroupBox *gbProcessingOptions;
	TLabel *lFrameStart;
	TLabel *lFrameEnd;
	TLabel *Label5;
	TLabel *lLayer;
	TEdit *eFrameStart;
	TEdit *eFrameEnd;
	TRadioButton *rbProcessMode2;
	TRadioButton *rbProcessMode1;
	TCheckBox *cbErase;
	TRadioButton *rbProcessMode3;
	TComboBox *cbLayer;
	TGroupBox *gbActionList;
	TSpeedButton *sbClear;
	TSpeedButton *sbRemoveSelected;
	TLabel *Label4;
	TListBox *lbActions;
	TGroupBox *gbPostProcessing;
	TSpeedButton *SpeedButton11;
	TSpeedButton *SpeedButton12;
	TLabel *Label15;
	TListBox *lbPostProcessing;
	TTabSheet *tsOptions;
	TGroupBox *gbBrush;
	TShape *sCB1TransparentColour;
	TShape *sCB2TransparentColour;
	TLabel *Label17;
	TLabel *Label18;
	TBitBtn *bCustomBrush1;
	TBitBtn *bCopyBrush2ColoursSource;
	TCheckBox *cbCB1Transparent;
	TBitBtn *bCustomBrush2;
	TCheckBox *cbCB2Transparent;
	TBitBtn *bCopyBrush1ColoursSource;
	TGroupBox *gbColourCycling;
	TLabel *Label13;
	TLabel *Label14;
	TLabel *lSpoon;
	TLabel *Label23;
	TLabel *Label24;
	TLabel *Label1;
	TColorListBox *clbSource;
	TColorListBox *clbTarget;
	TBitBtn *bAddColour;
	TBitBtn *bDeleteColour;
	TBitBtn *bOpenSourceColours;
	TBitBtn *bSaveSourceColours;
	TBitBtn *BitBtn5;
	TBitBtn *BitBtn6;
	TBitBtn *bOpenTargetColours;
	TBitBtn *bSaveTargetColours;
	TBitBtn *bColourUp;
	TBitBtn *bColourDown;
	TBitBtn *BitBtn10;
	TBitBtn *BitBtn11;
	TBitBtn *sbClearSourceColours;
	TBitBtn *sbClearTargetColours;
	TColorListBox *clbUser;
	TBitBtn *BitBtn12;
	TBitBtn *BitBtn13;
	TComboBox *cbTargetSkip;
	TBitBtn *bSaveAutomation;
	TBitBtn *bLoadAutomation;
	TColorDialog *cdColours;
	TSaveDialog *sdSaveBrush;
	TOpenDialog *odLoadBrush;
	void __fastcall SpeedButton15Click(TObject *Sender);
	void __fastcall bLoadAutomationClick(TObject *Sender);
	void __fastcall bOKClick(TObject *Sender);
	void __fastcall bSaveAutomationClick(TObject *Sender);
	void __fastcall bAddColourClick(TObject *Sender);
	void __fastcall bCustomBrush1Click(TObject *Sender);
	void __fastcall bColourUpClick(TObject *Sender);
	void __fastcall bColourDownClick(TObject *Sender);
	void __fastcall bCopyBrush1ColoursSourceClick(TObject *Sender);
	void __fastcall bDeleteColourClick(TObject *Sender);
	void __fastcall sbClearSourceColoursClick(TObject *Sender);
	void __fastcall BitBtn12Click(TObject *Sender);
	void __fastcall bOpenSourceColoursClick(TObject *Sender);
	void __fastcall bSaveSourceColoursClick(TObject *Sender);
	void __fastcall lbActionsDblClick(TObject *Sender);
	void __fastcall sbClearClick(TObject *Sender);
	void __fastcall sbMirrorClick(TObject *Sender);
	void __fastcall sbRemoveSelectedClick(TObject *Sender);
	void __fastcall sCB1TransparentColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
	void __fastcall sbCyclingLinearClick(TObject *Sender);
	void __fastcall sRevealColourMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
private:	// User declarations

	std::vector<std::wstring> CustomBrush[2];

	MatrixMode aiMode;
	int aiWidth = 0;
	int aiHeight = 0;

	void SaveAutomation(const std::wstring);


	void LoadAutomation(const std::wstring);
	int LoadDataParameterType(const std::wstring);


	int GetActionIDFromName(const std::wstring);

	void LoadColours(TColorListBox*, const std::wstring);
	void SaveColours(TColorListBox*, const std::wstring);

	void SetGuiLanguageText();


public:		// User declarations
	__fastcall TfrmAutomate(TComponent* Owner);

	std::wstring LastFileName = L"";

	RGBPaletteColours RGPC;

	void SetFromAutomationInput(AutomationInput &, std::vector<std::wstring> &, std::vector<int> &, ActionObject ao);
	void SetAutomationInputFromGui(AutomationInput &, ActionObject &ao);

	void SetCaption(const std::wstring);
};


bool OpenAutomate(AutomationInput &, RGBPaletteColours rgbpc, std::vector<std::wstring> &layers, std::vector<int> &colours, ActionObject &ao);

//---------------------------------------------------------------------------
extern PACKAGE TfrmAutomate *frmAutomate;
//---------------------------------------------------------------------------
#endif
