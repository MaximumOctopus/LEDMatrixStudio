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

#ifndef FormNewProjectH
#define FormNewProjectH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.pngimage.hpp>

#include "ProjectSettings.h"
//---------------------------------------------------------------------------


class TfrmNewProject : public TForm
{
__published:	// IDE-managed Components
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *gbAnimation;
	TLabel *Label3;
	TLabel *Label5;
	TComboBox *cbFrames;
	TPageControl *pcNew;
	TTabSheet *tsCustom;
	TGroupBox *gbMatrixOptions;
	TLabel *Label1;
	TShape *sBackground;
	TLabel *lBackground;
	TLabel *Label11;
	TComboBox *cbHeight;
	TComboBox *cbWidth;
	TComboBox *cbMatrixType;
	TRadioButton *rbCommon;
	TRadioButton *rbAll;
	TComboBox *cbCustomShape;
	TComboBox *cbCustomShapeParam;
	TTabSheet *tsFromPreset;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *Label9;
	TLabel *lPresetType;
	TLabel *lPresetWidth;
	TLabel *lPresetHeight;
	TComboBox *cbPresets;
	TGroupBox *GroupBox5;
	TCheckBox *cbClearAll;
	TGroupBox *gbPixelShape;
	TShape *shapeSquare;
	TShape *shapeCircle;
	TShape *shapeRoundRect;
	TMemo *mHelp;
	TColorDialog *cdNewProject;
	TTabSheet *tsFreeform;
	TGroupBox *gbFreeform;
	TShape *sBackgroundFreeform;
	TLabel *lBackgroundFreeform;
	TComboBox *cbMatrixTypeFreeform;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall bOKClick(TObject *Sender);
	void __fastcall cbCustomShapeChange(TObject *Sender);
	void __fastcall cbMatrixTypeChange(TObject *Sender);
	void __fastcall cbPresetsChange(TObject *Sender);
	void __fastcall cbWidthChange(TObject *Sender);
	void __fastcall rbCommonClick(TObject *Sender);
	void __fastcall sBackgroundMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall shapeSquareMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
private:

	void SetGUILanguageText();

	void UpdateHelp(MatrixColourMode);

    void BuildPresetList();

public:

	void BuildFrom(ProjectSettings&);
	void SetTo(ProjectSettings&);

	System::UnicodeString OldWidth = "";
	System::UnicodeString OldHeight = "";

	bool ClearStatus = false;

	__fastcall TfrmNewProject(TComponent* Owner);
};

//---------------------------------------------------------------------------

ProjectSettings OpenNewProject(ProjectSettings &OldProjectSettings, bool appstatus);

//---------------------------------------------------------------------------
extern PACKAGE TfrmNewProject *frmNewProject;
//---------------------------------------------------------------------------
#endif
