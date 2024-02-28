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
	TBevel *Bevel19;
	TBitBtn *bOK;
	TBitBtn *bCancel;
	TGroupBox *gbAnimation;
	TLabel *Label3;
	TLabel *Label5;
	TGroupBox *GroupBox4;
	TLabel *Label4;
	TComboBox *ComboBox7;
	TComboBox *ComboBox8;
	TComboBox *ComboBox9;
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
	TGroupBox *GroupBox2;
	TLabel *Label2;
	TComboBox *ComboBox1;
	TComboBox *ComboBox2;
	TComboBox *ComboBox3;
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
	TGroupBox *GroupBox6;
	TLabel *Label8;
	TComboBox *ComboBox5;
	TComboBox *ComboBox6;
	TComboBox *ComboBox10;
	TCheckBox *cbClearAll;
	TGroupBox *gbPixelShape;
	TShape *shapeSquare;
	TShape *shapeCircle;
	TShape *shapeRoundRect;
	TGroupBox *GroupBox8;
	TLabel *Label10;
	TComboBox *ComboBox4;
	TComboBox *ComboBox11;
	TComboBox *ComboBox12;
	TMemo *mHelp;
	TColorDialog *cdNewProject;
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

	void UpdateHelp(MatrixMode);

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
