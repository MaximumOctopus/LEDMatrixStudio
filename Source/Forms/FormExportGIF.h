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

#ifndef FormExportGIFH
#define FormExportGIFH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Samples.Spin.hpp>
#include <Vcl.Dialogs.hpp>

//---------------------------------------------------------------------------
class TfrmExportGIF : public TForm
{
__published:	// IDE-managed Components
	TImage *Image1;
	TBevel *Bevel1;
	TBitBtn *bOk;
	TBitBtn *bCancel;
	TGroupBox *GroupBox1;
	TLabel *Label1;
	TSpeedButton *bSave;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *Label5;
	TShape *ShapeNorfolkDigital;
	TLabel *lAnimationSpeed;
	TLabel *lAnimationSpeedHelp;
	TEdit *eFileName;
	TEdit *ePixelSize;
	TRadioButton *rbSquare;
	TRadioButton *rbCircle;
	TSpinEdit *seAnimationSpeed;
	TRadioButton *rbRoundRect;
	TSaveDialog *sdExportGIF;
	TColorDialog *cdExportGIF;

	void __fastcall ShapeNorfolkDigitalMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
	void __fastcall eFileNameChange(TObject *Sender);
	void __fastcall bSaveClick(TObject *Sender);
private:

    void SetGuiLanguageText();

public:
	__fastcall TfrmExportGIF(TComponent* Owner);
};

//---------------------------------------------------------------------------

ExportGIFSettings OpenExportGIF(ExportGIFSettings);

//---------------------------------------------------------------------------
extern PACKAGE TfrmExportGIF *frmExportGIF;
//---------------------------------------------------------------------------
#endif
