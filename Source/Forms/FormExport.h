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

#ifndef FormExportH
#define FormExportH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "MatrixConstants.h"
#include "TheMatrix.h"
//---------------------------------------------------------------------------
class TfrmExport : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TGroupBox *gbProfiles;
	TBevel *Bevel2;
	TBitBtn *sbSave;
	TBitBtn *sbOpen;
	TComboBox *cbProfileList;
	TBitBtn *sbDelete;
	TGroupBox *GroupBox6;
	TBitBtn *bCancel;
	TBitBtn *bExport;
	TBitBtn *bCopyToClipboard;
	TBitBtn *bClose;
	TCheckBox *cbAutoPreview;
	TBitBtn *bBuildCode;
	TPageControl *pcExport;
	TTabSheet *tsCode;
	TPanel *Panel2;
	TGroupBox *gbSource;
	TSpeedButton *sbDataRows;
	TSpeedButton *sbDataColumns;
	TLabel *lFrame;
	TLabel *Label2;
	TLabel *lSelectiveOutput;
	TLabel *Label9;
	TComboBox *cbDirection;
	TComboBox *cbScanDirection;
	TEdit *eFrameStart;
	TEdit *eFrameEnd;
	TCheckBox *cbOptimise;
	TEdit *eSelectiveStart;
	TEdit *eSelectiveEnd;
	TGroupBox *gbLSB;
	TSpeedButton *sbLSBLeft;
	TSpeedButton *sbLSBRight;
	TGroupBox *gbExportFormat;
	TComboBox *cbLanguageFormat;
	TCheckBox *cbIncludeExample;
	TGroupBox *gbNumberFormat;
	TSpeedButton *sbNumberDecimal;
	TSpeedButton *sbNumberBinary;
	TSpeedButton *sbNumberHex;
	TGroupBox *gbNumberGrouping;
	TSpeedButton *sbNumberSize8bit;
	TSpeedButton *sbNumberSize16bit;
	TSpeedButton *sbNumberSize32bit;
	TSpeedButton *sbNumberSize8bitSwap;
	TSpeedButton *sbNumberSize16bitSwap;
	TGroupBox *gbEachLine;
	TSpeedButton *sbOutputRow;
	TSpeedButton *sbOutputFrame;
	TSpeedButton *sbOutputBytes;
	TComboBox *cbLineCount;
	TGroupBox *gbRGB;
	TSpeedButton *sbRGB;
	TSpeedButton *sbBGR;
	TSpeedButton *sbGRB;
	TShape *shapeBackgroundPixels;
	TLabel *Label1;
	TSpeedButton *sbBRG;
	TLabel *Label6;
	TLabel *Label7;
	TCheckBox *cbChangeBackgroundPixels;
	TEdit *groupBoxRGBBrightness;
	TGroupBox *gbNumberGroupingRGB;
	TSpeedButton *sbNumberSizeRGB8bits;
	TSpeedButton *sbNumberSizeRGB32bits;
	TGroupBox *gbRGBColourSpace;
	TSpeedButton *sbCSRGB32;
	TSpeedButton *sbCSRGB565;
	TPanel *Panel4;
	TRichEdit *reExport;
	TPanel *pPreviewStatus;
	TTabSheet *tsBinary;
	TPanel *Panel3;
	TGroupBox *gbSourceBinary;
	TSpeedButton *sbBinaryDataRows;
	TSpeedButton *sbBinaryDataColumns;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *lBinarySelectiveOutput;
	TLabel *Label10;
	TComboBox *cbBinaryDirection;
	TComboBox *cbBinaryScanDirection;
	TEdit *eBinaryFrameStart;
	TEdit *eBinaryFrameEnd;
	TCheckBox *cbBinaryOptimise;
	TEdit *eBinarySelectiveStart;
	TEdit *eBinarySelectiveEnd;
	TGroupBox *gbLSBBinary;
	TSpeedButton *sbBinaryLSBLeft;
	TSpeedButton *sbBinaryLSBRight;
	TGroupBox *gbNumberGroupingBinary;
	TSpeedButton *sbBinaryNumberSize8bit;
	TSpeedButton *sbBinaryNumberSize8bitSwap;
	TSpeedButton *sbBinaryNumberSize16bitSwap;
	TGroupBox *gbBinaryRGB;
	TSpeedButton *sbBinaryRGB;
	TSpeedButton *sbBinaryBGR;
	TSpeedButton *sbBinaryGRB;
	TShape *shapeBinaryBackgroundPixels;
	TLabel *Label5;
	TSpeedButton *sbBinaryBRG;
	TLabel *Label8;
	TLabel *Label11;
	TCheckBox *cbBinaryChangeBackgroundPixels;
	TEdit *groupBoxBinaryRGBBrightness;
	TGroupBox *gbNumberGroupingBinaryRGB;
	TSpeedButton *sbBinaryNumberSizeRGB8bits;
	TGroupBox *gbFileContents;
	TRadioButton *rbSaveAnimation;
	TRadioButton *rbSaveFrame;
	TGroupBox *gbBinaryColourSpaceRGB;
	TSpeedButton *sbBCSRGB32;
	TSpeedButton *sbBCSRGB565;
	TMemo *mBinary;
	TSaveDialog *sdExport;
	TColorDialog *cdExport;
	TBitBtn *bResetCode;
	TBitBtn *bResetBinary;
	void __fastcall shapeBackgroundPixelsMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
	void __fastcall sbRGBClick(TObject *Sender);
	void __fastcall sbNumberSize8bitClick(TObject *Sender);
	void __fastcall cbDirectionChange(TObject *Sender);
	void __fastcall cbOptimiseClick(TObject *Sender);
	void __fastcall bCopyToClipboardClick(TObject *Sender);
	void __fastcall bExportClick(TObject *Sender);
	void __fastcall bBuildCodeClick(TObject *Sender);
	void __fastcall FormConstrainedResize(TObject *Sender, int &MinWidth, int &MinHeight,
          int &MaxWidth, int &MaxHeight);
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall sbSaveClick(TObject *Sender);
	void __fastcall sbDeleteClick(TObject *Sender);
	void __fastcall sbOpenClick(TObject *Sender);
	void __fastcall sbBinaryDataRowsClick(TObject *Sender);
	void __fastcall bCloseClick(TObject *Sender);
	void __fastcall reExportMouseWheelDown(TObject *Sender, TShiftState Shift, TPoint &MousePos,
          bool &Handled);
	void __fastcall reExportMouseWheelUp(TObject *Sender, TShiftState Shift, TPoint &MousePos,
          bool &Handled);
	void __fastcall bResetCodeClick(TObject *Sender);
	void __fastcall bResetBinaryClick(TObject *Sender);
private:

    int LastScrollValue = 0;
	std::vector<std::wstring> Output;

	void CreateExportOptions();
	void CreateBinaryExportOptions();

	void ToggleControlStatus(bool);

    void AddPreviewSection();

	void Preview();
    void PreviewCode();
	void PreviewBinary();
	void UpdatePreview();

    void AddExampleCode();

	bool ValidateNumberEdit(TEdit*);

	void LoadProfile(const std::wstring);

	bool SaveBinaryData(const std::wstring);

    void PopulateProfileList();

public:
	__fastcall TfrmExport(TComponent* Owner);

	TheMatrix *matrix = nullptr;

	bool IsBuilding = false;
	bool IsUpdating = false;

	std::wstring ProfileExtension = L"";
	MatrixMode Mode = MatrixMode::kNone;
	int LastRow = 0;
	int MaxFrameCount = 0;
	int PixelCount = 0;
	int PixelCountFrame = 0;

	ExportOptions InternalEO;

    void BuildUI(ExportOptions);

    void SetGuiLanguageText();

	void SetMaxFrameCount(int);

    void BuildFromProfile(ExportOptions eeo);
};

//---------------------------------------------------------------------------

void OpenExportData(TheMatrix*, ExportOptions&, ExportSource, MatrixMode);

//---------------------------------------------------------------------------
extern PACKAGE TfrmExport *frmExport;
//---------------------------------------------------------------------------
#endif
