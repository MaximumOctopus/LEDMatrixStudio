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

#ifndef FormOptimiseH
#define FormOptimiseH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>

#include <vector>

#include "MatrixConstants.h"
#include "TheMatrix.h"

//---------------------------------------------------------------------------
class TfrmOptimise : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TSpeedButton *sbOptimise;
	TSpeedButton *sbCopyOutput;
	TMemo *mMemo;
	TMemo *mData;
	TPanel *Panel1;
	TGroupBox *gbOutputOptions;
	TLabel *Label4;
	TLabel *Label5;
	TLabel *Label7;
	TLabel *Label6;
	TComboBox *cbDataSize;
	TComboBox *cbLanguageFormat;
	TComboBox *cbPerRow;
	void __fastcall sbCopyOutputClick(TObject *Sender);
	void __fastcall sbOptimiseClick(TObject *Sender);
private:

	int MaxFrames = 0;
	bool HexFormat = false;
	int ColumnsLSB = 0;
	int RowsLSB = 0;
	int ExportFormat = 0;
	int ColumnsDirection = 0;
	int XType = 0;
	int beforeData = 0;
	int afterData = 0;

	std::vector<std::wstring> MatrixData;

	NumberSize GetDataSize();

	void PopulateMatrixData();

	void SetGuiLanguageText();

public:		// User declarations
	__fastcall TfrmOptimise(TComponent* Owner);

	TheMatrix *thematrix = nullptr;
};

void OpenOptimise(TheMatrix *thematrix);

//---------------------------------------------------------------------------
extern PACKAGE TfrmOptimise *frmOptimise;
//---------------------------------------------------------------------------
#endif
