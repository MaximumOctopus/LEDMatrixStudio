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

#ifndef FrameLayerPanelH
#define FrameLayerPanelH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Grids.hpp>
#include <Vcl.Menus.hpp>

#include "TheMatrix.h"

//---------------------------------------------------------------------------
class TframeLayers : public TFrame
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TSpeedButton *sbLayersRename;
	TSpeedButton *sbDeleteLayer;
	TSpeedButton *sbAddNewLayer;
	TSpeedButton *sbLayerPanelClose;
	TSpeedButton *sbLayerUp;
	TSpeedButton *sbLayerDown;
	TCheckBox *cbSyncAllLayers;
	TStringGrid *sgLayers;
	TPopupMenu *puLayerTable;
	TMenuItem *Clearselectedlayer1;
	void __fastcall sbLayerUpClick(TObject *Sender);
	void __fastcall sbLayersRenameClick(TObject *Sender);
	void __fastcall sbLayerPanelCloseClick(TObject *Sender);
	void __fastcall sbLayerDownClick(TObject *Sender);
	void __fastcall sbDeleteLayerClick(TObject *Sender);
	void __fastcall sbAddNewLayerClick(TObject *Sender);
	void __fastcall sgLayersSelectCell(TObject *Sender, int ACol, int ARow, bool &CanSelect);
	void __fastcall sgLayersClick(TObject *Sender);
	void __fastcall Clearselectedlayer1Click(TObject *Sender);

private:

	static const int CCellVisible = 0;
	static const int CCellLocked  = 1;
	static const int CCellName    = 2;

public:
	__fastcall TframeLayers(TComponent* Owner);

	TheMatrix *ParentMatrix;

	void SetGuiLanguageText();
	void UpdateLayerTable();

    void UpdateExisting();

	void SetSyncAll(bool);
	bool GetSyncAll();

	// callbacks
    std::function<void(TframeLayers*)> OnClose;
	std::function<void(int)> OnMenu;
};
//---------------------------------------------------------------------------
extern PACKAGE TframeLayers *frameLayers;
//---------------------------------------------------------------------------
#endif
