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

#ifndef FrameUndoPanelH
#define FrameUndoPanelH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
//---------------------------------------------------------------------------
class TframeUndos : public TFrame
{
__published:	// IDE-managed Components
	TListBox *lbUndos;
	void __fastcall lbUndosClick(TObject *Sender);
private:

public:
	__fastcall TframeUndos(TComponent* Owner);

	void SetUndos(int);

	// callbacks
	std::function<void(int)> OnUndoSelected;
};

//---------------------------------------------------------------------------
extern PACKAGE TframeUndos *frameUndos;
//---------------------------------------------------------------------------
#endif
