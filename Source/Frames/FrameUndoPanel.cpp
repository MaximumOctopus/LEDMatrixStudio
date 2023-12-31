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

#include <vcl.h>
#pragma hdrstop

#include "FrameUndoPanel.h"
#include "Utility.h"

#pragma package(smart_init)
#pragma resource "*.dfm"
TframeUndos *frameUndos;

__fastcall TframeUndos::TframeUndos(TComponent* Owner)
	: TFrame(Owner)
{
}


void __fastcall TframeUndos::lbUndosClick(TObject *Sender)
{
	if (OnUndoSelected) OnUndoSelected(lbUndos->ItemIndex);
}


void TframeUndos::SetUndos(int count)
{
	lbUndos->Clear();

	for (int t = 0; t < count; t++)
	{
		lbUndos->Items->Add(Utility::WS2US(L"Undo " + std::to_wstring(t)));
	}
}
