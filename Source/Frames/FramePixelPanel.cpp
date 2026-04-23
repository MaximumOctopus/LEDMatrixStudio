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

#include <vcl.h>
#pragma hdrstop

#include "FramePixelPanel.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TframePixel *framePixel;
//---------------------------------------------------------------------------
__fastcall TframePixel::TframePixel(TComponent* Owner)
	: TFrame(Owner)
{
}


void TframePixel::Build(int x, int y, int group, int order)
{
	Enable();

	ePositionX->Text = x;
	ePositionY->Text = y;
	eGroup->Text = group;

	lOrder->Caption = order;
	lId->Caption = SelectedPixelId;
}


void TframePixel::Enable()
{
	ePositionX->Enabled = true;
	ePositionY->Enabled = true;
	eGroup->Enabled = true;

	ePositionX->Text = L"";
	ePositionY->Text = L"";
	eGroup->Text = L"";

	lOrder->Caption = L"";
	lId->Caption = L"";
}


void TframePixel::Disable()
{
	ePositionX->Enabled = false;
	ePositionY->Enabled = false;
	eGroup->Enabled = false;

	ePositionX->Text = L"";
	ePositionY->Text = L"";
	eGroup->Text = L"";

	lOrder->Caption = L"";
	lId->Caption = L"";
}


void __fastcall TframePixel::ePositionXExit(TObject *Sender)
{
	int x = ePositionX->Text.ToIntDef(-1);

	if (x >= 0)
	{
		if (OnNewX)
		{
			OnNewX(x);
		}
	}
}


void __fastcall TframePixel::ePositionYExit(TObject *Sender)
{
	int y = ePositionY->Text.ToIntDef(-1);

	if (y >= 0)
	{
		if (OnNewY)
		{
			OnNewY(y);
		}
	}
}


void __fastcall TframePixel::eGroupExit(TObject *Sender)
{
	int g = eGroup->Text.ToIntDef(-1);

	if (g >= 0)
	{
		if (OnNewGroup)
		{
			OnNewGroup(g);
		}
	}
}
