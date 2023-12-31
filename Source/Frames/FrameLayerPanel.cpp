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

#include "FrameLayerPanel.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"

#include "FormAddLayer.h"
#include "Utility.h"

extern LanguageHandler *GLanguage;

#pragma package(smart_init)
#pragma resource "*.dfm"
TframeLayers *frameLayers;

__fastcall TframeLayers::TframeLayers(TComponent* Owner)
	: TFrame(Owner)
{
}


void __fastcall TframeLayers::sbLayerUpClick(TObject *Sender)
{
	ParentMatrix->MoveUp(ParentMatrix->GetCurrentLayer());
}


void __fastcall TframeLayers::sbLayersRenameClick(TObject *Sender)
{
	if (sgLayers->Selection.Top >= 1)
	{
		std::wstring name = InputBox(GLanguageHandler->Text[kChangeLayerName].c_str(),
									 GLanguageHandler->Text[kNewName].c_str(),
									 sgLayers->Cells[CCellLocked][sgLayers->Selection.Top]).c_str();

		if (!name.empty())
		{
			ParentMatrix->SetLayerName(name, sgLayers->RowCount - 1 - sgLayers->Selection.Top);
		}
	}
}


void __fastcall TframeLayers::sbLayerPanelCloseClick(TObject *Sender)
{
	if (OnClose) OnClose(this);
}


void __fastcall TframeLayers::sbLayerDownClick(TObject *Sender)
{
	ParentMatrix->MoveDown(ParentMatrix->GetCurrentLayer());
}


void __fastcall TframeLayers::sbAddNewLayerClick(TObject *Sender)
{
	std::vector<std::wstring> layers;

	for (int t = 0; t < ParentMatrix->GetLayerCount(); t++)
	{
		layers.push_back(ParentMatrix->GetLayerName(t));
	}

	AddObject ao = OpenAddLayer(layers);

	if (ao.Process)
	{
		if (!ao.Name.empty())
		{
			if (ao.CopyFrom)
			{
				ParentMatrix->AddLayerAsCopy(ao.Name, ao.CopyLayer);
			}
			else
			{
				ParentMatrix->AddLayer(ao.Name);
			}
		}
	}
}


void __fastcall TframeLayers::sbDeleteLayerClick(TObject *Sender)
{
	if (MessageDlg(Utility::WS2US(GLanguageHandler->Text[kDeleteCurrentLayer] + L"\n\n" + GLanguageHandler->Text[kThisCannotBeUndone]), mtWarning, mbYesNo, 0) == mrYes)
	{
		ParentMatrix->DeleteLayer(sgLayers->RowCount - 1 - sgLayers->Selection.Top);
	}
}


void __fastcall TframeLayers::sgLayersClick(TObject *Sender)
{
	ParentMatrix->SetCurrentLayer(sgLayers->RowCount - 1 - sgLayers->Selection.Top);

	if (ParentMatrix->GetCurrentLayer() == 0)
	{
		sbLayerDown->Enabled = false;
	}
	else
	{
		sbLayerDown->Enabled = true;
	}

	if (ParentMatrix->GetCurrentLayer() == ParentMatrix->GetLayerCount() - 1)
	{
		sbLayerUp->Enabled = false;
	}
	else
	{
		sbLayerUp->Enabled = true;
	}
}


void __fastcall TframeLayers::sgLayersSelectCell(TObject *Sender, int ACol, int ARow, bool &CanSelect)
{
	CanSelect = true;

	int layer = 0;

	if (ACol == 0 && ARow > 0)
	{
		layer = sgLayers->RowCount - 1 - ARow;

		if (sgLayers->Cells[ACol][ARow] == L"x")
		{
			sgLayers->Cells[ACol][ARow] = L"";

			ParentMatrix->SetVisibility(layer, false);
		}
		else
		{
			sgLayers->Cells[ACol][ARow] = L"x";

			ParentMatrix->SetVisibility(layer, true);
		}
	}
	else if (ACol == 1 && ARow > 0)
	{
		layer = sgLayers->RowCount - 1 - ARow;

		if (sgLayers->Cells[ACol][ARow] == L"L")
		{
			sgLayers->Cells[ACol][ARow] = L"";

			ParentMatrix->UnlockLayer(layer);
		}
		else
		{
			sgLayers->Cells[ACol][ARow] = L"L";

			ParentMatrix->LockLayer(layer);
		}
	}
}


void __fastcall TframeLayers::Clearselectedlayer1Click(TObject *Sender)
{
	if (OnMenu)
	{
		TMenuItem *mi = (TMenuItem*)Sender;

		OnMenu(mi->Tag);
	}
}


void TframeLayers::SetGuiLanguageText()
{
	cbSyncAllLayers->Caption = GLanguageHandler->Text[kSyncAll].c_str();
}


void TframeLayers::UpdateLayerTable()
{
	int count = ParentMatrix->GetLayerCount();

	sgLayers->RowCount = count + 1;

	for (int t = 0; t < count; t++)
	{
		sgLayers->Cells[CCellName][1 + count - 1 - t] = ParentMatrix->GetLayerName(t).c_str();

		if (ParentMatrix->IsVisible(t))
		{
			sgLayers->Cells[CCellVisible][1 + count - 1 - t] = L"x";
		}
		else
		{
			if (count == 1)
			{
				ParentMatrix->SetVisibility(t, true);

				sgLayers->Cells[CCellVisible][1 + count - 1 - t] = L"x";
			}
			else
			{
				sgLayers->Cells[CCellVisible][1 + count - 1 - t] = L"";
			}
		}

		if (ParentMatrix->IsLayerLocked(t))
		{
			sgLayers->Cells[CCellLocked][1 + count - 1 - t] = L"L";
		}
		else
		{
			sgLayers->Cells[CCellLocked][1 + count - 1 - t] = L"";
		}
	}

	if (ParentMatrix->GetSoftwareMode() == SoftwareMode::kAnimation && count > 1)
	{
		sbDeleteLayer->Enabled = true;
	}
	else
	{
		sbDeleteLayer->Enabled = false;
	}

	if (ParentMatrix->GetSoftwareMode() == SoftwareMode::kAnimation)
	{
		sbAddNewLayer->Enabled = true;
	}
	else
	{
		sbAddNewLayer->Enabled = false;
	}

	TGridRect selection;

	selection.Left = 0;
	selection.Top = 1 + count - 1 - ParentMatrix->GetCurrentLayer();
	selection.Right = 1;
	selection.Bottom = 1 + count - 1 - ParentMatrix->GetCurrentLayer();

	sgLayers->Selection = selection;

	if (ParentMatrix->GetCurrentLayer() == 0)
	{
		sbLayerDown->Enabled = false;
	}
	else
	{
		sbLayerDown->Enabled = true;
	}

	if (ParentMatrix->GetCurrentLayer() == ParentMatrix->GetLayerCount() - 1)
	{
		sbLayerUp->Enabled = false;
	}
	else
	{
		sbLayerUp->Enabled = true;
	}
}


// updates the lock/visible status of currently available layers
void TframeLayers::UpdateExisting()
{
	int count = ParentMatrix->GetLayerCount();

	sgLayers->RowCount = count + 1;

	for (int t = 0; t < count; t++)
	{
		sgLayers->Cells[CCellName][1 + count - 1 - t] = ParentMatrix->GetLayerName(t).c_str();

		if (ParentMatrix->IsVisible(t))
		{
			sgLayers->Cells[CCellVisible][1 + count - 1 - t] = L"x";
		}
		else
		{
			if (count == 1)
			{
				ParentMatrix->SetVisibility(t, true);

				sgLayers->Cells[CCellVisible][1 + count - 1 - t] = L"x";
			}
			else
			{
				sgLayers->Cells[CCellVisible][1 + count - 1 - t] = L"";
			}
		}

		if (ParentMatrix->IsLayerLocked(t))
		{
			sgLayers->Cells[CCellLocked][1 + count - 1 - t] = L"L";
		}
		else
		{
			sgLayers->Cells[CCellLocked][1 + count - 1 - t] = L"";
		}
	}
}


void TframeLayers::SetSyncAll(bool sync)
{
	cbSyncAllLayers->Checked = sync;
}


bool TframeLayers::GetSyncAll()
{
	return cbSyncAllLayers->Checked;
}
