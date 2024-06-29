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

#include <algorithm>
#include <fstream>
#include <vector>

#include "ActionObject.h"
#include "AutomationConstants.h"
#include "ColourUtility.h"
#include "Convert.h"
#include "Formatting.h"
#include "LanguageConstants.h"
#include "LanguageHandler.h"
#include "SystemSettings.h"
#include "Utility.h"

#include "FormAutomate.h"
#include "FormNewBrush.h"

extern LanguageHandler *GLanguageHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmAutomate *frmAutomate;

//---------------------------------------------------------------------------

bool OpenAutomate(AutomationInput &ai, RGBPaletteColours rgbpc, std::vector<std::wstring> &layers, std::vector<int> &colours, ActionObject &ao)
{
	TfrmAutomate *frmAutomate = new TfrmAutomate(Application);

	bool UserAccept = false;

	frmAutomate->SetFromAutomationInput(ai, layers, colours, ao);

	frmAutomate->LastFileName = ao.LastFileName;

	//if not(DirectoryExists(ExtractFilePath(FLastFileName))) then
	//	FLastFileName = ExtractFilePath(Application->ExeName) + "automate\";

	frmAutomate->SetCaption(frmAutomate->LastFileName);

	frmAutomate->RGPC.Left   = rgbpc.Left;
	frmAutomate->RGPC.Middle = rgbpc.Middle;
	frmAutomate->RGPC.Right  = rgbpc.Right;

	for (int t = 0; t < 21; t++)
	{
		frmAutomate->RGPC.History[t] = rgbpc.History[t];
	}

	if (frmAutomate->ShowModal() == mrOk)
	{
		UserAccept = true;

		frmAutomate->SetAutomationInputFromGui(ai, ao);

		ao.Clear(); // resets automation parameters
	}

	delete frmAutomate;

	return UserAccept;
}


__fastcall TfrmAutomate::TfrmAutomate(TComponent* Owner)
	: TForm(Owner)
{
	SetGuiLanguageText();
}


void TfrmAutomate::SetFromAutomationInput(AutomationInput &ai, std::vector<std::wstring> &layers, std::vector<int> &colours, ActionObject ao)
{
	aiMode = ai.Mode;
	aiWidth = ai.Width;
	aiHeight = ai.Height;

	if (ai.Width % 2 != 0 && ai.Height % 2 != 0)
	{
		for (int t = 0; t < kWipeOddCount; t++)
		{
			cbWipe->Items->Add(kAutomationActions[kActionsWipeOdd[t]].c_str());
		}
	}
	else if (ai.Width % 2 != 0)
	{
		for (int t = 0; t < kWipeOddWCount; t++)
		{
			cbWipe->Items->Add(kAutomationActions[kActionsWipeOddW[t]].c_str());
		}
	}
	else if (ai.Height % 2 != 0)
	{
		for (int t = 0; t < kWipeOddHCount; t++)
		{
			cbWipe->Items->Add(kAutomationActions[kActionsWipeOddH[t]].c_str());
		}
	}
	else
	{
		for (int t = 0; t < kWipeCount; t++)
		{
			cbWipe->Items->Add(kAutomationActions[kActionsWipe[t]].c_str());
		}
	}

	for (int t = 0; t < kRevealCount; t++)
	{
		cbReveal->Items->Add(kAutomationActions[kActionsReveal[t]].c_str());
	}

	cbWipe->ItemIndex   = 0;
	cbReveal->ItemIndex = 0;

	eFrameStart->Text = ai.FrameCurrent;
	eFrameEnd->Text = ai.FrameMax;

	// set from actionobject data (previous users settings if they exist)

	switch (ao.Source)
	{
	case AutomateSource::kFirstFrame:
		rbProcessMode1->Checked = true;
		break;
	case AutomateSource::kEachFrame:
		rbProcessMode2->Checked = true;
		break;
	case AutomateSource::kEachFrameInc :
		rbProcessMode3->Checked = true;
        break;
	}

	cbErase->Checked   = ao.EraseBehind;

	for (int t = 0; t < layers.size(); t++)
	{
		cbLayer->Items->Add(layers[t].c_str());
	}

	cbLayer->ItemIndex = ao.Layer;

	if (ao.ActionList.size() != 0)
	{
		for (int t = 0; t < ao.ActionList.size(); t++)
		{
			lbActions->Items->Add(kAutomationActions[ao.ActionList[t]].c_str());
		}

		bOK->Enabled = true;
	}

	if (ao.PostProcessList.size() != 0)
	{
		for (int t = 0; t < ao.PostProcessList.size(); t++)
		{
			lbPostProcessing->Items->Add(kAutomationActions[ao.PostProcessList[t]].c_str());
		}

		bOK->Enabled = true;
	}

	sRevealColour->Brush->Color = TColor(ao.ParameterRevealColour);

	for (int t = 0; t < ao.SourceColours.size(); t++)
	{
		TObject *ob = (TObject*)ao.SourceColours[t];

		clbSource->AddItem(ColourUtility::RGBPlusInteger(ao.SourceColours[t], 100).c_str(), ob);
	}

	for (int t = 0; t < ao.TargetColours.size(); t++)
	{
		TObject *ob = (TObject*)ao.SourceColours[t];

		clbTarget->AddItem(ColourUtility::RGBPlusInteger(ao.TargetColours[t], 100).c_str(), ob);
	}

	cbTargetSkip->ItemIndex = ao.TargetSkip;

	for (int t = 0; t < colours.size(); t++)
	{
		TObject *ob = (TObject*)colours[t];

		clbUser->AddItem(ColourUtility::RGBPlusInteger(colours[t], 100).c_str(), ob);
	}

	// == ensure that all changes are reflected after form closes ============

	ao.ActionList.clear();
	ao.PostProcessList.clear();
	ao.SourceColours.clear();
	ao.TargetColours.clear();
}


void TfrmAutomate::SetAutomationInputFromGui(AutomationInput &ai, ActionObject &ao)
{
	if (!LastFileName.empty())
	{
		ao.LastFileName = LastFileName;
	}

	ao.EraseBehind = cbErase->Checked;

	if (rbProcessMode1->Checked)
	{
		ao.Source = AutomateSource::kFirstFrame;
	}
	else if (rbProcessMode2->Checked)
	{
		ao.Source = AutomateSource::kEachFrame;
	}
	else
	{
		ao.Source = AutomateSource::kEachFrameInc;
	}

	ao.FrameStart = StrToIntDef(eFrameStart->Text, 0) - 1;  // matrix frames indexed from zero
	ao.FrameEnd   = StrToIntDef(eFrameEnd->Text, 0) - 1;    // matrix frames indexed from zero

	ao.ActionList.clear();

	ao.Layer = cbLayer->ItemIndex;

	for (int t = 0; t < lbActions->Items->Count; t++)
	{
		int aoid = GetActionIDFromName(lbActions->Items->Strings[t].c_str());

		if (aoid != -1)
		{
			ao.ActionList.push_back(aoid);
		}
	}

	for (int t = 0; t < lbPostProcessing->Items->Count; t++)
	{
		int aoid = GetActionIDFromName(lbPostProcessing->Items->Strings[t].c_str());

		if (aoid != -1)
		{
			ao.PostProcessList.push_back(aoid);
		}
	}

	// brush 1

	ao.Brushes[0].BrushData.clear();
	for (int t = 0; t < CustomBrush[0].size(); t++)
	{
		ao.Brushes[0].BrushData.push_back(CustomBrush[0][t]);
	}

	ao.Brushes[0].Transparent       = cbCB1Transparent->Checked;
	ao.Brushes[0].TransparentColour = sCB1TransparentColour->Brush->Color;

	// brush 2

	ao.Brushes[1].BrushData.clear();
	for (int t = 0; t < CustomBrush[1].size(); t++)
	{
		ao.Brushes[1].BrushData.push_back(CustomBrush[1][t]);
	}

	ao.Brushes[1].Transparent       = cbCB2Transparent->Checked;
	ao.Brushes[1].TransparentColour = sCB2TransparentColour->Brush->Color;

	ao.ParameterRevealColour = sRevealColour->Brush->Color;

	// ==============

	for (int t = 0; t < clbSource->Count; t++)
	{
		int colour = reinterpret_cast<int>(clbSource->Items->Objects[t]);

		ao.SourceColours.push_back(colour);
	}

	for (int t = 0; t < clbTarget->Count; t++)
	{
		int colour = reinterpret_cast<int>(clbTarget->Items->Objects[t]);

		ao.TargetColours.push_back(colour);
	}

	ao.TargetSkip = cbTargetSkip->ItemIndex;
}


void __fastcall TfrmAutomate::SpeedButton15Click(TObject *Sender)
{
	TSpeedButton *sb = (TSpeedButton*)Sender;

	switch (sb->Tag)
	{
	case 0:
		lbActions->Items->Add(cbWipe->Text);
		break;
	case 1:
		lbActions->Items->Add(cbReveal->Text);
		break;
	}

	bOK->Enabled = true;
}


void __fastcall TfrmAutomate::bLoadAutomationClick(TObject *Sender)
{
	odLoadBrush->DefaultExt = L"automation";
	odLoadBrush->Filter     = GLanguageHandler->Text[kAutomationFiles] + L" (*.automation)|*.automation";

	if (!LastFileName.empty())
	{
		odLoadBrush->InitialDir = ExtractFilePath(LastFileName.c_str()).c_str();
	}
	else
	{
		std::wstring path = GSystemSettings->App.LMSFilePath + L"automate\\";

		odLoadBrush->InitialDir = path.c_str();
	}

	if (odLoadBrush->Execute())
	{
		LoadAutomation(odLoadBrush->FileName.c_str());

		if (lbActions->Count != 0)
		{
			bOK->Enabled = true;
		}

		bCopyBrush1ColoursSource->Enabled = (CustomBrush[0].size() != 0);
		bCopyBrush2ColoursSource->Enabled = (CustomBrush[1].size() != 0);

		LastFileName = odLoadBrush->FileName;

		SetCaption(LastFileName);
	}
}


void __fastcall TfrmAutomate::bOKClick(TObject *Sender)
{
	int fs = eFrameStart->Text.ToIntDef(-1);
	int fe = eFrameEnd->Text.ToIntDef(-1);

	if (fs == -1 || fe == -1)
	{
		MessageDlg(GLanguageHandler->Text[kInvalidFrameStartFrameEndValues].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
	}
	else
	{
		ModalResult = mrOk;
	}
}


void __fastcall TfrmAutomate::bSaveAutomationClick(TObject *Sender)
{
	sdSaveBrush->DefaultExt = "automation";
	sdSaveBrush->Filter     = GLanguageHandler->Text[kAutomationFiles] + L" (*.automation)|*.automation";

	if (!LastFileName.empty())
	{
		sdSaveBrush->InitialDir = ExtractFilePath(LastFileName.c_str()).c_str();
	}
	else
	{
		std::wstring path = GSystemSettings->App.LMSFilePath + L"automate\\";

		sdSaveBrush->InitialDir = path.c_str();
	}

	if (sdSaveBrush->Execute())
	{
		SaveAutomation(sdSaveBrush->FileName.c_str());

		LastFileName = sdSaveBrush->FileName;

		SetCaption(LastFileName);
	}
}
//---------------------------------------------------------------------------


#pragma region Tab_Brush
void __fastcall TfrmAutomate::bAddColourClick(TObject *Sender)
{
	if (cdColours->Execute())
	{
		TBitBtn *bb = (TBitBtn*)Sender;

		TObject *col = (TObject*)cdColours->Color;

		if (bb->Tag == 0)
		{
			clbSource->AddItem(ColourUtility::RGBPlusInteger(cdColours->Color, 100).c_str(), col);
		}
		else
		{
			clbTarget->AddItem(ColourUtility::RGBPlusInteger(cdColours->Color, 100).c_str(), col);
		}
	}
}


void __fastcall TfrmAutomate::bCustomBrush1Click(TObject *Sender)
{
	TBitBtn *bb = (TBitBtn*)Sender;

	int brushindex = bb->Tag;

	MatrixSettings settings;

	settings.Mode = aiMode;
	settings.Width      = aiWidth;
	settings.Height     = aiHeight;

	std::vector<std::wstring> brush;

	if (CustomBrush[brushindex].size() != 0)
	{
		for (int t = 0; t < CustomBrush[brushindex].size(); t++)
		{
			brush.push_back(CustomBrush[brushindex][t]);
		}
	}

	NewBrush newbrush = OpenNewBrush(brush, settings, RGPC);

	if (newbrush.Proceed)
	{
		CustomBrush[brushindex].clear();
	}

	for (int t = 0; t < brush.size(); t++)
	{
		CustomBrush[brushindex].push_back(brush[t]);
	}

	if (brushindex == 0)
	{
		bCopyBrush1ColoursSource->Enabled = true;
	}
	else
	{
		bCopyBrush2ColoursSource->Enabled = true;
	}
}
//---------------------------------------------------------------------------


void __fastcall TfrmAutomate::bColourUpClick(TObject *Sender)
{
	TColorListBox *clb = nullptr;

	TBitBtn *bb = (TBitBtn*)Sender;

	if (bb->Tag == 0)
	{
		clb = clbSource;
	}
	else
	{
		clb = clbTarget;
	}

	if (clb->ItemIndex != -1)
	{
		if (clb->ItemIndex > 0)
		{
			int index = clb->ItemIndex;

			clb->Items->Move(clb->ItemIndex, clb->ItemIndex - 1);

			clb->ItemIndex = index - 1;
		}
	}
}


void __fastcall TfrmAutomate::bColourDownClick(TObject *Sender)
{
	TColorListBox *clb = nullptr;

	TBitBtn *bb = (TBitBtn*)Sender;

	if (bb->Tag == 0)
	{
		clb = clbSource;
	}
	else
	{
		clb = clbTarget;
	}

	if (clb->ItemIndex != -1)
	{
		if (clb->ItemIndex < clb->Items->Count - 1)
		{
			int index = clb->ItemIndex;

			clb->Items->Move(clb->ItemIndex, clb->ItemIndex + 1);

			clb->ItemIndex = index + 1;
		}
	}
}


void __fastcall TfrmAutomate::bCopyBrush1ColoursSourceClick(TObject *Sender)
{
	TBitBtn *bb = (TBitBtn*)Sender;

	int brushindex = bb->Tag;

	std::vector<std::wstring> colourlist;
	//  lColourList->Sorted = true;

	std::wstring colour = L"";

	for (int row = 0; row < CustomBrush[brushindex].size(); row++)
	{
		for (int i = 0; CustomBrush[brushindex][row].length(); i++)
		{
			if (CustomBrush[brushindex][row][i] == L' ' || i == CustomBrush[brushindex][row].length())
			{
				if (std::find(colourlist.begin(), colourlist.end(), colour) == colourlist.end())
				{
					colourlist.push_back(colour);
				}

				colour = L"";
			}
			else
			{
				colour += CustomBrush[brushindex][row][i];
			}
		}
	}

	if (colourlist.size() != 0)
	{
		clbSource->Clear();

		for (int i = 0; i < colourlist.size(); i++)
		{
			TObject *ob = (TObject*)Convert::HexToInt(colourlist[i]);

			clbSource->AddItem(ColourUtility::RGBPlusInteger(Convert::HexToInt(colourlist[i]), 100).c_str(), ob);
		}
	}
}


void __fastcall TfrmAutomate::bDeleteColourClick(TObject *Sender)
{
	TBitBtn *bb = (TBitBtn*)Sender;

	if (bb->Tag == 0)
	{
		clbSource->DeleteSelected();
	}
	else
	{
		clbTarget->DeleteSelected();
	}
}


void __fastcall TfrmAutomate::sbClearSourceColoursClick(TObject *Sender)
{
	TSpeedButton *sb = (TSpeedButton*)Sender;

	if (sb->Tag == 0)
	{
		clbSource->Clear();
	}
	else
	{
		clbTarget->Clear();
	}
}


void __fastcall TfrmAutomate::BitBtn12Click(TObject *Sender)
{
	if (clbUser->ItemIndex != -1)
	{
		TSpeedButton *sb = (TSpeedButton*)Sender;

		int colour = reinterpret_cast<int>(clbUser->Items->Objects[clbUser->ItemIndex]);

		if (sb->Tag == 0)
		{
			clbSource->AddItem(ColourUtility::RGBPlusInteger(colour, 100).c_str(), (TObject*)colour);
		}
		else
		{
			clbTarget->AddItem(ColourUtility::RGBPlusInteger(colour, 100).c_str(), (TObject*)colour);
		}
	}
}


void __fastcall TfrmAutomate::bOpenSourceColoursClick(TObject *Sender)
{
	odLoadBrush->DefaultExt = L"colours";
	odLoadBrush->Filter     = GLanguageHandler->Text[kColourLists] + L" (*.colours)|*.colours";
	odLoadBrush->InitialDir = Utility::WS2US(GSystemSettings->App.LMSFilePath + L"automate\\colours\\");

	if (odLoadBrush->Execute())
	{
		TBitBtn *bb = (TBitBtn*)Sender;

		if (bb->Tag == 0)
		{
			LoadColours(clbSource, odLoadBrush->FileName.c_str());
		}
		else
		{
			LoadColours(clbTarget, odLoadBrush->FileName.c_str());
		}
	}
}


void __fastcall TfrmAutomate::bSaveSourceColoursClick(TObject *Sender)
{
	sdSaveBrush->DefaultExt = L"colours";
	sdSaveBrush->Filter     = GLanguageHandler->Text[kColourLists] + L" (*.colours)|*.colours";
	sdSaveBrush->InitialDir = Utility::WS2US(GSystemSettings->App.LMSFilePath + L"automate\\colours\\");

	if (sdSaveBrush->Execute())
	{
		TBitBtn *bb = (TBitBtn*)Sender;

		if (bb->Tag == 0)
		{
			SaveColours(clbSource, sdSaveBrush->FileName.c_str());
		}
		else
		{
			SaveColours(clbTarget, sdSaveBrush->FileName.c_str());
		}
	}
}
#pragma end_region


void __fastcall TfrmAutomate::lbActionsDblClick(TObject *Sender)
{
	TListBox *lb = (TListBox*)Sender;

	if (lb->Tag == 0)
	{
		sbRemoveSelectedClick(lbActions);
	}
	else
	{
		sbRemoveSelectedClick(lbPostProcessing);
	}
}


void __fastcall TfrmAutomate::sbClearClick(TObject *Sender)
{
	TSpeedButton *sb = (TSpeedButton*)Sender;

	if (sb->Tag == 0)
	{
		lbActions->Clear();
	}
	else
	{
		lbPostProcessing->Clear();
	}

	if (lbActions->Count == 0 && lbPostProcessing->Count == 0)
	{
		bOK->Enabled = false;
	}
	else
	{
		bOK->Enabled = true;
	}
}


void __fastcall TfrmAutomate::sbMirrorClick(TObject *Sender)
{
	bool canadd = false;

	TSpeedButton *sb = (TSpeedButton*)Sender;

	switch (sb->Tag)
	{
	case 19:
	case 20:
		if (CustomBrush[0].size() != 0)
		{
			canadd = true;
		}
		else
		{
			MessageDlg(GLanguageHandler->Text[kNoCustomBrush1Selected].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
		}
		break;
	case 21:
	case 22:
		if (CustomBrush[1].size() != 0)
		{
			canadd = true;
		}
		else
		{
			MessageDlg(GLanguageHandler->Text[kNoCustomBrush2Selected].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
		}
		break;

	default:
		canadd = true;
	}

	if (canadd)
	{
		lbActions->Items->Add(kAutomationActions[sb->Tag].c_str());

		bOK->Enabled = true;
	}
}


void __fastcall TfrmAutomate::sbRemoveSelectedClick(TObject *Sender)
{
	TSpeedButton *sb = (TSpeedButton*)Sender;

	if (sb->Tag == 0)
	{
		lbActions->DeleteSelected();
	}
	else
	{
		lbPostProcessing->DeleteSelected();
	}

	if (lbActions->Count == 0 && lbPostProcessing->Count == 0)
	{
		bOK->Enabled = false;
	}
	else
	{
		bOK->Enabled = true;
	}
}


void __fastcall TfrmAutomate::sCB1TransparentColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	TShape *shape = (TShape*)Sender;

	cdColours->Color = shape->Brush->Color;

	if (cdColours->Execute())
	{
		shape->Brush->Color = cdColours->Color;
	}
}


void __fastcall TfrmAutomate::sbCyclingLinearClick(TObject *Sender)
{
	bool canadd = false;

	TSpeedButton *sb = (TSpeedButton*)Sender;

	switch (sb->Tag)
	{
	case 27:
	case 28:
		if (clbSource->Count != 0 && clbTarget->Count != 0)
		{
			canadd = true;
		}
		else
		{
			MessageDlg(GLanguageHandler->Text[kNoSourcTargetColoursSelected].c_str(), mtWarning, TMsgDlgButtons() << mbOK, 0);
		}
		break;

	default:
		canadd = true;
	}

	if (canadd)
	{
		lbPostProcessing->Items->Add(kAutomationActions[sb->Tag].c_str());


		bOK->Enabled = true;
	}
}


void TfrmAutomate::SaveAutomation(const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		file << Formatting::to_utf8(L"{" + kFileHeaderData + L"\n");

		if (rbProcessMode1->Checked)
		{
			file << Formatting::to_utf8(kAutomationProcessMode + L":1\n");
		}
		else if (rbProcessMode2->Checked)
		{
			file << Formatting::to_utf8(kAutomationProcessMode + L":0\n");
		}
		else
		{
			file << Formatting::to_utf8(kAutomationProcessMode + L":2\n");
		}

		file << Formatting::to_utf8(kAutomationStartFrame + L":" + eFrameStart->Text.c_str() + L"\n");
		file << Formatting::to_utf8(kAutomationEndFrame +   L":" + eFrameEnd->Text.c_str() + L"\n");

		if (cbErase->Checked)
		{
			file << Formatting::to_utf8(kAutomationErase + L":1\n");
		}
		else
		{
			file << Formatting::to_utf8(kAutomationErase + L":0\n");
		}

		file << Formatting::to_utf8(kDataBlockEndS + L"\n");;

		// ===========================================================================

		if (lbActions->Count != 0)
		{
			file << Formatting::to_utf8(L"{" + kFileHeaderActions + L"\n");

			for (int t = 0; t < lbActions->Count; t++)
			{
				file << Formatting::to_utf8(kAutomationActionItem + L":" + lbActions->Items->Strings[t].c_str() + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		if (lbPostProcessing->Count != 0)
		{
			file << Formatting::to_utf8(L"{" + kFileHeaderPostProcessing + L"\n");

			for (int t = 0; t < lbPostProcessing->Count; t++)
			{
				file << Formatting::to_utf8(kAutomationPostProcessingItem + L":" + lbPostProcessing->Items->Strings[t].c_str() + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===========================================================================

		if (CustomBrush[0].size() != 0)
		{
			file << Formatting::to_utf8(L"{" + kFileHeaderBrush1 + L"\n");

			file << Formatting::to_utf8(kAutomationBrushColour +      L":" + std::to_wstring(sCB1TransparentColour->Brush->Color) + L"\n");
			file << Formatting::to_utf8(kAutomationBrushTransparent + L":" + std::to_wstring(cbCB1Transparent->Checked) + L"\n");

			for (int t = 0; t < CustomBrush[0].size(); t++)
			{
				file << Formatting::to_utf8(kAutomationBrushRowData + L":" + CustomBrush[0][t] + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===================================================================

		if (CustomBrush[1].size() != 0)
		{
			file << Formatting::to_utf8(L"{" + kFileHeaderBrush2 + L"\n");

			file << Formatting::to_utf8(kAutomationBrushColour +      L":" + std::to_wstring(sCB2TransparentColour->Brush->Color) + L"\n");
			file << Formatting::to_utf8(kAutomationBrushTransparent + L":" + std::to_wstring(cbCB2Transparent->Checked) + L"\n");

			for (int t = 0; t < CustomBrush[1].size(); t++)
			{
				file << Formatting::to_utf8(kAutomationBrushRowData + L":" + CustomBrush[1][t] + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===================================================================

		if (clbSource->Count != 0)
		{
			file << Formatting::to_utf8(L"{" + kFileHeaderSource + L"\n");

			for (int t = 0; t < clbSource->Count; t++)
			{
				int colour = reinterpret_cast<int>(clbSource->Items->Objects[t]);

				file << Formatting::to_utf8(kAutomationColor + L":" + std::to_wstring(colour) + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===========================================================================

		if (clbTarget->Count != 0)
		{
			file << Formatting::to_utf8(L"{" + kFileHeaderTarget + L"\n");

			for (int t = 0; t < clbTarget->Count; t++)
			{
				int colour = reinterpret_cast<int>(clbSource->Items->Objects[t]);

				file << Formatting::to_utf8(kAutomationColor + L":" + std::to_wstring(colour) + L"\n");
			}

			file << Formatting::to_utf8(kDataBlockEndS + L"\n");
		}

		// ===================================================================

		file.close();
	}
}


int TfrmAutomate::LoadDataParameterType(const std::wstring input)
{
	if (input.find(L"{" + kFileHeaderData) != std::wstring::npos)
	{
		return 1;
	}
	else if (input.find(L"{" + kFileHeaderActions) != std::wstring::npos)
	{
		return 2;
	}
	else if (input.find(L"{" + kFileHeaderPostProcessing) != std::wstring::npos)
	{
		return 3;
	}
	else if (input.find(L"{" + kFileHeaderBrush1) != std::wstring::npos)
	{
		return 4;
	}
	else if (input.find(L"{" + kFileHeaderBrush2) != std::wstring::npos)
	{
		return 5;
	}
	else if (input.find(L"{" + kFileHeaderSource) != std::wstring::npos)
	{
		return 6;
	}
	else if (input.find(L"{" + kFileHeaderTarget) != std::wstring::npos)
	{
		return 7;
	}
	else if (input[0] == kDataBlockEnd)
	{
		return 8;
	}
	else
	{
		switch (input[0])
		{
		case kAutomationBrushColourF:
			return 10;
		case kAutomationBrushTransparentF:
			return 11;
		case kAutomationColorF:
			return 20;
		case kAutomationActionItemF:
			return 21;
		case kAutomationProcessModeF:
			return 30;
		case kAutomationStartFrameF:
			return 31;
		case kAutomationEndFrameF:
			return 32;
		case kAutomationEraseF:
			return 33;
		}
	}

	return -1;
}


void TfrmAutomate::LoadAutomation(const std::wstring file_name)
{
	lbActions->Clear();
	lbPostProcessing->Clear();
	clbSource->Clear();
	clbTarget->Clear();
	CustomBrush[0].clear();
	CustomBrush[1].clear();

	std::wifstream file(file_name);

	if (file)
	{
		std::wstring s(L"");

		int ProcessMode = 0;

		while (std::getline(file, s))
		{
			if (s != L"")
			{
				if (s[0] == L'/' || s[0] == L'#')
				{
					// comment, do nothing
				}
				else
				{
					std::wstring input = L"";

					if (input.length() > 2)
					{
					    input =	s.substr(2);
					}

					std::transform(s.begin(), s.end(), s.begin(), ::tolower);

					switch (LoadDataParameterType(s))
					{
					case 1:
						ProcessMode = 1;
						break;
					case 2:
						ProcessMode = 2;
						break;
					case 3:
						ProcessMode = 3;
						break;
					case 4:
						ProcessMode = 4;
						break;
					case 5:
						ProcessMode = 5;
						break;
					case 6:
						ProcessMode = 6;
						break;
					case 7:
						ProcessMode = 7;
						break;
					case 8:
						ProcessMode = 0;
						break;

					case 10:
					{
						switch (ProcessMode)
						{
						case 4:
							sCB1TransparentColour->Brush->Color = TColor(stoi(input));
							break;
						case 5:
							sCB2TransparentColour->Brush->Color = TColor(stoi(input));
							break;
						}
						break;
					}
					case 11:
					{
						switch (ProcessMode)
						{
						case 4:
							cbCB1Transparent->Checked = stoi(input);
							break;
						case 5:
							cbCB2Transparent->Checked = stoi(input);
							break;
						}
						break;
					}

					case 20:
					{
						int colour = stoi(input);

						TObject *c = (TObject*)colour;

						if (ProcessMode == 6)
						{
							clbSource->AddItem(ColourUtility::RGBPlusInteger(colour, 100).c_str(), c);
						}
						else if (ProcessMode == 7)
						{
							clbTarget->AddItem(ColourUtility::RGBPlusInteger(colour, 100).c_str(), c);
						}
						break;
					}
					case 21:
					{
						switch (ProcessMode)
						{
						case 2:
							lbActions->Items->Add(input.c_str());
							break;
						case 3:
							lbPostProcessing->Items->Add(input.c_str());
							break;
						case 4:
							CustomBrush[0].push_back(input);
							break;
						case 5:
							CustomBrush[1].push_back(input);
							break;
						}
						break;
					}

					case 30:
						if (ProcessMode == 1)
						{
							if (input == L"1")
							{
								rbProcessMode1->Checked = true;
							}
							else if (input == L"0")
							{
								rbProcessMode2->Checked = true;
							}
							else
							{
								rbProcessMode3->Checked = true;
							}
						}
						break;
					case 31:
						if (ProcessMode == 1)
						{
							eFrameStart->Text = input.c_str();
						}
						break;
					case 32:
						if (ProcessMode == 1)
						{
							eFrameEnd->Text = input.c_str();
						}
						break;
					case 33:
						if (ProcessMode == 1)
						{
							if (input == L"1")
							{
								cbErase->Checked = true;
							}
							else
							{
								cbErase->Checked = false;
							}
						}
						break;
					}
				}
			}
		}

		file.close();
	}
}

void TfrmAutomate::SaveColours(TColorListBox *clb, const std::wstring file_name)
{
	std::ofstream file(file_name);

	if (file)
	{
		for (int i = 0; i < clb->Items->Count; i++)
		{
			int colour = reinterpret_cast<int>(clb->Items->Objects[i]);

			file << Formatting::to_utf8(kColoursData + L":" + std::to_wstring(colour) + L"\n");
		}

		file.close();
	}
}


int TfrmAutomate::GetActionIDFromName(const std::wstring name)
{
	for (int t = 0; t < kActionsCount; t++)
	{
		if (name == kAutomationActions[t])
		{
			return t;
		}
	}

	return -1;
}


void TfrmAutomate::LoadColours(TColorListBox *clb, const std::wstring file_name)
{
	clb->Clear();

	std::wifstream file(file_name);

	if (file)
	{
		std::wstring s(L"");

		while (std::getline(file, s))
		{
			if (!s.empty())
			{
				if (s[0] == L'/' || s[0] == L'#')
				{
					// comment, do nothing
				}
				else
				{
					int i = 0;

					if (s.find(L"$") != std::wstring::npos)
					{
						i = Convert::HexToInt(s.substr(5));  // col:$n
					}
					else
					{
						i = stoi(s.substr(4));   			// col:n
					}

					clb->AddItem(ColourUtility::RGBPlusInteger(i, 100).c_str(), (TObject*)i);
				}
			}
		}

		file.close();
	}
}


void TfrmAutomate::SetGuiLanguageText()
{
	Caption = GLanguageHandler->Text[kAutomate].c_str();

	tsActions->Caption = GLanguageHandler->Text[kActions].c_str();
	gbActions->Caption = GLanguageHandler->Text[kAvailableActions].c_str();
	Label9->Caption = GLanguageHandler->Text[kWipe].c_str();
	Label20->Caption = GLanguageHandler->Text[kReveal].c_str();
	lColour->Caption = GLanguageHandler->Text[kColour].c_str();
	Label6->Caption = GLanguageHandler->Text[kProcess].c_str();
	sbMirror->Caption = GLanguageHandler->Text[kMirror].c_str();
	sbFlip->Caption = GLanguageHandler->Text[kFlip].c_str();
	sbInvert->Caption = GLanguageHandler->Text[kInvert].c_str();
	Label7->Caption = GLanguageHandler->Text[kScroll].c_str();
	Label8->Caption = GLanguageHandler->Text[kRotate].c_str();
	Label10->Caption = GLanguageHandler->Text[kJiggle].c_str();
	Label11->Caption = GLanguageHandler->Text[kBounce].c_str();
	Label19->Caption = GLanguageHandler->Text[kAlternate].c_str();
	lBrush->Caption = GLanguageHandler->Text[kBrushNo1].c_str();
	SpeedButton7->Caption = GLanguageHandler->Text[kEveryFrame].c_str();
	SpeedButton8->Caption = GLanguageHandler->Text[kFirstFrame].c_str();
	Label16->Caption = GLanguageHandler->Text[kBrushNo2].c_str();
	SpeedButton9->Caption = GLanguageHandler->Text[kEveryFrame].c_str();
	SpeedButton13->Caption = GLanguageHandler->Text[kFirstFrame].c_str();
	Label12->Caption = GLanguageHandler->Text[kColourCycle].c_str();
	sbCyclingLinear->Caption = GLanguageHandler->Text[kCyclingLinear].c_str();
	SpeedButton10->Caption = GLanguageHandler->Text[kCyclingBounce].c_str();

	gbProcessingOptions->Caption = GLanguageHandler->Text[kProcessingOptions].c_str();
	rbProcessMode1->Caption = GLanguageHandler->Text[kUseFirstFrameAsSource].c_str();
	rbProcessMode2->Caption = GLanguageHandler->Text[kEachFrameIndividually].c_str();
	rbProcessMode3->Caption = GLanguageHandler->Text[kEachFrameIndividuallyIncrement].c_str();
	cbErase->Caption = GLanguageHandler->Text[kEraseWipeJiggleModes].c_str();
	lFrameStart->Caption = GLanguageHandler->Text[kFrameStart].c_str();
	lFrameEnd->Caption = GLanguageHandler->Text[kFrameEnd].c_str();
	lLayer->Caption = GLanguageHandler->Text[kLayer].c_str();

	gbActionList->Caption = GLanguageHandler->Text[kActionList].c_str();
	Label4->Caption = GLanguageHandler->Text[kProcessedOnEachFrame].c_str();
	sbClear->Caption = GLanguageHandler->Text[kClear].c_str();
	sbRemoveSelected->Caption = GLanguageHandler->Text[kRemove].c_str();

	gbPostProcessing->Caption = GLanguageHandler->Text[kPostProcessing].c_str();
	Label15->Caption = GLanguageHandler->Text[kPostProcessingHelp].c_str();
	SpeedButton11->Caption = GLanguageHandler->Text[kClear].c_str();
	SpeedButton12->Caption = GLanguageHandler->Text[kRemove].c_str();

	tsOptions->Caption = GLanguageHandler->Text[kOptions].c_str();

	gbBrush->Caption = GLanguageHandler->Text[kBrush].c_str();
	Label17->Caption = GLanguageHandler->Text[kBrushNo1].c_str();
	bCustomBrush1->Caption = GLanguageHandler->Text[kCustomBrush].c_str();
	cbCB1Transparent->Caption = GLanguageHandler->Text[kTransparent].c_str();
	bCopyBrush1ColoursSource->Caption = GLanguageHandler->Text[kCopyColoursToSource].c_str();

	Label18->Caption = GLanguageHandler->Text[kBrushNo2].c_str();
	bCustomBrush2->Caption = GLanguageHandler->Text[kCustomBrush].c_str();
	cbCB2Transparent->Caption = GLanguageHandler->Text[kTransparent].c_str();
	bCopyBrush2ColoursSource->Caption = GLanguageHandler->Text[kCopyColoursToSource].c_str();

	gbColourCycling->Caption = GLanguageHandler->Text[kColourCycling].c_str();
	Label13->Caption = GLanguageHandler->Text[kSourceColourxs].c_str();
	sbClearSourceColours->Caption = GLanguageHandler->Text[kClear].c_str();
	bOpenSourceColours->Caption = GLanguageHandler->Text[kLoad].c_str();
	bSaveSourceColours->Caption = GLanguageHandler->Text[kSave].c_str();
	Label14->Caption = GLanguageHandler->Text[kTargetColours].c_str();
	sbClearTargetColours->Caption = GLanguageHandler->Text[kClear].c_str();
	bOpenTargetColours->Caption = GLanguageHandler->Text[kLoad].c_str();
	bSaveTargetColours->Caption = GLanguageHandler->Text[kSave].c_str();
	Label24->Caption = GLanguageHandler->Text[kSkip].c_str();
	Label1->Caption = GLanguageHandler->Text[kFrames].c_str();
	Label23->Caption = GLanguageHandler->Text[kFirst32CcoloursFromCurrentAnimation].c_str();
	lSpoon->Caption = GLanguageHandler->Text[kColourCyclingHelp].c_str();

	lSuggestion->Caption = GLanguageHandler->Text[kAutomateHelp].c_str();

	bLoadAutomation->Caption = GLanguageHandler->Text[kLoad].c_str();
	bSaveAutomation->Caption = GLanguageHandler->Text[kSave].c_str();

	bOK->Caption = GLanguageHandler->Text[kOK].c_str();
	bCancel->Caption = GLanguageHandler->Text[kCancel].c_str();
}


void TfrmAutomate::SetCaption(const std::wstring path)
{
	Caption = GLanguageHandler->Text[kAutomate].c_str();

	if (!path.empty())
	{
		std::wstring p = L" \"" + path + L"\"";

		Caption += p.c_str();
	}
}


void __fastcall TfrmAutomate::sRevealColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
	if (cdColours->Execute())
	{
		sRevealColour->Brush->Color = cdColours->Color;
	}
}
