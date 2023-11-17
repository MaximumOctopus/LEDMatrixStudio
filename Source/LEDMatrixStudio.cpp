//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
#include <Vcl.Styles.hpp>
#include <Vcl.Themes.hpp>
USEFORM("main.cpp", frmMain);
USEFORM("Forms\FormSaveRange.cpp", Form20);
USEFORM("Forms\FormSetIgnoredPixels.cpp", frmSetIgnoredPixels);
USEFORM("Forms\FormToggleLockStatus.cpp", Form22);
USEFORM("Forms\FormPlaybackSpeed.cpp", Form18);
USEFORM("Forms\FormPreferences.cpp", frmPreferences);
USEFORM("Forms\FormPreviewPopout.cpp", frmPreviewPopout);
USEFORM("Frames\FramePalettePanel.cpp", framePalette); /* TFrame: File Type */
USEFORM("Frames\FrameQuickData.cpp", Frame5); /* TFrame: File Type */
USEFORM("Frames\FrameUndoPanel.cpp", frameUndos); /* TFrame: File Type */
USEFORM("Frames\FrameFontPanel.cpp", frameFont); /* TFrame: File Type */
USEFORM("Frames\FrameGradientPanel.cpp", frameGradient); /* TFrame: File Type */
USEFORM("Frames\FrameLayerPanel.cpp", frameLayers); /* TFrame: File Type */
USEFORM("Forms\FormOptimise.cpp", frmOptimise);
USEFORM("Forms\FormDeleteMultiple.cpp", Form7);
USEFORM("Forms\FormExport.cpp", frmExport);
USEFORM("Forms\FormExportCode.cpp", frmExportCode);
USEFORM("Forms\FormAutomate.cpp", frmAutomate);
USEFORM("Forms\FormColourChange.cpp", frmColourChange);
USEFORM("Forms\FormCopyMultiple.cpp", frmCopyMultiple);
USEFORM("Forms\FormMerge.cpp", Form13);
USEFORM("Forms\FormNewBrush.cpp", frmNewBrush);
USEFORM("Forms\FormNewProject.cpp", Form16);
USEFORM("Forms\FormExportGIF.cpp", Form10);
USEFORM("Forms\FormFontViewer.cpp", frmFontViewer);
USEFORM("Forms\FormImportBitmap.cpp", frmImportBitmap);
USEFORM("Forms\FormAbout.cpp", frmAbout);
USEFORM("Forms\FormAddLayer.cpp", Form3);
//---------------------------------------------------------------------------
#include "LanguageHandler.h"
#include "SystemSettings.h"

extern LanguageHandler *GLanguageHandler;
extern SystemSettings *GSystemSettings;

//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		GSystemSettings = new SystemSettings();

		GLanguageHandler = new LanguageHandler(GSystemSettings->App.Language);

		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		TStyleManager::TrySetStyle("Carbon");
		Application->CreateForm(__classid(TfrmMain), &frmMain);
		Application->CreateForm(__classid(TfrmAbout), &frmAbout);
		Application->CreateForm(__classid(TfrmPreviewPopout), &frmPreviewPopout);
		Application->CreateForm(__classid(TfrmImportBitmap), &frmImportBitmap);
		Application->Run();
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw Exception("");
		}
		catch (Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
