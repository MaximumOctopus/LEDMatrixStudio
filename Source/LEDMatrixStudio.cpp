//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
#include <Vcl.Styles.hpp>
#include <Vcl.Themes.hpp>
USEFORM("Frames\FrameUndoPanel.cpp", frameUndos); /* TFrame: File Type */
USEFORM("Frames\FrameLayerPanel.cpp", frameLayers); /* TFrame: File Type */
USEFORM("Frames\FramePalettePanel.cpp", framePalette); /* TFrame: File Type */
USEFORM("Frames\FrameQuickData.cpp", Frame5); /* TFrame: File Type */
USEFORM("Forms\FormNewProject.cpp", frmNewProject);
USEFORM("Forms\FormOptimise.cpp", frmOptimise);
USEFORM("Forms\FormPlaybackSpeed.cpp", frmPlaybackSpeed);
USEFORM("Forms\FormImportBitmap.cpp", frmImportBitmap);
USEFORM("Forms\FormMerge.cpp", frmMerge);
USEFORM("Forms\FormNewBrush.cpp", frmNewBrush);
USEFORM("Forms\FormPreferences.cpp", frmPreferences);
USEFORM("Forms\FormToggleLockStatus.cpp", frmToggleLockStatus);
USEFORM("Frames\FrameFontPanel.cpp", frameFont); /* TFrame: File Type */
USEFORM("Frames\FrameGradientPanel.cpp", frameGradient); /* TFrame: File Type */
USEFORM("Forms\FormPreviewPopout.cpp", frmPreviewPopout);
USEFORM("Forms\FormSaveRange.cpp", frmSaveRange);
USEFORM("Forms\FormSetIgnoredPixels.cpp", frmSetIgnoredPixels);
USEFORM("main.cpp", frmMain);
USEFORM("Forms\FormAddLayer.cpp", Form3);
USEFORM("Forms\FormAutomate.cpp", frmAutomate);
USEFORM("Forms\FormCheckVersion.cpp", frmCheckVersion);
USEFORM("Forms\FormAbout.cpp", frmAbout);
USEFORM("Forms\FormColourChange.cpp", frmColourChange);
USEFORM("Forms\FormExportCode.cpp", frmExportCode);
USEFORM("Forms\FormExportGIF.cpp", frmExportGIF);
USEFORM("Forms\FormFontViewer.cpp", frmFontViewer);
USEFORM("Forms\FormCopyMultiple.cpp", frmCopyMultiple);
USEFORM("Forms\FormDeleteMultiple.cpp", frmDeleteMultiple);
USEFORM("Forms\FormExport.cpp", frmExport);
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
