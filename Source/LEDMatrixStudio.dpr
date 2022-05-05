// ===================================================================
//
// (c) Paul Alan Freshney 2012-2021
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://sourceforge.net/projects/led-matrix-studio/
//
// Please do not redistribute the source code!
//
//   Started: June 10th 2012
//  Modified: October 12th 2021
//
// ===================================================================


program LEDMatrixStudio;

uses
  Forms,
  formAutomate in 'code\forms\formAutomate.pas' {frmAutomate},
  formPlaybackSpeed in 'code\forms\formPlaybackSpeed.pas' {frmCustomPlayback},
  formNewBrush in 'code\forms\formNewBrush.pas' {frmNewBrush},
  formCopyMultiple in 'code\forms\formCopyMultiple.pas' {frameCopyMultiple},
  formDeleteMultipl in 'code\forms\formDeleteMultipl.pas' {frmDeleteMultiple},
  Vcl.Themes,
  Vcl.Styles,
  formSaveRange in 'code\forms\formSaveRange.pas' {frmSaveRange},
  formPreviewPopout in 'code\forms\formPreviewPopout.pas' {frmPreviewPopout},
  formToggleLockStatus in 'code\forms\formToggleLockStatus.pas' {frmToggleLockStatus},
  formColourChange in 'code\forms\formColourChange.pas' {frmColourChange},
  frameGradientPanel in 'code\frames\frameGradientPanel.pas' {frameGradient: TFrame},
  framePalettePanel in 'code\frames\framePalettePanel.pas' {framePalette: TFrame},
  frameFontPanel in 'code\frames\frameFontPanel.pas' {frameFont: TFrame},
  formMerge in 'code\forms\formMerge.pas' {frmMerge},
  frameLayerPanel in 'code\frames\frameLayerPanel.pas' {frameLayers: TFrame},
  formAddLayer in 'code\forms\formAddLayer.pas' {frmAddLayer},
  frameUndoPanel in 'code\frames\frameUndoPanel.pas' {frameUndos: TFrame},
  formSetIgnoredPixels in 'code\forms\formSetIgnoredPixels.pas' {frmSetIgnoredPixels},
  appsettings in 'code\units\appsettings.pas',
  datadisplay in 'code\units\datadisplay.pas',
  exportoptions in 'code\units\exportoptions.pas',
  exportoutputbinary in 'code\units\exportoutputbinary.pas',
  importdata in 'code\units\importdata.pas',
  matrixconstants in 'code\units\matrixconstants.pas',
  optimisation in 'code\units\optimisation.pas',
  previewsettings in 'code\units\previewsettings.pas',
  projectsettings in 'code\units\projectsettings.pas',
  toolbars in 'code\units\toolbars.pas',
  utility in 'code\units\utility.pas',
  xglobal in 'code\units\xglobal.pas',
  colours in 'code\classes\colours.pas',
  drawingdata in 'code\classes\drawingdata.pas',
  exportoptions_monobi in 'code\classes\exportoptions_monobi.pas',
  exportoptions_rgb in 'code\classes\exportoptions_rgb.pas',
  exportoptions_rgb_3bpp in 'code\classes\exportoptions_rgb_3bpp.pas',
  exportutility in 'code\classes\exportutility.pas',
  layer in 'code\classes\layer.pas',
  matrix in 'code\classes\matrix.pas',
  matrixdead in 'code\classes\matrixdead.pas',
  systemsettings in 'code\classes\systemsettings.pas',
  thematrix in 'code\classes\thematrix.pas',
  formExportGIF in 'code\forms\formExportGIF.pas' {frmExportGIF},
  example_PICAXE in 'code\classes\examples\example_PICAXE.pas',
  example_fastLED in 'code\classes\examples\example_fastLED.pas',
  example_PYTHON in 'code\classes\examples\example_PYTHON.pas',
  example_CPP in 'code\classes\examples\example_CPP.pas',
  example_Microchip in 'code\classes\examples\example_Microchip.pas',
  actionobject in 'code\units\actionobject.pas',
  formAbout in 'code\forms\formAbout.pas' {frmAbout},
  main in 'code\forms\main.pas' {frmMain},
  formImportBitmap in 'code\forms\formImportBitmap.pas' {frmImportBitmap},
  formOptimise in 'code\forms\formOptimise.pas' {frmOptimise},
  formPreferences in 'code\forms\formPreferences.pas' {frmPreferences},
  formExportCode in 'code\forms\formExportCode.pas' {frmExportCode},
  formNewProject in 'code\forms\formNewProject.pas' {frmNewProject},
  formFontViewer in 'code\forms\formFontViewer.pas' {frmFontViewer},
  formExport in 'code\forms\formExport.pas' {frmExport},
  formCheckVersion in 'code\forms\formCheckVersion.pas' {frmCheckVersion},
  presethandler in 'code\classes\presethandler.pas',
  profilehandler in 'code\classes\profilehandler.pas',
  fonthandler in 'code\classes\fonthandler.pas';

{$R *.res}


begin
  Application.Initialize;

  Application.MainFormOnTaskbar := True;

  TStyleManager.TrySetStyle('Carbon');
  Application.Title := 'LEDMatrixStudio';

  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmPreviewPopout, frmPreviewPopout);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
