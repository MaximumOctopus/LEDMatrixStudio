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

#ifndef FormPreviewPopoutH
#define FormPreviewPopoutH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.ImageList.hpp>
#include <Vcl.ImgList.hpp>

typedef void __fastcall (__closure *PreviewWindowEvent)(int);


class TfrmPreviewPopout : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TPanel *pAnimationToolbar;
	TLabel *lFrame;
	TBevel *Bevel5;
	TBevel *Bevel7;
	TBitBtn *bPlayAnimation;
	TBitBtn *bStopAnimation;
	TBitBtn *bPreviousFrame;
	TBitBtn *bNextFrame;
	TTrackBar *tbFrames;
	TBitBtn *bStartFrame;
	TBitBtn *bEndFrame;
	TImageList *ImageList1;
	void __fastcall bPlayAnimationClick(TObject *Sender);
	void __fastcall tbFramesTracking(TObject *Sender);
private:

    void SetGuiLanguageText();

public:		// User declarations
	__fastcall TfrmPreviewPopout(TComponent* Owner);

	void SetForPlaybackStart();
	void SetForPlaybackStop();

	std::function<void(int)> OnCommand;
	std::function<void(int)> OnNewFrame;
};

extern PACKAGE TfrmPreviewPopout *frmPreviewPopout;

#endif
