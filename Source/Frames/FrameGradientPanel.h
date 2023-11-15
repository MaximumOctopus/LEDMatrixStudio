//---------------------------------------------------------------------------

#ifndef FrameGradientPanelH
#define FrameGradientPanelH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TframeGradient : public TFrame
{
__published:	// IDE-managed Components
	TSpeedButton *sbOpenGradient;
	TSpeedButton *sbSaveGradient;
	TSpeedButton *sbClearGradient;
	TShape *sRGBPaletteColour;
	TLabel *lPaletteColourText;
	TSpeedButton *sbAddColour;
	TSpeedButton *sbRemoveColour;
	TSpeedButton *bFromCustom;
	TSpeedButton *bFromShades;
	TSpeedButton *sbCopyToBrush;
	TColorListBox *clbGradient;
	TTrackBar *tbRed;
	TEdit *eRed;
	TEdit *eGreen;
	TTrackBar *tbGreen;
	TTrackBar *tbBlue;
	TEdit *eBlue;
	TColorDialog *cdGradient;
	TOpenDialog *odGradient;
	TSaveDialog *sdGradient;
	void __fastcall eRedKeyPress(TObject *Sender, System::WideChar &Key);
	void __fastcall clbGradientClick(TObject *Sender);
	void __fastcall sbOpenGradientClick(TObject *Sender);
	void __fastcall sbSaveGradientClick(TObject *Sender);
	void __fastcall sbCopyToBrushClick(TObject *Sender);
	void __fastcall sbAddColourClick(TObject *Sender);
	void __fastcall sbRemoveColourClick(TObject *Sender);
	void __fastcall sRGBPaletteColourMouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
	void __fastcall tbRedChange(TObject *Sender);
	void __fastcall bFromShadesClick(TObject *Sender);
	void __fastcall bFromCustomClick(TObject *Sender);
	void __fastcall sbClearGradientClick(TObject *Sender);
private:

	static const int CRed = 0;
	static const int CGreen = 1;
	static const int CBlue  = 2;

    enum class LoadGradient { kUnknown = 0, kLoadBegin, kLoadEnd, kLoadData };

	bool LoadGradient(const std::wstring);
	bool SaveGradient(const std::wstring);
	void SetSlidersFromColour(int);

public:
	__fastcall TframeGradient(TComponent* Owner);

	void SetGuiLanguageText();

	int GetColour(int);
	void AddColour(int);
	int GetColourCount();

	// callbacks
	std::function<void()> OnCopy;
	std::function<void()> OnFromCustom;
	std::function<void()> OnFromShades;
};


//---------------------------------------------------------------------------
extern PACKAGE TframeGradient *frameGradient;
//---------------------------------------------------------------------------
#endif
