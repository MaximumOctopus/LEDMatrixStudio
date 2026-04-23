//---------------------------------------------------------------------------

#ifndef FramePixelPanelH
#define FramePixelPanelH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
//---------------------------------------------------------------------------
class TframePixel : public TFrame
{
__published:	// IDE-managed Components
	TEdit *ePositionX;
	TEdit *ePositionY;
	TEdit *eGroup;
	TLabel *Label1;
	TLabel *Label2;
	TLabel *Label3;
	TLabel *Label4;
	TLabel *lOrder;
	TLabel *Label5;
	TLabel *lId;
	TLabel *Label6;
	void __fastcall ePositionXExit(TObject *Sender);
	void __fastcall ePositionYExit(TObject *Sender);
	void __fastcall eGroupExit(TObject *Sender);
private:
public:
	__fastcall TframePixel(TComponent* Owner);

	int SelectedPixelId = -1;

	void Build(int, int, int, int);

	void Enable();
    void Disable();

	// callbacks
	std::function<void(int)> OnNewX;
	std::function<void(int)> OnNewY;
	std::function<void(int)> OnNewGroup;
};
//---------------------------------------------------------------------------
extern PACKAGE TframePixel *framePixel;
//---------------------------------------------------------------------------
#endif
