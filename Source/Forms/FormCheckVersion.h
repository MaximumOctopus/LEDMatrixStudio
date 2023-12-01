//---------------------------------------------------------------------------

#ifndef FormCheckVersionH
#define FormCheckVersionH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdHTTP.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>
#include <Vcl.Buttons.hpp>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmCheckVersion : public TForm
{
__published:	// IDE-managed Components
	TShape *Shape1;
	TLabel *Label5;
	TBitBtn *bClose;
	TBitBtn *bHistory;
	TMemo *mHistory;
	TBitBtn *bWebsite;
	TGroupBox *gbInstalledVersion;
	TLabel *Label6;
	TLabel *Label7;
	TLabel *lIVDate;
	TLabel *lIVVersion;
	TGroupBox *gbLatestVersion;
	TLabel *Label10;
	TLabel *Label11;
	TLabel *lLADate;
	TLabel *lLAVersion;
	TGroupBox *GroupBox1;
	TLabel *lWhat;
	TIdHTTP *httpMain;
	void __fastcall bWebsiteClick(TObject *Sender);
	void __fastcall bHistoryClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:

	void SetGuiLanguageText();

public:
	__fastcall TfrmCheckVersion(TComponent* Owner);

	bool AutoClose = false;
};

void OpenCheckForNewVersion(std::wstring, std::wstring, bool);

//---------------------------------------------------------------------------
extern PACKAGE TfrmCheckVersion *frmCheckVersion;
//---------------------------------------------------------------------------
#endif
