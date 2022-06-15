// ===================================================================
//
// (c) Paul Alan Freshney 2012-2022
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section
//
// ===================================================================

unit frameUndoPanel;


interface


uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;


type
  TUndoSelected = procedure(aUndo : integer) of object;

  TframeUndos = class(TFrame)
    lbUndos: TListBox;
    procedure lbUndosClick(Sender: TObject);
  private
    FUndoSelected : TUndoSelected;
  public
    procedure SetUndos(aCount : integer);

    property OnUndoSelected  : TUndoSelected read FUndoSelected write FUndoSelected;
  end;


implementation


{$R *.dfm}


procedure TframeUndos.lbUndosClick(Sender: TObject);
begin
  if Assigned(FUndoSelected) then
    FUndoSelected(lbUndos.ItemIndex);
end;


procedure TframeUndos.SetUndos(aCount : integer);
var
  t : integer;

begin
  lbUndos.Clear;

  for t := 1 to aCount do
    lbUndos.Items.Add('Undo ' + IntToStr(t));
end;


end.
