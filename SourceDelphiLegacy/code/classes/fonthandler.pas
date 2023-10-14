// ===================================================================
//
// (c) Paul Alan Freshney 2012-2023
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://github.com/MaximumOctopus/LEDMatrixStudio
//
// Please do not modifiy this comment section
//
// ===================================================================


unit fonthandler;


interface


uses SysUtils, Vcl.StdCtrls,

     utility;


type
  TFontHandler = class
    class function GetFontList(aPath : string; aFontList : TComboBox): boolean;
  end;


implementation


class function TFontHandler.GetFontList(aPath : string; aFontList : TComboBox): boolean;
var
  lSearchResult : TSearchRec;

begin
  Result := True;

  if FindFirst(aPath, faAnyFile, lSearchResult) = 0 then begin
    aFontList.Clear;

    repeat
      aFontList.Items.Add(TUtility.RemoveExtension(lSearchResult.Name));
    until FindNext(lSearchResult) <> 0;

    FindClose(lSearchResult);
  end
  else
    Result := False; // no fonts found
end;


end.
