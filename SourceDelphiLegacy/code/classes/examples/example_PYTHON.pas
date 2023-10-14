// ===================================================================
//
// (c) Paul Alan Freshney 2012-2023
// www.freshney.org :: paul@freshney.org :: maximumoctopus.com
//
// https://sourceforge.net/projects/led-matrix-studio/
//
// Please do not redistribute the source code!
//
//   Started: October 13th 2021
//  Modified: October 13th 2021
//
// ===================================================================


unit example_PYTHON;


interface


uses SysUtils;


type
  TExamplePYTHON = class
    class function GetExample: string;
  end;


implementation


class function TExamplePYTHON.GetExample: string;
begin
  Result := '';
end;


end.
