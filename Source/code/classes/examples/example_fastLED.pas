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


unit example_fastLED;


interface


uses SysUtils;


type
  TExampleFastLED = class
    class function GetExample(aFrameStart, aFrameEnd, aPixelsPerFrame : integer): string;
  end;



implementation


class function TExampleFastLED.GetExample(aFrameStart, aFrameEnd, aPixelsPerFrame : integer): string;
var
  t : integer;

begin
  Result := '';

  Result := Result + 'const int __NUM_LEDS = ' + IntToStr(aPixelsPerFrame) + '; // per frame' + #13#10;
  Result := Result + 'const unsigned long __DELAY_MS = 225;' + #13#10;
  Result := Result + #13#10;
  Result := Result + 'void loop() {' + #13#10;
  Result := Result + #13#10;

  for t := aFrameStart to aFrameEnd do begin
    Result := Result + '  FastLED.clear();' + #13#10;
    Result := Result + '  for(int i = 0; i < __NUM_LEDS; i++)' + #13#10;
    Result := Result + '  {' + #13#10;
    Result := Result + '    leds[i] = pgm_read_dword(&(ledarray' + IntToStr(t - 1) + '[i]));' + #13#10;
    Result := Result + '  }' + #13#10;
    Result := Result + '  FastLED.show();' + #13#10;
    Result := Result + '  delay(__DELAY_MS);' + #13#10;
    Result := Result + '' + #13#10;
  end;

  Result := Result + '}' + #13#10;
end;


end.
