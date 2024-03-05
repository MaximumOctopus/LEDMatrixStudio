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

#pragma once


namespace ExampleFastLED
{
	std::wstring Get(int start, int end, int pixelsperframe)
	{
		std::wstring s = L"";

		s += L"const int __NUM_LEDS = ' + IntToStr(aPixelsPerFrame) + '; // per frame\n";
		s += L"const unsigned long __DELAY_MS = 225;\n";
		s += L"\n";
		s += L"void loop() {\n";
		s += L"\n";

		for (int t = start; t <= end; t++)
		{
			s += L"  FastLED.clear();\n";
			s += L"  for(int i = 0; i < __NUM_LEDS; i++)\n";
			s += L"  {\n";
			s += L"    leds[i] = pgm_read_dword(&(ledarray' + IntToStr(t - 1) + '[i]));\n";
			s += L"  }\n";
			s += L"  FastLED.show();\n";
			s += L"  delay(__DELAY_MS);\n";
			s += L"\n";
		}

		return s += L"}\n";
	}
}
