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


struct ExportGIFSettings
{
	bool Process = false;
	std::wstring FileName = L"";
	int PixelSize = 1;
	int PixelShape = 0;
	int Background = 0x00000000;
	int AnimationSpeed = 100;		// percent of "normal" - 50 = half speed, 100 = normal, 1000 = 10x speed
};