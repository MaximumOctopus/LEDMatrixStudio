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


struct ProjectColours
{
	bool HasData = false;

	int DrawColours[4] = { 0, 0, 0, 0 }; // index 0 isn't used, but added to ensure LMB, MMB and RMB align to indicies 1,2,3 respectively...
	int CustomColours[16];

	int PaletteHistory[28];
};
