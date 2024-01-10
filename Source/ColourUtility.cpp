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

#include "ColourUtility.h"
#include "Convert.h"


namespace ColourUtility
{
	std::wstring RGBPlusInteger(int windowsformatcolour, int brightness)
	{
		std::wstring i2h = IntToHex(RGBConvertTo32(windowsformatcolour, RGBMode::kRGB, LeastSignificantBit::kBottomRight, brightness), 6).c_str();

		return L"0x" + i2h + L" (" + std::to_wstring(windowsformatcolour) + L")";
	}


	int RGBConvertTo16(int rgb, RGBMode convertmode, LeastSignificantBit lsblocation, ColourSpace colourspace, int brightness)
	{
		int r = (rgb & 0x0000ff);         // Windows colour structure = BGR
		int b = (rgb & 0xff0000) >> 16;
		int g = (rgb & 0x00ff00) >> 8;

		if (brightness != 100)
		{
			r = std::round((brightness / 100) * r);
			g = std::round((brightness / 100) * g);
			b = std::round((brightness / 100) * b);
		}

		if (colourspace == ColourSpace::kRGB565)
		{
			int xT = 0;

			r = std::round((r / 255) * 31);
			g = std::round((g / 255) * 63);
			b = std::round((b / 255) * 31);

			switch (convertmode)
			{
			case RGBMode::kRGB:
				xT = (r << 11) + (g << 5) + b;
				break;
			case RGBMode::kBGR:
				xT = (b << 11) + (g << 5) + r;
				break;
			case RGBMode::kGRB:
				xT = (g << 10) + (r << 5) + b;
				break;
			case RGBMode::kBRG:
				xT = (b << 11) + (r << 6) + g;
				break;

			default:
			  xT = 0;
			}

			if (lsblocation == LeastSignificantBit::kTopLeft) // flip bit order: 1111000011001100 -> 0011001100001111
			{
				int total = 0;

				for (int t = 0; t < 16; t++)
				{
					if ((xT & powers[t]) == powers[t])
					{
						total += powers[15 - t];
					}
				}

                return total;
			}
			else
			{
				return xT;
			}
		}

		return 0;
	}


	int RGBConvertTo32(int rgb, RGBMode convertmode, LeastSignificantBit lsblocation, int brightness)
	{
		int r = (rgb & 0x0000ff);         // Windows colour structure = BGR
		int b = (rgb & 0xff0000) >> 16;
		int g = (rgb & 0x00ff00) >> 8;

		int xT = 0;

		if (brightness != 100)
		{
			r = std::round((brightness / 100) * r);
			g = std::round((brightness / 100) * g);
			b = std::round((brightness / 100) * b);
		}

		switch (convertmode)
		{
		case RGBMode::kRGB:
			xT = (r << 16) + (g << 8) + b;
			break;
		case RGBMode::kBGR:
			xT = (b << 16) + (g << 8) + r;
			break;
		case RGBMode::kGRB:
			xT = (g << 16) + (r << 8) + b;
			break;
		case RGBMode::kBRG:
			xT = (b << 16) + (r << 8) + g;
			break;

		default:
			xT = 0;
		}

		if (lsblocation == LeastSignificantBit::kTopLeft) // flip bit order
		{
			int total = 0;

			for (int t = 0; t < 24; t++)
			{
				if ((xT & powers[t]) == powers[t])
				{
					total += total + powers[31 - t];
				}
			}

			return total;
		}

		return xT;
	}


	std::wstring RGBColourNumberFormat(NumberFormat nf, int nybbles, int colour)
	{
		switch (nf)
		{
		case NumberFormat::kDecimal:
			return std::to_wstring(colour);
		case NumberFormat::kBinary:
		{
			int bits = (nybbles * 4) - 1;

			return Convert::IntegerToBinary(bits, colour);
		}
		case NumberFormat::kHex:
			return IntToHex(colour, nybbles).c_str();
		}

		return L"errorRGBCNF";
	}


	// converts windows format colour to separate R G B values
	// eg ff0000 (blue)
	// => 00 00 ff
	// and converts to different colour space as required
	std::wstring RGBConvertToSplit(int rgb, RGBMode convertmode, int brightness, NumberFormat nf, const std::wstring prefix, const std::wstring spacer, ColourSpace colourspace)
	{
		int r = (rgb & 0x0000ff);         // Windows colour structure = BGR
		int b = (rgb & 0xff0000) >> 16;
		int g = (rgb & 0x00ff00) >> 8;

		int colour = 0;

		if (brightness != 100)
		{
			r = std::round(((double)brightness / 100) * (double)r);
			g = std::round(((double)brightness / 100) * (double)g);
			b = std::round(((double)brightness / 100) * (double)b);
		}

		switch (colourspace)
		{
		case ColourSpace::kRGB32:
		{
			switch (convertmode)
			{
			case RGBMode::kRGB:
				return prefix + RGBColourNumberFormat(nf, 2, r) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, g) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, b) + spacer;
			case RGBMode::kBGR:
				return prefix + RGBColourNumberFormat(nf, 2, b) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, g) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, r) + spacer;
			case RGBMode::kGRB:
				return prefix + RGBColourNumberFormat(nf, 2, g) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, r) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, b) + spacer;
			case RGBMode::kBRG:
				return prefix + RGBColourNumberFormat(nf, 2, b) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, r) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, g) + spacer;
			case RGBMode::kRGBSimple:
				return std::to_wstring(r) + spacer + std::to_wstring(g) + spacer + std::to_wstring(b);

			default:
				return prefix + L"00" + spacer + prefix + L"00" + spacer + prefix + L"00" + spacer;   // !
			}
			break;
		}
		case ColourSpace::kRGB565:
		{
			r = std::round((r / 255) * 31); // 5 bits
			g = std::round((g / 255) * 63); // 6 bits
			b = std::round((b / 255) * 31); // 5 bits

			switch (convertmode)
			{
			case RGBMode::kRGB:
				colour = (r << 11) + (g << 5) + b;
				break;
			case RGBMode::kBGR:
				colour = (b << 11) + (g << 5) + r;
				break;
			case RGBMode::kGRB:
				colour = (g << 10) + (r << 5) + b;
				break;
			case RGBMode::kBRG:
				colour = (b << 11) + (r << 6) + g;
				break;
			case RGBMode::kRGBSimple:
				colour = (r << 11) + (g << 5) + b;
				break;

			default:
				colour = 0;
			}

			return prefix + RGBColourNumberFormat(nf, 2, (colour >> 8)) + spacer +
				   prefix + RGBColourNumberFormat(nf, 2, (colour & 0x00FF)) + spacer;
		}
		}

		return L"errorRGBCTS";
	}


	std::wstring RGB3BPPFormatOutput(int r, int g, int b, RGBMode rgbmode, NumberFormat nf, NumberSize ns, int brightness, const std::wstring prefix, const std::wstring spacer)
	{
		if (ns == NumberSize::kRGB8bit)
		{
			switch (rgbmode)
			{
			case RGBMode::kRGB:
				return prefix + RGBColourNumberFormat(nf, 2, r) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, g) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, b) + spacer;
			case RGBMode::kBGR:
				return prefix + RGBColourNumberFormat(nf, 2, b) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, g) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, r) + spacer;
			case RGBMode::kGRB:
				return prefix + RGBColourNumberFormat(nf, 2, g) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, r) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, b) + spacer;
			case RGBMode::kBRG:
				return prefix + RGBColourNumberFormat(nf, 2, b) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, r) + spacer +
					   prefix + RGBColourNumberFormat(nf, 2, g) + spacer;

			default:
				return L"errorFO";
			}
		}
		else
		{
			switch (rgbmode)
			{
			case RGBMode::kRGB:
				return prefix + RGBColourNumberFormat(NumberFormat::kHex, 2, r) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, g) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, b) + spacer;
			case RGBMode::kBGR:
				return prefix + RGBColourNumberFormat(NumberFormat::kHex, 2, b) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, g) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, r) + spacer;
			case RGBMode::kGRB:
				return prefix + RGBColourNumberFormat(NumberFormat::kHex, 2, g) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, r) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, b) + spacer;
			case RGBMode::kBRG:
				return prefix + RGBColourNumberFormat(NumberFormat::kHex, 2, b) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, r) +
					   RGBColourNumberFormat(NumberFormat::kHex, 2, g) + spacer;
			}
		}

        return L"errorFOx";
	}


	int DarkenRGB(int RGB)
	{
		int r = (RGB & 0x0000ff);         // Windows colour structure = BGR
		int b = (RGB & 0xff0000) >> 16;
		int g = (RGB & 0x00ff00) >> 8;

		r = std::round((double)r * 0.8);
		g = std::round((double)g * 0.8);
		b = std::round((double)b * 0.8);

		return (b << 16) + (g << 8) + r;
	}


	int RandomColour(int RGB, int coeff)
	{
		int r = (RGB & 0x0000ff);         // Windows colour structure = BGR
		int b = (RGB & 0xff0000) >> 16;
		int g = (RGB & 0x00ff00) >> 8;

		r = (r - coeff) + (random(2 * coeff));
		g = (g - coeff) + (random(2 * coeff));
		b = (b - coeff) + (random(2 * coeff));

		if (r > 255) r = 255;
		if (g > 255) g = 255;
		if (b > 255) b = 255;

		if (r < 0) r = 0;
		if (g < 0) g = 0;
		if (b < 0) b = 0;

		return (b << 16) + (g << 8) + r;
	}
}
