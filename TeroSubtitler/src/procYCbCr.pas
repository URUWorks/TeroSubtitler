{ Conversion to and from YCbCr
 
  Copyright (C) 2021 Bernd Kreuss prof7bit@gmail.com
 
  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:
 
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.
 
  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.
 
  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit procYCbCr;
 
{$mode ObjFPC}{$H+}
 
interface
 
uses
  Graphics, FPImage;
 
type
  TItuRec = (
    ituBT601,  // ITU-R BT.601, full range
    ituBT709,  // ITU-R BT.709, full range
    ituBT2020  // ITU-R BT.2020, full range
  );
 
procedure ColorToYCbCr(C: TColor; out Y, Cb, Cr: Byte; Mode: TItuRec); overload;
procedure ColorToYCbCr(C: TFPColor; out Y, Cb, Cr: Byte; Mode: TItuRec); overload;
function YCbCrToColor(Y, Cb, Cr: Byte; Mode: TItuRec): TColor;
 
implementation

uses
  Math;
 
type
  TCoeff = array[0..10] of Double;
 
var
  BT601  : TCoeff;
  BT709  : TCoeff;
  BT2020  : TCoeff;
 
procedure ColorToYCbCr(C: TColor; out Y, Cb, Cr: Byte; constref K: TCoeff);
var
  RGB: LongInt;
  R, G, B: Byte;
begin
  RGB := ColorToRGB(C);
  R := Red(RGB);
  G := Green(RGB);
  B := Blue(RGB);
  Y  := EnsureRange(Round(K[0] * R + K[1] * G + K[2] * B), 0, 255);
  Cb := EnsureRange(Round(K[3] * R + K[4] * G +  0.5 * B + 128), 0, 255);
  Cr := EnsureRange(Round( 0.5 * R + K[5] * G + K[6] * B + 128), 0, 255);
end;

procedure ColorToYCbCr(C: TFPColor; out Y, Cb, Cr: Byte; constref K: TCoeff);
begin
  Y  := EnsureRange(Round(K[0] * C.Red + K[1] * C.Green + K[2] * C.Blue), 0, 255);
  Cb := EnsureRange(Round(K[3] * C.Red + K[4] * C.Green +  0.5 * C.Blue + 128), 0, 255);
  Cr := EnsureRange(Round( 0.5 * C.Red + K[5] * C.Green + K[6] * C.Blue + 128), 0, 255);
end;

function YCbCrToColor(Y, Cb, Cr: Byte; constref K: TCoeff): TColor;
var
  Cb0, Cr0: Integer;
  R, G, B: Byte;
begin
  Cb0 := Cb - 128;
  Cr0 := Cr - 128;
  R := EnsureRange(Round(Y + K[7] * Cr0              ), 0, 255);
  G := EnsureRange(Round(Y + K[8] * Cr0 + K[9]  * Cb0), 0, 255);
  B := EnsureRange(Round(Y              + K[10] * Cb0), 0, 255);
  Result := RGBToColor(R, G, B);
end;
 
procedure ColorToYCbCr(C: TColor; out Y, Cb, Cr: Byte; Mode: TItuRec);
begin
  case Mode of
    ituBT601  : ColorToYCbCr(C, Y, Cb, Cr, BT601);
    ituBT709  : ColorToYCbCr(C, Y, Cb, Cr, BT709);
    ituBT2020 : ColorToYCbCr(C, Y, Cb, Cr, BT2020);
  end;
end;

procedure ColorToYCbCr(C: TFPColor; out Y, Cb, Cr: Byte; Mode: TItuRec);
begin
  case Mode of
    ituBT601  : ColorToYCbCr(C, Y, Cb, Cr, BT601);
    ituBT709  : ColorToYCbCr(C, Y, Cb, Cr, BT709);
    ituBT2020 : ColorToYCbCr(C, Y, Cb, Cr, BT2020);
  end;
end;

function YCbCrToColor(Y, Cb, Cr: Byte; Mode: TItuRec): TColor;
begin
  case Mode of
    ituBT601  : Result := YCbCrToColor(Y, Cb, Cr, BT601);
    ituBT709  : Result := YCbCrToColor(Y, Cb, Cr, BT709);
    ituBT2020  : Result := YCbCrToColor(Y, Cb, Cr, BT2020);
  end;
end;
 
procedure InitYCbCrCoef(out K: TCoeff; Kr, Kg: Double);
begin                                 //  example values for BT601
  K[0] := Kr;                         //  0.299
  K[1] := Kg;                         //  0.587
  K[2] := 1 - Kr - Kg; // Kb          //  0.114
  K[3] := -Kr / (2 * (Kr + Kg));      // -0.1687
  K[4] := -Kg / (2 * (Kr + Kg));      // -0.3313
  K[5] := -Kg / (2 * (Kg + K[2]));    // -0.4187
  K[6] := -K[2] / (2 * (Kg + K[2]));  // -0.0813
  K[7] := 2 * (Kg + K[2]);            //  1.402
  K[8] := -Kr * 2 * (Kg + K[2]) / Kg; // -0.7141
  K[9] := -K[2] * 2 * (Kr + Kg) / Kg; // -0.3441
  k[10] := 2 * (Kr + Kg);             //  1.772
end;
 
initialization
  InitYCbCrCoef(BT601, 0.299, 0.587);
  InitYCbCrCoef(BT709, 0.2126, 0.7152);
  InitYCbCrCoef(BT2020, 0.2627, 0.6780);
end.
