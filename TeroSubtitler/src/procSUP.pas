{*
 *  URUWorks
 *
 *  The contents of this file are used with permission, subject to
 *  the Mozilla Public License Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/2.0.html
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2023 URUWorks, uruworks@gmail.com.
 *}

unit procSUP;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UWSubtitleAPI, BGRABitmap;

procedure WriteSUPFrame(const AStream: TStream; const ASubtitles: TUWSubtitles; const AIndex, ACompositionNumber: Integer; const AImage: TBGRABitmap; const AVideoWidth, AVideoHeight: Integer);

// -----------------------------------------------------------------------------

implementation

uses
  procYCbCr, UWSubtitleAPI.SUP, UWSubtitleAPI.Utils, FPimage, BGRABitmapTypes,
  BGRAColorQuantization;

// -----------------------------------------------------------------------------

function BGRAColorToARGB(const AColor: TBGRAPixel): Integer;
begin
  with AColor do
    Result := (Alpha shl 24) or (Red shl 16) or (Green shl 8) or Blue;
end;

// -----------------------------------------------------------------------------

function EncodeImage(const AImage: TBGRABitmap; out ABuffer: TBytes; out APalette: TFPPalette): Integer;
var
  bmp : TBGRABitmap;
  quant : TBGRAColorQuantizer;
  x, y, i, len : Integer;
  p, r : PBGRAPixel;
  bytes : TBytesStream;
  clr : Integer;
begin
  // Reduce image
  bmp := TBGRABitmap.Create(AImage);
  quant := TBGRAColorQuantizer.Create(bmp, acFullChannelInPalette, 256); // reduce colors
  try
    quant.ApplyDitheringInplace(daNearestNeighbor, bmp);
    bmp.UsePalette := True;

    APalette := TFPPalette.Create(quant.ReducedPalette.Count);
    bmp.Palette.Clear;
    for i := 0 to quant.ReducedPalette.Count-1 do // copy reduced colors to palette
    begin
      bmp.Palette.Add(quant.ReducedPalette.Color[i].ToFPColor);
      APalette.Color[i] := quant.ReducedPalette.Color[i].ToFPColor;
    end;

    // RLE compress image
    bytes := TBytesStream.Create;
    try
      for y := 0 to bmp.Height-1 do
      begin
        p := bmp.Scanline[y];
        x := 0;
        while x < bmp.Width do
        begin
          i := quant.ReducedPalette.IndexOfColor(p[x]);
          if i >= 0 then
            clr := i
          else
            clr := quant.ReducedPalette.FindNearestColorIndex(p[x]);

          r := bmp.Scanline[y];
          len := 1;
          while (x + len < bmp.Width) and (len < $3FFF) do
          begin
            if r[x + len] <> p[x] then Break;
            Inc(len);
          end;

          if (len <= 2) and (clr <> 0) then // One pixel in color C
          begin
            bytes.WriteByte(clr);
            if len = 2 then bytes.WriteByte(clr);
          end
          else
          begin
            // rle id
            bytes.WriteByte(0);

            if (clr = 0) and (len < $40) then // L pixels in color 0 (L between 1 and 63)
              bytes.WriteByte(len)
            else if (clr = 0) then  // L pixels in color 0 (L between 64 and 16383)
            begin
              bytes.WriteByte($40 or (len shr 8));
              bytes.WriteByte(len);
            end
            else if len < $40 then // L pixels in color C (L between 3 and 63)
            begin
              bytes.WriteByte($80 or len);
              bytes.WriteByte(clr);
            end
            else // L pixels in color C (L between 64 and 16383)
            begin
              bytes.WriteByte($C0 or (len shr 8));
              bytes.WriteByte(len);
              bytes.WriteByte(clr);
            end;
          end;
          Inc(x, len);
        end;
        // end rle id
        bytes.WriteByte(0);
        bytes.WriteByte(0);
      end;
    finally
      Result := bytes.Size;
      SetLength(ABuffer, Result);
      Move(bytes.Bytes[0], ABuffer[0], Result);
      bytes.Free;
    end;
  finally
    quant.Free;
    bmp.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure WriteSUPFrame(const AStream: TStream; const ASubtitles: TUWSubtitles; const AIndex, ACompositionNumber: Integer; const AImage: TBGRABitmap; const AVideoWidth, AVideoHeight: Integer);
var
  pal : TFPPalette = NIL;
  rlebuf : TBytes;
  rlesize : Integer;
  x, it, ft : Integer;
  Y, Cb, Cr : Byte;
  pgs : TPGS;
  pcs : TPCS;
  wds : TWDS;
  pds : TPDS;
  pdse : TPDSEntry;
  ods : TODS;
  odse : TODSEntry;
  co  : TCO;
begin
  // Set 90kHz times
  it := ASubtitles[AIndex].InitialTime * 90;
  ft := ASubtitles[AIndex].FinalTime   * 90;

  // Get image buffer/pallete
  rlesize := EncodeImage(AImage, rlebuf, pal);

  // PCS - START
  with pgs do
  begin
    SetWord(PG, mwPG);
    SetDWord(PTS, it);
    SetDWord(DTS, 0);
    SegmentType := stfPCS;
    SetWord(SegmentSize, SizeOf(pcs) + SizeOf(co));
  end;
  AStream.Write(pgs, SizeOf(pgs));
  with pcs do
  begin
    SetWord(VideoWidth, AVideoWidth);
    SetWord(VideoHeight, AVideoHeight);
    FrameRate := $10;
    SetWord(CompositionNumber, ACompositionNumber);
    CompositionState := csfEpochStart;
    PaletteUpdateFlag := pufFalse;
    PaletteID := 0;
    NumberOfCompositionObjects := 1;
  end;
  AStream.Write(pcs, SizeOf(pcs));
  // CO
  with co do
  begin
    SetWord(ObjectID, 0);
    WindowID := 0;
    ObjectCroppedFlag := ocfOff;
    SetWord(ObjectHorizontalPosition, 0);
    SetWord(ObjectVerticalPosition, 0);
    SetWord(ObjectCroppingHorizontalPosition, 0);
    SetWord(ObjectCroppingVerticalPosition, 0);
    SetWord(ObjectCroppingWidth, 0);
    SetWord(ObjectCroppingHeightPosition, 0);
  end;
  AStream.Write(co, SizeOf(co));

  // WDS
  with pgs do
  begin
    SegmentType := stfWDS;
    SetWord(SegmentSize, SizeOf(wds));
  end;
  AStream.Write(pgs, SizeOf(pgs));
  with wds do
  begin
    NumberOfWindows := 1;
    WindowID := 0;
    SetWord(WindowHorizontalPosition, 0);
    SetWord(WindowVerticalPosition, 0);
    SetWord(WindowWidth, AImage.Width);
    SetWord(WindowHeight, AImage.Height);
  end;
  AStream.Write(wds, SizeOf(wds));

  // PDS
  with pgs do
  begin
    SegmentType := stfPDS;
    SetWord(SegmentSize, SizeOf(pds) + (SizeOf(pdse) * pal.Count));
  end;
  AStream.Write(pgs, SizeOf(pgs));
  with pds do
  begin
    PaletteID := 0;
    PaletteVersionNumber := 0;
    AStream.Write(pds, SizeOf(pds));
    with pdse do
    begin
      for x := 0 to pal.Count-1 do
      begin
        PaletteEntryID := x;
        ColorToYCbCr(pal.Color[x], Y, Cb, Cr, ituBT601);
        Luminance := Y;
        ColorDifferenceRed := Cr;
        ColorDifferenceBlue := Cb;
        Transparency := pal.Color[x].Alpha;
        AStream.Write(pdse, SizeOf(pdse));
      end;
    end;
    pal.Free;
  end;

  // ODS
  with pgs do
  begin
    SegmentType := stfODS;
    SetWord(SegmentSize, SizeOf(ods) + SizeOf(odse) + rlesize);
  end;
  AStream.Write(pgs, SizeOf(pgs));
  with ods do
  begin
    SetWord(ObjectID, 0);
    ObjectVersionNumber := 0;
    LastInSequenceFlag := lsfFirstAndLast;
  end;
  AStream.Write(ods, SizeOf(ods));
  with odse do
  begin
    SetDWord(ObjectDataLength, rlesize);
    SetWord(Width, AImage.Width);
    SetWord(Height, AImage.Height);
  end;
  AStream.Write(odse, SizeOf(odse));
  AStream.Write(rlebuf[0], rlesize); // RLE Data
  SetLength(rlebuf, 0);

  // END
  with pgs do
  begin
    SegmentType := stfEND;
    SetWord(SegmentSize, 0);
  end;
  AStream.Write(pgs, SizeOf(pgs));

  // PCS - END
  with pgs do
  begin
    SetDWord(PTS, ft);
    SegmentType := stfPCS;
    SetWord(SegmentSize, SizeOf(pcs));
  end;
  AStream.Write(pgs, SizeOf(pgs));
  with pcs do
  begin
    SetWord(CompositionNumber, ACompositionNumber + 1);
    CompositionState := csfNormal;
    NumberOfCompositionObjects := 0;
  end;
  AStream.Write(pcs, SizeOf(pcs));

  // WDS
  with pgs do
  begin
    SegmentType := stfWDS;
    SetWord(SegmentSize, SizeOf(wds));
  end;
  AStream.Write(pgs, SizeOf(pgs));
  AStream.Write(wds, SizeOf(wds));

  // END
  with pgs do
  begin
    SegmentType := stfEND;
    SetWord(SegmentSize, 0);
  end;
  AStream.Write(pgs, SizeOf(pgs));
end;

// -----------------------------------------------------------------------------

end.

