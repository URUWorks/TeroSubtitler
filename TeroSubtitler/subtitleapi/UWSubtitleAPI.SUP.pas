{*
 *  URUWorks Subtitle API
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
 *
 *  INFO: https://blog.thescorpius.com/index.php/2017/07/15/presentation-graphic-stream-sup-files-bluray-subtitle-format/
 *
 *}

unit UWSubtitleAPI.SUP;

//------------------------------------------------------------------------------

interface

type

  { TPGS }

  TPGS = packed record                 // Presentation Graphic Stream
    PG          : array[0..1] of Byte; // $50 $47 (Magic Word)
    PTS         : array[0..3] of Byte; // PTS (Presentation Timestamp) // Divide decimal value by 90 and the result is the value you are looking for in milliseconds
    DTS         : array[0..3] of Byte; // DTS (Decoding Timestamp) // Normally zero
    SegmentType : Byte;                // Segment Type (see Values below)
    SegmentSize : array[0..1] of Byte; // Size of the segment
  end;

const

  // Magic Word

  mwPG = $5047;

  // SegmentType

  stfPDS = $14;
  stfODS = $15;
  stfPCS = $16;
  stfWDS = $17;
  stfEND = $80;

type

  { TPCS }

  TPCS = packed record                                // Presentation Composition Segment
    VideoWidth                 : array[0..1] of Byte; // in pixels (ex. 0x780 = 1920)
    VideoHeight                : array[0..1] of Byte; // in pixels (ex. 0x438 = 1080)
    FrameRate                  : Byte;                // Always 0x10? (Can be ignored)
    CompositionNumber          : array[0..1] of Byte; // Number of this specific composition (It is incremented by one every time a graphics update occurs)
    CompositionState           : Byte;                // Type of this composition (see Values below)
    PaletteUpdateFlag          : Byte;                // Indicates if this PCS describes a Palette only Display Update (see Values below)
    PaletteID                  : Byte;                // ID of the palette to be used in the Palette only Display Update
    NumberOfCompositionObjects : Byte;                // Number of composition objects defined in this segment
  end;

const

  // FrameRate

  frf23976 = $10;
  frf24    = $20;
  frf25    = $30;
  frf2997  = $40;
  frf30    = $50;
  frf50    = $60;
  frf5994  = $70;

  // CompositionState

  csfNormal           = $00; // This defines a display update, and contains only functional segments with elements that are different from the preceding composition. It’s mostly used to stop displaying objects on the screen by defining a composition with no composition objects (a value of zero in the Number of Composition Objects flag) but also used to define a new composition with new objects and objects defined since the Epoch Start.
  csfAcquisitionPoint = $40; // This defines a display refresh. This is used to compose in the middle of the Epoch. It includes functional segments with new objects to be used in a new composition, replacing old objects with the same Object ID.
  csfEpochStart       = $80; // This defines a new display. The Epoch Start contains all functional segments needed to display a new composition on the screen.

  // PaletteUpdateFlag

  pufFalse = $00;
  pufTrue  = $80;

type

  { TCO }

  TCO = packed record                                       // Composition objects
    ObjectID                         : array[0..1] of Byte; // ID of the ODS segment that defines the image to be shown
    WindowID                         : Byte;                // Id of the WDS segment to which the image is allocated in the PCS. Up to two images may be assigned to one window
    ObjectCroppedFlag                : Byte;                // see Values below
    ObjectHorizontalPosition         : array[0..1] of Byte; // X offset from the top left pixel of the image on the screen
    ObjectVerticalPosition           : array[0..1] of Byte; // Y offset from the top left pixel of the image on the screen
    ObjectCroppingHorizontalPosition : array[0..1] of Byte; // X offset from the top left pixel of the cropped object in the screen. Only used when the Object Cropped Flag is set to $40
    ObjectCroppingVerticalPosition   : array[0..1] of Byte; // Y offset from the top left pixel of the cropped object in the screen. Only used when the Object Cropped Flag is set to $40
    ObjectCroppingWidth              : array[0..1] of Byte; // Width of the cropped object in the screen. Only used when the Object Cropped Flag is set to $40
    ObjectCroppingHeightPosition     : array[0..1] of Byte; // Height of the cropped object in the screen. Only used when the Object Cropped Flag is set to $40
  end;

const

  // CompositionObjects

  ocfOff          = $00; // Off
  ocfForceDisplay = $40; // Force display of the cropped image object

type

  { TWDS }

  TWDS = packed record                              // Window Definition Segment
    NumberOfWindows          : Byte;                // Number of windows defined in this segment
    WindowID                 : Byte;                // ID of this window
    WindowHorizontalPosition : array[0..1] of Byte; // X offset from the top left pixel of the window in the screen
    WindowVerticalPosition   : array[0..1] of Byte; // Y offset from the top left pixel of the window in the screen
    WindowWidth              : array[0..1] of Byte; // Width of the window
    WindowHeight             : array[0..1] of Byte; // Height of the window
  end;

  { TPDS }

  TPDS = packed record           // Palette Definition Segment
    PaletteID            : Byte; // ID of the palette
    PaletteVersionNumber : Byte; // Version of this palette within the Epoch
  end;

  TPDSEntry = packed record      // Palette Definition Segment Entry
    PaletteEntryID       : Byte; // Entry number of the palette
    Luminance            : Byte; // Luminance (Y value)
    ColorDifferenceRed   : Byte; // Color Difference Red (Cr value)
    ColorDifferenceBlue  : Byte; // Color Difference Blue (Cb value)
    Transparency         : Byte; // Transparency (Alpha value)
  end;

  { TODS }

  TODS = packed record                         // Object Definition Segment
    ObjectID            : array[0..1] of Byte; // ID of this object
    ObjectVersionNumber : Byte;                // Version of this object
    LastInSequenceFlag  : Byte;                // If the image is split into a series of consecutive fragments, the last fragment has this flag set (see Values below)
  end;

  TODSEntry = packed record                    // Object Definition Segment Entry
    ObjectDataLength    : array[0..2] of Byte; // The length of the Run-length Encoding (RLE) data buffer with the compressed image data
    Width               : array[0..1] of Byte; // Width of the image
    Height              : array[0..1] of Byte; // Height of the image
    //ObjectData          : array of Byte;       // This is the image data compressed using Run-length Encoding (RLE). The size of the data is defined in the Object Data Length field
  end;

const

  // LastInSequenceFlag

  lsfLast         = $40; // Last in sequence
  lsfFirst        = $80; // First in sequence
  lsfFirstAndLast = $C0; // First and last in sequence

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.
