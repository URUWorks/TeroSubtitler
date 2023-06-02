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
 *  Copyright (C) 2001-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSubtitleAPI.Formats.CheetahCaption.Types;

//------------------------------------------------------------------------------

interface

type

  { THeaderBlock }

  THeaderBlock = record
    Header           : array[0..3] of Byte;   // $EA $22 1 0
    NumberOfLines    : array[0..1] of Byte;   // low high
    UnknowCodes1     : array[0..3] of Byte;   // 9 $A8 $AF $4F
    UnknowCodes2     : array[0..117] of Byte; // $0
  end;

  { TAlignmentBlock }

  TAlignmentBlock = record
    UnknowCodes1  : array[0..1] of Byte; // 0 0
    Justification : Byte;                // 3   1=left 2=right 3=center
    HorizontalPos : Byte;                // $E  1=top F=bottom
    VerticalPos   : Byte;                // $10 3=left $10=center $19=right
  end;

  { TStyleBlock }

  TStyleBlock = record
    UnknowCodes1  : array[0..5] of Byte; // $12 1 0 0 0 0
    Justification : Byte;                // 3   1=left 2=right 3=center
    HorizontalPos : Byte;                // $F  1=top F=bottom
    VerticalPos   : Byte;                // $10 3=left $10=center $19=right
  end;

  //Normal        : 12 01 00 00 00 00 03 0F 10
  //Right-top     : 12 01 00 00 00 00 03 01 1C
  //Top           : 12 01 00 00 00 00 03 01 10
  //Left-top      : 12 01 00 00 00 00 03 01 05
  //Left          : 12 01 00 00 00 00 03 0F 0A
  //Right         : 12 01 00 00 00 00 03 0F 1E
  //Left          : 12 03 00 00 00 00 03 0F 07

const


  TLatinCodes : array[0..20] of Byte =
  (
    $81, // ♪
    $82, // á
    $83, // é
    $84, // í
    $85, // ó
    $86, // ú
    $87, // â
    $88, // ê
    $89, // î
    $8A, // ô
    $8B, // û
    $8C, // à
    $8D, // è
    $8E, // Ñ
    $8F, // ñ
    $90, // ç
    $91, // ¢
    $92, // £
    $93, // ¿
    $94, // ½
    $95  // ®
  );

  TLatinLetters : array[0..20] of WideChar =
  (
    '♪',
    'á',
    'é',
    'í',
    'ó',
    'ú',
    'â',
    'ê',
    'î',
    'ô',
    'û',
    'à',
    'è',
    'Ñ',
    'ñ',
    'ç',
    '¢',
    '''',
    '¿',
    '½',
    '®'
  );

//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.
