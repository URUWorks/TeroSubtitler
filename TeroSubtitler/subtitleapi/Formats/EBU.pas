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
 *  Copyright (C) 2001-2026 URUWorks, uruworks@gmail.com.
 *}

unit EBU;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, UWSubtitleAPI, UWSystem.SysUtils,
  UWSubtitleAPI.Formats, StrUtils;

type

  { TUWEBU }

  TUWEBU = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTextBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.ExtraInfo, EBU.Types, UWSystem.Encoding, Math;

// -----------------------------------------------------------------------------

procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);
var
  i, MaxLen: Integer;
begin
  MaxLen := High(Dest);
  if MaxLen > Length(Source) then MaxLen := Length(Source)-1;

  for i := 0 to MaxLen do
    Dest[i] := Source[i+1];
end;

// -----------------------------------------------------------------------------

procedure SubtitleNumberToByteArray(var Dest: array of Byte; const Source: Cardinal);
begin
  // little-endian (spec EBU Tech 3264). Antes IntToHex(...,4) con numeros
  // > FFFFh daba 5 digitos y los bytes salian desplazados.
  Dest[0] := Source and $FF;
  Dest[1] := (Source shr 8) and $FF;
end;

// -----------------------------------------------------------------------------

// Milisegundos -> timecode HH MM SS FF a la frecuencia del ARCHIVO (25/30).
// Antes se escribía con 'hhmmssff' al FPS de la app: con 23.976 fps en un
// STL25 los 'ff' se leían luego como /25 y los tiempos se desviaban.
procedure TimeToEBUTimeCode(var Dest: array of Byte; const TimeMs: Integer; const FileFps: Integer);
var
  Total, Rem: Int64;
begin
  if TimeMs <= 0 then
    Total := 0
  else
    Total := Round(TimeMs / 1000 * FileFps);

  Dest[0] := Byte(Total div (3600 * FileFps));
  Rem     := Total mod (3600 * FileFps);
  Dest[1] := Byte(Rem div (60 * FileFps));
  Rem     := Rem mod (60 * FileFps);
  Dest[2] := Byte(Rem div FileFps);
  Dest[3] := Byte(Rem mod FileFps);
end;

// -----------------------------------------------------------------------------

function FieldStr(const A; const Size: Integer): AnsiString;
begin
  SetString(Result, PAnsiChar(@A), Size);
end;

// -----------------------------------------------------------------------------

function TUWEBU.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWEBU.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfEBU;
end;

// -----------------------------------------------------------------------------

function TUWEBU.Extension: String;
begin
  Result := '*.stl';
end;

// -----------------------------------------------------------------------------

function TUWEBU.IsTextBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWEBU.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWEBU.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  sub: TUWSubtitles;
begin
  Result := False;
  if LowerCase(ExtractFileExt(SubtitleFile.FileName)) <> '.stl' then Exit;

  sub := TUWSubtitles.Create;
  try
    Result := LoadSubtitle(SubtitleFile, 0, sub);
  finally
    sub.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWEBU.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  UWFile: TFileStream;
  GSIBlock: TGSIBlock;
  TTIBlock: TTTIBlock;
  Encoding: TEncoding;
  Cur: TBytes;
  CurLen, n, k: Integer;
  Pending: Boolean;
  ExtraInfo: PEBU_ExtraInfo;
  FileFps: Integer;
  InitialTime, FinalTime: Integer;
  cCS, cVP, cJC, cCF: Byte;
  Text: String;
  NewIdx: Integer;
  TotalRows: Integer;
  Info: PEBU_Info;

  procedure CommitCue;
  begin
    Pending := False;
    if (cCF <> CF_TFHaveData) or (CurLen = 0) then Exit; // comentario o vacio
    SetLength(Cur, CurLen);
    Text := EBUBytesToText(Cur, Encoding);
    if Text = '' then Exit;

    if (InitialTime < 0) or (FinalTime < 0) then Exit;

    New(ExtraInfo);
    ExtraInfo^.CumulativeStatus  := cCS;
    ExtraInfo^.VerticalPosition  := cVP;
    ExtraInfo^.JustificationCode := cJC;
    ExtraInfo^.CommentFlag       := cCF;

    NewIdx := Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo);
    if NewIdx < 0 then
    begin
      Dispose(ExtraInfo);
      Exit;
    end;

    // Justification/VerticalPosition también en los campos genéricos Align/VAlign
    case cJC of
      JC_Left:   Subtitles.ItemPointer[NewIdx]^.Align := shaLeft;
      JC_Center: Subtitles.ItemPointer[NewIdx]^.Align := shaCenter;
      JC_Right:  Subtitles.ItemPointer[NewIdx]^.Align := shaRight;
      // JC_Unchanged: sin override explícito, se deja el default del item
    end;

    if TotalRows > 2 then
    begin
      if cVP < (TotalRows div 3) then
        Subtitles.ItemPointer[NewIdx]^.VAlign := svaTop
      else if cVP >= (2 * TotalRows div 3) then
        Subtitles.ItemPointer[NewIdx]^.VAlign := svaBottom
      else
        Subtitles.ItemPointer[NewIdx]^.VAlign := svaCenter;
    end;
  end;

begin
  Result := False;
  Encoding := NIL;
  Pending := False;
  CurLen := 0;
  TotalRows := 23;
  // Archivo inexistente o inaccesible: UWFile queda en NIL y se devuelve False
  // (antes TUWFile.Ready = False).
  try
    UWFile := TFileStream.Create(SubtitleFile.FileName, fmOpenRead or fmShareDenyWrite);
  except
    UWFile := NIL;
  end;
  try
    if Assigned(UWFile) then
    begin
      if UWFile.Read(GSIBlock, GSIBlockSize) = GSIBlockSize then
      begin
        // El GSI debe empezar por 'STL' en el DFC (STL25.01 / STL30.01). Sin esto,
        // cualquier archivo .stl de texto (p.ej. Spruce) se "parseaba" como
        // binario y podia dar subtitulos basura.
        if not ((GSIBlock.DiskFormatCode[0] = 'S') and (GSIBlock.DiskFormatCode[1] = 'T') and
                (GSIBlock.DiskFormatCode[2] = 'L')) then Exit;

        // FrameRate = FRAMES POR SEGUNDO del archivo ('STL25.01' -> 25)
        FileFps := StrToIntDef(Trim(String(FieldStr(GSIBlock.DiskFormatCode[3], 2))), 25);
        if (FileFps < 20) or (FileFps > 60) then FileFps := 25;

        TotalRows := StrToIntDef(Trim(String(FieldStr(GSIBlock.MaxNumberDisplayableRows, 2))), 23);
        if TotalRows <= 0 then TotalRows := 23;

        // Propiedades del GSI para el round-trip (antes solo se escribian desde la
        // config: al guardar un STL cargado el CCT salia vacio, etc.)
        Info := @Subtitles.FormatProperties^.EBU;
        if FileFps = 25 then Info^.DiskFormatCode := 0 else Info^.DiskFormatCode := 1;
        Info^.CodePageNumber            := Trim(FieldStr(GSIBlock.CodePageNumber, 3));
        Info^.DisplayStandardCode       := GSIBlock.DisplayStandardCode;
        Info^.CharCodeTableNumber       := FieldStr(GSIBlock.CharCodeTableNumber, 2);
        Info^.LanguageCode              := FieldStr(GSIBlock.LanguageCode, 2);
        Info^.CountryOrigin             := Trim(FieldStr(GSIBlock.CountryOrigin, 3));
        Info^.MaxNumberDisplayableChars := FieldStr(GSIBlock.MaxNumberDisplayableChars, 2);
        Info^.MaxNumberDisplayableRows  := FieldStr(GSIBlock.MaxNumberDisplayableRows, 2);

        // instancia compartida (caché): no se libera
        Encoding := GetCachedEncoding(CharCodeTableToEncoding(FieldStr(GSIBlock.CharCodeTableNumber, 2)));
        try
          SetLength(Cur, 0);
          InitialTime := -1;
          FinalTime   := -1;
          cCS := 0; cVP := 0; cJC := 0; cCF := 0;

          // Un bloque TTI incompleto al final (archivo truncado) se descarta:
          // antes BlockRead lo daba por bueno y se procesaban bytes sobrantes
          // del bloque anterior.
          while UWFile.Position < UWFile.Size do
            if UWFile.Read(TTIBlock, TTIBlockSize) = TTIBlockSize then
            with TTIBlock do
            begin
              if ExtBlockNumber = $FE then Continue; // datos de usuario

              if not Pending then
              begin
                // primer bloque de un subtitulo: tiempos y atributos
                InitialTime := Round(((TimeCodeIn[0]*3600 + TimeCodeIn[1]*60 + TimeCodeIn[2]) + TimeCodeIn[3] / FileFps) * 1000);
                FinalTime   := Round(((TimeCodeOut[0]*3600 + TimeCodeOut[1]*60 + TimeCodeOut[2]) + TimeCodeOut[3] / FileFps) * 1000);
                cCS := CumulativeStatus;
                cVP := VerticalPosition;
                cJC := JustificationCode;
                cCF := CommentFlag;
                CurLen := 0;
              end;

              // Texto hasta el primer 8Fh (relleno). Los bloques de extension
              // (EBN 00h-EFh) continuan el texto del subtitulo; EBN = FFh lo cierra.
              n := 0;
              while (n <= High(TextField)) and (TextField[n] <> TF_UnusedSpace) do Inc(n);
              if CurLen + n > Length(Cur) then SetLength(Cur, (CurLen + n) * 2);
              for k := 0 to n - 1 do
              begin
                Cur[CurLen] := TextField[k];
                Inc(CurLen);
              end;

              if ExtBlockNumber = $FF then
                CommitCue
              else
                Pending := True;
            end;

          if Pending then CommitCue; // archivo truncado en medio de un subtitulo
        finally
          // Encoding es una instancia compartida de la caché, no se libera
        end;
      end;
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiEBU;
      Result := True;
    end;
    UWFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWEBU.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
type
  TTTIArray = array of TTTIBlock;
var
  Stream: TStream;
  GSIBlock: TGSIBlock;
  TTIBlock: TTTIBlock;
  Blocks: TTTIArray;
  BlockCount: Integer;
  i, b, n, FileFps, TotalRows, SubCount: Integer;
  Info: PEBU_Info;
  CCT: AnsiString;
  Enc: TEncoding;
  Data: TBytes;
  VP, JC: Byte;
  Cnt: Integer;
  sTCF: String;
  TC: array[0..3] of Byte;
begin
  Result := False;
  Info := @Subtitles.FormatProperties^.EBU;

  if Info^.DiskFormatCode = 0 then FileFps := 25 else FileFps := 30;

  TotalRows := StrToIntDef(Trim(String(Info^.MaxNumberDisplayableRows)), 23);
  if TotalRows <= 0 then TotalRows := 23;

  // El texto EBU se codifica SIEMPRE con la tabla de caracteres del propio GSI
  // (CCT), igual que al cargar; el 'Encoding' del usuario (p.ej. UTF-8) daria
  // bytes que ningun lector EBU entiende. CCT vacio/invalido -> '00' (Latin).
  CCT := Trim(Info^.CharCodeTableNumber);
  if (Length(CCT) <> 2) or not (CCT[1] in ['0'..'9']) or not (CCT[2] in ['0'..'9']) then
    CCT := '00';
  Enc := GetCachedEncoding(CharCodeTableToEncoding(CCT)); // compartida, no se libera
  try
    // --- Bloques TTI (se construyen antes: el GSI lleva el total de bloques) ---
    SubCount   := 0;
    BlockCount := 0;
    SetLength(Blocks, 0);

    for i := FromItem to ToItem do
    begin
      Data := EBUTextToBytes(iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]), Enc);
      if Length(Data) > 239 * 112 + 111 then SetLength(Data, 239 * 112 + 111); // max EBN EFh

      if (Subtitles.ExtraInfoType = eiEBU) and (PEBU_ExtraInfo(Subtitles.ExtraInfo[i]) <> NIL) then
      begin
        VP := PEBU_ExtraInfo(Subtitles.ExtraInfo[i])^.VerticalPosition;
        JC := PEBU_ExtraInfo(Subtitles.ExtraInfo[i])^.JustificationCode;
      end
      else
      begin
        // Sin ExtraInfo EBU (item creado en la app o cargado de otro formato):
        // se deriva de Align/VAlign (antes siempre fila 0 = arriba de todo).
        case Subtitles[i].Align of
          shaLeft:   JC := JC_Left;
          shaCenter: JC := JC_Center;
          shaRight:  JC := JC_Right;
        else
          JC := JC_Unchanged;
        end;

        case Subtitles[i].VAlign of
          svaTop:    VP := TotalRows div 6;
          svaCenter: VP := TotalRows div 2;
        else
          VP := Max(0, TotalRows - 2);
        end;
      end;

      // Un subtitulo = 1..240 bloques de 112 bytes; el ultimo debe terminar en
      // 8Fh (siempre hay al menos un byte de relleno). Antes el texto se
      // TRUNCABA a 112 bytes.
      n := (Length(Data) div 112) + 1;
      SetLength(Blocks, BlockCount + n);
      for b := 0 to n - 1 do
      begin
        FillChar(TTIBlock, SizeOf(TTTIBlock), TF_UnusedSpace);
        with TTIBlock do
        begin
          SubtitleGroupNumber := $00;
          SubtitleNumberToByteArray(SubtitleNumber, SubCount);
          if b = n - 1 then ExtBlockNumber := $FF else ExtBlockNumber := b;
          CumulativeStatus := CS_Not;
          TimeToEBUTimeCode(TimeCodeIn, Subtitles.InitialTime[i], FileFps);
          TimeToEBUTimeCode(TimeCodeOut, Subtitles.FinalTime[i], FileFps);
          VerticalPosition  := VP;
          JustificationCode := JC;
          CommentFlag       := CF_TFHaveData;
          Cnt := Length(Data) - b * 112;
          if Cnt > 112 then Cnt := 112;
          if Cnt > 0 then Move(Data[b * 112], TextField[0], Cnt);
        end;
        Blocks[BlockCount + b] := TTIBlock;
      end;
      Inc(BlockCount, n);
      Inc(SubCount);
    end;

    // --- GSI ---
    FillChar(GSIBlock, SizeOf(TGSIBlock), DSC_Undefined);
    with GSIBlock do
    begin
      AnsiStringToAnsiChar(CodePageNumber, Info^.CodePageNumber);
      if FileFps = 25 then DiskFormatCode := DFC_25 else DiskFormatCode := DFC_30;
      // DSC 0 no es un valor valido (20h = no definido)
      if Info^.DisplayStandardCode = 0 then DisplayStandardCode := DSC_Undefined
      else DisplayStandardCode := Info^.DisplayStandardCode;
      AnsiStringToAnsiChar(CharCodeTableNumber, CCT);
      AnsiStringToAnsiChar(LanguageCode, Info^.LanguageCode);
      AnsiStringToAnsiChar(CreationDate, AnsiString(FormatDateTime('yymmdd', Now)));
      RevisionDate   := CreationDate;
      RevisionNumber := '00';
      // TNB = total de bloques TTI (con extensiones), TNS = total de subtitulos
      AnsiStringToAnsiChar(TotalNumberTTIBlocks, AnsiString(AddChar('0', IntToStr(BlockCount), 5)));
      AnsiStringToAnsiChar(TotalNumberSubtitles, AnsiString(AddChar('0', IntToStr(SubCount), 5)));
      TotalNumberSubtitleGroups := '000';
      AnsiStringToAnsiChar(MaxNumberDisplayableChars, Info^.MaxNumberDisplayableChars);
      AnsiStringToAnsiChar(MaxNumberDisplayableRows, Info^.MaxNumberDisplayableRows);
      TimeCodeStatus := TCS_Use;
      TimeCodeStartProgramme := '00000000';
      if SubCount > 0 then
      begin
        TimeToEBUTimeCode(TC, Subtitles.InitialTime[FromItem], FileFps);
        sTCF := SysUtils.Format('%.2d%.2d%.2d%.2d', [TC[0], TC[1], TC[2], TC[3]]);
        AnsiStringToAnsiChar(TimeCodeFirstCue, AnsiString(sTCF));
      end
      else
        TimeCodeFirstCue := '00000000';
      TotalNumberDisks   := $31;
      DiskSequenceNumber := $31;
      AnsiStringToAnsiChar(CountryOrigin, Info^.CountryOrigin);
      AnsiStringToAnsiChar(Publisher, 'URUWorks');
      AnsiStringToAnsiChar(EditorName, 'URUWorks');
      AnsiStringToAnsiChar(EditorContact, 'uruworks.net');
    end;

    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.WriteBuffer(GSIBlock, SizeOf(TGSIBlock));
      for i := 0 to BlockCount - 1 do
        Stream.WriteBuffer(Blocks[i], SizeOf(TTTIBlock));
      Result := True;
    finally
      Stream.Free;
    end;
  finally
    // Enc es una instancia compartida de la caché, no se libera
  end;
end;

// -----------------------------------------------------------------------------

end.
