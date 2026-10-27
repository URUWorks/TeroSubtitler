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

unit MicroDVD;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWMicroDVD }

  TUWMicroDVD = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

function TUWMicroDVD.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWMicroDVD.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfMicroDVD;
end;

// -----------------------------------------------------------------------------

function TUWMicroDVD.Extension: String;
begin
  Result := '*.sub';
end;

// -----------------------------------------------------------------------------

function TUWMicroDVD.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWMicroDVD.HasStyleSupport: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWMicroDVD.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
var
  L, A, B: String;
  P2, P4: Integer;
begin
  Result := False;
  L := SubtitleFile[Row];
  if (L = '') or (L[1] <> '{') then Exit;
  P2 := Pos('}', L);
  if (P2 < 2) or (P2 >= Length(L)) or (L[P2 + 1] <> '{') then Exit;
  P4 := Pos('}', Copy(L, P2 + 2, MaxInt));
  if P4 = 0 then Exit;
  A := Trim(Copy(L, 2, P2 - 2));
  B := Trim(Copy(L, P2 + 2, P4 - 1));
  // Cada número es un cuadro o está vacío (inicio = "justo después del anterior",
  // fin = duración automática), pero no los dos. Antes la condición miraba
  // también las líneas vecinas y leía SubtitleFile[Row+1] sin comprobar el
  // final de la lista.
  Result := ((A = '') or IsInteger(A)) and ((B = '') or IsInteger(B)) and not ((A = '') and (B = ''));
end;

// -----------------------------------------------------------------------------

function TUWMicroDVD.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Double; var Subtitles: TUWSubtitles): Boolean;
var
  i         : Integer;
  StartStr, EndStr : String;
  P1, P2, P3, P4   : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  AFPS        : Single;
  FpsVal      : Double;
  FS          : TFormatSettings;
  ExtraInfo   : PMicroDVD_ExtraInfo;
  P_Start, P_Comma, P_End: Integer;
begin
  Result := False;
  try
    AFPS := FPS;
    for i := 0 to SubtitleFile.Count-1 do
    begin
      // DivXG400 FPS Info tag
      if (Copy(SubtitleFile[i], 1, 6) = '{1}{1}') or (Copy(SubtitleFile[i], 1, 6) = '{0}{0}') then
      begin
        // Antes IsInteger(...) antes de StrToFloat: el tag se escribe como
        // '{1}{1}23.976' y con el punto decimal IsInteger fallaba, así que el
        // FPS del archivo NUNCA se aplicaba (ni con un archivo de esta misma
        // app). Además se cambiaba el DecimalSeparator GLOBAL; ahora uno local.
        FS := FormatSettings;
        FS.DecimalSeparator := '.';
        FpsVal := StrToFloatDef(Trim(Copy(SubtitleFile[i], 7, Length(SubtitleFile[i]))), 0, FS);
        if (FpsVal >= 1) and (FpsVal <= 240) then
          AFPS := FpsVal;
        Continue;
      end;

      P1 := Pos('{', SubtitleFile[i]);
      if P1 > 0 then
      begin
        P2 := Pos('}', SubtitleFile[i], P1 + 1);
        if P2 > P1 then
        begin
          P3 := Pos('{', SubtitleFile[i], P2 + 1);
          if P3 > P2 then
          begin
            P4 := Pos('}', SubtitleFile[i], P3 + 1);
            if P4 > P3 then
            begin
              StartStr := Copy(SubtitleFile[i], P1 + 1, P2 - P1 - 1);
              EndStr   := Copy(SubtitleFile[i], P3 + 1, P4 - P3 - 1);
              Text     := Copy(SubtitleFile[i], P4 + 1, Length(SubtitleFile[i]));

              // Reemplazo de saltos de línea
              Text := ReplaceString(Text, '|', LineEnding);

              // Tiempos Iniciales
              if StartStr <> '' then
                InitialTime := FramesToTime(StrToIntDef(StartStr, 0), AFPS)
              else if (i > 0) and (Subtitles.Count > 0) then
                InitialTime := Subtitles.FinalTime[Subtitles.Count - 1] // Hereda final del anterior real
              else
                InitialTime := 0;

              // Tiempos Finales (sin cuadro de fin, la duración sale del largo
              // del texto, ver más abajo)
              if EndStr <> '' then
                FinalTime := FramesToTime(StrToIntDef(EndStr, 0), AFPS)
              else
                FinalTime := -1;

              // Extraer y ELIMINAR el código {P:X,Y}
              ExtraInfo := NIL;
              P_Start := Pos('{P:', Text);
              if P_Start > 0 then
              begin
                P_End := Pos('}', Text, P_Start);
                if P_End > P_Start then
                begin
                  P_Comma := Pos(',', Text, P_Start);
                  if (P_Comma > P_Start) and (P_Comma < P_End) then
                  begin
                    New(ExtraInfo);
                    ExtraInfo^.X := StrToIntDef(Copy(Text, P_Start + 3, P_Comma - P_Start - 3), 0);
                    ExtraInfo^.Y := StrToIntDef(Copy(Text, P_Comma + 1, P_End - P_Comma - 1), 0);

                    Delete(Text, P_Start, P_End - P_Start + 1);
                  end;
                end;
              end;

              // Parseo de tags
              Text := MicroDVDTagsToTS(Text);

              if EndStr = '' then
                FinalTime := InitialTime + CalculateOptimalDisplayMS(RemoveTSTags(Text));

              if (InitialTime >= 0) and (FinalTime > InitialTime) then
                Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo)
              else if ExtraInfo <> NIL then
                Dispose(ExtraInfo); // antes se perdía sin liberar
            end;
          end;
        end;
      end;
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiMicroDVD;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWMicroDVD.SaveSubtitle(const FileName: String; const FPS: Double; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const SubtitleMode: TSubtitleMode; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  i, T1, T2  : Integer;
  FS         : TFormatSettings;
  XY         : String;
  OutText    : String;
begin
  Result  := False;

  // DivXG400 FPS Info tag (TFormatSettings local en vez de cambiar y
  // restaurar el DecimalSeparator GLOBAL)
  FS := FormatSettings;
  FS.DecimalSeparator := '.';
  StringList.Add(SysUtils.Format('{1}{1}%.3f', [FPS], FS));

  for i := FromItem to ToItem do
  begin
    XY := '';
    if (Subtitles.ExtraInfoType = eiMicroDVD) and (Subtitles.ExtraInfo[i] <> NIL) then
      with PMicroDVD_ExtraInfo(Subtitles.ExtraInfo[i])^ do
        if (X > 0) or (Y > 0) then
          XY := SysUtils.Format('{P:%d,%d}', [X, Y]);

    OutText := iff(SubtitleMode = smText, Subtitles.Text[i], Subtitles.Translation[i]);
    OutText := TSTagsToMicroDVD(OutText);
    OutText := ReplaceEnters(OutText);

    // Un cue sin texto no se escribe; sin cuadros negativos y con el fin
    // posterior al inicio (el loader descarta los cues con fin <= inicio).
    if Trim(ReplaceString(OutText, '|', '')) = '' then Continue;
    T1 := TimeToFrames(Subtitles.InitialTime[i], FPS);
    T2 := TimeToFrames(Subtitles.FinalTime[i], FPS);
    if T1 < 0 then T1 := 0;
    if T2 <= T1 then T2 := T1 + 1;

    StringList.Add('{' + IntToStr(T1) + '}' +
                   '{' + IntToStr(T2) + '}' +
                   XY + OutText);
  end;

  try
    StringList.SaveToFile(FileName, Encoding);
    Result := True;
  except
  end;
end;

// -----------------------------------------------------------------------------

end.
