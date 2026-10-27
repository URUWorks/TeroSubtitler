{*
 *  URUWorks Waveform Displayer Control
 *
 *  Copyright (C) 2021-2023 URUWorks, uruworks@gmail.com.
 *
 *  Based on the great work of:
 * -----------------------------------------------------------------------------
 *  VisualSubSync
 * -----------------------------------------------------------------------------
 *  Copyright (C) 2003 Christophe Paris
 * -----------------------------------------------------------------------------
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *  http://www.gnu.org/copyleft/gpl.html
 * -----------------------------------------------------------------------------
 *}

// -----------------------------------------------------------------------------

{ DrawAlphaRect }

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawAlphaRect(const ABitmap:{$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const x1, x2 : Integer; const y1, y2 : Integer; const AColor: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF});
var
  r : TRect;
begin
  r.Left   := x1;
  r.Right  := x2;
  r.Top    := y1;
  r.Bottom := y2;

  {$IFDEF USEBGRABITMAP}
  ABitmap.FillRect(r, ColorToBGRA(AColor, 10), dmDrawWithTransparency)
  {$ELSE}
  laz.VTGraphics.AlphaBlend(ABitmap.Canvas.Handle, ABitmap.Canvas.Handle, r,
    Point(0, 0), bmConstantAlphaAndColor, 40, AColor);
  {$ENDIF}
end;

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawAlphaRectByTimes(const ABitmap:{$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const t1, t2 : Integer; const y1, y2 : Integer; const AColor: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF});
var
  x1, x2 : Integer;
begin
  x1 := TimeToPixel(t1 - FPositionMS);
  x2 := TimeToPixel(t2 - FPositionMS);

  Constrain(x1, 0, Width);
  Constrain(x2, 0, Width);
  DrawAlphaRect(ABitmap, x1, x2, y1, y2, AColor);
end;

// -----------------------------------------------------------------------------

{ DrawWave }

// -----------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawWave(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  x, y1, y2            : Integer;
  x1_update, x2_update : Integer;
  x_scaled, Next_x_scaled : Integer;
  PeaksPerPixelScaled  : Double;
  StartPositionInPeaks : Double;
  CurrentPeakPos       : Double;
  Middle               : Integer;
  PeakMax, PeakMin     : Integer;
  i                    : Integer;
  RectHeight           : Integer;
  {$IFNDEF USEBGRABITMAP}
  WavePoints           : array of TPoint;
  TotalPoints          : Integer;
  {$ENDIF}
  PointIdx             : Integer;
  ScaleFactorY         : Double;
begin
  {$IFDEF USEBGRABITMAP}
  ABitmap.Fill(CustomColors.Background);
  {$ELSE}
  ABitmap.Canvas.Brush.Color := CustomColors.Background;
  ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
  {$ENDIF}
  DrawGridLines(ABitmap);

  if not FPeakDataLoaded then Exit;

  // Preparar area de dibujo
  {$IFDEF USEBGRABITMAP}
  //Pen.Color := CustomColors.ItemIT; // Comentado en original
  ABitmap.CanvasBGRA.Pen.Style := psDot;
  ABitmap.CanvasBGRA.Line(0, GetWAVECanvasY, Width, GetWAVECanvasY);
  {$ELSE}
  ABitmap.Canvas.Pen.Style := psDot;
  ABitmap.Canvas.Line(0, GetWAVECanvasY, Width, GetWAVECanvasY);
  {$ENDIF}

  // Calculos previos
  PeaksPerPixelScaled := (((FPageSizeMS / 1000.0) * FWaveFormat.nSamplesPerSec) / FSamplesPerPeak) / Width;
  StartPositionInPeaks := ((FPositionMS / 1000.0) * FWaveFormat.nSamplesPerSec) / FSamplesPerPeak;

  // the whole wave is redrawn: it's cheap with VertLine (copying the old
  // part on scroll is not worth it, the grid/time line are on this layer)
  x1_update := 0;
  x2_update := Width;

  if x1_update >= x2_update then Exit;

  RectHeight := GetSubCanvasHeight;
  Middle     := GetWAVECanvasY + (RectHeight div 2);

  // Pre-calcular factor de escala Y
  ScaleFactorY := (FVerticalScaling * RectHeight) / (100.0 * 65536.0);

  {$IFNDEF USEBGRABITMAP}
  // Preparar array de puntos para Polygon
  // Necesitamos (Ancho * 2) puntos: ida por arriba, vuelta por abajo
  TotalPoints := (x2_update - x1_update);
  SetLength(WavePoints, (TotalPoints * 2));
  {$ENDIF}

  CurrentPeakPos := (x1_update * PeaksPerPixelScaled) + StartPositionInPeaks;
  PointIdx := 0;

  for x := x1_update to x2_update - 1 do
  begin
    x_scaled := Round(CurrentPeakPos);
    if (x_scaled >= FPeakTabSize) then x_scaled := FPeakTabSize - 1;

    // Calcular el siguiente indice para el bucle interno
    CurrentPeakPos := CurrentPeakPos + PeaksPerPixelScaled;
    Next_x_scaled := Round(CurrentPeakPos);
    if Next_x_scaled > FPeakTabSize then Next_x_scaled := FPeakTabSize;
    if Next_x_scaled <= x_scaled then Next_x_scaled := x_scaled + 1;

    // Buscar Min/Max en el rango
    PeakMax := FPeakTab[x_scaled].Max;
    PeakMin := FPeakTab[x_scaled].Min;

    // Bucle interno optimizado (se ejecuta solo si hay más de 1 muestra por pixel)
    if Next_x_scaled > x_scaled + 1 then
    begin
      for i := x_scaled + 1 to Next_x_scaled - 1 do
      begin
        if FPeakTab[i].Max > PeakMax then PeakMax := FPeakTab[i].Max;
        if FPeakTab[i].Min < PeakMin then PeakMin := FPeakTab[i].Min;
      end;
    end;

    // Calcular coordenadas Y usando el factor pre-calculado
    y1 := Round(PeakMax * ScaleFactorY);
    y2 := Round(PeakMin * ScaleFactorY);

    {$IFDEF USEBGRABITMAP}
    // a vertical line per column: the same shape as the filled polygon,
    // ~15x faster than BGRABitmap Polygon
    ABitmap.VertLine(x, Middle - y1, Middle - y2, CustomColors.Waveform, dmSet);
    {$ELSE}
    // Guardar puntos en el array
    // Parte superior (indice 0 hacia adelante)
    WavePoints[PointIdx] := Point(x, Middle - y1);
    // Parte inferior (indice Final hacia atrás)
    WavePoints[Length(WavePoints) - 1 - PointIdx] := Point(x, Middle - y2);
    {$ENDIF}

    Inc(PointIdx);
  end;

  // Dibujar el Poligono completo
  with ABitmap.{$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF} do
  begin
    {$IFDEF USEBGRABITMAP}
    Brush.BGRAColor := CustomColors.Waveform;
    Pen.BGRAColor   := CustomColors.Waveform;
    {$ELSE}
    Brush.Color := CustomColors.Waveform;
    Pen.Color   := CustomColors.Waveform;
    {$ENDIF}
    Brush.Style := bsSolid;
    Pen.Style   := psSolid;

    {$IFNDEF USEBGRABITMAP}
    Polygon(WavePoints);
    {$ENDIF}

    // Restaurar brush para otros dibujos
    Brush.Style := bsClear;

    // Zero line
    Pen.Color := CustomColors.Waveform;
    Line(0, Middle, Width, Middle);
  end;
end;

// -----------------------------------------------------------------------------

{ DrawTimeLine }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawTimeLine(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  PosRect : TRect;
  PosString : String;
  p, x, x1, x2, oldx : Integer;
  StepFrames, SubFrames, k : Integer;
  f : Int64;
begin
  if not IsTimeLineEnabled then Exit;

  with ABitmap do
  begin
    // Set the text font
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Color := CustomColors.Text;
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;
    //CanvasBGRA.Font.Name := 'Tahoma';
    {$IFDEF USEBGRABITMAP}
    CanvasBGRA.Font.Height := 10;
    {$ELSE}
    Canvas.Font.Size := 7;
    {$ENDIF}
    // text height + a strip below it for the ticks, so labels and ticks don't mix
    FTimeLineHeight := {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.TextHeight('0') + 5;

    PosRect        := ClientRect;
    PosRect.Bottom := PosRect.Bottom - FScrollBar.Height;
    PosRect.Top    := PosRect.Bottom - FTimeLineHeight;

    // Draw background
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Brush.Style := bsClear;
    {$IFDEF USEBGRABITMAP}CanvasBGRA.Font.BGRAColor{$ELSE}Canvas.Font.Color{$ENDIF} := CustomColors.Text;
    oldx := -1;

    if FFPSTimeMode then
    begin
      // Frames mode: ticks on whole frames, step in whole frames
      GetFrameRulerStep(StepFrames, SubFrames);
      f := RulerFirstFrame(StepFrames);
      p := RulerFrameToMs(f);
      while (p < FPositionMS + FPageSizeMS) do
      begin
        // Draw main division
        x := TimeToPixel(p - FPositionMS);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, PosRect.Top +2, x, PosRect.Bottom);
        PosString := MSToHHMMSSFFTime(p + FOffsetMs, FFPS, FTimecodeMode);
        x1 := x + 3;
        if oldx <> x1 then
        begin
          DoTextOut(ABitmap, x1, PosRect.Top, PosString);
          oldx := x1;
        end;
        // Draw subdivisions (and a small tick per frame if there is room)
        for k := 1 to StepFrames - 1 do
          if (SubFrames > 0) and (k mod SubFrames = 0) then
          begin
            x2 := TimeToPixel(RulerFrameToMs(f + k) - FPositionMS);
            {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x2, PosRect.Bottom -4, x2, PosRect.Bottom);
          end
          else if GetFramePixels >= MinFramePixels then
          begin
            x2 := TimeToPixel(RulerFrameToMs(f + k) - FPositionMS);
            {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x2, PosRect.Bottom -2, x2, PosRect.Bottom);
          end;
        f := f + StepFrames;
        p := RulerFrameToMs(f);
      end;
      Exit;
    end;

    p := (FPositionMS div FStepMs * FStepMs);
    while (p < FPositionMS + FPageSizeMS) do
    begin
      // Draw main division
      x := TimeToPixel(p - FPositionMS);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, PosRect.Top +2, x, PosRect.Bottom);
      PosString := TimeMSToShortString(p + FOffsetMs, FStepLog);
      // Calculate text coordinate
      x1 := x + 3; //x - (CanvasBGRA.TextWidth(PosString) div 2);
      // Draw text
      if oldx <> x1 then
      begin
        DoTextOut(ABitmap, x1, PosRect.Top, PosString);
        oldx := x1;
      end;
      // Draw subdivision
      x2 := x + TimeToPixel(FStepMs div 2);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x2, PosRect.Bottom -4, x2, PosRect.Bottom);
      p := p + FStepMs;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ DrawItemsCanvas }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawItemsCanvas(const ACompleteDraw: Boolean = False);
var
  y1, y2: Integer;
begin
  if not Assigned(FSubtitles) then Exit;

  if ACompleteDraw then
  begin
    // Draw WAVE
    {$IFDEF USEBGRABITMAP}
    FBackBufferItems.Assign(FBackBufferWAVE);
    {$ELSE}
    FBackBufferItems.Canvas.Draw(0, 0, FBackBufferWAVE);
    {$ENDIF}
    // Draw Subtitle Items
    //FBackBufferItems.FontStyle := [TFontStyle.fsBold];
    y1 := GetSubCanvasY;
    y2 := y1 + GetSubCanvasHeight;
    DrawMinimumBlank(FBackBufferItems, y1, y2);
    DrawSceneChange(FBackBufferItems, GetWAVECanvasY, GetWAVECanvasY + GetSubCanvasHeight);
    DrawSubtitleItem(FBackBufferItems, y1, y2);

    {$IFDEF USEBGRABITMAP}
    FBackBuffer.Assign(FBackBufferItems);
    {$ELSE}
    FBackBuffer.Canvas.Draw(0, 0, FBackBufferItems);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF USEBGRABITMAP}
    FBackBuffer.Assign(FBackBufferItems);
    {$ELSE}
    FBackBuffer.Canvas.Draw(0, 0, FBackBufferItems);
    {$ENDIF}
  end;

  {$IFDEF USEBGRABITMAP}
  FBackBuffer.Rectangle(FBackBuffer.CanvasBGRA.ClipRect, BGRA(40, 40, 40)); // Some border
  {$ELSE}
  FBackBuffer.Canvas.Pen.Color := RGBToColor(40, 40, 40);
  FBackBuffer.Canvas.Brush.Style := bsClear;
  FBackBuffer.Canvas.Rectangle(FBackBuffer.Canvas.ClipRect); // Some border
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ DrawSubtitleItem }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawSubtitleItem(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);

  procedure _TextOut(const R: TRect; const S, N: String; ALayout: TTextLayout);
  begin
    with ABitmap.{$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF} do
    begin
      Font.Height := 14;
      Font.Style := [fsBold];
      {$IFDEF USEBGRABITMAP}Font.BGRAColor{$ELSE}Font.Color{$ENDIF} := CustomColors.Text;
      FTS.Layout := ALayout;
      if N <> '' then
        DoTextRect(ABitmap, R, R.Left, R.Top, S + sLineBreak + sLineBreak + '🗭', FTS)
      else
        DoTextRect(ABitmap, R, R.Left, R.Top, S, FTS);
    end;
  end;

const
  TextMargin = 4;

var
  i, StartIdx : Integer;
  r : TUWSubtitleItem;
  x1, x2,
  y1, y2 : Integer;
  FullHLines : Boolean;
  CustomDrawRect : TRect;
  ViewEndTime : Integer;
begin
  if FSubtitles.Count = 0 then Exit;

  with ABitmap do
  begin
    FTS.Alignment := taLeftJustify;
    y1  := ATop;
    y2  := ABottom;
    ViewEndTime := FPositionMS + FPageSizeMS;

    // OPTIMIZACIÓN: Búsqueda Binaria del inicio
    // En lugar de empezar en 0, buscamos dónde empieza la vista actual.
    // Usamos FindInsertPos para obtener un indice aproximado.
    StartIdx := FSubtitles.FindInsertPos(FPositionMS, -1);

    // Retrocedemos uno o dos items por seguridad, por si hay un subtítulo
    // que empezó antes de FPositionMS pero termina dentro de la pantalla.
    if StartIdx > 0 then Dec(StartIdx);
    if (StartIdx > 0) and (FSubtitles[StartIdx].FinalTime < FPositionMS) then
       // Si aun retrocediendo estamos atrás, avanzamos para no procesar de más
       Inc(StartIdx);

    if StartIdx < 0 then StartIdx := 0;
    if StartIdx >= FSubtitles.Count then Exit;
    // ---------------------------------------------------

    for i := StartIdx to FSubtitles.Count-1 do
    begin
      r  := FSubtitles[i];

      // OPTIMIZACIÓN: Salida temprana
      // Si el subtítulo empieza después de que termina la vista,
      // y como la lista está ordenada, ya no hay nada más que dibujar.
      if r.InitialTime > ViewEndTime then Break;
      // ----------------------------------------------------

      // Si el subtítulo termina antes de que empiece la vista, continuamos
      // (Esto maneja el caso de solapamientos en el StartIdx)
      if r.FinalTime < FPositionMS then Continue;

      x1 := -1;
      x2 := -1;

      if (r.InitialTime >= FPositionMS) and (r.InitialTime <= ViewEndTime) then
        x1 := TimeToPixel(r.InitialTime - FPositionMS);

      if (r.FinalTime >= FPositionMS) and (r.FinalTime <= ViewEndTime) then
        x2 := TimeToPixel(r.FinalTime - FPositionMS);

      // La comprobación de x1 > Width ya no es estrictamente necesaria gracias al Break de arriba,
      // pero la mantenemos por seguridad visual.
      if x1 > Width then Break;

      FullHLines := (r.InitialTime < FPositionMS) and (r.FinalTime > ViewEndTime);

      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      if FullHLines then
      begin
        x1 := 0;
        x2 := Width-1;
      end;

      if ((x1 <> -1) or (x2 <> -1)) then
      begin
        if (x1 = -1) then x1 := 0
        else if (x2 = -1) then x2 := Width-1;

        // Dibujar Fondo
        {$IFDEF USEBGRABITMAP}
        ABitmap.FillRect(x1, y1, x2, y2, CustomColors.Item, dmDrawWithTransparency);
        {$ELSE}
        ABitmap.Canvas.Brush.Color := CustomColors.Item;
        ABitmap.Canvas.FillRect(x1, y1, x2, y2);
        {$ENDIF}

        // Dibujar Texto (Solo si hay espacio suficiente)
        if ((x2 - x1) > 10) then
        begin
          CustomDrawRect.Top    := y1+TextMargin;
          CustomDrawRect.Left   := x1+TextMargin;
          CustomDrawRect.Right  := x2-TextMargin;
          CustomDrawRect.Bottom := y2-TextMargin;

          if Assigned(FOnCustomDrawSubtitleItem) then
            FOnCustomDrawSubtitleItem(Self, Canvas, i, r, CustomDrawRect)
          else
          begin
            case FSubtitles.ItemPointer[i]^.Align of
              shaCenter : FTS.Alignment := taCenter;
              shaRight  : FTS.Alignment := taRightJustify;
            else
              FTS.Alignment := taLeftJustify;
            end;

            _TextOut(CustomDrawRect, RemoveTSTags(FSubtitles.ItemPointer[i]^.Text), FSubtitles.ItemPointer[i]^.Notes, tlTop);
            FTS.Alignment := taLeftJustify;

            if (CustomDrawRect.Height > (FTimeLineHeight*2)) and (Pos(LineEnding, FSubtitles.ItemPointer[i]^.Text) = 0) then
            begin
              if not FFPSTimeMode then
                _TextOut(CustomDrawRect, '#' + IntToStr(i+1) + '  ' + TimeMSToShortString(FSubtitles.Duration[i], 1), '', tlBottom)
              else
                _TextOut(CustomDrawRect, '#' + IntToStr(i+1) + '  ' + TimeMSToShortStringFrames(FSubtitles.Duration[i], 1, FFPS), '', tlBottom);
            end;
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ DrawThumbnails }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawThumbnails( const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
var
  i, j, idx, h, w, x1, x2, lo, hi, mid, ViewEnd: Integer;
  ar: Double;
  Src: TBitmap;
begin
  // one thumbnail for each scene change (the first frame of the shot), from
  // its position to the next scene change; if it doesn't fit, it's cropped,
  // and if there is almost no room, it's skipped (like in video editors)
  h := ABottom - ATop;
  if (h <= 0) or (Length(FSceneChangeList) = 0) then Exit;

  // first scene change on screen, or the one before it (its shot is visible)
  lo := 0;
  hi := Length(FSceneChangeList);
  while lo < hi do
  begin
    mid := (lo + hi) div 2;
    if FSceneChangeList[mid] <= FPositionMS then
      lo := mid + 1
    else
      hi := mid;
  end;
  i := Max(lo - 1, 0);

  ViewEnd := FPositionMS + FPageSizeMS;
  while (i <= High(FSceneChangeList)) and (FSceneChangeList[i] < ViewEnd) do
  begin
    // the next different scene change ends this shot
    j := i + 1;
    while (j <= High(FSceneChangeList)) and (FSceneChangeList[j] = FSceneChangeList[i]) do
      Inc(j);

    x1 := TimeToPixel(FSceneChangeList[i] - FPositionMS);
    if j <= High(FSceneChangeList) then
      x2 := TimeToPixel(FSceneChangeList[j] - FPositionMS)
    else
      x2 := Max(TimeToPixel(FLengthMS - FPositionMS), Width);

    // generated thumbnail of this scene change, or the default one
    idx := BinarySearch_IntArray(FThumbTimes, FSceneChangeList[i]);
    if (idx > High(FThumbTimes)) or (idx > High(FThumbnails)) or (FThumbTimes[idx] <> FSceneChangeList[i]) or
      (FThumbnails[idx] = NIL) or (FThumbnails[idx].Width = 0) or (FThumbnails[idx].Height = 0) then
      idx := -1;

    if idx >= 0 then
      Src := FThumbnails[idx]
    else
      Src := FDefaultThumbnail;

    if (Src.Width > 0) and (Src.Height > 0) then
    begin
      if FStretchThumbnails then
      begin
        if x2 > x1 then
          PutThumbnail(ABitmap, Rect(x1, ATop, x2, ABottom), idx);
      end
      else
      begin
        ar := Src.Width / Src.Height;
        w  := Max(Round(h * ar), 1);
        if (x2 - x1) >= w then
          PutThumbnail(ABitmap, Rect(x1, ATop, x1 + w, ABottom), idx)
        else if (x2 - x1) >= (w div 3) then
          PutThumbnail(ABitmap, Rect(x1, ATop, x1 + w, ABottom), idx, x2 - x1);
      end;
    end;

    i := j;
  end;
end;

//------------------------------------------------------------------------------

{ DrawSelection }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawSelection(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  x1, x2 : Integer;
begin
  // Selection
  if (FDynamicSelection.FinalTime > 0) then
    with ABitmap do
    begin
      //Brush.Style := bsSolid;
      //Pen.Style   := psDot;
      //Pen.Color   := clWhite;
      //Pen.Width   := 1;

      x1 := TimeToPixel(FDynamicSelection.InitialTime - FPositionMS);
      x2 := TimeToPixel(FDynamicSelection.FinalTime - FPositionMS);

      if (x1 = x2) then
      begin
        // points are on each other and in the display Subtitle
        if (FDynamicSelection.InitialTime >= FPositionMS) and
           (FDynamicSelection.InitialTime <= FPositionMS + FPageSizeMS) then
        begin
          {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x1, GetSubCanvasY, x1, GetSubCanvasY+GetSubCanvasHeight);
        end;
      end
      else
      begin
        Constrain(x1, 0, Width);
        Constrain(x2, 0, Width);
        if (x1 <> x2) then
        begin
          if FSelectedSubtitle = NIL then
            {$IFDEF USEBGRABITMAP}
            ABitmap.FillRect(x1, GetWAVECanvasY, x2, GetWAVECanvasY+GetSubCanvasHeight, CustomColors.ItemSelected, dmDrawWithTransparency, 3070)
            {$ELSE}
            DrawAlphaRect(ABitmap, x1, x2, GetWAVECanvasY, GetWAVECanvasY+GetSubCanvasHeight, CustomColors.ItemSelected)
            {$ENDIF}
          else
            {$IFDEF USEBGRABITMAP}
            ABitmap.FillRect(x1, GetSubCanvasY, x2, GetSubCanvasY+GetSubCanvasHeight, CustomColors.ItemSelected, dmDrawWithTransparency, 3070);
            {$ELSE}
            DrawAlphaRect(ABitmap, x1, x2, GetSubCanvasY, GetSubCanvasY+GetSubCanvasHeight, CustomColors.ItemSelected);
            {$ENDIF}
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------

{ DrawCursor }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawCursor(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  x: Integer;
begin
  // Cursor
  if (FCursorMS >= FPositionMS) and (FCursorMS <= FPositionMS + FPageSizeMS) then
    with ABitmap do
    begin
      //Canvas.Pen.Style := psSolid;
      //Canvas.Pen.Mode  := pmCopy;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Color := CustomColors.Cursor;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      x := TimeToPixel(FCursorMS - FPositionMS);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetCanvasHeight);
    end;
end;

//------------------------------------------------------------------------------

{ DrawPlayCursor }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawPlayCursor(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  x: Integer;
begin
  // Play Cursor
  if (FPlayCursorMS >= FPositionMS) and (FPlayCursorMS <= FPositionMS + FPageSizeMS) then
    with ABitmap do
    begin
      //Canvas.Pen.Style := psSolid; //psDot;
      //Canvas.Pen.Mode  := pmCopy;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Style := psDot;
      {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.PlayCursor;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      x := TimeToPixel(FPlayCursorMS - FPositionMS);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetCanvasHeight);
    end;
end;

//------------------------------------------------------------------------------

{ DrawGridLines }

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.GetMinorGridColor: {$IFDEF USEBGRABITMAP}TBGRAPixel{$ELSE}TColor{$ENDIF};
{$IFNDEF USEBGRABITMAP}
var
  g, b: TColor;
{$ENDIF}
begin
  // halfway between the grid line and the background colors
  {$IFDEF USEBGRABITMAP}
  Result := MergeBGRA(CustomColors.GridLine, CustomColors.Background);
  {$ELSE}
  g := ColorToRGB(CustomColors.GridLine);
  b := ColorToRGB(CustomColors.Background);
  Result := RGBToColor((Red(g) + Red(b)) div 2, (Green(g) + Green(b)) div 2, (Blue(g) + Blue(b)) div 2);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawGridLines(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  i, p, x, x1: Integer;
  StepFrames, SubFrames, k : Integer;
  f : Int64;
begin
  with ABitmap do
  begin
    //Canvas.Pen.Style := psSolid;
    //Canvas.Pen.Mode  := pmCopy;
    {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.GridLine;
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

    if not IsTimeLineEnabled then
    begin
      i := 0;
      while i < {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Width do
      begin
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(i, 0, i, GetCanvasHeight);
        Inc(i, 30);
      end;
    end
    else if FFPSTimeMode then
    begin
      // Same steps as the frames mode time line
      GetFrameRulerStep(StepFrames, SubFrames);

      // A softer line for every frame when there is room
      if GetFramePixels >= MinFramePixels then
      begin
        {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := GetMinorGridColor;
        f := RulerFirstFrame(1);
        p := RulerFrameToMs(f);
        while (p < FPositionMS + FPageSizeMS) do
        begin
          if (f mod StepFrames <> 0) then
          begin
            x := TimeToPixel(p - FPositionMS);
            {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetCanvasHeight);
          end;
          Inc(f);
          p := RulerFrameToMs(f);
        end;
        {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.GridLine;
      end;

      f := RulerFirstFrame(StepFrames);
      p := RulerFrameToMs(f);
      while (p < FPositionMS + FPageSizeMS) do
      begin
        x := TimeToPixel(p - FPositionMS);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetCanvasHeight);

        if (SubFrames > 0) and (StepFrames div SubFrames = 2) then
        begin
          x1 := TimeToPixel(RulerFrameToMs(f + SubFrames) - FPositionMS);
          {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x1, 0, x1, GetCanvasHeight);
        end;

        f := f + StepFrames;
        p := RulerFrameToMs(f);
      end;
    end
    else
    begin
      p := (FPositionMS div FStepMs * FStepMs);
      while (p < FPositionMS + FPageSizeMS) do
      begin
        // Draw main division
        x := TimeToPixel(p - FPositionMS);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetCanvasHeight);

        x1 := x + TimeToPixel(FStepMs div 2);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x1, 0, x1, GetCanvasHeight);

        p := p + FStepMs;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ DrawSceneChange }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawSceneChange(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
var
  x, i, SceneChange : Integer;
  lo, hi, mid       : Integer;
begin
  if FSceneChangeEnabled then
    with ABitmap do
    begin
      //CanvasBGRA.Brush.Style := bsSolid;
      //CanvasBGRA.Pen.Style   := psSolid;
      {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.SceneChange;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      // the list is sorted: binary search the first visible scene change and
      // stop at the first one past the view, instead of walking all of them
      lo := 0;
      hi := Length(FSceneChangeList);
      while lo < hi do
      begin
        mid := (lo + hi) div 2;
        if FSceneChangeList[mid] < FPositionMS then
          lo := mid + 1
        else
          hi := mid;
      end;

      for i := lo to High(FSceneChangeList) do
      begin
        SceneChange := FSceneChangeList[i];
        if SceneChange > FPositionMs + FPageSizeMS then Break;
        if (SceneChange >= FPositionMS) then
        begin
          x := TimeToPixel(SceneChange - FPositionMS);
          {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, ATop, x, ABottom);
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------

{ DrawMinimumBlank }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DrawMinimumBlank(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const ATop, ABottom: Integer);
begin
  if FDrawGAP then
    with ABitmap do
    begin
      //CanvasBGRA.Brush.Style := bsSolid;
      //CanvasBGRA.Pen.Style   := psSolid;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      if (FMinBlankInfo1.Exists) then
        DrawAlphaRectByTimes(ABitmap, FMinBlankInfo1.GetStart(FGAP),
          FMinBlankInfo1.GetStop(FGAP), ATop, ABottom, clWhite);

      if (FMinBlankInfo2.Exists) then
        DrawAlphaRectByTimes(ABitmap, FMinBlankInfo2.GetStart(FGAP),
          FMinBlankInfo2.GetStop(FGAP), ATop, ABottom, clWhite);
    end;
end;

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------

{ Text cache }

//------------------------------------------------------------------------------

{$IFDEF USEBGRABITMAP}
type

  { TWaveCachedText }

  // the opaque part of a rendered text and its offset in the text rect
  TWaveCachedText = class
    Bitmap : TBGRABitmap;
    X, Y   : Integer;
    destructor Destroy; override;
  end;

destructor TWaveCachedText.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

const
  // the cache is emptied when it grows over this (~16 MB) or these entries
  MaxTextCachePixels = 4000000;
  MaxTextCacheCount  = 2000;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ClearTextCache;
begin
  FTextCache.Clear;
  FTextCachePixels := 0;
end;

//------------------------------------------------------------------------------

// Each BGRABitmap TextOut/TextRect costs ~1 ms (the LCL draws the text in a
// temporary bitmap every time), and the time line and the subtitles draw the
// same texts again on every scroll step. So every text is rendered once in a
// transparent bitmap (with the same font, colors, size and text style), and
// then only put with transparency at its position.
procedure TUWWaveformDisplayer.DrawCachedText(const ABitmap: TBGRABitmap; const AIsRect: Boolean; const R: TRect; const X, Y: Integer; const S: String; const ATS: TTextStyle);
var
  Key  : String;
  Idx  : Integer;
  Item : TWaveCachedText;
  Tmp  : TBGRABitmap;
  W, H : Integer;
  B    : TRect;
  Sz   : TSize;
begin
  if S = '' then Exit;

  with ABitmap.CanvasBGRA.Font do
    if AIsRect then
      Key := Format('R%d|%d|%d|%d|%d|%d|%s|%d|%d|%d|%d|%x|%d|%s',
        [R.Width, R.Height, X - R.Left, Y - R.Top, Ord(ATS.Alignment), Ord(ATS.Layout),
         Name, Height, Ord(fsBold in Style) + Ord(fsItalic in Style)*2 + Ord(fsUnderline in Style)*4 + Ord(fsStrikeOut in Style)*8, Ord(Quality), Ord(ABitmap.FontQuality), DWord(BGRAColor),
         Ord(ATS.SingleLine) + Ord(ATS.Wordbreak)*2 + Ord(ATS.Clipping)*4, S])
    else
      Key := Format('T%s|%d|%d|%d|%d|%x|%s',
        [Name, Height, Ord(fsBold in Style) + Ord(fsItalic in Style)*2 + Ord(fsUnderline in Style)*4 + Ord(fsStrikeOut in Style)*8, Ord(Quality), Ord(ABitmap.FontQuality), DWord(BGRAColor), S]);

  if FTextCache.Find(Key, Idx) then
    Item := TWaveCachedText(FTextCache.Objects[Idx])
  else
  begin
    if AIsRect then
    begin
      W := R.Width;
      H := R.Height;
    end
    else
    begin
      Sz := ABitmap.CanvasBGRA.TextExtent(S);
      W  := Sz.cx + 2;
      H  := Sz.cy;
    end;
    if (W <= 0) or (H <= 0) then Exit;

    if (FTextCachePixels + (W * H) > MaxTextCachePixels) or (FTextCache.Count >= MaxTextCacheCount) then
      ClearTextCache;

    Item := TWaveCachedText.Create;
    Tmp  := TBGRABitmap.Create(W, H, BGRAPixelTransparent);
    try
      Tmp.FontQuality   := ABitmap.FontQuality;
      Tmp.FontAntialias := ABitmap.FontAntialias;
      Tmp.CanvasBGRA.Font.Assign(ABitmap.CanvasBGRA.Font);
      Tmp.CanvasBGRA.Brush.Style := bsClear;
      if AIsRect then
        Tmp.CanvasBGRA.TextRect(Rect(0, 0, W, H), X - R.Left, Y - R.Top, S, ATS)
      else
        Tmp.CanvasBGRA.TextOut(0, 0, S);

      // keep only the part with text
      B := Tmp.GetImageBounds(cAlpha);
      if not B.IsEmpty then
      begin
        Item.Bitmap := TBGRABitmap(Tmp.GetPart(B));
        Item.X      := B.Left;
        Item.Y      := B.Top;
        Inc(FTextCachePixels, B.Width * B.Height);
      end;
    finally
      Tmp.Free;
    end;
    FTextCache.AddObject(Key, Item);
  end;

  if Assigned(Item.Bitmap) then
    if AIsRect then
      ABitmap.PutImage(R.Left + Item.X, R.Top + Item.Y, Item.Bitmap, dmDrawWithTransparency)
    else
      ABitmap.PutImage(X + Item.X, Y + Item.Y, Item.Bitmap, dmDrawWithTransparency);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DoTextOut(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const X, Y: Integer; const S: String);
{$IFDEF USEBGRABITMAP}
var
  TS: TTextStyle;
{$ENDIF}
begin
  {$IFDEF USEBGRABITMAP}
  FillByte(TS, SizeOf(TS), 0);
  DrawCachedText(ABitmap, False, Rect(0, 0, 0, 0), X, Y, S, TS);
  {$ELSE}
  ABitmap.Canvas.TextOut(X, Y, S);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DoTextRect(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const R: TRect; const X, Y: Integer; const S: String; const ATS: TTextStyle);
begin
  {$IFDEF USEBGRABITMAP}
  DrawCachedText(ABitmap, True, R, X, Y, S, ATS);
  {$ELSE}
  ABitmap.Canvas.TextRect(R, X, Y, S, ATS);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ Thumbnails cache }

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ClearThumbCache(const AOnlyDefault: Boolean = False);
{$IFDEF USEBGRABITMAP}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF USEBGRABITMAP}
  FreeAndNil(FDefaultThumbCache);
  if AOnlyDefault then Exit;

  for i := 0 to High(FThumbCache) do
    FThumbCache[i].Free;

  FThumbCache := NIL;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.SetThumbnails(const ATimes: TIntegerDynArray; const ABitmaps: TThumbnails);
begin
  // new/freed bitmaps: the converted ones are not valid anymore
  ClearThumbCache;
  FThumbTimes := ATimes;
  FThumbnails := ABitmaps;
  if ThumbnailsVisible then
    UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.ThumbnailChanged(const AIndex: Integer);
var
  t, NextT: Integer;
begin
  if (AIndex < 0) or (AIndex > High(FThumbTimes)) then Exit;
  {$IFDEF USEBGRABITMAP}
  if AIndex <= High(FThumbCache) then
    FreeAndNil(FThumbCache[AIndex]);
  {$ENDIF}

  // redraw only if its shot is on screen
  if not ThumbnailsVisible then Exit;
  t := FThumbTimes[AIndex];
  if AIndex < High(FThumbTimes) then
    NextT := FThumbTimes[AIndex+1]
  else
    NextT := MaxInt;

  if (t < FPositionMS + FPageSizeMS) and (NextT > FPositionMS) then
    UpdateView([uvfPageSize]);
end;

//------------------------------------------------------------------------------

function TUWWaveformDisplayer.ThumbnailsVisible: Boolean;
begin
  Result := FDrawThumbnails and (Length(FSceneChangeList) > 0);
end;

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.DefaultThumbnailChange(Sender: TObject);
begin
  ClearThumbCache(True);
end;

//------------------------------------------------------------------------------

{$IFDEF USEBGRABITMAP}
// AIndex = -1: default thumbnail
function TUWWaveformDisplayer.GetThumbBGRA(const AIndex, AWidth, AHeight: Integer): TBGRABitmap;
var
  Src : TBitmap;
  Tmp : TBGRABitmap;
begin
  Result := NIL;
  if (AWidth <= 0) or (AHeight <= 0) then Exit;

  if AIndex < 0 then
  begin
    Src    := FDefaultThumbnail;
    Result := FDefaultThumbCache;
  end
  else
  begin
    Src := FThumbnails[AIndex];
    if Length(FThumbCache) < Length(FThumbnails) then
      SetLength(FThumbCache, Length(FThumbnails)); // new ones are NIL
    Result := FThumbCache[AIndex];
  end;

  if (Src = NIL) or (Src.Width = 0) or (Src.Height = 0) then Exit(NIL);

  if (Result = NIL) or (Result.Width <> AWidth) or (Result.Height <> AHeight) then
  begin
    FreeAndNil(Result);
    // converted and resampled once for this size (StretchDraw on the
    // BGRABitmap Canvas synchronized the whole back buffer every time)
    Tmp := TBGRABitmap.Create(Src);
    try
      Result := TBGRABitmap(Tmp.Resample(AWidth, AHeight, rmFineResample));
    finally
      Tmp.Free;
    end;

    if AIndex < 0 then
      FDefaultThumbCache := Result
    else
      FThumbCache[AIndex] := Result;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TUWWaveformDisplayer.PutThumbnail(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const R: TRect; const AIndex: Integer; const AVisibleWidth: Integer = -1);
// AVisibleWidth >= 0: only the left part of the thumbnail is drawn (cropped)
var
  {$IFDEF USEBGRABITMAP}
  Bmp: TBGRABitmap;
  {$ELSE}
  OldClip: TRect;
  OldClipping: Boolean;
  {$ENDIF}
begin
  {$IFDEF USEBGRABITMAP}
  Bmp := GetThumbBGRA(AIndex, R.Width, R.Height);
  if Assigned(Bmp) then
  begin
    if (AVisibleWidth >= 0) and (AVisibleWidth < R.Width) then
      ABitmap.PutImagePart(R.Left, R.Top, Bmp, Rect(0, 0, AVisibleWidth, R.Height), dmSet)
    else
      ABitmap.PutImage(R.Left, R.Top, Bmp, dmSet);
  end;
  {$ELSE}
  OldClipping := ABitmap.Canvas.Clipping;
  OldClip     := ABitmap.Canvas.ClipRect;
  if (AVisibleWidth >= 0) and (AVisibleWidth < R.Width) then
  begin
    ABitmap.Canvas.ClipRect := Rect(R.Left, R.Top, R.Left + AVisibleWidth, R.Bottom);
    ABitmap.Canvas.Clipping := True;
  end;
  try
    if AIndex < 0 then
      ABitmap.Canvas.StretchDraw(R, FDefaultThumbnail)
    else
      ABitmap.Canvas.StretchDraw(R, FThumbnails[AIndex]);
  finally
    ABitmap.Canvas.ClipRect := OldClip;
    ABitmap.Canvas.Clipping := OldClipping;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

