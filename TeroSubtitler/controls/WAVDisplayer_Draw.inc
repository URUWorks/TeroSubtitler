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

procedure TUWWaveformDisplayer.DrawWave(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF}; const TryOptimize: Boolean = False);
var
  x, y1, y2            : Integer;
  x1_update, x2_update,
  x_optim              : Integer;
  x_scaled, Next_x_scaled : Integer;
  PeaksPerPixelScaled  : Double;
  StartPositionInPeaks : Double;
  CurrentPeakPos       : Double;
  Middle               : Integer;
  PeakMax, PeakMin     : Integer;
  i                    : Integer;
  RectHeight           : Integer;
  WavePoints           : array of TPoint;
  PointIdx, TotalPoints: Integer;
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

  x1_update := 0;
  x2_update := Width;

  if TryOptimize and (FOldPageSizeMs = FPageSizeMs) then
  begin
    // Calculate intersection between old and new view
    if (FPositionMs > FOldPositionMs) then
    begin
      x_optim   := TimeToPixel(FPositionMs - FOldPositionMs);
      x2_update := Width;
      x1_update := x2_update - x_optim;
      Constrain(x1_update, 0, Width);
      if (x1_update <> 0) then
        {$IFDEF USEBGRABITMAP}
        ABitmap.CanvasBGRA.CopyRect(Rect(0, 0, x1_update, Height), ABitmap, Rect(x_optim, 0, x1_update, Height));
        {$ELSE}
        ABitmap.Canvas.CopyRect(Rect(0, 0, x1_update, Height), ABitmap.Canvas, Rect(x_optim, 0, x1_update, Height));
        {$ENDIF}
    end
    else
    begin
      x_optim   := TimeToPixel(FOldPositionMs - FPositionMs);
      x1_update := 0;
      x2_update := x_optim;
      Constrain(x2_update, 0, Width);

      if (x2_update <> Width) then
        {$IFDEF USEBGRABITMAP}
        ABitmap.CanvasBGRA.CopyRect(Rect(x_optim, 0, Width-x_optim, Height), ABitmap, Rect(0, 0, Width-x_optim, Height));
        {$ELSE}
        ABitmap.Canvas.CopyRect(Rect(x_optim, 0, Width-x_optim, Height), ABitmap.Canvas, Rect(0, 0, Width-x_optim, Height));
        {$ENDIF}
    end;
  end;

  if x1_update >= x2_update then Exit;

  RectHeight := GetSubCanvasHeight;
  Middle     := GetWAVECanvasY + (RectHeight div 2);

  // Pre-calcular factor de escala Y
  ScaleFactorY := (FVerticalScaling * RectHeight) / (100.0 * 65536.0);

  // Preparar array de puntos para Polygon
  // Necesitamos (Ancho * 2) puntos: ida por arriba, vuelta por abajo
  TotalPoints := (x2_update - x1_update);
  SetLength(WavePoints, (TotalPoints * 2));

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

    // Guardar puntos en el array
    // Parte superior (indice 0 hacia adelante)
    WavePoints[PointIdx] := Point(x, Middle - y1);
    // Parte inferior (indice Final hacia atrás)
    WavePoints[Length(WavePoints) - 1 - PointIdx] := Point(x, Middle - y2);

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

    Polygon(WavePoints);

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
    FTimeLineHeight := {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.TextHeight('0');

    PosRect        := ClientRect;
    PosRect.Bottom := PosRect.Bottom - FScrollBar.Height;
    PosRect.Top    := PosRect.Bottom - FTimeLineHeight;

    // Draw background
    {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Brush.Style := bsClear;
    oldx := -1;
    p := (FPositionMS div FStepMs * FStepMs);
    while (p < FPositionMS + FPageSizeMS) do
    begin
      // Draw main division
      x := TimeToPixel(p - FPositionMS);
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, PosRect.Top +2, x, PosRect.Bottom);
      if not FFPSTimeMode then
        PosString := TimeMSToShortString(p + FOffsetMs, FStepLog)
      else
        PosString := TimeMSToShortStringFrames(p + FOffsetMs, 1, FFPS);
      // Calculate text coordinate
      x1 := x + 3; //x - (CanvasBGRA.TextWidth(PosString) div 2);
      // Draw text
      {$IFDEF USEBGRABITMAP}CanvasBGRA.Font.BGRAColor{$ELSE}Canvas.Font.Color{$ENDIF} := CustomColors.Text;
      if oldx <> x1 then
      begin
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.TextOut(x1, PosRect.Top, PosString);
        oldx := x1;
      end;
      // Draw subdivision
      if not FFPSTimeMode then
      begin
        x2 := x + TimeToPixel(FStepMs div 2);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x2, PosRect.Bottom -4, x2, PosRect.Bottom);
      end;
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
        TextRect(R, R.Left, R.Top, S + sLineBreak + sLineBreak + '🗭', FTS)
      else
        TextRect(R, R.Left, R.Top, S, FTS);
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
        Canvas.FillRect(x1, y1, x2, y2);
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
            case Subtitles[i].Align of
              shaCenter : FTS.Alignment := taCenter;
              shaRight  : FTS.Alignment := taRightJustify;
            else
              FTS.Alignment := taLeftJustify;
            end;

            _TextOut(CustomDrawRect, RemoveTSTags(Subtitles[i].Text), Subtitles[i].Notes, tlTop);
            FTS.Alignment := taLeftJustify;

            if (CustomDrawRect.Height > (FTimeLineHeight*2)) and (Pos(LineEnding, Subtitles[i].Text) = 0) then
            begin
              if not FFPSTimeMode then
                _TextOut(CustomDrawRect, '#' + IntToStr(i+1) + '  ' + TimeMSToShortString(Subtitles.Duration[i], 1), '', tlBottom)
              else
                _TextOut(CustomDrawRect, '#' + IntToStr(i+1) + '  ' + TimeMSToShortStringFrames(Subtitles.Duration[i], 1, FFPS), '', tlBottom);
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
  i, h, x1, x2, w: Integer;
  tStart, tEnd, ar: Double;
  r: TRect;
  ready: Boolean;
begin
  ready := Assigned(FThumbnails) and (Length(FThumbnails) = DefaultThumbnailsCount);
  if not ready and (FDefaultThumbnail.Handle = 0) then Exit;

  h := ABottom - ATop;
  if h <= 0 then Exit;

  // Calcular aspecto (solo si no estiramos)
  if not FStretchThumbnails then
  begin
    if ready then
      ar := FThumbnails[0].Width / FThumbnails[0].Height
    else
      ar := FDefaultThumbnail.Width / FDefaultThumbnail.Height;

    w := Round(h * ar);
    if w <= 0 then w := 1;

    // Dibujar repitiendo miniaturas
    x1 := 0;
    while x1 < Width do
    begin
      tStart := (FPositionMS + (x1 * FPageSizeMs / Width)); // tiempo en ms en esta posición

      i := Trunc((tStart * DefaultThumbnailsCount) / FLengthMS);
      Constrain(i, 0, DefaultThumbnailsCount - 1);

      r := Rect(x1, ATop, x1 + w, ABottom);

      if ready and (FThumbnails[i].Width > 0) then
        ABitmap.Canvas.StretchDraw(r, FThumbnails[i])
      else
        ABitmap.Canvas.StretchDraw(r, FDefaultThumbnail);

      x1 += w;
    end;
  end
  else
  begin
    // Dibujar estirando miniaturas
    for i := 0 to DefaultThumbnailsCount - 1 do
    begin
      tStart := (i     * FLengthMS) / DefaultThumbnailsCount;
      tEnd   := ((i+1) * FLengthMS) / DefaultThumbnailsCount;

      if (tEnd < FPositionMS) or (tStart > FPositionMS + FPageSizeMs) then
        Continue;

      x1 := Round((tStart - FPositionMS) * Width / FPageSizeMs);
      x2 := Round((tEnd   - FPositionMS) * Width / FPageSizeMs);
      if x2 <= x1 then x2 := x1 + 1;

      r := Rect(x1, ATop, x2, ABottom);

      if ready and (FThumbnails[i].Width > 0) then
        ABitmap.Canvas.StretchDraw(r, FThumbnails[i])
      else
        ABitmap.Canvas.StretchDraw(r, FDefaultThumbnail);
    end;
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

procedure TUWWaveformDisplayer.DrawGridLines(const ABitmap: {$IFDEF USEBGRABITMAP}TBGRABitmap{$ELSE}TBitmap{$ENDIF});
var
  i, p, x, x1: Integer;
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
    else
    begin
      p := (FPositionMS div FStepMs * FStepMs);
      while (p < FPositionMS + FPageSizeMS) do
      begin
        // Draw main division
        x := TimeToPixel(p - FPositionMS);
        {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x, 0, x, GetCanvasHeight);

        if not FFPSTimeMode then
        begin
          x1 := x + TimeToPixel(FStepMs div 2);
          {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Line(x1, 0, x1, GetCanvasHeight);
        end;

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
begin
  if FSceneChangeEnabled then
    with ABitmap do
    begin
      //CanvasBGRA.Brush.Style := bsSolid;
      //CanvasBGRA.Pen.Style   := psSolid;
      {$IFDEF USEBGRABITMAP}CanvasBGRA.Pen.BGRAColor{$ELSE}Canvas.Pen.Color{$ENDIF} := CustomColors.SceneChange;
      {$IFDEF USEBGRABITMAP}CanvasBGRA{$ELSE}Canvas{$ENDIF}.Pen.Width := 1;

      for i := Low(FSceneChangeList) to High(FSceneChangeList) do
      begin
        SceneChange := FSceneChangeList[i];
        if (SceneChange >= FPositionMS) and (SceneChange <= FPositionMs + FPageSizeMS) then
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

