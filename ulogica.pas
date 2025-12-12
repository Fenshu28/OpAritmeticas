unit uLogica;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, ExtCtrls, LCLType, FPImage;

type
  // Estructura de un píxel lógico RGB
  TRGBPixel = record
    R, G, B: Byte;
  end;

  // Matriz 2D de píxeles
  TMatrizPixeles = array of array of TRGBPixel;

  // Nombre del tipo array para poder usarlo en propiedades
  TBancoMemorias = array[0..2] of TMatrizPixeles;

  { Clase para gestionar la lógica de las imágenes }
  TGestorImagenes = class
  private
    FMemorias: TBancoMemorias;
    FAncho, FAlto: Integer;

    procedure RedimensionarMatriz(Index, W, H: Integer);
  public
    constructor Create;

    // Carga robusta: Acepta cualquier formato y lo convierte a RGB
    procedure CargarImagen(Index: Integer; RutaArchivo: String; Destino: TImage);
    procedure ActualizarTImage(Index: Integer; Destino: TImage);

    property Ancho: Integer read FAncho;
    property Alto: Integer read FAlto;
    property Memorias: TBancoMemorias read FMemorias;
  end;

implementation

{ TGestorImagenes }

constructor TGestorImagenes.Create;
begin
  FAncho := 0;
  FAlto := 0;
end;

procedure TGestorImagenes.RedimensionarMatriz(Index, W, H: Integer);
begin
  SetLength(FMemorias[Index], W, H);
end;

procedure TGestorImagenes.CargarImagen(Index: Integer; RutaArchivo: String; Destino: TImage);
var
  PicTemp: TPicture;
  BmpTemp: TBitmap;
  x, y: Integer;
  ColorTemp: TColor;
begin
  if not FileExists(RutaArchivo) then Exit;

  PicTemp := TPicture.Create;
  BmpTemp := TBitmap.Create;
  try
    try
      // 1. Cargamos la imagen original (PNG, JPG, etc.)
      PicTemp.LoadFromFile(RutaArchivo);

      // 2. TÉCNICA DE APLANADO (Solución al error de imagen negra)
      // En lugar de Assign, preparamos un Bitmap blanco y "pintamos" la imagen encima.
      BmpTemp.SetSize(PicTemp.Width, PicTemp.Height);
      BmpTemp.PixelFormat := pf24bit; // Forzamos RGB estándar

      // Llenamos de blanco primero (para que las transparencias no sean negras)
      BmpTemp.Canvas.Brush.Color := clWhite;
      BmpTemp.Canvas.FillRect(0, 0, PicTemp.Width, PicTemp.Height);

      // Dibujamos la imagen cargada sobre el canvas preparado
      BmpTemp.Canvas.Draw(0, 0, PicTemp.Graphic);

    except
      ShowMessage('Error al procesar la imagen.');
      Exit;
    end;

    // Si es la primera imagen (Index 0) o no hay dimensiones definidas
    if (Index = 0) or ((FAncho = 0) and (FAlto = 0)) then
    begin
      FAncho := BmpTemp.Width;
      FAlto := BmpTemp.Height;
    end;

    RedimensionarMatriz(Index, FAncho, FAlto);

    // Guardamos en la matriz lógica
    for x := 0 to FAncho - 1 do
    begin
      for y := 0 to FAlto - 1 do
      begin
        // Protección de límites
        if (x < BmpTemp.Width) and (y < BmpTemp.Height) then
          ColorTemp := BmpTemp.Canvas.Pixels[x, y]
        else
          ColorTemp := clBlack; // Relleno negro si la imagen es más pequeña que el lienzo

        FMemorias[Index][x, y].R := Red(ColorTemp);
        FMemorias[Index][x, y].G := Green(ColorTemp);
        FMemorias[Index][x, y].B := Blue(ColorTemp);
      end;
    end;

    // Actualizamos la vista
    ActualizarTImage(Index, Destino);

  finally
    PicTemp.Free;
    BmpTemp.Free;
  end;
end;

procedure TGestorImagenes.ActualizarTImage(Index: Integer; Destino: TImage);
var
  Bmp: TBitmap;
  x, y: Integer;
begin
  // Si no hay datos, no hacemos nada
  if (FAncho = 0) or (FAlto = 0) then Exit;

  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit; // Mantenemos coherencia
    Bmp.SetSize(FAncho, FAlto);

    for x := 0 to FAncho - 1 do
    begin
      for y := 0 to FAlto - 1 do
      begin
        Bmp.Canvas.Pixels[x, y] := RGBToColor(
          FMemorias[Index][x, y].R,
          FMemorias[Index][x, y].G,
          FMemorias[Index][x, y].B
        );
      end;
    end;

    Destino.Picture.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

end.
