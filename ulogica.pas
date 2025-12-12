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
    procedure SumaImagenes(Destino: TImage);
    procedure SumaImagenes2(Destino: TImage);
    procedure RestaImagenes1(Destino: TImage);
    procedure RestaImagenes2(Destino: TImage);
    procedure RestaImagenes3(Destino: TImage);
    
    // Reflexiones
    procedure ReflexionHorizontal(Index: Integer; Destino: TImage);
    procedure ReflexionVertical(Index: Integer; Destino: TImage);
    procedure ReflexionDoble(Index: Integer; Destino: TImage);

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

procedure TGestorImagenes.SumaImagenes(Destino: TImage);
var
  x, y: Integer;
begin
  // Verificamos que existan dimensiones base
  if (FAncho = 0) or (FAlto = 0) then Exit;

  // Redimensionamos la memoria de resultado (Index 2)
  RedimensionarMatriz(2, FAncho, FAlto);

  for x := 0 to FAncho - 1 do
  begin
    for y := 0 to FAlto - 1 do
    begin
       // Suma con normalización (promedio)
       FMemorias[2][x, y].R := (Integer(FMemorias[0][x, y].R) + Integer(FMemorias[1][x, y].R)) div 2;
       FMemorias[2][x, y].G := (Integer(FMemorias[0][x, y].G) + Integer(FMemorias[1][x, y].G)) div 2;
       FMemorias[2][x, y].B := (Integer(FMemorias[0][x, y].B) + Integer(FMemorias[1][x, y].B)) div 2;
    end;
  end;

  ActualizarTImage(2, Destino);
end;

procedure TGestorImagenes.SumaImagenes2(Destino: TImage);
var
  x, y: Integer;
  R, G, B: Integer;
begin
  if (FAncho = 0) or (FAlto = 0) then Exit;

  RedimensionarMatriz(2, FAncho, FAlto);

  for x := 0 to FAncho - 1 do
  begin
    for y := 0 to FAlto - 1 do
    begin
       // Suma 2: FARS (Acotación rango superior)
       // Si x + y < 255 -> x + y
       // Si x + y >= 255 -> 255
       
       R := Integer(FMemorias[0][x, y].R) + Integer(FMemorias[1][x, y].R);
       if R > 255 then R := 255;
       
       G := Integer(FMemorias[0][x, y].G) + Integer(FMemorias[1][x, y].G);
       if G > 255 then G := 255;
       
       B := Integer(FMemorias[0][x, y].B) + Integer(FMemorias[1][x, y].B);
       if B > 255 then B := 255;

       FMemorias[2][x, y].R := Byte(R);
       FMemorias[2][x, y].G := Byte(G);
       FMemorias[2][x, y].B := Byte(B);
    end;
  end;

  ActualizarTImage(2, Destino);
end;


procedure TGestorImagenes.RestaImagenes1(Destino: TImage);
var
  x, y: Integer;
  R, G, B: Integer;
begin
  if (FAncho = 0) or (FAlto = 0) then Exit;

  RedimensionarMatriz(2, FAncho, FAlto);

  for x := 0 to FAncho - 1 do
  begin
    for y := 0 to FAlto - 1 do
    begin
       // Resta 1: Resta a cero (Rango [0, 255])
       // Z = x - y si x >= y, sino 0
       
       R := Integer(FMemorias[0][x, y].R) - Integer(FMemorias[1][x, y].R);
       if R < 0 then R := 0;
       
       G := Integer(FMemorias[0][x, y].G) - Integer(FMemorias[1][x, y].G);
       if G < 0 then G := 0;
       
       B := Integer(FMemorias[0][x, y].B) - Integer(FMemorias[1][x, y].B);
       if B < 0 then B := 0;

       FMemorias[2][x, y].R := Byte(R);
       FMemorias[2][x, y].G := Byte(G);
       FMemorias[2][x, y].B := Byte(B);
    end;
  end;

  ActualizarTImage(2, Destino);
end;

procedure TGestorImagenes.RestaImagenes2(Destino: TImage);
var
  x, y: Integer;
begin
  if (FAncho = 0) or (FAlto = 0) then Exit;

  RedimensionarMatriz(2, FAncho, FAlto);

  for x := 0 to FAncho - 1 do
  begin
    for y := 0 to FAlto - 1 do
    begin
       // Resta 2: Valor absoluto
       FMemorias[2][x, y].R := Abs(Integer(FMemorias[0][x, y].R) - Integer(FMemorias[1][x, y].R));
       FMemorias[2][x, y].G := Abs(Integer(FMemorias[0][x, y].G) - Integer(FMemorias[1][x, y].G));
       FMemorias[2][x, y].B := Abs(Integer(FMemorias[0][x, y].B) - Integer(FMemorias[1][x, y].B));
    end;
  end;

  ActualizarTImage(2, Destino);
end;

procedure TGestorImagenes.RestaImagenes3(Destino: TImage);
var
  x, y: Integer;
begin
  if (FAncho = 0) or (FAlto = 0) then Exit;

  RedimensionarMatriz(2, FAncho, FAlto);

  for x := 0 to FAncho - 1 do
  begin
    for y := 0 to FAlto - 1 do
    begin
       // Resta 3: Ajuste de media (Norma shift)
       // Z = 127 + (x - y) / 2
       // Usamos 127 (aprox de (255)/2)
       
       FMemorias[2][x, y].R := 127 + (Integer(FMemorias[0][x, y].R) - Integer(FMemorias[1][x, y].R)) div 2;
       FMemorias[2][x, y].G := 127 + (Integer(FMemorias[0][x, y].G) - Integer(FMemorias[1][x, y].G)) div 2;
       FMemorias[2][x, y].B := 127 + (Integer(FMemorias[0][x, y].B) - Integer(FMemorias[1][x, y].B)) div 2;
    end;
  end;

  ActualizarTImage(2, Destino);
end;

procedure TGestorImagenes.ReflexionHorizontal(Index: Integer; Destino: TImage);
var
  x, y: Integer;
  Temp: TRGBPixel;
begin
  if (FAncho = 0) or (FAlto = 0) then Exit;

  // Intercambiar columnas: Solo iteramos hasta la mitad del ancho
  for x := 0 to (FAncho div 2) - 1 do
  begin
    for y := 0 to FAlto - 1 do
    begin
      Temp := FMemorias[Index][x, y];
      FMemorias[Index][x, y] := FMemorias[Index][FAncho - 1 - x, y];
      FMemorias[Index][FAncho - 1 - x, y] := Temp;
    end;
  end;

  ActualizarTImage(Index, Destino);
end;

procedure TGestorImagenes.ReflexionVertical(Index: Integer; Destino: TImage);
var
  x, y: Integer;
  Temp: TRGBPixel;
begin
  if (FAncho = 0) or (FAlto = 0) then Exit;

  // Intercambiar renglones: Solo iteramos hasta la mitad del alto
  for x := 0 to FAncho - 1 do
  begin
    for y := 0 to (FAlto div 2) - 1 do
    begin
      Temp := FMemorias[Index][x, y];
      FMemorias[Index][x, y] := FMemorias[Index][x, FAlto - 1 - y];
      FMemorias[Index][x, FAlto - 1 - y] := Temp;
    end;
  end;

  ActualizarTImage(Index, Destino);
end;

procedure TGestorImagenes.ReflexionDoble(Index: Integer; Destino: TImage);
begin
  // Una reflexión doble es equivalente a una horizontal seguida de una vertical
  // Como ya tenemos los métodos que operan sobre la misma memoria, podemos encadenarlos.
  // Pero ojo: cada uno llama a ActualizarTImage. Para eficiencia podríamos hacerlo directo, 
  // pero para reutilización llamaremos a los otros.
  
  // Como estamos modificando la memoria 'in-place', el orden no importa para el resultado final.
  
  // Nota: Al llamar a ReflexionHorizontal, se actualizará la imagen una vez.
  // Luego ReflexionVertical actualizará otra vez. Es aceptable.
  
  ReflexionHorizontal(Index, Destino);
  ReflexionVertical(Index, Destino);
end;

end.
