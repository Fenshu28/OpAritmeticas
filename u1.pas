unit u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComboEx, StdCtrls, ExtDlgs, StrUtils, FPReadJPEG, FPReadPNG, FPWriteJPEG, FPWritePNG, // (O simplemente 'JPEG', 'PNG' dependiendo de tu versión de Lazarus)
  uLogica;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnCargImg1: TButton;
    btnCargImg2: TButton;
    btnOperar: TButton;
    btnDescarga: TButton;
    btnVertical: TButton;
    btnHorizontal: TButton;
    btnDoble: TButton;
    cbReflexion: TComboBox;
    cbOpera: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    btnSwap: TButton;
    btnCopyCA: TButton;
    btnCopyCB: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    lbOperacion: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    OpenPictureDialog1: TOpenPictureDialog; // <--- Asegúrate de poner este componente
    procedure btnCargImg1Click(Sender: TObject);
    procedure btnCargImg2Click(Sender: TObject);
    procedure cbOperaChange(Sender: TObject);
    procedure btnOperarClick(Sender: TObject);
    procedure btnVerticalClick(Sender: TObject);
    procedure btnHorizontalClick(Sender: TObject);
    procedure btnDobleClick(Sender: TObject);
    procedure btnDescargaClick(Sender: TObject);
    // Nuevos eventos
    procedure btnSwapClick(Sender: TObject);
    procedure btnCopyCAClick(Sender: TObject);
    procedure btnCopyCBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    // Instancia de tu gestor lógico
    Gestor: TGestorImagenes;
    function ObtenerImagenObjetivo(Index: Integer): TImage;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Inicializamos la lógica al arrancar
  Gestor := TGestorImagenes.Create;
  // Asignación manual para asegurar que funcionamiento
  btnOperar.OnClick := @btnOperarClick;
  btnVertical.OnClick := @btnVerticalClick;
  btnHorizontal.OnClick := @btnHorizontalClick;
  btnDoble.OnClick := @btnDobleClick;
  btnDescarga.OnClick := @btnDescargaClick;
  
  // Nuevos botones
  btnSwap.OnClick := @btnSwapClick;
  btnCopyCA.OnClick := @btnCopyCAClick;
  btnCopyCB.OnClick := @btnCopyCBClick;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Liberamos memoria al cerrar
  if Assigned(Gestor) then Gestor.Free;
end;

// Cargar Imagen 1
procedure TForm1.btnCargImg1Click(Sender: TObject);
begin
  // Verificación de seguridad
  if not Assigned(Gestor) then
  begin
    ShowMessage('Error: El Gestor no se inicializó. Verifica el evento OnCreate del Formulario.');
    Exit;
  end;

  if OpenPictureDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      Gestor.CargarImagen(0, OpenPictureDialog1.FileName, Image1);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

// Cargar Imagen 2
procedure TForm1.btnCargImg2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      // Índice 1 = Imagen 2
      Gestor.CargarImagen(1, OpenPictureDialog1.FileName, Image2);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.btnOperarClick(Sender: TObject);
begin
  if not Assigned(Gestor) then Exit;
  
  // ShowMessage('Operando...'); // Debug

  Screen.Cursor := crHourGlass;
  try
    // Verificamos cuál operación está seleccionada
    if AnsiStartsText('Suma1', cbOpera.Text) then
    begin
      Gestor.SumaImagenes(Image3);
    end
    else if AnsiStartsText('Suma2', cbOpera.Text) then
    begin
      Gestor.SumaImagenes2(Image3);
    end
    else if AnsiStartsText('Resta1', cbOpera.Text) then
    begin
      Gestor.RestaImagenes1(Image3);
    end
    else if AnsiStartsText('Resta2', cbOpera.Text) then
    begin
      Gestor.RestaImagenes2(Image3);
    end
    else if AnsiStartsText('Resta3', cbOpera.Text) then
    begin
      Gestor.RestaImagenes3(Image3);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.cbOperaChange(Sender: TObject);
begin
  if AnsiStartsText('Suma', cbOpera.Text) then
     lbOperacion.Caption:= '+'
  else
      lbOperacion.Caption:='-';
end;

function TForm1.ObtenerImagenObjetivo(Index: Integer): TImage;
begin
  Result := nil;
  case Index of
    0: Result := Image1;
    1: Result := Image2;
    2: Result := Image3;
  end;
end;

procedure TForm1.btnVerticalClick(Sender: TObject);
var
  Idx: Integer;
begin
  if not Assigned(Gestor) then Exit;
  Idx := cbReflexion.ItemIndex;

  if (Idx < 0) or (Idx > 2) then
  begin
    ShowMessage('Selecciona una imagen válida');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    Gestor.ReflexionVertical(Idx, ObtenerImagenObjetivo(Idx));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnHorizontalClick(Sender: TObject);
var
  Idx: Integer;
begin
  if not Assigned(Gestor) then Exit;
  Idx := cbReflexion.ItemIndex;

  if (Idx < 0) or (Idx > 2) then
  begin
    ShowMessage('Selecciona una imagen válida');
    Exit;
  end;
  
  Screen.Cursor := crHourGlass;
  try
    Gestor.ReflexionHorizontal(Idx, ObtenerImagenObjetivo(Idx));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnDobleClick(Sender: TObject);
var
  Idx: Integer;
begin
  if not Assigned(Gestor) then Exit;
  Idx := cbReflexion.ItemIndex;

  if (Idx < 0) or (Idx > 2) then
  begin
    ShowMessage('Selecciona una imagen válida');
    Exit;
  end;
  
  Screen.Cursor := crHourGlass;
  try
    Gestor.ReflexionDoble(Idx, ObtenerImagenObjetivo(Idx));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnDescargaClick(Sender: TObject);
var
  SaveDlg: TSavePictureDialog;
begin
  // Validar si hay imagen en Image3
  if (Image3.Picture.Graphic = nil) or (Image3.Picture.Width = 0) then
  begin
    ShowMessage('No hay resultado para descargar. Realiza una operación primero.');
    Exit;
  end;

  SaveDlg := TSavePictureDialog.Create(nil);
  try
    SaveDlg.Title := 'Guardar Resultado';
    SaveDlg.Filter := 'Imagen PNG|*.png|Imagen JPEG|*.jpg';
    SaveDlg.DefaultExt := 'png';
    SaveDlg.FileName := 'resultado';

    if SaveDlg.Execute then
    begin
       try
         Image3.Picture.SaveToFile(SaveDlg.FileName);
         ShowMessage('Imagen guardada exitosamente.');
       except
         on E: Exception do
           ShowMessage('Error al guardar: ' + E.Message);
       end;
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TForm1.btnSwapClick(Sender: TObject);
begin
  if not Assigned(Gestor) then Exit;
  Screen.Cursor := crHourGlass;
  try
    Gestor.Intercambiar(0, 1, Image1, Image2);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnCopyCAClick(Sender: TObject);
begin
  if not Assigned(Gestor) then Exit;
  
  // Validamos si hay resultado (Index 2)
  if (Image3.Picture.Graphic = nil) then 
  begin
    ShowMessage('No hay imagen en C (Resultado) para copiar.');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    Gestor.Copiar(2, 0, Image1);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnCopyCBClick(Sender: TObject);
begin
  if not Assigned(Gestor) then Exit;

  if (Image3.Picture.Graphic = nil) then 
  begin
    ShowMessage('No hay imagen en C (Resultado) para copiar.');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    Gestor.Copiar(2, 1, Image2);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
