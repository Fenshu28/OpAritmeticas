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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    // Instancia de tu gestor lógico
    Gestor: TGestorImagenes;
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
  // Asignación manual para asegurar que funcione incluso si el LFM no se recarga
  btnOperar.OnClick := @btnOperarClick;
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
    Gestor.CargarImagen(0, OpenPictureDialog1.FileName, Image1);
  end;
end;

// Cargar Imagen 2
procedure TForm1.btnCargImg2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    // Índice 1 = Imagen 2
    Gestor.CargarImagen(1, OpenPictureDialog1.FileName, Image2);
  end;
end;

procedure TForm1.btnOperarClick(Sender: TObject);
begin
  if not Assigned(Gestor) then Exit;
  
  ShowMessage('Operando...'); // Debug

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
end;

procedure TForm1.cbOperaChange(Sender: TObject);
begin
  if AnsiStartsText('Suma', cbOpera.Text) then
     lbOperacion.Caption:= '+'
  else
      lbOperacion.Caption:='-';
end;

end.
