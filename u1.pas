unit u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComboEx, StdCtrls, StrUtils;

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
    procedure cbOperaChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbOperaChange(Sender: TObject);
begin
  if AnsiStartsText('Suma', cbOpera.Text) then
     lbOperacion.Caption:= '+'
  else
      lbOperacion.Caption:='-';
end;

end.

