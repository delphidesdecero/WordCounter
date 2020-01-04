unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TFPrincipal = class(TForm)
    MainMenu1: TMainMenu;
    StyleBook1: TStyleBook;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    gbTexto: TGroupBox;
    mmoText: TMemo;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem11: TMenuItem;
    lblCounters: TLabel;
    procedure MenuItem10Click(Sender: TObject);
    procedure mmoTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure WordCountToLabel;
    procedure mmoTextChangeTracking(Sender: TObject);
  private
    { Private declarations }
    function WordCount(CText: String): Longint;
  public
    { Public declarations }
  end;

var
  FPrincipal: TFPrincipal;

implementation

{$R *.fmx}

procedure TFPrincipal.MenuItem10Click(Sender: TObject);
begin
  Application.Terminate;
end;

function TFPrincipal.WordCount(CText: String): Longint;
var
  Ix: Word;
  Work_Count: Longint;

  function Seps(As_Arg: Char): Boolean;
  begin
    Seps := As_Arg In [#0 .. #$1F, ' ', '.', ',', '?', ':', ';', '(', ')', '/', '\'];
  end;

begin
  Work_Count := 0;
  Ix := 1;
  while Ix <= Length(CText) Do
  begin
    while (Ix <= Length(CText)) And (Seps(CText[Ix])) Do
      Inc(Ix);
    if Ix <= Length(CText) Then
    begin
      Inc(Work_Count);

      while (Ix <= Length(CText)) And (Not Seps(CText[Ix])) Do
        Inc(Ix);
    end;
  end;
  Result := Work_Count;
end;

procedure TFPrincipal.mmoTextChangeTracking(Sender: TObject);
begin
  WordCountToLabel;
end;

procedure TFPrincipal.mmoTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  WordCountToLabel;
end;

procedure TFPrincipal.WordCountToLabel;
begin
  lblCounters.Text := IntToStr(WordCount(mmoText.Text)) + ' words ' + IntToStr(mmoText.Text.Length) + ' characters';
end;

end.
