unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, System.Rtti, FMX.Grid.Style, FMX.Grid;

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
    sgDetails: TStringGrid;
    Details: TStringColumn;
    Value: TStringColumn;
    procedure MenuItem10Click(Sender: TObject);
    procedure mmoTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure WordCountToLabel;
    procedure mmoTextChangeTracking(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    function WordCount(vText: String): Int64;
    function ParagraphCount(vText: String): Int64;
    function SentencesCount(vText: String): Int64;
    function SegToHour(vSeg: Int64): String;
  public
    { Public declarations }
  end;

var
  FPrincipal: TFPrincipal;

implementation

{$R *.fmx}

uses USplash;

procedure TFPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.Terminate;
end;

procedure TFPrincipal.MenuItem10Click(Sender: TObject);
begin
  Close;
end;

function TFPrincipal.WordCount(vText: String): Int64;
var
  vIx: Int64;
  vWord_Count: Int64;

  function Word(vAs_Arg: Char): Boolean;
  begin
    Result := vAs_Arg In [#0 .. #$1F, ' ', '.', ',', '?', ':', ';', '(', ')', '/', '\'];
  end;

begin
  vWord_Count := 0;
  vIx := 1;
  while vIx <= Length(vText) Do
  begin
    while (vIx <= Length(vText)) And (Word(vText[vIx])) Do
      Inc(vIx);
    if vIx <= Length(vText) Then
    begin
      Inc(vWord_Count);

      while (vIx <= Length(vText)) And (Not Word(vText[vIx])) Do
        Inc(vIx);
    end;
  end;
  Result := vWord_Count;
end;

function TFPrincipal.ParagraphCount(vText: String): Int64;
var
  vIx: Int64;
  vParagraph_Count: Int64;

  function Paragraph(vAs_Arg: Char): Boolean;
  begin
    Result := vAs_Arg In [#0 .. #$1F];
  end;

begin
  vParagraph_Count := 0;
  vIx := 1;
  while vIx <= Length(vText) Do
  begin
    while (vIx <= Length(vText)) And (Paragraph(vText[vIx])) Do
      Inc(vIx);
    if vIx <= Length(vText) Then
    begin
      Inc(vParagraph_Count);

      while (vIx <= Length(vText)) And (Not Paragraph(vText[vIx])) Do
        Inc(vIx);
    end;
  end;
  Result := vParagraph_Count;
end;

function TFPrincipal.SentencesCount(vText: String): Int64;
var
  vIx: Int64;
  vSentences_Count: Int64;

  function Sentences(vAs_Arg: Char): Boolean;
  begin
    Result := vAs_Arg In [#0 .. #$1F, '.'];
  end;

begin
  vSentences_Count := 0;
  vIx := 1;
  while vIx <= Length(vText) Do
  begin
    while (vIx <= Length(vText)) And (Sentences(vText[vIx])) Do
      Inc(vIx);
    if vIx <= Length(vText) Then
    begin
      Inc(vSentences_Count);

      while (vIx <= Length(vText)) And (Not Sentences(vText[vIx])) Do
        Inc(vIx);
    end;
  end;
  Result := vSentences_Count;
end;

procedure TFPrincipal.mmoTextChangeTracking(Sender: TObject);
begin
  WordCountToLabel;
end;

procedure TFPrincipal.mmoTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  WordCountToLabel;
end;

function TFPrincipal.SegToHour(vSeg: Int64): String;
var
  vHour: Integer;
  vMin: Integer;
  vAuxStr: String;
begin
  vHour := vSeg div 3600;
  vMin := vSeg div 60 mod 60;
  vSeg := vSeg mod 60;

  vAuxStr := '';
  if vHour > 0 then
    vAuxStr := IntToStr(vHour) + ' Hours ';

  if (vHour > 0) or (vMin > 0) then
    vAuxStr := vAuxStr + IntToStr(vMin) + ' Min ';

  vAuxStr := vAuxStr + IntToStr(vSeg) + ' Seg';

  Result := vAuxStr;
end;

procedure TFPrincipal.WordCountToLabel;
var
  vWordsCount: Int64;
  vCharacteresCount: Int64;
  vParagraphCount: Int64;
  vSentences: Int64;
begin
  // Counts
  vWordsCount := WordCount(mmoText.Text);
  vCharacteresCount := mmoText.Text.Length;
  vSentences := SentencesCount(mmoText.Text);
  vParagraphCount := ParagraphCount(mmoText.Text);

  // Label
  lblCounters.Text := IntToStr(vWordsCount) + ' words ' + IntToStr(vCharacteresCount) + ' characters ';

  // Details
  sgDetails.Cells[0, 0] := 'Words';
  sgDetails.Cells[1, 0] := IntToStr(vWordsCount);

  sgDetails.Cells[0, 1] := 'Characters';
  sgDetails.Cells[1, 1] := IntToStr(vCharacteresCount);

  sgDetails.Cells[0, 2] := 'Sentences';
  sgDetails.Cells[1, 2] := IntToStr(vSentences);

  sgDetails.Cells[0, 3] := 'Paragraphs';
  sgDetails.Cells[1, 3] := IntToStr(vParagraphCount);

  sgDetails.Cells[0, 4] := 'Average words per sentences';
  sgDetails.Cells[1, 4] := FloatToStrF(vWordsCount / vSentences, Fffixed, 10, 2);

  sgDetails.Cells[0, 5] := 'Average characters per sentences';
  sgDetails.Cells[1, 5] := FloatToStrF(vCharacteresCount / vSentences, Fffixed, 10, 2);

  sgDetails.Cells[0, 6] := 'Average words per paragraph';
  sgDetails.Cells[1, 6] := FloatToStrF(vWordsCount / vParagraphCount, Fffixed, 10, 2);

  sgDetails.Cells[0, 7] := 'Average characters per paragraph';
  sgDetails.Cells[1, 7] := FloatToStrF(vCharacteresCount / vParagraphCount, Fffixed, 10, 2);

  sgDetails.Cells[0, 8] := 'Average reading time';
  sgDetails.Cells[1, 8] := SegToHour(Round(vWordsCount * 60 / 275));

  sgDetails.Cells[0, 9] := 'Average speaking time';
  sgDetails.Cells[1, 9] := SegToHour(Round(vWordsCount * 60 / 180));

end;

end.
