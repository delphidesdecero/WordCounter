unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Rtti,
  IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Grid.Style, FMX.Grid, FMX.DialogService;

type
  TCountType = (ctWord, ctParagraph, ctSentence);

  TFPrincipal = class(TForm)
    MainMenu1: TMainMenu;
    StyleBook1: TStyleBook;
    MenuFile: TMenuItem;
    MenuItem3: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    gbTexto: TGroupBox;
    mmoText: TMemo;
    MenuSaveAs: TMenuItem;
    MenuExit: TMenuItem;
    MenuItem11: TMenuItem;
    lblCounters: TLabel;
    sgDetails: TStringGrid;
    Details: TStringColumn;
    Value: TStringColumn;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    MenuNew: TMenuItem;
    procedure MenuExitClick(Sender: TObject);
    procedure mmoTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure WordCountToLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure mmoTextChangeTracking(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
  private
    { Private declarations }
    procedure SaveFile;
    procedure SaveAsFile;
    procedure IsSaved;
    function Counter(vText: String; vCountType: TCountType): Int64;
    function SegToHour(vSeg: Int64): String;
  public
    { Public declarations }
    vLastPath: String;
    vSaved: Boolean;
    vIsOpenFile: Boolean;
    vPathFileOpen: String;
  end;

var
  FPrincipal: TFPrincipal;

implementation

{$R *.fmx}

uses USplash, UAbout;

procedure TFPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
var
  vFile: TIniFile;
begin
  vFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    with vFile do
    begin
      WriteString('Config', 'LastPath', vLastPath);
    end;
  finally
    vFile.Free;
  end;

  IsSaved;

  Application.Terminate;
end;

procedure TFPrincipal.IsSaved;
begin
  if not vSaved then
  begin
    TDialogService.MessageDialog('The document is not saved. Do you want to save it now?', TMsgDlgType.mtConfirmation,
      mbYesNo, TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
          SaveFile;
      end);
  end;
end;

procedure TFPrincipal.FormShow(Sender: TObject);
var
  vFile: TIniFile;
begin
  { Config }
  vFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    with vFile do
    begin
      vLastPath := ReadString('Config', 'LastPath', ExtractFilePath(ParamStr(0)));
    end;
  finally
    vFile.Free;
  end;
  { Default Variables }
  vSaved := True;
  vIsOpenFile := False;
  vPathFileOpen := '';

  mmoText.SetFocus;
end;

procedure TFPrincipal.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFPrincipal.MenuItem11Click(Sender: TObject);
begin
  FAbout.ShowModal;
end;

procedure TFPrincipal.MenuNewClick(Sender: TObject);
begin
  IsSaved;

  mmoText.Lines.Clear;
  vIsOpenFile := False;
  vPathFileOpen := '';
  vSaved := True;
end;

procedure TFPrincipal.MenuOpenClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    InitialDir := vLastPath;
    if Execute then
    begin
      vLastPath := ExtractFilePath(FileName);
      vPathFileOpen := FileName;
      mmoText.Lines.Clear;
      mmoText.Lines.LoadFromFile(FileName);
      vIsOpenFile := True;
      vSaved := True;
    end;
  end;
end;

procedure TFPrincipal.MenuSaveAsClick(Sender: TObject);
begin
  SaveAsFile;
end;

procedure TFPrincipal.MenuSaveClick(Sender: TObject);
begin
  SaveFile;
end;

procedure TFPrincipal.SaveFile;
begin
  if vIsOpenFile then
  begin
    mmoText.Lines.SaveToFile(vPathFileOpen);
  end
  else
  begin
    SaveAsFile;
  end;
end;

procedure TFPrincipal.SaveAsFile;
begin
  with SaveDialog do
  begin
    InitialDir := vLastPath;
    if Execute then
    begin
      vLastPath := ExtractFilePath(FileName);
      vPathFileOpen := FileName;
      mmoText.Lines.SaveToFile(FileName);
      vIsOpenFile := True;
      vSaved := True;
    end;
  end;
end;

function TFPrincipal.Counter(vText: String; vCountType: TCountType): Int64;
var
  vIx: Int64;
  vWord_Count: Int64;

  function Separator(vAs_Arg: Char; vCountType: TCountType): Boolean;
  begin
    case vCountType of
      ctWord:
        Result := vAs_Arg In [#0 .. #$1F, ' ', '.', ',', '?', ':', ';', '(', ')', '/', '\'];
      ctParagraph:
        Result := vAs_Arg In [#0 .. #$1F];
      ctSentence:
        Result := vAs_Arg In [#0 .. #$1F, '.'];
    end;
  end;

begin
  vWord_Count := 0;
  vIx := 1;
  while vIx <= Length(vText) Do
  begin
    while (vIx <= Length(vText)) And (Separator(vText[vIx], vCountType)) Do
      Inc(vIx);
    if vIx <= Length(vText) Then
    begin
      Inc(vWord_Count);
      while (vIx <= Length(vText)) And (Not Separator(vText[vIx], vCountType)) Do
        Inc(vIx);
    end;
  end;
  Result := vWord_Count;
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
  vSaved := False;

  { Counts }
  vWordsCount := Counter(mmoText.Text, ctWord);
  vCharacteresCount := mmoText.Text.Length;
  vSentences := Counter(mmoText.Text, ctSentence);
  vParagraphCount := Counter(mmoText.Text, ctParagraph);

  { Label }
  lblCounters.Text := IntToStr(vWordsCount) + ' words ' + IntToStr(vCharacteresCount) + ' characters ';

  { Details }
  sgDetails.Cells[0, 0] := 'Words';
  sgDetails.Cells[1, 0] := IntToStr(vWordsCount);

  sgDetails.Cells[0, 1] := 'Characters';
  sgDetails.Cells[1, 1] := IntToStr(vCharacteresCount);

  sgDetails.Cells[0, 2] := 'Sentences';
  sgDetails.Cells[1, 2] := IntToStr(vSentences);

  sgDetails.Cells[0, 3] := 'Paragraphs';
  sgDetails.Cells[1, 3] := IntToStr(vParagraphCount);

  sgDetails.Cells[0, 4] := 'Average words per sentences';
  sgDetails.Cells[1, 4] := FloatToStrF(vWordsCount / vSentences, fffixed, 10, 2);

  sgDetails.Cells[0, 5] := 'Average characters per sentences';
  sgDetails.Cells[1, 5] := FloatToStrF(vCharacteresCount / vSentences, fffixed, 10, 2);

  sgDetails.Cells[0, 6] := 'Average words per paragraph';
  sgDetails.Cells[1, 6] := FloatToStrF(vWordsCount / vParagraphCount, fffixed, 10, 2);

  sgDetails.Cells[0, 7] := 'Average characters per paragraph';
  sgDetails.Cells[1, 7] := FloatToStrF(vCharacteresCount / vParagraphCount, fffixed, 10, 2);

  sgDetails.Cells[0, 8] := 'Average reading time';
  sgDetails.Cells[1, 8] := SegToHour(Round(vWordsCount * 60 / 275));

  sgDetails.Cells[0, 9] := 'Average speaking time';
  sgDetails.Cells[1, 9] := SegToHour(Round(vWordsCount * 60 / 180));

end;

end.
