unit UPrincipal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Rtti, System.Generics.Collections, Generics.Defaults,
  IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo,
  FMX.Grid.Style, FMX.Grid, FMX.DialogService, System.ImageList, FMX.ImgList, FMX.Edit, FMX.EditBox, FMX.SpinBox;

type
  TCountWords = record
    Word: string;
    Count: Integer;
  end;

  TObjectWords = class(Tobject)
  public
    Data: TCountWords end;

    TCountType = (ctWord, ctParagraph, ctSentence);

    TFPrincipal = class(TForm)MainMenu1: TMainMenu;
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
    ToolBar1: TToolBar;
    sbNewFile: TSpeedButton;
    ilIconMenu: TImageList;
    sbOpenFile: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Keywords: TGroupBox;
    sgKeywords: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    Splitter1: TSplitter;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    sbKeyword: TSpinBox;
    Label2: TLabel;
    Label3: TLabel;
    SpinBox1: TSpinBox;
    Label4: TLabel;
    procedure MenuExitClick(Sender: Tobject);
    procedure mmoTextKeyUp(Sender: Tobject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure WordCountToLabel;
    procedure FormClose(Sender: Tobject; var Action: TCloseAction);
    procedure FormShow(Sender: Tobject);
    procedure MenuOpenClick(Sender: Tobject);
    procedure MenuSaveClick(Sender: Tobject);
    procedure MenuNewClick(Sender: Tobject);
    procedure mmoTextChangeTracking(Sender: Tobject);
    procedure MenuSaveAsClick(Sender: Tobject);
    procedure MenuItem11Click(Sender: Tobject);
    procedure sbNewFileClick(Sender: Tobject);
    procedure sbOpenFileClick(Sender: Tobject);
    procedure SpeedButton1Click(Sender: Tobject);
    procedure FormResize(Sender: Tobject);
    procedure sbKeywordChange(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
  private
    { Private declarations }
    procedure SaveFile;
    procedure OpenFile;
    procedure SaveAsFile;
    procedure IsSaved;
    procedure NewFile;
    function CleanString(vText: string): string;
    function Wrap(vText: String): String;
    function Counter(vText: String; vCountType: TCountType): Int64;
    function SegToHour(vSeg: Int64): String;
  public
    { Public declarations }
    vLastPath: String;
    vSaved: Boolean;
    vIsOpenFile: Boolean;
    vPathFileOpen: String;
    vaWords: array of TCountWords;
    TSortListWords: TObjectList<TObjectWords>;
  end;

var
  FPrincipal: TFPrincipal;

implementation

{$R *.fmx}

uses USplash, UAbout;

procedure TFPrincipal.FormClose(Sender: Tobject; var Action: TCloseAction);
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

  TSortListWords.Free;
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

procedure TFPrincipal.FormShow(Sender: Tobject);
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

procedure TFPrincipal.MenuExitClick(Sender: Tobject);
begin
  Close;
end;

procedure TFPrincipal.MenuItem11Click(Sender: Tobject);
begin
  FAbout.ShowModal;
end;

procedure TFPrincipal.MenuNewClick(Sender: Tobject);
begin
  NewFile;
end;

procedure TFPrincipal.NewFile;
begin
  IsSaved;

  mmoText.Lines.Clear;
  vIsOpenFile := False;
  vPathFileOpen := '';
  vSaved := True;
end;

procedure TFPrincipal.MenuOpenClick(Sender: Tobject);
begin
  OpenFile;
end;

procedure TFPrincipal.OpenFile;
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

procedure TFPrincipal.MenuSaveAsClick(Sender: Tobject);
begin
  SaveAsFile;
end;

procedure TFPrincipal.MenuSaveClick(Sender: Tobject);
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

procedure TFPrincipal.sbKeywordChange(Sender: TObject);
begin
  WordCountToLabel;
end;

procedure TFPrincipal.sbNewFileClick(Sender: Tobject);
begin
  NewFile;
end;

procedure TFPrincipal.sbOpenFileClick(Sender: Tobject);
begin
  OpenFile;
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

function TFPrincipal.Wrap(vText: String): String;
var
  vList: TStringList;
  vAuxStr: string;
  vAuxWord: string;
  I: Int64;
  K: Int64;
  vAuxKeyWord: TCountWords;
  c: TObjectWords;
  vTop: Integer;
begin
  vAuxStr := WrapText(vText, 1);

  { List of Word }
  vList := TStringList.Create;
  vList.Text := vAuxStr;
  vList.sort;

  { Clean string of strange characters }
  for I := 0 to vList.Count - 1 do
  begin
    vList[I] := Trim(CleanString(vList[I]));
    if vList[I].Length < 3 then
    begin
      vList[I] := '';
    end;
  end;

  // Showmessage('List Count '+IntToStr(vList.Count));

  vAuxWord := '';
  K := 0;
  SetLength(vaWords, 0);

  for I := 0 to vList.Count - 1 do
  begin
    if vList[I].Length > 3 then
    begin
      if vList[I] = vAuxWord then
      begin
        Inc(vaWords[K - 1].Count);
      end
      else
      begin
        SetLength(vaWords, Length(vaWords) + 1);
        vaWords[K].Word := vList[I];
        vaWords[K].Count := 1;
        vAuxWord := vList[I];
        Inc(K);
      end;
    end;
  end;

  TSortListWords := TObjectList<TObjectWords>.Create(True);
  for I := 0 to Length(vaWords) - 1 do
  begin
    c := TObjectWords.Create;
    c.Data.Word := vaWords[I].Word;
    c.Data.Count := vaWords[I].Count;

    TSortListWords.Add(c);
  end;

  { Sort }
  TSortListWords.sort(TComparer<TObjectWords>.Construct(
    function(const a, b: TObjectWords): Integer
    begin
      { Sort Count }
      if TObjectWords(a).Data.Count < TObjectWords(b).Data.Count then
        Result := 1
      else if TObjectWords(a).Data.Count > TObjectWords(b).Data.Count then
        Result := -1
      else
        { Sort Count + Word }
        if LowerCase(TObjectWords(a).Data.Word) > LowerCase(TObjectWords(b).Data.Word) then
          Result := 1
        else if LowerCase(TObjectWords(a).Data.Word) < LowerCase(TObjectWords(b).Data.Word) then
          Result := -1
        else
          Result := 0;
    end));


  if TSortListWords.Count < sbKeyword.Value then
    vTop := TSortListWords.Count - 1
  else
    vTop := round(sbKeyword.Value) - 1;

  sgKeywords.RowCount := vTop + 1;


  for I := 0 to vTop do
  begin
    with TObjectWords(TSortListWords.Items[I]) do
    begin
      sgKeywords.Cells[0, I] := Data.Word;
      sgKeywords.Cells[1, I] := IntToStr(Data.Count);
    end;
  end;

end;

function TFPrincipal.CleanString(vText: string): string;
var
  I: Int64;
  vAuxStr: string;
  vAuxChar: Char;
  vLowerText: string;

  function NumberCharacter(vAs_Arg: Char): Boolean;
  begin
    Result := vAs_Arg In ['0' .. '9'];
  end;

begin
  try
    vAuxStr := '';
    vLowerText := LowerCase(vText);

    for I := 0 to vText.Length do
    begin
      vAuxChar := vLowerText[I];
      if vAuxChar In ['a' .. 'z'] then
      begin
        vAuxStr := vAuxStr + vText[I];
      end
      else
      begin
        if NumberCharacter(vAuxChar) then
        begin
          vAuxStr := '';
          Break;
        end;
      end;
    end;
  except
    vAuxStr := '';
  end;

  Result := vAuxStr;

end;

procedure TFPrincipal.mmoTextChangeTracking(Sender: Tobject);
begin
  WordCountToLabel;
end;

procedure TFPrincipal.mmoTextKeyUp(Sender: Tobject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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

procedure TFPrincipal.SpeedButton1Click(Sender: Tobject);
begin
  SaveFile;
end;

procedure TFPrincipal.SpinBox1Change(Sender: TObject);
begin
  ShowMessage('Soon');
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

  { Keywords }
  Wrap(mmoText.Text);
end;

procedure TFPrincipal.FormResize(Sender: Tobject);
begin
  if FPrincipal.Height < 600 then
    FPrincipal.Height := 600;
  if FPrincipal.Width < 800 then
    FPrincipal.Width := 800;
end;

end.
