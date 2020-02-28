unit UKeywordsMining;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP;

type
  TFKeywordMining = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    mmoKeywords: TMemo;
    edtKeywordSearch: TEdit;
    btnStart: TButton;
    tmrMining: TTimer;
    lblNKey: TLabel;
    Button1: TButton;
    IdHTTP1: TIdHTTP;
    cbTag: TCheckBox;
    procedure btnStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure StartStop;
    procedure tmrMiningTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    vVariants: TStringList;
    vKeywordList: TStringList;
    vIdKeywordList: Integer;
    vIdVariants: Integer;
    vStart: Boolean;
  public
    { Public declarations }
  end;

  TMining = class(TThread)
  private
    vKeywordSearch: string;
    vResult: string;
    procedure RefreshMemo;
    function GetURLAsString(vKeyword: string): string;
  protected
    procedure Execute; override;
  public
    constructor Create(vKeyword: string);
    destructor Destroy; override;
  end;

var
  FKeywordMining: TFKeywordMining;

implementation

{$R *.fmx}

uses UPrincipal;
{ ********************************************* }
{ *                 TMining                   * }
{ ********************************************* }

constructor TMining.Create(vKeyword: string);
begin
  inherited Create(True);
  vKeywordSearch := vKeyword;
end;

destructor TMining.Destroy;
begin
  inherited;
end;

procedure TMining.Execute;
begin
  vResult := GetURLAsString(vKeywordSearch);
  Synchronize(RefreshMemo);
end;

function TMining.GetURLAsString(vKeyword: string): string;
var
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create;
  lHTTP.Request.AcceptCharSet := 'utf-8';
  try
    Result := lHTTP.Get('https://suggestqueries.google.com/complete/search?jsonp=delphidesdecero&q=' + vKeyword +
      '&client=chrome');
  finally
    lHTTP.Free;
  end;
end;

procedure TMining.RefreshMemo;
var
  vAuxStr: string;
  vAuxLeft: string;
  vAuxRight: string;
begin
  if vResult.Length > 4 then
  begin
    vKeywordSearch := StringReplace(vKeywordSearch, '+', ' ', [rfReplaceAll, rfIgnoreCase]);

    vAuxLeft := 'delphidesdecero(["' + vKeywordSearch + '",[';
    vAuxRight := '"],';

    vAuxStr := copy(vResult, pos(vAuxLeft, vResult) + vAuxLeft.Length, pos(vAuxRight, vResult) - vAuxLeft.Length);
    vAuxStr := StringReplace(vAuxStr, '"', '', [rfReplaceAll, rfIgnoreCase]);
    vAuxStr := wraptext(vAuxStr, #13#10, [','], 1);
    vAuxStr := StringReplace(vAuxStr, ',', '', [rfReplaceAll, rfIgnoreCase]);

    if vAuxStr.Length > 0 then
    begin
      if FKeywordMining.cbTag.IsChecked then
        vAuxStr := '#' + StringReplace(vAuxStr, ' ', '', [rfReplaceAll, rfIgnoreCase]);

      FKeywordMining.mmoKeywords.Lines.Add(vAuxStr);

      FKeywordMining.vKeywordList := TStringList.Create;
      FKeywordMining.vKeywordList.Text := FKeywordMining.mmoKeywords.Text;

      FKeywordMining.lblNKey.Text := IntToStr(FKeywordMining.vKeywordList.Count)+' Keywords';

      FKeywordMining.mmoKeywords.GoToTextEnd;
    end;
  end;
end;

{ ********************************************* }
{ *               End TMining                 * }
{ ********************************************* }

procedure TFKeywordMining.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFKeywordMining.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  vVariants.Free;
  vKeywordList.Free;

  tmrMining.Enabled := False;
end;

procedure TFKeywordMining.FormShow(Sender: TObject);
var
  vAuxStr: string;
begin
  vAuxStr := wraptext('a b c d e f g h i j k l m n o p q r s t u v w x y z', 1);

  vVariants := TStringList.Create;
  vVariants.Text := vAuxStr;

  vStart := False;
  StartStop;

  lblNKey.Text := '0 Keywords';

  edtKeywordSearch.SetFocus;
end;

procedure TFKeywordMining.btnStartClick(Sender: TObject);
var
  vAuxStr: string;
begin
  vStart := not vStart;
  StartStop;
end;

procedure TFKeywordMining.StartStop;
var
  vAuxStr: string;
  I: Integer;
begin
  if vStart then
  begin
    if Length(edtKeywordSearch.Text) = 0 then
    begin
      vStart := False;
    end
    else
    begin
      vIdKeywordList := 0; { ItemIndex  Keyword List }
      vIdVariants := -1; { ItemIndex  Variants List }

      lblNKey.Text := '0 Keywords';

      btnStart.Text := 'Stop mining';
      mmoKeywords.Lines.Clear;

      vKeywordList := TStringList.Create;


      vAuxStr := Trim(edtKeywordSearch.Text);
      vAuxStr := StringReplace(vAuxStr, ',', ' ', [rfReplaceAll, rfIgnoreCase]);
      vAuxStr := StringReplace(vAuxStr, ' ', '%20', [rfReplaceAll, rfIgnoreCase]);
      vKeywordList.Text := vAuxStr;

      for I := 0 to vKeywordList.Count - 1 do
        mmoKeywords.Lines.Add(StringReplace(vKeywordList[I], '%20', ' ', [rfReplaceAll, rfIgnoreCase]));
    end;
  end;

  if not vStart then
  begin
    btnStart.Text := 'Start mining';
  end;

  tmrMining.Enabled := vStart;
end;

procedure TFKeywordMining.tmrMiningTimer(Sender: TObject);
var
  vKeywordSearch: string;
begin
  if vIdVariants = -1 then
  begin
    vKeywordSearch := Trim(StringReplace(vKeywordList[vIdKeywordList], ' ', '%20', [rfReplaceAll, rfIgnoreCase]));
  end
  else
  begin
    vKeywordSearch := Trim(StringReplace(vKeywordList[vIdKeywordList], ' ', '%20', [rfReplaceAll, rfIgnoreCase]))+'+'+Trim(vVariants[vIdVariants]);
  end;


  inc(vIdVariants);

  if vIdVariants > vVariants.Count - 1 then
  begin
    vIdVariants := -1;
    inc(vIdKeywordList);
  end;

  if vIdKeywordList > vKeywordList.Count - 1 then
  begin
    { End mining }
    vStart := False;
    StartStop;
  end
  else
  begin
    with TMining.Create(Trim(vKeywordSearch)) do
    begin
      Priority := tpLower;
      Resume;
    end;
  end;




end;

end.
