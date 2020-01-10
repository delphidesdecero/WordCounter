unit UAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Windows, FMX.Layouts, FMX.ExtCtrls,
  FMX.Controls.Presentation, FMX.StdCtrls, ShellApi, FMX.ScrollBox, FMX.Memo;

type
  TFAbout = class(TForm)
    ImageViewer1: TImageViewer;
    lblUrl: TLabel;
    btnClose: TButton;
    lblVersion: TLabel;
    Memo1: TMemo;
    procedure lblUrlClick(Sender: TObject);
    procedure ImageViewer1Click(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure ShellOpen(const Url: string; const Params: string = '');
    function GetAppVersionStr: string;
  public
    { Public declarations }
  end;

var
  FAbout: TFAbout;

implementation

{$R *.fmx}

Uses UPrincipal;

procedure TFAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFAbout.FormShow(Sender: TObject);
begin
  lblVersion.Text := 'Versión 1.0';
  //lblVersion.Text := 'Versión ' + GetAppVersionStr;
end;

function TFAbout.GetAppVersionStr: string;
var
  Rec: LongRec;
begin
  Rec := LongRec(GetFileVersion(ParamStr(0)));
  Result := Format('%d.%d', [Rec.Hi, Rec.Lo])
end;

procedure TFAbout.ImageViewer1Click(Sender: TObject);
begin
  ShellOpen('https://delphidesdecero.com');
end;

procedure TFAbout.lblUrlClick(Sender: TObject);
begin
  ShellOpen('https://delphidesdecero.com');
end;

procedure TFAbout.ShellOpen(const Url: string; const Params: string = '');
begin
  ShellAPI.ShellExecute(0, 'Open', PChar(Url), PChar(Params), nil, SW_SHOWNORMAL);
end;

end.
