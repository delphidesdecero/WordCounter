unit USplash;

interface

uses
  FMX.Forms, System.Classes, FMX.Types;

type
  TFSplash = class(TForm)
    TmrSplash: TTimer;
    procedure TmrSplashTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    vCompletado: Boolean;
  end;

var
  FSplash: TFSplash;


implementation

{$R *.fmx}

uses UPrincipal;

procedure TFSplash.FormShow(Sender: TObject);
begin
  vCompletado := False;
end;

procedure TFSplash.TmrSplashTimer(Sender: TObject);
begin
  vCompletado := True;
end;

end.
